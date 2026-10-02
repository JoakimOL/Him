-- | Running language servers (ADR-29): the process, a reader thread that
-- splits its output into messages, a writer thread with a queue (so the
-- editor never blocks on a busy server), automatic replies to the server's
-- own requests, and the initialize handshake.
module Him.Lsp.Server
  ( Server
  , startServer
  , sendMessage
  , stopServer
  , findRoot
  ) where

import Control.Concurrent (forkIO, threadDelay)
import Control.Concurrent.MVar (newEmptyMVar, takeMVar, tryPutMVar)
import Control.Concurrent.STM (TQueue, atomically, newTQueueIO, readTQueue, writeTQueue)
import Control.Exception (IOException, SomeException, try)
import Control.Monad (forever, void)
import Data.ByteString qualified as BS
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Text qualified as T
import Him.Json
import Him.Log (logMsg)
import Him.Lsp.Config (ServerConfig (..))
import Him.Lsp.Protocol
import Him.Lsp.State (ServerInfo (..), Sync (..))
import System.Directory (doesPathExist)
import System.FilePath (takeDirectory, (</>))
import System.IO (BufferMode (..), Handle, hClose, hFlush, hSetBinaryMode, hSetBuffering)
import System.Posix.Process (getProcessID)
import System.Process (CreateProcess (..), ProcessHandle, StdStream (..), createProcess, proc, terminateProcess)
import System.Timeout (timeout)

data Server = Server
  { svQueue :: !(TQueue Value)
  , svProcess :: !ProcessHandle
  , svInput :: !Handle
  }

-- | The nearest directory at or above the file's with a root marker, or
-- the file's own directory.
findRoot :: [FilePath] -> FilePath -> IO FilePath
findRoot markers file = go (takeDirectory file)
  where
    go dir = do
      found <- or <$> mapM (\m -> doesPathExist (dir </> m)) markers
      let parent = takeDirectory dir
      if found then pure dir else if parent == dir then pure (takeDirectory file) else go parent

-- | Start a server for a root and complete the handshake. Messages the
-- editor must see (replies to its requests, notifications) go to
-- @deliver@; @exited@ runs when the server's output ends.
startServer :: ServerConfig -> FilePath -> (Value -> IO ()) -> IO () -> IO (Either Text (Server, ServerInfo))
startServer config root deliver exited = do
  started <- try @IOException $ createProcess (proc (scCommand config) (scArgs config)) {cwd = Just root, std_in = CreatePipe, std_out = CreatePipe, std_err = CreatePipe}
  case started of
    Left e -> pure (Left (T.pack (scCommand config) <> ": " <> T.pack (show e)))
    Right (Just hin, Just hout, Just herr, ph) -> do
      mapM_ (`hSetBinaryMode` True) [hin, hout, herr]
      hSetBuffering hin (BlockBuffering Nothing)
      queue <- newTQueueIO
      let server = Server queue ph hin
      -- Writer: one message at a time, in order.
      _ <- forkIO . void . try @SomeException . forever $ do
        msg <- atomically (readTQueue queue)
        BS.hPut hin (frameMessage msg)
        hFlush hin
      -- Standard error is drained to the log so the server never blocks
      -- on a full pipe.
      _ <- forkIO (drain herr)
      initialized <- newEmptyMVar
      let handle v = case classify v of
            Reply (-1) result -> void (tryPutMVar initialized result)
            -- Edits the server wants made go to the editor (it applies them
            -- in order); the server hears they were applied.
            ServerRequest i "workspace/applyEdit" _ -> do
              deliver v
              sendMessage server (response i (object [("applied", JBool True)]))
            ServerRequest i method params -> sendMessage server (response i (autoReply method params))
            _ -> deliver v
      _ <- forkIO (reader hout handle >> exited)
      pid <- getProcessID
      sendMessage server (request (-1) "initialize" (initializeParams (fromIntegral pid) root))
      -- Some servers index before answering; give them time.
      answer <- timeout 60000000 (takeMVar initialized)
      case answer of
        Just (Right result) -> do
          sendMessage server (notification "initialized" (object []))
          pure (Right (server, serverInfo config root result))
        Just (Left e) -> Left ("initialize failed: " <> e) <$ stopServer server
        Nothing -> Left "initialize timed out" <$ stopServer server
    Right _ -> pure (Left "could not open pipes")
  where
    drain h = void . try @SomeException . forever $ do
      chunk <- BS.hGetSome h 65536
      if BS.null chunk then fail "eof" else logMsg ("lsp stderr: " <> show (BS.take 200 chunk))

-- | Read messages until the output ends.
reader :: Handle -> (Value -> IO ()) -> IO ()
reader h handle = go emptyFramer
  where
    go framer = do
      chunk <- either (const BS.empty) id <$> try @IOException (BS.hGetSome h 65536)
      if BS.null chunk
        then pure ()
        else do
          let (bodies, framer') = feedFramer framer chunk
          mapM_ (either (\e -> logMsg ("lsp: bad message: " <> show e)) handle . parseJson) bodies
          go framer'

-- | Answers for the server's requests that need no decision from the
-- editor: configuration (none), progress tokens and registrations (fine).
autoReply :: Text -> Value -> Value
autoReply method params = case method of
  "workspace/configuration" -> JArray [JNull | _ <- fromMaybe [] (key "items" params >>= asArray)]
  _ -> JNull

sendMessage :: Server -> Value -> IO ()
sendMessage server msg = atomically (writeTQueue (svQueue server) msg)

-- | Ask the server to shut down, then stop it.
stopServer :: Server -> IO ()
stopServer server = do
  sendMessage server (request (-2) "shutdown" JNull)
  sendMessage server (notification "exit" JNull)
  _ <- forkIO $ do
    threadDelay 500000
    void (try @SomeException (hClose (svInput server)))
    void (try @SomeException (terminateProcess (svProcess server)))
  pure ()

initializeParams :: Int -> FilePath -> Value
initializeParams pid root =
  object
    [ ("processId", JInt (fromIntegral pid))
    , ("clientInfo", object [("name", JString "him")])
    , ("rootUri", JString (pathToUri root))
    , ("rootPath", JString (T.pack root))
    , ("workspaceFolders", JArray [object [("uri", JString (pathToUri root)), ("name", JString (T.pack root))]])
    , ( "capabilities"
      , object
          [ ("general", object [("positionEncodings", JArray [JString "utf-8", JString "utf-16"])])
          , ( "textDocument"
            , object
                [ ("synchronization", object [("didSave", JBool True)])
                , ("publishDiagnostics", object [])
                , ("hover", object [("contentFormat", JArray [JString "plaintext", JString "markdown"])])
                , ("definition", object [])
                , ("references", object [])
                , ( "completion"
                  , object
                      [ ( "completionItem"
                        , object
                            [ ("snippetSupport", JBool False)
                            , -- Lets servers send imports and docs only for the item chosen.
                              ("resolveSupport", object [("properties", JArray [JString "additionalTextEdits", JString "detail", JString "documentation"])])
                            ]
                        )
                      ]
                  )
                ]
            )
          , ("workspace", object [("configuration", JBool True), ("workspaceFolders", JBool True)])
          ]
      )
    ]

serverInfo :: ServerConfig -> FilePath -> Value -> ServerInfo
serverInfo config root result =
  ServerInfo
    { siEncoding = case capability ["positionEncoding"] >>= asText of
        Just "utf-8" -> Utf8
        Just "utf-32" -> Utf32
        _ -> Utf16
    , siTriggers = strings ["completionProvider", "triggerCharacters"]
    , siSignatureTriggers = strings ["signatureHelpProvider", "triggerCharacters"] <> strings ["signatureHelpProvider", "retriggerCharacters"]
    , siSync = case capability ["textDocumentSync"] of
        Just v | Just n <- asInt v -> kind n
        Just v | Just n <- key "change" v >>= asInt -> kind n
        _ -> SyncFull
    , siName = T.pack (scCommand config)
    , siRoot = root
    , siCapabilities = fromMaybe JNull (key "capabilities" result)
    }
  where
    capability ks = path ("capabilities" : ks) result
    strings ks = [t | Just ts <- [capability ks >>= asArray], Just t <- map asText ts]
    kind = \case
      0 -> SyncNone
      2 -> SyncIncremental
      _ -> SyncFull
