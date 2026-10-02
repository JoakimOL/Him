-- | The chat provider for Claude Code (ADR-42): the @claude@ program you
-- are logged in to, so no API key is needed. One @claude -p@ process per
-- chat keeps the conversation (messages go in as stream-json, events come
-- out as stream-json; a cancelled turn is picked up again with
-- @--resume@).
--
-- Claude Code's own editing and command tools are off: it gets only
-- read-only search (@Grep@, @Glob@) and him's tools, served over MCP by
-- the bridge ("Him.Mcp"). Its tool calls arrive here as 'ChatToolCall'
-- events, and Claude Code waits until the editor answers ('sessAnswer') -
-- for an edit, once the user approved or denied it.
module Him.Chat.ClaudeCode
  ( claudeCodeProvider
  , claudeArgs
  , claudeEvent
  , serveToolPipes
  ) where

import Control.Concurrent (forkIO, killThread)
import GHC.Clock (getMonotonicTimeNSec)
import Control.Concurrent.MVar (modifyMVar_, newMVar, readMVar, swapMVar, withMVar)
import Control.Exception (IOException, SomeException, try)
import Control.Monad (forever, void, when)
import Data.ByteString qualified as BS
import Data.ByteString.Char8 qualified as BC
import Data.IORef (newIORef, readIORef, writeIORef)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding (decodeUtf8Lenient)
import Him.Chat
import Him.Json
import Him.Mcp (answersPipe, callsPipe)
import System.Directory (createDirectoryIfMissing, getTemporaryDirectory, removeDirectoryRecursive)
import System.Environment (getExecutablePath)
import System.IO (Handle, IOMode (..), hClose, hFlush, hSetBinaryMode, openFile)
import System.Posix.Files (createNamedPipe, ownerReadMode, ownerWriteMode, unionFileModes)
import System.Posix.Process (getProcessID)
import System.Process (CreateProcess (..), ProcessHandle, StdStream (..), createProcess, proc, terminateProcess)

claudeCodeProvider :: ChatProvider
claudeCodeProvider = ChatProvider "claude-code" start

-- | A running @claude@: its input, the process, and the reader thread.
data Running = Running
  { rIn :: Handle
  , rProcess :: ProcessHandle
  }

start :: IO ChatSession
start = do
  tmp <- getTemporaryDirectory
  pid <- getProcessID
  -- A directory for this session's pipes, unique to the process and time.
  stamp <- getMonotonicTimeNSec
  let dir = tmp <> "/him-mcp-" <> show pid <> "-" <> show stamp
  createDirectoryIfMissing True dir
  mapM_ (\p -> createNamedPipe p (unionFileModes ownerReadMode ownerWriteMode)) [callsPipe dir, answersPipe dir]
  emitRef <- newIORef (\_ -> pure ())
  sessionId <- newIORef Nothing
  running <- newMVar Nothing
  inTurn <- newIORef False
  (answer, stopPipes) <- serveToolPipes dir (\call -> readIORef emitRef >>= \emit -> emit (ChatToolCall call))
  let stop = modifyMVar_ running $ \case
        Just r -> do
          void (try @SomeException (hClose (rIn r)))
          void (try @SomeException (terminateProcess (rProcess r)))
          pure Nothing
        Nothing -> pure Nothing
      send cc req emit = do
        writeIORef emitRef emit
        -- A conversation that starts over starts a new Claude Code session.
        when (length (crMessages req) <= 1) $ stop >> writeIORef sessionId Nothing
        writeIORef inTurn True
        r <- ensure cc req
        case r of
          Left e -> do
            writeIORef inTurn False
            emit (ChatFailed e)
            pure (pure ())
          Right h -> do
            let message = case reverse (crMessages req) of
                  m : _ -> fromMaybe (JString "") (key "content" m)
                  [] -> JString ""
            sent <- try @IOException (BS.hPut h (renderJson (object [("type", JString "user"), ("message", object [("role", JString "user"), ("content", message)])]) <> "\n") >> hFlush h)
            case sent of
              Left e -> writeIORef inTurn False >> emit (ChatFailed ("claude: " <> T.pack (show e)))
              Right () -> pure ()
            -- Cancelling ends the process; the next turn resumes the session.
            pure (writeIORef inTurn False >> stop)
      ensure cc req = do
        current <- readMVar running
        case current of
          Just r -> pure (Right (rIn r))
          Nothing -> do
            self <- getExecutablePath
            sid <- readIORef sessionId
            started <- try @IOException (createProcess (proc "claude" (claudeArgs self dir cc (crSystem req) sid)) {std_in = CreatePipe, std_out = CreatePipe, std_err = CreatePipe})
            case started of
              Left e -> pure (Left ("could not start claude: " <> T.pack (show e)))
              Right (Just hin, Just hout, Just herr, ph) -> do
                mapM_ (`hSetBinaryMode` True) [hin, hout, herr]
                _ <- swapMVar running (Just (Running hin ph))
                void . forkIO $ readOutput hout herr
                pure (Right hin)
              Right _ -> pure (Left "could not start claude")
      readOutput hout herr = do
        textSeen <- newIORef False
        let loop =
              try @IOException (BC.hGetLine hout) >>= \case
                Left _ -> pure ()
                Right line -> do
                  case parseJson line of
                    Right v -> do
                      mapM_ (writeIORef sessionId . Just) (key "session_id" v >>= asText)
                      seen <- readIORef textSeen
                      let (events, seen') = claudeEvent v seen
                      writeIORef textSeen seen'
                      emit <- readIORef emitRef
                      mapM_ (\ev -> finishing ev >> emit ev) events
                    Left _ -> pure ()
                  loop
            finishing = \case
              ChatFinished {} -> writeIORef inTurn False
              ChatFailed _ -> writeIORef inTurn False
              _ -> pure ()
        loop
        -- The process ended; a turn still running failed.
        err <- either (const "") decodeUtf8Lenient <$> try @IOException (BS.hGetContents herr)
        modifyMVar_ running (const (pure Nothing))
        still <- readIORef inTurn
        when still $ do
          writeIORef inTurn False
          emit <- readIORef emitRef
          emit (ChatFailed ("claude stopped" <> (if T.null (T.strip err) then "" else ": " <> T.strip (T.takeEnd 400 err))))
      close = do
        stop
        stopPipes
        void (try @IOException (removeDirectoryRecursive dir))
  pure (ChatSession send answer close)

-- | How @claude@ is started: printing stream-json, reading stream-json,
-- him's tools over MCP (and no other MCP servers), read-only search as its
-- only own tools, nothing else allowed (no prompts), the model and effort
-- from @[chat]@, and the session to resume.
claudeArgs :: FilePath -> FilePath -> ChatConfig -> Text -> Maybe Text -> [String]
claudeArgs self dir cc systemPrompt resume =
  [ "-p"
  , "--input-format"
  , "stream-json"
  , "--output-format"
  , "stream-json"
  , "--verbose"
  , "--include-partial-messages"
  , "--model"
  , T.unpack (ccModel cc)
  , "--effort"
  , T.unpack (ccEffort cc)
  , "--tools"
  , "Grep,Glob"
  , "--mcp-config"
  , T.unpack (decodeUtf8Lenient (renderJson mcpConfig))
  , "--strict-mcp-config"
  , "--allowedTools"
  , T.unpack (T.intercalate "," (["mcp__him__" <> t | t <- ["read_file", "list_files", "edit_file", "write_file"]] <> ["Grep", "Glob"]))
  , "--permission-mode"
  , "dontAsk"
  , "--append-system-prompt"
  , T.unpack (systemPrompt <> "Your tools for files are him's (mcp__him__*): use them, not others, to read and change files.")
  ]
    <> maybe [] (\s -> ["--resume", T.unpack s]) resume
  where
    mcpConfig =
      object [("mcpServers", object [("him", object [("command", JString (T.pack self)), ("args", JArray [JString "--mcp-bridge", JString (T.pack dir)])])])]

-- | One line of Claude Code's output: what to show, and whether text was
-- shown this turn (a new text block after a tool gets a blank line).
claudeEvent :: Value -> Bool -> ([ChatEvent], Bool)
claudeEvent v seen = case key "type" v >>= asText of
  Just "stream_event" | Just e <- key "event" v -> case key "type" e >>= asText of
    Just "content_block_start"
      | Just "text" <- path ["content_block", "type"] e >>= asText -> ([ChatText "\n\n" | seen], seen)
      | Just "tool_use" <- path ["content_block", "type"] e >>= asText
      , Just name <- path ["content_block", "name"] e >>= asText
      , not ("mcp__him__" `T.isPrefixOf` name) ->
          ([ChatText ("\n[" <> name <> "]\n")], seen)
    Just "content_block_delta"
      | Just "text_delta" <- path ["delta", "type"] e >>= asText
      , Just t <- path ["delta", "text"] e >>= asText ->
          ([ChatText t], True)
    _ -> ([], seen)
  Just "result"
    | (key "is_error" v >>= asBool) == Just True ->
        ([ChatFailed (fromMaybe (fromMaybe "error" (key "subtype" v >>= asText)) (key "result" v >>= asText))], False)
    | otherwise -> ([ChatFinished "end_turn" (object [("role", JString "assistant"), ("content", JArray [])]) []], False)
  _ -> ([], seen)

-- | The editor's end of the pipes: tool calls from the bridge go to
-- @onCall@ (as they come); the first action answers one, the second stops
-- serving.
--
-- Both pipes are opened read-write here, which on Linux never blocks: the
-- bridge's opens then always find this end, and when Claude Code restarts
-- the bridge, this end never sees the pipe close. (An answer the old
-- bridge never read is ignored by the next: its ids differ.)
serveToolPipes :: FilePath -> (ToolCall -> IO ()) -> IO (Text -> Bool -> Text -> IO (), IO ())
serveToolPipes dir onCall = do
  calls <- openFile (callsPipe dir) ReadWriteMode
  answers <- openFile (answersPipe dir) ReadWriteMode
  mapM_ (`hSetBinaryMode` True) [calls, answers]
  lock <- newMVar ()
  thread <- forkIO . void . try @IOException . forever $ do
    line <- BC.hGetLine calls
    case parseJson line of
      Right v | Just i <- key "id" v >>= asText, Just name <- key "name" v >>= asText ->
        onCall (ToolCall i name (Right (fromMaybe (object []) (key "arguments" v))))
      _ -> pure ()
  let answer i isError text =
        withMVar lock $ \_ ->
          void . try @IOException $ BS.hPut answers (renderJson (object [("id", JString i), ("isError", JBool isError), ("text", JString text)]) <> "\n") >> hFlush answers
      stop = do
        killThread thread
        mapM_ (void . try @IOException . hClose) [calls, answers]
  pure (answer, stop)
