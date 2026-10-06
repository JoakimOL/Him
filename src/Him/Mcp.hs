-- | An MCP server for the chat's tools (ADR claude-code-provider), so Claude Code can call
-- them. Claude Code starts MCP servers itself, over stdio, so the server
-- is a small bridge, @him --mcp-bridge DIR@, that forwards each tool call
-- to the running editor and waits for its answer. The editor answers an
-- edit only when the user approved or denied it, so nothing is written
-- before then.
--
-- The bridge and the editor talk through two named pipes in @DIR@ (the
-- boot libraries have no sockets): @calls@ (bridge to editor, one JSON
-- object per line: @id@, @name@, @arguments@) and @answers@ (editor to
-- bridge: @id@, @isError@, @text@). The editor holds both open (read-write,
-- see 'Him.Chat.ClaudeCode.serveToolPipes') before Claude Code starts, so
-- the bridge's opens never wait. Call ids carry the bridge's process id,
-- so a restarted bridge never takes an answer meant for the one before.
module Him.Mcp
  ( McpStep (..)
  , mcpStep
  , mcpTools
  , runBridge
  , runBridgeWith
  , callsPipe
  , answersPipe
  ) where

import Control.Concurrent (forkIO)
import Control.Concurrent.MVar (modifyMVar, modifyMVar_, newEmptyMVar, newMVar, putMVar, takeMVar, withMVar)
import Control.Exception (IOException, try)
import Control.Monad (forever, void)
import Data.ByteString qualified as BS
import Data.ByteString.Char8 qualified as BC
import Data.Map.Strict qualified as Map
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding (encodeUtf8)
import Him.Chat.Tools (chatTools)
import Him.Json
import System.FilePath ((</>))
import System.Posix.Process (getProcessID)
import System.IO (BufferMode (..), Handle, IOMode (..), hFlush, hSetBinaryMode, hSetBuffering, openFile, stdin, stdout)

callsPipe, answersPipe :: FilePath -> FilePath
callsPipe dir = dir </> "calls"
answersPipe dir = dir </> "answers"

-- | What to do with one message from the MCP client.
data McpStep
  = -- | Send this response.
    Reply !Value
  | -- | A tool call (the request's id, the tool, its arguments): ask the
    -- editor, then reply.
    Forward !Value !Text !Value
  | -- | A notification or a response: nothing to send.
    NoReply
  deriving stock (Eq, Show)

-- | The chat's tools in MCP's shape (@inputSchema@).
mcpTools :: [Value]
mcpTools = map toMcp chatTools
  where
    toMcp = \case
      JObject kvs -> JObject [(if k == "input_schema" then "inputSchema" else k, v) | (k, v) <- kvs]
      v -> v

-- | Handle one JSON-RPC message from the client (Claude Code).
mcpStep :: Value -> McpStep
mcpStep msg = case (key "method" msg >>= asText, key "id" msg) of
  (Just method, Just rid) -> case method of
    "initialize" ->
      Reply . result rid $
        object
          [ ("protocolVersion", fromMaybe (JString "2025-06-18") (path ["params", "protocolVersion"] msg))
          , ("capabilities", object [("tools", object [])])
          , ("serverInfo", object [("name", JString "him"), ("version", JString "0.1.0")])
          ]
    "ping" -> Reply (result rid (object []))
    "tools/list" -> Reply (result rid (object [("tools", JArray mcpTools)]))
    "tools/call" ->
      Forward rid (fromMaybe "" (path ["params", "name"] msg >>= asText)) (fromMaybe (object []) (path ["params", "arguments"] msg))
    _ -> Reply (object [("jsonrpc", JString "2.0"), ("id", rid), ("error", object [("code", JInt (-32601)), ("message", JString ("method not found: " <> method))])])
  _ -> NoReply
  where
    result rid r = object [("jsonrpc", JString "2.0"), ("id", rid), ("result", r)]

-- | The bridge: MCP on stdin/stdout, tool calls through the pipes in a
-- directory. Ends when the client closes stdin.
runBridge :: FilePath -> IO ()
runBridge = runBridgeWith stdin stdout

-- | The same with the client on other handles (the tests).
runBridgeWith :: Handle -> Handle -> FilePath -> IO ()
runBridgeWith input output dir = do
  mapM_ (`hSetBinaryMode` True) [input, output]
  hSetBuffering output (BlockBuffering Nothing)
  calls <- openFile (callsPipe dir) WriteMode
  answers <- openFile (answersPipe dir) ReadMode
  hSetBuffering calls LineBuffering
  waiting <- newMVar Map.empty
  outLock <- newMVar ()
  callLock <- newMVar ()
  counter <- newMVar (0 :: Int)
  pid <- getProcessID
  let send v = withMVar outLock $ \_ -> BS.hPut output (renderJson v <> "\n") >> hFlush output
  -- Answers from the editor go to the call waiting for them.
  _ <- forkIO . void . try @IOException . forever $ do
    line <- BC.hGetLine answers
    case parseJson line of
      Right v | Just i <- key "id" v >>= asText -> do
        waiter <- modifyMVar waiting (\m -> pure (Map.delete i m, Map.lookup i m))
        mapM_ (`putMVar` (fromMaybe False (key "isError" v >>= asBool), fromMaybe "" (key "text" v >>= asText))) waiter
      _ -> pure ()
  let loop =
        try @IOException (BC.hGetLine input) >>= \case
          Left _ -> pure ()
          Right line -> do
            case parseJson line of
              Left _ -> pure ()
              Right msg -> case mcpStep msg of
                Reply v -> send v
                NoReply -> pure ()
                Forward rid name args -> do
                  n <- modifyMVar counter (\c -> pure (c + 1, c + 1))
                  let i = T.pack (show pid <> "-" <> show n)
                  answer <- newEmptyMVar
                  modifyMVar_ waiting (pure . Map.insert i answer)
                  withMVar callLock $ \_ -> writeLine calls (object [("id", JString i), ("name", JString name), ("arguments", args)])
                  void . forkIO $ do
                    (isError, text) <- takeMVar answer
                    send (object [("jsonrpc", JString "2.0"), ("id", rid), ("result", object [("content", JArray [object [("type", JString "text"), ("text", JString text)]]), ("isError", JBool isError)])])
            loop
  loop

writeLine :: Handle -> Value -> IO ()
writeLine h v = BS.hPut h (renderJson v <> encodeUtf8 "\n") >> hFlush h
