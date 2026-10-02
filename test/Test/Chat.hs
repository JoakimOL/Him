-- | The AI chat: the Claude API's event stream (recorded, not live), the
-- request, edits, and the whole flow through the keys with a scripted
-- provider. Nothing here talks to a real model.
module Test.Chat
  ( chatTests
  ) where

import Control.Exception (finally)
import Control.Monad (when)
import Control.Monad.Trans.State.Strict (execStateT)
import Data.ByteString.Char8 qualified as BC
import Data.Foldable (foldlM)
import Data.IORef (atomicModifyIORef', modifyIORef', newIORef, readIORef, writeIORef)
import Data.List (find, mapAccumL)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.IO qualified as TIO
import Him.Buffer qualified as B
import Him.Chat
import Him.Chat.Anthropic (newStream, requestBody, streamEnd, streamStep)
import Him.Chat.ClaudeCode (claudeArgs, claudeEvent, serveToolPipes)
import Him.Mcp (McpStep (..), answersPipe, callsPipe, mcpStep, runBridgeWith)
import Control.Concurrent (forkIO)
import Control.Concurrent.MVar (newEmptyMVar, putMVar, readMVar, takeMVar)
import System.IO (hFlush)
import System.Posix.Files (createNamedPipe, ownerReadMode, ownerWriteMode, unionFileModes)
import System.Process (createPipe)
import System.Timeout (timeout)
import Him.Chat.Tools
import Him.Config (Config (..))
import Him.Config.Default (defaultConfig)
import Him.Document
import Him.Editor
import Him.Event (Event (..))
import Him.Json
import Him.Key (parseKeys)
import Him.Session (handleEvent)
import System.Directory (createDirectoryIfMissing, doesDirectoryExist, getCurrentDirectory, getTemporaryDirectory, removeDirectoryRecursive, setCurrentDirectory)
import Test.Harness
import Test.Util

chatTests :: IO [Test]
chatTests = do
  flow <- flowTests
  bridge <- bridgeTests
  live <- liveTests
  pure (pureTests <> claudeCodeTests <> flow <> bridge <> live)

-- | Claude Code's output (recorded shapes) and the MCP messages.
claudeCodeTests :: [Test]
claudeCodeTests =
  [ test "Claude Code's stream: text as it comes, its own tools noted, the result ends the turn" $
      let ev t = object [("type", JString "stream_event"), ("event", t)]
          textStart = ev (object [("type", JString "content_block_start"), ("content_block", object [("type", JString "text")])])
          delta t = ev (object [("type", JString "content_block_delta"), ("delta", object [("type", JString "text_delta"), ("text", JString t)])])
          toolStart n = ev (object [("type", JString "content_block_start"), ("content_block", object [("type", JString "tool_use"), ("name", JString n)])])
          lines' = [textStart, delta "Hi", toolStart "Grep", toolStart "mcp__him__read_file", textStart, delta "Done", object [("type", JString "result"), ("subtype", JString "success")]]
          (events, _) = foldl (\(acc, seen) v -> let (es, seen') = claudeEvent v seen in (acc <> es, seen')) ([], False) lines'
       in assertEqual
            [ChatText "Hi", ChatText "\n[Grep]\n", ChatText "\n\n", ChatText "Done", ChatFinished "end_turn" (object [("role", JString "assistant"), ("content", JArray [])]) []]
            events
  , test "a failed Claude Code turn fails the chat's turn" $
      assertEqual [ChatFailed "rate limited"] (fst (claudeEvent (object [("type", JString "result"), ("is_error", JBool True), ("result", JString "rate limited")]) True))
  , test "claude runs with him's tools only, read-only search, and nothing that asks" $
      let args = claudeArgs "/bin/him" "/tmp/x" defaultChatConfig "sys" (Just "abc")
          after flag = case dropWhile (/= flag) args of _ : v : _ -> Just v; _ -> Nothing
       in assertEqual
            (Just "Grep,Glob", Just "dontAsk", True, Just "abc", True)
            (after "--tools", after "--permission-mode", "--strict-mcp-config" `elem` args, after "--resume", maybe False ("mcp__him__edit_file" `T.isInfixOf`) (T.pack <$> after "--allowedTools"))
  , test "MCP: initialize, tools/list in MCP's shape, tools/call forwarded, unknown methods, notifications" $
      let req m ps = object [("jsonrpc", JString "2.0"), ("id", JInt 1), ("method", JString m), ("params", ps)]
          initReply = mcpStep (req "initialize" (object [("protocolVersion", JString "2025-06-18")]))
          listed = case mcpStep (req "tools/list" (object [])) of
            Reply r -> path ["result", "tools"] r >>= asArray
            _ -> Nothing
       in assertEqual
            ( Just "2025-06-18"
            , Just ["read_file", "list_files", "edit_file", "write_file"]
            , True
            , Forward (JInt 1) "edit_file" (object [("path", JString "a")])
            , Just (-32601)
            , NoReply
            )
            ( case initReply of Reply r -> path ["result", "protocolVersion"] r >>= asText; _ -> Nothing
            , map (\t -> fromMaybe "" (key "name" t >>= asText)) <$> listed
            , maybe False (all (\t -> key "inputSchema" t /= Nothing)) listed
            , mcpStep (req "tools/call" (object [("name", JString "edit_file"), ("arguments", object [("path", JString "a")])]))
            , case mcpStep (req "resources/list" (object [])) of Reply r -> path ["error", "code"] r >>= asInt; _ -> Nothing
            , mcpStep (object [("jsonrpc", JString "2.0"), ("method", JString "notifications/initialized")])
            )
  ]

-- | A client talks MCP to the bridge; the call reaches the editor's end
-- of the pipes, and the editor's answer comes back as the MCP result.
bridgeTests :: IO [Test]
bridgeTests = do
  tmp <- getTemporaryDirectory
  let dir = tmp <> "/him-mcp-test"
  -- A run that failed may have left its pipes behind.
  exists <- doesDirectoryExist dir
  when exists (removeDirectoryRecursive dir)
  createDirectoryIfMissing True dir
  mapM_ (\p -> createNamedPipe p (unionFileModes ownerReadMode ownerWriteMode)) [callsPipe dir, answersPipe dir]
  answerRef <- newEmptyMVar
  received <- newEmptyMVar
  (answer, stop) <- serveToolPipes dir (\call -> putMVar received call >> readMVar answerRef >>= \f -> f (tcId call) False "done")
  putMVar answerRef answer
  -- Two pipes, each (read end, write end): client to bridge, bridge to client.
  (bridgeReads, clientWrites) <- createPipe
  (clientReads, bridgeWrites) <- createPipe
  _ <- forkIO (runBridgeWith bridgeReads bridgeWrites dir)
  let send v = BC.hPutStrLn clientWrites (renderJson v) >> hFlush clientWrites
  send (object [("jsonrpc", JString "2.0"), ("id", JInt 7), ("method", JString "tools/call"), ("params", object [("name", JString "read_file"), ("arguments", object [("path", JString "a.txt")])])])
  call <- timeout 3000000 (takeMVar received)
  reply <- timeout 3000000 (BC.hGetLine clientReads)
  stop
  removeDirectoryRecursive dir
  let parsed = either (const Nothing) Just . parseJson =<< reply
  pure
    [ test "a tool call goes from the MCP client through the bridge to the editor" $
        assertEqual (Just ("read_file", Right (object [("path", JString "a.txt")]))) ((\c -> (tcName c, tcInput c)) <$> call)
    , test "the editor's answer comes back as the MCP result, with the request's id" $
        assertEqual (Just (JInt 7), Just "done", Just False)
          (parsed >>= key "id", parsed >>= path ["result", "content"] >>= asArray >>= \cs -> case cs of c : _ -> key "text" c >>= asText; [] -> Nothing, parsed >>= path ["result", "isError"] >>= asBool)
    ]

-- | The live flow (Claude Code): the model waits for each edit; approving
-- answers it, and the turn ends when the model says so.
liveTests :: IO [Test]
liveTests = do
  tmp <- getTemporaryDirectory
  original <- getCurrentDirectory
  let dir = tmp <> "/him-chat-live-test"
  createDirectoryIfMissing True dir
  TIO.writeFile (dir <> "/a.txt") "one\ntwo\n"
  answers <- newIORef []
  emitRef <- newIORef (\_ -> pure ())
  defaults <- either (fail . show) pure defaultConfig
  let editCall = ToolCall "c1" "edit_file" (Right (object [("path", JString "a.txt"), ("old_text", JString "two"), ("new_text", JString "TWO")]))
      fake = ChatProvider "live" . pure $ ChatSession
        { sessSend = \_ _ emit -> do
            writeIORef emitRef emit
            emit (ChatText "Editing.")
            emit (ChatToolCall editCall)
            pure (pure ())
        , sessAnswer = \i isError text -> do
            modifyIORef' answers (<> [(i, isError, text)])
            emit <- readIORef emitRef
            emit (ChatText "Thanks.")
            emit (ChatFinished "end_turn" (object [("role", JString "assistant"), ("content", JArray [])]) [])
        , sessClose = pure ()
        }
      config = defaults {cfgChatProviders = [fake], cfgChat = defaultChatConfig {ccProvider = "live"}}
      typeKeys ks ed = foldlM (\e k -> execStateT (handleEvent config (EvKey k)) e) ed (fromMaybe (error ("bad keys: " <> show ks)) (parseKeys ks))
      transcript ed = maybe "" (B.toText . docBuffer) (find (\d -> case docKind d of ChatDoc _ -> True; _ -> False) (allDocuments ed))
  (got, disk, done) <-
    ( do
        setCurrentDirectory dir
        rt <- testRuntime config
        let settleLive = settleUntil config rt 3000
        start <- (\t -> newDocument (Just "a.txt") (buf (fromMaybe t (T.stripSuffix "\n" t)))) <$> TIO.readFile "a.txt"
        asked <- settleLive (T.isInfixOf "[edit #1" . transcript) =<< typeKeys "space c c h i ret" (newEditor (24, 100) start)
        finished <- settleLive (T.isInfixOf "Thanks." . transcript) =<< typeKeys "esc space c a" asked
        (,,) <$> readIORef answers <*> TIO.readFile "a.txt" <*> pure finished
    )
      `finally` setCurrentDirectory original
  removeDirectoryRecursive dir
  pure
    [ test "a live edit is answered when it is approved, and saved first" $
        assertEqual ([("c1", False, "The user approved the edit; a.txt is saved.")], "one\nTWO\n") (got, disk)
    , test "after the decision the model goes on in the same turn" (assertEqual True ("Thanks." `T.isInfixOf` transcript done))
    ]

-- | A recorded stream: thinking (with its signature), text, a tool call
-- whose input arrives in pieces.
recorded :: [Text]
recorded =
  [ "event: message_start"
  , "data: {\"type\":\"message_start\",\"message\":{\"id\":\"msg_1\",\"role\":\"assistant\",\"content\":[]}}"
  , ""
  , "data: {\"type\":\"content_block_start\",\"index\":0,\"content_block\":{\"type\":\"thinking\",\"thinking\":\"\"}}"
  , "data: {\"type\":\"content_block_delta\",\"index\":0,\"delta\":{\"type\":\"signature_delta\",\"signature\":\"sig\"}}"
  , "data: {\"type\":\"content_block_stop\",\"index\":0}"
  , "data: {\"type\":\"content_block_start\",\"index\":1,\"content_block\":{\"type\":\"text\",\"text\":\"\"}}"
  , "data: {\"type\":\"content_block_delta\",\"index\":1,\"delta\":{\"type\":\"text_delta\",\"text\":\"Hel\"}}"
  , "data: {\"type\":\"content_block_delta\",\"index\":1,\"delta\":{\"type\":\"text_delta\",\"text\":\"lo\"}}"
  , "data: {\"type\":\"content_block_start\",\"index\":2,\"content_block\":{\"type\":\"tool_use\",\"id\":\"toolu_1\",\"name\":\"read_file\",\"input\":{}}}"
  , "data: {\"type\":\"content_block_delta\",\"index\":2,\"delta\":{\"type\":\"input_json_delta\",\"partial_json\":\"{\\\"path\\\": \"}}"
  , "data: {\"type\":\"content_block_delta\",\"index\":2,\"delta\":{\"type\":\"input_json_delta\",\"partial_json\":\"\\\"a.txt\\\"}\"}}"
  , "data: {\"type\":\"message_delta\",\"delta\":{\"stop_reason\":\"tool_use\"}}"
  , "data: {\"type\":\"message_stop\"}"
  ]

feed :: [Text] -> ([ChatEvent], Maybe ChatEvent)
feed ls = let (st, evs) = mapAccumL (\s l -> let (e, s') = streamStep l s in (s', e)) newStream ls in (concat evs, streamEnd st)

pureTests :: [Test]
pureTests =
  [ test "the stream gives the text as it comes, then the message and its tool calls" $
      let (events, end) = feed recorded
          expected =
            object
              [ ("role", JString "assistant")
              , ( "content"
                , JArray
                    [ object [("type", JString "thinking"), ("thinking", JString ""), ("signature", JString "sig")]
                    , object [("type", JString "text"), ("text", JString "Hello")]
                    , object [("type", JString "tool_use"), ("id", JString "toolu_1"), ("name", JString "read_file"), ("input", object [("path", JString "a.txt")])]
                    ]
                )
              ]
       in assertEqual ([ChatText "Hel", ChatText "lo"], Just (ChatFinished "tool_use" expected [ToolCall "toolu_1" "read_file" (Right (object [("path", JString "a.txt")]))])) (events, end)
  , test "a tool input that is not valid JSON is kept raw (and {} in the history)" $
      let ls = take 9 recorded <> [ "data: {\"type\":\"content_block_start\",\"index\":2,\"content_block\":{\"type\":\"tool_use\",\"id\":\"t\",\"name\":\"read_file\",\"input\":{}}}", "data: {\"type\":\"content_block_delta\",\"index\":2,\"delta\":{\"type\":\"input_json_delta\",\"partial_json\":\"{\\\"path\\\": \\\"a\"}}", "data: {\"type\":\"message_stop\"}"]
       in case snd (feed ls) of
            Just (ChatFinished _ _ [call]) ->
              assertEqual (Left "{\"path\": \"a", Left (T.pack (BC.unpack (renderJson (object [("INVALID_JSON", JString "{\"path\": \"a")])))))
                (tcInput call, parseToolCall call >> Right ())
            other -> Left ("unexpected: " <> show other)
  , test "an error event or a plain error reply fails the request" $
      assertEqual
        [Just (ChatFailed "Overloaded"), Just (ChatFailed "invalid x-api-key")]
        [ snd (feed ["event: error", "data: {\"type\":\"error\",\"error\":{\"type\":\"overloaded_error\",\"message\":\"Overloaded\"}}"])
        , snd (feed ["{\"type\":\"error\",\"error\":{\"type\":\"authentication_error\",\"message\":\"invalid x-api-key\"}}"])
        ]
  , test "the request streams, with adaptive thinking, the effort, the fallback and eager tool input" $
      let body = requestBody defaultChatConfig (ChatRequest "sys" [] chatTools)
          firstTool = key "tools" body >>= asArray >>= \ts -> case ts of t : _ -> Just t; [] -> Nothing
       in assertEqual
            (Just "claude-opus-5-5", Just True, Just "adaptive", Just "high", Just "default", Just True)
            ( key "model" body >>= asText
            , key "stream" body >>= asBool
            , path ["thinking", "type"] body >>= asText
            , path ["output_config", "effort"] body >>= asText
            , key "fallbacks" body >>= asText
            , firstTool >>= key "eager_input_streaming" >>= asBool
            )
  , test "an edit replaces whole lines, and denying it puts them back" $
      let d = newDocument (Just "a.hs") (buf "x = 1\ny = 2\nz = 3")
       in case proposeEdit 1 "t" "a.hs" "2\nz" "20\nzz" d of
            Right (pe, d') ->
              assertEqual
                ("x = 1\ny = 20\nzz = 3", 1, ["y = 2", "z = 3"], ["y = 20", "zz = 3"], "x = 1\ny = 2\nz = 3")
                (B.toText (docBuffer d'), peLine pe, peOld pe, peNew pe, B.toText (docBuffer (revertEdit pe d')))
            Left e -> Left (T.unpack e)
  , test "an edit needs its text exactly once" $
      let d = newDocument (Just "a.hs") (buf "a a")
       in assertEqual [Left "old_text was not found in a.hs", Left "old_text occurs more than once in a.hs; include more context"]
            [() <$ proposeEdit 1 "t" "a.hs" "b" "c" d, () <$ proposeEdit 1 "t" "a.hs" "a" "c" d]
  ]

-- | The plugin end to end, with a provider that plays back scripted
-- replies and records the requests.
flowTests :: IO [Test]
flowTests = do
  tmp <- getTemporaryDirectory
  original <- getCurrentDirectory
  let dir = tmp <> "/him-chat-test"
  createDirectoryIfMissing True dir
  TIO.writeFile (dir <> "/a.txt") "one\ntwo\n"
  sent <- newIORef []
  script <- newIORef []
  defaults <- either (fail . show) pure defaultConfig
  let fake = ChatProvider "fake" . pure $ ChatSession
        { sessSend = \_ req emit -> do
            modifyIORef' sent (<> [req])
            reply <- atomicModifyIORef' script (\s -> (drop 1 s, take 1 s))
            mapM_ (mapM_ emit) reply
            pure (pure ())
        , sessAnswer = \_ _ _ -> pure ()
        , sessClose = pure ()
        }
      config = defaults {cfgChatProviders = [fake], cfgChat = defaultChatConfig {ccProvider = "fake"}}
      typeKeys ks ed = foldlM (\e k -> execStateT (handleEvent config (EvKey k)) e) ed (fromMaybe (error ("bad keys: " <> show ks)) (parseKeys ks))
      assistant calls = object [("role", JString "assistant"), ("content", JArray [object [("type", JString "tool_use"), ("id", JString c), ("name", JString "x"), ("input", object [])] | c <- calls])]
      editCall = ToolCall "t1" "edit_file" (Right (object [("path", JString "a.txt"), ("old_text", JString "two"), ("new_text", JString "TWO")]))
      readCall = ToolCall "t2" "read_file" (Right (object [("path", JString "a.txt")]))
      firstReply = [ChatText "Changing it.", ChatFinished "tool_use" (assistant ["t1", "t2"]) [editCall, readCall]]
      secondReply = [ChatText "Done.", ChatFinished "end_turn" (object [("role", JString "assistant"), ("content", JArray [])]) []]
      transcript ed = maybe "" (B.toText . docBuffer) (find isChatDoc (allDocuments ed))
      isChatDoc d = case docKind d of
        ChatDoc _ -> True
        _ -> False
      fileText ed = maybe "" (B.toText . docBuffer) (find ((== Just "a.txt") . docPath) (allDocuments ed))
  (approved, approvedRequests, onDisk, pendingShown, deniedText, deniedDisk, deniedResult) <-
    ( do
        setCurrentDirectory dir
        rt <- testRuntime config
        let settleChat = settleUntil config rt 3000
        start <- either (fail . T.unpack) pure =<< (fmap (\d -> d {docPath = Just "a.txt"}) <$> loadDocumentText "a.txt")
        -- Approve: the edit is kept and saved; the conversation goes on.
        modifyIORef' script (const [firstReply, secondReply])
        asked <- settleChat (T.isInfixOf "[edit #1" . transcript) =<< typeKeys "space c c h i ret" (newEditor (24, 100) start)
        let shown = maybe [] (pendingEditLines asked . docId) (find ((== Just "a.txt") . docPath) (allDocuments asked))
        done <- settleChat (T.isInfixOf "Done." . transcript) =<< typeKeys "esc space c a" asked
        requests <- readIORef sent
        disk <- TIO.readFile "a.txt"
        -- Deny (a fresh start): the old line comes back, the file stays.
        TIO.writeFile "a.txt" "one\ntwo\n"
        modifyIORef' sent (const [])
        modifyIORef' script (const [firstReply, secondReply])
        asked2 <- settleChat (T.isInfixOf "[edit #1" . transcript) =<< typeKeys "space c c h i ret" (newEditor (24, 100) start)
        denied <- settleChat (T.isInfixOf "Done." . transcript) =<< typeKeys "esc space c d" asked2
        disk2 <- TIO.readFile "a.txt"
        requests2 <- readIORef sent
        pure (done, requests, disk, shown, fileText denied, disk2, requests2)
    )
      `finally` setCurrentDirectory original
  removeDirectoryRecursive dir
  let lastResults reqs = case reverse (concatMap crMessages (drop 1 reqs)) of
        m : _ -> key "content" m >>= asArray
        [] -> Nothing
      resultIds reqs = map (\r -> key "tool_use_id" r >>= asText) (fromMaybe [] (lastResults reqs))
  pure
    [ test "an edit shows in the editor, highlighted, until it is decided" (assertEqual [(1, 2)] pendingShown)
    , test "approving keeps and saves the edit, and the conversation goes on" $
        assertEqual ("one\nTWO\n", True, 2) (onDisk, "Done." `T.isInfixOf` transcript approved, length approvedRequests)
    , test "the tool results go back in the order of the calls, after the decision" $
        assertEqual [Just "t1", Just "t2"] (resultIds approvedRequests)
    , test "the history is only appended to" $
        case approvedRequests of
          [first, second] -> assertEqual True (crMessages first `isPrefixOfList` crMessages second)
          _ -> Left "expected two requests"
    , test "denying puts the old line back and leaves the file alone" $
        assertEqual ("one\ntwo", "one\ntwo\n") (deniedText, deniedDisk)
    , test "the model is told an edit was rejected" $
        assertEqual True (any ("rejected" `T.isInfixOf`) [t | Just rs <- [lastResults deniedResult], r <- rs, Just t <- [key "content" r >>= asText]])
    ]
  where
    isPrefixOfList a b = take (length a) b == a
    loadDocumentText p = do
      t <- TIO.readFile p
      pure (Right (newDocument (Just p) (buf (fromMaybe t (T.stripSuffix "\n" t)))))
