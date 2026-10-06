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
import Him.Chat.Transcript
import Data.IntMap.Strict qualified as IntMap
import Him.Diff (diffLines)
import Him.Review (DisplayRow (..), approveHunk, approveOnto, denyHunk, displayRows, hunkAtLine, rowOfLine)
import Him.Selection (primary, rangeHead)
import Him.Position (Pos (..))
import Him.Config (Config (..))
import Him.Config.Default (defaultConfig)
import Him.Document
import Him.Editor
import Him.Event (Event (..))
import Him.Json
import Him.Key (parseKeys)
import Him.Mode (Mode (..))
import Him.Session (handleEvent)
import System.Directory (createDirectoryIfMissing, doesDirectoryExist, getCurrentDirectory, getTemporaryDirectory, removeDirectoryRecursive, setCurrentDirectory)
import Test.Harness
import Test.Util

chatTests :: IO [Test]
chatTests = do
  flow <- flowTests
  bridge <- bridgeTests
  live <- liveTests
  pure (pureTests <> transcriptTests <> claudeCodeTests <> flow <> bridge <> live)

-- | The chat buffer's layout (ADR chat-panel).
transcriptTests :: [Test]
transcriptTests =
  [ test "a long line is wrapped at spaces, list items under their text" $
      assertEqual
        (["one two three", "four five"], ["- alpha beta", "  gamma"], ["abcdefghij", "klm"])
        (wrapLine 13 "one two three four five", wrapLine 12 "- alpha beta gamma", wrapLine 10 "abcdefghijklm")
  , test "the model's text goes above the prompt, wrapped as it streams, code blocks kept whole and marked" $
      let d = foldl (\doc t -> appendModel 12 t doc) newChatDocument ["Here is ", "some code for you:\n```\nlet x = a very long line\n```\nok"]
          marks = maybe [] (IntMap.toList . csMarks) (chatState d)
       in assertEqual
            ( ["Here is some", "code for", "you:", "```", "let x = a very long line", "```", "ok", chatPrompt]
            , [(3, MarkFence), (4, MarkCode), (5, MarkFence)]
            , fmap csInput (chatState d)
            )
            (B.toLines (docBuffer d), marks, Just (Pos 7 (T.length chatPrompt)))
  , test "lines of the editor's own start on a new line, a gap is one blank line" $
      let d = appendLines 40 MarkTool ["◦ Read a.txt"] (appendGap 40 (appendModel 40 "Text" newChatDocument))
       in assertEqual (["Text", "", "◦ Read a.txt", "", chatPrompt], Just MarkTool) (B.toLines (docBuffer d), chatState d >>= IntMap.lookup 2 . csMarks)
  , test "typing waits below output: the cursor in the input moves down with it" $
      let typed = (setMessage "hi" newChatDocument)
          d = appendModel 40 "one\ntwo\n" typed
       in assertEqual (Pos 3 (T.length chatPrompt + 2), Just "hi") (rangeHead (primary (docSelection d)), fst <$> takeMessage d)
  , test "taking the message empties the input" $
      case takeMessage (setMessage "a\nb" newChatDocument) of
        Just (t, d) -> assertEqual ("a\nb", [ "", chatPrompt]) (t, B.toLines (docBuffer d))
        Nothing -> Left "no message"
  , test "inline code, bold and headings in prose" $
      assertEqual
        ([(4, 9, InlineCode), (10, 17, InlineBold)], [(0, 5, InlineHeading)], [])
        (inlineSpans "use `foo` **bar** x", inlineSpans "# Hi!", inlineSpans "a * b ` c")
  , test "the code block under the cursor, else the last one above it" $
      let d = appendModel 40 "```\nA\nB\n```\ntext\n```\nC\n```\nend\n" newChatDocument
       in assertEqual (Just "A\nB", Just "A\nB", Just "C") (codeBlockAt 2 d, codeBlockAt 4 d, codeBlockAt maxBound d)
  ]

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
            [ChatText "Hi", ChatActivity "Searched the code", ChatText "\n\n", ChatText "Done", ChatFinished "end_turn" (object [("role", JString "assistant"), ("content", JArray [])]) []]
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

-- | The live flow (Claude Code): a tool call during the turn is answered
-- at once (an edit as proposed), and the turn goes on.
liveTests :: IO [Test]
liveTests = do
  tmp <- getTemporaryDirectory
  original <- getCurrentDirectory
  let dir = tmp <> "/him-chat-live-test"
  createDirectoryIfMissing True dir
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
  (got, finished, diskBefore, diskAfter) <-
    ( do
        setCurrentDirectory dir
        TIO.writeFile "a.txt" "one\ntwo\n"
        rt <- testRuntime config
        let settleLive = settleUntil config rt 3000
        start <- (\t -> newDocument (Just "a.txt") (buf (fromMaybe t (T.stripSuffix "\n" t)))) <$> TIO.readFile "a.txt"
        finished <- settleLive (T.isInfixOf "1 proposed change" . transcript) =<< typeKeys "space c c h i ret" (newEditor (24, 100) start)
        before <- TIO.readFile "a.txt"
        _ <- typeKeys "space c a" finished
        (,,,) <$> readIORef answers <*> pure finished <*> pure before <*> TIO.readFile "a.txt"
    )
      `finally` setCurrentDirectory original
  removeDirectoryRecursive dir
  pure
    [ test "a live edit is answered at once as proposed, and the turn goes on" $
        assertEqual ([("c1", False)], True) ([(i, e) | (i, e, _) <- got], "Thanks." `T.isInfixOf` transcript finished)
    , test "a live change is written only once approved" (assertEqual ("one\ntwo\n", "one\nTWO\n") (diskBefore, diskAfter))
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
  , test "edit_file replaces one exact occurrence; it must occur exactly once" $
      let b = buf "x = 1\ny = 2\nz = 3"
       in assertEqual
            [Right "x = 1\ny = 20\nzz = 3", Left "old_text was not found in a.hs", Left "old_text occurs more than once in a.hs; include more context"]
            (map (fmap B.toText) [editBuffer "a.hs" "2\nz" "20\nzz" b, editBuffer "a.hs" "w" "v" b, editBuffer "a.hs" " = " "=" b])
  , test "approving a change puts it in the base; denying puts the base's lines back in the buffer" $
      let base = ["a", "b", "c", "d"]
          current = ["a", "B", "c", "D", "e"]
       in case diffLines base current of
            h : rest ->
              assertEqual (1, ["a", "B", "c", "d"], ["a", "b", "c", "D", "e"])
                (length rest, approveHunk h base current, denyHunk h base current)
            [] -> Left "expected changes"
  , test "approving writes only the chat's change; the user's own unsaved edits stay unsaved" $
      -- On disk "b"; the user changed it to "USER" (unsaved) before the
      -- chat changed "d" to "CHAT".
      let disk = ["a", "b", "c", "d"]
          base = ["a", "USER", "c", "d"]
          current = ["a", "USER", "c", "CHAT"]
       in case diffLines base current of
            [h] -> assertEqual (Just ["a", "b", "c", "CHAT"], Just ["a", "USER", "c", "CHAT"]) (approveOnto h base current disk, approveOnto h base current base)
            _ -> Left "expected one change"
  , test "a chat change overlapping the user's own unsaved edit is not approved" $
      let disk = ["a", "b", "c"]
          base = ["a", "USER", "c"]
          current = ["a", "CHAT", "c"]
       in case diffLines base current of
            [h] -> assertEqual Nothing (approveOnto h base current disk)
            _ -> Left "expected one change"
  , test "the change at a line: its added lines, or for a removal the line after it" $
      let changed = diffLines ["a", "b", "c"] ["a", "X", "c"]
          removed = diffLines ["a", "b", "c"] ["a", "c"]
          found hunks l = fst <$> hunkAtLine 3 l hunks
       in assertEqual [Just 0, Nothing, Just 0, Nothing] [found changed 1, found changed 0, found removed 1, found removed 0]
  , test "a review shows each change's header and removed lines above its new lines" $
      case diffLines ["a", "b", "c"] ["a", "X", "c"] of
        hunks@(h : _) ->
          let rows = displayRows (Just (Review 1 "a.txt" ["a", "b", "c"] hunks 0)) 3 0 6
           in assertEqual ([LineRow 0, HeaderRow 1 1 h, RemovedRow "b", LineRow 1, LineRow 2, EmptyRow], Just 3) (rows, rowOfLine rows 1)
        [] -> Left "expected a change"
  ]

-- | The plugin end to end, with a provider that plays back scripted
-- replies (the API provider's way: tool calls with the reply) and records
-- the requests.
flowTests :: IO [Test]
flowTests = do
  tmp <- getTemporaryDirectory
  original <- getCurrentDirectory
  let dir = tmp <> "/him-chat-test"
  createDirectoryIfMissing True dir
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
      edit c old new = ToolCall c "edit_file" (Right (object [("path", JString "a.txt"), ("old_text", JString old), ("new_text", JString new)]))
      readCall = ToolCall "r" "read_file" (Right (object [("path", JString "a.txt")]))
      -- One turn proposes two changes (and reads the file), then ends.
      proposing = [ChatText "Two changes.", ChatFinished "tool_use" (assistant ["e1", "e2", "r"]) [edit "e1" "two" "TWO", edit "e2" "four" "FOUR", readCall]]
      done = [ChatText "Done.", ChatFinished "end_turn" (object [("role", JString "assistant"), ("content", JArray [])]) []]
      transcript ed = maybe "" (B.toText . docBuffer) (find isChatDoc (allDocuments ed))
      isChatDoc d = case docKind d of
        ChatDoc _ -> True
        _ -> False
      fileDoc ed = find ((== Just "a.txt") . docPath) (allDocuments ed)
      fileText ed = maybe "" (B.toText . docBuffer) (fileDoc ed)
      reviewCount ed = maybe 0 (\d -> maybe 0 (length . rvHunks) (reviewFor ed (docId d))) (fileDoc ed)
      cursorLine ed = posLine (rangeHead (primary (docSelection (edDoc ed))))
  results <-
    ( do
        setCurrentDirectory dir
        TIO.writeFile "a.txt" "one\ntwo\nthree\nfour\n"
        rt <- testRuntime config
        let settleChat = settleUntil config rt 3000
        start <- (\t -> newDocument (Just "a.txt") (buf (fromMaybe t (T.stripSuffix "\n" t)))) <$> TIO.readFile "a.txt"
        modifyIORef' script (const [proposing, done, done])
        -- Both changes come in one turn; the turn ends with them under
        -- review, and the editor on the first one.
        proposed <- settleChat (T.isInfixOf "2 changes to review" . transcript) =<< typeKeys "space c c h i ret" (newEditor (24, 100) start)
        diskAfterProposing <- TIO.readFile "a.txt"
        -- Out of order: the second first (approve), then the first (deny).
        secondApproved <- typeKeys "] c space c a" proposed
        diskAfterApprove <- TIO.readFile "a.txt"
        firstDenied <- typeKeys "[ c space c d" secondApproved
        diskAfterDeny <- TIO.readFile "a.txt"
        -- The next message tells the model what became of them.
        finished <- settleChat (T.isInfixOf "Done." . T.takeEnd 40 . transcript) =<< typeKeys "space c c o k ret" firstDenied
        -- Earlier messages come back with up, newest first.
        recalled <- typeKeys "up up" finished
        requests <- readIORef sent
        pure (proposed, diskAfterProposing, secondApproved, diskAfterApprove, firstDenied, diskAfterDeny, requests, (finished, recalled))
    )
      `finally` setCurrentDirectory original
  removeDirectoryRecursive dir
  let (proposed, diskAfterProposing, secondApproved, diskAfterApprove, firstDenied, diskAfterDeny, requests, (finished, recalled)) = results
      chatDoc ed = find isChatDoc (allDocuments ed)
      markedLines m ed = case chatDoc ed of
        Just d | Just cs <- chatState d -> [B.lineAt l (docBuffer d) | (l, m') <- IntMap.toList (csMarks cs), m' == m]
        _ -> []
      messageText m = T.concat [t | Just cs <- [key "content" m >>= asArray], c <- cs, Just t <- [key "text" c >>= asText]]
      lastUser = case requests of
        [] -> ""
        _ -> messageText (last (crMessages (last requests)))
      toolResults = case requests of
        _ : second : _ -> [(key "tool_use_id" r >>= asText, key "content" r >>= asText) | Just rs <- [key "content" (last (crMessages second)) >>= asArray], r <- rs]
        _ -> []
  pure
    [ test "all of a turn's changes are proposed at once, and nothing is written" $
        assertEqual (2, "one\nTWO\nthree\nFOUR", "one\ntwo\nthree\nfour\n") (reviewCount proposed, fileText proposed, diskAfterProposing)
    , test "the turn goes on at once: every call is answered, edits as proposed" $
        assertEqual [Just "e1", Just "e2", Just "r"] (map fst toolResults)
    , test "after the turn the editor is on the first change, in normal mode" $
        assertEqual (Just "a.txt", 1, Normal) (docPath (edDoc proposed), cursorLine proposed, edMode proposed)
    , test "changes are decided in any order with the cursor on them: approving writes only that one" $
        assertEqual (1, "one\ntwo\nthree\nFOUR\n") (reviewCount secondApproved, diskAfterApprove)
    , test "denying puts the old lines back in the buffer and leaves the file" $
        assertEqual (0, "one\ntwo\nthree\nFOUR", "one\ntwo\nthree\nFOUR\n") (reviewCount firstDenied, fileText firstDenied, diskAfterDeny)
    , test "the transcript: your messages and the answers as blocks, what the model read and proposed" $
        assertEqual
          (["You", "You"], ["hi", "ok"], ["Claude", "Claude"], ["◦ Read a.txt"], ["✎ a.txt  +1 −1", "✎ a.txt  +1 −1"])
          (markedLines MarkUser finished, markedLines MarkUserText finished, markedLines MarkClaude finished, markedLines MarkTool finished, markedLines MarkChange finished)
    , test "up recalls the messages sent, the last first" $
        assertEqual (Just "hi") (fst <$> (chatDoc recalled >>= takeMessage))
    , test "the next message tells the model what was approved and rejected" $
        assertEqual (True, True) ("approved a.txt:4" `T.isInfixOf` lastUser, "rejected a.txt:2" `T.isInfixOf` lastUser)
    ]

