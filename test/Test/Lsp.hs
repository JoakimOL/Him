-- | The LSP client: protocol, sync, edits, and a real server.
module Test.Lsp
  ( lspProtocolTests
  , syncTests
  , lspEditTests
  , lspTests
  ) where

import Control.Monad.Trans.State.Strict (execStateT)
import Data.ByteString qualified as BS
import Data.Foldable (foldlM)
import Data.Maybe (fromMaybe)
import Him.App (handleEvent)
import Him.Buffer qualified as B
import Him.Config.Default (defaultConfig)
import Him.Document
import Him.Editor
import Him.Event (Event (..))
import Him.File (loadDocument)
import System.Directory (findExecutable, createDirectoryIfMissing, getTemporaryDirectory)
import Him.Picker
import Him.Lsp.Protocol
import Him.Lsp.State (Attachment (..), Completion (..), DocLsp (..), ServerInfo (..), ShownDiagnostic (..), Sync (..), shownDiagnostics)
import Him.Lsp.Sync (syncMessages)
import Him.Lsp.Edit
import Data.Maybe (isJust)
import Him.Json hiding (path)
import Him.Json qualified as J
import Him.Key
import Him.Mode (Mode (..))
import Him.Position (Pos (..))
import Him.Selection
import Data.Foldable (toList)
import Data.Text qualified as T
import Test.Harness
import Test.Util

lspProtocolTests :: [Test]
lspProtocolTests =
  [ test "framing: messages split anywhere come out whole" $
      let msgs = [JObject [("id", JInt n), ("x", JString "æ漢\r\n")] | n <- [1 .. 3]]
          stream = BS.concat (map frameMessage msgs)
          feedAll = go emptyFramer
          go f (c : cs) = let (out, f') = feedFramer f c in out <> go f' cs
          go _ [] = []
          expected = map renderJson msgs
       in assertEqual [] [n | n <- [0 .. BS.length stream], let (a, z) = BS.splitAt n stream, feedAll [a, z] /= expected]
  , test "framing: byte by byte" $
      assertEqual [renderJson (JArray [])] (fst (foldl (\(out, f) b -> let (o, f') = feedFramer f (BS.singleton b) in (out <> o, f')) ([], emptyFramer) (BS.unpack (frameMessage (JArray [])))))
  , test "framing: header case and extra headers" $
      assertEqual (["{}"], emptyFramer) (feedFramer emptyFramer "content-length: 2\r\nContent-Type: x\r\n\r\n{}")
  , test "URIs round-trip with odd characters" $
      let p = "/tmp/a b/æ#%.hs" in assertEqual (Just p, "file:///tmp/a%20b/%C3%A6%23%25.hs") (uriToPath (pathToUri p), pathToUri p)
  , test "columns in UTF-8, UTF-16 and UTF-32" $
      let line = "aé😀b"
       in assertEqual ([1, 3, 7], [1, 2, 4], [1, 2, 3], 3) (map (toLspColumn Utf8 line) [1, 2, 3], map (toLspColumn Utf16 line) [1, 2, 3], map (toLspColumn Utf32 line) [1, 2, 3], fromLspColumn Utf16 line 4)
  , test "classifying messages" $
      assertEqual
        [Reply 1 (Right JNull), Reply 2 (Left "bad"), ServerRequest (JInt 7) "workspace/configuration" JNull, Notification "x" JNull]
        (map classify [JObject [("id", JInt 1), ("result", JNull)], JObject [("id", JInt 2), ("error", JObject [("message", JString "bad")])], JObject [("id", JInt 7), ("method", JString "workspace/configuration")], JObject [("method", JString "x")]])
  , test "diagnostics" $
      assertEqual
        (Just ("/a.c", [((1, 2), (1, 5), SevWarning, "unused", "clang")]))
        (fmap (map (\d -> (diagStart d, diagEnd d, diagSeverity d, diagMessage d, diagSource d))) <$> parseDiagnostics (JObject [("uri", JString "file:///a.c"), ("diagnostics", JArray [JObject [("range", rng 1 2 1 5), ("severity", JInt 2), ("message", JString "unused"), ("source", JString "clang")]])]))
  , test "locations and location links" $
      assertEqual [Location "/a" (3, 4), Location "/b" (5, 6)]
        (parseLocations (JArray [JObject [("uri", JString "file:///a"), ("range", rng 3 4 3 5)], JObject [("targetUri", JString "file:///b"), ("targetSelectionRange", rng 5 6 5 7), ("targetRange", rng 0 0 9 0)]]))
  , test "hover contents" $
      assertEqual ["```haskell", "f :: Int", "```"] (parseHover (JObject [("contents", JObject [("kind", JString "markdown"), ("value", JString "\n```haskell\nf :: Int\n```\n")])]))
  , test "snippets become plain text" $
      assertEqual ["foo(x, y)", "if  then", "a$b", "choice"] (map stripSnippet ["foo(${1:x}, ${2:y})$0", "if $1 then", "a\\$b", "${1|choice,other|}"])
  , test "completion items" $
      assertEqual [("print", "print()", Just ((0, 0), (0, 2)))]
        [(ciLabel c, ciInsert c, ciReplace c) | c <- parseCompletion (JObject [("items", JArray [JObject [("label", JString "print"), ("insertTextFormat", JInt 2), ("textEdit", JObject [("range", rng 0 0 0 2), ("newText", JString "print($0)")])]])])]
  ]
  where
    rng a b c d = JObject [("start", JObject [("line", JInt a), ("character", JInt b)]), ("end", JObject [("line", JInt c), ("character", JInt d)])]

syncTests :: [Test]
syncTests =
  [ test "the first sync opens the document" (assertEqual ["textDocument/didOpen"] (methods (fst (syncMessages info fresh (docAt 1 "ab")))))
  , test "an edit is sent as one range change" $
      let (_, at1) = syncMessages info fresh (docAt 1 "ab\ncd")
          (msgs, _) = syncMessages info at1 (docAt 2 "ab\ncdX")
       in assertEqual
            [Just (JArray [JObject [("range", rangeOf 1 2 1 2), ("text", JString "X")]])]
            [J.path ["params", "contentChanges"] m | m <- msgs]
  , test "an undo back to the sent text sends nothing" $
      let (_, at1) = syncMessages info fresh (docAt 1 "ab")
       in assertEqual [] (fst (syncMessages info at1 (docAt 2 "ab")))
  , test "a save is reported once" $
      let (_, at1) = syncMessages info fresh (docAt 1 "ab")
          saved = (docAt 1 "ab") {docSaves = 1}
          (msgs, at2) = syncMessages info at1 saved
       in assertEqual (["textDocument/didSave"], []) (methods msgs, methods (fst (syncMessages info at2 saved)))
  , test "a full-sync server gets the whole text" $
      let full = fmap (\i -> i {siSync = SyncFull}) info
          (_, at1) = syncMessages full fresh (docAt 1 "ab")
       in assertEqual [Just (JArray [JObject [("text", JString "ab!\n")]])] [J.path ["params", "contentChanges"] m | m <- fst (syncMessages full at1 (docAt 2 "ab!"))]
  ]
  where
    info = Just (ServerInfo Utf8 [] [] SyncIncremental "fake" "/" JNull)
    fresh = Attachment "fake" "/x.c" "c" (-1) Nothing 0
    docAt v t = (newDocument (Just "/x.c") (buf t)) {docVersion = v}
    methods = map (\m -> fromMaybe "" (J.path ["method"] m >>= asText))
    rangeOf a b c d = JObject [("start", JObject [("line", JInt a), ("character", JInt b)]), ("end", JObject [("line", JInt c), ("character", JInt d)])]

lspEditTests :: [Test]
lspEditTests =
  [ test "edits apply from the end, positions in the old text" $
      assertEqual "int  y = 1;\nint z;" (B.toText (applyTextEdits Utf8 [TextEdit (0, 4) (0, 5) " y", TextEdit (1, 4) (1, 5) "z"] (buf "int x = 1;\nint w;")))
  , test "insertions at one place keep their order" $
      assertEqual "ab" (B.toText (applyTextEdits Utf8 [TextEdit (0, 0) (0, 0) "a", TextEdit (0, 0) (0, 0) "b"] (buf "")))
  , test "UTF-16 columns" (assertEqual "😀X" (B.toText (applyTextEdits Utf16 [TextEdit (0, 2) (0, 3) "X"] (buf "😀y"))))
  , test "an edit past the end appends" (assertEqual "a\nb" (B.toText (applyTextEdits Utf8 [TextEdit (5, 0) (5, 0) "\nb"] (buf "a"))))
  , test "workspace edits: changes and documentChanges" $
      let e = JObject [("newText", JString "x"), ("range", JObject [("start", pos 0 0), ("end", pos 0 1)])]
       in assertEqual
            ([("/a", [TextEdit (0, 0) (0, 1) "x"])], [("/b", [TextEdit (0, 0) (0, 1) "x"])])
            ( parseWorkspaceEdit (JObject [("changes", JObject [("file:///a", JArray [e])])])
            , parseWorkspaceEdit (JObject [("documentChanges", JArray [JObject [("textDocument", JObject [("uri", JString "file:///b")]), ("edits", JArray [e])]])])
            )
  ]
  where
    pos l c = JObject [("line", JInt l), ("character", JInt c)]

-- | The LSP client against clangd, when it is installed.
lspTests :: IO [Test]
lspTests = do
  clangd <- findExecutable "clangd"
  case clangd of
    Nothing -> pure [test "clangd not found: skipped" (Right ())]
    Just _ -> do
      config <- either (fail . T.unpack) pure defaultConfig
      dir <- getTemporaryDirectory
      let project = dir <> "/him-test-lsp"
          file = project <> "/main.c"
      createDirectoryIfMissing True project
      writeFile file "int add(int a, int b) { return a + b; }\nint main(void) {\n    int x = add(1, \"two\");\n    return x;\n}\n"
      writeFile (project <> "/compile_flags.txt") "-std=c11\n"
      rt <- testRuntime config
      doc <- either (fail . T.unpack) pure =<< loadDocument file
      let start = newEditor (24, 80) doc
          run ed k = execStateT (handleEvent config (EvKey k)) ed
          keys ks ed = foldlM run ed (fromMaybe (error ks) (parseKeys (T.pack ks)))
          diagnosed ed = not (null (shownDiagnostics (edLsp ed) (docLsp (edDoc ed)) (docBuffer (edDoc ed))))
          errorsOn ed = [(sdLine sd, sdSeverity sd) | sd <- shownDiagnostics (edLsp ed) (docLsp (edDoc ed)) (docBuffer (edDoc ed)), sdSeverity sd == SevError]
      attached <- settleUntil config rt 20000 diagnosed =<< execStateT (handleEvent config (EvResize 24 80)) start
      -- Line 2 is `    int x = add(1, "two");`: add at 12, "two" at 19-23.
      let selecting l a b ed = ed {edDoc = (edDoc ed) {docSelection = single (Range (Pos l a) (Pos l b) Nothing)}}
      hovered <- settleUntil config rt 10000 (isJust . edPopup) =<< keys "space k" (selecting 2 12 12 attached)
      defined <- settleUntil config rt 10000 ((/= Pos 2 12) . rangeHead . primary . docSelection . edDoc) =<< keys "g d" (selecting 2 12 12 attached)
      fixed <- settleUntil config rt 20000 (null . errorsOn) =<< keys "c 2 esc" (selecting 2 19 23 attached)
      -- Completion: type "ad" on a new line; the menu opens by itself.
      menu <- settleUntil config rt 10000 (isJust . edCompletion) =<< keys "g g j o a d" fixed
      accepted <- keys "ret" menu
      let ex line ed = foldlM run ed ([plain (KChar ':')] <> map (\c -> plain (KChar c)) (T.unpack line) <> [plain KEnter])
          isAttached ed = case docLsp (edDoc ed) of LspAttached _ -> True; _ -> False
      -- Rename add -> plus (cursor on its definition).
      -- (After the completion above the text is: the add function, main,
      -- the completed word, then `    int x = add(1, 2);` on line 3.)
      renamed <- settleUntil config rt 10000 (("plus" `T.isInfixOf`) . B.lineAt 0 . docBuffer . edDoc) =<< keys "space r backspace backspace backspace p l u s ret" . selecting 0 4 4 =<< keys "esc" accepted
      -- Symbols, signature help, code actions.
      symbolsShown <- settleUntil config rt 10000 (isJust . edPicker) =<< keys "space s" renamed
      actionsShown <- settleUntil config rt 10000 (isJust . edPicker) =<< keys "space a" (selecting 3 12 21 renamed)
      -- (This one edits the text, so it comes after the others.)
      signature <- settleUntil config rt 10000 (isJust . edPopup) =<< keys "g g j o p l u s (" renamed

      -- Formatting.
      let ugly = "int  main( void ){return 0;}\n"
      writeFile (project <> "/ugly.c") ugly
      uglyDoc <- either (fail . T.unpack) pure =<< loadDocument (project <> "/ugly.c")
      uglyOpen <- settleUntil config rt 20000 isAttached =<< execStateT (handleEvent config (EvResize 24 80)) (newEditor (24, 80) uglyDoc)
      formatted <- settleUntil config rt 10000 ((/= T.strip (T.pack ugly)) . B.toText . docBuffer . edDoc) =<< ex "format" uglyOpen
      -- Apply a code action (clangd's tweaks are commands, whose edits come
      -- back as a workspace/applyEdit request), in a file of its own.
      writeFile (project <> "/act.c") "int add(int a, int b) { return a + b; }\nint main(void) {\n    int x = add(1, 2) * 3;\n    return x;\n}\n"
      actDoc <- either (fail . T.unpack) pure =<< loadDocument (project <> "/act.c")
      actOpen <- settleUntil config rt 20000 isAttached =<< execStateT (handleEvent config (EvResize 24 80)) (newEditor (24, 80) actDoc)
      picked <- settleUntil config rt 10000 (isJust . edPicker) =<< keys "space a" (selecting 2 12 20 actOpen)
      -- Workspace symbols, asked again as the query changes.
      symbolsFound <-
        settleUntil config rt 10000 (maybe False (any ((== "add") . piLabel) . pkMatches) . edPicker)
          =<< keys "space S a d d" actOpen
      -- Completing printf without stdio.h adds the include (clangd sends it
      -- with the item).
      writeFile (project <> "/imp.c") "int main(void) {\n    return 0;\n}\n"
      impDoc <- either (fail . T.unpack) pure =<< loadDocument (project <> "/imp.c")
      impOpen <- settleUntil config rt 20000 isAttached =<< execStateT (handleEvent config (EvResize 24 80)) (newEditor (24, 80) impDoc)
      impMenu <- settleUntil config rt 10000 (maybe False (any (T.isInfixOf "printf" . ciLabel) . cmShown) . edCompletion) =<< keys "j o p r i n t f" impOpen
      imported <- settleUntil config rt 10000 (T.isPrefixOf "#include <stdio.h>" . B.lineAt 0 . docBuffer . edDoc) =<< keys "ret" impMenu
      extracted <- settleUntil config rt 10000 (T.isInfixOf "placeholder" . B.toText . docBuffer . edDoc) =<< keys "e x t r a c t ret" picked
      -- Server commands (last: a restart replaces the server the states
      -- above were talking to).
      infoShown <- ex "lsp-info" attached
      stopped <- ex "lsp-stop" attached
      restarted <- settleUntil config rt 20000 (\ed -> isAttached ed && diagnosed ed) =<< ex "lsp-restart" attached
      startedAgain <- settleUntil config rt 20000 isAttached =<< ex "lsp-start" stopped
      pure
        [ test "the document attaches to clangd" (assertEqual True (case docLsp (edDoc attached) of LspAttached _ -> True; _ -> False))
        , test "an error is reported on its line" (assertEqual [(2, SevError)] (take 1 (errorsOn attached)))
        , test "hover shows a popup" (assertEqual True (isJust (edPopup hovered)))
        , test "g d goes to the definition of add" (assertEqual (Pos 0 4) (rangeHead (primary (docSelection (edDoc defined)))))
        , test "fixing the error clears it" (assertEqual [] (errorsOn fixed))
        , test "space r renames every use" $
            assertEqual (True, True) ("int plus(" `T.isPrefixOf` B.lineAt 0 (docBuffer (edDoc renamed)), "plus(1" `T.isInfixOf` B.lineAt 3 (docBuffer (edDoc renamed)))
        , test "space s lists the symbols" $
            assertEqual True (maybe False (\p -> all (`elem` map (T.strip . piLabel) (toList (pkItems p))) ["plus", "main"]) (edPicker symbolsShown))
        , test "typing ( shows the signature" $
            assertEqual (Just "signature", True) (infoTitle <$> edPopup signature, maybe False (any (T.isInfixOf "int a" . fst) . infoRows) (edPopup signature))
        , test "space a lists code actions" (assertEqual (Just "code actions") (pkTitle <$> edPicker actionsShown))
        , test "space S finds workspace symbols as you type" (assertEqual (Just "workspace symbols") (pkTitle <$> edPicker symbolsFound) >> assertEqual True (maybe False (any ((== "add") . piLabel) . pkMatches) (edPicker symbolsFound)))
        , test "a completion brings its import" $
            assertEqual ("#include <stdio.h>", True) (B.lineAt 0 (docBuffer (edDoc imported)), "printf" `T.isInfixOf` B.toText (docBuffer (edDoc imported)))
        , test "a picked code action is applied" (assertEqual True (T.isInfixOf "placeholder" (B.toText (docBuffer (edDoc extracted)))))
        , test ":format formats the file" (assertEqual "int main(void) { return 0; }" (T.strip (B.toText (docBuffer (edDoc formatted)))))
        , test ":lsp-info names the server" (assertEqual True (maybe False (\(Status _ m) -> "clangd" `T.isInfixOf` m) (edStatus infoShown)))
        , test ":lsp-stop detaches and drops the diagnostics" (assertEqual (LspNone, False) (docLsp (edDoc stopped), diagnosed stopped))
        , test ":lsp-restart attaches again and diagnostics return" (assertEqual (True, True) (isAttached restarted, diagnosed restarted))
        , test ":lsp-start starts a stopped server" (assertEqual True (isAttached startedAgain))
        , test "typing a word opens the completion menu" $
            assertEqual (True, Completing) (any ((== "add") . ciInsert) (maybe [] cmShown (edCompletion menu)), keymapMode menu)
        , test "ret inserts the selected completion" $
            assertEqual (True, Nothing) ("add" `T.isPrefixOf` T.strip (B.lineAt 2 (docBuffer (edDoc accepted))), edCompletion accepted)
        ]
