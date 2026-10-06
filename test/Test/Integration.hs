-- | Keys through the real config, buffers, loading files, processes.
module Test.Integration
  ( integrationTests
  , openBufferTests
  , loadingTests
  , processTests
  ) where

import Control.Monad ((<=<))
import Control.Monad.Trans.State.Strict (execStateT)
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.Foldable (foldlM)
import Data.List (isPrefixOf)
import Data.Maybe (fromMaybe, listToMaybe)
import Him.App (handleEvent)
import Him.Buffer qualified as B
import Him.Config.Default (defaultConfig)
import Him.Document
import Him.Editor
import Him.Event (Event (..))
import Him.Ex (previewedTheme)
import Him.Config (Config (..))
import Him.Actions.Search (refreshSearchPreview)
import Him.File (decodeDocument, encodeDocument, loadDocument, loadDocumentChunked, saveDocument)
import Data.Text.Encoding qualified as TE
import System.Directory (canonicalizePath, createDirectoryIfMissing, createDirectoryLink, doesPathExist, getTemporaryDirectory, removeDirectoryRecursive, removeFile)
import Him.FileTree (WalkOptions (..), defaultWalk, listFiles)
import Him.Options
import Him.Picker
import Him.Effect (Effect (..), Job (..), JobResult (..))
import Him.Process (ProcessResult (..), runProcess)
import System.Exit (ExitCode (..))
import Him.Directory (entriesIn, entryAt)
import Him.Key
import Him.Mode (Mode (..))
import Him.Position (Pos (..))
import Him.Selection
import Him.View (View (..))
import Him.Render (render)
import Him.Render.Theme (defaultTheme)
import Data.Foldable (toList)
import Data.Text qualified as T
import Test.Harness
import Test.Util

-- | Feed key sequences through 'handleEvent' with the real keymaps.
integrationTests :: IO [Test]
integrationTests = do
  config <- either (fail . show) pure defaultConfig
  let start t = newEditor (24, 80) (newDocument Nothing (buf t))
      -- Like the main loop: handle the key, then refresh the search preview.
      typeKeys ks ed = foldlM (\e k -> refreshSearchPreview <$> execStateT (handleEvent config (EvKey k)) e) ed (fromMaybe (error ("bad keys: " <> show ks)) (parseKeys ks))
      selectionAfter t ks = (\e -> let r = primary (docSelection (edDoc e)) in (rangeAnchor r, rangeHead r)) <$> typeKeys ks (start t)
      textAfter t ks = B.toText . docBuffer . edDoc <$> typeKeys ks (start t)
  typed <- textAfter "" "i h i space t h e r e esc"
  newline <- textAfter "ab" "a ret c esc"
  opened <- textAfter "one\ntwo" "o x esc"
  backspace <- textAfter "abc" "a a backspace backspace esc"
  quitDirty <- typeKeys ": q ret" =<< typeKeys "i x esc" (start "")
  quitClean <- typeKeys ": q ret" (start "")
  cmdEsc <- typeKeys ": w esc" (start "")
  wordDelete <- textAfter "hello world" "w d"
  lineDelete <- textAfter "one\ntwo\nthree" "j x d"
  lineTwice <- textAfter "one\ntwo\nthree" "x x d"
  change <- textAfter "foo bar" "e c b a z esc"
  gotoEnd <- textAfter "one\ntwo\nthree" "g e x d"
  gotoTop <- textAfter "one\ntwo" "j g g x d"
  selectExtend <- textAfter "abcdef" "v l l esc d"
  collapsed <- textAfter "hello world" "w ; d"
  undoInsert <- textAfter "" "i a b c esc u"
  redoInsert <- textAfter "" "i a b c esc u U"
  undoChange <- textAfter "foo bar" "e c x esc u"
  undoTwice <- textAfter "a\nb\nc" "x d x d u"
  undoClean <- typeKeys "i x esc u" (start "abc")
  nothingToUndo <- typeKeys "u" (start "abc")
  pasteLineBelow <- textAfter "one\ntwo\nthree" "x y j p"
  movedLine <- textAfter "a\nb\nc" "x d p"
  pasteAtEnd <- textAfter "a\nb" "x y g e p"
  pasteAbove <- textAfter "one\ntwo" "j x y g g P"
  pasteChars <- textAfter "hello world" "e y p"
  undoPaste <- textAfter "hello world" "e y p u"
  searched <- selectionAfter "one two\nthree two" "/ t w o ret"
  searchNext <- selectionAfter "one two\nthree two" "/ t w o ret n"
  searchWrap <- selectionAfter "one two\nthree two" "/ t w o ret n n"
  searchBack <- selectionAfter "one two\nthree two" "g e ? t w o ret"
  searchPrev <- selectionAfter "one two\nthree two" "/ t w o ret n N"
  previewed <- selectionAfter "one two\nthree two" "/ t h r"
  cancelled <- selectionAfter "one two\nthree two" "l / t h r esc"
  starSearch <- selectionAfter "one two\none" "e * n"
  notFound <- typeKeys "/ z z ret" (start "abc")
  deleteMatch <- textAfter "one two three" "/ t w o ret d"
  multiInsert <- textAfter "ab\nab\nab" "C C i X esc"
  multiPaste <- textAfter "one two" "% s o ret y ; p"
  multiCount <- typeKeys "% s a ret" (start "a b a b a")
  keepOne <- typeKeys "% s a ret ," (start "a b a b a")
  splitLines <- textAfter "ab\ncd" "% A-s d"
  selectEsc <- typeKeys "l % s a esc" (start "a b a")
  selectNone <- typeKeys "% s z z ret" (start "a b a")
  adjacentAppend <- typeKeys "% s a ret a backspace" (start "aab")
  adjacentInsert <- textAfter "aab" "% s a ret i backspace esc"
  adjacentType <- textAfter "aab" "% s a ret a X esc"
  multiUndo <- textAfter "ab\nab" "C i X Y esc u"
  findF <- selectionAfter "hello world" "f o"
  findT2 <- selectionAfter "a.b.c.d" "2 t ."
  findBack <- selectionAfter "hello world" "g l F o"
  findRepeat <- selectionAfter "a.b.c.d" "f . A-."
  findCancelled <- typeKeys "f esc l" (start "abc")
  findNewline <- selectionAfter "ab\ncd" "f ret"
  let lines40 = T.intercalate "\n" [T.pack (show i) | i <- [1 .. 100 :: Int]]
  halfDown <- typeKeys "C-d" (start lines40)
  pageDown <- typeKeys "C-f" (start lines40)
  pageBack <- typeKeys "C-f C-b" (start lines40)
  gotoFive <- selectionAfter lines40 "5 g g"
  gotoFirst <- selectionAfter lines40 "j j g g"
  suspended <- typeKeys "C-z" (start "abc")
  countDown <- selectionAfter "a\nb\nc\nd\ne" "3 j"
  countTwelve <- selectionAfter (T.intercalate "\n" (replicate 20 "x")) "1 2 j"
  countWords <- textAfter "one two three four" "2 w d"
  countLines <- textAfter "a\nb\nc\nd" "2 x d"
  countIgnored <- textAfter "a\nb\nc" "3 u"
  countCleared <- selectionAfter "a\nb\nc\nd\ne" "3 esc j"
  countPending <- typeKeys "4 2" (start "abc")
  zeroAlone <- typeKeys "0" (start "abc")
  countInsert <- textAfter "" "i 3 esc"
  infoG <- typeKeys "g" (start "abc")
  infoAfterG <- typeKeys "g g" (start "abc")
  infoColon <- typeKeys ": w" (start "abc")
  infoArgs <- typeKeys ": o space x" (start "abc")
  completeName <- typeKeys ": b u f f e r - n tab" (start "abc")
  completeMany <- typeKeys ": w r i tab" (start "abc")
  cycleOnce <- typeKeys ": w r i tab tab" (start "abc")
  cycleTwice <- typeKeys ": w r i tab tab tab" (start "abc")
  cycleBack <- typeKeys ": w r i tab S-tab" (start "abc")
  cycleWrap <- typeKeys ": w r i tab tab tab tab tab tab" (start "abc")
  cycleTyped <- typeKeys ": w r i tab tab tab a" (start "abc")
  cycleAtPrefix <- typeKeys ": w r i t e tab" (start "abc")
  themeLine <- typeKeys ": t h e m e space n o r d" (start "abc")
  themeEsc <- typeKeys ": t h e m e space n o r d esc" (start "abc")
  themeBare <- typeKeys ": t h e m e space" (start "abc")
  pendingG <- typeKeys "g" (start "abc")
  badChord <- typeKeys "g z" (start "abc")
  -- Settings change what keys do.
  let with f t = (start t) {edOptions = f defaultOptions}
  expanded <- B.toText . docBuffer . edDoc <$> typeKeys "i tab esc" (with (\o -> o {optExpandTab = True, optTabWidth = 2}) "x")
  noWrap <- typeKeys "g e / o n e ret" (with (\o -> o {optWrapAround = False}) "one\ntwo")
  exactCase <- typeKeys "/ o n e ret" (with (\o -> o {optSmartCase = False}) "x ONE one")
  relativeEd <- typeKeys "j j" (with (\o -> o {optLineNumbers = LineNumbersRelative}) "a\nb\nc\nd")
  let relative = render defaultTheme Nothing relativeEd
  pure
    [ test "expand-tab inserts tab-width spaces" (assertEqual "  x" expanded)
    , test "without wrap-around a search stops at the end" (assertEqual (Just (Status Error "pattern not found: one")) (edStatus noWrap))
    , test "without smart case a lower-case pattern matches exactly" $
        assertEqual (Pos 0 6) (rangeStart (primary (docSelection (edDoc exactCase))))
    , test "relative line numbers count from the cursor's line" $
        assertEqual ["   2 a", "   1 b", "   3 c", "   1 d"] [T.take 6 (rowText relative r) | r <- [0 .. 3]]
    , test "typing in insert mode" (assertEqual "hi there" typed)
    , test "append then newline" (assertEqual "a\ncb" newline)
    , test "open below" (assertEqual "one\nx\ntwo" opened)
    , test "backspace in insert mode" (assertEqual "bc" backspace)
    , test ":q refuses when dirty" (assertEqual (False, True) (edQuit quitDirty, isError (edStatus quitDirty)))
    , test ":q quits when clean" (assertEqual True (edQuit quitClean))
    , test "esc leaves the command line" (assertEqual (Normal, "") (edMode cmdEsc, edCmdLine cmdEsc))
    , test "w d deletes a word and its blanks" (assertEqual "world" wordDelete)
    , test "x d deletes a line" (assertEqual "one\nthree" lineDelete)
    , test "x x d deletes two lines" (assertEqual "three" lineTwice)
    , test "e c replaces a word" (assertEqual "baz bar" change)
    , test "g e goes to the last line" (assertEqual "one\ntwo" gotoEnd)
    , test "g g goes to the first line" (assertEqual "two" gotoTop)
    , test "select mode extends" (assertEqual "def" selectExtend)
    , test "; collapses the selection" (assertEqual "helloworld" collapsed)
    , test "u undoes a whole insert session" (assertEqual "" undoInsert)
    , test "U redoes it" (assertEqual "abc" redoInsert)
    , test "u undoes a change (c + typing) at once" (assertEqual "foo bar" undoChange)
    , test "u undoes one step at a time" (assertEqual "b\nc" undoTwice)
    , test "undo back to the saved text is not dirty" (assertEqual False (docDirty (edDoc undoClean)))
    , test "nothing to undo is reported" (assertEqual (Just (Status Info "nothing to undo")) (edStatus nothingToUndo))
    , test "p pastes a yanked line below" (assertEqual "one\ntwo\none\nthree" pasteLineBelow)
    , test "x d p moves a line down" (assertEqual "b\na\nc" movedLine)
    , test "p pastes a line below the last line" (assertEqual "a\nb\na" pasteAtEnd)
    , test "P pastes a line above" (assertEqual "two\none\ntwo" pasteAbove)
    , test "p pastes characters after the selection" (assertEqual "hellohello world" pasteChars)
    , test "u undoes a paste" (assertEqual "hello world" undoPaste)
    , test "/ selects the first match" (assertEqual (Pos 0 4, Pos 0 6) searched)
    , test "n selects the next match" (assertEqual (Pos 1 6, Pos 1 8) searchNext)
    , test "n wraps around" (assertEqual (Pos 0 4, Pos 0 6) searchWrap)
    , test "? searches backward" (assertEqual (Pos 0 4, Pos 0 6) searchBack)
    , test "N goes back" (assertEqual (Pos 0 4, Pos 0 6) searchPrev)
    , test "typing previews the match" (assertEqual (Pos 1 0, Pos 1 2) previewed)
    , test "esc restores the selection" (assertEqual (Pos 0 1, Pos 0 1) cancelled)
    , test "* then n searches for the selection" (assertEqual (Pos 1 0, Pos 1 2) starSearch)
    , test "a missing pattern is reported" (assertEqual (Just (Status Error "pattern not found: zz")) (edStatus notFound))
    , test "d deletes the match" (assertEqual "one  three" deleteMatch)
    , test "C then insert types at every cursor" (assertEqual "Xab\nXab\nXab" multiInsert)
    , test "yank and paste per selection" (assertEqual "oone twoo" multiPaste)
    , test "% s selects every match" (assertEqual 3 (rangeCount (docSelection (edDoc multiCount))))
    , test ", keeps the primary" (assertEqual 1 (rangeCount (docSelection (edDoc keepOne))))
    , test "A-s d deletes each line's text" (assertEqual "\n" splitLines)
    , test "esc on the s prompt restores the selection" (assertEqual [Range (Pos 0 0) (Pos 0 5) Nothing] (ranges (docSelection (edDoc selectEsc))))
    , test "s without matches is reported" (assertEqual (Just (Status Error "no matches: zz")) (edStatus selectNone))
    , test "backspace at adjacent cursors deletes both characters" $
        assertEqual ("b", [Range (Pos 0 0) (Pos 0 0) Nothing]) (B.toText (docBuffer (edDoc adjacentAppend)), ranges (docSelection (edDoc adjacentAppend)))
    , test "backspace at adjacent cursors, one at the start" (assertEqual "ab" adjacentInsert)
    , test "typing at adjacent cursors" (assertEqual "aXaXb" adjacentType)
    , test "u undoes a multi-cursor insert at once" (assertEqual "ab\nab" multiUndo)
    , test "f selects to a character" (assertEqual (Pos 0 0, Pos 0 4) findF)
    , test "2 t . stops before the second dot" (assertEqual (Pos 0 0, Pos 0 2) findT2)
    , test "F searches back" (assertEqual (Pos 0 10, Pos 0 7) findBack)
    , test "A-. repeats the last find" (assertEqual (Pos 0 1, Pos 0 3) findRepeat)
    , test "esc cancels f; the next key works as usual" (assertEqual (Nothing, Pos 0 1) (edAwait findCancelled, rangeHead (primary (docSelection (edDoc findCancelled)))))
    , test "f ret finds the line break" (assertEqual (Pos 0 0, Pos 0 2) findNewline)
    , test "C-d moves half a page, view and cursor" $
        -- (The view then keeps 3 lines of margin above the cursor.)
        assertEqual (Pos 11 0, 8) (rangeHead (primary (docSelection (edDoc halfDown))), viewTop (edView halfDown))
    , test "C-f moves a page; C-b back" $
        assertEqual (Pos 22 0, Pos 0 0) (rangeHead (primary (docSelection (edDoc pageDown))), rangeHead (primary (docSelection (edDoc pageBack))))
    , test "5 g g goes to line 5; g g to the first" (assertEqual ((Pos 4 0, Pos 4 0), (Pos 0 0, Pos 0 0)) (gotoFive, gotoFirst))
    , test "C-z asks to suspend" (assertEqual True (Suspend `elem` edEffects suspended))
    , test "a count repeats a motion" (assertEqual (Pos 3 0, Pos 3 0) countDown)
    , test "counts have several digits" (assertEqual (Pos 12 0, Pos 12 0) countTwelve)
    , test "2 w d deletes the second word's selection" (assertEqual "one three four" countWords)
    , test "2 x selects two lines" (assertEqual "c\nd" countLines)
    , test "a count on an action without one is ignored" (assertEqual "a\nb\nc" countIgnored)
    , test "an unbound key clears the count" (assertEqual (Pos 1 0, Pos 1 0) countCleared)
    , test "the count is shown while typed" (assertEqual (Just 42) (edCount countPending))
    , test "0 does not start a count" (assertEqual Nothing (edCount zeroAlone))
    , test "digits type in insert mode" (assertEqual "3" countInsert)
    , test "g shows the goto keys" $
        assertEqual
          (Just ("goto", Just "Go to the first line, or to line <count> (5 g g)"))
          ((\b -> (infoTitle b, lookup "g" (infoRows b))) <$> edInfo infoG)
    , test "the info box goes away after the chord" (assertEqual Nothing (edInfo infoAfterG))
    , test ": lists the matching commands" $
        assertEqual (Just ["write, w", "write-quit, wq, x", "write-all, wa", "write-quit-all, wqa, xa"]) (map fst . infoRows <$> edInfo infoColon)
    , test "after the name, the command is described" $
        assertEqual (Just ["open, o, edit, e"]) (map fst . infoRows <$> edInfo infoArgs)
    , test "tab completes a unique command" (assertEqual "buffer-next " (edCmdLine completeName))
    , test "tab extends to the common prefix and lists candidates" $
        assertEqual ("write", Just ["write", "write-quit", "write-all", "write-quit-all"]) (edCmdLine completeMany, ccShown <$> edCompletions completeMany)
    , test "tab again puts the first candidate on the line" $
        assertEqual ("write ", Just (Just 0)) (edCmdLine cycleOnce, ccSelected <$> edCompletions cycleOnce)
    , test "tab cycles to the next candidate" (assertEqual "write-quit " (edCmdLine cycleTwice))
    , test "S-tab cycles backwards" (assertEqual "write-quit-all " (edCmdLine cycleBack))
    , test "cycling wraps around" (assertEqual "write " (edCmdLine cycleWrap))
    , test "typing ends the cycle" (assertEqual ("write-quit a", Nothing) (edCmdLine cycleTyped, edCompletions cycleTyped))
    , test "tab at the common prefix goes straight to the first candidate" $
        assertEqual ("write ", Just 0) (edCmdLine cycleAtPrefix, ccSelected =<< edCompletions cycleAtPrefix)
    , test "the info box highlights the candidate on the line" (assertEqual (Just (Just 1)) (infoSelected <$> edInfo cycleTwice))
    , test ":theme <name> previews the theme" $
        assertEqual (Just "nord") (previewedTheme (cfgExCommands config) themeLine)
    , test "esc ends the theme preview" $
        assertEqual Nothing (previewedTheme (cfgExCommands config) themeEsc)
    , test ":theme without a name previews nothing" $
        assertEqual Nothing (previewedTheme (cfgExCommands config) themeBare)
    , test "g waits for the next key" (assertEqual [plain (KChar 'g')] (edPending pendingG))
    , test "an unknown chord is dropped" (assertEqual ([], "abc") (edPending badChord, B.toText (docBuffer (edDoc badChord))))
    ]
  where
    isError = \case
      Just (Status Error _) -> True
      _ -> False

-- | Opening, switching, closing and saving buffers, on real files.
openBufferTests :: IO [Test]
openBufferTests = do
  config <- either (fail . T.unpack) pure defaultConfig
  dir <- getTemporaryDirectory
  let fileA = dir <> "/him-test-buf-a.txt"
      fileB = dir <> "/him-test-buf-b.txt"
      fileNew = dir <> "/him-test-buf-new.txt"
  writeFile fileA "alpha\n"
  writeFile fileB "beta\n"
  docA <- either (fail . T.unpack) pure =<< loadDocument fileA
  let run ed k = execStateT (handleEvent config (EvKey k)) ed
      keys ks = foldlM run `flip` fromMaybe (error ks) (parseKeys (T.pack ks))
      -- Type a : command literally (paths contain characters key syntax
      -- would read differently).
      ex line ed = foldlM run ed ([plain (KChar ':')] <> map charKey (T.unpack line) <> [plain KEnter])
      charKey c = if c == ' ' then plain (KChar ' ') else plain (KChar c)
      start = newEditor (24, 80) docA
      current ed = (docPath (edDoc ed), bufferIndex ed)
      a = Just fileA
      b = Just fileB
  opened <- ex ("o " <> T.pack fileB) start
  nextWraps <- keys "g n" opened
  reopened <- ex ("e " <> T.pack fileA) opened
  closed <- ex "bc" opened
  newFile <- ex ("o " <> T.pack fileNew) start
  editedB <- keys "i X esc" opened
  quitDirty <- ex "q" =<< keys "g p" editedB
  closeDirty <- ex "bc" editedB
  writtenAll <- ex "wa" =<< keys "g p" editedB
  savedB <- readFile fileB
  quitAfter <- ex "q" writtenAll
  scratch <- ex "n" start
  pickedBuffer <- keys "space b down ret" opened
  -- tab marks files, ret opens them all and ends on the last.
  openedMarked <- keys "tab tab ret" start {edPicker = Just (newPicker "files" [pickerItem (T.pack f) (PickFile f) "" | f <- [fileA, fileB]]), edMode = Picking}
  pickerTyped <- keys "space b 2 backspace" opened
  pickerEsc <- keys "space b esc" opened
  scanned <- settle config =<< foldlM run start [plain (KChar ' '), plain (KChar 'f')]
  -- A large picker filters in a background job.
  let many = [pickerItem (T.pack ("dir/file" <> show i <> (if i `mod` 1000 == 0 then "_needle" else ""))) (PickFile "") "" | i <- [1 .. syncLimit + 5000 :: Int]]
      bigPicker = start {edPicker = Just (newPicker "files" many) {pkGeneration = 7}, edMode = Picking}
  typedBig <- foldlM run bigPicker (map charKey "needle")
  filteredBig <- settle config typedBig
  staleDropped <- execStateT (handleEvent config (EvJob (PickerFiltered 7 "need" [] 0))) filteredBig
  otherScan <- execStateT (handleEvent config (EvJob (FilesFound 99 ["x"]))) filteredBig
  pickedFile <- settle config =<< foldlM run scanned (map charKey "test/Spec.hs" <> [plain KEnter])
  -- The global search, over this repository.
  searched <- settle config =<< foldlM run start ([plain (KChar ' '), charKey '/'] <> map charKey "maxHitText")
  searchedAgain <- foldlM run searched (map charKey " ::")
  let firstHit = edPicker searched >>= selectedItem
  pickedHit <- foldlM run searched [plain KEnter]
  searchStale <- execStateT (handleEvent config (EvJob (GrepFound (maybe 0 pkGeneration (edPicker searched)) "maxHit" [pickerItem "x" (PickFile "x") ""] 1))) searched
  -- listFiles on a small tree with a hidden directory.
  let tree = dir <> "/him-test-tree"
  createDirectoryIfMissing True (tree <> "/sub/deeper")
  createDirectoryIfMissing True (tree <> "/.hidden")
  mapM_ (\f -> writeFile (tree <> "/" <> f) "") ["b.txt", "a.txt", "sub/c.txt", "sub/deeper/d.txt", ".hidden/x.txt", ".dotfile"]
  listed <- listFiles (defaultWalk 100) tree
  listedFew <- listFiles (defaultWalk 2) tree
  listedHidden <- listFiles (defaultWalk 100) {woHidden = True} tree
  createDirectoryIfMissing True (tree <> "/build")
  mapM_ (\f -> writeFile (tree <> "/" <> f) "") ["x.log", "sub/y.log", "sub/keep.log", "build/out.txt", "sub/deeper/gen.txt"]
  writeFile (tree <> "/.gitignore") "*.log\nbuild/\n"
  writeFile (tree <> "/sub/.gitignore") "!keep.log\n"
  writeFile (tree <> "/sub/.ignore") "deeper/gen.txt\n"
  listedIgnoring <- listFiles (defaultWalk 100) tree
  listedNoGit <- listFiles (defaultWalk 100) {woGitIgnore = False} tree
  -- Started below a repository root, the root's ignore files still apply.
  createDirectoryIfMissing True (tree <> "/.git/info")
  writeFile (tree <> "/.gitignore") "/sub/c.txt\n"
  writeFile (tree <> "/.git/info/exclude") "d.txt\n"
  listedInRepo <- listFiles (defaultWalk 100) (tree <> "/sub")
  -- Links: a link to a directory is followed; a link back up is not.
  let ltree = dir <> "/him-test-links"
  createDirectoryIfMissing True (ltree <> "/real")
  writeFile (ltree <> "/real/r.txt") ""
  createDirectoryLink (ltree <> "/real") (ltree <> "/alias")
  createDirectoryLink ltree (ltree <> "/real/loop")
  listedLinks <- listFiles (defaultWalk 100) ltree
  listedNoLinks <- listFiles (defaultWalk 100) {woFollowLinks = False} ltree
  removeDirectoryRecursive ltree
  removeDirectoryRecursive tree
  -- Directory listings.
  let dtree = dir <> "/him-test-dired"
  createDirectoryIfMissing True (dtree <> "/sub")
  mapM_ (\f -> writeFile (dtree <> "/" <> f) "x\n") ["b.txt", "a.txt", "sub/c.txt"]
  dcanon <- canonicalizePath dtree
  let lines' ed = B.toLines (docBuffer (edDoc ed))
      cursorLine ed = posLine (rangeHead (primary (docSelection (edDoc ed))))
  listing <- ex ("o " <> T.pack dtree) start
  entered <- keys "ret" listing
  backUp <- keys "minus" entered
  caretUp <- keys "^" entered
  openedFile <- keys "j ret" listing
  backToListing <- keys "space D" =<< keys "space d" openedFile
  ofFile <- keys "space d" openedFile
  refused <- keys "i" listing
  refusedDelete <- keys "x d" listing
  refusedWrite <- ex "w" listing
  writeFile (dtree <> "/new.txt") ""
  refreshed <- keys "g r" listing
  cwd <- canonicalizePath "."
  removeDirectoryRecursive dtree
  -- File operations in a listing.
  let otree = dir <> "/him-test-ops"
      typeLine t ed = foldlM run ed (map charKey (T.unpack t) <> [plain KEnter])
      findEntry t = typeLine t <=< keys "/"
      exists p = doesPathExist (otree <> "/" <> p)
      entryLine ed = (\e -> deName e) <$> entryAt (cursorLine ed) (edDoc ed)
  createDirectoryIfMissing True otree
  mapM_ (\f -> writeFile (otree <> "/" <> f) "x\n") ["a.txt", "b.txt", "f1", "f2", ".dotfile"]
  ops <- ex ("o " <> T.pack otree) start
  canonOps <- canonicalizePath otree
  created <- typeLine "new.txt" =<< keys "a" ops
  createdDeep <- typeLine "deep/x.txt" =<< keys "a" ops
  createdDir <- typeLine "made/" =<< keys "a" ops
  createdPlus <- typeLine "plus" =<< keys "+" ops
  createdTwice <- typeLine "a.txt" =<< keys "a" ops
  newExists <- mapM exists ["new.txt", "deep/x.txt", "made", "plus"]
  -- Open a.txt, go back to the listing, rename a.txt: the buffer follows.
  withA <- keys "space d" =<< keys "ret" =<< findEntry "a.txt" ops
  renamed <- typeLine "renamed.txt" =<< keys "r backspace backspace backspace backspace backspace" =<< findEntry "a.txt" withA
  renameExists <- mapM exists ["a.txt", "renamed.txt"]
  notDeleted <- typeLine "n" =<< keys "d" =<< findEntry "b.txt" renamed
  bStill <- exists "b.txt"
  deleted <- typeLine "y" =<< keys "d" =<< findEntry "b.txt" renamed
  bGone <- exists "b.txt"
  deletedTwo <- typeLine "y" =<< keys "x x d" =<< findEntry "f1" deleted
  fsGone <- mapM exists ["f1", "f2"]
  deleteUp <- keys "g g j d" deleted
  hiddenShown <- keys "g ." ops
  -- Deleting a link to a directory removes the link, not the directory.
  let outside = dir <> "/him-test-ops-target"
  createDirectoryIfMissing True outside
  writeFile (outside <> "/keep.txt") "keep\n"
  createDirectoryLink outside (otree <> "/link")
  linkDeleted <- typeLine "y" =<< keys "d" =<< findEntry "link" =<< keys "g r" ops
  linkGone <- not <$> exists "link"
  targetKept <- doesPathExist (outside <> "/keep.txt")
  removeDirectoryRecursive outside
  removeDirectoryRecursive otree
  -- Reloading files changed by someone else.
  let fileR = dir <> "/him-test-reload.txt"
      fileS = dir <> "/him-test-reload2.txt"
      linesOf = B.toLines . docBuffer . edDoc
  writeFile fileR "one\ntwo\n"
  writeFile fileS "x\n"
  docR <- either (fail . T.unpack) pure =<< loadDocument fileR
  let startR = (newEditor (24, 80) docR) {edDoc = (edDoc (newEditor (24, 80) docR)) {docSelection = single (point (Pos 1 2))}}
  writeFile fileR "one\nTWO\nthree\n"
  reloadedR <- ex "reload" startR
  undoneR <- keys "u" reloadedR
  dirtyR <- keys "i X esc" startR
  refusedR <- ex "reload" dirtyR
  forcedR <- ex "reload!" dirtyR
  upToDate <- ex "reload" reloadedR
  withS <- ex ("o " <> T.pack fileS) dirtyR
  writeFile fileS "y\n"
  allReloaded <- ex "rla" withS
  removeFile fileS
  goneS <- ex "reload" allReloaded
  mapM_ removeFile [fileR]
  mapM_ removeFile [fileA, fileB]
  pure
    [ test ":reload reads the file again; the cursor stays" $
        assertEqual (["one", "TWO", "three"], Pos 1 2, False) (linesOf reloadedR, rangeHead (primary (docSelection (edDoc reloadedR))), docDirty (edDoc reloadedR))
    , test "a reload can be undone" (assertEqual ["one", "two"] (linesOf undoneR))
    , test ":reload refuses unsaved changes" (assertEqual (Just (Status Error "unsaved changes (use :reload! to discard them)"), "twXo") (edStatus refusedR, B.lineAt 1 (docBuffer (edDoc refusedR))))
    , test ":reload! discards them" (assertEqual (["one", "TWO", "three"], False) (linesOf forcedR, docDirty (edDoc forcedR)))
    , test "an unchanged file is up to date" (assertEqual (Just (Status Info "already up to date")) (edStatus upToDate))
    , test ":reload-all reloads clean buffers and skips modified ones" $
        assertEqual (["y"], Just (Status Info "reloaded 1 buffer(s), skipped 1 (unsaved changes or missing files)")) (linesOf allReloaded, edStatus allReloaded)
    , test "a deleted file is not reloaded" (assertEqual (Just (Status Error (T.pack fileS <> " no longer exists")), ["y"]) (edStatus goneS, linesOf goneS))
    , test "zipper: open, switch and close" $
        let e0 = newEditor (24, 80) (newDocument (Just "1") (buf "1"))
            e1 = openBuffer (newDocument (Just "2") (buf "2")) e0
            e2 = openBuffer (newDocument (Just "3") (buf "3")) (switchBuffer 1 e1)
         in assertEqual
              -- [1, 3, 2]: a new buffer opens right after the current one
              [(Just "3", (1, 3)), (Just "2", (2, 3)), (Just "2", (1, 2)), (Just "3", (1, 2))]
              [current e2, current (switchBuffer 1 e2), current (closeBuffer e2), current (closeBuffer (switchBuffer 1 e2))]
    , test "closing the only buffer leaves a scratch buffer" (assertEqual (Nothing, (0, 1)) (current (closeBuffer start)))
    , test ":o opens a second buffer and shows it" (assertEqual (b, (1, 2)) (current opened))
    , test "g n wraps around" (assertEqual (a, (0, 2)) (current nextWraps))
    , test ":e of an open file switches to it" (assertEqual (a, (0, 2)) (current reopened))
    , test ":bc closes the buffer" (assertEqual (a, (0, 1)) (current closed))
    , test ":o of a missing file opens an empty buffer" (assertEqual (Just fileNew, "") (docPath (edDoc newFile), B.toText (docBuffer (edDoc newFile))))
    , test ":q refuses with unsaved changes in another buffer" $
        assertEqual (False, Just (Status Error ("unsaved changes in " <> T.pack fileB <> " (use :q! to discard them, or :wq to save)"))) (edQuit quitDirty, edStatus quitDirty)
    , test ":bc refuses with unsaved changes" (assertEqual (b, (1, 2)) (current closeDirty))
    , test ":wa writes the other buffer and stays" (assertEqual ("Xbeta\n", a) (savedB, docPath (edDoc writtenAll)))
    , test ":q quits once everything is saved" (assertEqual True (edQuit quitAfter))
    , test ":n opens a scratch buffer" (assertEqual (Nothing, (1, 2)) (current scratch))
    , test "ret opens every marked file" $
        assertEqual (b, 2, Normal) (docPath (edDoc openedMarked), length (fst (buffers openedMarked)), edMode openedMarked)
    , test "space b picks a buffer" (assertEqual (a, (0, 2), Normal) (docPath (edDoc pickedBuffer), bufferIndex pickedBuffer, edMode pickedBuffer))
    , test "typing narrows the picker, backspace widens it" (assertEqual (Just ("", 2)) ((\p -> (pkQuery p, length (pkMatches p))) <$> edPicker pickerTyped))
    , test "esc closes the picker" (assertEqual (Nothing, Normal, b) (pkTitle <$> edPicker pickerEsc, edMode pickerEsc, docPath (edDoc pickerEsc)))
    , test "space f scans in the background" $
        assertEqual (Just (False, True)) ((\p -> (pkLoading p, any ((== "test/Spec.hs") . piLabel) (toList (pkItems p)))) <$> edPicker scanned)
    , test "typing in a large picker marks it stale and asks for a filter job" $
        assertEqual (Just True, True)
          (pkStale <$> edPicker typedBig, any (\case StartJob (FilterPicker 7 "needle" _) -> True; _ -> False) (edEffects typedBig))
    , test "the filter job's answer is applied" $
        assertEqual (Just (False, 25)) ((\p -> (pkStale p, pkMatchCount p)) <$> edPicker filteredBig)
    , test "an answer for an older query or another scan is dropped" $
        assertEqual (edPicker filteredBig, edPicker filteredBig) (edPicker staleDropped, edPicker otherScan)
    , test "space f opens the chosen file" (assertEqual (Just "test/Spec.hs", (1, 2)) (docPath (edDoc pickedFile), bufferIndex pickedFile))
    , test "space / finds the lines that contain the query" $
        assertEqual (Just (False, True))
          ((\p -> (pkLoading p, any (\i -> piDetail i == "maxHitText :: Int" && (case piTarget i of PickPosition f _ _ _ -> f == "src/Him/Grep.hs"; _ -> False)) (pkMatches p))) <$> edPicker searched)
    , test "space /: a new query keeps the old hits (stale) and searches again" $
        assertEqual (Just (True, True, pkMatches <$> edPicker searched), True)
          ((\p -> (pkLoading p, pkStale p, Just (pkMatches p))) <$> edPicker searchedAgain, any (\case StartJob (GrepFiles _ "maxHitText ::" _ _) -> True; _ -> False) (edEffects searchedAgain))
    , test "space /: hits for another query are dropped" (assertEqual (edPicker searched) (edPicker searchStale))
    , test "space / opens the hit at its line and column" $
        assertEqual ((\case PickPosition f l c _ -> Just (Just f, Pos l c); _ -> Nothing) . piTarget =<< firstHit)
          (Just (docPath (edDoc pickedHit), rangeHead (primary (docSelection (edDoc pickedHit)))))
    , test ":o of a directory lists it" $
        assertEqual (Just dcanon, [T.pack dcanon <> ":  (ret opens, - goes up)", "../", "sub/", "a.txt", "b.txt"], 2, Directory)
          (docPath (edDoc listing), lines' listing, cursorLine listing, keymapMode listing)
    , test "ret enters a directory in the same buffer" (assertEqual (Just (dcanon <> "/sub"), bufferIndex listing) (docPath (edDoc entered), bufferIndex entered))
    , test "^ goes up too" (assertEqual (Just dcanon) (docPath (edDoc caretUp)))
    , test "- goes up, onto the directory it came from" (assertEqual (Just dcanon, 2) (docPath (edDoc backUp), cursorLine backUp))
    , test "ret on a file opens it as a buffer" (assertEqual (Just (dcanon <> "/a.txt"), (2, 3)) (docPath (edDoc openedFile), bufferIndex openedFile))
    , test "space d shows the file's directory, on the file" (assertEqual (Just dcanon, 3, (1, 3)) (docPath (edDoc ofFile), cursorLine ofFile, bufferIndex ofFile))
    , test "space D opens the working directory" (assertEqual (Just cwd, Directory) (docPath (edDoc backToListing), keymapMode backToListing))
    , test "insert mode is refused in a listing" (assertEqual (Normal, Just (Status Error "a directory listing is read-only (ret opens an entry, - goes up)")) (edMode refused, edStatus refused))
    , test "deleting is refused in a listing" (assertEqual (lines' listing) (lines' refusedDelete))
    , test ":w is refused in a listing" (assertEqual (Just (Status Error "a directory listing cannot be written")) (edStatus refusedWrite))
    , test "g r lists the directory again" (assertEqual ["sub/", "a.txt", "b.txt", "new.txt"] (drop 2 (lines' refreshed)))
    , test "a creates files, directories and parents; + creates a directory" (assertEqual [True, True, True, True] newExists)
    , test "the cursor lands on what was created" (assertEqual [Just "new.txt", Just "deep", Just "made", Just "plus"] (map entryLine [created, createdDeep, createdDir, createdPlus]))
    , test "creating an existing name is refused" (assertEqual (Just (Status Error "a.txt already exists")) (edStatus createdTwice))
    , test "r renames, and open buffers follow" $
        assertEqual ([False, True], Just "renamed.txt", [canonOps <> "/renamed.txt"])
          (renameExists, entryLine renamed, [p | Just p <- map (docPath . bufDoc) (fst (buffers renamed)), (canonOps <> "/") `isPrefixOf` p])
    , test "d asks first; anything but y keeps the file" (assertEqual (True, Just (Status Info "nothing deleted")) (bStill, edStatus notDeleted))
    , test "d y deletes" (assertEqual (False, Just (Status Info "deleted 1 entry")) (bGone, edStatus deleted))
    , test "d deletes every selected entry" (assertEqual ([False, False], Just (Status Info "deleted 2 entries")) (fsGone, edStatus deletedTwo))
    , test "d on a link to a directory removes only the link" (assertEqual (True, True, Just (Status Info "deleted 1 entry")) (linkGone, targetKept, edStatus linkDeleted))
    , test "d on .. deletes nothing" (assertEqual (Just (Status Error "no entries selected")) (edStatus deleteUp))
    , test "dotfiles are hidden until g ." $
        assertEqual (True, Just ".dotfile")
          ( any ("1 hidden" `T.isInfixOf`) (take 1 (lines' ops))
          , (\e -> deName e) <$> listToMaybe [e | e <- entriesIn 2 100 (edDoc hiddenShown), "." `isPrefixOf` deName e]
          )
    , test "listFiles lists files sorted, skipping hidden entries" (assertEqual ["a.txt", "b.txt", "sub/c.txt", "sub/deeper/d.txt"] listed)
    , test "listFiles stops at the limit" (assertEqual ["a.txt", "b.txt"] listedFew)
    , test "listFiles lists hidden entries when asked (never .git)" $
        assertEqual [".dotfile", ".hidden/x.txt", "a.txt", "b.txt", "sub/c.txt", "sub/deeper/d.txt"] listedHidden
    , test "listFiles without .gitignore still honours .ignore" $
        assertEqual ["a.txt", "b.txt", "build/out.txt", "sub/c.txt", "sub/deeper/d.txt", "sub/keep.log", "sub/y.log", "x.log"] listedNoGit
    , test "listFiles can leave linked directories alone" (assertEqual ["real/r.txt"] listedNoLinks)
    , test "listFiles honours .gitignore and .ignore at every level" $
        assertEqual ["a.txt", "b.txt", "sub/c.txt", "sub/deeper/d.txt", "sub/keep.log"] listedIgnoring
    , test "listFiles follows a directory link once and never a cycle" $
        assertEqual ["alias/r.txt", "real/r.txt"] listedLinks
    , test "listFiles below a repository root uses the root's ignore files" $
        assertEqual ["keep.log", "y.log"] listedInRepo
    ]

-- | Files larger than the read chunk, with multi-byte characters straddling
-- chunk boundaries, load exactly like an in-memory decode.
loadingTests :: IO [Test]
loadingTests = do
  dir <- getTemporaryDirectory
  let path = dir <> "/him-test-load.txt"
      line i = T.pack (show i) <> " æøå 漢字 😀 lorem ipsum\r\n"
      bytes = TE.encodeUtf8 (T.concat (map line [1 .. 60000 :: Int]) <> "tail")
      summary d = (B.toLines (docBuffer d), docLineEnding d, docTrailingNewline d)
      expected = summary (decodeDocument (Just path) bytes)
  BS.writeFile path bytes
  loaded <- loadDocument path
  -- Saving an unedited large file writes its pinned regions directly; an
  -- edit in the middle mixes both paths.
  savedBytes <- case loaded of
    Right d -> do
      let d' = d {docBuffer = fst (B.insertText (Pos 30000 2) "EDIT" (docBuffer d))}
      _ <- saveDocument (path <> ".out") d
      plainSave <- BS.readFile (path <> ".out")
      _ <- saveDocument (path <> ".out") d'
      editedSave <- BS.readFile (path <> ".out")
      removeFile (path <> ".out")
      pure (Just (plainSave, editedSave, encodeDocument d'))
    Left _ -> pure Nothing
  -- Invalid UTF-8 takes the lenient decoding fallback.
  let invalid = "ok\n\255\254 bad\r\nend" :: ByteString
  BS.writeFile path invalid
  loadedInvalid <- loadDocument path
  removeFile path
  -- What pipes get (no known size): chunked reads, with characters and
  -- lines straddling the 1 MB chunks.
  BS.writeFile path bytes
  loadedChunked <- loadDocumentChunked path
  removeFile path
  pure
    [ test "large file loads like an in-memory decode" (assertEqual (Right expected) (summary <$> loaded))
    , test "saving a loaded file writes the same bytes" (assertEqual (Just bytes) ((\(a, _, _) -> a) <$> savedBytes))
    , test "saving after an edit matches encodeDocument" (assertEqual True (maybe False (\(_, b, c) -> b == c) savedBytes))
    , test "invalid UTF-8 loads like a lenient decode" $
        assertEqual (Right (summary (decodeDocument (Just path) invalid))) (summary <$> loadedInvalid)
    , test "chunked reads (pipes) load the same" (assertEqual (Right expected) (summary <$> loadedChunked))
    ]

processTests :: IO [Test]
processTests = do
  echoed <- runProcess "cat" [] Nothing "hello"
  failing <- runProcess "sh" ["-c", "echo err >&2; exit 3"] Nothing ""
  missing <- runProcess "him-no-such-command" [] Nothing ""
  let big = BS.replicate 3000000 65
  bigEcho <- runProcess "cat" [] Nothing big
  pure
    [ test "stdin goes in, stdout comes out" (assertEqual (Right (ExitSuccess, "hello", "")) (summary <$> echoed))
    , test "stderr and the exit code" (assertEqual (Right (ExitFailure 3, "", "err\n")) (summary <$> failing))
    , test "a missing program is an error, not an exception" (assertEqual True (either (const True) (const False) missing))
    , test "3 MB through a pipe both ways does not deadlock" (assertEqual (Right 3000000) (BS.length . prStdout <$> bigEcho))
    ]
  where
    summary r = (prExit r, prStdout r, prStderr r)
