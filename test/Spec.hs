module Main (main) where

import Control.Monad (when, (<=<))
import Control.Monad.Trans.State.Strict (execStateT)
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.ByteString.Builder (toLazyByteString)
import Data.ByteString.Lazy qualified as BL
import Data.Foldable (foldlM)
import Data.List (isPrefixOf, nub, sort)
import Data.Maybe (fromMaybe, listToMaybe)
import Data.Text (Text)
import Him.Action
import Him.App (handleEvent)
import Him.Buffer qualified as B
import Data.Map.Strict qualified as Map
import Him.Config (Config (..))
import Him.Config.Default (allActions, configWith, defaultConfig)
import Him.Document
import Him.Edit
import Him.Editor
import Him.Event (Event (..))
import Him.Ex (parseExLine)
import Him.Search (Direction (..), Match (..), compileNeedle, findMatch, selectMatches)
import Him.Commands.Search (refreshSearchPreview)
import Him.History qualified as H
import Him.File (decodeChunks, decodeDocument, encodeDocument, loadDocument, loadDocumentChunked, saveDocument)
import Data.Text.Encoding qualified as TE
import System.Directory (findExecutable, getHomeDirectory, canonicalizePath, createDirectoryIfMissing, createDirectoryLink, doesPathExist, getTemporaryDirectory, removeDirectoryRecursive, removeFile)
import Him.FileTree (listFiles)
import Him.Picker
import Him.Effect (Effect (..), Job (..), JobResult (..))
import Him.Runtime (Runtime, newRuntime)
import Him.Runtime qualified as Runtime
import Control.Concurrent.STM (TChan, atomically, newTChanIO, readTChan, writeTChan)
import System.Timeout (timeout)
import Him.Ignore
import Him.Diff
import Him.Syntax
import Him.Regex
import Him.Lsp.Protocol
import Him.Lsp.State (Attachment (..), Completion (..), DocLsp (..), ServerInfo (..), ShownDiagnostic (..), Sync (..), shownDiagnostics)
import Him.Lsp.Sync (syncMessages)
import Him.Commands.Lsp (lspFlush)
import GHC.Clock (getMonotonicTime)
import Him.Syntax.TreeSitter (findRuntime, readQuery, treeSitter)
import System.FilePath ((</>))
import Data.List (find)
import Data.Maybe (isJust)
import Him.Language (detectLanguage, langName, languages)
import Data.IORef (newIORef, readIORef, writeIORef)
import Him.GitState
import Him.Commands.Git (gitHousekeeping)
import Data.IntMap.Strict qualified as IntMap
import Him.Palette (paletteItems)
import Him.Json hiding (path)
import Him.Json qualified as J
import Him.Process (ProcessResult (..), runProcess)
import System.Exit (ExitCode (..))
import Him.Directory (entriesIn, entryAt, listingDocument)
import Him.Key
import Him.Keymap
import Him.Mode (Mode (..))
import Him.Motion
import Him.Position (Pos (..))
import Him.Render.Diff (diffFrames)
import Him.Render.Frame (blankFrame, putCells, putText)
import Him.Selection
import Him.Terminal.Ansi (Color (..), Style (..), defaultStyle, packStyle, sgr, unpackStyle)
import Him.Terminal.Input (decodeKeys)
import Him.View (View (..), scrollToCursor)
import Him.TextWidth (charIndexAtCol, charWidth, displayCol, glyphs, isWide)
import Him.Render (render)
import Him.Render.Frame (Cell (..), Frame (..), ScrollInfo (..), continuation)
import Him.Render.Theme (Theme (..), defaultTheme, scopeStyle)
import Data.Foldable (toList)
import Data.Sequence qualified as Seq
import Data.Text qualified as T
import Test.Harness

-- | Collapse runs of spaces in the details (they are padded into columns).
squeeze :: [(Text, Text)] -> [(Text, Text)]
squeeze = map (fmap (T.unwords . T.words))

firstText :: [Text] -> Text
firstText = \case
  x : _ -> x
  [] -> ""

-- | Keys through a configuration with user bindings on top of the defaults.
rebindTests :: IO [Test]
rebindTests = do
  config <-
    either (fail . T.unpack) pure $
      configWith
        ( Map.fromList
            [ (Normal, [("C-d", "move_line_down 2"), ("j", "no_op"), ("space i", "insert_text \"// \""), ("Q", "ex q!"), ("g 3", "goto_line 3"), ("F", "search_text two")])
            , (Insert, [("C-a", "set_mode normal")])
            ]
        )
  let start t = newEditor (24, 80) (newDocument Nothing (buf t))
      typeKeys ks ed = foldlM (\e k -> execStateT (handleEvent config (EvKey k)) e) ed (fromMaybe (error ("bad keys: " <> show ks)) (parseKeys ks))
      headAfter t ks = rangeHead . primary . docSelection . edDoc <$> typeKeys ks (start t)
      textAfter t ks = B.toText . docBuffer . edDoc <$> typeKeys ks (start t)
  ctrlD <- headAfter "a\nb\nc\nd" "C-d"
  disabled <- headAfter "a\nb" "j"
  inherited <- headAfter "a\nb\nc\nd" "v C-d"
  inserted <- textAfter "x" "space i"
  quitByKey <- typeKeys "i y esc Q" (start "")
  gotoThree <- headAfter "a\nb\nc\nd" "g 3"
  gotoStill <- headAfter "a\nb\nc\nd" "g e g g"
  searchKey <- headAfter "one two" "F"
  insertExit <- typeKeys "i C-a" (start "")
  palette <- typeKeys "space ?" (start "a\nb")
  paletteDone <- typeKeys "space ? g o t o _ f i l e _ s t a r t ret" =<< typeKeys "j" (start "a\nb")
  let paletteRan = rangeHead (primary (docSelection (edDoc paletteDone)))
  paletteArgs <- typeKeys "space ? g o t o _ l i n e ret" (start "a\nb")
  actionByName <- headAfter "a\nb\nc\nd" ": a c t i o n space g o t o _ l i n e space 3 ret"
  actionBad <- typeKeys ": a c t i o n space g o t o _ l i n e ret" (start "x")
  let idsBefore = start "x"
  idsAfter <- typeKeys ": n ret" idsBefore
  idsTyped <- typeKeys "i a b esc" idsBefore
  let badConfig =
        configWith
          ( Map.fromList
              [ (Normal, [("a", "fly"), ("b", "goto_line")])
              , (Insert, [("C-x", "set_mode command")])
              ]
          )
  pure
    [ test "a key bound to an action with an argument" (assertEqual (Pos 2 0) ctrlD)
    , test "no_op disables a default key" (assertEqual (Pos 0 0) disabled)
    , test "select mode inherits user normal bindings" (assertEqual (Pos 2 0) inherited)
    , test "insert_text with a quoted argument" (assertEqual "// x" inserted)
    , test "ex runs a : command" (assertEqual True (edQuit quitByKey))
    , test "a new chord next to default ones" (assertEqual (Pos 2 0) gotoThree)
    , test "default chords on the same prefix still work" (assertEqual (Pos 0 0) gotoStill)
    , test "search_text selects the match" (assertEqual (Pos 0 6) searchKey)
    , test "set_mode" (assertEqual Normal (edMode insertExit))
    , test "space ? opens the palette" (assertEqual (Just "commands", Picking) (pkTitle <$> edPicker palette, edMode palette))
    , test "the palette runs the chosen action" (assertEqual (Pos 0 0, Normal) (paletteRan, edMode paletteDone))
    , test "an action with arguments is completed on the : line" (assertEqual (CmdLine, "action goto_line ") (edMode paletteArgs, edCmdLine paletteArgs))
    , test "palette rows show parameters, keys and docs" $
        let rows = paletteItems config Normal
            row n = [(piLabel i, piDetail i) | i <- rows, piTarget i `elem` [PickAction n True, PickAction n False]]
         in assertEqual
              [ [("goto_line <line>", "g 3 (3) Go to a line (counting from 1)")]
              , [("insert_newline", "insert: ret Insert a line break")]
              ]
              [squeeze (row "goto_line"), squeeze (row "insert_newline")]
    , test "palette descriptions line up" $
        let detailOf n = firstText [piDetail i | i <- paletteItems config Normal, piTarget i == PickAction n False]
            column n doc = T.length (fst (T.breakOn doc (detailOf n)))
         in assertEqual (column "goto_file_start" "Go to the first") (column "select_all" "Select the whole")
    , test "palette keys include rebound ones with their arguments" $
        assertEqual True (any (\i -> piTarget i == PickAction "move_line_down" False && "C-d (2)" `T.isInfixOf` piDetail i) (paletteItems config Normal))
    , test ":action runs an action by its invocation" (assertEqual (Pos 2 0) actionByName)
    , test ":action reports a bad invocation" (assertEqual (Just (Status Error "goto_line: missing argument <line>")) (edStatus actionBad))
    , test "documents get ids, and edits bump the version" $
        assertEqual (1, 2, 0, 2) (docId (edDoc idsBefore), docId (edDoc idsAfter), docVersion (edDoc idsBefore), docVersion (edDoc idsTyped))
    , test "every bad binding is reported" $
        assertEqual
          ( Left
              ( T.intercalate
                  "\n"
                  [ "Normal mode, a: unknown action: fly"
                  , "Normal mode, b: goto_line: missing argument <line>"
                  , "Insert mode, C-x: set_mode: <mode> must be one of normal, insert, select, got command"
                  ]
              )
          )
          (() <$ badConfig)
    , test "the default keymaps cover every mode" (assertEqual [minBound .. maxBound] (Map.keys (cfgKeymaps (either (error . T.unpack) id defaultConfig))))
    ]

-- | Like the main loop: start the jobs the editor asked for on a real
-- runtime and handle their results as events, until nothing arrives for a
-- while (results may ask for more jobs).
settle :: Config -> Editor -> IO Editor
settle config ed0 = do
  rt <- testRuntime config
  settleWith config rt ed0

-- | A runtime and its event channel, to share between 'settleWith' calls
-- (state such as highlighters lives in the runtime, as in the editor).
testRuntime :: Config -> IO (Runtime, TChan Event)
testRuntime config = do
  events <- newTChanIO
  runtime <- newRuntime (cfgSyntaxProviders config) (atomically . writeTChan events)
  pure (runtime, events)

-- | Handle job results until the editor satisfies a condition, or a
-- timeout (milliseconds) passes. For answers that take a while (language
-- servers).
settleUntil :: Config -> (Runtime, TChan Event) -> Int -> (Editor -> Bool) -> Editor -> IO Editor
settleUntil config (runtime, events) ms done ed0 = do
  deadline <- (+ fromIntegral ms / 1000) <$> getMonotonicTime
  let loop ed
        | done ed = pure ed
        | otherwise = do
            mapM_ (Runtime.perform runtime) (edEffects ed)
            now <- getMonotonicTime
            next <- timeout (max 1 (round ((deadline - now) * 1000000))) (atomically (readTChan events))
            case next of
              Nothing -> pure ed {edEffects = []}
              Just ev -> do
                ed' <- execStateT (handleEvent config ev) ed {edEffects = []}
                -- Like the main loop: send the text before the next frame.
                execStateT lspFlush ed' >>= loop
  loop ed0

settleWith :: Config -> (Runtime, TChan Event) -> Editor -> IO Editor
settleWith config (runtime, events) ed0 = do
  let loop ed = do
        mapM_ (Runtime.perform runtime) (edEffects ed)
        next <- timeout 300000 (atomically (readTChan events))
        case next of
          Nothing -> pure ed {edEffects = []}
          Just ev -> execStateT (handleEvent config ev) ed {edEffects = []} >>= loop
  loop ed0

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
  -- listFiles on a small tree with a hidden directory.
  let tree = dir <> "/him-test-tree"
  createDirectoryIfMissing True (tree <> "/sub/deeper")
  createDirectoryIfMissing True (tree <> "/.hidden")
  mapM_ (\f -> writeFile (tree <> "/" <> f) "") ["b.txt", "a.txt", "sub/c.txt", "sub/deeper/d.txt", ".hidden/x.txt", ".dotfile"]
  listed <- listFiles 100 tree
  listedFew <- listFiles 2 tree
  createDirectoryIfMissing True (tree <> "/build")
  mapM_ (\f -> writeFile (tree <> "/" <> f) "") ["x.log", "sub/y.log", "sub/keep.log", "build/out.txt", "sub/deeper/gen.txt"]
  writeFile (tree <> "/.gitignore") "*.log\nbuild/\n"
  writeFile (tree <> "/sub/.gitignore") "!keep.log\n"
  writeFile (tree <> "/sub/.ignore") "deeper/gen.txt\n"
  listedIgnoring <- listFiles 100 tree
  -- Started below a repository root, the root's ignore files still apply.
  createDirectoryIfMissing True (tree <> "/.git/info")
  writeFile (tree <> "/.gitignore") "/sub/c.txt\n"
  writeFile (tree <> "/.git/info/exclude") "d.txt\n"
  listedInRepo <- listFiles 100 (tree <> "/sub")
  -- Links: a link to a directory is followed; a link back up is not.
  let ltree = dir <> "/him-test-links"
  createDirectoryIfMissing True (ltree <> "/real")
  writeFile (ltree <> "/real/r.txt") ""
  createDirectoryLink (ltree <> "/real") (ltree <> "/alias")
  createDirectoryLink ltree (ltree <> "/real/loop")
  listedLinks <- listFiles 100 ltree
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
  mapM_ removeFile [fileA, fileB]
  pure
    [ test "zipper: open, switch and close" $
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
    , test ":o of a directory lists it" $
        assertEqual (Just dcanon, [T.pack dcanon <> ":", "../", "sub/", "a.txt", "b.txt"], 2, Directory)
          (docPath (edDoc listing), lines' listing, cursorLine listing, keymapMode listing)
    , test "ret enters a directory in the same buffer" (assertEqual (Just (dcanon <> "/sub"), bufferIndex listing) (docPath (edDoc entered), bufferIndex entered))
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
          ( any ("(1 hidden" `T.isInfixOf`) (take 1 (lines' ops))
          , (\e -> deName e) <$> listToMaybe [e | e <- entriesIn 2 100 (edDoc hiddenShown), "." `isPrefixOf` deName e]
          )
    , test "listFiles lists files sorted, skipping hidden entries" (assertEqual ["a.txt", "b.txt", "sub/c.txt", "sub/deeper/d.txt"] listed)
    , test "listFiles stops at the limit" (assertEqual ["a.txt", "b.txt"] listedFew)
    , test "listFiles honours .gitignore and .ignore at every level" $
        assertEqual ["a.txt", "b.txt", "sub/c.txt", "sub/deeper/d.txt", "sub/keep.log"] listedIgnoring
    , test "listFiles follows a directory link once and never a cycle" $
        assertEqual ["alias/r.txt", "real/r.txt"] listedLinks
    , test "listFiles below a repository root uses the root's ignore files" $
        assertEqual ["keep.log", "y.log"] listedInRepo
    ]

main :: IO ()
main = do
  integration <- integrationTests
  rebinding <- rebindTests
  processes <- processTests
  gitIO <- gitTests
  syntaxIO <- syntaxIOTests
  treeSitterIO <- treeSitterTests
  lspIO <- lspTests
  bufferIO <- openBufferTests
  loading <- loadingTests
  runTests
    [ group "Him.Key" keyTests
    , group "Him.Terminal.Input.decodeKeys" decodeTests
    , group "Him.Buffer" bufferTests
    , group "Him.Buffer (randomized against a list model)" ropeModelTests
    , group "Him.Buffer.changeBetween" changeTests
    , group "Him.Search (randomized against a naive search)" searchTests
    , group "Him.Motion" motionTests
    , group "Him.Edit" editTests
    , group "Him.History" historyTests
    , group "Him.Keymap" keymapTests
    , group "Him.Action" actionTests
    , group "Him.Picker" pickerTests
    , group "Him.Ignore" ignoreTests
    , group "Him.Json" jsonTests
    , group "Him.Diff" diffTests'
    , group "Him.GitState" gitStateTests
    , group "git (in a temporary repository)" gitIO
    , group "Him.Syntax" syntaxTests
    , group "Him.Regex" regexTests
    , group "Him.Lsp.Protocol" lspProtocolTests
    , group "Him.Lsp.Sync" syncTests
    , group "LSP client (with clangd)" lspIO
    , group "highlighting through a provider" syntaxIO
    , group "Him.Syntax.TreeSitter (with the installed grammars)" treeSitterIO
    , group "Him.Process" processes
    , group "multiple selections" multiSelectionTests
    , group "Him.File" fileTests
    , group "Him.Ex" exTests
    , group "Him.View" viewTests
    , group "Him.TextWidth" widthTests
    , group "Him.Render" renderTests
    , group "Him.Render.Diff" diffTests
    , group "keys through the default config" integration
    , group "rebinding keys to actions" rebinding
    , group "buffers" bufferIO
    , group "Him.File (from disk)" loading
    ]

keyTests :: [Test]
keyTests =
  [ test "parse plain char" (assertEqual (Just (plain (KChar 'a'))) (parseKey "a"))
  , test "parse ctrl" (assertEqual (Just (ctrl 's')) (parseKey "C-s"))
  , test "parse named" (assertEqual (Just (plain KEnter)) (parseKey "ret"))
  , test "parse F-key" (assertEqual (Just (plain (KF 5))) (parseKey "F5"))
  , test "parse sequence" (assertEqual (Just [plain (KChar 'g'), plain (KChar 'g')]) (parseKeys "g g"))
  , test "lone C is the letter" (assertEqual (Just (plain (KChar 'C'))) (parseKey "C"))
  , test "C-- is ctrl minus" (assertEqual (Just (ctrl '-')) (parseKey "C--"))
  , test "unknown name fails" (assertEqual Nothing (parseKey "nope"))
  , group "show/parse round trip" [roundTrip t | t <- ["a", "C-s", "A-S-left", "space", "ret", "F12", "C-A-x", "minus"]]
  ]
  where
    roundTrip :: Text -> Test
    roundTrip t = test (show t) (assertEqual (Just t) (showKey <$> parseKey t))

decodeTests :: [Test]
decodeTests =
  [ decodes "ascii" "ab" [ch 'a', ch 'b']
  , decodes "enter / tab / backspace" "\r\t\DEL" [plain KEnter, plain KTab, plain KBackspace]
  , decodes "ctrl letters" "\SOH\DC3" [ctrl 'a', ctrl 's']
  , decodes "arrows" "\ESC[A\ESC[B\ESC[C\ESC[D" [plain KUp, plain KDown, plain KRight, plain KLeft]
  , decodes "ss3 arrows" "\ESCOA" [plain KUp]
  , decodes "ctrl-right" "\ESC[1;5C" [withMod Ctrl (plain KRight)]
  , decodes "shift-alt-up" "\ESC[1;4A" [withMod Shift (withMod Alt (plain KUp))]
  , decodes "delete, pgup" "\ESC[3~\ESC[5~" [plain KDelete, plain KPageUp]
  , decodes "F5" "\ESC[15~" [plain (KF 5)]
  , decodes "shift-tab" "\ESC[Z" [withMod Shift (plain KTab)]
  , decodes "alt-x" "\ESCx" [alt 'x']
  , decodes "utf-8" "\195\166\226\130\172" [ch 'æ', ch '€']
  , decodes "unknown CSI is dropped" "\ESC[99xa" [ch 'a']
  , test "lone ESC waits for more" (assertEqual ([], "\ESC") (decodeKeys False "\ESC"))
  , test "lone ESC on timeout is Esc" (assertEqual ([plain KEsc], "") (decodeKeys True "\ESC"))
  , test "partial CSI waits" (assertEqual ([ch 'a'], "\ESC[1;") (decodeKeys False "a\ESC[1;"))
  , test "partial utf-8 waits" (assertEqual ([], "\226\130") (decodeKeys False "\226\130"))
  ]
  where
    ch = plain . KChar
    decodes :: String -> ByteString -> [Key] -> Test
    decodes name input expected = test name (assertEqual (expected, "") (decodeKeys False input))

buf :: Text -> B.Buffer
buf = B.fromText

bufferTests :: [Test]
bufferTests =
  [ test "empty has one line" (assertEqual 1 (B.lineCount B.empty))
  , test "fromText/toText round trip" (assertEqual "a\nb\n" (B.toText (buf "a\nb\n")))
  , test "insert in line" (assertEqual ("abXc", Pos 0 3) (ins (Pos 0 2) "X" "abc"))
  , test "insert newline" (assertEqual ("ab\nc", Pos 1 0) (ins (Pos 0 2) "\n" "abc"))
  , test "insert multi-line" (assertEqual ("aX\nYZ\nYb", Pos 2 1) (ins (Pos 0 1) "X\nYZ\nY" "ab"))
  , test "delete within line" (assertEqual "ac" (B.toText (B.deleteRange (Pos 0 1) (Pos 0 2) (buf "abc"))))
  , test "delete across lines" (assertEqual "ad" (B.toText (B.deleteRange (Pos 0 1) (Pos 1 1) (buf "ab\ncd"))))
  , test "textRange across lines" (assertEqual "b\nc" (B.textRange (Pos 0 1) (Pos 1 1) (buf "ab\ncd")))
  , test "nextPos crosses line end" (assertEqual (Pos 1 0) (B.nextPos (buf "a\nb") (Pos 0 1)))
  , test "nextPos stops at end" (assertEqual (Pos 1 1) (B.nextPos (buf "a\nb") (Pos 1 1)))
  , test "prevPos crosses line start" (assertEqual (Pos 0 1) (B.prevPos (buf "a\nb") (Pos 1 0)))
  , test "charAt line end is newline" (assertEqual (Just '\n') (B.charAt (Pos 0 1) (buf "a\nb")))
  , test "charAt end of file" (assertEqual Nothing (B.charAt (Pos 1 1) (buf "a\nb")))
  ]
  where
    ins :: Pos -> Text -> Text -> (Text, Pos)
    ins p t s = let (b', p') = B.insertText p t (buf s) in (B.toText b', p')

-- | A tiny deterministic pseudo-random generator (no QuickCheck: boot
-- libraries only).
randoms :: Int -> [Int]
randoms = drop 1 . iterate (\x -> (x * 6364136223846793005 + 1442695040888963407) `mod` (2 ^ (62 :: Int)))

changeTests :: [Test]
changeTests =
  [ test "equal texts have no change" (assertEqual Nothing (B.changeBetween (buf "a\nb") (buf "a\nb")))
  , test "a typed character" (assertEqual (Just (Pos 1 2, Pos 1 2, "X")) (B.changeBetween (buf "ab\ncd\nef") (buf "ab\ncdX\nef")))
  , test "a line added at the end" (assertEqual (Just (Pos 1 1, Pos 1 1, "\nc")) (B.changeBetween (buf "a\nb") (buf "a\nb\nc")))
  , test "the last line removed" (assertEqual (Just (Pos 0 1, Pos 1 1, "")) (B.changeBetween (buf "a\nb") (buf "a")))
  , test "random edits: the change rebuilds the new text" (mapM_ changeModel (take 500 (chunks (randoms 31))))
  , test "edits in a large loaded file are found" $
      let big = B.fromRegions [(False, T.intercalate "\n" [T.pack (show i) | i <- [n .. n + 999 :: Int]]) | n <- [0, 1000 .. 9000]]
          (edited, _) = B.insertText (Pos 5500 1) "X" big
       in assertEqual (Just (Pos 5500 1, Pos 5500 1, "X")) (B.changeBetween big edited)
  ]
  where
    chunks xs = let (a, z) = splitAt 6 xs in a : chunks z
    alphabet = ["a", "bc", "", "def"]
    changeModel rs = case rs of
      (r1 : r2 : r3 : r4 : r5 : _) ->
        let old = T.intercalate "\n" [alphabet !! ((r1 `div` (4 ^ i)) `mod` 4) | i <- [0 .. r2 `mod` 6 :: Int]]
            cut = r3 `mod` (T.length old + 1)
            len = r4 `mod` 4
            ins = ["", "x", "\n", "y\nz\n", "\n\n"] !! (r5 `mod` 5)
            new = T.take cut old <> ins <> T.drop (cut + len) old
         in case B.changeBetween (buf old) (buf new) of
              Nothing -> if old == new then Right () else Left ("missed: " <> show (old, new))
              Just (s0, e0, t) ->
                let rebuilt = T.take (offsetOf old s0) old <> t <> T.drop (offsetOf old e0) old
                 in if rebuilt == new then Right () else Left (show (old, new, s0, e0, t))
      _ -> Right ()
    offsetOf t (Pos l c) = sum [T.length x + 1 | x <- take l (T.splitOn "\n" t)] + c

ropeModelTests :: [Test]
ropeModelTests =
  [ test "fromRegions joins regions" (assertEqual ["a", "b", "c", "d"] (B.toLines (B.fromRegions [(False, "a\nb"), (False, "c\nd")])))
  , test "CR regions strip \\r" (assertEqual ["a", "b"] (B.toLines (B.fromRegions [(True, "a\r\nb\r")])))
  , test "2000 random edits match the model" (runModel 2000 1)
  , test "edits on a single line" (runModel 500 7)
  ]
  where
    start = B.fromRegions [(False, T.intercalate "\n" [T.pack ("line " <> show i) | i <- [n .. n + 49 :: Int]]) | n <- [0, 50 .. 450]]
    model0 = B.toLines start
    runModel steps seed = go steps (randoms seed) start model0
    go :: Int -> [Int] -> B.Buffer -> [Text] -> Either String ()
    go 0 _ b m = check b m
    go n (r1 : r2 : r3 : r4 : rs) b m = case check b m of
      Left e -> Left ("before step " <> show n <> ": " <> e)
      Right () ->
        let lineN = length m
            l1 = r1 `mod` lineN
            c1 = r2 `mod` (T.length (m !! l1) + 1)
            p1 = Pos l1 c1
         in if even r3
              then
                let t = ["x", "\n", "ab\ncd", "\n\n", "é"] !! (r4 `mod` 5)
                    (b', _) = B.insertText p1 t b
                 in go (n - 1) rs b' (modelInsert m p1 t)
              else
                let l2 = min (lineN - 1) (l1 + r4 `mod` 3)
                    c2 = (r4 `div` 3) `mod` (T.length (m !! l2) + 1)
                    p2 = max p1 (Pos l2 c2)
                 in go (n - 1) rs (B.deleteRange p1 p2 b) (modelDelete m p1 p2)
    go _ _ b m = check b m
    check b m
      | B.toLines b /= m = Left ("lines differ: " <> show (take 3 (B.toLines b)) <> " vs " <> show (take 3 m))
      | B.lineCount b /= length m = Left "lineCount differs"
      | [B.lineAt i b | i <- [0 .. length m - 1]] /= m = Left "lineAt differs"
      | B.linesDownFrom (length m - 1) b /= reverse m = Left "linesDownFrom differs"
      | otherwise = Right ()
    modelText = T.intercalate "\n"
    offsetOf m (Pos l c) = sum [T.length x + 1 | x <- take l m] + c
    modelInsert m p t = let whole = modelText m; (a, z) = T.splitAt (offsetOf m p) whole in T.splitOn "\n" (a <> t <> z)
    modelDelete m p q = let whole = modelText m in T.splitOn "\n" (T.take (offsetOf m p) whole <> T.drop (offsetOf m q) whole)

searchTests :: [Test]
searchTests =
  [ test "empty and multi-line patterns are rejected" (assertEqual (Nothing, Nothing) (compileNeedle "", compileNeedle "a\nb"))
  , test "smart case: lower-case pattern ignores case" (assertEqual (Just (Pos 0 4)) (matchStart <$> findIn "abc ABC" "abc" (Pos 0 0)))
  , test "smart case: upper-case pattern is exact" (assertEqual (Just (Pos 0 4)) (matchStart <$> findIn "abc ABC Abc" "ABC" (Pos 0 0)))
  , test "match end is inclusive" (assertEqual (Just (Pos 0 6)) (matchEnd <$> findIn "abc ABC" "abc" (Pos 0 0)))
  , test "wraps around" (assertEqual (Just (Match (Pos 0 0) (Pos 0 1) True)) (findIn "ab ab" "ab" (Pos 0 3)))
  , test "multi-byte columns" (assertEqual (Just (Pos 0 3)) (matchStart <$> findIn "漢字 x" "x" (Pos 0 0)))
  , test "1500 random searches match the naive search" (randomSearches 1500)
  ]
  where
    findIn t p pos = compileNeedle p >>= \n -> findMatch Forward n (buf t) pos
    alphabet = "aAbB é漢\t" :: String
    randomSearches :: Int -> Either String ()
    randomSearches count = go count (randoms 99)
      where
        go 0 _ = Right ()
        go k (r1 : r2 : r3 : r4 : r5 : r6 : rs) =
          let (b, rs') = randomBuffer r1 rs
              ls = B.toLines b
              needle = T.pack [alphabet !! (x `mod` length alphabet) | x <- take (1 + r2 `mod` 3) rs']
              l = r3 `mod` length ls
              pos = Pos l (r4 `mod` (T.length (ls !! l) + 1))
              dir = if even r5 then Forward else Backward
              expected = naive dir needle ls pos
              actual = (\n -> matchStart <$> findMatch dir n b pos) =<< compileNeedle needle
           in if expected == actual
                then go (k - 1) (drop 6 rs')
                else Left (show (dir, needle, pos, ls) <> ": expected " <> show expected <> ", got " <> show actual <> show r6)
        go _ _ = Right ()
    -- Several regions (blocks) plus a few edits, so searches cross block
    -- boundaries and edited single-line blocks.
    randomBuffer r rs =
      let lineOf xs = T.pack [alphabet !! (x `mod` length alphabet) | x <- xs]
          rows = [lineOf (take (x `mod` 7) (drop (i * 7) rs)) | (i, x) <- zip [0 .. 11] (drop 100 rs)]
          regions = [(False, T.intercalate "\n" chunk) | chunk <- chunksOf (1 + r `mod` 4) rows]
          b0 = B.fromRegions regions
          b1 = fst (B.insertText (Pos (r `mod` B.lineCount b0) 0) (lineOf (take 3 (drop 200 rs))) b0)
       in (b1, drop 300 rs)
    chunksOf n xs = case splitAt n xs of
      (a, []) -> [a]
      (a, z) -> a : chunksOf n z
    -- All match starts in document order, overlapping matches included.
    naive dir needle ls pos =
      let fold = not (T.any isUpperAscii needle)
          norm = if fold then T.map lowerAscii else id
          n = norm needle
          starts = [Pos li c | (li, line) <- zip [0 ..] ls, let ln = norm line, c <- [0 .. T.length ln - T.length n], n `T.isPrefixOf` T.drop c ln]
       in case dir of
            Forward -> case filter (> pos) starts of
              (p : _) -> Just p
              [] -> case starts of
                (p : _) -> Just p
                [] -> Nothing
            Backward -> case reverse (filter (< pos) starts) of
              (p : _) -> Just p
              [] -> case reverse starts of
                (p : _) -> Just p
                [] -> Nothing
    isUpperAscii c = c >= 'A' && c <= 'Z'
    lowerAscii c = if isUpperAscii c then toEnum (fromEnum c + 32) else c

-- | Apply a motion to a collapsed range at a position; returns (anchor, head).
runMotion :: Motion -> Text -> Pos -> (Pos, Pos)
runMotion m t p = let r = m (buf t) (point p) in (rangeAnchor r, rangeHead r)

motionTests :: [Test]
motionTests =
  [ test "h wraps to previous line end" (assertEqual (Pos 0 2, Pos 0 2) (runMotion charLeft "ab\ncd" (Pos 1 0)))
  , test "j keeps desired column" $
      let b = buf "abcd\na\nabcd"
          r1 = lineDown b (point (Pos 0 3))
          r2 = lineDown b r1
       in assertEqual (Pos 1 1, Pos 2 3) (rangeHead r1, rangeHead r2)
  , test "w selects word and trailing blanks" (assertEqual (Pos 0 0, Pos 0 5) (runMotion nextWordStart "hello world" (Pos 0 0)))
  , test "w from end of word selects next word" (assertEqual (Pos 0 6, Pos 0 10) (runMotion nextWordStart "hello world" (Pos 0 5)))
  , test "w skips to next line" (assertEqual (Pos 1 0, Pos 1 2) (runMotion nextWordStart "ab\ncde" (Pos 0 1)))
  , test "w treats punctuation as its own word" (assertEqual (Pos 0 0, Pos 0 2) (runMotion nextWordStart "foo.bar" (Pos 0 0)))
  , test "e selects to end of word" (assertEqual (Pos 0 0, Pos 0 4) (runMotion nextWordEnd "hello world" (Pos 0 0)))
  , test "e from end of word goes to next word end" (assertEqual (Pos 0 5, Pos 0 10) (runMotion nextWordEnd "hello world" (Pos 0 4)))
  , test "b selects back to word start" (assertEqual (Pos 0 8, Pos 0 6) (runMotion prevWordStart "hello world" (Pos 0 8)))
  , test "b from word start goes to previous word" (assertEqual (Pos 0 5, Pos 0 0) (runMotion prevWordStart "hello world" (Pos 0 6)))
  , test "b at file start stays" (assertEqual (Pos 0 0, Pos 0 0) (runMotion prevWordStart "hello" (Pos 0 0)))
  , test "w at end of file stays" (assertEqual (Pos 0 5, Pos 0 5) (runMotion nextWordStart "hello" (Pos 0 5)))
  , test "x selects the line with its newline" (assertEqual (Pos 1 0, Pos 1 2) (runMotion selectLine "ab\ncd\nef" (Pos 1 1)))
  , test "x twice extends a line" $
      let b = buf "ab\ncd\nef"
          r = selectLine b (selectLine b (point (Pos 0 1)))
       in assertEqual (Pos 0 0, Pos 1 2) (rangeAnchor r, rangeHead r)
  , test "extend keeps the anchor" $
      let r = applyMotion Extend charRight (buf "abc") (Range (Pos 0 0) (Pos 0 1) Nothing)
       in assertEqual (Pos 0 0, Pos 0 2) (rangeAnchor r, rangeHead r)
  ]

runEdit :: Edit -> Text -> Range -> (Text, Pos)
runEdit e t r = let (b', r') = e (buf t) r in (B.toText b', rangeHead r')

editTests :: [Test]
editTests =
  [ test "backspace joins lines" (assertEqual ("abcd", Pos 0 2) (runEdit deleteBackward "ab\ncd" (point (Pos 1 0))))
  , test "backspace at start does nothing" (assertEqual ("ab", Pos 0 0) (runEdit deleteBackward "ab" (point (Pos 0 0))))
  , test "newline keeps indentation" (assertEqual ("  ab\n  c", Pos 1 2) (runEdit insertNewline "  abc" (point (Pos 0 4))))
  , test "delete selection is inclusive" (assertEqual ("ad", Pos 0 1) (runEdit deleteSelection "abcd" (Range (Pos 0 1) (Pos 0 2) Nothing)))
  , test "delete a line with its newline" (assertEqual ("ab\nef", Pos 1 0) (runEdit deleteSelection "ab\ncd\nef" (Range (Pos 1 0) (Pos 1 2) Nothing)))
  , test "delete the last line removes it" (assertEqual ("ab", Pos 0 0) (runEdit deleteSelection "ab\ncd" (Range (Pos 1 0) (Pos 1 2) Nothing)))
  , test "open line below" (assertEqual ("  ab\n  \ncd", Pos 1 2) (runEdit openLineBelow "  ab\ncd" (point (Pos 0 1))))
  ]

historyTests :: [Test]
historyTests =
  [ test "nothing to undo" (assertEqual Nothing (fst <$> H.undo (snap "a") H.emptyHistory))
  , test "only the first state of a change is kept" $
      let h = H.commit (H.beginChange (snap "b") (H.beginChange (snap "a") H.emptyHistory))
       in assertEqual (Just (snap "a")) (fst <$> H.undo (snap "c") h)
  , test "undo then redo returns to the current state" $
      let h = H.commit (H.beginChange (snap "a") H.emptyHistory)
       in assertEqual (Just (snap "b")) (H.undo (snap "b") h >>= \(s', h') -> fst <$> H.redo s' h')
  , test "a new change clears redo" $
      let h1 = H.commit (H.beginChange (snap "a") H.emptyHistory)
          h2 = maybe H.emptyHistory snd (H.undo (snap "b") h1)
          h3 = H.commit (H.beginChange (snap "a") h2)
       in assertEqual Nothing (fst <$> H.redo (snap "x") h3)
  ]
  where
    snap t = H.Snapshot (buf t) (single (point (Pos 0 0)))

keymapTests :: [Test]
keymapTests =
  [ test "single key" (assertEqual (Found "one") (resolve km (keys "a")))
  , test "prefix needs more" (assertEqual NeedMore (resolve km (keys "g")))
  , test "chord" (assertEqual (Found "top") (resolve km (keys "g g")))
  , test "unknown" (assertEqual NoMatch (resolve km (keys "z")))
  , test "unknown after prefix" (assertEqual NoMatch (resolve km (keys "g z")))
  , test "union overrides and merges prefixes" $
      let over = either (error . show) id (fromBindings [("g e", "end"), ("a", "other")])
          u = unionKeymap over km
       in assertEqual [Found "other", Found "top", Found "end"] (map (resolve u . keys) ["a", "g g", "g e"])
  , test "invalid key sequence is an error" (assertEqual (Left "invalid key sequence: nope") (() <$ fromBindings [("nope", "x" :: Text)]))
  , test "default config is valid" (assertEqual (Right ()) (() <$ defaultConfig))
  ]
  where
    km = either (error . show) id (fromBindings [("a", "one" :: Text), ("g g", "top")])
    keys = fromMaybe [] . parseKeys

data CursorEdit = Insert' | Backspace' | Delete'

multiSelectionTests :: [Test]
multiSelectionTests =
  [ test "fromRanges sorts and keeps the primary" $
      assertEqual (Just ([r 0 4 0 5, r 1 0 1 0], 1)) (shape <$> fromRanges [r 1 0 1 0, r 0 4 0 5] 0)
  , test "fromRanges merges overlaps" $
      assertEqual (Just ([r 0 0 0 6], 0)) (shape <$> fromRanges [r 0 0 0 3, r 0 2 0 6] 1)
  , test "fromRanges merges equal cursors" $
      assertEqual (Just ([r 0 2 0 2], 0)) (shape <$> fromRanges [r 0 2 0 2, r 0 2 0 2] 1)
  , test "remove and rotate the primary" $
      let sel = sel_ [r 0 0 0 0, r 1 0 1 0, r 2 0 2 0] 1
       in assertEqual ([r 0 0 0 0, r 2 0 2 0], 1, 2, 0) (ranges (removePrimary sel), primaryIndex (removePrimary sel), primaryIndex (rotatePrimary 1 sel), primaryIndex (rotatePrimary (-1) sel))
  , test "inserting at several cursors" $
      let sel = sel_ [r 0 1 0 1, r 1 0 1 0, r 0 3 0 3] 0
          (b, sel') = applyEdits (const (insertAtHead "X")) (buf "abcd\nef") sel
       in assertEqual ("aXbcXd\nXef", [r 0 2 0 2, r 0 5 0 5, r 1 1 1 1]) (B.toText b, ranges sel')
  , test "newlines at several cursors" $
      let sel = sel_ [r 0 1 0 1, r 0 2 0 2] 0
          (b, sel') = applyEdits (const insertNewline) (buf "abc") sel
       in assertEqual ("a\nb\nc", [r 1 0 1 0, r 2 0 2 0]) (B.toText b, ranges sel')
  , test "deleting several selections" $
      let sel = sel_ [r 0 0 0 1, r 0 4 0 5, r 1 1 1 2] 0
          (b, _) = applyEdits (const deleteSelection) (buf "one two\nthree") sel
       in assertEqual "e o\ntee" (B.toText b)
  , test "random multi-cursor inserts match a model" (multiModel 300 11 Insert')
  , test "random multi-cursor backspaces match a model" (multiModel 300 13 Backspace')
  , test "random multi-cursor forward deletes match a model" (multiModel 300 19 Delete')
  , test "random multi-range deletes match a model" (mapM_ deleteModel (take 400 (chunks (randoms 17))))
  , test "deleting a line and the last line" $
      let (b, _) = applyEdits (const deleteSelection) (buf "a\nb\nc") (sel_ [r 1 0 1 1, r 2 0 2 1] 0)
       in assertEqual "a" (B.toText b)
  , test "deleteForward at adjacent cursors" $
      let (b, sel') = applyEdits (const deleteForward) (buf "abcd") (sel_ [r 0 1 0 1, r 0 2 0 2] 0)
       in assertEqual ("ad", [r 0 1 0 1]) (B.toText b, ranges sel')
  , test "copy_selection_on_next_line skips short lines" $
      let sel = copySelectionBelow (buf "abcd\nx\nabcd") (single (r 0 2 0 3))
       in assertEqual ([r 0 2 0 3, r 2 2 2 3], 1) (shape sel)
  , test "split on newlines drops the line breaks" $
      assertEqual ([r 0 1 0 2, r 1 0 1 2, r 3 0 3 1], 0) (shape (splitOnNewlines (buf "abc\ndef\n\nghi") (single (r 0 1 3 1))))
  , test "select matches inside the selection" $
      let needle = needle_ "ab"
       in assertEqual (Just ([r 0 0 0 1, r 0 3 0 4, r 1 2 1 3], 0)) (shape <$> selectMatches needle (buf "ab ab\nxxab ab") (single (r 0 0 1 4)))
  , test "no matches" $
      let needle = needle_ "zz"
       in assertEqual Nothing (shape <$> selectMatches needle (buf "ab") (single (r 0 0 0 1)))
  ]
  where
    r l1 c1 l2 c2 = Range (Pos l1 c1) (Pos l2 c2) Nothing
    sel_ rs i = fromMaybe (error "no ranges") (fromRanges rs i)
    needle_ t = fromMaybe (error "bad needle") (compileNeedle t)
    shape sel = (ranges sel, primaryIndex sel)
    -- Point cursors at random offsets (at least 2 apart), one random edit
    -- applied to all of them, compared with the same edit on a string.
    multiModel :: Int -> Int -> CursorEdit -> Either String ()
    multiModel n seed kind = mapM_ (one kind) (take n (chunks (randoms seed)))
    chunks xs = let (a, b) = splitAt 8 xs in a : chunks b
    one kind rs = case rs of
      (r1 : r2 : r3 : more) ->
        let txt = T.intercalate "\n" (take (2 + r1 `mod` 4) ["hello", "", "a b c", "xyz", "q"])
            len = T.length txt
            offs = spread (map (`mod` (len + 1)) (take (1 + r2 `mod` 4) more))
            ins = ["X", "\n", "ab\ncd", ""] !! (r3 `mod` 3)
            sel = sel_ [let p = posOf txt o in Range p p Nothing | o <- offs] 0
            edit1 = case kind of
              Insert' -> insertAtHead ins
              Backspace' -> deleteBackward
              Delete' -> deleteForward
            (b, sel') = applyEdits (const edit1) (buf txt) sel
            (expectText, expectOffs) = case kind of
              Insert' -> (insertAll txt offs ins, [o + i * T.length ins + T.length ins | (i, o) <- zip [0 ..] offs])
              Backspace' -> deleteAll txt [o - 1 | o <- offs, o > 0] offs
              Delete' -> deleteAll txt [o | o <- offs, o < len] offs
            got = (B.toText b, map (offOf (B.toText b) . rangeHead) (ranges sel'))
         in if got == (expectText, nub expectOffs) then Right () else Left (show (txt, offs, ins, got, (expectText, expectOffs)))
      _ -> Right ()
    -- Random sorted, non-overlapping (often adjacent) ranges deleted at
    -- once, compared with deleting the characters they cover from a string.
    deleteModel rs = case rs of
      (r1 : r2 : more) ->
        let txt = T.intercalate "\n" (take (2 + r1 `mod` 3) ["hello", "", "ab c", "xyz"])
            len = T.length txt
            cuts = nub (sort (map (`mod` (len + 1)) (take (2 + 2 * (r2 `mod` 3)) more)))
            spans = pairs cuts
            sel = sel_ [Range (posOf txt a) (posOf txt e) Nothing | (a, e) <- spans] 0
            (b, _) = applyEdits (const deleteSelection) (buf txt) sel
            covered = [i | (a, e) <- spans, i <- [a .. min (len - 1) e]]
            -- A range from a line start to the end also takes the line
            -- break before it.
            trailing = [a - 1 | (a, e) <- take 1 (reverse spans), e >= len, a > 0, T.index txt (a - 1) == '\n']
            gone = covered <> trailing
            expect = T.pack [c | (i, c) <- zip [0 ..] (T.unpack txt), i `notElem` gone]
         in if null spans || B.toText b == expect then Right () else Left (show (txt, spans, B.toText b, expect))
      _ -> Right ()
    pairs (a : e : more) = (a, e) : pairs more
    pairs _ = []
    -- Sorted, at least 2 apart.
    spread = foldr keep [] . nub . sort
    keep o acc = case acc of
      (x : _) | x - o < 1 -> acc
      _ -> o : acc
    posOf t o = let before = T.take o t in Pos (T.count "\n" before) (T.length (T.takeWhileEnd (/= '\n') before))
    offOf t (Pos l c) = sum [T.length x + 1 | x <- take l (T.splitOn "\n" t)] + c
    insertAll t offs ins = foldr (\o acc -> T.take o acc <> ins <> T.drop o acc) t offs
    -- Delete the characters at the given indices; each cursor moves left
    -- by the deletions before it.
    deleteAll t dels offs =
      let t' = T.pack [c | (i, c) <- zip [0 ..] (T.unpack t), i `notElem` dels]
       in (t', [o - length [d | d <- dels, d < o] | o <- offs])

ignoreTests :: [Test]
ignoreTests =
  [ test "a name matches at any depth" (assertEqual [True, True, False] (map (ign "*.o") ["a.o", "dir/b.o", "a.oo"]))
  , test "a leading slash anchors" (assertEqual [True, False] (map (ign "/build") ["build", "src/build"]))
  , test "a slash in the middle anchors" (assertEqual [True, False, False] (map (ign "doc/*.txt") ["doc/a.txt", "doc/sub/a.txt", "x/doc/a.txt"]))
  , test "a trailing slash matches directories only" $
      assertEqual (Just True, Nothing) (matchRules (parseIgnore "build/") "build" True, matchRules (parseIgnore "build/") "build" False)
  , test "**/ matches any directories" (assertEqual [True, True] (map (ign "**/foo") ["foo", "a/b/foo"]))
  , test "/**/ matches zero or more directories" (assertEqual [True, True, False] (map (ign "a/**/b") ["a/b", "a/x/y/b", "c/a/b"]))
  , test "/** matches everything inside" (assertEqual [True, True, False] (map (ign "abc/**") ["abc/x", "abc/x/y", "abc"]))
  , test "? and character classes" $
      assertEqual [True, False, True, False, True, False] (map (uncurry ign) [("a?c", "abc"), ("a?c", "a/c"), ("[a-c]x", "bx"), ("[a-c]x", "dx"), ("[!a]x", "bx"), ("[!a]x", "ax")])
  , test "comments, blank lines, escapes and trailing spaces" $
      assertEqual [Nothing, Just True, Just True] [matchRules (parseIgnore "# c\n\n") "c" False, matchRules (parseIgnore "\\#x") "#x" False, matchRules (parseIgnore "foo   ") "foo" False]
  , test "the last matching rule wins; ! re-includes" $
      assertEqual [True, False] (map (\f -> isIgnored [(Below "", parseIgnore "*.log\n!keep.log")] f False) ["a.log", "keep.log"])
  , test "a deeper file overrides" $
      let ig = [(Below "", parseIgnore "*.txt"), (Below "sub", parseIgnore "!keep.txt")]
       in assertEqual [False, True] (map (\f -> isIgnored ig f False) ["sub/keep.txt", "other/keep.txt"])
  , test "rules from an ancestor see the root's path in it" $
      assertEqual [True, False] (map (\f -> isIgnored [(Above "src", parseIgnore "/src/gen")] f True) ["gen", "other"])
  ]
  where
    ign pat path = isIgnored [(Below "", parseIgnore pat)] path False

diffTests' :: [Test]
diffTests' =
  [ test "identical texts have no hunks" (assertEqual [] (diffLines ["a", "b"] ["a", "b"]))
  , test "an added line" (assertEqual [Hunk 1 0 1 1] (diffLines ["a", "c"] ["a", "b", "c"]))
  , test "a removed line" (assertEqual [Hunk 1 1 1 0] (diffLines ["a", "b", "c"] ["a", "c"]))
  , test "a changed line" (assertEqual [Hunk 1 1 1 1] (diffLines ["a", "b", "c"] ["a", "x", "c"]))
  , test "separate hunks" (assertEqual [Hunk 0 1 0 1, Hunk 3 0 3 1] (diffLines ["a", "b", "c"] ["x", "b", "c", "d"]))
  , test "kinds" (assertEqual [Added, Removed, Changed] (map hunkKind [Hunk 1 0 1 2, Hunk 1 2 1 0, Hunk 0 1 0 1]))
  , test "mapLine shifts lines after a hunk" $
      -- A line inserted before old line 1; old line 2 replaced by new line 3.
      assertEqual [0, 2, 3, 4] (map (mapLine [Hunk 1 0 1 1, Hunk 2 1 3 1]) [0, 1, 2, 3])
  , test "random edits: hunks rebuild the new text and are minimal" (mapM_ diffModel (take 400 (chunks' (randoms 29))))
  , test "a huge rewrite falls back to one hunk" $
      let old = [T.pack (show i) | i <- [1 .. 3000 :: Int]]
          new = [T.pack ("x" <> show i) | i <- [1 .. 3000 :: Int]]
       in assertEqual [Hunk 0 3000 0 3000] (diffLines old new)
  ]
  where
    chunks' xs = let (a, b) = splitAt 30 xs in a : chunks' b
    diffModel rs = case rs of
      (r1 : r2 : more) ->
        let alphabet = ["a", "b", "c", "d"]
            old = [alphabet !! (r `mod` 4) | r <- take (r1 `mod` 12) more]
            new = [alphabet !! (r `div` 5 `mod` 4) | r <- take (r2 `mod` 12) (drop 12 more)]
            hs = diffLines old new
            changed = sum [hOldCount h + hNewCount h | h <- hs]
            optimal = length old + length new - 2 * lcs old new
         in if applyHunks hs old new /= new
              then Left ("does not rebuild: " <> show (old, new, hs))
              else if changed /= optimal then Left ("not minimal: " <> show (old, new, hs)) else Right ()
      _ -> Right ()
    -- Longest common subsequence, by the textbook dynamic program.
    lcs xs ys = last (foldl step (replicate (length ys + 1) 0) xs)
      where
        step prev x = scanl (\left (y, diag, up) -> if x == y then diag + 1 else max left up) 0 (zip3 ys prev (drop 1 prev))

gitStateTests :: [Test]
gitStateTests =
  [ test "stage all lines of a hunk" $
      assertEqual ["a", "X", "c"] (apply ["a", "b", "c"] ["a", "X", "c"] (const True))
  , test "stage nothing" (assertEqual ["a", "b", "c"] (apply ["a", "b", "c"] ["a", "X", "c"] (const False)))
  , test "changed lines are paired, so one line can be staged" $
      assertEqual ["X", "b", "c"] (apply ["a", "b", "c"] ["X", "Y", "c"] (== 0))
  , test "extra added lines are staged when selected" $
      assertEqual ["a", "new2", "b"] (apply ["a", "b"] ["a", "new1", "new2", "b"] (== 2))
  , test "a removal is staged through the line above it" $
      assertEqual (["a", "c"], ["a", "b", "c"]) (apply ["a", "b", "c"] ["a", "c"] (== 0), apply ["a", "b", "c"] ["a", "c"] (== 1))
  , test "reverting = applying the unselected changes" $
      let old = ["a", "b", "c", "d"]
          new = ["a", "B", "c", "D"]
       in assertEqual ["a", "b", "c", "D"] (apply old new (/= 1))
  , test "signs: added, changed, removed; unstaged wins" $
      let t = GitTracking (GitBase "" "" "" [] True [] True) [Hunk 0 0 0 1, Hunk 2 1 3 0] [Hunk 0 1 0 1, Hunk 4 1 5 1] 0 False False
       in assertEqual
            [(0, Sign SignAdded False), (2, Sign SignRemoved False), (5, Sign SignChanged True)]
            (IntMap.toList (gitSigns t 0 10))
  ]
  where
    apply old new = applySelected old new (diffLines old new)

-- | Git end to end in a temporary repository.
gitTests :: IO [Test]
gitTests = do
  config <- either (fail . T.unpack) pure defaultConfig
  dir <- getTemporaryDirectory
  let repo = dir <> "/him-test-git"
      file = repo <> "/f.txt"
      g args = runProcess "git" args (Just repo) ""
      run ed k = execStateT (handleEvent config (EvKey k)) ed
      keys ks ed = foldlM run ed (fromMaybe (error ks) (parseKeys (T.pack ks)))
      stagedDiff = either (const "") (TE.decodeUtf8 . prStdout) <$> g ["diff", "--cached", "--no-color", "-U0"]
  exists <- doesPathExist repo
  when exists (removeDirectoryRecursive repo)
  createDirectoryIfMissing True repo
  _ <- g ["init", "-q"]
  _ <- g ["config", "user.email", "t@t"]
  _ <- g ["config", "user.name", "t"]
  writeFile file "one\ntwo\nthree\n"
  _ <- g ["add", "f.txt"]
  _ <- g ["commit", "-q", "-m", "init"]
  doc <- either (fail . T.unpack) pure =<< loadDocument file
  opened <- settle config =<< execStateT gitHousekeeping (newEditor (24, 80) doc)
  -- Change line 2, add a line after line 3.
  edited <- settle config =<< keys "j e c T W O esc g e o f o u r esc" opened
  let hunksOf ed = (gtUnstaged <$> tracking (docGit (edDoc ed)), gtStaged <$> tracking (docGit (edDoc ed)))
  -- Stage only line 2 (the change), not the added line.
  staged <- settle config =<< keys "g g j x space g s" edited
  cached <- stagedDiff
  unstaged <- settle config =<< keys "g g j x space g u" staged
  cachedAfter <- stagedDiff
  reset <- settle config =<< keys "g g j x space g r" edited
  outside <- settle config =<< execStateT gitHousekeeping (newEditor (24, 80) (newDocument (Just "/") (buf "")))
  removeDirectoryRecursive repo
  pure
    [ test "a tracked file gets its git base" (assertEqual (Just "f.txt") (gbPath . gtBase <$> tracking (docGit (edDoc opened))))
    , test "edits show as unstaged hunks" (assertEqual (Just [Hunk 1 1 1 1, Hunk 3 0 3 1], Just []) (hunksOf edited))
    , test "staging the selected line writes only it to the index" $
        assertEqual True ("-two\n+TWO\n" `T.isInfixOf` cached && not ("+four" `T.isInfixOf` cached))
    , test "after staging, the change is staged and the rest unstaged" (assertEqual (Just [Hunk 3 0 3 1], Just [Hunk 1 1 1 1]) (hunksOf staged))
    , test "unstaging the line empties the index diff" (assertEqual ("", Just [Hunk 1 1 1 1, Hunk 3 0 3 1]) (cachedAfter, fst (hunksOf unstaged)))
    , test "reset puts the index version of the selected line back" (assertEqual ["one", "two", "three", "four"] (B.toLines (docBuffer (edDoc reset))))
    , test "outside a repository there is no git state" (assertEqual GitOutside (docGit (edDoc outside)))
    ]

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
        (Just ("/a.c", [Diagnostic (1, 2) (1, 5) SevWarning "unused" "clang"]))
        (parseDiagnostics (JObject [("uri", JString "file:///a.c"), ("diagnostics", JArray [JObject [("range", rng 1 2 1 5), ("severity", JInt 2), ("message", JString "unused"), ("source", JString "clang")]])]))
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

regexTests :: [Test]
regexTests =
  [ test "matches from the query files" $
      assertEqual
        (replicate 12 True <> replicate 6 False)
        ( map (uncurry m)
            [ ("^[A-Z][A-Z\\d_]*$", "MAX_SIZE2")
            , ("^_", "_unused")
            , ("^(self|super)$", "super")
            , ("^[A-Z]", "Maybe")
            , ("^(true|false)$", "true")
            , ("\\.(js|ts)$", "a.ts")
            , ("a.*?b", "xaxxbyb")
            , ("^a{2,3}$", "aaa")
            , ("\\bend\\b", "the end")
            , ("^[^0-9]+$", "abc")
            , ("x(?:ab)+y", "xababy")
            , ("^$", "")
            , ("^[A-Z][A-Z\\d_]*$", "Max")
            , ("^(self|super)$", "superb")
            , ("^a{2,3}$", "aaaa")
            , ("\\bend\\b", "endless")
            , ("^[^0-9]+$", "ab1")
            , ("x(?:ab)+y", "xy")
            ]
        )
  , test "the first match and its extent" $
      assertEqual [Just (1, 4), Just (2, 3), Nothing] [findRegex' "b+c" "abbcd", findRegex' "a*?b" "xxb", findRegex' "z" "abc"]
  , test "Lua patterns" $
      assertEqual [True, True, False, True] [lua "^%u%l+$" "Hello", lua "^%d+%.%d+$" "1.25", lua "^%a+$" "ab1", lua "^a.-b$" "axxb"]
  , test "bad patterns are errors" (assertEqual [True, True] (map (either (const True) (const False) . compileRegex) ["(ab", "[ab"]))
  ]
  where
    m pat str = either (const False) (`matchesRegex` str) (compileRegex pat)
    findRegex' pat str = either (const Nothing) (`findRegex` str) (compileRegex pat)
    lua pat str = either (const False) (`matchesRegex` str) (compileLua pat)

syntaxTests :: [Test]
syntaxTests =
  [ test "flatten: the earlier span wins an overlap" $
      assertEqual [LineSpan 0 4 "a", LineSpan 4 6 "b"] (flatten [LineSpan 0 4 "a", LineSpan 2 6 "b"])
  , test "flatten: a later span is cut around an earlier one inside it" $
      assertEqual [LineSpan 0 2 "outer", LineSpan 2 4 "inner", LineSpan 4 8 "outer"] (flatten [LineSpan 2 4 "inner", LineSpan 0 8 "outer"])
  , test "flatten: adjacent spans of one scope merge" (assertEqual [LineSpan 0 6 "k"] (flatten [LineSpan 0 3 "k", LineSpan 3 6 "k"]))
  , test "languages by extension, file name and shebang" $
      assertEqual [Just "rust", Just "make", Just "python", Just "bash", Nothing]
        [ langName <$> detectLanguage languages "src/main.rs" ""
        , langName <$> detectLanguage languages "dir/Makefile" ""
        , langName <$> detectLanguage languages "script" "#!/usr/bin/env python3"
        , langName <$> detectLanguage languages "run" "#!/bin/bash -e"
        , langName <$> detectLanguage languages "notes.xyz" "hello"
        ]
  , test "scopes resolve by their longest known prefix" $
      assertEqual
        [scopeStyle defaultTheme "keyword.control", scopeStyle defaultTheme "comment", Nothing]
        [scopeStyle defaultTheme "keyword.control.import", scopeStyle defaultTheme "comment.line.double-slash", scopeStyle defaultTheme "nothing.like.this"]
  ]

-- | A provider for the tests: highlights the word "let" in Haskell files,
-- through the same interface tree-sitter uses.
fakeProvider :: SyntaxProvider
fakeProvider = SyntaxProvider "fake" $ \language ->
  if langName language /= "haskell"
    then pure Nothing
    else do
      current <- newIORef B.empty
      pure . Just $
        SyntaxSession
          { ssUpdate = \_ b _ -> writeIORef current b
          , ssHighlight = \from to -> do
              b <- readIORef current
              pure $
                IntMap.fromList
                  [ (l, [LineSpan i (i + 3) "keyword" | i <- occurrences "let" (B.lineAt l b)])
                  | l <- [from .. min to (B.lineCount b - 1)]
                  ]
          , ssClose = pure ()
          }
  where
    occurrences needle hay = [T.length before | (before, _) <- T.breakOnAll needle hay]

-- | The tree-sitter provider on real grammars, when a runtime directory
-- with the Rust grammar exists (otherwise the tests pass, saying so).
treeSitterTests :: IO [Test]
treeSitterTests =
  findRuntime "rust" >>= \case
    Nothing -> pure [test "tree-sitter runtime not found: skipped" (Right ())]
    Just _ -> do
      let rust = fromMaybe (error "rust") (find ((== "rust") . langName) languages)
          source = B.fromText "// note\nfn main() {\n    let x = \"hi\\n\";\n}"
      session <- spStart treeSitter rust
      spans <- case session of
        Nothing -> pure IntMap.empty
        Just s -> ssUpdate s 1 source [] >> ssHighlight s 0 3
      home <- getHomeDirectory
      inherited <- readQuery (home </> ".config/helix/runtime/queries") "typescript"
      let scopesOn l = [lsScope sp | sp <- IntMap.findWithDefault [] l spans]
      pure
        [ test "a Rust grammar loads" (assertEqual True (isJust session))
        , test "a comment" (assertEqual ["comment.line"] (map (T.take 12) (scopesOn 0)))
        , test "keywords, functions and strings" $
            assertEqual (True, True, True)
              ( any ("keyword" `T.isPrefixOf`) (scopesOn 1)
              , any ("function" `T.isPrefixOf`) (scopesOn 1)
              , any ("string" `T.isPrefixOf`) (scopesOn 2)
              )
        , test "an escape inside a string wins over the string" $
            assertEqual True (any ("constant.character.escape" `T.isPrefixOf`) (scopesOn 2))
        , test "spans are sorted and do not overlap" $
            assertEqual True (and [lsEnd a <= lsStart b | l <- [0 .. 3], let ss = IntMap.findWithDefault [] l spans, (a, b) <- zip ss (drop 1 ss)])
        , test "inherited queries are read in place of the inherits line" $
            assertEqual (Just False) (T.isInfixOf "; inherits" <$> inherited)
        ]

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
      pure
        [ test "the document attaches to clangd" (assertEqual True (case docLsp (edDoc attached) of LspAttached _ -> True; _ -> False))
        , test "an error is reported on its line" (assertEqual [(2, SevError)] (take 1 (errorsOn attached)))
        , test "hover shows a popup" (assertEqual True (isJust (edPopup hovered)))
        , test "g d goes to the definition of add" (assertEqual (Pos 0 4) (rangeHead (primary (docSelection (edDoc defined)))))
        , test "fixing the error clears it" (assertEqual [] (errorsOn fixed))
        , test "typing a word opens the completion menu" $
            assertEqual (True, Completing) (any ((== "add") . ciInsert) (maybe [] cmShown (edCompletion menu)), keymapMode menu)
        , test "ret inserts the selected completion" $
            assertEqual (True, Nothing) ("add" `T.isPrefixOf` T.strip (B.lineAt 2 (docBuffer (edDoc accepted))), edCompletion accepted)
        ]

-- | Highlighting through an injected provider.
syntaxIOTests :: IO [Test]
syntaxIOTests = do
  base <- either (fail . T.unpack) pure defaultConfig
  let decline = SyntaxProvider "declines" (const (pure Nothing))
      config = base {cfgSyntaxProviders = [decline, fakeProvider]}
      start path t = execStateT (handleEvent config (EvResize 10 40)) (newEditor (10, 40) (newDocument (Just path) (buf t)))
      run ed k = execStateT (handleEvent config (EvKey k)) ed
      keys ks ed = foldlM run ed (fromMaybe (error ks) (parseKeys (T.pack ks)))
      syntaxOf ed = docSyntax (edDoc ed)
  rt <- testRuntime config
  opened <- settleWith config rt =<< start "a.hs" "let a = 1\nin a"
  edited <- settleWith config rt =<< keys "i l e t space esc" opened
  plainFile <- settle config =<< start "a.txt" "let a = 1"
  let frame = render defaultTheme Nothing opened
      cellStyleAt row col = fmap cellStyle (Seq.lookup col =<< Seq.lookup row (frameCells frame))
  pure
    [ test "the first provider that accepts the language is used" (assertEqual (SyntaxActive "fake") (siStatus (syntaxOf opened)))
    , test "spans arrive for the visible lines" (assertEqual (Just [LineSpan 0 3 "keyword"], Just []) (IntMap.lookup 0 (siSpans (syntaxOf opened)), IntMap.lookup 1 (siSpans (syntaxOf opened))))
    , test "an edit is highlighted again" (assertEqual (Just [LineSpan 0 3 "keyword", LineSpan 4 7 "keyword"]) (IntMap.lookup 0 (siSpans (syntaxOf edited))))
    , test "a file without a language is not highlighted" (assertEqual SyntaxNone (siStatus (syntaxOf plainFile)))
    , test "spans are drawn in their scope's style" $
        -- Column 6: past the gutter and the cursor cell, inside "let".
        assertEqual (packStyle <$> scopeStyle defaultTheme "keyword") (cellStyleAt 0 6)
    ]

jsonTests :: [Test]
jsonTests =
  [ test "objects, arrays and literals" $
      assertEqual
        (Right (JObject [("a", JArray [JInt 1, JBool True, JNull]), ("b", JObject [])]))
        (parseJson " { \"a\" : [1, true, null], \"b\": {} } ")
  , test "numbers" $
      assertEqual (Right [JInt 0, JInt (-12), JDouble 1.5, JDouble 1000, JInt 123456789012345678901234567890])
        (traverse parseJson ["0", "-12", "1.5", "1e3", "123456789012345678901234567890"])
  , test "escapes" (assertEqual (Right (JString "\"\\/\b\f\n\r\t\233")) (parseJson "\"\\\"\\\\\\/\\b\\f\\n\\r\\t\\u00e9\""))
  , test "surrogate pairs join; a lone surrogate is replaced" $
      assertEqual (Right [JString "\128512", JString "\65533x"]) (traverse parseJson ["\"\\ud83d\\ude00\"", "\"\\ud83dx\""])
  , test "raw UTF-8 in strings" (assertEqual (Right (JString "æ漢")) (parseJson (TE.encodeUtf8 "\"æ漢\"")))
  , test "malformed input is an error" $
      assertEqual [True, True, True, True, True, True]
        (map (isLeft . parseJson) ["{\"a\" 1}", "[1,]", "\"open", "tru", "1 2", "\"a\nb\""])
  , test "accessors" $
      let v = JObject [("result", JObject [("n", JInt 3), ("s", JString "x")])]
       in assertEqual (Just 3, Just "x", Nothing) (J.path ["result", "n"] v >>= asInt, J.path ["result", "s"] v >>= asText, J.path ["result", "z"] v)
  , test "random values round-trip through render and parse" $
      let vals = take 300 (map (fst . genValue 3) (chunksOf 40 (randoms 23)))
       in assertEqual [] [v | v <- vals, parseJson (renderJson v) /= Right v]
  ]
  where
    isLeft = either (const True) (const False)
    chunksOf n xs = let (a, b) = splitAt n xs in a : chunksOf n b
    -- A random value from a supply of random numbers.
    genValue :: Int -> [Int] -> (Value, [Int])
    genValue depth (r : rs) = case r `mod` (if depth <= 0 then 5 else 7) of
      0 -> (JNull, rs)
      1 -> (JBool (even (r `div` 7)), rs)
      2 -> (JInt (fromIntegral (r `div` 7) - 2 ^ (40 :: Int)), rs)
      3 -> (JDouble (fromIntegral (r `mod` 100000) / 64), rs)
      4 -> (JString (T.pack (take (r `mod` 6) (map (pickChar . (`div` 3)) rs))), drop 6 rs)
      5 -> let (vs, rest) = many (r `mod` 4) (depth - 1) rs in (JArray vs, rest)
      _ -> let (vs, rest) = many (r `mod` 4) (depth - 1) rs in (JObject (zip ["k", "é", "\"q", "\n"] vs), rest)
    genValue _ [] = (JNull, [])
    many 0 _ rs = ([], rs)
    many n d rs = let (v, rs') = genValue d rs; (vs, rs'') = many (n - 1 :: Int) d rs' in (v : vs, rs'')
    pickChar n = let pool = "aZ \"\\\n\t\0é漢😀\DEL" in pool !! (n `mod` length pool)

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

pickerTests :: [Test]
pickerTests =
  [ test "fuzzy: characters in order" (assertEqual (Just 0, Just 2, Nothing) (fuzzyScore "ab" "xaby", fuzzyScore "ab" "axxb", fuzzyScore "ba" "ab"))
  , test "fuzzy: the best start wins" (assertEqual (Just 0) (fuzzyScore "ab" "a_xab"))
  , test "fuzzy: case is ignored" (assertEqual (Just 0) (fuzzyScore "SPEC" "test/Spec.hs"))
  , test "matches: best first, shorter on ties" $
      assertEqual ["src/ab.hs", "a/b.hs", "src/a/long/b.hs"] (map piLabel (matches "ab" (items ["src/a/long/b.hs", "a/b.hs", "src/ab.hs", "xyz"])))
  , test "an empty query keeps the order" (assertEqual ["b", "a"] (map piLabel (matches "" (items ["b", "a"]))))
  , test "moving wraps around" $
      let p = newPicker "t" (items ["a", "b", "c"])
       in assertEqual [1, 0, 2] (map (pkSelected . ($ p)) [moveSelection 1, moveSelection 3, moveSelection (-1)])
  , test "an exact first word or file name wins a tie" $
      assertEqual ["goto_line <line>", "x/b.hs"] (map (piLabel . head' . matches' (items ["goto_line_end", "goto_line <line>", "goto_line_start"])) ["goto_line"] <> map (piLabel . head' . matches' (items ["x/ab.hs", "x/b.hs.bak", "x/b.hs"])) ["b.hs"])
  , test "a label match beats a detail match" $
      assertEqual ["xy", "other"] (map piLabel (matches "xy" [pickerItem "other" (PickFile "") "xy here", pickerItem "xy" (PickFile "") ""]))
  , test "a new query selects the best match" $
      assertEqual (Just "b") (piLabel <$> selectedItem (setQuery "b" (moveSelection 2 (newPicker "t" (items ["a", "b", "c"])))))
  ]
  where
    items = map (\l -> pickerItem l (PickFile (T.unpack l)) "")
    matches' xs q = matches q xs
    head' = \case
      x : _ -> x
      [] -> pickerItem "" (PickFile "") ""

actionTests :: [Test]
actionTests =
  [ test "a bare name" (assertEqual (Right (Invocation "undo" [])) (parseInvocation "undo"))
  , test "arguments" (assertEqual (Right (Invocation "goto_line" ["12"])) (parseInvocation "  goto_line   12 "))
  , test "quoted argument with escapes" $
      assertEqual (Right (Invocation "insert_text" ["a \"b\"\n\\"])) (parseInvocation "insert_text \"a \\\"b\\\"\\n\\\\\"")
  , test "empty quoted argument" (assertEqual (Right (Invocation "insert_text" [""])) (parseInvocation "insert_text \"\""))
  , test "unterminated string" (assertEqual (Left "unterminated string") (parseInvocation "insert_text \"abc"))
  , test "unknown escape" (assertEqual (Left "unknown escape \\q") (parseInvocation "insert_text \"\\q\""))
  , test "empty binding" (assertEqual (Left "empty action") (parseInvocation "  "))
  , test "invalid name" (assertEqual (Left "invalid action name: Undo") (parseInvocation "Undo"))
  , test "render round-trips" $
      let invs = [Invocation "insert_text" ["a b", "", "q\"\\\n\t"], Invocation "goto_line" ["3"], Invocation "undo" []]
       in assertEqual (map Right invs) (map (parseInvocation . renderInvocation) invs)
  , test "unknown action" (assertEqual (Left "unknown action: fly") (bindErr "fly"))
  , test "missing argument" (assertEqual (Left "goto_line: missing argument <line>") (bindErr "goto_line"))
  , test "not a number" (assertEqual (Left "goto_line: <line> must be a number, got x") (bindErr "goto_line x"))
  , test "too many arguments" (assertEqual (Left "undo: unexpected argument 1 \"a b\"") (bindErr "undo 1 \"a b\""))
  , test "optional argument may be left out" (assertEqual (Right ()) (bindErr "move_line_down"))
  , test "optional argument may be given" (assertEqual (Right ()) (bindErr "move_line_down -3"))
  , test "bad choice" $
      assertEqual (Left "set_mode: <mode> must be one of normal, insert, select, got command") (bindErr "set_mode command")
  , test "duplicate action names are rejected" $
      assertEqual (Left "duplicate action names: a") (() <$ mkActionRegistry [simple "a" GMisc "" (pure ()), simple "a" GMisc "" (pure ())])
  , test "parameters describe themselves" $
      assertEqual
        (Just [Param "count" PInt (Just "1")])
        (actParams <$> lookupAction "move_line_down" registry)
  , test "every action name is a valid binding target" $
      assertEqual [] [actName a | a <- allActions, (invAction <$> parseInvocation (actName a)) /= Right (actName a)]
  , test "registry lists actions by group" $
      let groups = map actGroup (registryActions registry)
       in assertEqual True (and (zipWith (<=) groups (drop 1 groups)))
  ]
  where
    registry = either (error . T.unpack) id (mkActionRegistry allActions)
    bindErr t = () <$ bindText registry t

fileTests :: [Test]
fileTests =
  [ test "trailing newline is remembered" (assertEqual "a\nb\n" (roundTrip "a\nb\n"))
  , test "missing trailing newline is kept" (assertEqual "a\nb" (roundTrip "a\nb"))
  , test "CRLF is kept" (assertEqual "a\r\nb\r\n" (roundTrip "a\r\nb\r\n"))
  , test "CRLF lines are split" (assertEqual ["a", "b"] (B.toLines (docBuffer (decodeDocument Nothing "a\r\nb\r\n"))))
  , test "empty file stays empty" (assertEqual "" (roundTrip ""))
  , test "edited CRLF file saves CRLF everywhere" (assertEqual "a\r\nXb\r\nc\r\n" (editThenSave "a\r\nb\r\nc\r\n"))
  , test "edited LF file" (assertEqual "a\nXb\nc" (editThenSave "a\nb\nc"))
  , test "edited last line keeps no final newline" (assertEqual "a\nb\nXc" (encodeDocument (edit (Pos 2 0) (decodeDocument Nothing "a\nb\nc"))))
  , test "no final newline is remembered" (assertEqual False (docTrailingNewline (decodeDocument Nothing "a\nb")))
  , test "mixed endings follow the first line" (assertEqual ["a", "b\r", "c"] (B.toLines (docBuffer (decodeDocument Nothing "a\nb\r\nc"))))
  , test "chunks split anywhere decode the same" $
      let whole = "first line\r\nsecond æøå 漢字\r\n\r\nlast" :: ByteString
          expected = docBuffer (decodeDocument Nothing whole)
          splits = [docBuffer (decodeChunks Nothing [BS.take i whole, BS.drop i whole]) | i <- [0 .. BS.length whole]]
       in assertEqual [] [i | (i, b) <- zip [0 :: Int ..] splits, b /= expected]
  , test "many small chunks" $
      let whole = "a\nbb\nccc\n" :: ByteString
       in assertEqual (docBuffer (decodeDocument Nothing whole)) (docBuffer (decodeChunks Nothing [BS.singleton w | w <- BS.unpack whole]))
  ]
  where
    roundTrip :: ByteString -> ByteString
    roundTrip = encodeDocument . decodeDocument Nothing
    edit p d = d {docBuffer = fst (B.insertText p "X" (docBuffer d))}
    editThenSave = encodeDocument . edit (Pos 1 0) . decodeDocument Nothing

exTests :: [Test]
exTests =
  [ test "name and args" (assertEqual (Just ("w", ["file.txt"])) (parseExLine "w file.txt"))
  , test "blank line" (assertEqual Nothing (parseExLine "   "))
  ]

viewTests :: [Test]
viewTests =
  [ test "scrolls down with scrolloff" (assertEqual (View 3 0) (scrollToCursor (10, 80) 3 (9, 0) (View 0 0)))
  , test "scrolls up with scrolloff" (assertEqual (View 2 0) (scrollToCursor (10, 80) 3 (5, 0) (View 10 0)))
  , test "no scroll when visible" (assertEqual (View 0 0) (scrollToCursor (10, 80) 3 (4, 0) (View 0 0)))
  , test "scrolls right" (assertEqual (View 0 21) (scrollToCursor (10, 80) 3 (0, 100) (View 0 0)))
  ]

diffTests :: [Test]
diffTests =
  [ test "styles survive packing" $
      let samples = [defaultStyle, defaultStyle {styleFg = Rgb 1 2 3, styleBg = Indexed 240, styleBold = True, styleReverse = True}, defaultStyle {styleFg = Ansi 9, styleItalic = True, styleUnderline = True}]
       in assertEqual samples (map (unpackStyle . packStyle) samples)
  , test "300 random frame sequences replay exactly in a terminal model" (randomDiffs 300)
  , test "identical frames redraw no rows" (assertEqual False ("top" `isInfix` emit (Just f1) f1))
  , test "only the changed row is drawn" $
      let out = emit (Just f1) f2
       in assertEqual (True, False) ("\ESC[2;1H" `isInfix` out, "\ESC[1;1H" `isInfix` out)
  , test "no previous frame clears the screen" (assertEqual True ("\ESC[2J" `isInfix` emit Nothing f1))
  ]
  where
    f1 = putText 0 0 defaultStyle "top" (blankFrame 3 10)
    f2 = putText 1 0 defaultStyle "changed" f1
    emit p f = toLazyByteString (diffFrames p f)
    isInfix needle hay = BL.toStrict needle `BS.isInfixOf` BL.toStrict hay

widthTests :: [Test]
widthTests =
  [ test "ascii is narrow" (assertEqual 1 (charWidth 'a'))
  , test "CJK is wide" (assertEqual 2 (charWidth '漢'))
  , test "emoji is wide" (assertEqual 2 (charWidth '😀'))
  , test "control chars show as ^X" (assertEqual ("^A", 2) (glyphs '\SOH' 2, charWidth '\SOH'))
  , test "tab expands to the next stop" (assertEqual 4 (displayCol "\tx" 1))
  , test "tab after text" (assertEqual 4 (displayCol "ab\tx" 3))
  , test "wide chars shift columns" (assertEqual 4 (displayCol "漢字x" 2))
  , test "column inside a wide char maps to it" (assertEqual 1 (charIndexAtCol "漢字x" 3))
  , test "column past the end" (assertEqual 3 (charIndexAtCol "abc" 10))
  , test "j keeps the visual column across tabs" $
      let b = buf "\tabc\n    xyz"
          r = lineDown b (point (Pos 0 2))
       in assertEqual (Pos 1 5) (rangeHead r)
  ]

renderTests :: [Test]
renderTests =
  [ test "a listing colours its header and directories" $
      let doc = listingDocument "/x" 0 [DirEntry "f" False, DirEntry "d" True]
          f = render defaultTheme Nothing (newEditor (8, 30) doc)
          -- Column 6: past the gutter (sign lane, number, padding) and the cursor cell.
          styleOn row = fmap cellStyle (Seq.lookup 6 =<< Seq.lookup row (frameCells f))
       in assertEqual
            [Just (packStyle (themeDirectoryHeader defaultTheme)), Just (packStyle (themeDirectory defaultTheme)), Just (packStyle (themeText defaultTheme))]
            [styleOn 0, styleOn 2, styleOn 3]
  , test "a closed info box is redrawn, not copied from the row cache" $
      let ed = newEditor (12, 40) (newDocument Nothing (buf (T.intercalate "\n" (replicate 20 "some text here"))))
          withBox = ed {edInfo = Just (InfoBox "goto" [("g", "Go to the first line")] BottomRight)}
          f1 = render defaultTheme Nothing withBox
       in assertEqual (frameCells (render defaultTheme Nothing ed)) (frameCells (render defaultTheme (Just f1) ed))
  , test "gutter shows a sign lane and line numbers" (assertEqual "   1 hello" (T.take 10 (rowText (frameOf "hello") 0)))
  , test "wide chars use a continuation cell" $
      assertEqual [Just '漢', Just continuation, Just 'x'] (map (cellAt (frameOf "漢x") 0) [5, 6, 7])
  , test "control chars are drawn as ^X" (assertEqual "^[x" (T.take 3 (T.drop 5 (rowText (frameOf "\ESCx") 0))))
  , test "rendering with the previous frame gives the same frame" $
      let base = start "hello\nworld\nthird line"
          eds = [base, base {edMode = Insert}, base {edDoc = (edDoc base) {docSelection = single (Range (Pos 0 1) (Pos 1 2) Nothing)}}, base]
          withPrev = go Nothing eds
          go _ [] = []
          go p (e : es) = let f = render defaultTheme p e in f : go (Just f) es
       in assertEqual (map (frameCells . render defaultTheme Nothing) eds) (map frameCells withPrev)
  , test "long file names are shortened" $
      let ed = (start "") {edSize = (5, 30), edDoc = (newDocument (Just (replicate 60 'p' <> "/name.txt")) B.empty) {docDirty = True}}
          status = rowText (render defaultTheme Nothing ed) 3
       in assertEqual (True, True) ("[+]" `T.isInfixOf` status, "name.txt" `T.isInfixOf` status)
  ]
  where
    start t = newEditor (5, 40) (newDocument Nothing (buf t))
    frameOf t = render defaultTheme Nothing (start t)
    rowText f r = T.pack [c | Cell c _ <- maybe [] toList (lookupRow f r)]
    lookupRow f r = case drop r (toList (frameCells f)) of
      (row : _) -> Just row
      [] -> Nothing
    cellAt f r c = case drop c (maybe [] toList (lookupRow f r)) of
      (Cell ch _ : _) -> Just ch
      [] -> Nothing

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

-- | Replays diff output on a minimal terminal model and checks that the
-- screen equals the new frame (characters and styles, cell by cell).
randomDiffs :: Int -> Either String ()
randomDiffs n = go n (randoms 5) Nothing emptyScreen
  where
    rows = 5
    cols = 12
    styles = [defaultStyle, defaultStyle {styleReverse = True}, defaultStyle {styleFg = Indexed 240}]
    chars = "ab  漢x" :: String
    emptyScreen = replicate rows (replicate cols (' ', sgrText defaultStyle))
    go 0 _ _ _ = Right ()
    go k rs prev screen =
      let (frame, rs') = randomFrame rs
          out = TE.decodeUtf8 (BL.toStrict (toLazyByteString (diffFrames prev frame)))
          screen' = replay (T.unpack out) screen
          expected = [[(c, sgrText (unpackStyle st)) | Cell c st <- toList row] | row <- toList (frameCells frame)]
       in if screen' == expected
            then go (k - 1) rs' (Just frame) screen'
            else Left ("mismatch at step " <> show (n - k) <> ": " <> show out)
    -- Random text, and a random view position for rows 0-3 (so the diff
    -- sometimes scrolls the terminal).
    randomFrame rs0 =
      let (f, rs1) = foldl addText (blankFrame rows cols, rs0) [0 .. 5 :: Int]
          (r, rs2) = case rs1 of
            (x : xs) -> (x, xs)
            [] -> (0, [])
       in ((sanitize f) {frameScroll = Just (ScrollInfo 0 4 (r `mod` 6))}, rs2)
      where
        addText (f, r1 : r2 : r3 : r4 : rest) _ =
          let t = T.pack [chars !! (x `mod` length chars) | x <- take (r3 `mod` 6) rest]
              st = packStyle (styles !! (r4 `mod` length styles))
              cells = concat [if isWide c then [Cell c st, Cell continuation st] else [Cell c st] | c <- T.unpack t]
           in (putCells (r1 `mod` rows) (r2 `mod` cols) cells f, drop 6 rest)
        addText acc _ = acc
    -- Frames from the renderer never contain half a wide character; make
    -- the random ones valid the same way.
    sanitize f = f {frameCells = fmap (Seq.fromList . fixRow . toList) (frameCells f)}
    fixRow (Cell a sa : Cell b sb : rest)
      | isWide a && b == continuation = Cell a sa : Cell b sb : fixRow rest
      | isWide a = Cell ' ' sa : fixRow (Cell b sb : rest)
      | a == continuation = Cell ' ' sa : fixRow (Cell b sb : rest)
    fixRow [Cell a sa] | isWide a || a == continuation = [Cell ' ' sa]
    fixRow (c : rest) = c : fixRow rest
    fixRow [] = []
    sgrText st = TE.decodeUtf8 (BL.toStrict (toLazyByteString (sgr st)))
    -- The terminal model: cursor, current SGR, scroll region, and a grid of
    -- (char, SGR).
    replay str scr = run str (0 :: Int, 0 :: Int) (sgrText defaultStyle) (0, rows - 1) scr
    run [] _ _ _ scr = scr
    run ('\ESC' : '[' : rest) cur cs region scr =
      let (params, rest1) = span (\c -> c >= '0' && c <= '?') rest
          (inter, rest2) = span (\c -> c >= ' ' && c <= '/') rest1
          blankRow = [(' ', cs) | _ <- [1 .. cols]]
          (rt, rb) = region
       in case rest2 of
            (final : rest3) -> case final of
              'H' -> let (r, c) = break (== ';') params in run rest3 (read r - 1, read (drop 1 c) - 1) cs region scr
              'm' -> run rest3 cur cs' region scr where cs' = T.pack ("\ESC[" <> params <> inter <> "m")
              'K' -> let (r, c) = cur in run rest3 cur cs region (setRow r [(c', (' ', cs)) | c' <- [c .. cols - 1]] scr)
              'J' -> run rest3 cur cs region [blankRow | _ <- [1 .. rows]]
              'r' -> case break (== ';') params of
                ("", _) -> run rest3 (0, 0) cs (0, rows - 1) scr
                (a, b) -> run rest3 (0, 0) cs (read a - 1, read (drop 1 b) - 1) scr
              'S' -> run rest3 cur cs region (scrollRows (read params) rt rb blankRow scr)
              'T' -> run rest3 cur cs region (scrollRows (negate (read params)) rt rb blankRow scr)
              _ -> run rest3 cur cs region scr
            [] -> scr
    run (ch : rest) (r, c) cs region scr
      | isWide ch = run rest (r, c + 2) cs region (setRow r [(c, (ch, cs)), (c + 1, (continuation, cs))] scr)
      | otherwise = run rest (r, c + 1) cs region (setRow r [(c, (ch, cs))] scr)
    -- Positive: contents move up.
    scrollRows d rt rb blankRow scr =
      [ if i < rt || i > rb
          then row
          else case i + d of
            j | j >= rt && j <= rb -> scr !! j
            _ -> blankRow
      | (i, row) <- zip [0 ..] scr
      ]
    setRow r updates scr =
      [ if i == r then [maybe old id (lookup j updates) | (j, old) <- zip [0 ..] row] else row
      | (i, row) <- zip [0 ..] scr
      ]

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
  pendingG <- typeKeys "g" (start "abc")
  badChord <- typeKeys "g z" (start "abc")
  pure
    [ test "typing in insert mode" (assertEqual "hi there" typed)
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
          (Just ("goto", Just "Go to the first line"))
          ((\b -> (infoTitle b, lookup "g" (infoRows b))) <$> edInfo infoG)
    , test "the info box goes away after the chord" (assertEqual Nothing (edInfo infoAfterG))
    , test ": lists the matching commands" $
        assertEqual (Just ["write, w", "write-quit, wq, x", "write-all, wa", "write-quit-all, wqa, xa"]) (map fst . infoRows <$> edInfo infoColon)
    , test "after the name, the command is described" $
        assertEqual (Just ["open, o, edit, e"]) (map fst . infoRows <$> edInfo infoArgs)
    , test "tab completes a unique command" (assertEqual "buffer-next " (edCmdLine completeName))
    , test "tab extends to the common prefix and lists candidates" $
        assertEqual ("write", ["write", "write-quit", "write-all", "write-quit-all"]) (edCmdLine completeMany, edCompletions completeMany)
    , test "g waits for the next key" (assertEqual [plain (KChar 'g')] (edPending pendingG))
    , test "an unknown chord is dropped" (assertEqual ([], "abc") (edPending badChord, B.toText (docBuffer (edDoc badChord))))
    ]
  where
    isError = \case
      Just (Status Error _) -> True
      _ -> False
