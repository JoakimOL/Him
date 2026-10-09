-- | The plugin building blocks of ADR plugin-canvas (highlights, a buffer's own keys, a
-- canvas with its keys, timers), through a small plugin written against
-- "Him.Plugin"; and the contrib plugins built on them (magit,
-- tetris).
module Test.PluginCanvas
  ( pluginCanvasTests
  ) where

import Control.Monad.Trans.State.Strict (execStateT)
import Data.Foldable (foldlM)
import Data.IntMap.Strict qualified as IntMap
import Data.Map.Strict qualified as Map
import Data.Maybe (fromMaybe)
import Data.Sequence qualified as Seq
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as T
import Him.Buffer qualified as B
import Him.Config (Config (..))
import Him.Config.Default (configWithPlugins, defaultPluginsOf, plugins)
import Him.Contrib.Magit
import Him.Contrib.Tetris
import Him.Document
import Him.Editor
import Him.Effect (Effect (..), JobResult (..))
import Him.Event qualified as Ev
import Him.Key (parseKeys)
import Him.Mode (Mode (..))
import Him.Plugin
import Him.PluginState (lookupState)
import Him.PluginUI (OpenCanvas (..), puHighlights, puKeymaps)
import Him.Render (render)
import Him.Render.Frame (Cell (..), Frame (..))
import Him.Render.Theme (Theme (..), defaultTheme, scopeStyle)
import Him.Session (handleEvent, switchPlugins)
import Him.Terminal.Ansi (packStyle, patchStyle)
import System.Directory (createDirectoryIfMissing, getTemporaryDirectory, removeDirectoryRecursive, doesDirectoryExist)
import System.Process (callProcess, readProcess)
import Test.Harness
import Test.Util

-- | A plugin using each new building block; its state is what it heard.
demo :: PluginSpec [Text]
demo =
  (pluginSpec "demo" "uses highlights, buffer keys, a canvas and a timer" [])
    { psActions =
        [ action "demo_open" "a scratch buffer with highlights and keys" $ do
            b <- openScratch "demo" "abc\ndef"
            setHighlights b [Highlight 0 0 3 (face "keyword")]
            setBufferKeymap b (Just "buf")
        , action "demo_mark" "" (notify "marked")
        , action "demo_canvas" "" $ do
            showCanvas "box" (canvas "Box" 10 3) {canvasRows = [[("hi", face "keyword")]], canvasKeymap = Just "box"}
            startTimer "tick" 100
        , action "demo_left" "" (notify "left")
        ]
    , psBindings = [(Normal, "space z", "demo_open"), (Normal, "space Z", "demo_canvas"), (Normal, "space x", "command_mode_with \"magit-commit \"")]
    , psKeymaps = [("buf", [("s", "demo_mark")]), ("box", [("left", "demo_left")])]
    , psOnEvent = \case
        CanvasKey _ k -> modifyState (<> [k])
        CanvasClosed n -> modifyState (<> ["closed " <> n])
        TimerFired n -> modifyState (<> ["timer " <> n])
        _ -> pure ()
    }

pluginCanvasTests :: IO [Test]
pluginCanvasTests = do
  let every = plugins <> [hostPlugin demo]
  config <- either (fail . T.unpack) pure (configWithPlugins every (defaultPluginsOf every) Map.empty)
  off <- either (fail . T.unpack) pure (configWithPlugins every (Set.delete "demo" (defaultPluginsOf every)) Map.empty)
  let typeKeys ks ed = foldlM (\e k -> execStateT (handleEvent config (Ev.EvKey k)) e) ed (fromMaybe (error ks) (parseKeys (T.pack ks)))
      start = newEditor (24, 80) (newDocument Nothing (buf "plain text"))
      heard ed = fromMaybe [] (lookupState "demo" (edPluginStates ed)) :: [Text]
      status ed = fmap (\(Status _ t) -> t) (edStatus ed)
  -- A scratch buffer with a highlight and its own s.
  opened <- typeKeys "space z" start
  marked <- typeKeys "s" opened
  elsewhere <- typeKeys "s" start
  let f = render defaultTheme Nothing opened
      cellsOf r = maybe [] (foldr (:) []) (Seq.lookup r (frameCells f))
      -- b: a is under the cursor.
      styleOfB = [st | Cell 'b' st <- cellsOf 0]
      keyword = maybe (themeText defaultTheme) (patchStyle (themeText defaultTheme)) (scopeStyle defaultTheme "keyword")
  -- A canvas: drawn over everything, its keys, then esc.
  boxed <- typeKeys "space Z" start
  leftKey <- typeKeys "left" boxed
  unbound <- typeKeys "r j" boxed
  closed <- typeKeys "esc" unbound
  ticked <- execStateT (handleEvent config (Ev.EvJob (TimerTick "demo:tick"))) boxed
  switchedOff <- execStateT (switchPlugins config off) boxed
  prefilled <- typeKeys "space x" start
  let boxRows = [rowText (render defaultTheme Nothing boxed) r | r <- [0 .. 23]]
  -- magit on a real repository.
  gitTests <- magitIO
  pure $
    [ test "setHighlights colours the buffer's text" (assertEqual [packStyle keyword] (take 1 styleOfB))
    , test "the highlights and keymap are the plugin's UI" (assertEqual (True, Just "demo:buf") (any (IntMap.member 0 . snd) (IntMap.toList (maybe IntMap.empty puHighlights (Map.lookup "demo" (edPluginUI opened)))), IntMap.lookup (docId (edDoc opened)) (maybe IntMap.empty puKeymaps (Map.lookup "demo" (edPluginUI opened)))))
    , test "a buffer's keymap comes before normal mode's" (assertEqual (Just "marked") (status marked))
    , test "other buffers keep normal mode's keys" (assertEqual False (status elsewhere == Just "marked"))
    , test "a canvas is drawn with its title and cells" (assertEqual (True, True) (any (T.isInfixOf "Box") boxRows, any (T.isInfixOf "│hi") boxRows))
    , test "showing a canvas can start a timer" (assertEqual True (TimerStart "demo:tick" 100 `elem` edEffects boxed))
    , test "the canvas's keymap runs its actions" (assertEqual (Just "left") (status leftKey))
    , test "unbound keys go to the plugin, not the editor" (assertEqual (["r", "j"], posOf boxed) (heard unbound, posOf unbound))
    , test "esc closes the canvas and tells the plugin" (assertEqual (Nothing, ["r", "j", "closed box"]) (ocName <$> edCanvas closed, heard closed))
    , test "a timer's tick reaches its plugin" (assertEqual ["timer tick"] (heard ticked))
    , test "switching a plugin off closes its canvas and stops its timers" (assertEqual (Nothing, True) (ocName <$> edCanvas switchedOff, TimerStopAll "demo" `elem` edEffects switchedOff))
    , test "command_mode_with opens : with text" (assertEqual (CmdLine, "magit-commit ") (edMode prefilled, edCmdLine prefilled))
    -- magit, pure.
    , test "git status -z: branch, sections, renames" $
        assertEqual
          ("main", [Change Staged 'M' "a", Change Unstaged 'M' "a", Change Staged 'R' "new", Change Untracked '?' "u v", Change Unstaged 'U' "c"])
          (parseStatus "## main...origin/main [ahead 1]\0MM a\0R  new\0old\0?? u v\0UU c\0")
    , test "git status on a new repository" (assertEqual "main (no commits yet)" (fst (parseStatus "## No commits yet on main\0")))
    , test "a diff splits into files and hunks" (assertEqual [("a.txt", 2, 4), ("gone", 1, 3)] [(fdPath d, length (fdHunks d), length (fdHeader d)) | d <- parseDiff diffLines])
    , test "a hunk's patch is the file header and the hunk" (assertEqual (Just (T.unlines (take 4 diffLines <> ["@@ -9 +9 @@", "-x", "+y"]))) (parseDiff diffLines `atHunk` 1))
    , test "staging chosen lines: other additions go, other removals stay" $
        assertEqual
          (Just (T.unlines (header <> ["@@ -1,4 +1,4 @@", " one", "-two", "+TWO", " three", " four"])))
          (linesPatch False twoChanges (Set.fromList [(0, 2), (0, 3)]))
    , test "unstaging chosen lines is the other way round" $
        assertEqual
          (Just (T.unlines (header <> ["@@ -1,3 +1,4 @@", " one", "+TWO", " three", " FOUR"])))
          (linesPatch True twoChanges (Set.fromList [(0, 3)]))
    , test "a hunk with no chosen change is left out, and the next one's new start follows" $
        assertEqual
          (Just (T.unlines (header <> ["@@ -10,2 +10,3 @@", " x", "+y", " z"])))
          (linesPatch False (FileDiff "a.txt" header [Hunk "@@ -1,2 +1,3 @@" [" a", "+b", " c"], Hunk "@@ -10,2 +11,3 @@" [" x", "+y", " z"]]) (Set.fromList [(0, 1), (1, 2)]))
    , test "only context chosen: no patch" (assertEqual Nothing (linesPatch False twoChanges (Set.fromList [(0, 1)])))
    , test "the layout marks each line" $
        let (texts, hls, rows) = layout "" "main" [Change Unstaged 'M' "a.txt"] (Map.fromList [((Unstaged, "a.txt"), d) | d <- take 1 (parseDiff diffLines)]) (Set.singleton (Unstaged, "a.txt"))
         in assertEqual
              (Just "  modified   a.txt", Just (RowFile Unstaged "a.txt"), Just (RowHunk Unstaged "a.txt" 0), Just (RowHunkLine Unstaged "a.txt" 0 2), True)
              (lookupLine 3 texts, IntMap.lookup 3 rows, IntMap.lookup 4 rows, IntMap.lookup 6 rows, Highlight 6 0 4 (face "diff.plus") `elem` hls)
    -- tetris, pure.
    , test "a new game's piece is on the board" (assertEqual True (all (\(r, c) -> r >= 0 && c >= 0 && c < boardWidth) (cells (newGame 1))))
    , test "walls stop a piece" (assertEqual True (all ((>= 0) . snd) (cells (iterate (move (-1)) (newGame 7) !! 20))))
    , test "turning at a wall keeps the piece on the board" (assertEqual True (and [all (\(_, c) -> c >= 0 && c < boardWidth) (cells (rotate 1 (iterate (move d) g !! 20))) | d <- [-1, 1], k <- [0 .. 6], let g = (newGame 3) {gPiece = k}]))
    , test "a full row clears and scores" $
        let g = (newGame 5) {gPiece = 0, gRotation = 0, gRow = 0, gCol = 3, gBoard = Map.fromList [((boardHeight - 1, c), 1) | c <- [0, 1, 2, 7, 8, 9]]}
            g' = dropPiece g
         in assertEqual (1, Map.empty, True) (gLines g', gBoard g', gScore g' >= 100)
    , test "a piece that cannot come in ends the game" $
        let g = (newGame 9) {gBoard = Map.fromList [((r, c), 0) | r <- [2 .. boardHeight - 1], c <- [0 .. boardWidth - 2]]}
         in assertEqual True (gOver (fall (fall g)))
    ]
      <> gitTests
  where
    posOf ed = docSelection (edDoc ed)
    lookupLine i xs = case drop i xs of
      x : _ -> Just x
      [] -> Nothing
    atHunk ds k = case ds of
      d : _ -> hunkPatch d k
      [] -> Nothing
    header = ["diff --git a/a.txt b/a.txt", "--- a/a.txt", "+++ b/a.txt"]
    twoChanges = FileDiff "a.txt" header [Hunk "@@ -1,4 +1,4 @@" [" one", "-two", "+TWO", " three", "-four", "+FOUR"]]
    diffLines =
      [ "diff --git a/a.txt b/a.txt"
      , "index 1..2 100644"
      , "--- a/a.txt"
      , "+++ b/a.txt"
      , "@@ -1,2 +1,2 @@"
      , " one"
      , "+two"
      , "@@ -9 +9 @@"
      , "-x"
      , "+y"
      , "diff --git a/gone b/gone"
      , "--- a/gone"
      , "+++ /dev/null"
      , "@@ -1 +0,0 @@"
      , "-bye"
      ]

-- | The magit plugin in a temporary repository: the buffer lists the
-- change, and s on it stages the file.
magitIO :: IO [Test]
magitIO = do
  tmp <- getTemporaryDirectory
  let repo = tmp <> "/him-test-magit"
  exists <- doesDirectoryExist repo
  if exists then removeDirectoryRecursive repo else pure ()
  createDirectoryIfMissing True repo
  let git args = callProcess "git" (["-C", repo, "-c", "user.name=t", "-c", "user.email=t@t"] <> args)
  git ["init", "-q", "-b", "main"]
  writeFile (repo <> "/a.txt") "one\n"
  git ["add", "a.txt"]
  git ["commit", "-q", "-m", "init"]
  writeFile (repo <> "/a.txt") "one\ntwo\n"
  let every = plugins
  config <- either (fail . T.unpack) pure (configWithPlugins every (Set.insert "magit" (defaultPluginsOf every)) Map.empty)
  let typeKeys ks ed = foldlM (\e k -> execStateT (handleEvent config (Ev.EvKey k)) e) ed (fromMaybe (error ks) (parseKeys (T.pack ks)))
      start = newEditor (24, 80) (newDocument (Just (repo <> "/a.txt")) (buf "one\ntwo\n"))
      text = B.toText . docBuffer . edDoc
  rt <- testRuntime config
  shown <- settleUntil config rt 5000 (T.isInfixOf "Unstaged changes" . text) =<< typeKeys "space g g" start
  -- The file is on line 4 (Head, blank, header, file).
  staged <- settleUntil config rt 5000 (T.isInfixOf "Staged changes" . text) =<< typeKeys "4 g g s" shown
  porcelain <- readProcess "git" ["-C", repo, "status", "--porcelain"] ""
  removeDirectoryRecursive repo
  pure
    [ test "magit lists the unstaged change" (assertEqual True (T.isInfixOf "modified   a.txt" (text shown)))
    , test "s stages the file under the cursor" (assertEqual ("M  a.txt\n", True) (porcelain, T.isInfixOf "Staged changes (1)" (text staged)))
    ]
