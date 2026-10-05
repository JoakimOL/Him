-- | What plugins build on (ADR-50): events, their processes, status line
-- segments, gutter signs and annotations.
module Test.PluginApi
  ( pluginApiTests
  ) where

import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.State.Strict (execStateT, runStateT)
import Him.Buffer qualified as B
import Him.Key (KeyCode (..), plain)
import Him.Picker (Picker (..), PickerItem (..))
import Him.Plugin (getState, modifyState, openScratch, putState, replaceRange)
import Him.Plugin.Types (Ctx (..), runPluginM)
import Him.Position (Pos (..))
import Him.UserConfig (UserConfig (..), applyEditorOptions, applyUserConfig, parseUserConfig)
import System.Directory (doesDirectoryExist, getTemporaryDirectory, removeDirectoryRecursive)
import System.Environment (setEnv)
import System.FilePath (takeFileName)
import Data.Version (showVersion)
import Him.Rebuild (ListedPlugin (..), PluginList (..), Source (..), himSnapshot, parsePluginList, projectFiles)
import Paths_him (version)
import Data.Foldable (foldlM)
import Data.IORef (modifyIORef', newIORef, readIORef)
import Data.IntMap.Strict qualified as IntMap
import Data.IntSet qualified as IntSet
import Data.Map.Strict qualified as Map
import Data.Maybe (fromMaybe)
import Data.Text qualified as T
import Him.App (handleEvent)
import Him.Session (housekeeping)
import Him.Config (Config (..), Plugin (..), plugin)
import Him.Config.Default (defaultConfig)
import Him.Document
import Him.Editor
import Him.EditorM (request)
import Him.Effect (Effect (..))
import Him.Event qualified as Ev
import Him.Key (parseKeys)
import Him.Mode (Mode (..))
import Him.PluginEvent
import Him.PluginUI
import Him.Render (render)
import Him.Render.Theme (defaultTheme)
import Test.Harness
import Test.Util

pluginApiTests :: IO [Test]
pluginApiTests = do
  config0 <- either (fail . T.unpack) pure defaultConfig
  -- A plugin that writes down every event, and starts a process when the
  -- first document opens.
  seen <- newIORef []
  let recorder =
        (plugin "test" "records events")
          { plEvent = \ev -> do
              liftIO (modifyIORef' seen (<> [ev]))
              case ev of
                BufferOpened 1 -> request (ProcessStart "test:p" "printf" ["a\\nb"] Nothing)
                _ -> pure ()
          }
      config = config0 {cfgPlugins = cfgPlugins config0 <> [recorder]}
      typeKeys ks ed = foldlM (\e k -> execStateT (handleEvent config (Ev.EvKey k)) e) ed (fromMaybe (error ks) (parseKeys (T.pack ks)))
      start = newEditor (10, 40) (newDocument Nothing (buf "hello"))
  -- As the editor starts: housekeeping before the first key.
  _ <- settle config =<< typeKeys "i x esc" =<< execStateT (housekeeping config) start
  events <- readIORef seen
  -- The contrib plugins, switched on by a config file (ADR-51).
  uc <- either (fail . T.unpack . T.unlines) pure (parseUserConfig "[plugins]\nrecent-files = true\n[plugins.wordcount]\nenabled = true\nmax-lines = 3\n")
  contrib <- either (fail . T.unpack) pure (applyUserConfig uc)
  dir <- getTemporaryDirectory
  let stateDir = dir <> "/him-test-plugin-state"
      fileA = dir <> "/him-test-recent-a.txt"
      fileB = dir <> "/him-test-recent-b.txt"
  removeIfThere stateDir
  setEnv "HIM_STATE" stateDir
  writeFile fileA "alpha beta\n"
  writeFile fileB "gamma\n"
  let keysWith c ks ed = foldlM (\e k -> execStateT (handleEvent c (Ev.EvKey k)) e) ed (fromMaybe (error ks) (parseKeys (T.pack ks)))
      ex c line ed = foldlM (\e k -> execStateT (handleEvent c (Ev.EvKey k)) e) ed ([plain (KChar ':')] <> [plain (KChar ch) | ch <- T.unpack line] <> [plain KEnter])
      contribStart = applyEditorOptions uc (newEditor (10, 60) (newDocument Nothing (buf "one two three")))
  started <- execStateT (housekeeping contrib) contribStart
  typed <- keysWith contrib "i x space esc" started
  long <- keysWith contrib "o a ret b ret c esc" typed
  opened <- ex contrib ("o " <> T.pack fileB) =<< ex contrib ("o " <> T.pack fileA) started
  picked <- keysWith contrib "space o" opened
  forgot <- keysWith contrib "del" picked
  remembered <- readFile (stateDir <> "/plugins/recent-files/files")
  pluginsPicker <- ex contrib "plugins" start
  toggled <- keysWith contrib "ret" pluginsPicker
  -- The API on its own, for a plugin named t.
  let ctx = Ctx "t" (0 :: Int)
  (count, afterApi) <-
    runStateT
      ( runPluginM ctx $ do
          putState 5
          modifyState (+ 1)
          replaceRange 1 (Pos 0 0) (Pos 0 5) "bye"
          _ <- openScratch "out" "results"
          _ <- openScratch "out" "again"
          getState
      )
      (newEditor (10, 40) (newDocument Nothing (buf "hello world")))
  removeIfThere stateDir
  stackYaml <- readFile "stack.yaml"
  pure
    [ test "signs: the higher priority wins where they overlap; only the asked lines" $
        let ui n spans = (n, emptyPluginUI {puSigns = IntMap.singleton 1 spans})
            uis = Map.fromList [ui "a" [SignSpan 0 3 (sign "a" 1)], ui "b" [SignSpan 2 6 (sign "b" 2)]]
         in assertEqual [(1, "a"), (2, "b"), (3, "b")] (IntMap.toList (gsText <$> signsIn 1 1 3 uis))
    , test "segments: for this document or every one, best first" $
        let uis = Map.singleton "a" emptyPluginUI {puSegments = [(segment "x") {segDoc = Just 2}, (segment "y") {segPriority = 1}, segment "z"]}
         in assertEqual (["y", "z"], ["y", "x", "z"]) (map segText (segmentsFor 1 uis), map segText (segmentsFor 2 uis))
    , test "annotations by line, in the range" $
        let uis = Map.singleton "a" emptyPluginUI {puAnnotations = IntMap.singleton 1 [Annotation 0 "zero" (face "comment"), Annotation 5 "five" (face "comment")]}
         in assertEqual [0] (IntMap.keys (annotationsIn 1 0 3 uis))
    , test "closed documents' signs and segments are forgotten" $
        let uis = Map.singleton "a" emptyPluginUI {puSigns = IntMap.fromList [(1, []), (2, [])], puSegments = [(segment "x") {segDoc = Just 2}]}
         in assertEqual (Map.singleton "a" emptyPluginUI {puSigns = IntMap.singleton 1 []}) (dropDocuments (IntSet.singleton 1) uis)
    , test "events: opened and entered at first, then changes, saves, closes, modes" $
        let d v s i = (newDocument Nothing (buf "")) {docId = i, docVersion = v, docSaves = s}
            (first, s1) = detectEvents [d 0 0 1] 1 (Pos 0 0) Normal unseen
            (second, s2) = detectEvents [d 1 0 1, d 0 0 2] 2 (Pos 0 0) Insert s1
            (third, s3) = detectEvents [d 1 1 1] 1 (Pos 0 0) Insert s2
            (fourth, _) = detectEvents [d 1 1 1] 1 (Pos 0 3) Insert s3
         in assertEqual
              ( [BufferOpened 1, BufferEntered 1]
              , [BufferOpened 2, BufferChanged 1 1, BufferEntered 2, ModeChanged Normal Insert, CursorMoved 2 (Pos 0 0)]
              , [BufferClosed 2, BufferSaved 1, BufferEntered 1, CursorMoved 1 (Pos 0 0)]
              , [CursorMoved 1 (Pos 0 3)]
              )
              (first, second, third, fourth)
    , test "a plugin hears what happens, and its process's lines" $
        assertEqual
          [ BufferOpened 1
          , BufferEntered 1
          , ModeChanged Normal Insert
          , BufferChanged 1 1
          , CursorMoved 1 (Pos 0 1)
          , ModeChanged Insert Normal
          , ProcessOutput "p" "a"
          , ProcessOutput "p" "b"
          , ProcessExited "p" 0
          ]
          events
    , test "wordcount: the count follows edits; too long a buffer is not counted" $
        let segs e = map segText (segmentsFor (docId (edDoc e)) (edPluginUI e))
         in assertEqual (["3 words"], ["4 words"], []) (segs started, segs typed, segs long)
    , test "recent-files: space o lists the others, del forgets them, the list is kept in a file" $
        assertEqual
          (Just ["him-test-recent-a.txt"], ["him-test-recent-b.txt"], Nothing)
          ( map (T.pack . takeFileName . T.unpack . piLabel) . pkMatches <$> edPicker picked
          , map takeFileName (lines remembered)
          , map piLabel . pkMatches <$> edPicker forgot
          )
    , test "the :plugins picker lists them all; ret switches the chosen one" $
        assertEqual
          (Just ["git", "lsp", "repl", "chat", "wordcount", "recent-files"], [PluginCommand (Just ("git", False))])
          (map piLabel . pkMatches <$> edPicker pluginsPicker, [e | e@(PluginCommand _) <- edEffects toggled])
    , test "[plugins.<name>]: enabled and settings; unknown settings are errors" $
        assertEqual
          (Right (Map.singleton "wordcount" True), Left ["plugins.wordcount.colour: unknown setting (known: enabled, max-lines)"])
          (ucPlugins <$> parseUserConfig "[plugins.wordcount]\nenabled = true\n", parseUserConfig "[plugins.wordcount]\ncolour = 1\n")
    , test "the API: state, an edit as one change, a scratch buffer made once" $
        assertEqual
          (6, "bye world", ["[scratch]", "[out]"], "again", True)
          ( count
          , B.toText (docBuffer (head' (allDocuments afterApi)))
          , map displayName (allDocuments afterApi)
          , B.toText (docBuffer (edDoc afterApi))
          , isReadOnly (edDoc afterApi)
          )
    , test "plugins.toml: him from the release by default; plugins from git or a path" $
        assertEqual
          ( Right
              ( FromGit "https://github.com/JoakimOL/Him" (T.pack ("v" <> showVersion version))
              , [ ListedPlugin "harpoon" (FromGit "https://x/h" "v1") "him-harpoon" "Harpoon" "harpoon"
                , ListedPlugin "mine" (FromPath "p/mine") "mine" "My.Plugin" "mine"
                ]
              )
          )
          ( (\l -> (plHim l, plPlugins l))
              <$> parsePluginList
                ( T.unlines
                    [ "[plugins.harpoon]"
                    , "git = \"https://x/h\""
                    , "ref = \"v1\""
                    , "package = \"him-harpoon\""
                    , "module = \"Harpoon\""
                    , "spec = \"harpoon\""
                    , "[plugins.mine]"
                    , "path = \"p/mine\""
                    , "module = \"My.Plugin\""
                    , "spec = \"mine\""
                    ]
                )
          )
    , test "plugins.toml: problems are reported" $
        assertEqual
          [ Left ["plugins.a: module and spec are needed"]
          , Left ["plugins.b: give path, or git and ref"]
          , Left ["plugins.c: module must be a module name (Harpoon) and spec a name (harpoon)"]
          ]
          [ () <$ parsePluginList "[plugins.a]\npath = \"x\"\n"
          , () <$ parsePluginList "[plugins.b]\ngit = \"x\"\nmodule = \"B\"\nspec = \"b\"\n"
          , () <$ parsePluginList "[plugins.c]\npath = \"x\"\nmodule = \"c\"\nspec = \"C\"\n"
          ]
    , test "the project --rebuild writes: him and the plugins as dependencies, a Main of hostPlugin" $
        let files = projectFiles (PluginList (FromPath "/him") [ListedPlugin "h" (FromGit "u" "r") "him-h" "Harpoon" "harpoon"])
            file f = maybe [] T.lines (lookup f files)
         in assertEqual
              (True, True, True)
              ( all (`elem` file "stack.yaml") ["- /him", "- git: u", "  commit: r"]
              , "main = himMain [hostPlugin Harpoon.harpoon]" `elem` file "Main.hs"
              , "  build-depends: base, him, him-h" `elem` file "him-personal.cabal"
              )
    , test "personal builds use him's own snapshot" $
        assertEqual True (("snapshot: " <> T.unpack himSnapshot) `elem` lines stackYaml)
    , test "a segment, a sign and an annotation are drawn" $
        let ed0 = newEditor (6, 40) (newDocument Nothing (buf "hello\nworld"))
            ui =
              emptyPluginUI
                { puSegments = [segment "SEG"]
                , puSigns = IntMap.singleton 1 [SignSpan 1 2 (sign "*" 1)]
                , puAnnotations = IntMap.singleton 1 [Annotation 0 "note" (face "comment")]
                }
            ed = ed0 {edPluginUI = Map.singleton "t" ui, edSignLane = True}
            rows = [rowText (render defaultTheme Nothing ed) r | r <- [0 .. 5]]
         in assertEqual (True, True, True) (any (T.isInfixOf "hello note") rows, any (T.isPrefixOf "*") rows, any (T.isInfixOf "SEG") rows)
    ]
  where
    sign t p = GutterSign t (face "diff.plus") p
    head' = \case
      x : _ -> x
      [] -> error "no documents"
    removeIfThere d = doesDirectoryExist d >>= \e -> if e then removeDirectoryRecursive d else pure ()
