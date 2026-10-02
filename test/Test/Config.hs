-- | The config file, themes, plugins, rebinding keys.
module Test.Config
  ( userConfigTests
  , themeTests
  , pluginTests
  , remapTests
  , rebindTests
  ) where

import Control.Monad.Trans.State.Strict (execStateT)
import Data.ByteString qualified as BS
import Data.ByteString.Builder (toLazyByteString)
import Data.ByteString.Lazy qualified as BL
import Data.Foldable (foldlM)
import Data.Maybe (fromMaybe)
import Him.Action
import Him.App (handleEvent)
import Him.Buffer qualified as B
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Him.Config (Config (..), Plugin (..))
import Him.Actions.Git (gitPlugin)
import Him.Actions.Lsp (lspPlugin)
import Him.Config.Default (allPlugins, configWith, defaultConfig)
import Him.Document
import Him.Editor
import Him.Event (Event (..))
import Him.Ex (ExCommand (..))
import System.Directory (getTemporaryDirectory)
import Him.Options
import Him.Picker
import Him.Effect (Effect (..))
import Him.UserConfig
import Him.Lsp.Config (ServerConfig (..), defaultServers)
import Him.Config.Default (defaultBindings)
import System.Environment (setEnv, unsetEnv)
import Him.Lsp.Protocol
import Him.Lsp.State (DocLsp (..))
import Data.Maybe (isJust)
import Him.GitState
import Him.Palette (paletteItems)
import Him.Key
import Him.Keymap
import Him.Mode (Mode (..))
import Him.Position (Pos (..))
import Him.Render.Diff (diffFrames)
import Him.Selection
import Him.Terminal.Ansi (Color (..), Style (..), Underline (..), defaultStyle, patchStyle)
import Him.Render (render)
import Him.Render.Theme (Theme (..), defaultTheme, fromScopes)
import Him.Theme (defaultThemeText, downsample, mergeThemeFiles, parseColor, parseThemeFile, resolveTheme)
import Data.Text qualified as T
import Test.Harness
import Test.Util

userConfigTests :: [Test]
userConfigTests =
  [ test "the dumped defaults read back as the defaults" $
      case parseUserConfig defaultConfigText of
        Left e -> Left (show e)
        Right uc -> do
          assertEqual defaultBindings (ucBindings uc)
          assertEqual defaultOptions (userOptions uc)
          assertEqual (Right defaultServers) (cfgServers <$> applyUserConfig uc)
  , test "unknown sections, modes and settings are reported, all of them" $
      assertEqual
        (Left ["unknown section [keyz] (known: editor, keys, language-server, repl, chat, plugins)", "unknown setting editor.tabs (known in [editor]: scrolloff, show-hidden-files, tab-width, expand-tab, line-number, escape-timeout)", "unknown mode [keys.nromal] (known: normal, select, insert, command, picker, directory, completion, repl, chat)"])
        (parseUserConfig "[keyz]\n[editor]\ntabs = 2\n[keys.nromal]\n")
  , test "a binding must be an action in quotes" $
      assertEqual (Left ["keys.normal.j: the value must be an action in quotes, e.g. \"move_line_down\""]) (parseUserConfig "[keys.normal]\nj = 5\n")
  , test "an unknown action is caught when applied" $
      assertEqual (Left "Normal mode, j: unknown action: fly") (() <$ (applyUserConfig =<< either (Left . T.unlines) Right (parseUserConfig "[keys.normal]\nj = \"fly\"\n")))
  , test "servers: change, disable, add" $
      let uc = either (error . show) id (parseUserConfig "[language-server.rust]\nargs = [\"--x\"]\n[language-server.go]\nenabled = false\n[language-server.lua]\ncommand = \"lua-ls\"\n")
          table = either (error . T.unpack) cfgServers (applyUserConfig uc)
       in assertEqual (Just ("rust-analyzer", ["--x"]), Nothing, Just "lua-ls")
            (fmap (\sc -> (scCommand sc, scArgs sc)) (Map.lookup "rust" table), Map.lookup "go" table, scCommand <$> Map.lookup "lua" table)
  , test "a new language needs a command" $
      assertEqual (Left "language-server.zig: no built-in server, so a command is needed") (() <$ (applyUserConfig =<< either (Left . T.unlines) Right (parseUserConfig "[language-server.zig]\nargs = []\n")))
  , test "editor settings apply to the editor, also from sub-tables" $
      let uc = either (error . show) id (parseUserConfig "[editor]\nscrolloff = 7\nshow-hidden-files = true\nline-number = \"relative\"\n[editor.search]\nsmart-case = false\n[editor.cursor-shape]\ninsert = \"underline\"\n")
          o = edOptions (applyEditorOptions uc (newEditor (24, 80) (newDocument Nothing (buf ""))))
       in assertEqual (7, True, LineNumbersRelative, False, CursorKindUnderline)
            (optScrolloff o, optShowHidden o, optLineNumbers o, optSmartCase o, optCursorInsert o)
  , test "settings are checked: types, ranges, choices, unknown keys in sub-tables" $
      assertEqual
        (Left ["editor.tab-width must be a whole number, at least 1", "editor.line-number must be one of \"absolute\", \"relative\", \"off\"", "editor.search.smart-case must be true or false", "unknown setting editor.search.fuzzy (known in [editor.search]: smart-case, wrap-around)"])
        (parseUserConfig "[editor]\ntab-width = 0\nline-number = \"roman\"\n[editor.search]\nsmart-case = 1\nfuzzy = true\n")
  ]

themeTests :: [Test]
themeTests =
  [ test "colours: palette (also chained), hex, #rgb, index, names, default" $
      let palette = Map.fromList [("fg", "accent"), ("accent", "#ff8000")]
       in assertEqual
            [Just (Rgb 255 128 0), Just (Rgb 1 2 3), Just (Rgb 0xcc 0x77 0xcc), Just (Indexed 110), Just (Ansi 9), Just (Ansi 7), Just DefaultColor, Nothing, Nothing]
            (map (parseColor palette) ["fg", "#010203", "#c7c", "110", "light-red", "light-gray", "default", "nope", "#12345"])
  , test "styles: a string is the foreground; tables have colours, modifiers, underlines" $
      let tf = themeFile "\"a\" = \"red\"\n\"b\" = { fg = \"c\", bg = \"#000000\", modifiers = [\"bold\", \"crossed_out\"] }\n\"d.e\".underline = { color = \"c\", style = \"curl\" }\n[palette]\nc = \"5\"\n"
       in assertEqual
            ( Map.fromList
                [ ("a", defaultStyle {styleFg = Ansi 1})
                , ("b", defaultStyle {styleFg = Indexed 5, styleBg = Rgb 0 0 0, styleBold = True, styleStrike = True})
                , ("d.e", defaultStyle {styleUnderline = UnderlineCurl, styleUnderlineColor = Indexed 5})
                ]
            , []
            )
            (resolveTheme tf)
  , test "what is not understood is skipped with a warning" $
      let tf = themeFile "\"a\" = { fg = \"nope\", modifiers = [\"bold\", \"sparkly\"] }\nrainbow = [\"red\"]\n"
       in assertEqual (Map.fromList [("a", defaultStyle {styleBold = True})], ["a: unknown colour nope", "a: unknown modifier sparkly"]) (resolveTheme tf)
  , test "a child replaces its parent's entries whole and merges palettes; its palette colours the parent's" $
      let parent = themeFile "\"a\" = { fg = \"x\", bg = \"y\" }\n\"b\" = \"y\"\n[palette]\nx = \"1\"\ny = \"2\"\n"
          child = themeFile "inherits = \"p\"\n\"a\" = { fg = \"x\" }\n[palette]\ny = \"3\"\n"
       in assertEqual
            (Map.fromList [("a", defaultStyle {styleFg = Indexed 1}), ("b", defaultStyle {styleFg = Indexed 3})])
            (fst (resolveTheme (mergeThemeFiles parent child)))
  , test "the built-in theme parses without warnings" $
      assertEqual (Right []) (snd . resolveTheme <$> parseThemeFile defaultThemeText)
  , test "UI styles fall back to Helix's scopes" $
      let t = fromScopes "t" (Map.fromList [("ui.statusline", defaultStyle {styleBg = Indexed 1}), ("ui.statusline.normal", defaultStyle {styleFg = Indexed 2}), ("diff.plus", defaultStyle {styleFg = Indexed 3}), ("error", defaultStyle {styleFg = Indexed 4})])
       in assertEqual
            ( defaultStyle {styleFg = Indexed 2, styleBg = Indexed 1}
            , defaultStyle {styleFg = Indexed 3, styleDim = True}
            , defaultStyle {styleUnderline = UnderlineLine, styleUnderlineColor = Indexed 4}
            )
            (themeMode t CmdLine, themeGitSign t SignAdded True, themeDiagnosticText t SevError)
  , test "24-bit colours become the nearest of the 256" $
      assertEqual [Indexed 196, Indexed 16, Indexed 231, Indexed 244, Indexed 3]
        (map (styleFg . downsample . (\c -> defaultStyle {styleFg = c})) [Rgb 255 0 0, Rgb 0 0 0, Rgb 255 255 255, Rgb 128 128 128, Indexed 3])
  , test "laying styles over each other keeps what the top one leaves out" $
      assertEqual
        defaultStyle {styleFg = Indexed 1, styleBg = Indexed 2, styleBold = True, styleItalic = True}
        (patchStyle defaultStyle {styleFg = Indexed 1, styleBold = True} defaultStyle {styleBg = Indexed 2, styleItalic = True})
  , test "the first frame sets the theme's default colours; an unchanged one does not repeat them" $
      let t = fromScopes "t" (Map.fromList [("ui.background", defaultStyle {styleBg = Rgb 0x28 0x2c 0x34}), ("ui.text", defaultStyle {styleFg = Indexed 15})])
          ed = newEditor (5, 30) (newDocument Nothing (buf "x"))
          f = render t Nothing ed
          out p = BL.toStrict (toLazyByteString (diffFrames p f))
       in assertEqual (True, True, False)
            ("\ESC]11;rgb:28/2c/34\ESC\\" `BS.isInfixOf` out Nothing, "\ESC]10;rgb:ff/ff/ff\ESC\\" `BS.isInfixOf` out Nothing, "\ESC]1" `BS.isInfixOf` out (Just f))
  ]

pluginTests :: [Test]
pluginTests =
  [ test "a plugin switched off has no actions, keys or : commands; user keys to it are left out" $
      let c = either (error . T.unpack) id $ configWith (Set.fromList ["git"]) (Map.fromList [(Normal, [("Z", "goto_definition"), ("Y", "goto_next_change")])])
          normal = Map.findWithDefault emptyKeymap Normal (cfgKeymaps c)
          found ks = case resolve normal (fromMaybe [] (parseKeys ks)) of
            Found _ -> True
            _ -> False
       in assertEqual
            (Nothing, True, [False, False, True, True], False, ["git"])
            ( fmap actName (lookupAction "goto_definition" (cfgActions c))
            , isJust (lookupAction "git_stage_file" (cfgActions c))
            , map found ["g d", "Z", "] g", "Y"]
            , any (("lsp-info" `elem`) . exNames) (cfgExCommands c)
            , map plName (cfgPlugins c)
            )
  , test "[plugins] switches plugins off; unknown names are errors" $
      assertEqual
        (Right (Set.fromList ["chat", "git", "repl"]), Left ["unknown plugin gti (known: chat, git, lsp, repl)"])
        (enabledPlugins <$> parseUserConfig "[plugins]\nlsp = false\n", parseUserConfig "[plugins]\ngti = false\n")
  , test "without sign-drawing plugins the gutter has no sign lane" $
      let ed = (newEditor (5, 40) (newDocument Nothing (buf "hello"))) {edSignLane = False}
       in assertEqual "  1 hello" (T.take 9 (rowText (render defaultTheme Nothing ed) 0))
  , test "switching git off forgets the signs; switching the LSP off stops the servers" $
      let ed = newEditor (5, 40) (newDocument Nothing (buf "x"))
          gitOff = runNoIO (plDisable gitPlugin) ed {edDoc = (edDoc ed) {docGit = GitOutside}}
          lspOff = runNoIO (plDisable lspPlugin) ed {edDoc = (edDoc ed) {docLsp = LspNone}}
       in assertEqual (GitUnknown, LspUnknown, [LspStopAll]) (docGit (edDoc gitOff), docLsp (edDoc lspOff), edEffects lspOff)
  ]

-- | A user config remapping keys, used like the editor uses it.
remapTests :: IO [Test]
remapTests = do
  let uc = either (error . show) id (parseUserConfig "[keys.normal]\n\"j\" = \"move_line_up\"\n\"C-j\" = \"move_line_down 2\"\n\"x\" = \"no_op\"\n[keys.insert]\n\"C-a\" = \"normal_mode\"\n")
  config <- either (fail . T.unpack) pure (applyUserConfig uc)
  let start t = newEditor (24, 80) (newDocument Nothing (buf t))
      run ed k = execStateT (handleEvent config (EvKey k)) ed
      keys ks ed = foldlM run ed (fromMaybe (error ks) (parseKeys (T.pack ks)))
      headOf = rangeHead . primary . docSelection . edDoc
  down <- keys "C-j" (start "a\nb\nc\nd")
  up <- keys "j" =<< keys "C-j" (start "a\nb\nc\nd")
  unbound <- keys "x" (start "a\nb")
  insertExit <- keys "i C-a" (start "")
  dir <- getTemporaryDirectory
  let path = dir <> "/him-test-config/config.toml"
  setEnv "HIM_CONFIG" path
  opened <- keys ": c o n f i g minus o p e n ret" (start "")
  unsetEnv "HIM_CONFIG"
  pure
    [ test "a key bound to an action with an argument" (assertEqual (Pos 2 0) (headOf down))
    , test "a default key rebound" (assertEqual (Pos 1 0) (headOf up))
    , test "a key unbound with no_op" (assertEqual (Pos 0 0, Pos 0 0) (rangeAnchor (primary (docSelection (edDoc unbound))), headOf unbound))
    , test "insert-mode bindings" (assertEqual Normal (edMode insertExit))
    , test ":config-open on a missing file shows the defaults, unsaved, at the path" $
        assertEqual (Just path, True, True) (docPath (edDoc opened), docDirty (edDoc opened), "[keys.normal]" `T.isInfixOf` B.toText (docBuffer (edDoc opened)))
    ]

-- | Keys through a configuration with user bindings on top of the defaults.
rebindTests :: IO [Test]
rebindTests = do
  config <-
    either (fail . T.unpack) pure $
      configWith allPlugins
        ( Map.fromList
            [ (Normal, [("C-d", "move_line_down 2"), ("j", "no_op"), ("space i", "insert_text \"// \""), ("Q", "ex q!"), ("g 3", "goto_line 3"), ("F", "search_text two")])
            , (Insert, [("C-a", "set_mode normal"), ("j j", "normal_mode")])
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
  jjExit <- typeKeys "i a j j" (start "")
  jTyped <- typeKeys "i a j o j k" (start "")
  jPending <- typeKeys "i j" (start "")
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
        configWith allPlugins
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
    , test "an insert-mode chord (j j) runs" (assertEqual (Normal, "a") (edMode jjExit, B.toText (docBuffer (edDoc jjExit))))
    , test "a chord's first key not followed by the rest is typed" (assertEqual (Insert, "ajojk") (edMode jTyped, B.toText (docBuffer (edDoc jTyped))))
    , test "a chord's first key waits for the next" (assertEqual ("", [plain (KChar 'j')], Nothing) (B.toText (docBuffer (edDoc jPending)), edPending jPending, edInfo jPending))
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
    , test "picker details stay in one column while filtering" $
        let p = newPicker "t" [pickerItem "a" (PickFile "") "x", pickerItem "a much longer label" (PickFile "") "y"]
         in assertEqual (19, 19) (pkLabelWidth p, pkLabelWidth (setQuery "a" p))
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
