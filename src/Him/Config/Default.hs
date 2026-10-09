-- | The default actions and keybindings.
--
-- To add a keybinding, add a @(keys, action)@ pair to the right mode below;
-- the action may take arguments (@"move_line_down 5"@). To add an action,
-- add it to one of the @Him.Actions.*@ modules (or a new one, listed in
-- 'allActions').
module Him.Config.Default
  ( defaultConfig
  , defaultBindings
  , bindingsWith
  , configWith
  , configWithPlugins
  , allActions
  , actionsFor
  , plugins
  , allPlugins
  , defaultPlugins
  , defaultPluginsOf
  , pluginOf
  , pluginOfIn
  , fallback
  ) where

import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Data.Text (Text)
import Data.List (find)
import Data.Text qualified as T
import Him.Action (Action (..), Invocation (..), parseInvocation)
import Him.EditorM (EditorM, failWith, request)
import Him.Effect (Effect (..))
import Him.Ex (ExArgs (..), ExCommand (..))
import Him.Actions.CommandLine qualified as CommandLine
import Him.Actions.Directory qualified as Directory
import Him.Actions.Edit qualified as Edit
import Him.Actions.Register qualified as Register
import Him.Actions.Git qualified as Git
import Him.Actions.Lsp qualified as Lsp
import Him.Actions.File qualified as File
import Him.Actions.Motion qualified as Motion
import Him.Actions.Picker qualified as Picker
import Him.Actions.Jump qualified as Jump
import Him.Actions.Search qualified as Search
import Him.Actions.Window qualified as Window
import Him.Actions.Match qualified as Match
import Him.Actions.Repl qualified as Repl
import Him.Actions.Chat qualified as Chat
import Him.Chat (ChatProvider)
import Him.Chat.Anthropic (anthropicProvider)
import Him.Chat.ClaudeCode (claudeCodeProvider)
import Him.Contrib (contribPlugins)
import Him.Config (Bindings, Config (..), Plugin (..), buildConfig, overrideBindings)
import Him.Key (Key (..), KeyCode (..), Modifier (..), plain)
import Him.Mode (Mode (..))
import Him.Syntax (SyntaxProvider)
import Him.Syntax.TreeSitter (treeSitter)

-- | Every plugin there is (ADR git-and-lsp-as-plugins): the built-in ones, then the contrib
-- collection (ADR plugin-api), in the order their hooks run.
plugins :: [Plugin]
plugins = [Git.gitPlugin, Lsp.lspPlugin, Repl.replPlugin, Chat.chatPlugin] <> contribPlugins

-- | All of them switched on (the default).
allPlugins :: Set.Set Text
allPlugins = Set.fromList (map plName plugins)

-- | The plugin an action comes from, if any.
pluginOf :: Text -> Maybe Plugin
pluginOf = pluginOfIn plugins

pluginOfIn :: [Plugin] -> Text -> Maybe Plugin
pluginOfIn every name = find (\p -> name `elem` map actName (plActions p)) every

-- | The core's actions, without the command-line ones (which need the
-- @:@ commands, see 'actionsWith').
coreActions :: [Action]
coreActions =
  Motion.actions
    <> Edit.actions
    <> Register.actions
    <> Search.actions
    <> File.actions
    <> Picker.actions
    <> Jump.actions
    <> Directory.actions
    <> Window.actions
    <> Match.actions

-- | The actions with these plugins on (of all these).
actionsWith :: [Plugin] -> [Plugin] -> [Action]
actionsWith every on = coreActions <> concatMap plActions on <> CommandLine.actions (exCommandsWith every on)

-- | Every action, of the core and of every plugin (for the dumped config).
allActions :: [Action]
allActions = actionsFor plugins

-- | Every action of the core and of these plugins.
actionsFor :: [Plugin] -> [Action]
actionsFor every = actionsWith every every

exCommandsWith :: [Plugin] -> [Plugin] -> [ExCommand]
exCommandsWith every on = File.exCommands <> Window.exCommands <> Register.exCommands <> pluginCommands every <> concatMap plExCommands on

-- | Switching plugins while running (carried out by the main loop).
pluginCommands :: [Plugin] -> [ExCommand]
pluginCommands every =
  [ ExCommand ["plugins"] "List the plugins and whether they are on" NoArgs $ \_ -> request (PluginCommand Nothing)
  , ExCommand ["plugin-enable"] "Switch a plugin on" (NameArgs names) $ \case
      [name] -> request (PluginCommand (Just (name, True)))
      _ -> failWith ("usage: :plugin-enable <name> (" <> T.intercalate ", " names <> ")")
  , ExCommand ["plugin-disable"] "Switch a plugin off" (NameArgs names) $ \case
      [name] -> request (PluginCommand (Just (name, False)))
      _ -> failWith ("usage: :plugin-disable <name> (" <> T.intercalate ", " names <> ")")
  ]
  where
    names = map plName every

-- | Movement keys shared by normal, select and insert mode.
arrowBindings :: [(Text, Text)]
arrowBindings =
  [ ("left", "move_char_left")
  , ("right", "move_char_right")
  , ("up", "move_line_up")
  , ("down", "move_line_down")
  , ("home", "goto_line_start")
  , ("end", "goto_line_end")
  ]

normalBindings :: [(Text, Text)]
normalBindings =
  arrowBindings
    <> [ ("h", "move_char_left")
       , ("j", "move_line_down")
       , ("k", "move_line_up")
       , ("l", "move_char_right")
       , ("w", "move_next_word_start")
       , ("b", "move_prev_word_start")
       , ("e", "move_next_word_end")
       , ("x", "select_line")
       , ("X", "extend_to_line_bounds")
       , ("f", "find_next_char")
       , ("t", "find_till_char")
       , ("F", "find_prev_char")
       , ("T", "till_prev_char")
       , ("A-.", "repeat_last_find")
       , ("C-f", "page_down")
       , ("C-b", "page_up")
       , ("C-d", "half_page_down")
       , ("C-u", "half_page_up")
       , ("pagedown", "page_down")
       , ("pageup", "page_up")
       , ("C-z", "suspend")
       , (";", "collapse_selection")
       , ("%", "select_all")
       , ("s", "select_matches")
       , ("C", "copy_selection_on_next_line")
       , (",", "keep_primary_selection")
       , ("A-,", "remove_primary_selection")
       , (")", "rotate_selections_forward")
       , ("(", "rotate_selections_backward")
       , ("A-s", "split_selection_on_newline")
       , ("v", "select_mode")
       , ("d", "delete_selection")
       , ("c", "change_selection")
       , ("y", "yank")
       , ("p", "paste_after")
       , ("P", "paste_before")
       , ("\"", "select_register")
       , ("space y", "yank_to_clipboard")
       , ("space p", "paste_clipboard_after")
       , ("space P", "paste_clipboard_before")
       , ("r", "replace")
       , ("R", "replace_with_yanked")
       , ("space R", "replace_with_clipboard")
       , ("u", "undo")
       , ("U", "redo")
       , ("g g", "goto_file_start")
       , ("g e", "goto_last_line")
       , ("g h", "goto_line_start")
       , ("g l", "goto_line_end")
       , ("space f", "file_picker")
       , ("space b", "buffer_picker")
       , ("space /", "global_search")
       , ("space j", "jumplist_picker")
       , ("C-o", "jump_backward")
       , ("C-i", "jump_forward")
       , ("tab", "jump_forward")
       , ("C-s", "save_selection")
       , ("space ?", "command_palette")
       , ("space d", "directory_of_buffer")
       , ("space D", "directory_of_cwd")
       , ("g n", "buffer_next")
       , ("g p", "buffer_previous")
       , ("i", "insert_mode")
       , ("a", "append_mode")
       , ("m m", "match_brackets")
       , ("m s", "surround_add")
       , ("m r", "surround_replace")
       , ("m d", "surround_delete")
       , ("m i", "select_textobject_inner")
       , ("m a", "select_textobject_around")
       , ("I", "insert_at_line_start")
       , ("A", "insert_at_line_end")
       , ("o", "open_below")
       , ("O", "open_above")
       , (":", "command_mode")
       , ("/", "search_forward")
       , ("?", "search_backward")
       , ("n", "search_next")
       , ("N", "search_prev")
       , ("*", "search_selection")
       ]
    <> Window.windowBindings

-- | Select mode is normal mode with these overrides.
selectBindings :: [(Text, Text)]
selectBindings =
  [ ("esc", "normal_mode")
  , ("v", "normal_mode")
  ]

insertBindings :: [(Text, Text)]
insertBindings =
  arrowBindings
    <> [ ("esc", "normal_mode")
       , ("ret", "insert_newline")
       , ("tab", "insert_tab")
       , ("backspace", "delete_char_backward")
       , ("del", "delete_char_forward")
       , ("C-r", "insert_register")
       ]

commandBindings :: [(Text, Text)]
commandBindings =
  [ ("esc", "cmdline_cancel")
  , ("ret", "cmdline_execute")
  , ("tab", "cmdline_complete")
  , ("S-tab", "cmdline_complete_previous")
  , ("backspace", "cmdline_backspace")
  ]

-- | Normal mode in a directory listing is normal mode with these.
directoryBindings :: [(Text, Text)]
directoryBindings =
  [ ("ret", "directory_open")
  , ("-", "directory_parent")
  , ("^", "directory_parent")
  , ("backspace", "directory_parent")
  , ("g r", "directory_refresh")
  , ("g .", "directory_toggle_hidden")
  , ("a", "directory_new_file")
  , ("+", "directory_new_directory")
  , ("r", "directory_rename")
  , ("d", "directory_delete")
  ]

pickerBindings :: [(Text, Text)]
pickerBindings =
  [ ("esc", "picker_close")
  , ("C-c", "picker_close")
  , ("ret", "picker_accept")
  , ("down", "picker_next")
  , ("C-n", "picker_next")
  , ("tab", "picker_mark")
  , ("up", "picker_previous")
  , ("C-p", "picker_previous")
  , ("S-tab", "picker_previous")
  , ("backspace", "picker_backspace")
  , ("del", "picker_secondary")
  ]

-- | The default bindings, of the core and every plugin. Select mode also
-- gets normal mode's bindings (see 'Him.Config.inheritsFrom').
defaultBindings :: Bindings
defaultBindings = bindingsWith plugins

-- | The core's bindings, then the plugins'.
bindingsWith :: [Plugin] -> Bindings
bindingsWith on = foldl' (Map.unionWith (<>)) coreBindings (map plBindings on)

coreBindings :: Bindings
coreBindings =
  Map.fromList
    [ (Normal, normalBindings)
    , (Select, selectBindings)
    , (Insert, insertBindings)
    , (CmdLine, commandBindings)
    , (Picking, pickerBindings)
    , (Directory, directoryBindings)
    ]

-- | The default configuration: the plugins that are on by default. Every
-- binding is checked against the actions; an error lists each bad binding.
defaultConfig :: Either Text Config
defaultConfig = configWith defaultPlugins Map.empty

-- | The plugins on by default (the built-in ones; contrib plugins are off).
defaultPlugins :: Set.Set Text
defaultPlugins = defaultPluginsOf plugins

defaultPluginsOf :: [Plugin] -> Set.Set Text
defaultPluginsOf every = Set.fromList [plName p | p <- every, plDefaultOn p]

-- | The configuration with these plugins on, and the user's bindings over
-- the defaults. User bindings to a switched-off plugin's actions are left
-- out (they come back with the plugin).
configWith :: Set.Set Text -> Bindings -> Either Text Config
configWith = configWithPlugins plugins

-- | The same, for a build with these plugins ("Him.Main").
configWithPlugins :: [Plugin] -> Set.Set Text -> Bindings -> Either Text Config
configWithPlugins every enabled user = do
  let on = [p | p <- every, plName p `Set.member` enabled]
      offActions = Set.fromList [actName a | p <- every, plName p `Set.notMember` enabled, a <- plActions p]
      usable (_, inv) = either (const True) ((`Set.notMember` offActions) . invAction) (parseInvocation inv)
  config <- buildConfig (actionsWith every on) (overrideBindings (Map.map (filter usable) user) (bindingsWith on)) fallback
  pure
    config
      { cfgExCommands = exCommandsWith every on
      , cfgPrefixNames = prefixNames <> Map.fromList (concatMap plPrefixNames on)
      , cfgSyntaxProviders = syntaxProviders
      , cfgChatProviders = chatProviders
      , cfgPlugins = on
      , cfgAllPlugins = every
      }

-- | Chat providers (ADR ai-chat); @[chat] provider@ names the one used.
chatProviders :: [ChatProvider]
chatProviders = [claudeCodeProvider, anthropicProvider]

-- | Highlighters, tried in order for each language (ADR syntax-providers). A TextMate
-- provider would be added here, and nowhere else.
syntaxProviders :: [SyntaxProvider]
syntaxProviders = [treeSitter]

-- | Titles of the key prefixes, shown above the keys that can follow them.
prefixNames :: Map.Map [Key] Text
prefixNames =
  Map.fromList
    [ ([plain (KChar 'g')], "goto")
    , ([plain (KChar ' ')], "space")
    , ([plain (KChar ']')], "next")
    , ([plain (KChar 'm')], "match")
    , ([ctrlW], "window")
    , ([ctrlW, plain (KChar 'n')], "new split")
    , ([plain (KChar ' '), plain (KChar 'w')], "window")
    , ([plain (KChar ' '), plain (KChar 'w'), plain (KChar 'n')], "new split")
    , ([plain (KChar '[')], "previous")
    ]

ctrlW :: Key
ctrlW = Key (KChar 'w') (Set.singleton Ctrl)

-- | Unbound printable characters are typed in insert and command mode.
fallback :: Mode -> Key -> Maybe (EditorM ())
fallback mode (Key (KChar c) mods)
  | Set.null (Set.delete Shift mods) = case mode of
      Insert -> Just (Edit.insertChar c)
      CmdLine -> Just (CommandLine.cmdlineInsert c)
      Picking -> Just (Picker.pickerInsert c)
      _ -> Nothing
fallback _ _ = Nothing
