-- | The default actions and keybindings.
--
-- To add a keybinding, add a @(keys, action)@ pair to the right mode below;
-- the action may take arguments (@"move_line_down 5"@). To add an action,
-- add it to one of the @Him.Commands.*@ modules (or a new one, listed in
-- 'allActions').
module Him.Config.Default
  ( defaultConfig
  , defaultBindings
  , configWith
  , allActions
  , fallback
  ) where

import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Data.Text (Text)
import Him.Action (Action)
import Him.Command (EditorM)
import Him.Commands.CommandLine qualified as CommandLine
import Him.Commands.Directory qualified as Directory
import Him.Commands.Edit qualified as Edit
import Him.Commands.Git qualified as Git
import Him.Commands.Lsp qualified as Lsp
import Him.Commands.File qualified as File
import Him.Commands.Motion qualified as Motion
import Him.Commands.Picker qualified as Picker
import Him.Commands.Search qualified as Search
import Him.Config (Bindings, Config (..), buildConfig, overrideBindings)
import Him.Ex (ExCommand)
import Him.Key (Key (..), KeyCode (..), Modifier (..), plain)
import Him.Mode (Mode (..))
import Him.Syntax (SyntaxProvider)
import Him.Syntax.TreeSitter (treeSitter)

allActions :: [Action]
allActions =
  Motion.actions
    <> Edit.actions
    <> Search.actions
    <> File.actions
    <> Picker.actions
    <> Directory.actions
    <> Git.actions
    <> Lsp.actions
    <> CommandLine.actions exCommands

exCommands :: [ExCommand]
exCommands = File.exCommands <> Lsp.exCommands

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
       , ("u", "undo")
       , ("U", "redo")
       , ("g g", "goto_file_start")
       , ("g e", "goto_last_line")
       , ("g h", "goto_line_start")
       , ("g l", "goto_line_end")
       , ("space f", "file_picker")
       , ("space b", "buffer_picker")
       , ("space ?", "command_palette")
       , ("space d", "directory_of_buffer")
       , ("space D", "directory_of_cwd")
       , ("space g s", "git_stage_selection")
       , ("space g u", "git_unstage_selection")
       , ("space g S", "git_stage_file")
       , ("space g U", "git_unstage_file")
       , ("space g r", "git_reset_selection")
       , ("space k", "lsp_hover")
       , ("space x", "diagnostics_picker")
       , ("g d", "goto_definition")
       , ("g R", "goto_references")
       , ("g y", "goto_type_definition")
       , ("g i", "goto_implementation")
       , ("space r", "rename_symbol")
       , ("space a", "code_action")
       , ("space s", "document_symbols")
       , ("] d", "goto_next_diagnostic")
       , ("[ d", "goto_prev_diagnostic")
       , ("] g", "goto_next_change")
       , ("[ g", "goto_prev_change")
       , ("g n", "buffer_next")
       , ("g p", "buffer_previous")
       , ("i", "insert_mode")
       , ("a", "append_mode")
       , ("o", "open_below")
       , (":", "command_mode")
       , ("/", "search_forward")
       , ("?", "search_backward")
       , ("n", "search_next")
       , ("N", "search_prev")
       , ("*", "search_selection")
       ]

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
       , ("C-x", "completion")
       ]

commandBindings :: [(Text, Text)]
commandBindings =
  [ ("esc", "cmdline_cancel")
  , ("ret", "cmdline_execute")
  , ("tab", "cmdline_complete")
  , ("backspace", "cmdline_backspace")
  ]

-- | Insert mode with the completion menu open is insert mode with these.
completionBindings :: [(Text, Text)]
completionBindings =
  [ ("tab", "completion_next")
  , ("C-n", "completion_next")
  , ("down", "completion_next")
  , ("S-tab", "completion_previous")
  , ("C-p", "completion_previous")
  , ("up", "completion_previous")
  , ("ret", "completion_accept")
  , ("esc", "completion_cancel")
  ]

-- | Normal mode in a directory listing is normal mode with these.
directoryBindings :: [(Text, Text)]
directoryBindings =
  [ ("ret", "directory_open")
  , ("-", "directory_parent")
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
  , ("tab", "picker_next")
  , ("up", "picker_previous")
  , ("C-p", "picker_previous")
  , ("S-tab", "picker_previous")
  , ("backspace", "picker_backspace")
  ]

-- | The default bindings. Select mode also gets normal mode's bindings
-- (see 'Him.Config.inheritsFrom').
defaultBindings :: Bindings
defaultBindings =
  Map.fromList
    [ (Normal, normalBindings)
    , (Select, selectBindings)
    , (Insert, insertBindings)
    , (CmdLine, commandBindings)
    , (Picking, pickerBindings)
    , (Directory, directoryBindings)
    , (Completing, completionBindings)
    ]

-- | The default configuration. Every binding is checked against the
-- actions; an error lists each bad binding.
defaultConfig :: Either Text Config
defaultConfig = configWith Map.empty

-- | The defaults with some bindings replaced, e.g. from a config file.
configWith :: Bindings -> Either Text Config
configWith user = do
  config <- buildConfig allActions (overrideBindings user defaultBindings) fallback
  pure config {cfgExCommands = exCommands, cfgPrefixNames = prefixNames, cfgSyntaxProviders = syntaxProviders}

-- | Highlighters, tried in order for each language (ADR-26). A TextMate
-- provider would be added here, and nowhere else.
syntaxProviders :: [SyntaxProvider]
syntaxProviders = [treeSitter]

-- | Titles of the key prefixes, shown above the keys that can follow them.
prefixNames :: Map.Map [Key] Text
prefixNames =
  Map.fromList
    [ ([plain (KChar 'g')], "goto")
    , ([plain (KChar ' ')], "space")
    , ([plain (KChar ' '), plain (KChar 'g')], "git")
    , ([plain (KChar ']')], "next")
    , ([plain (KChar '[')], "previous")
    ]

-- | Unbound printable characters are typed in insert and command mode.
fallback :: Mode -> Key -> Maybe (EditorM ())
fallback mode (Key (KChar c) mods)
  | Set.null (Set.delete Shift mods) = case mode of
      Insert -> Just (Edit.insertChar c)
      CmdLine -> Just (CommandLine.cmdlineInsert c)
      Picking -> Just (Picker.pickerInsert c)
      _ -> Nothing
fallback _ _ = Nothing
