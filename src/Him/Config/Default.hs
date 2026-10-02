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
import Him.Commands.Edit qualified as Edit
import Him.Commands.File qualified as File
import Him.Commands.Motion qualified as Motion
import Him.Commands.Search qualified as Search
import Him.Config (Bindings, Config (..), buildConfig, overrideBindings)
import Him.Ex (ExCommand)
import Him.Key (Key (..), KeyCode (..), Modifier (..), plain)
import Him.Mode (Mode (..))

allActions :: [Action]
allActions =
  Motion.actions
    <> Edit.actions
    <> Search.actions
    <> File.actions
    <> CommandLine.actions exCommands

exCommands :: [ExCommand]
exCommands = File.exCommands

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
       ]

commandBindings :: [(Text, Text)]
commandBindings =
  [ ("esc", "cmdline_cancel")
  , ("ret", "cmdline_execute")
  , ("tab", "cmdline_complete")
  , ("backspace", "cmdline_backspace")
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
    ]

-- | The default configuration. Every binding is checked against the
-- actions; an error lists each bad binding.
defaultConfig :: Either Text Config
defaultConfig = configWith Map.empty

-- | The defaults with some bindings replaced, e.g. from a config file.
configWith :: Bindings -> Either Text Config
configWith user = do
  config <- buildConfig allActions (overrideBindings user defaultBindings) fallback
  pure config {cfgExCommands = exCommands, cfgPrefixNames = prefixNames}

-- | Titles of the key prefixes, shown above the keys that can follow them.
prefixNames :: Map.Map [Key] Text
prefixNames =
  Map.fromList
    [ ([plain (KChar 'g')], "goto")
    , ([plain (KChar ' ')], "space")
    ]

-- | Unbound printable characters are typed in insert and command mode.
fallback :: Mode -> Key -> Maybe (EditorM ())
fallback mode (Key (KChar c) mods)
  | Set.null (Set.delete Shift mods) = case mode of
      Insert -> Just (Edit.insertChar c)
      CmdLine -> Just (CommandLine.cmdlineInsert c)
      _ -> Nothing
fallback _ _ = Nothing
