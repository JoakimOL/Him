-- | The default commands and keybindings.
--
-- To add a keybinding, add a @(keys, command name)@ pair to the right mode
-- below. To add a command, add it to one of the @Him.Commands.*@ modules
-- (or a new one, listed in 'allCommands').
module Him.Config.Default
  ( defaultConfig
  , allCommands
  ) where

import Control.Monad (unless)
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as T
import Him.Command (Command, EditorM, mkRegistry)
import Him.Commands.CommandLine qualified as CommandLine
import Him.Commands.Edit qualified as Edit
import Him.Commands.File qualified as File
import Him.Commands.Motion qualified as Motion
import Him.Config (Config (..))
import Him.Ex (ExCommand)
import Him.Key (Key (..), KeyCode (..), Modifier (..))
import Him.Keymap
import Him.Mode (Mode (..))

allCommands :: [Command]
allCommands =
  Motion.commands
    <> Edit.commands
    <> CommandLine.commands exCommands

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
       , ("i", "insert_mode")
       , ("a", "append_mode")
       , ("o", "open_below")
       , (":", "command_mode")
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
  , ("backspace", "cmdline_backspace")
  ]

-- | Build and validate the configuration: every bound name must be a
-- registered command.
defaultConfig :: Either Text Config
defaultConfig = do
  normal <- fromBindings normalBindings
  select <- fromBindings selectBindings
  insert <- fromBindings insertBindings
  command <- fromBindings commandBindings
  let keymaps =
        Map.fromList
          [ (Normal, normal)
          , (Select, unionKeymap select normal)
          , (Insert, insert)
          , (CmdLine, command)
          ]
      registry = mkRegistry allCommands
      missing = [n | km <- Map.elems keymaps, n <- boundCommands km, Map.notMember n registry]
  unless (null missing) $
    Left ("keymap refers to unknown commands: " <> T.intercalate ", " missing)
  pure Config {cfgRegistry = registry, cfgKeymaps = keymaps, cfgFallback = fallback}

-- | Unbound printable characters are typed in insert and command mode.
fallback :: Mode -> Key -> Maybe (EditorM ())
fallback mode (Key (KChar c) mods)
  | Set.null (Set.delete Shift mods) = case mode of
      Insert -> Just (Edit.insertChar c)
      CmdLine -> Just (CommandLine.cmdlineInsert c)
      _ -> Nothing
fallback _ _ = Nothing
