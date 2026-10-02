-- | Editor modes.
module Him.Mode
  ( Mode (..)
  , modeLabel
  ) where

import Data.Text (Text)

data Mode
  = Normal
  | Insert
  | -- | Like normal mode, but motions extend the selection (Helix @v@).
    Select
  | -- | Typing a @:@ command.
    CmdLine
  | -- | Choosing from a picker (@space f@).
    Picking
  | -- | Insert mode with the completion menu open: a keymap layer, like
    -- 'Directory' (see 'Him.Editor.keymapMode').
    Completing
  | -- | Normal mode in a directory listing. Never 'Him.Editor.edMode'
    -- itself: it names the keymap layer used there (see
    -- 'Him.Editor.keymapMode').
    Directory
  | -- | Insert mode in a REPL buffer: a keymap layer (@ret@ sends the input).
    Repl
  deriving stock (Eq, Ord, Show, Enum, Bounded)

-- | Short label for the status line.
modeLabel :: Mode -> Text
modeLabel = \case
  Normal -> "NOR"
  Insert -> "INS"
  Select -> "SEL"
  CmdLine -> "CMD"
  Picking -> "PIK"
  Directory -> "DIR"
  Completing -> "INS"
  Repl -> "INS"
