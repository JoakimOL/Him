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
  deriving stock (Eq, Ord, Show, Enum, Bounded)

-- | Short label for the status line.
modeLabel :: Mode -> Text
modeLabel = \case
  Normal -> "NOR"
  Insert -> "INS"
  Select -> "SEL"
  CmdLine -> "CMD"
  Picking -> "PIK"
