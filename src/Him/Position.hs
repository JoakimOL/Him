-- | Positions in a buffer.
module Him.Position
  ( Pos (..)
  ) where

-- | A 0-based line and a 0-based character index within that line.
-- @posCol == lineLength@ addresses the line's end (its newline, or the end of
-- the file on the last line). 'Ord' is document order.
data Pos = Pos
  { posLine :: !Int
  , posCol :: !Int
  }
  deriving stock (Eq, Ord, Show)
