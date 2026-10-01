-- | A frame is a grid of styled cells: what the screen should look like.
-- Components draw into it purely; "Him.Render.Diff" turns it into output.
module Him.Render.Frame
  ( Cell (..)
  , Frame (..)
  , Rect (..)
  , continuation
  , blankFrame
  , putCell
  , putText
  , fillRect
  ) where

import Data.Sequence (Seq)
import Data.Sequence qualified as Seq
import Data.Text (Text)
import Data.Text qualified as T
import Him.Terminal.Ansi (CursorShape (..), Style, defaultStyle)

data Cell = Cell
  { cellChar :: !Char
  , cellStyle :: !Style
  }
  deriving stock (Eq, Show)

-- | The marker in the cell to the right of a wide character, which the
-- terminal fills by itself; it is skipped when emitting output.
continuation :: Char
continuation = '\0'

data Frame = Frame
  { frameRows :: !Int
  , frameCols :: !Int
  , frameCells :: !(Seq (Seq Cell))
  , frameCursor :: !(Maybe (Int, Int))
  -- ^ Where the terminal cursor goes, as @(row, col)@; hidden if 'Nothing'.
  , frameCursorShape :: !CursorShape
  }
  deriving stock (Eq, Show)

-- | A screen region: top-left corner, height and width.
data Rect = Rect
  { rectRow :: !Int
  , rectCol :: !Int
  , rectHeight :: !Int
  , rectWidth :: !Int
  }
  deriving stock (Eq, Show)

blankFrame :: Int -> Int -> Frame
blankFrame rows cols =
  Frame rows cols (Seq.replicate rows (Seq.replicate cols (Cell ' ' defaultStyle))) Nothing CursorBlock

-- | Set one cell; out-of-bounds writes are ignored.
putCell :: Int -> Int -> Cell -> Frame -> Frame
putCell row col cell f
  | row < 0 || col < 0 || row >= frameRows f || col >= frameCols f = f
  | otherwise = f {frameCells = Seq.adjust' (Seq.update col cell) row (frameCells f)}

-- | Write text starting at @(row, col)@, clipped at the frame edge.
putText :: Int -> Int -> Style -> Text -> Frame -> Frame
putText row col style t f =
  foldl' (\acc (i, c) -> putCell row (col + i) (Cell c style) acc) f (zip [0 ..] (T.unpack t))

fillRect :: Rect -> Style -> Frame -> Frame
fillRect (Rect r c h w) style f =
  foldl' (\acc (row, col) -> putCell row col (Cell ' ' style) acc) f [(row, col) | row <- [r .. r + h - 1], col <- [c .. c + w - 1]]
