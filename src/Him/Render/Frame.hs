-- | A frame is a grid of styled cells: what the screen should look like.
-- Components draw into it purely; "Him.Render.Diff" turns it into output.
module Him.Render.Frame
  ( Cell (..)
  , Frame (..)
  , Rect (..)
  , continuation
  , blankFrame
  , putCell
  , putCells
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
putCell row col cell = putCells row col [cell]

-- | Write a run of cells starting at @(row, col)@, clipped at the frame
-- edges. The row is updated with one splice, which is much cheaper than
-- updating cell by cell; components should prefer whole runs.
putCells :: Int -> Int -> [Cell] -> Frame -> Frame
putCells row col cells f
  | row < 0 || row >= frameRows f || n == 0 = f
  | otherwise = f {frameCells = Seq.adjust' splice row (frameCells f)}
  where
    start = max 0 col
    visible = take (frameCols f - start) (drop (start - col) cells)
    n = length visible
    splice r = Seq.take start r <> Seq.fromList visible <> Seq.drop (start + n) r

-- | Write text starting at @(row, col)@, clipped at the frame edge.
putText :: Int -> Int -> Style -> Text -> Frame -> Frame
putText row col style t = putCells row col (map (`Cell` style) (T.unpack t))

fillRect :: Rect -> Style -> Frame -> Frame
fillRect (Rect r c h w) style f =
  foldl' (\acc row -> putCells row c (replicate w (Cell ' ' style)) acc) f [r .. r + h - 1]
