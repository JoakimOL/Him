-- | A frame is a grid of styled cells: what the screen should look like.
-- Components draw into it purely; "Him.Render.Diff" turns it into output.
module Him.Render.Frame
  ( Cell (..)
  , Frame (..)
  , Rect (..)
  , continuation
  , blankCell
  , RowKey (..)
  , ScrollInfo (..)
  , blankFrame
  , copyCells
  , putCell
  , putCells
  , putText
  , fillRect
  ) where

import Data.IntMap.Strict (IntMap)
import Data.IntMap.Strict qualified as IntMap
import Data.Sequence (Seq)
import Data.Sequence qualified as Seq
import Data.Text (Text)
import Data.Text qualified as T
import Him.Terminal.Ansi (CursorShape (..), PackedStyle, Style, packStyle, packedDefault)

data Cell = Cell
  { cellChar :: {-# UNPACK #-} !Char
  , cellStyle :: {-# UNPACK #-} !PackedStyle
  }
  deriving stock (Eq, Show)

-- | A space in the default style (what a cleared terminal shows).
blankCell :: Cell
blankCell = Cell ' ' packedDefault

-- | The marker in the cell to the right of a wide character, which the
-- terminal fills by itself; it is skipped when emitting output.
continuation :: Char
continuation = '\0'

-- | What a text-area row was drawn from (see "Him.Render.TextArea"). Equal
-- keys give equal cells, so the next frame can copy the row instead of
-- drawing it again.
data RowKey = RowKey
  { rkLine :: !Int
  , rkText :: !Text
  , rkSpans :: ![(Int, Int)]
  , rkCursors :: ![Int]
  , rkLeft :: !Int
  , rkCol :: !Int
  , rkWidth :: !Int
  , rkClass :: !Int
  -- ^ How the line is coloured as a whole (e.g. a directory in a listing).
  }
  deriving stock (Eq, Show)

-- | The rows that scroll together with the view (text area and gutter),
-- and the view's first line. Two frames with the same region but different
-- tops can be turned into each other by scrolling the terminal.
data ScrollInfo = ScrollInfo
  { siRow :: !Int
  , siHeight :: !Int
  , siTop :: !Int
  }
  deriving stock (Eq, Show)

data Frame = Frame
  { frameRows :: !Int
  , frameCols :: !Int
  , frameCells :: !(Seq (Seq Cell))
  , frameCursor :: !(Maybe (Int, Int))
  -- ^ Where the terminal cursor goes, as @(row, col)@; hidden if 'Nothing'.
  , frameCursorShape :: !CursorShape
  , frameRowKeys :: !(IntMap RowKey)
  -- ^ Keys of the text-area rows, by screen row.
  , frameScroll :: !(Maybe ScrollInfo)
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
  Frame rows cols (Seq.replicate rows (Seq.replicate cols blankCell)) Nothing CursorBlock IntMap.empty Nothing

-- | Copy @width@ cells at column @col@ from row @srcRow@ of another frame
-- (of the same size) to row @row@: one slice and one splice.
copyCells :: Int -> Int -> Int -> Int -> Frame -> Frame -> Frame
copyCells srcRow row col width from f = case Seq.lookup srcRow (frameCells from) of
  Nothing -> f
  Just src ->
    let piece = Seq.take width (Seq.drop col src)
     in f {frameCells = Seq.adjust' (\r -> Seq.take col r <> piece <> Seq.drop (col + Seq.length piece) r) row (frameCells f)}

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
putText row col style t = putCells row col (map (`Cell` packStyle style) (T.unpack t))

fillRect :: Rect -> Style -> Frame -> Frame
fillRect (Rect r c h w) style f =
  foldl' (\acc row -> putCells row c (replicate w (Cell ' ' (packStyle style))) acc) f [r .. r + h - 1]
