-- | The main text area: buffer lines, selections, and the cursor position.
module Him.Render.TextArea
  ( drawTextArea
  , cursorPosition
  , cursorDisplayCol
  ) where

import Data.Maybe (fromMaybe)
import Data.Text qualified as T
import Him.Buffer (lineAt, lineCount)
import Him.Document (Document (..))
import Him.Editor (Editor (..))
import Him.Mode (Mode (..))
import Him.Position (Pos (..))
import Him.Render.Frame
import Him.Render.Theme
import Him.Selection
import Him.TextWidth (displayCol, glyphs, isWide, layoutLine)
import Him.View (View (..))

drawTextArea :: Theme -> Editor -> Rect -> Frame -> Frame
drawTextArea theme ed rect frame0 = foldl' drawRow frame0 [0 .. rectHeight rect - 1]
  where
    doc = edDoc ed
    buf = docBuffer doc
    View top left = edView ed
    sel = docSelection doc
    prim = primary sel
    -- In insert mode the cursor is a bar between characters, so a collapsed
    -- range is not shown as a one-character selection.
    shown = [(rangeStart r, rangeEnd r) | r <- ranges sel, edMode ed /= Insert || not (isCollapsed r)]
    secondaryHeads = [rangeHead r | r <- ranges sel, r /= prim]

    drawRow f r
      | line >= lineCount buf = putText screenRow (rectCol rect) (themeTilde theme) "~" f
      | otherwise = putCells screenRow (rectCol rect) visible f
      where
        line = top + r
        screenRow = rectRow rect + r
        text = lineAt line buf
        len = T.length text
        -- Selected column intervals and secondary cursors on this line,
        -- computed once per row rather than per character.
        spans = [(colOn s0 0, colOn e0 len) | (s0, e0) <- shown, posLine s0 <= line, line <= posLine e0]
        colOn (Pos l c) dflt = if l == line then c else dflt
        cursors = [c | Pos l c <- secondaryHeads, l == line]
        styleAt i
          | i `elem` cursors = Just (themeCursor theme)
          | any (\(a, b) -> a <= i && i <= b) spans = Just (themeSelection theme)
          | otherwise = Nothing
        -- The cells of the whole line from display column 0, then the part
        -- inside the horizontal scroll window.
        lineCells = concatMap charCells (layoutLine text) <> lineEndCell
        visible = fixEdges (take (rectWidth rect) (drop left lineCells))
        charCells (i, _, w, c) =
          let style = fromMaybe (themeText theme) (styleAt i)
           in if isWide c
                then [Cell c style, Cell continuation style]
                else map (`Cell` style) (glyphs c w)
        -- The line end only shows when it is selected (or a cursor).
        lineEndCell = maybe [] (\st -> [Cell ' ' st]) (styleAt len)
        -- A wide character cut by the left or right edge becomes a blank, so
        -- the terminal never draws half of it.
        fixEdges cs = case cs of
          (Cell c st : rest) | c == continuation -> fixRight (Cell ' ' st : rest)
          _ -> fixRight cs
        fixRight cs = case reverse cs of
          (Cell c st : rest) | isWide c -> reverse (Cell ' ' st : rest)
          _ -> cs

-- | Display column of the primary cursor.
cursorDisplayCol :: Editor -> Int
cursorDisplayCol ed = displayCol (lineAt l buf) c
  where
    buf = docBuffer (edDoc ed)
    Pos l c = rangeHead (primary (docSelection (edDoc ed)))

-- | Screen position of the primary cursor, if it is inside the area.
cursorPosition :: Editor -> Rect -> Maybe (Int, Int)
cursorPosition ed rect
  | row >= 0 && row < rectHeight rect && col >= 0 && col < rectWidth rect =
      Just (rectRow rect + row, rectCol rect + col)
  | otherwise = Nothing
  where
    View top left = edView ed
    row = posLine (rangeHead (primary (docSelection (edDoc ed)))) - top
    col = cursorDisplayCol ed - left
