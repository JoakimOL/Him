-- | The main text area: buffer lines, selections, and the cursor position.
module Him.Render.TextArea
  ( drawTextArea
  , cursorPosition
  , cursorDisplayCol
  ) where

import Data.Text qualified as T
import Him.Buffer (lineAt, lineCount)
import Him.Document (Document (..))
import Him.Editor (Editor (..))
import Him.Mode (Mode (..))
import Him.Position (Pos (..))
import Him.Render.Frame
import Him.Render.Theme
import Him.Selection
import Him.TextWidth (displayCol, layoutLine)
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
    shown = [r | r <- ranges sel, edMode ed /= Insert || not (isCollapsed r)]
    styleAt p
      | any (\r -> r /= prim && rangeHead r == p) (ranges sel) = Just (themeCursor theme)
      | any (contains p) shown = Just (themeSelection theme)
      | otherwise = Nothing

    drawRow f r
      | line >= lineCount buf = putText screenRow (rectCol rect) (themeTilde theme) "~" f
      | otherwise = foldl' drawChar f (layoutLine text <> [(T.length text, displayCol text (T.length text), 1, ' ')])
      where
        line = top + r
        screenRow = rectRow rect + r
        text = lineAt line buf
        drawChar acc (i, col, w, c) =
          let style = styleAt (Pos line i)
              base = maybe (themeText theme) id style
              shownChar = if c == '\t' then ' ' else c
              cells = (col, shownChar) : [(col + k, ' ') | k <- [1 .. w - 1]]
              isLineEnd = i == T.length text
           in if isLineEnd && style == Nothing
                then acc
                else foldl' (\a (cc, ch) -> putVisible cc (Cell ch base) a) acc cells
          where
            putVisible cc cell a
              | cc < left || cc - left >= rectWidth rect = a
              | otherwise = putCell screenRow (rectCol rect + cc - left) cell a

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
