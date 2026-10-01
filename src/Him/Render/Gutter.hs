-- | Line numbers to the left of the text area.
module Him.Render.Gutter
  ( drawGutter
  , gutterWidth
  ) where

import Data.Text qualified as T
import Him.Buffer (lineCount)
import Him.Document (Document (..))
import Him.Editor (Editor (..))
import Him.Position (Pos (..))
import Him.Render.Frame
import Him.Render.Theme
import Him.Selection (primary, rangeHead)
import Him.View (View (..))

-- | Width of the gutter: the digits of the largest line number (at least
-- three) plus one column of padding.
gutterWidth :: Editor -> Int
gutterWidth ed = max 3 (length (show (lineCount (docBuffer (edDoc ed))))) + 1

drawGutter :: Theme -> Editor -> Rect -> Frame -> Frame
drawGutter theme ed rect frame0 = foldl' drawRow frame0 [0 .. rectHeight rect - 1]
  where
    doc = edDoc ed
    top = viewTop (edView ed)
    current = posLine (rangeHead (primary (docSelection doc)))
    digits = rectWidth rect - 1
    drawRow f r
      | line >= lineCount (docBuffer doc) = f
      | otherwise = putText (rectRow rect + r) (rectCol rect) style label f
      where
        line = top + r
        style = if line == current then themeGutterCurrent theme else themeGutter theme
        label = T.justifyRight digits ' ' (T.pack (show (line + 1))) <> " "
