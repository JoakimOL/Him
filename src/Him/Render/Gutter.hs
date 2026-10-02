-- | Line numbers to the left of the text area.
module Him.Render.Gutter
  ( drawGutter
  , gutterWidth
  ) where

import Data.Text qualified as T
import Him.Buffer (lineCount)
import Data.IntMap.Strict qualified as IntMap
import Him.GitState (Sign (..), SignKind (..), gitSigns, tracking)
import Him.Document (Document (..))
import Him.Editor (Editor (..))
import Him.Position (Pos (..))
import Him.Render.Frame
import Him.Render.Theme
import Him.Selection (primary, rangeHead)
import Him.View (View (..))

-- | Width of the gutter: a column for git signs, the digits of the largest
-- line number (at least three), and one column of padding.
gutterWidth :: Editor -> Int
gutterWidth ed = 1 + max 3 (length (show (lineCount (docBuffer (edDoc ed))))) + 1

drawGutter :: Theme -> Editor -> Rect -> Frame -> Frame
drawGutter theme ed rect frame0 = foldl' drawRow frame0 [0 .. rectHeight rect - 1]
  where
    doc = edDoc ed
    top = viewTop (edView ed)
    current = posLine (rangeHead (primary (docSelection doc)))
    digits = rectWidth rect - 2
    signs = maybe mempty (\t -> gitSigns t top (top + rectHeight rect - 1)) (tracking (docGit doc))
    drawRow f r
      | line >= lineCount (docBuffer doc) = f
      | otherwise =
          putText (rectRow rect + r) (rectCol rect + 1) style label $
            putText (rectRow rect + r) (rectCol rect) signStyle signText f
      where
        line = top + r
        style = if line == current then themeGutterCurrent theme else themeGutter theme
        label = T.justifyRight digits ' ' (T.pack (show (line + 1))) <> " "
        (signText, signStyle) = case IntMap.lookup line signs of
          Just (Sign kind staged) -> (glyph kind, themeGitSign theme kind staged)
          Nothing -> (" ", themeGutter theme)
        glyph = \case
          SignAdded -> "▎"
          SignChanged -> "▎"
          SignRemoved -> "▁"
