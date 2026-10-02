-- | Line numbers to the left of the text area.
module Him.Render.Gutter
  ( drawGutter
  , gutterWidth
  ) where

import Data.Text qualified as T
import Him.Buffer (lineCount)
import Data.IntMap.Strict qualified as IntMap
import Him.GitState (Sign (..), SignKind (..), gitSigns, tracking)
import Him.Lsp.State (ShownDiagnostic (..), shownDiagnostics)
import Him.Document (Document (..))
import Him.Editor (Editor (..))
import Him.Options (LineNumbers (..), Options (..))
import Him.Position (Pos (..))
import Him.Render.Frame
import Him.Render.Theme
import Him.Selection (primary, rangeHead)
import Him.View (View (..))

-- | Width of the gutter: a column for git signs, the digits of the largest
-- line number (at least three; none when line numbers are off), and one
-- column of padding.
gutterWidth :: Editor -> Int
gutterWidth ed = case optLineNumbers (edOptions ed) of
  LineNumbersOff -> 2
  _ -> 1 + max 3 (length (show (lineCount (docBuffer (edDoc ed))))) + 1

drawGutter :: Theme -> Editor -> Rect -> Frame -> Frame
drawGutter theme ed rect frame0 = foldl' drawRow frame0 [0 .. rectHeight rect - 1]
  where
    doc = edDoc ed
    top = viewTop (edView ed)
    current = posLine (rangeHead (primary (docSelection doc)))
    digits = rectWidth rect - 2
    signs = maybe mempty (\t -> gitSigns t top (top + rectHeight rect - 1)) (tracking (docGit doc))
    -- Diagnostics win over git signs: the most severe on each line.
    diagnostics =
      IntMap.fromListWith min [(sdLine sd, sdSeverity sd) | sd <- shownDiagnostics (edLsp ed) (docLsp doc) (docBuffer doc), sdLine sd >= top, sdLine sd < top + rectHeight rect]
    drawRow f r
      | line >= lineCount (docBuffer doc) = f
      | otherwise =
          putText (rectRow rect + r) (rectCol rect + 1) style label $
            putText (rectRow rect + r) (rectCol rect) signStyle signText f
      where
        line = top + r
        style = if line == current then themeGutterCurrent theme else themeGutter theme
        -- Relative numbers count from the cursor's line, which shows its own.
        number = case optLineNumbers (edOptions ed) of
          LineNumbersRelative | line /= current -> abs (line - current)
          _ -> line + 1
        label = if digits <= 0 then "" else T.justifyRight digits ' ' (T.pack (show number)) <> " "
        (signText, signStyle) = case (IntMap.lookup line diagnostics, IntMap.lookup line signs) of
          (Just sev, _) -> ("●", themeDiagnostic theme sev)
          (_, Just (Sign kind staged)) -> (glyph kind, themeGitSign theme kind staged)
          _ -> (" ", themeGutter theme)
        glyph = \case
          SignAdded -> "▎"
          SignChanged -> "▎"
          SignRemoved -> "▁"
