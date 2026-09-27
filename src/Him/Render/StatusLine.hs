-- | The status line: mode, file name, pending keys, cursor position.
module Him.Render.StatusLine
  ( drawStatusLine
  ) where

import Data.Text qualified as T
import Him.Document (Document (..), displayName)
import Him.Editor (Editor (..))
import Him.Key (showKeys)
import Him.Mode (modeLabel)
import Him.Position (Pos (..))
import Him.Render.Frame
import Him.Render.Theme
import Him.Selection (primary, rangeHead)

drawStatusLine :: Theme -> Editor -> Rect -> Frame -> Frame
drawStatusLine theme ed rect =
  putText row (rectCol rect + rectWidth rect - T.length right) style right
    . putText row (rectCol rect + T.length mode) style file
    . putText row (rectCol rect) (themeMode theme (edMode ed)) mode
    . fillRect rect style
  where
    row = rectRow rect
    style = themeStatusLine theme
    doc = edDoc ed
    mode = " " <> modeLabel (edMode ed) <> " "
    file = " " <> displayName doc <> (if docDirty doc then " [+]" else "")
    Pos l c = rangeHead (primary (docSelection doc))
    right = showKeys (edPending ed) <> "  " <> T.pack (show (l + 1) <> ":" <> show (c + 1)) <> " "
