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
import Him.Selection (primary, primaryIndex, rangeCount, rangeHead)

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
    dirty = if docDirty doc then " [+]" else ""
    -- Shorten the file name from the left so the dirty marker and the right
    -- section always fit.
    room = rectWidth rect - T.length mode - T.length right - T.length dirty - 2
    name = displayName doc
    shortName
      | T.length name <= room = name
      | room <= 1 = ""
      | otherwise = "…" <> T.takeEnd (room - 1) name
    file = " " <> shortName <> dirty
    sel = docSelection doc
    Pos l c = rangeHead (primary sel)
    sels
      | rangeCount sel > 1 = T.pack (show (primaryIndex sel + 1) <> "/" <> show (rangeCount sel) <> " sels  ")
      | otherwise = ""
    right = maybe "" (T.pack . show) (edCount ed) <> showKeys (edPending ed) <> "  " <> sels <> T.pack (show (l + 1) <> ":" <> show (c + 1)) <> " "
