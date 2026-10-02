-- | The status line of a window: mode, file name, pending keys, cursor
-- position. An unfocused window's is dimmer and has no mode.
module Him.Render.StatusLine
  ( drawStatusLine
  ) where

import Data.Text qualified as T
import Him.Document (DocKind (..), Document (..), displayName, unsaved)
import Him.Editor (Editor (..), allDocuments, keymapMode, reviewFor)
import Him.Chat (ChatState (..), ChatStatus (..), Review (..))
import Data.List (findIndex)
import Him.Key (showKeys)
import Him.Mode (modeLabel)
import Him.Position (Pos (..))
import Him.Render.Frame
import Him.Render.Theme
import Him.Selection (primary, primaryIndex, rangeCount, rangeHead)

drawStatusLine :: Theme -> Bool -> Editor -> Rect -> Frame -> Frame
drawStatusLine theme focused ed rect =
  putText row (rectCol rect + rectWidth rect - T.length right) style right
    . putText row (rectCol rect + T.length mode) style file
    . putText row (rectCol rect) (if focused then themeMode theme (keymapMode ed) else style) mode
    . fillRect rect style
  where
    row = rectRow rect
    style = if focused then themeStatusLine theme else themeStatusLineInactive theme
    doc = edDoc ed
    mode = if focused then " " <> modeLabel (keymapMode ed) <> " " else " "
    dirty = if unsaved doc then " [+]" else ""
    -- The shown document's place in the buffer list (an unfocused window
    -- shows one that need not be the current buffer).
    docs = allDocuments ed
    bufs = case (findIndex ((== docId doc) . docId) docs, length docs) of
      (_, 1) -> ""
      (i, n) -> "[" <> maybe "?" (T.pack . show . (+ 1)) i <> "/" <> T.pack (show n) <> "] "
    -- Shorten the file name from the left so the dirty marker and the right
    -- section always fit.
    room = rectWidth rect - T.length mode - T.length right - T.length dirty - T.length bufs - 2
    name = displayName doc
    shortName
      | T.length name <= room = name
      | room <= 1 = ""
      | otherwise = "…" <> T.takeEnd (room - 1) name
    file = " " <> bufs <> shortName <> dirty
    sel = docSelection doc
    Pos l c = rangeHead (primary sel)
    sels
      | rangeCount sel > 1 = T.pack (show (primaryIndex sel + 1) <> "/" <> show (rangeCount sel) <> " sels  ")
      | otherwise = ""
    -- Proposed changes waiting in this buffer (ADR-43).
    review = case maybe 0 (length . rvHunks) (reviewFor ed (docId doc)) of
      0 -> working
      n -> T.pack (show n) <> " to review  "
    -- The chat while its model answers.
    working = case docKind doc of
      ChatDoc cs | csStatus cs == ChatWaiting -> "working…  "
      _ -> ""
    right = maybe "" (T.pack . show) (edCount ed) <> showKeys (edPending ed) <> "  " <> review <> sels <> T.pack (show (l + 1) <> ":" <> show (c + 1)) <> " "
