-- | A plugin's canvas (ADR plugin-canvas): a bordered box in the middle of the
-- screen, filled with the texts and faces the plugin gave, over
-- everything else.
module Him.Render.Canvas
  ( drawCanvas
  ) where

import Data.Text qualified as T
import Him.Editor (Editor (..))
import Him.PluginUI (Canvas (..), OpenCanvas (..))
import Him.Render.Frame
import Him.Render.Theme
import Him.Terminal.Ansi (packStyle, patchStyle)

drawCanvas :: Theme -> Editor -> Rect -> Frame -> Frame
drawCanvas theme ed (Rect r c h w) f = case edCanvas ed of
  Just oc | h >= 3 && w >= 4 -> draw (ocCanvas oc)
  _ -> f
  where
    draw cv =
      let -- The inside shrinks to fit the area.
          iw = max 1 (min (w - 2) (canvasWidth cv))
          ih = max 1 (min (h - 2) (canvasHeight cv))
          top = r + (h - ih - 2) `div` 2
          left = c + (w - iw - 2) `div` 2
          base = themePopup theme
          blank = Cell ' ' (packStyle base)
          title = if T.null (canvasTitle cv) then "" else " " <> canvasTitle cv <> " "
          border = foldl' (\fr row -> putText row left base "│" (putText row (left + iw + 1) base "│" fr)) f [top + 1 .. top + ih]
          framed =
            putText top left base ("┌" <> T.take iw ("─" <> title <> T.replicate iw "─") <> "┐") $
              putText (top + ih + 1) left base ("└" <> T.replicate iw "─" <> "┘") border
          -- Each run's face over the box's colours; one cell per character.
          cells runs = take iw (concat [map (`Cell` packStyle (base `patchStyle` faceStyle theme fc)) (T.unpack t) | (t, fc) <- runs] <> repeat blank)
          rows = take ih (canvasRows cv <> repeat [])
          filled = foldl' (\fr (row, runs) -> putCells row (left + 1) (cells runs) fr) framed (zip [top + 1 ..] rows)
       in forgetRows top (ih + 2) filled
