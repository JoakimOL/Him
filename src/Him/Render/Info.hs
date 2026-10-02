-- | The info box ('InfoBox'): a bordered popup in a bottom corner of the
-- text area.
module Him.Render.Info
  ( drawInfo
  ) where

import Data.IntMap.Strict qualified as IntMap
import Data.Text qualified as T
import Him.Editor
import Him.Render.Frame
import Him.Render.Theme

-- | Draw over the area. The rows it covers are dropped from the frame's
-- row keys, so the next frame redraws them instead of copying the popup.
drawInfo :: Theme -> Editor -> Rect -> Frame -> Frame
drawInfo theme ed area f = case edInfo ed of
  Just box | rectHeight area >= 3 && rectWidth area >= 8 -> draw box
  _ -> f
  where
    draw (InfoBox title rows place) =
      let maxRows = rectHeight area - 2
          shown
            | length rows > maxRows = take (maxRows - 1) rows <> [("…", T.pack (show (length rows - maxRows + 1)) <> " more")]
            | otherwise = rows
          keyW = maximum (0 : map (T.length . fst) shown)
          docW = maximum (0 : map (T.length . snd) shown)
          -- One column of padding on each side inside the border.
          inner = min (rectWidth area - 2) (2 + maximum [T.length title + 2, keyW + if docW > 0 then 2 + docW else 0])
          h = length shown + 2
          w = inner + 2
          top = rectRow area + rectHeight area - h
          left = case place of
            BottomRight -> rectCol area + rectWidth area - w
            BottomLeft -> rectCol area
          titled = T.take inner (" " <> title <> " ")
          border l r fill t = l <> t <> T.replicate (inner - T.length t) fill <> r
          line (k, d) = T.take inner (" " <> k <> T.replicate (keyW - T.length k) " " <> (if T.null d then "" else "  " <> d))
          lines' =
            [(top, border "┌" "┐" "─" titled, themePopup theme)]
              <> [(top + 1 + i, border "│" "│" " " (line r), themePopup theme) | (i, r) <- zip [0 ..] shown]
              <> [(top + h - 1, border "└" "┘" "─" "", themePopup theme)]
          drawn = foldl' (\fr (row, t, st) -> putText row left st t fr) f lines'
          keys = [(top + 1 + i, k) | (i, (k, _)) <- zip [0 ..] shown]
          withKeys = foldl' (\fr (row, k) -> putText row (left + 2) (themePopupKey theme) (T.take (inner - 1) k) fr) drawn keys
       in withKeys {frameRowKeys = foldr IntMap.delete (frameRowKeys withKeys) [top .. top + h - 1]}
