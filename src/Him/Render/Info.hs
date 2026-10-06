-- | The info box ('InfoBox'): a bordered popup in a bottom corner of the
-- text area.
module Him.Render.Info
  ( drawInfo
  ) where

import Control.Applicative ((<|>))
import Data.Text qualified as T
import Him.Editor
import Him.Render.Frame
import Him.Render.Theme

-- | Draw the info box, or else the popup, over the area. The rows it
-- covers are dropped from the frame's row keys, so the next frame redraws
-- them instead of copying the popup. A box 'AtCursor' goes below the
-- cursor (screen position given), or above it when there is no room.
drawInfo :: Theme -> Editor -> Rect -> Maybe (Int, Int) -> Frame -> Frame
drawInfo theme ed area cursor f = case edInfo ed <|> edPopup ed of
  Just box | rectHeight area >= 3 && rectWidth area >= 8 -> draw box
  _ -> f
  where
    draw (InfoBox title rows place selected) =
      let maxRows = rectHeight area - 2
          -- When the rows do not fit, scroll so the selected one shows.
          (skip, shown)
            | length rows > maxRows =
                let fit = maxRows - 1
                    from = maybe 0 (\i -> max 0 (min (length rows - fit) (i - fit + 1))) selected
                    rest = length rows - from - fit
                 in (from, take fit (drop from rows) <> [("…", T.pack (show rest) <> " more") | rest > 0])
            | otherwise = (0, rows)
          isSelected i = fmap (subtract skip) selected == Just i
          keyW = maximum (0 : map (T.length . fst) shown)
          docW = maximum (0 : map (T.length . snd) shown)
          -- One column of padding on each side inside the border.
          inner = min (rectWidth area - 2) (2 + maximum [T.length title + 2, keyW + if docW > 0 then 2 + docW else 0])
          h = length shown + 2
          w = inner + 2
          (top, left) = case (place, cursor) of
            (BottomRight, _) -> (rectRow area + rectHeight area - h, rectCol area + rectWidth area - w)
            (AboveCursor, Just (cr, cc))
              | cr - h >= rectRow area -> (cr - h, max (rectCol area) (min cc (rectCol area + rectWidth area - w)))
            (_, Just (cr, cc)) | place /= BottomLeft ->
              let below = cr + 1
                  above = cr - h
                  row = if below + h <= rectRow area + rectHeight area || above < rectRow area then below else above
               in (row, max (rectCol area) (min cc (rectCol area + rectWidth area - w)))
            _ -> (rectRow area + rectHeight area - h, rectCol area)
          titled = T.take inner (" " <> title <> " ")
          border l r fill t = l <> t <> T.replicate (inner - T.length t) fill <> r
          line (k, d) = T.take inner (" " <> k <> T.replicate (keyW - T.length k) " " <> (if T.null d then "" else "  " <> d))
          lines' =
            [(top, border "┌" "┐" "─" titled, themePopup theme)]
              <> [(top + 1 + i, border "│" "│" " " (line r), themePopup theme) | (i, r) <- zip [0 ..] shown]
              <> [(top + h - 1, border "└" "┘" "─" "", themePopup theme)]
          selectedRows = [(top + 1 + i, border "" "" " " (line r), themePopupSelected theme) | (i, r) <- zip [0 ..] shown, isSelected i]
          drawn = foldl' (\fr (row, t, st) -> putText row left st t fr) f lines'
          highlighted = foldl' (\fr (row, t, st) -> putText row (left + 1) st t fr) drawn selectedRows
          keys = [(top + 1 + i, k, if isSelected i then themePopupSelected theme else themePopupKey theme) | (i, (k, _)) <- zip [0 ..] shown]
          withKeys = foldl' (\fr (row, k, st) -> putText row (left + 2) st (T.take (inner - 1) k) fr) highlighted keys
       in forgetRows top h withKeys
