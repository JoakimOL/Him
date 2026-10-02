-- | The picker: a bordered box over the text area with the query, the
-- number of matches, and the matching items around the selected one.
module Him.Render.Picker
  ( drawPicker
  , pickerCursor
  ) where

import Data.IntMap.Strict qualified as IntMap
import Data.Text qualified as T
import Him.Editor
import Him.Picker
import Him.Render.Frame
import Him.Render.Theme
import Him.Terminal.Ansi (Style (..))

-- | Where the box goes inside the area: most of it, centred.
box :: Rect -> Rect
box (Rect r c h w) = Rect (r + top) (c + left) bh bw
  where
    bw = max 10 (min w (max 40 (w * 4 `div` 5)))
    bh = max 4 (min h (max 8 (h * 4 `div` 5)))
    left = (w - bw) `div` 2
    top = (h - bh) `div` 2

drawPicker :: Theme -> Editor -> Rect -> Frame -> Frame
drawPicker theme ed area f = case edPicker ed of
  Just p | rectHeight area >= 4 && rectWidth area >= 10 -> draw p
  _ -> f
  where
    draw p =
      let Rect top left h w = box area
          inner = w - 2
          listRows = h - 3
          ms = pkMatches p
          sel = pkSelected p
          -- Scroll the list so the selected item is visible.
          first = max 0 (sel - listRows + 1)
          visible = zip [first ..] (take listRows (drop first ms))
          count = T.pack (show (pkMatchCount p) <> "/" <> show (length (pkItems p))) <> if pkLoading p then "…" else ""
          titled = T.take inner (" " <> pkTitle p <> " ")
          border l r fill t = l <> t <> T.replicate (inner - T.length t) fill <> r
          fit t = T.take inner t <> T.replicate (inner - T.length t) " "
          queryLine = fit (T.take (inner - T.length count - 1) ("> " <> pkQuery p) `padTo` (inner - T.length count) <> count)
          padTo t n = t <> T.replicate (n - T.length t) " "
          -- Labels are padded to a common width so details line up.
          labelW = min (inner `div` 2) (maximum (0 : [T.length (piLabel it) | (_, it) <- visible]))
          rowStyle i = if i == sel then themePopupSelected theme else themePopup theme
          row i item = (rowStyle i, fit (" " <> piLabel item))
          detailCol = left + 2 + labelW + 2
          lines' =
            [(top, themePopup theme, border "┌" "┐" "─" titled), (top + 1, themePopup theme, "│" <> queryLine <> "│")]
              <> [(top + 2 + j, themePopup theme, "│" <> fit "" <> "│") | j <- [0 .. listRows - 1]]
              <> [(top + h - 1, themePopup theme, border "└" "┘" "─" "")]
          framed = foldl' (\fr (r, st, t) -> putText r left st t fr) f lines'
          withLabels = foldl' (\fr (j, (i, item)) -> let (st, t) = row i item in putText (top + 2 + j) (left + 1) st t fr) framed (zip [0 ..] visible)
          detailStyle i = (rowStyle i) {styleFg = styleFg (themePopupDetail theme)}
          withItems =
            foldl'
              (\fr (j, (i, item)) -> if T.null (piDetail item) then fr else putText (top + 2 + j) detailCol (detailStyle i) (T.take (left + w - 1 - detailCol) (piDetail item)) fr)
              withLabels
              (zip [0 ..] visible)
       in withItems {frameRowKeys = foldr IntMap.delete (frameRowKeys withItems) [top .. top + h - 1]}

-- | The terminal cursor sits at the end of the query.
pickerCursor :: Editor -> Rect -> Maybe (Int, Int)
pickerCursor ed area = do
  p <- edPicker ed
  let Rect top left _ w = box area
  pure (top + 1, left + 1 + min (w - 3) (2 + T.length (pkQuery p)))
