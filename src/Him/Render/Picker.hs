-- | The picker: a bordered box over the text area with the query, the
-- number of matches, and the matching items around the selected one.
module Him.Render.Picker
  ( drawPicker
  , pickerCursor
  ) where

import Data.Text qualified as T
import Him.Buffer qualified as Buffer
import Him.Editor
import Him.Picker
import Him.Options (Options (..))
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
          opts = edOptions ed
          preview = if optPreview opts && w >= optPreviewMinWidth opts then selectedItem p >>= previewFor ed . piTarget else Nothing
          -- With a preview, the list takes the left part of the box.
          inner = case preview of
            Just _ -> (w - 3) * 2 `div` 5
            Nothing -> w - 2
          listRows = h - 3
          ms = pkMatches p
          sel = pkSelected p
          -- Scroll the list so the selected item is visible.
          first = max 0 (sel - listRows + 1)
          visible = zip [first ..] (take listRows (drop first ms))
          -- A search shows the matching lines it found (it keeps only the
          -- first ones); other pickers, matches out of all items.
          count = T.pack (if pkSource p == GrepQuery then show (pkMatchCount p) else show (pkMatchCount p) <> "/" <> show (length (pkItems p))) <> (if pkLoading p || pkStale p then "…" else "") <> (if markCount p > 0 then " · " <> T.pack (show (markCount p)) <> " marked" else "")
          titled = T.take inner (" " <> pkTitle p <> " ")
          fit t = T.take inner t <> T.replicate (inner - T.length t) " "
          queryLine = fit (T.take (inner - T.length count - 1) ("> " <> pkQuery p) `padTo` (inner - T.length count) <> count)
          padTo t n = t <> T.replicate (n - T.length t) " "
          -- Labels are padded to a common width so details line up.
          labelW = min (inner `div` 2) (pkLabelWidth p)
          rowStyle i = if i == sel then themePopupSelected theme else themePopup theme
          -- A marked item (ADR-48) has a dot before it.
          row i item = (rowStyle i, fit ((if isMarked p item then "●" else " ") <> clip item))
          -- A label too long for its column, when a detail follows, is cut
          -- so the detail does not cover it: a search hit's path from the
          -- left (the file name and line matter most), others from the right.
          clip item
            | T.null (piDetail item) || piLength item <= labelW + 1 = piLabel item
            | pkSource p == GrepQuery = "…" <> T.takeEnd labelW (piLabel item)
            | otherwise = T.take labelW (piLabel item) <> "…"
          detailCol = left + 2 + labelW + 2
          full = w - 2
          wide t = T.take full t <> T.replicate (full - T.length t) " "
          lines' =
            [(top, themePopup theme, "┌" <> T.take full (titled <> T.replicate full "─") <> "┐"), (top + 1, themePopup theme, "│" <> wide queryLine <> "│")]
              <> [(top + 2 + j, themePopup theme, "│" <> wide "" <> "│") | j <- [0 .. listRows - 1]]
              <> [(top + h - 1, themePopup theme, "└" <> T.replicate full "─" <> "┘")]
          framed = foldl' (\fr (r, st, t) -> putText r left st t fr) f lines'
          withLabels = foldl' (\fr (j, (i, item)) -> let (st, t) = row i item in putText (top + 2 + j) (left + 1) st t fr) framed (zip [0 ..] visible)
          detailStyle i = (rowStyle i) {styleFg = styleFg (themePopupDetail theme)}
          withItems =
            foldl'
              (\fr (j, (i, item)) -> if T.null (piDetail item) then fr else putText (top + 2 + j) detailCol (detailStyle i) (T.take (left + 1 + inner - detailCol) (piDetail item)) fr)
              withLabels
              (zip [0 ..] visible)
          withPreview = maybe withItems (drawPreview top left h w inner withItems) preview
       in forgetRows top h withPreview

    -- The preview: right of a divider, the file around the target line
    -- (highlighted), with line numbers; or why there is nothing to show.
    drawPreview top left h w listInner fr (title, content) =
      let divider = left + 1 + listInner
          col = divider + 1
          pw = left + w - 1 - col
          rows = h - 2
          dividers = foldl' (\acc r -> putText r divider (themePopup theme) "│" acc) fr [top + 1 .. top + h - 2]
          titled = putText top divider (themePopup theme) ("┬" <> T.take (pw - 1) (" " <> title <> " ")) $
            putText (top + h - 1) divider (themePopup theme) "┴" dividers
       in case content of
            Left why -> putText (top + 1) (col + 1) (themePopup theme) {styleFg = styleFg (themePopupDetail theme)} (T.take (pw - 1) why) titled
            Right (buf, line) ->
              let firstLine = max 0 (line - rows `div` 3)
                  numberW = length (show (firstLine + rows))
                  shown =
                    [ (r, l, Buffer.lineAt l buf)
                    | (r, l) <- zip [top + 1 ..] [firstLine .. min (Buffer.lineCount buf - 1) (firstLine + rows - 1)]
                    ]
                  rowText l text =
                    let number = T.justifyRight numberW ' ' (T.pack (show (l + 1)))
                        body = T.replace "\t" (T.replicate (optTabWidth (edOptions ed)) " ") text
                     in T.take pw (" " <> number <> " " <> body) <> T.replicate (pw - 2 - numberW - T.length body) " "
                  styleOf l = if l == line then themePopupSelected theme else themePopup theme
               in foldl' (\acc (r, l, text) -> putText r col (styleOf l) (T.take pw (rowText l text)) acc) titled shown

-- | The terminal cursor sits at the end of the query.
pickerCursor :: Editor -> Rect -> Maybe (Int, Int)
pickerCursor ed area = do
  p <- edPicker ed
  let Rect top left _ w = box area
  pure (top + 1, left + 1 + min (w - 3) (2 + T.length (pkQuery p)))
