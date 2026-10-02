-- | The completion menu: below the cursor (above when there is no room),
-- labels with their details dimmed, the selected one highlighted.
module Him.Render.Completion
  ( drawCompletion
  ) where

import Data.IntMap.Strict qualified as IntMap
import Data.Text qualified as T
import Him.Editor
import Him.Lsp.Protocol (CompletionItem (..))
import Him.Lsp.State (Completion (..))
import Him.Render.Frame
import Him.Render.Theme
import Him.Terminal.Ansi (Style (..))

-- | At most this many rows are shown; the list scrolls with the selection.
menuRows :: Int
menuRows = 10

drawCompletion :: Theme -> Editor -> Rect -> Maybe (Int, Int) -> Frame -> Frame
drawCompletion theme ed area cursor f = case (edCompletion ed, cursor) of
  (Just c, Just (cr, cc)) | not (null (cmShown c)) -> draw c cr cc
  _ -> f
  where
    draw c cr cc =
      let items = cmShown c
          sel = cmSelected c
          rows = min menuRows (length items)
          first = max 0 (sel - rows + 1)
          visible = zip [first ..] (take rows (drop first items))
          labelW = min 40 (maximum (0 : [T.length (ciLabel i) | (_, i) <- visible]))
          detailW = min 30 (maximum (0 : [T.length (ciDetail i) | (_, i) <- visible]))
          w = min (rectWidth area) (labelW + (if detailW > 0 then detailW + 3 else 2))
          below = cr + 1
          top = if below + rows <= rectRow area + rectHeight area then below else max (rectRow area) (cr - rows)
          left = max (rectCol area) (min cc (rectCol area + rectWidth area - w))
          rowStyle i = if i == sel then themePopupSelected theme else themePopup theme
          line i item =
            let label = T.justifyLeft labelW ' ' (T.take labelW (ciLabel item))
                detail = T.take detailW (ciDetail item)
             in (rowStyle i, " " <> label <> (if T.null detail then " " else "  "), detail)
          drawn =
            foldl'
              ( \fr (j, (i, item)) ->
                  let (st, text, detail) = line i item
                      row = top + j
                      padded = T.justifyLeft w ' ' (text <> detail)
                   in putText row (left + T.length text) (st {styleFg = styleFg (themePopupDetail theme)}) detail $
                        putText row left st (T.take w padded) fr
              )
              f
              (zip [0 ..] visible)
       in drawn {frameRowKeys = foldr IntMap.delete (frameRowKeys drawn) [top .. top + rows - 1]}
