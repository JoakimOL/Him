-- | Turning frames into terminal output, redrawing only what changed.
module Him.Render.Diff
  ( diffFrames
  ) where

import Data.ByteString.Builder (Builder, charUtf8)
import Data.Foldable (toList)
import Data.Sequence (Seq)
import Data.Sequence qualified as Seq
import Him.Render.Frame
import Him.Terminal.Ansi

-- | Output that turns the previous frame into the new one. Without a
-- previous frame, or if the size changed, the whole screen is redrawn.
diffFrames :: Maybe Frame -> Frame -> Builder
diffFrames prev new = hideCursor <> body <> sgr defaultStyle <> cursor
  where
    sameSize p = frameRows p == frameRows new && frameCols p == frameCols new
    rows = zip [0 ..] (toList (frameCells new))
    body = case prev of
      Just p
        | sameSize p ->
            foldMap
              (\(i, row) -> if Seq.lookup i (frameCells p) == Just row then mempty else drawRow i row)
              rows
      _ -> clearScreen <> foldMap (uncurry drawRow) rows
    drawRow i row = moveCursor i 0 <> renderCells row
    cursor = case frameCursor new of
      Just (r, c) -> moveCursor r c <> cursorShape (frameCursorShape new) <> showCursor
      Nothing -> mempty

-- | A row of cells, emitting a style change only where the style changes.
renderCells :: Seq Cell -> Builder
renderCells = go Nothing . toList
  where
    go _ [] = mempty
    go current (Cell c _ : rest) | c == continuation = go current rest
    go current (Cell c style : rest) =
      (if current == Just style then mempty else sgr style) <> charUtf8 c <> go (Just style) rest
