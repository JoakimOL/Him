-- | Turning frames into terminal output, redrawing only what changed.
--
-- Rows that changed are compared cell by cell, and only the runs of changed
-- cells are written (nearby runs are merged: a cursor move costs about as
-- much as a few cells). Blank cells at the end of a row are cleared with
-- one "erase to end of line" instead of being written out.
module Him.Render.Diff
  ( diffFrames
  ) where

import Data.ByteString.Builder (Builder, charUtf8)
import Data.Foldable (toList)

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
              ( \(i, row) -> case Seq.lookup i (frameCells p) of
                  Just old | old == row -> mempty
                  Just old -> drawChanges i (toList old) (toList row)
                  Nothing -> drawRow i (toList row)
              )
              rows
      _ -> clearScreen <> foldMap (\(i, row) -> drawRow i (toList row)) rows
    cursor = case frameCursor new of
      Just (r, c) -> moveCursor r c <> cursorShape (frameCursorShape new) <> showCursor
      Nothing -> mempty

-- | A whole row (after a clear screen, so trailing blanks can be skipped).
drawRow :: Int -> [Cell] -> Builder
drawRow i cells = case blankTail cells of
  0 -> mempty
  n -> moveCursor i 0 <> renderCells (take n cells)

-- | Only the parts of a row that differ from the old one.
drawChanges :: Int -> [Cell] -> [Cell] -> Builder
drawChanges i old new = foldMap drawRun (runs changed) <> clearTail
  where
    width = length new
    contentEnd = blankTail new
    changed = [c | (c, a, b) <- zip3 [0 ..] old new, a /= b, c < contentEnd]
    -- Changed cells past the content need clearing (if the old row had
    -- something there).
    tailChanged = or [a /= b | (a, b) <- drop contentEnd (zip old new)]
    clearTail
      | tailChanged = moveCursor i contentEnd <> sgr defaultStyle <> clearToEndOfLine
      | otherwise = mempty
    drawRun (from, to) =
      let from' = if isContinuation (cellAt from) then from - 1 else from
          to' = if to + 1 < width && isContinuation (cellAt (to + 1)) then to + 1 else to
       in moveCursor i from' <> renderCells (take (to' - from' + 1) (drop from' new))
    cellAt c = case drop c new of
      (x : _) -> x
      [] -> Cell ' ' defaultStyle
    isContinuation (Cell ch _) = ch == continuation

-- | Group sorted columns into @(first, last)@ runs, merging runs separated by
-- fewer than 'mergeGap' unchanged cells.
runs :: [Int] -> [(Int, Int)]
runs [] = []
runs (c : cs) = go c c cs
  where
    go s e [] = [(s, e)]
    go s e (x : xs)
      | x - e <= mergeGap = go s x xs
      | otherwise = (s, e) : go x x xs

mergeGap :: Int
mergeGap = 6

-- | Length of a row without its trailing default-style blanks.
blankTail :: [Cell] -> Int
blankTail cells = length (dropWhileEndBlank cells)
  where
    dropWhileEndBlank = reverse . dropWhile (== Cell ' ' defaultStyle) . reverse

-- | Cells, emitting a style change only where the style changes.
renderCells :: [Cell] -> Builder
renderCells = go Nothing
  where
    go _ [] = mempty
    go current (Cell c _ : rest) | c == continuation = go current rest
    go current (Cell c style : rest) =
      (if current == Just style then mempty else sgr style) <> charUtf8 c <> go (Just style) rest

