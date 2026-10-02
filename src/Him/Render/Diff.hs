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
import Data.Sequence (Seq)

import Data.Sequence qualified as Seq
import Him.Render.Frame
import Him.Terminal.Ansi

-- | Output that turns the previous frame into the new one. Without a
-- previous frame, or if the size changed, the whole screen is redrawn.
--
-- When the view scrolled by less than a screen, the terminal is told to
-- scroll the text area first ('scrollRegion'), and the new frame is compared
-- with the shifted old one: only the lines that came into view (and cells
-- that really changed) are written.
diffFrames :: Maybe Frame -> Frame -> Builder
diffFrames prev new = hideCursor <> colors <> body <> sgr defaultStyle <> cursor
  where
    -- The theme's default colours, first, so a cleared screen has them.
    colors
      | fmap frameColors prev == Just (frameColors new) = mempty
      | otherwise = uncurry setDefaultColors (frameColors new)
    sameSize p = frameRows p == frameRows new && frameCols p == frameCols new
    rows = zip [0 ..] (toList (frameCells new))
    body = case prev of
      Just p
        | sameSize p ->
            let (scrollOut, old) = scrollRegion p new
             in scrollOut
                  <> foldMap
                    ( \(i, row) -> case Seq.lookup i old of
                        Just o | o == row -> mempty
                        Just o -> drawChanges i (toList o) (toList row)
                        Nothing -> drawRow i (toList row)
                    )
                    rows
      _ -> clearScreen <> foldMap (\(i, row) -> drawRow i (toList row)) rows
    cursor = case frameCursor new of
      Just (r, c) -> moveCursor r c <> cursorShape (frameCursorShape new) <> showCursor
      Nothing -> mempty

-- | If both frames scroll the same region and the view moved by less than
-- its height, the output that scrolls the terminal, and the old rows as they
-- are on screen afterwards.
scrollRegion :: Frame -> Frame -> (Builder, Seq (Seq Cell))
scrollRegion p new = case (frameScroll p, frameScroll new) of
  (Just a, Just b)
    | siRow a == siRow b && siHeight a == siHeight b && d /= 0 && abs d < siHeight b ->
        ( sgr defaultStyle
            <> setScrollRegion (siRow b) (siHeight b)
            <> (if d > 0 then scrollUp d else scrollDown (negate d))
            <> resetScrollRegion
        , shifted
        )
    where
      d = siTop b - siTop a
      blank = Seq.replicate (frameCols p) blankCell
      old = frameCells p
      shifted = Seq.mapWithIndex shiftRow old
      shiftRow i row
        | i < siRow b || i >= siRow b + siHeight b = row
        | otherwise = case i + d of
            j | j >= siRow b && j < siRow b + siHeight b -> Seq.index old j
            _ -> blank
  _ -> (mempty, frameCells p)

-- | A whole row (after a clear screen, so trailing blanks can be skipped).
drawRow :: Int -> [Cell] -> Builder
drawRow i cells = case blankTail cells of
  0 -> mempty
  n -> moveCursor i 0 <> renderCells (take n cells)

-- | Only the parts of a row that differ from the old one, in one pass.
-- Changed cells are collected into runs; a run absorbs the unchanged cells
-- up to the next change when that is within 'mergeGap' cells (a cursor
-- move costs about as much). Changes past the row's content are cleared
-- with one "erase to end of line".
drawChanges :: Int -> [Cell] -> [Cell] -> Builder
drawChanges i old new = go 0 Nothing (zip old new) <> clearTail
  where
    contentEnd = blankTail new
    tailChanged = or [a /= b | (a, b) <- drop contentEnd (zip old new)]
    clearTail
      | tailChanged = moveCursor i contentEnd <> sgr defaultStyle <> clearToEndOfLine
      | otherwise = mempty
    -- The open run: its start column, its cells (reversed), and the
    -- unchanged cells seen since its last change (reversed).
    go :: Int -> Maybe (Int, [Cell], [Cell]) -> [(Cell, Cell)] -> Builder
    go col run _
      | col >= contentEnd = flush run
    go col run ((o, n) : rest)
      | o /= n = case run of
          Just (start, acc, gap) -> go (col + 1) (Just (start, n : gap <> acc, [])) rest
          Nothing
            -- Never start drawing on the right half of a wide character.
            | isContinuation n && col > 0 -> go (col + 1) (Just (col - 1, [n, cellBefore col], [])) rest
            | otherwise -> go (col + 1) (Just (col, [n], [])) rest
      | otherwise = case run of
          Just (start, acc, gap)
            | length gap < mergeGap -> go (col + 1) (Just (start, acc, n : gap)) rest
            | otherwise -> flush run <> go (col + 1) Nothing rest
          Nothing -> go (col + 1) Nothing rest
    go _ run [] = flush run
    flush Nothing = mempty
    flush (Just (start, acc, _)) = moveCursor i start <> renderCells (reverse acc)
    cellBefore col = case drop (col - 1) new of
      (c : _) -> c
      [] -> blankCell
    isContinuation (Cell ch _) = ch == continuation

mergeGap :: Int
mergeGap = 6

-- | Length of a row without its trailing default-style blanks.
blankTail :: [Cell] -> Int
blankTail cells = length cells - length (takeWhile (== blankCell) (reverse cells))

-- | Cells, emitting a style change only where the style changes.
renderCells :: [Cell] -> Builder
renderCells = go Nothing
  where
    go _ [] = mempty
    go current (Cell c _ : rest) | c == continuation = go current rest
    go current (Cell c style : rest) =
      (if current == Just style then mempty else sgrPacked style) <> charUtf8 c <> go (Just style) rest

