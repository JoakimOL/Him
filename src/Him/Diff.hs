-- | Line diffs (ADR git): which lines changed between two versions of a
-- text, as hunks. Used for the git signs in the gutter and for staging.
--
-- Myers' O(ND) algorithm, after trimming the common prefix and suffix, so
-- a few edits in a large file stay cheap. A middle that needs more than
-- 'maxEdits' edits is reported as one hunk, which bounds time and memory
-- for a rewritten file (git does something similar).
module Him.Diff
  ( Hunk (..)
  , HunkKind (..)
  , hunkKind
  , diffLines
  , applyHunks
  , mapLine
  , maxEdits
  ) where

import Control.Monad (forM_, when)
import Control.Monad.ST (ST, runST)
import Data.Array (Array, listArray, (!))
import Data.Array.ST (STUArray, freeze, newArray, readArray, writeArray)
import Data.Array.Unboxed (UArray)
import Data.Array.Unboxed qualified as U
import Data.STRef (newSTRef, readSTRef, writeSTRef)
import Data.Text (Text)

-- | Lines @[oldStart, oldStart + oldCount)@ of the old text became lines
-- @[newStart, newStart + newCount)@ of the new one. Numbered from 0.
data Hunk = Hunk
  { hOldStart :: !Int
  , hOldCount :: !Int
  , hNewStart :: !Int
  , hNewCount :: !Int
  }
  deriving stock (Eq, Show)

data HunkKind = Added | Removed | Changed
  deriving stock (Eq, Show)

hunkKind :: Hunk -> HunkKind
hunkKind h
  | hOldCount h == 0 = Added
  | hNewCount h == 0 = Removed
  | otherwise = Changed

-- | Above this many edits in the trimmed middle, report one hunk.
maxEdits :: Int
maxEdits = 1000

-- | The hunks that turn the old lines into the new ones, in order.
diffLines :: [Text] -> [Text] -> [Hunk]
diffLines old new =
  let prefix = length (takeWhile id (zipWith (==) old new))
      old' = drop prefix old
      new' = drop prefix new
      suffix = length (takeWhile id (zipWith (==) (reverse old') (reverse new')))
      a = take (length old' - suffix) old'
      b = take (length new' - suffix) new'
   in map (shift prefix) (middle a b)
  where
    shift k (Hunk os oc ns nc) = Hunk (os + k) oc (ns + k) nc

-- | Diff of the trimmed middle.
middle :: [Text] -> [Text] -> [Hunk]
middle [] [] = []
middle a b
  | null a || null b = [Hunk 0 (length a) 0 (length b)]
  | otherwise = case myers (listArray (0, n - 1) a) n (listArray (0, m - 1) b) m of
      Just script -> hunksOf script
      Nothing -> [Hunk 0 n 0 m]
  where
    n = length a
    m = length b

-- | One step of an edit script.
data Step = Keep | Delete | Insert
  deriving stock (Eq)

-- | Group a script into hunks (runs of deletes and inserts).
hunksOf :: [Step] -> [Hunk]
hunksOf = go 0 0
  where
    go _ _ [] = []
    go x y (Keep : rest) = go (x + 1) (y + 1) rest
    go x y steps =
      let (run, rest) = span (/= Keep) steps
          dels = length (filter (== Delete) run)
          ins = length run - dels
       in Hunk x dels y ins : go (x + dels) (y + ins) rest

-- | Myers' greedy forward algorithm. The furthest-reaching x on each
-- diagonal is kept for every d, and the path is read back from those.
-- 'Nothing' when more than 'maxEdits' edits are needed.
myers :: Array Int Text -> Int -> Array Int Text -> Int -> Maybe [Step]
myers a n b m = runST $ do
  let offset = maxD + 1
      maxD = min maxEdits (n + m)
  v <- newArray (0, 2 * offset) 0 :: ST s (STUArray s Int Int)
  trace <- newSTRef []
  found <- newSTRef Nothing
  let snake x y = if x < n && y < m && a ! x == b ! y then snake (x + 1) (y + 1) else x
      loop d
        | d > maxD = pure ()
        | otherwise = do
            forM_ [-d, -d + 2 .. d] $ \k -> do
              done <- readSTRef found
              when (done == Nothing) $ do
                down <- if k == -d then pure True else if k == d then pure False else (<) <$> readArray v (offset + k - 1) <*> readArray v (offset + k + 1)
                xStart <- if down then readArray v (offset + k + 1) else (+ 1) <$> readArray v (offset + k - 1)
                let x = snake xStart (xStart - k)
                writeArray v (offset + k) x
                when (x >= n && x - k >= m) (writeSTRef found (Just d))
            snapshot <- freeze v
            modifyTrace trace snapshot
            readSTRef found >>= \case
              Just _ -> pure ()
              Nothing -> loop (d + 1)
      modifyTrace ref s = readSTRef ref >>= writeSTRef ref . (s :)
  loop 0
  readSTRef found >>= \case
    Nothing -> pure Nothing
    Just d -> do
      snapshots <- readSTRef trace
      pure (Just (backtrack offset d (reverse snapshots)))
  where
    -- From the end, step back through the saved arrays: at each d, decide
    -- whether the last move was a delete (from diagonal k+1) or an insert
    -- (from k-1), with the matching run of keeps after it.
    backtrack :: Int -> Int -> [UArray Int Int] -> [Step]
    backtrack offset dFinal snaps = go dFinal n m []
      where
        snapAt d = snaps !! d
        go d x y acc
          | d == 0 = replicate x Keep <> acc
          | otherwise =
              let prev = snapAt (d - 1)
                  k = x - y
                  down = k == -d || (k /= d && prev U.! (offset + k - 1) < prev U.! (offset + k + 1))
                  prevK = if down then k + 1 else k - 1
                  prevX = prev U.! (offset + prevK)
                  prevY = prevX - prevK
                  -- After the move: (prevX, prevY+1) for an insert, (prevX+1, prevY) for a delete.
                  (midX, step) = if down then (prevX, Insert) else (prevX + 1, Delete)
                  keeps = x - midX
               in go (d - 1) prevX prevY (step : replicate keeps Keep <> acc)

-- | Apply hunks (from 'diffLines' old new) to the old lines, taking the
-- replacement lines from the new ones.
applyHunks :: [Hunk] -> [Text] -> [Text] -> [Text]
applyHunks hunks old new = go 0 hunks old
  where
    go _ [] rest = rest
    go pos (h : hs) rest =
      let (keep, rest') = splitAt (hOldStart h - pos) rest
       in keep
            <> take (hNewCount h) (drop (hNewStart h) new)
            <> go (hOldStart h + hOldCount h) hs (drop (hOldCount h) rest')

-- | Where an old line is in the new text: shifted by the hunks before it;
-- a line inside a hunk maps to the start of its replacement.
mapLine :: [Hunk] -> Int -> Int
mapLine hunks line = go 0 hunks
  where
    go delta [] = line + delta
    go delta (h : hs)
      | line < hOldStart h = line + delta
      | line < hOldStart h + hOldCount h = hNewStart h
      | otherwise = go (delta + hNewCount h - hOldCount h) hs
