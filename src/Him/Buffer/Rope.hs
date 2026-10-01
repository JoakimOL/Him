-- | The storage behind "Him.Buffer": a weight-balanced tree of blocks.
--
-- A 'Block' is one contiguous UTF-8 'Text' holding many lines separated by
-- @\\n@, plus an array of line start offsets. A loaded file is a handful of
-- large blocks (one per read chunk), so a line costs 4 bytes of offset
-- instead of a heap object per line. Edits split blocks by slicing (O(1))
-- and insert small new blocks for the changed lines.
--
-- The tree caches the number of lines of every subtree, so finding a line
-- is O(log blocks), and it is balanced by the number of blocks (Adams'
-- weight-balanced trees, with the (3, 2) parameters proven correct by Hirai
-- and Yamamoto).
module Him.Buffer.Rope
  ( Rope
  , Block
  , blockFromText
  , ropeFromBlocks
  , ropeLines
  , ropeLineAt
  , ropeSplitAt
  , ropeAppend
  , ropeLinesFrom
  , ropeLinesDownFrom
  , ropeBlocksFrom
  , ropeBlocksDownFrom
  , blockLines
  , blockLine
  , blockRegion
  , blockLineOfOffset
  , blockLineStart
  ) where

import Data.Text (Text)
import Data.Text.Array qualified as A
import Data.Text.Internal (Text (..))
import Data.Text.Unsafe (dropWord8, takeWord8)
import Him.Native (Offsets, lineStarts, offsetAt)

-- | Lines @first .. first + count - 1@ of a text. Line @k@ spans the bytes
-- @[start k, start (k + 1) - 1)@, minus a trailing @\\r@ when 'blkCR' is set
-- (CRLF files).
data Block = Block
  { blkText :: !Text
  , blkStarts :: !Offsets
  , blkFirst :: !Int
  , blkCount :: !Int
  , blkCR :: !Bool
  }

-- | All lines of a text (split on @\\n@) as one block; nothing is copied.
blockFromText :: Bool -> Text -> Block
blockFromText cr t = Block t starts 0 count cr
  where
    (starts, count) = lineStarts t

blockLines :: Block -> Int
blockLines = blkCount

-- | Byte offset of a line's start within the block's text.
blockLineStart :: Block -> Int -> Int
blockLineStart b k = offsetAt (blkStarts b) (blkFirst b + k)

blockLine :: Block -> Int -> Text
blockLine b k = slice s (lineEnd b k - s) (blkText b)
  where
    s = blockLineStart b k

lineEnd :: Block -> Int -> Int
lineEnd b k
  | blkCR b && e > s && byteAt (blkText b) (e - 1) == 13 = e - 1
  | otherwise = e
  where
    s = blockLineStart b k
    e = blockLineStart b (k + 1) - 1

byteAt :: Text -> Int -> Int
byteAt (Text arr off _) i = fromIntegral (A.unsafeIndex arr (off + i))

slice :: Int -> Int -> Text -> Text
slice start len = takeWord8 len . dropWord8 start

-- | The block's lines as one contiguous text (joined by their original
-- line breaks), and the byte offset of its first line within 'blkText'.
-- Used by search to scan many lines with one call.
blockRegion :: Block -> (Text, Int)
blockRegion b = (slice s (e - s) (blkText b), s)
  where
    s = blockLineStart b 0
    e = blockLineStart b (blkCount b) - 1

-- | The line (relative to the block) containing a byte offset of
-- 'blkText', by binary search over the line starts.
blockLineOfOffset :: Block -> Int -> Int
blockLineOfOffset b off = go 0 (blkCount b - 1)
  where
    go lo hi
      | lo >= hi = lo
      | otherwise =
          let mid = (lo + hi + 1) `div` 2
           in if blockLineStart b mid <= off then go mid hi else go lo (mid - 1)

dropLines, takeLines :: Int -> Block -> Block
dropLines k b = b {blkFirst = blkFirst b + k, blkCount = blkCount b - k}
takeLines k b = b {blkCount = k}

-- Tree ---------------------------------------------------------------------

-- | Subtree size (in blocks), line count, left, block, right.
data Rope = Tip | Bin !Int !Int !Rope !Block !Rope

size :: Rope -> Int
size Tip = 0
size (Bin s _ _ _ _) = s

ropeLines :: Rope -> Int
ropeLines Tip = 0
ropeLines (Bin _ n _ _ _) = n

bin :: Rope -> Block -> Rope -> Rope
bin l b r = Bin (size l + size r + 1) (ropeLines l + ropeLines r + blkCount b) l b r

singleton :: Block -> Rope
singleton b = bin Tip b Tip

delta, ratio :: Int
delta = 3
ratio = 2

balance :: Rope -> Block -> Rope -> Rope
balance l b r
  | sl + sr <= 1 = bin l b r
  | sr > delta * sl = rotateL l b r
  | sl > delta * sr = rotateR l b r
  | otherwise = bin l b r
  where
    sl = size l
    sr = size r

rotateL, rotateR :: Rope -> Block -> Rope -> Rope
rotateL l b r@(Bin _ _ rl _ rr)
  | size rl < ratio * size rr = singleL l b r
  | otherwise = doubleL l b r
rotateL l b Tip = bin l b Tip
rotateR l@(Bin _ _ ll _ lr) b r
  | size lr < ratio * size ll = singleR l b r
  | otherwise = doubleR l b r
rotateR Tip b r = bin Tip b r

singleL, singleR, doubleL, doubleR :: Rope -> Block -> Rope -> Rope
singleL l b (Bin _ _ rl rb rr) = bin (bin l b rl) rb rr
singleL l b Tip = bin l b Tip
singleR (Bin _ _ ll lb lr) b r = bin ll lb (bin lr b r)
singleR Tip b r = bin Tip b r
doubleL l b (Bin _ _ (Bin _ _ rll rlb rlr) rb rr) = bin (bin l b rll) rlb (bin rlr rb rr)
doubleL l b r = singleL l b r
doubleR (Bin _ _ ll lb (Bin _ _ lrl lrb lrr)) b r = bin (bin ll lb lrl) lrb (bin lrr b r)
doubleR l b r = singleR l b r

insertMin, insertMax :: Block -> Rope -> Rope
insertMin b Tip = singleton b
insertMin b (Bin _ _ l x r) = balance (insertMin b l) x r
insertMax b Tip = singleton b
insertMax b (Bin _ _ l x r) = balance l x (insertMax b r)

-- | Join two trees with a block between them.
link :: Rope -> Block -> Rope -> Rope
link Tip b r = insertMin b r
link l b Tip = insertMax b l
link l@(Bin sl _ ll lb lr) b r@(Bin sr _ rl rb rr)
  | delta * sl < sr = balance (link l b rl) rb rr
  | delta * sr < sl = balance ll lb (link lr b r)
  | otherwise = bin l b r

-- | Concatenate two trees.
ropeAppend :: Rope -> Rope -> Rope
ropeAppend Tip r = r
ropeAppend l Tip = l
ropeAppend l@(Bin sl _ ll lb lr) r@(Bin sr _ rl rb rr)
  | delta * sl < sr = balance (ropeAppend l rl) rb rr
  | delta * sr < sl = balance ll lb (ropeAppend lr r)
  | size l > size r = let (m, l') = deleteMax l in balance l' m r
  | otherwise = let (m, r') = deleteMin r in balance l m r'

deleteMin, deleteMax :: Rope -> (Block, Rope)
deleteMin (Bin _ _ Tip b r) = (b, r)
deleteMin (Bin _ _ l b r) = let (m, l') = deleteMin l in (m, balance l' b r)
deleteMin Tip = error "Rope.deleteMin: empty"
deleteMax (Bin _ _ l b Tip) = (b, l)
deleteMax (Bin _ _ l b r) = let (m, r') = deleteMax r in (m, balance l b r')
deleteMax Tip = error "Rope.deleteMax: empty"

-- | Blocks with no lines are dropped.
ropeFromBlocks :: [Block] -> Rope
ropeFromBlocks = foldl' (\t b -> if blkCount b == 0 then t else insertMax b t) Tip

-- | The block holding a line and the line's index within it.
lookupLine :: Int -> Rope -> Maybe (Block, Int)
lookupLine _ Tip = Nothing
lookupLine i (Bin _ _ l b r)
  | i < nl = lookupLine i l
  | i < nl + blkCount b = Just (b, i - nl)
  | otherwise = lookupLine (i - nl - blkCount b) r
  where
    nl = ropeLines l

ropeLineAt :: Int -> Rope -> Maybe Text
ropeLineAt i r = uncurry blockLine <$> lookupLine i r

-- | Lines @[0, i)@ and @[i, ..)@. A block containing the split point is
-- sliced, not copied.
ropeSplitAt :: Int -> Rope -> (Rope, Rope)
ropeSplitAt _ Tip = (Tip, Tip)
ropeSplitAt i t@(Bin _ _ l b r)
  | i <= 0 = (Tip, t)
  | i < nl = let (a, c) = ropeSplitAt i l in (a, link c b r)
  | i == nl = (l, insertMin b r)
  | i < nl + nb = (insertMax (takeLines k b) l, insertMin (dropLines k b) r)
  | otherwise = let (a, c) = ropeSplitAt (i - nl - nb) r in (link l b a, c)
  where
    nl = ropeLines l
    nb = blkCount b
    k = i - nl

-- | Lines from an index to the end, lazily.
ropeLinesFrom :: Int -> Rope -> [Text]
ropeLinesFrom i t = concat [map (blockLine b) [0 .. blkCount b - 1] | (_, b) <- ropeBlocksFrom i t]

-- | Lines from an index down to the first, lazily.
ropeLinesDownFrom :: Int -> Rope -> [Text]
ropeLinesDownFrom i t = concat [map (blockLine b) [blkCount b - 1, blkCount b - 2 .. 0] | (_, b) <- ropeBlocksDownFrom i t]

-- | The blocks from a line to the end, each with the index of its first
-- line; the first block is sliced to start at the line.
ropeBlocksFrom :: Int -> Rope -> [(Int, Block)]
ropeBlocksFrom i0 t0 = go (max 0 i0) 0 t0 []
  where
    -- @base@ is the global index of the subtree's first line.
    go _ _ Tip rest = rest
    go i base (Bin _ _ l b r) rest
      | i >= nl + nb = go (i - nl - nb) (base + nl + nb) r rest
      | i >= nl = (base + i, dropLines (i - nl) b) : go 0 (base + nl + nb) r rest
      | otherwise = go i base l ((base + nl, b) : go 0 (base + nl + nb) r rest)
      where
        nl = ropeLines l
        nb = blkCount b

-- | The blocks from a line down to the first, in reverse order, each with
-- the index of its first line; the first block is cut to end at the line.
ropeBlocksDownFrom :: Int -> Rope -> [(Int, Block)]
ropeBlocksDownFrom i0 t0 = go i0 0 t0 []
  where
    go _ _ Tip rest = rest
    go i base (Bin _ _ l b r) rest
      | i < 0 = rest
      | i < nl = go i base l rest
      | i < nl + nb = (base + nl, takeLines (i - nl + 1) b) : go (nl - 1) base l rest
      | otherwise = go (i - nl - nb) (base + nl + nb) r ((base + nl, b) : go (nl - 1) base l rest)
      where
        nl = ropeLines l
        nb = blkCount b
