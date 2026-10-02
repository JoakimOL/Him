-- | Helix-style selections.
--
-- A 'Range' covers every character from its start to its end /inclusive/,
-- so even a "cursor" selects the one character under it. The head is where
-- the cursor is drawn; the anchor is the other end. In insert mode the head
-- is instead read as a gap: text is inserted before the character at the head.
module Him.Selection
  ( Range (..)
  , point
  , rangeStart
  , rangeEnd
  , collapse
  , isCollapsed
  , contains
  , Selection
  , single
  , fromRanges
  , normalize
  , primary
  , primaryIndex
  , ranges
  , rangeCount
  , mapRanges
  , modifyPrimary
  , keepPrimary
  , removePrimary
  , rotatePrimary
  ) where

import Data.List (findIndex, sortOn)
import Data.List.NonEmpty (NonEmpty (..))
import Data.List.NonEmpty qualified as NE
import Data.Maybe (fromMaybe)
import Him.Position (Pos)

data Range = Range
  { rangeAnchor :: !Pos
  , rangeHead :: !Pos
  , rangeWantCol :: !(Maybe Int)
  -- ^ Display column to return to when moving vertically through shorter lines.
  }
  deriving stock (Eq, Show)

-- | A range covering exactly one position.
point :: Pos -> Range
point p = Range p p Nothing

rangeStart :: Range -> Pos
rangeStart r = min (rangeAnchor r) (rangeHead r)

rangeEnd :: Range -> Pos
rangeEnd r = max (rangeAnchor r) (rangeHead r)

collapse :: Range -> Range
collapse r = r {rangeAnchor = rangeHead r}

isCollapsed :: Range -> Bool
isCollapsed r = rangeAnchor r == rangeHead r

contains :: Pos -> Range -> Bool
contains p r = rangeStart r <= p && p <= rangeEnd r

-- | One or more ranges, one of which is primary. 'fromRanges' keeps them
-- sorted by start and merges overlapping ones, which multi-range edits
-- rely on ("Him.Edit.applyEdits"). 'mapRanges' may leave them overlapping
-- (two cursors moved onto the same character); edits normalize first.
data Selection = Selection
  { selRanges :: !(NonEmpty Range)
  , selPrimary :: !Int
  }
  deriving stock (Eq, Show)

single :: Range -> Selection
single r = Selection (r :| []) 0

primary :: Selection -> Range
primary (Selection rs i) = case NE.drop i rs of
  (r : _) -> r
  [] -> NE.head rs

-- | A selection from ranges in any order, with the @i@th as primary.
-- Ranges are sorted by start, and overlapping ones are merged (the merged
-- range keeps the direction of its first part). 'Nothing' for no ranges.
fromRanges :: [Range] -> Int -> Maybe Selection
fromRanges rs i = case merge (sortOn (rangeStart . fst) [(r, [j]) | (r, j) <- zip rs [0 :: Int ..]]) of
  [] -> Nothing
  g : gs -> Just (Selection (NE.map fst (g :| gs)) (fromMaybe 0 (findIndex (elem i . snd) (g : gs))))
  where
    merge ((a, ia) : (b, ib) : rest)
      | rangeStart b <= rangeEnd a = merge ((union a b, ia <> ib) : rest)
      | otherwise = (a, ia) : merge ((b, ib) : rest)
    merge xs = xs
    union a b
      | rangeEnd b <= rangeEnd a = a
      | otherwise =
          let s = rangeStart a
              e = rangeEnd b
           in if rangeAnchor a <= rangeHead a then Range s e Nothing else Range e s Nothing

-- | Sort and merge the ranges (see 'fromRanges').
normalize :: Selection -> Selection
normalize s@(Selection (_ :| []) _) = s
normalize s = fromMaybe s (fromRanges (ranges s) (selPrimary s))

primaryIndex :: Selection -> Int
primaryIndex = selPrimary

ranges :: Selection -> [Range]
ranges = NE.toList . selRanges

rangeCount :: Selection -> Int
rangeCount = length . selRanges

-- | Drop every range but the primary one.
keepPrimary :: Selection -> Selection
keepPrimary s = single (primary s)

-- | Drop the primary range; the next one becomes primary. A single range
-- stays.
removePrimary :: Selection -> Selection
removePrimary s@(Selection rs i) = case [r | (j, r) <- zip [0 ..] (NE.toList rs), j /= i] of
  [] -> s
  r : more -> Selection (r :| more) (if i >= length more + 1 then 0 else i)

-- | Make the next (or, for a negative step, previous) range primary,
-- wrapping around.
rotatePrimary :: Int -> Selection -> Selection
rotatePrimary step (Selection rs i) = Selection rs ((i + step) `mod` length rs)

mapRanges :: (Range -> Range) -> Selection -> Selection
mapRanges f s = s {selRanges = NE.map f (selRanges s)}

modifyPrimary :: (Range -> Range) -> Selection -> Selection
modifyPrimary f (Selection rs i) =
  Selection (NE.zipWith (\j r -> if j == i then f r else r) (NE.iterate (+ 1) 0) rs) i
