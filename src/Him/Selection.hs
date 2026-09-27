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
  , primary
  , ranges
  , mapRanges
  , modifyPrimary
  ) where

import Data.List.NonEmpty (NonEmpty (..))
import Data.List.NonEmpty qualified as NE
import Him.Position (Pos)

data Range = Range
  { rangeAnchor :: !Pos
  , rangeHead :: !Pos
  , rangeWantCol :: !(Maybe Int)
  -- ^ Column to return to when moving vertically through shorter lines.
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

-- | One or more ranges, one of which is primary. Only single-range
-- selections are created so far, but everything that can work on all ranges
-- (motions, rendering) already does.
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

ranges :: Selection -> [Range]
ranges = NE.toList . selRanges

mapRanges :: (Range -> Range) -> Selection -> Selection
mapRanges f s = s {selRanges = NE.map f (selRanges s)}

modifyPrimary :: (Range -> Range) -> Selection -> Selection
modifyPrimary f (Selection rs i) =
  Selection (NE.zipWith (\j r -> if j == i then f r else r) (NE.iterate (+ 1) 0) rs) i
