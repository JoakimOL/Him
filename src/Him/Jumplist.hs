-- | The jumplist (ADR-47): where the cursor was before each jump, per
-- window, as in Helix. @C-o@ goes back through it, @C-i@ / @tab@ forward,
-- @C-s@ saves the selection into it, and @space j@ lists it. Pure; the
-- editor-level parts are in "Him.Actions.Jump".
module Him.Jumplist
  ( Jump (..)
  , Jumplist (..)
  , emptyJumplist
  , capacity
  , push
  , backward
  , forward
  , remove
  , mapJumps
  , mapThroughChange
  ) where

import Data.Foldable (toList)
import Data.Sequence (Seq)
import Data.Sequence qualified as Seq
import Data.Text (Text)
import Data.Text qualified as T
import Him.Position (Pos (..))
import Him.Selection (Range (..), Selection, mapRanges)

-- | A place: a document (by id) and the selection there.
data Jump = Jump
  { jumpDoc :: !Int
  , jumpSelection :: !Selection
  }
  deriving stock (Eq, Show)

data Jumplist = Jumplist
  { jlJumps :: !(Seq Jump)
  -- ^ Oldest first.
  , jlCurrent :: !Int
  -- ^ Where @C-o@ / @C-i@ are in the list; its length when not walking it.
  }
  deriving stock (Eq, Show)

emptyJumplist :: Jumplist
emptyJumplist = Jumplist Seq.empty 0

-- | At most this many jumps are kept (Helix's number).
capacity :: Int
capacity = 30

-- | Remember a place. As in Helix, the jumps after the current one are
-- dropped (a new jump starts a new future), the same place is not
-- pushed twice in a row, and the oldest goes when the list is full.
push :: Jump -> Jumplist -> Jumplist
push j (Jumplist js cur) =
  let kept = Seq.take cur js
      added = case Seq.viewr kept of
        _ Seq.:> lastJump | lastJump == j -> kept
        _ -> kept Seq.|> j
      trimmed = Seq.drop (Seq.length added - capacity) added
   in Jumplist trimmed (Seq.length trimmed)

-- | Go back @n@ jumps from @here@ (where the cursor is). When not walking
-- the list yet, @here@ is pushed first, so @C-i@ can come back to it. A
-- jump to where the cursor already is is skipped.
backward :: Int -> Jump -> Jumplist -> Maybe (Jump, Jumplist)
backward n here jl@(Jumplist js cur)
  | n < 1 || cur < n = Nothing
  | otherwise =
      let pushing = cur == Seq.length js
          jl'@(Jumplist js' _) = if pushing then push here jl else jl
          -- Pushing may have dropped the oldest jump, moving the others.
          appended = pushing && Seq.lookup (Seq.length js - 1) js /= Just here
          dropped = if pushing then Seq.length js + fromEnum appended - Seq.length js' else 0
          target = cur - n - dropped
          target' = if Seq.lookup target js' == Just here then target - 1 else target
       in if target' < 0 then Nothing else (\j -> (j, jl' {jlCurrent = target'})) <$> Seq.lookup target' js'

-- | Go forward @n@ jumps, if there are that many after the current one.
forward :: Int -> Jumplist -> Maybe (Jump, Jumplist)
forward n (Jumplist js cur)
  | n >= 1, cur + n < Seq.length js = (\j -> (j, Jumplist js (cur + n))) <$> Seq.lookup (cur + n) js
  | otherwise = Nothing

-- | Take out the jump at an index (the jumplist picker's delete).
remove :: Int -> Jumplist -> Jumplist
remove i (Jumplist js cur)
  | i < 0 || i >= Seq.length js = Jumplist js cur
  | otherwise = Jumplist (Seq.deleteAt i js) (if i < cur then cur - 1 else min cur (Seq.length js - 1))

-- | Change or drop jumps (a document's text changed, or it was closed).
mapJumps :: (Jump -> Maybe Jump) -> Jumplist -> Jumplist
mapJumps f (Jumplist js cur) =
  let indexed = zip [0 ..] (toList js)
      kept = [(i, j') | (i, j) <- indexed, Just j' <- [f j]]
      cur' = length [() | (i, _) <- kept, i < cur]
   in Jumplist (Seq.fromList (map snd kept)) cur'

-- | Move a selection through one change of the text (from
-- 'Him.Buffer.changeBetween': the replaced span, old positions, and the
-- new text). Positions before the change stay, those after it move with
-- the text, and those inside it go to its start.
mapThroughChange :: (Pos, Pos, Text) -> Selection -> Selection
mapThroughChange (start, end, new) = mapRanges (\r -> r {rangeAnchor = move (rangeAnchor r), rangeHead = move (rangeHead r)})
  where
    newEnd = case T.splitOn "\n" new of
      [one] -> Pos (posLine start) (posCol start + T.length one)
      parts -> Pos (posLine start + length parts - 1) (T.length (last parts))
    move p
      | p < start = p
      | p < end = start
      | posLine p == posLine end = Pos (posLine newEnd) (posCol newEnd + posCol p - posCol end)
      | otherwise = p {posLine = posLine p + posLine newEnd - posLine end}
