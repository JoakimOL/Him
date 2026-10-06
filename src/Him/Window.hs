-- | Windows (splits, ADR window-splits): how the screen is divided between views, as
-- a tree like Helix's. Pure; the editor keeps the tree and the windows
-- that are not focused ("Him.Editor"), rendering draws each window in its
-- box ("Him.Render").
module Him.Window
  ( Axis (..)
  , Layout (..)
  , Window (..)
  , Box (..)
  , Side (..)
  , leaves
  , insertBeside
  , removeWindow
  , swapWindows
  , boxes
  , neighbour
  ) where

import Data.List (sortOn)
import Data.Maybe (fromMaybe, mapMaybe)
import Him.Selection (Selection)
import Him.View (View)

-- | How a split lays out its children: side by side (@:vsplit@), or one
-- above the other (@:hsplit@).
data Axis = Beside | Stacked
  deriving stock (Eq, Show)

-- | Windows by id, in the order they are shown (left to right, top to
-- bottom).
data Layout = Leaf !Int | Split !Axis ![Layout]
  deriving stock (Eq, Show)

-- | A window that is not focused: the document it shows (by id), where it
-- is scrolled, and its selection (the focused window's are the editor's
-- own 'Him.Editor.edView' and the document's selection).
data Window = Window
  { winDoc :: !Int
  , winView :: !View
  , winSelection :: !Selection
  }
  deriving stock (Eq, Show)

-- | A screen area: top row, left column, height, width.
data Box = Box
  { boxRow :: !Int
  , boxCol :: !Int
  , boxHeight :: !Int
  , boxWidth :: !Int
  }
  deriving stock (Eq, Show)

data Side = SideLeft | SideRight | SideUp | SideDown
  deriving stock (Eq, Show)

leaves :: Layout -> [Int]
leaves = \case
  Leaf w -> [w]
  Split _ cs -> concatMap leaves cs

-- | Put a new window next to (after) another. Inside a split along the
-- same axis it becomes a sibling; otherwise the window is split in two.
insertBeside :: Axis -> Int -> Int -> Layout -> Layout
insertBeside axis target new = go
  where
    go = \case
      Leaf w
        | w == target -> Split axis [Leaf w, Leaf new]
        | otherwise -> Leaf w
      Split a cs
        | a == axis, Leaf target `elem` cs -> Split a (concatMap (\c -> if c == Leaf target then [c, Leaf new] else [c]) cs)
        | otherwise -> Split a (map go cs)

-- | Take a window out; a split left with one child becomes that child.
-- The last window stays (there is always one).
removeWindow :: Int -> Layout -> Layout
removeWindow w layout = fromMaybe layout (go layout)
  where
    go = \case
      Leaf x -> if x == w then Nothing else Just (Leaf x)
      Split a cs -> case mapMaybe go cs of
        [] -> Nothing
        [c] -> Just c
        cs' -> Just (Split a cs')

-- | Exchange two windows' places.
swapWindows :: Int -> Int -> Layout -> Layout
swapWindows a b = \case
  Leaf w
    | w == a -> Leaf b
    | w == b -> Leaf a
    | otherwise -> Leaf w
  Split ax cs -> Split ax (map (swapWindows a b) cs)

-- | Each window's box inside an area. Children share their split's space
-- evenly; windows side by side are separated by a one-column border.
boxes :: Box -> Layout -> [(Int, Box)]
boxes box = \case
  Leaf w -> [(w, box)]
  Split axis cs ->
    let n = length cs
        total = case axis of
          Beside -> boxWidth box - (n - 1)
          Stacked -> boxHeight box
        sizes = [total `div` n + (if i < total `mod` n then 1 else 0) | i <- [0 .. n - 1]]
        offsets = scanl (\o s -> o + s + gap) 0 sizes
        gap = if axis == Beside then 1 else 0
        child off size = case axis of
          Beside -> box {boxCol = boxCol box + off, boxWidth = size}
          Stacked -> box {boxRow = boxRow box + off, boxHeight = size}
     in concat [boxes (child off size) c | (c, off, size) <- zip3 cs offsets sizes]

-- | The window next to one on a side: the nearest box that lies on that
-- side and overlaps it across, the one most in line with it first.
neighbour :: Side -> Int -> [(Int, Box)] -> Maybe Int
neighbour side w placed = case lookup w placed of
  Nothing -> Nothing
  Just b -> case sortOn snd [(x, rank b o) | (x, o) <- placed, x /= w, Just _ <- [gap b o], overlaps b o] of
    (x, _) : _ -> Just x
    [] -> Nothing
  where
    gap b o = case side of
      SideRight | boxCol o >= boxCol b + boxWidth b -> Just (boxCol o - boxCol b - boxWidth b)
      SideLeft | boxCol o + boxWidth o <= boxCol b -> Just (boxCol b - boxCol o - boxWidth o)
      SideDown | boxRow o >= boxRow b + boxHeight b -> Just (boxRow o - boxRow b - boxHeight b)
      SideUp | boxRow o + boxHeight o <= boxRow b -> Just (boxRow b - boxRow o - boxHeight o)
      _ -> Nothing
    across b o = case side of
      SideLeft -> (boxRow b, boxHeight b, boxRow o, boxHeight o)
      SideRight -> (boxRow b, boxHeight b, boxRow o, boxHeight o)
      _ -> (boxCol b, boxWidth b, boxCol o, boxWidth o)
    overlaps b o = let (s1, l1, s2, l2) = across b o in s1 < s2 + l2 && s2 < s1 + l1
    rank b o = let (s1, _, s2, _) = across b o in (fromMaybe 0 (gap b o), abs (s1 - s2))
