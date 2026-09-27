-- | The scroll position of the text area.
module Him.View
  ( View (..)
  , initialView
  , scrollToCursor
  ) where

data View = View
  { viewTop :: !Int
  -- ^ First visible line.
  , viewLeft :: !Int
  -- ^ First visible display column.
  }
  deriving stock (Eq, Show)

initialView :: View
initialView = View 0 0

-- | Scroll the minimum amount needed so that the cursor at
-- @(line, displayCol)@ is visible in an area of @(height, width)@, keeping
-- @scrolloff@ lines of context above and below it.
scrollToCursor :: (Int, Int) -> Int -> (Int, Int) -> View -> View
scrollToCursor (height, width) scrolloff (line, col) (View top left) = View top' left'
  where
    so = max 0 (min scrolloff ((height - 1) `div` 2))
    top'
      | line < top + so = max 0 (line - so)
      | line >= top + height - so = max 0 (line - height + so + 1)
      | otherwise = top
    left'
      | col < left = col
      | col >= left + width = col - width + 1
      | otherwise = left
