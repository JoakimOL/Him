-- | Pickers: a list to choose from, narrowed by typing a fuzzy query
-- (Helix's @space f@ and @space b@). Pure; the actions that open and
-- drive a picker are in "Him.Commands.Picker".
module Him.Picker
  ( Picker (..)
  , PickerItem (..)
  , PickTarget (..)
  , newPicker
  , matches
  , selectedItem
  , moveSelection
  , setQuery
  , fuzzyScore
  ) where

import Data.Char (toLower)
import Data.List (sortOn, tails)
import Data.Maybe (mapMaybe)
import Data.Text (Text)
import Data.Text qualified as T

-- | What choosing an item does.
data PickTarget
  = PickFile !FilePath
  | -- | A buffer, by its index in buffer order.
    PickBuffer !Int
  deriving stock (Eq, Show)

data PickerItem = PickerItem
  { piLabel :: !Text
  , piTarget :: !PickTarget
  }
  deriving stock (Eq, Show)

data Picker = Picker
  { pkTitle :: !Text
  , pkItems :: ![PickerItem]
  , pkQuery :: !Text
  , pkMatches :: ![PickerItem]
  -- ^ The items matching the query, best first (kept up to date by
  -- 'setQuery', so rendering does not filter again).
  , pkSelected :: !Int
  -- ^ Index into 'pkMatches'.
  }
  deriving stock (Eq, Show)

newPicker :: Text -> [PickerItem] -> Picker
newPicker title items = setQuery "" (Picker title items "" items 0)

-- | Change the query, filter again, and select the best match.
setQuery :: Text -> Picker -> Picker
setQuery q p = p {pkQuery = q, pkMatches = matches q (pkItems p), pkSelected = 0}

-- | Items matching a query, best first. An empty query keeps every item in
-- its original order.
matches :: Text -> [PickerItem] -> [PickerItem]
matches q items
  | T.null q = items
  | otherwise = map snd (sortOn fst (mapMaybe scored (zip [0 :: Int ..] items)))
  where
    scored (i, item) = (\s -> ((s, T.length (piLabel item), i), item)) <$> fuzzyScore q (piLabel item)

-- | Lower is better; 'Nothing' if the query's characters do not all occur
-- in order. The score is the number of characters skipped between the
-- first and the last matched one, so contiguous matches win ('matches'
-- breaks ties by the shorter label). Case is ignored.
fuzzyScore :: Text -> Text -> Maybe Int
fuzzyScore query label = case map toLower (T.unpack query) of
  [] -> Just 0
  qs@(q0 : _) ->
    -- Try each place the first character occurs; keep the best.
    case [s | t@(c : _) <- tails (map toLower (T.unpack label)), c == q0, Just s <- [skips qs t 0]] of
      [] -> Nothing
      ss -> Just (minimum ss)
  where
    skips [] _ cost = Just cost
    skips _ [] _ = Nothing
    skips qs@(q : qt) (c : ct) cost
      | q == c = skips qt ct cost
      | otherwise = skips qs ct (cost + 1)

selectedItem :: Picker -> Maybe PickerItem
selectedItem p = case drop (pkSelected p) (pkMatches p) of
  item : _ -> Just item
  [] -> Nothing

-- | Move the selection, wrapping around.
moveSelection :: Int -> Picker -> Picker
moveSelection n p = case length (pkMatches p) of
  0 -> p
  len -> p {pkSelected = (pkSelected p + n) `mod` len}
