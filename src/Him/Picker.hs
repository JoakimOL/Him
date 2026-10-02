-- | Pickers: a list to choose from, narrowed by typing a fuzzy query
-- (Helix's @space f@ and @space b@). Pure; the actions that open and
-- drive a picker are in "Him.Actions.Picker".
module Him.Picker
  ( Picker (..)
  , PickerItem (..)
  , pickerItem
  , PickTarget (..)
  , PickerSource (..)
  , newPicker
  , matches
  , selectedItem
  , moveSelection
  , setQuery
  , addItems
  , matchLimit
  , rank
  , syncLimit
  , fuzzyScore
  , labelWidth
  ) where

import Data.Foldable (toList)
import Data.IntMap.Strict qualified as IntMap
import Data.List (tails)
import Data.Sequence (Seq)
import Data.Sequence qualified as Seq
import Data.Text (Text)
import Data.Text qualified as T
import Him.Json (Value)

-- | What choosing an item does.
data PickTarget
  = PickFile !FilePath
  | -- | A buffer, by its index in buffer order.
    PickBuffer !Int
  | -- | Run an action (the command palette); 'True' when it needs
    -- arguments, which are then asked for on the @:@ line.
    PickAction !Text !Bool
  | -- | A place in a file: path, line, column; the column is in the
    -- characters, or in a language server's units when its encoding is
    -- named (converted once the file is open).
    PickPosition !FilePath !Int !Int !(Maybe Text)
  | -- | A language server's code action (or command), as it sent it.
    PickCodeAction !Value
  deriving stock (Eq, Show)

-- | Where a picker's items come from.
data PickerSource
  = -- | Given when it opened (or streamed in); filtered here.
    StaticItems
  | -- | Asked from a language server (by key) for each query, which also
    -- filters them (workspace symbols).
    ServerQuery !Text
  deriving stock (Eq, Show)

data PickerItem = PickerItem
  { piLabel :: !Text
  , piTarget :: !PickTarget
  , piDetail :: !Text
  -- ^ Shown dimmed after the label (e.g. keys and a description); also
  -- matched, after the label.
  , piKey :: !Text
  -- ^ The label in lower case, computed once ('pickerItem') for matching.
  , piName :: !Text
  -- ^ The key's last path component (a file's name).
  , piLength :: !Int
  -- ^ The label's length in characters.
  }
  deriving stock (Eq, Show)

pickerItem :: Text -> PickTarget -> Text -> PickerItem
pickerItem label target detail = PickerItem label target detail key (T.takeWhileEnd (/= '/') key) (T.length label)
  where
    key = T.toLower label

data Picker = Picker
  { pkTitle :: !Text
  , pkItems :: !(Seq PickerItem)
  , pkQuery :: !Text
  , pkMatches :: ![PickerItem]
  -- ^ The best matches of the query, best first, at most 'matchLimit'
  -- (kept up to date by 'setQuery' and 'addItems', so rendering does not
  -- filter again).
  , pkMatchCount :: !Int
  -- ^ How many items match in all.
  , pkSelected :: !Int
  -- ^ Index into 'pkMatches'.
  , pkGeneration :: !Int
  -- ^ Which background scan fills this picker (items from another are
  -- dropped); 0 for a picker filled at once.
  , pkLoading :: !Bool
  -- ^ Items are still arriving.
  , pkStale :: !Bool
  -- ^ The matches are for an earlier query; a background filter is running.
  , pkSource :: !PickerSource
  , pkLabelWidth :: !Int
  -- ^ The longest label of all items, so the detail column stays put while
  -- scrolling and filtering.
  }
  deriving stock (Eq, Show)

newPicker :: Text -> [PickerItem] -> Picker
newPicker title items = refilter (Picker title (Seq.fromList items) "" [] 0 0 0 False False StaticItems (labelWidth items))

-- | The longest label among items.
labelWidth :: [PickerItem] -> Int
labelWidth items = maximum (0 : map piLength items)

-- | Change the query, filter again, and select the best match.
setQuery :: Text -> Picker -> Picker
setQuery q p = refilter p {pkQuery = q, pkSelected = 0}

-- | Items that arrived (from a background scan): filter again, keeping the
-- selection where it is when possible.
addItems :: [PickerItem] -> Picker -> Picker
addItems new p = refilter p {pkItems = pkItems p <> Seq.fromList new, pkLabelWidth = max (pkLabelWidth p) (labelWidth new)}

refilter :: Picker -> Picker
refilter p =
  let (ms, n) = rank (pkQuery p) (toList (pkItems p))
   in p {pkMatches = ms, pkMatchCount = n, pkSelected = max 0 (min (pkSelected p) (length ms - 1))}

-- | Pickers with more items than this filter in a background job when the
-- query is not empty (ADR-24); smaller ones filter at once.
syncLimit :: Int
syncLimit = 20000

-- | At most this many matches are kept and shown.
matchLimit :: Int
matchLimit = 1000

-- | The best matches of a query (at most 'matchLimit'), best first. An
-- empty query keeps the original order.
matches :: Text -> [PickerItem] -> [PickerItem]
matches q = fst . rank q

-- | The best matches, and how many items match in all.
--
-- Fast path for large lists (ADR-24): a cheap in-order character check on
-- the precomputed lower-case key rejects most items before any scoring,
-- and instead of sorting every match the matches are bucketed by their
-- rank (score, exactness, length), which keeps the original order inside
-- a bucket; only the buckets needed for the first 'matchLimit' are read.
rank :: Text -> [PickerItem] -> ([PickerItem], Int)
rank q items
  | T.null q = (take matchLimit items, length items)
  | otherwise =
      let scored = [(bucket s item, item) | item <- items, Just s <- [score item]]
          buckets = IntMap.fromListWith (<>) [(k, [item]) | (k, item) <- scored]
       in (take matchLimit (concatMap reverse (IntMap.elems buckets)), length scored)
  where
    lq = T.toLower q
    qs = T.unpack lq
    single = T.length lq == 1
    -- A match in the label beats any match found only in the detail.
    score item
      | inOrder lq (piKey item) = Just (if single then 0 else bestSkips qs (T.unpack (piKey item)))
      | not (T.null (piDetail item)) = (+ 100000) <$> fuzzyScore lq (piDetail item)
      | otherwise = Nothing
    -- Score, then a label whose first word or file name is the query
    -- itself (goto_line before goto_line_end), then the shorter label.
    bucket s item =
      s * 2 ^ (21 :: Int)
        + (if exact item then 0 else 2 ^ (20 :: Int))
        + min (2 ^ (20 :: Int) - 1) (piLength item)
    -- The first word is the query: a prefix followed by the end or a space.
    exact item =
      piName item == lq
        || ( lq `T.isPrefixOf` piKey item
              && maybe True ((== ' ') . fst) (T.uncons (T.drop qlen (piKey item)))
           )
    qlen = T.length lq

-- | Do the query's characters occur in the key, in order?
inOrder :: Text -> Text -> Bool
inOrder q key = case T.uncons q of
  Nothing -> True
  Just (c, rest) -> case T.uncons (T.dropWhile (/= c) key) of
    Just (_, key') -> inOrder rest key'
    Nothing -> False

-- | Lower is better; 'Nothing' if the query's characters do not all occur
-- in order. The score is the number of characters skipped between the
-- first and the last matched one, so contiguous matches win ('matches'
-- breaks ties by the shorter label). Case is ignored.
fuzzyScore :: Text -> Text -> Maybe Int
fuzzyScore query label
  | inOrder q key = Just (bestSkips (T.unpack q) (T.unpack key))
  | otherwise = Nothing
  where
    q = T.toLower query
    key = T.toLower label

-- | For a query known to match in order: the fewest characters skipped,
-- trying each place the first character occurs.
bestSkips :: String -> String -> Int
bestSkips [] _ = 0
bestSkips qs@(q0 : _) key = minimum (maxBound : [s | t@(c : _) <- tails key, c == q0, Just s <- [skips qs t 0]])
  where
    skips [] _ cost = Just cost
    skips _ [] _ = Nothing
    skips (q : qt) (c : ct) cost
      | q == c = skips qt ct cost
      | otherwise = skips (q : qt) ct (cost + 1)

selectedItem :: Picker -> Maybe PickerItem
selectedItem p = case drop (pkSelected p) (pkMatches p) of
  item : _ -> Just item
  [] -> Nothing

-- | Move the selection, wrapping around.
moveSelection :: Int -> Picker -> Picker
moveSelection n p = case length (pkMatches p) of
  0 -> p
  len -> p {pkSelected = (pkSelected p + n) `mod` len}
