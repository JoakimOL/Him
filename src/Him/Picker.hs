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
  , toggleMark
  , unmark
  , chosenItems
  , isMarked
  , markCount
  , setQuery
  , addItems
  , numbered
  , matchLimit
  , rank
  , rankWith
  , Atom (..)
  , AtomKind (..)
  , Column (..)
  , parseQuery
  , defaultColumns
  , grepQuery
  , pathMatches
  , syncLimit
  , fuzzyScore
  , labelWidth
  , boxHeight
  , previewRows
  , previewTop
  , previewOffset
  ) where

import Data.Foldable (toList)
import Data.IntMap.Strict qualified as IntMap
import Data.IntSet (IntSet)
import Data.IntSet qualified as IntSet
import Data.Bits (setBit, testBit, (.|.))
import Data.Word (Word64)
import Data.Char (isDigit, isLower, isUpper)
import Data.List (foldl')
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
  | -- | An entry of the focused window's jumplist, by index (ADR jumplist).
    PickJump !Int
  | -- | Data for the picker's own actions to read (ADR picker-actions); choosing it
    -- with the built-in @picker_open@ does nothing.
    PickValue !Text
  deriving stock (Eq, Show)

-- | Where a picker's items come from.
data PickerSource
  = -- | Given when it opened (or streamed in); filtered here.
    StaticItems
  | -- | Asked from a language server (by key) for each query, which also
    -- filters them (workspace symbols).
    ServerQuery !Text
  | -- | Lines of the project's files that contain the query, found by a
    -- background search for each query (ADR global-search); shown as they arrive.
    GrepQuery
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
  , piId :: !Int
  -- ^ Its position among the picker's items (set when it is added), so a
  -- mark stays on it while the query changes.
  , piStarts :: Integer
  -- ^ Where words start in the label ('wordStarts'), as bits; lazy, so it is
  -- worked out once, the first time the item is scored. (Lower-casing
  -- rarely changes a text's length; where it does, these are a little off.)
  }
  deriving stock (Eq, Show)

pickerItem :: Text -> PickTarget -> Text -> PickerItem
pickerItem label target detail = PickerItem label target detail key (T.takeWhileEnd (/= '/') key) (T.length label) 0 (wordStarts label)
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
  , pkMarked :: !IntSet
  -- ^ The marked items, by 'piId' (ADR picker-actions).
  , pkMarkable :: !Bool
  -- ^ Whether items can be marked; when not, @tab@ only moves (the buffer
  -- picker: acting on several buffers makes no sense).
  , pkPrimary :: !Text
  -- ^ The action @ret@ runs (by name), on the 'chosenItems'.
  , pkSecondary :: !(Maybe Text)
  -- ^ The action @del@ runs, if the picker has one.
  , pkPreviewScroll :: !(Int, Int)
  -- ^ How many lines the preview is scrolled from where it starts, and the
  -- item ('piId') that is for; another selected item shows from the start.
  , pkColumns :: !(Text, Text)
  -- ^ The names of the label and detail columns, for @%name@ in the query
  -- (ADR picker-query).
  , pkKeepItems :: !Bool
  -- ^ Whether @'@ brings this picker back with its items (they took a scan
  -- or a search to find), rather than listing them afresh with its query
  -- (ADR last-picker).
  }
  deriving stock (Eq, Show)

newPicker :: Text -> [PickerItem] -> Picker
newPicker title items = refilter (Picker title (numbered 0 items) "" [] 0 0 0 False False StaticItems (labelWidth items) IntSet.empty True "picker_open" Nothing (-1, 0) defaultColumns False)

-- | The picker box's height in an area of the given height (most of it).
boxHeight :: Int -> Int
boxHeight h = max 4 (min h (max 8 (h * 4 `div` 5)))

-- | The preview's text rows for a terminal size: the box over all but the
-- last two rows (the status and command lines), less its borders.
previewRows :: (Int, Int) -> Int
previewRows (rows, _) = boxHeight (max 0 (rows - 2)) - 2

-- | The first line the preview shows: a third of the way above the target
-- line, moved by the scroll, and kept between the file's start and the
-- point where its end is in view.
previewTop :: Int -> Int -> Int -> Int -> Int
previewTop rows line count scroll = max 0 (min (max start (count - rows)) (start + scroll))
  where
    start = max 0 (line - rows `div` 3)

-- | The selected item's preview scroll.
previewOffset :: Picker -> Int
previewOffset p = case selectedItem p of
  Just item | piId item == fst (pkPreviewScroll p) -> snd (pkPreviewScroll p)
  _ -> 0

-- | Items with their ids, counting from n: the ids are their positions
-- in 'pkItems', so a picker that replaces its items clears 'pkMarked'.
numbered :: Int -> [PickerItem] -> Seq PickerItem
numbered n items = Seq.fromList (zipWith (\i item -> item {piId = i}) [n ..] items)

-- | The longest label among items.
labelWidth :: [PickerItem] -> Int
labelWidth items = maximum (0 : map piLength items)

-- | Change the query, filter again, and select the best match.
setQuery :: Text -> Picker -> Picker
setQuery q p = refilter p {pkQuery = q, pkSelected = 0}

-- | Items that arrived (from a background scan): filter again, keeping the
-- selection where it is when possible.
addItems :: [PickerItem] -> Picker -> Picker
addItems new p = refilter p {pkItems = pkItems p <> numbered (Seq.length (pkItems p)) new, pkLabelWidth = max (pkLabelWidth p) (labelWidth new)}

refilter :: Picker -> Picker
refilter p =
  let (ms, n) = rankWith (pkColumns p) (pkQuery p) (toList (pkItems p))
   in p {pkMatches = ms, pkMatchCount = n, pkSelected = max 0 (min (pkSelected p) (length ms - 1))}

-- | Pickers with more items than this filter in a background job when the
-- query is not empty (ADR streaming-file-picker); smaller ones filter at once.
syncLimit :: Int
syncLimit = 20000

-- | At most this many matches are kept and shown.
matchLimit :: Int
matchLimit = 1000

-- | The best matches of a query (at most 'matchLimit'), best first. An
-- empty query keeps the original order.
matches :: Text -> [PickerItem] -> [PickerItem]
matches q = fst . rank q

-- | The best matches, and how many items match in all, for a picker with
-- the default column names.
rank :: Text -> [PickerItem] -> ([PickerItem], Int)
rank = rankWith defaultColumns

-- | The best matches, and how many items match in all. The query is
-- words that must all match, in any order (ADR picker-query).
--
-- Fast path for large lists (ADR streaming-file-picker): a cheap in-order character check on
-- the precomputed lower-case key rejects most items before any scoring,
-- and instead of sorting every match the matches are bucketed by their
-- rank (score, exactness, length), which keeps the original order inside
-- a bucket; only the buckets needed for the first 'matchLimit' are read.
rankWith :: (Text, Text) -> Text -> [PickerItem] -> ([PickerItem], Int)
rankWith columns q items
  | null atoms = (take matchLimit items, length items)
  | otherwise =
      let scored = [(bucket s item, item) | item <- items, Just s <- [score item]]
          buckets = IntMap.fromListWith (<>) [(k, [item]) | (k, item) <- scored]
       in (take matchLimit (concatMap reverse (IntMap.elems buckets)), length scored)
  where
    atoms = parseQuery columns q
    score item = sum <$> traverse (atomScore item) atoms
    -- Score, then a label whose first word or file name is the first
    -- word itself (goto_line before goto_line_end), then the shorter label.
    bucket s item =
      s * 2 ^ (21 :: Int)
        + (if exact item then 0 else 2 ^ (20 :: Int))
        + min (2 ^ (20 :: Int) - 1) (piLength item)
    -- The first word is the query: a prefix followed by the end or a space.
    exact item = case [atText a | a <- atoms, atColumn a == Primary, atKind a == Fuzzy, not (atNegated a)] of
      lq : _ ->
        piName item == lq
          || ( lq `T.isPrefixOf` piKey item
                && maybe True ((== ' ') . fst) (T.uncons (T.drop (T.length lq) (piKey item)))
             )
      [] -> False

-- | Which part of an item a query word is matched against.
data Column
  = -- | The label, or else the detail (a label match ranks first).
    Primary
  | LabelColumn
  | DetailColumn
  deriving stock (Eq, Show)

-- | How a word matches, after fzf (and Helix): @word@ fuzzy, @'word@ as a
-- substring, @^word@ at the start, @word$@ at the end, @^word$@ the whole
-- text; @!@ before any of them excludes what matches.
data AtomKind = Fuzzy | Substring | Prefix | Suffix | Whole
  deriving stock (Eq, Show)

-- | A word of a query, lower case.
data Atom = Atom
  { atColumn :: !Column
  , atKind :: !AtomKind
  , atNegated :: !Bool
  , atText :: !Text
  }
  deriving stock (Eq, Show)

-- | A picker's column names when it names none: its label and its detail.
defaultColumns :: (Text, Text)
defaultColumns = ("name", "detail")

-- | Read a query (ADR picker-query): words separated by spaces; @%column@
-- (any prefix of a column's name: @%p@ for @%path@) sends the words after
-- it to that column; a backslash makes the next character plain (@\ @ a
-- space in a word, @\%@, @\!@, @\'@, @\^@, @\$@).
parseQuery :: (Text, Text) -> Text -> [Atom]
parseQuery columns = go Primary . queryWords
  where
    go _ [] = []
    go col (w : ws) = case w of
      ('%', False) : name
        | Just col' <- columnNamed columns (T.pack (map fst name)) -> go col' ws
      _ -> maybe id (:) (atom col w) (go col ws)

-- | The column a (non-empty) prefix names, the label's first.
columnNamed :: (Text, Text) -> Text -> Maybe Column
columnNamed (label, detail) name
  | T.null name = Nothing
  | n `T.isPrefixOf` T.toLower label = Just LabelColumn
  | n `T.isPrefixOf` T.toLower detail = Just DetailColumn
  | otherwise = Nothing
  where
    n = T.toLower name

-- | A word's characters, each with whether a backslash made it plain.
queryWords :: Text -> [[(Char, Bool)]]
queryWords = filter (not . null) . split . T.unpack
  where
    split s = case word s of
      (w, []) -> [w]
      (w, rest) -> w : split rest
    word = \case
      [] -> ([], [])
      '\\' : c : rest -> let (w, r) = word rest in ((c, True) : w, r)
      c : rest
        | c == ' ' || c == '\t' -> ([], rest)
        | otherwise -> let (w, r) = word rest in ((c, False) : w, r)

atom :: Column -> [(Char, Bool)] -> Maybe Atom
atom col w0 =
  let (negated, w1) = case w0 of ('!', False) : r -> (True, r); _ -> (False, w0)
      (kind0, w2) = case w1 of
        ('\'', False) : r -> (Substring, r)
        ('^', False) : r -> (Prefix, r)
        _ -> (Fuzzy, w1)
      (kind, w3) = case reverse w2 of
        ('$', False) : r | kind0 == Prefix -> (Whole, reverse r)
                         | kind0 == Fuzzy -> (Suffix, reverse r)
        _ -> (kind0, w2)
      t = T.toLower (T.pack (map fst w3))
   in if T.null t then Nothing else Just (Atom col kind negated t)

-- | An atom's score for an item (lower is better), or 'Nothing' when it
-- rules the item out. A primary word found only in the detail ranks after
-- every label match.
atomScore :: PickerItem -> Atom -> Maybe Int
atomScore item a = case atColumn a of
  LabelColumn -> verdict inLabel
  DetailColumn -> verdict inDetail
  Primary
    | atNegated a -> verdict (maybe inDetail Just inLabel)
    | otherwise -> maybe ((+ 100000) <$> inDetail) Just inLabel
  where
    verdict m = if atNegated a then maybe (Just 0) (const Nothing) m else m
    inLabel = matchAtom a (piKey item) (piStarts item)
    inDetail = if T.null (piDetail item) then Nothing else matchAtom a (T.toLower (piDetail item)) (wordStarts (piDetail item))

-- | Whether an atom's text matches a lower-case key (with its word starts),
-- and its score.
matchAtom :: Atom -> Text -> Integer -> Maybe Int
matchAtom a key starts = case atKind a of
  Fuzzy
    | inOrder t key -> Just (matchCost (T.unpack t) starts (T.unpack key))
    | otherwise -> Nothing
  Substring -> hit (t `T.isInfixOf` key)
  Prefix -> hit (t `T.isPrefixOf` key)
  Suffix -> hit (t `T.isSuffixOf` key)
  Whole -> hit (t == key)
  where
    t = atText a
    hit b = if b then Just 0 else Nothing

-- | The global search's query (ADR picker-query): the text before the first
-- @%path@ (any prefix of it, after a space or at the start) is the pattern,
-- spaces and all (@\%@ is a plain @%@); the words after it must match the
-- file's path.
grepQuery :: Text -> (Text, [Atom])
grepQuery q = case T.breakOn "%" q of
  (before, rest)
    | T.null rest -> (unescape q, [])
    | escaped before -> let (needle, atoms) = grepQuery (T.drop 1 rest) in (unescape (before <> "%") <> needle, atoms)
    | atBoundary before
    , Just LabelColumn <- columnNamed ("path", "") (T.takeWhile (not . isSpaceChar) (T.drop 1 rest)) ->
        (unescape (T.stripEnd before), parseQuery ("path", "") rest)
    | otherwise -> let (needle, atoms) = grepQuery (T.drop 1 rest) in (unescape (before <> "%") <> needle, atoms)
  where
    escaped before = "\\" `T.isSuffixOf` before
    atBoundary before = T.null before || isSpaceChar (T.last before)
    isSpaceChar c = c == ' ' || c == '\t'
    unescape = T.replace "\\%" "%"

-- | Whether a path matches all of the atoms (the global search's @%path@).
pathMatches :: [Atom] -> Text -> Bool
pathMatches atoms path = all (\a -> atomScore item a /= Nothing) atoms
  where
    item = pickerItem path (PickFile "") ""

-- | Do the query's characters occur in the key, in order?
inOrder :: Text -> Text -> Bool
inOrder q key = case T.uncons q of
  Nothing -> True
  Just (c, rest) -> case T.uncons (T.dropWhile (/= c) key) of
    Just (_, key') -> inOrder rest key'
    Nothing -> False

-- | Lower is better; 'Nothing' if the query's characters do not all occur
-- in order. Case is ignored. The score is 'matchCost'.
fuzzyScore :: Text -> Text -> Maybe Int
fuzzyScore query label
  | inOrder q key = Just (matchCost (T.unpack q) (wordStarts label) (T.unpack key))
  | otherwise = Nothing
  where
    q = T.toLower query
    key = T.toLower label

-- | What a run of matched characters costs when it starts inside a word
-- (ADR picker-query): more than a few skipped characters, so @lp@ prefers
-- @last-picker@ to @help@.
runPenalty :: Int
runPenalty = 8

-- | Where words start in a text, as bits (bit i for character i): the first
-- character, one after @/ - _ .@ or a space, an upper-case letter after a
-- lower-case one, and a digit after a non-digit. One pass, in a machine word
-- for the first 64 characters.
wordStarts :: Text -> Integer
wordStarts t = case T.foldl' step (Starts 0 ' ' 0 0) t of
  Starts _ _ low high -> high .|. toInteger low
  where
    step (Starts i prev low high) c
      | not (start prev c) = Starts (i + 1) c low high
      | i < 64 = Starts (i + 1) c (setBit low i) high
      | otherwise = Starts (i + 1) c low (setBit high i)
    start prev c =
      prev == '/' || prev == '-' || prev == '_' || prev == '.' || prev == ' '
        || (isUpper c && isLower prev)
        || (isDigit c && not (isDigit prev))

data Starts = Starts !Int !Char !Word64 !Integer

-- | For a query known to match in order: the lowest cost of a match, where
-- every character skipped between the first and the last matched one costs
-- 1 and every run of matched characters that does not start a word costs
-- 'runPenalty'. One row per query character over the key (the cheapest way
-- to have matched up to it with it at each position), so a match can pick
-- word starts rather than the first occurrences.
matchCost :: String -> Integer -> String -> Int
matchCost [] _ _ = 0
matchCost [q0] starts key = single 0 key
  where
    single !i = \case
      [] -> runPenalty
      c : rest
        | c == q0 && testBit starts i -> 0
        | otherwise -> single (i + 1) rest
matchCost (q0 : qs) starts key0 = foldl' min inf (foldl' row first qs)
  where
    inf = maxBound `div` 4
    penalty i = if testBit starts i then 0 else runPenalty
    -- Nothing before the first occurrence of the first character can match.
    skipped = length (takeWhile (/= q0) key0)
    key = drop skipped key0
    first = [let !v = if c == q0 then penalty i else inf in v | (i, c) <- zip [skipped ..] key]
    -- At position i: 'before' is the last row's cost at i - 1 (going on
    -- with the run), 'best' the least of its cost at j minus j for
    -- j <= i - 2 (a new run after a gap of i - j - 1).
    row prev q = go skipped inf inf key prev
      where
        go !i !best !before (c : rest) (p : ps) =
          let !here
                | c /= q = inf
                | otherwise = min inf (min before (best + i - 1 + penalty i))
              !best' = if i > skipped then min best (before - (i - 1)) else best
           in here : go (i + 1) best' p rest ps
        go _ _ _ _ _ = []

selectedItem :: Picker -> Maybe PickerItem
selectedItem p = case drop (pkSelected p) (pkMatches p) of
  item : _ -> Just item
  [] -> Nothing

-- | Move the selection, wrapping around.
moveSelection :: Int -> Picker -> Picker
moveSelection n p = case length (pkMatches p) of
  0 -> p
  len -> p {pkSelected = (pkSelected p + n) `mod` len}

-- | Mark the selected item, or unmark it.
toggleMark :: Picker -> Picker
toggleMark p = case selectedItem p of
  Just item
    | not (pkMarkable p) -> p
    | isMarked p item -> p {pkMarked = IntSet.delete (piId item) (pkMarked p)}
    | otherwise -> p {pkMarked = IntSet.insert (piId item) (pkMarked p)}
  Nothing -> p

-- | Unmark the selected item (if it is marked).
unmark :: Picker -> Picker
unmark p = case selectedItem p of
  Just item -> p {pkMarked = IntSet.delete (piId item) (pkMarked p)}
  Nothing -> p

isMarked :: Picker -> PickerItem -> Bool
isMarked p item = IntSet.member (piId item) (pkMarked p)

markCount :: Picker -> Int
markCount = IntSet.size . pkMarked

-- | What the picker's actions act on: the marked items in the order they
-- were listed, or else the selected one.
chosenItems :: Picker -> [PickerItem]
chosenItems p
  | IntSet.null (pkMarked p) = maybe [] pure (selectedItem p)
  | otherwise = [item | i <- IntSet.toAscList (pkMarked p), Just item <- [Seq.lookup i (pkItems p)]]
