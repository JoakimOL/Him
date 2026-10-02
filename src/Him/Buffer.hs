-- | Text storage. The representation ("Him.Buffer.Rope": a balanced tree
-- of multi-line blocks) is hidden behind this interface.
module Him.Buffer
  ( Buffer
  , empty
  , fromText
  , fromLines
  , fromRegions
  , linesFrom
  , linesDownFrom
  , toLines
  , toText
  , lineCount
  , lineAt
  , lineLength
  , endPos
  , clampPos
  , charAt
  , nextPos
  , prevPos
  , insertText
  , deleteRange
  , replaceLines
  , textRange
  , Region (..)
  , regions
  , findForwardFrom
  , findBackwardBefore
  , changeBetween
  ) where

import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Unsafe (dropWord8, lengthWord8, takeWord8)
import Him.Buffer.Rope
import Him.Position (Pos (..))

-- | Lines of text without their terminators.
-- Invariant: there is always at least one line.
newtype Buffer = Buffer Rope

instance Eq Buffer where
  a == b = lineCount a == lineCount b && toLines a == toLines b

instance Show Buffer where
  show b = "fromLines " <> show (toLines b)

empty :: Buffer
empty = fromText T.empty

fromLines :: [Text] -> Buffer
fromLines [] = empty
fromLines ls = fromText (T.intercalate "\n" ls)

-- | Split on @\\n@; a trailing newline produces a final empty line. The
-- lines share the text's memory.
fromText :: Text -> Buffer
fromText t = Buffer (ropeFromBlocks [blockFromText False t])

-- | From regions of whole lines, each one text with its lines joined by
-- line breaks (as read from a file). With the flag set, a @\\r@ before each
-- line break is not part of the line (CRLF). Nothing is copied.
fromRegions :: [(Bool, Text)] -> Buffer
fromRegions rs = case ropeFromBlocks [blockFromText cr t | (cr, t) <- rs] of
  r | ropeLines r == 0 -> empty
  r -> Buffer r

-- | Lines from an index to the end, lazily.
linesFrom :: Int -> Buffer -> [Text]
linesFrom i (Buffer r) = ropeLinesFrom i r

-- | Lines from an index down to the first, lazily.
linesDownFrom :: Int -> Buffer -> [Text]
linesDownFrom i (Buffer r) = ropeLinesDownFrom i r

toLines :: Buffer -> [Text]
toLines = linesFrom 0

toText :: Buffer -> Text
toText = T.intercalate "\n" . toLines

lineCount :: Buffer -> Int
lineCount (Buffer r) = ropeLines r

-- | The line at an index, or empty if out of range.
lineAt :: Int -> Buffer -> Text
lineAt i (Buffer r) = fromMaybe T.empty (ropeLineAt i r)

lineLength :: Int -> Buffer -> Int
lineLength i = T.length . lineAt i

-- | The position just past the last character.
endPos :: Buffer -> Pos
endPos b = let l = lineCount b - 1 in Pos l (lineLength l b)

-- | The nearest valid position.
clampPos :: Buffer -> Pos -> Pos
clampPos b (Pos l c) = Pos l' (clamp 0 (lineLength l' b) c)
  where
    l' = clamp 0 (lineCount b - 1) l

clamp :: Int -> Int -> Int -> Int
clamp lo hi = max lo . min hi

-- | The character at a position. A line end reads as @\\n@; the end of the
-- file has no character.
charAt :: Pos -> Buffer -> Maybe Char
charAt p b
  | p' >= endPos b = Nothing
  | posCol p' >= lineLength (posLine p') b = Just '\n'
  | otherwise = Just (T.index (lineAt (posLine p') b) (posCol p'))
  where
    p' = clampPos b p

-- | The next position in document order (crossing line ends). Stays put at
-- the end of the file.
nextPos :: Buffer -> Pos -> Pos
nextPos b p
  | c < lineLength l b = Pos l (c + 1)
  | l + 1 < lineCount b = Pos (l + 1) 0
  | otherwise = Pos l c
  where
    Pos l c = clampPos b p

-- | The previous position in document order. Stays put at the start.
prevPos :: Buffer -> Pos -> Pos
prevPos b p
  | c > 0 = Pos l (c - 1)
  | l > 0 = Pos (l - 1) (lineLength (l - 1) b)
  | otherwise = Pos 0 0
  where
    Pos l c = clampPos b p

-- | Replace lines @[from, to)@ with new lines.
replaceLines :: Int -> Int -> [Text] -> Buffer -> Buffer
replaceLines from to new (Buffer r) =
  case ropeAppend before (ropeAppend middle after) of
    r' | ropeLines r' == 0 -> empty
    r' -> Buffer r'
  where
    (before, rest) = ropeSplitAt from r
    (_, after) = ropeSplitAt (to - from) rest
    middle = case new of
      [] -> ropeFromBlocks []
      _ -> ropeFromBlocks [blockFromText False (T.intercalate "\n" new)]

-- | Insert text (which may contain newlines) before the given position.
-- Returns the new buffer and the position just after the inserted text.
insertText :: Pos -> Text -> Buffer -> (Buffer, Pos)
insertText pos t b = (replaceLines l (l + 1) [before <> t <> after] b, Pos (l + breaks) endCol)
  where
    Pos l c = clampPos b pos
    (before, after) = T.splitAt c (lineAt l b)
    breaks = T.count "\n" t
    endCol
      | breaks == 0 = c + T.length t
      | otherwise = T.length (T.takeWhileEnd (/= '\n') t)

-- | Delete the half-open range between two positions (in either order).
deleteRange :: Pos -> Pos -> Buffer -> Buffer
deleteRange p1 p2 b
  | from >= to = b
  | otherwise = replaceLines l1 (l2 + 1) [T.take c1 (lineAt l1 b) <> T.drop c2 (lineAt l2 b)] b
  where
    from@(Pos l1 c1) = clampPos b (min p1 p2)
    to@(Pos l2 c2) = clampPos b (max p1 p2)

-- | The text in the half-open range between two positions.
textRange :: Pos -> Pos -> Buffer -> Text
textRange p1 p2 b
  | l1 == l2 = T.take (c2 - c1) (T.drop c1 (lineAt l1 b))
  | otherwise =
      T.intercalate "\n" $
        [T.drop c1 (lineAt l1 b)] <> take (l2 - l1 - 1) (linesFrom (l1 + 1) b) <> [T.take c2 (lineAt l2 b)]
  where
    Pos l1 c1 = clampPos b (min p1 p2)
    Pos l2 c2 = clampPos b (max p1 p2)

-- | A run of lines stored contiguously: the bytes as they are in memory
-- (lines joined by their original line breaks, CRLF when 'regionCR'; no
-- trailing @\\r@), and the same lines one by one.
data Region = Region
  { regionCR :: !Bool
  , regionText :: !Text
  , regionLines :: [Text]
  }

-- | The buffer's storage regions, in order. Saving writes a region whose line
-- endings match the file's in one piece instead of line by line.
regions :: Buffer -> [Region]
regions (Buffer r) =
  [ Region cr (if cr then dropCR text else text) [blockLine b k | k <- [0 .. blockLines b - 1]]
  | (_, b) <- ropeBlocksFrom 0 r
  , let cr = blockCR b
        text = fst (blockRegion b)
  ]
  where
    -- The region ends just before its last line's newline, so a CRLF block
    -- would end with that line's @\\r@.
    dropCR t = if "\r" `T.isSuffixOf` t then T.dropEnd 1 t else t

-- Search ---------------------------------------------------------------------
--
-- Whole blocks are handed to the matcher, so an unedited file is scanned
-- with a few calls instead of one per line. Matches never span lines,
-- because needles never contain line breaks.

-- | The first match starting at or after a position. The matcher returns
-- the byte offset of the first match in a text.
findForwardFrom :: (Text -> Maybe Int) -> Pos -> Buffer -> Maybe Pos
findForwardFrom match p0 buf@(Buffer r) = go True (ropeBlocksFrom l r)
  where
    Pos l c = clampPos buf p0
    skip = lengthWord8 (T.take c (lineAt l buf))
    go _ [] = Nothing
    go isFirst ((firstLine, b) : rest) =
      let (region, base) = blockRegion b
          extra = if isFirst then skip else 0
       in case match (dropWord8 extra region) of
            Nothing -> go False rest
            Just off -> Just (posOfOffset firstLine b (base + extra + off))

-- | The last match starting before a position. The matcher returns the
-- byte offset of the last match in a text; @needleBytes@ is the needle's
-- length, so a match may extend past the position.
findBackwardBefore :: (Text -> Maybe Int) -> Int -> Pos -> Buffer -> Maybe Pos
findBackwardBefore match needleBytes p0 buf@(Buffer r) = go True (ropeBlocksDownFrom l r)
  where
    Pos l c = clampPos buf p0
    colBytes = lengthWord8 (T.take c (lineAt l buf))
    go _ [] = Nothing
    go isLast ((firstLine, b) : rest) =
      let (region, base) = blockRegion b
          -- In the block holding the position, only matches starting
          -- before it count.
          limit
            | isLast = blockLineStart b (l - firstLine) - base + colBytes + needleBytes - 1
            | otherwise = lengthWord8 region
       in case match (takeWord8 (min limit (lengthWord8 region)) region) of
            Nothing -> go False rest
            Just off -> Just (posOfOffset firstLine b (base + off))

-- | The position of a byte offset (into the block's text).
posOfOffset :: Int -> Block -> Int -> Pos
posOfOffset firstLine b off = Pos (firstLine + k) (T.length (takeWord8 (off - blockLineStart b k) (blockLine b k)))
  where
    k = blockLineOfOffset b off

-- | One edit that turns the first text into the second: the range of the
-- old text (inclusive start, exclusive end, as positions in the old text)
-- and its replacement. 'Nothing' when the texts are equal.
--
-- Lines the two share at the start and at the end are skipped a storage
-- block at a time where the blocks are equal (unchanged blocks are the
-- same text, so this is a memory comparison), then line by line; the
-- remaining lines are trimmed character by character. So a small edit in
-- a large file costs little more than the edited lines.
changeBetween :: Buffer -> Buffer -> Maybe (Pos, Pos, Text)
changeBetween old@(Buffer a) new@(Buffer b)
  | prefix == na && na == nb = Nothing
  | otherwise =
      let -- One shared line before the change keeps the window non-empty
          -- (an insertion at the very end has a line to attach to).
          from = if prefix > 0 then prefix - 1 else 0
          window buf n = window' (take (n - suffix - from) (linesFrom from buf))
          window' ls = if suffix > 0 then T.concat (map (<> "\n") ls) else T.intercalate "\n" ls
          oldW = window old na
          newW = window new nb
          common = maybe 0 (\(p, _, _) -> T.length p) (T.commonPrefixes oldW newW)
          maxSuffix = min (T.length oldW) (T.length newW) - common
          tailCommon = length (takeWhile id (take maxSuffix (zipWith (==) (T.unpack (T.reverse oldW)) (T.unpack (T.reverse newW)))))
          start = advance (Pos from 0) (T.take common oldW)
          end = advance (Pos from 0) (T.take (T.length oldW - tailCommon) oldW)
          text = T.take (T.length newW - common - tailCommon) (T.drop common newW)
       in if start == end && T.null text then Nothing else Just (start, end, text)
  where
    na = ropeLines a
    nb = ropeLines b
    prefix = min na nb `min` commonPrefixLines
    suffix = min (min na nb - prefix) commonSuffixLines
    commonPrefixLines = goP (ropeBlocksFrom 0 a) (ropeBlocksFrom 0 b) 0
    goP ((sa, ba) : ra) ((sb, bb) : rb) acc
      | sa == acc && sb == acc && sameBlock ba bb = goP ra rb (acc + blockLines ba)
    goP _ _ acc = acc + length (takeWhile id (zipWith (==) (ropeLinesFrom acc a) (ropeLinesFrom acc b)))
    -- From the end: a block matches when it ends equally far from the end.
    commonSuffixLines = goS (ropeBlocksDownFrom (na - 1) a) (ropeBlocksDownFrom (nb - 1) b) 0
    goS ((sa, ba) : ra) ((sb, bb) : rb) acc
      | na - (sa + blockLines ba) == acc && nb - (sb + blockLines bb) == acc && sameBlock ba bb = goS ra rb (acc + blockLines ba)
    goS _ _ acc = acc + length (takeWhile id (zipWith (==) (ropeLinesDownFrom (na - 1 - acc) a) (ropeLinesDownFrom (nb - 1 - acc) b)))
    sameBlock x y = blockLines x == blockLines y && blockCR x == blockCR y && fst (blockRegion x) == fst (blockRegion y)
    advance (Pos l c) t = case T.splitOn "\n" t of
      [single] -> Pos l (c + T.length single)
      parts -> Pos (l + length parts - 1) (T.length (last parts))
