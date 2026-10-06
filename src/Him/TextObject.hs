-- | Helix's match mode (@m@), pure (ADR match-mode): text objects (@m i w@,
-- @m a (@), the pair around a position, and the bracket matching one.
-- Positions are character positions; ranges are inclusive, like
-- selections.
module Him.TextObject
  ( textObject
  , surroundingPair
  , matchingBracket
  , pairFor
  ) where

import Data.Char (isAlphaNum, isSpace)
import Data.List (find, sortOn)
import Data.Maybe (mapMaybe)
import Data.Ord (Down (..))
import Data.Text qualified as T
import Him.Buffer (Buffer, charAt, lineAt, lineCount, lineLength)
import Him.Position (Pos (..))
import Him.Selection (Range (..))

-- | The open and close characters for a key: either bracket of a pair
-- names the pair; any other character is its own pair (a quote, @*@).
pairFor :: Char -> (Char, Char)
pairFor c = case find (\(o, cl) -> c == o || c == cl) brackets of
  Just p -> p
  Nothing -> (c, c)

brackets :: [(Char, Char)]
brackets = [('(', ')'), ('[', ']'), ('{', '}'), ('<', '>')]

quotes :: [Char]
quotes = ['"', '\'', '`']

-- | The range a text object covers around a position: @inside@ (@m i@) or
-- around (@m a@). Objects: @w@ a word, @W@ a WORD (non-blanks), @p@ a
-- paragraph, @m@ the closest pair, or a pair's character.
textObject :: Bool -> Char -> Buffer -> Pos -> Maybe Range
textObject inside obj buf pos = case obj of
  'w' -> word (\x -> isAlphaNum x || x == '_')
  'W' -> word (not . isSpace)
  'p' -> paragraph
  _ -> do
    (open, close) <- if obj == 'm' then closestPair buf pos else surroundingPair (pairFor obj) buf pos
    if inside
      then
        let from = after open
            to = before close
         in if from > to then Nothing else Just (Range from to Nothing)
      else Just (Range open close Nothing)
  where
    Pos l c = pos
    line = lineAt l buf
    -- The column just past a line's last character is its line break.
    after (Pos pl pc) = Pos pl (pc + 1)
    before (Pos pl pc)
      | pc > 0 = Pos pl (pc - 1)
      | pl > 0 = Pos (pl - 1) (lineLength (pl - 1) buf)
      | otherwise = Pos 0 0
    -- A word: the run of word characters (or of other non-blanks, or of
    -- blanks) the position is in; around adds the blanks after it (or
    -- before it, at the end of a line).
    word isWord = do
      ch <- if T.null line then Nothing else Just (T.index line (min c (T.length line - 1)))
      let same x = classOf x == classOf ch
          classOf x
            | isSpace x = 0 :: Int
            | isWord x = 1
            | otherwise = 2
          start = c - T.length (T.takeWhileEnd same (T.take c line))
          end = c + T.length (T.takeWhile same (T.drop c line)) - 1
          trailing = T.length (T.takeWhile isSpace (T.drop (end + 1) line))
          leading = T.length (T.takeWhileEnd isSpace (T.take start line))
      pure $
        if inside || isSpace ch
          then Range (Pos l start) (Pos l end) Nothing
          else
            if trailing > 0
              then Range (Pos l start) (Pos l (end + trailing)) Nothing
              else Range (Pos l (start - leading)) (Pos l end) Nothing
    -- A paragraph: the non-blank lines around the position (or the blank
    -- ones, on a blank line); around adds the blank lines after it.
    paragraph =
      let blank i = T.all isSpace (lineAt i buf)
          kind = blank l
          up = last (l : takeWhile (\i -> blank i == kind) [l - 1, l - 2 .. 0])
          down = last (l : takeWhile (\i -> blank i == kind) [l + 1 .. lineCount buf - 1])
          down' = if inside || kind then down else last (down : takeWhile blank [down + 1 .. lineCount buf - 1])
       in Just (Range (Pos up 0) (Pos down' (max 0 (lineLength down' buf))) Nothing)

-- | The positions of the pair around a position (the position may be on
-- either of them). Brackets nest; quotes pair up within a line, in order.
surroundingPair :: (Char, Char) -> Buffer -> Pos -> Maybe (Pos, Pos)
surroundingPair (open, close) buf pos
  | open == close = quotePair
  | otherwise = do
      o <- if charAt pos buf == Just open then Just pos else scanBack (0 :: Int) (backward buf pos)
      cl <- scanForward (0 :: Int) (forward buf o)
      if cl >= pos then Just (o, cl) else Nothing
  where
    scanBack _ [] = Nothing
    scanBack depth ((p, ch) : rest)
      | ch == open = if depth == 0 then Just p else scanBack (depth - 1) rest
      | ch == close && p /= pos = scanBack (depth + 1) rest
      | otherwise = scanBack depth rest
    scanForward _ [] = Nothing
    scanForward depth ((p, ch) : rest)
      | ch == close = if depth == 0 then Just p else scanForward (depth - 1) rest
      | ch == open = scanForward (depth + 1) rest
      | otherwise = scanForward depth rest
    Pos l c = pos
    quotePair =
      let cols = [i | (i, ch) <- zip [0 ..] (T.unpack (lineAt l buf)), ch == open]
          pairs = pairUp cols
       in case [(Pos l a, Pos l b) | (a, b) <- pairs, a <= c, c <= b] of
            p : _ -> Just p
            [] -> Nothing
    pairUp (a : b : rest) = (a, b) : pairUp rest
    pairUp _ = []

-- | The innermost pair of any kind around a position (@m i m@).
closestPair :: Buffer -> Pos -> Maybe (Pos, Pos)
closestPair buf pos =
  case sortOn (Down . fst) (mapMaybe (\p -> surroundingPair p buf pos) (brackets <> [(q, q) | q <- quotes])) of
    p : _ -> Just p
    [] -> Nothing

-- | The bracket matching the one at a position (@m m@).
matchingBracket :: Buffer -> Pos -> Maybe Pos
matchingBracket buf pos = do
  ch <- charAt pos buf
  (open, close) <- find (\(o, cl) -> ch == o || ch == cl) brackets
  (o, cl) <- surroundingPair (open, close) buf pos
  pure (if ch == open then cl else o)

-- | The characters before a position, nearest first, with line breaks.
backward :: Buffer -> Pos -> [(Pos, Char)]
backward buf (Pos l c) = concat [chars i (if i == l then c - 1 else lineLength i buf) | i <- [l, l - 1 .. 0]]
  where
    chars i from =
      let t = lineAt i buf
       in [(Pos i k, T.index t k) | k <- [min from (T.length t - 1), min from (T.length t - 1) - 1 .. 0]]
            <> [(Pos (i - 1) (lineLength (i - 1) buf), '\n') | i > 0]

-- | The characters after a position, nearest first.
forward :: Buffer -> Pos -> [(Pos, Char)]
forward buf (Pos l c) = concat [chars i (if i == l then c + 1 else 0) | i <- [l .. lineCount buf - 1]]
  where
    chars i from = let t = lineAt i buf in [(Pos i k, T.index t k) | k <- [from .. T.length t - 1]]
