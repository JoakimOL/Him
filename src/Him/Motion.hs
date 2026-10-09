-- | Pure motions. A motion maps the current range to a new one; how the
-- result is applied ('Move' or 'Extend') is decided by the caller, usually
-- from the editor mode.
module Him.Motion
  ( Motion
  , Movement (..)
  , applyMotion
  , charLeft
  , charRight
  , lineUp
  , lineDown
  , lineBy
  , lineStart
  , lineEnd
  , fileStart
  , lastLine
  , gotoLine
  , nextWordStart
  , nextWordEnd
  , prevWordStart
  , selectLine
  , extendToLineBounds
  , findChar
    -- * Whole selections
  , selectAll
  , copySelectionBelow
  , splitOnNewlines
  ) where

import Data.Char (isAlphaNum, isSpace)
import Data.Maybe (fromMaybe, listToMaybe, mapMaybe)
import Data.Text qualified as T
import Him.Buffer
import Him.Position (Pos (..))
import Him.Selection
import Him.TextWidth (charIndexAtCol, displayCol)

type Motion = Buffer -> Range -> Range

-- | 'Move' uses the range the motion produced; 'Extend' keeps the old anchor
-- and only takes the new head (select mode).
data Movement = Move | Extend
  deriving stock (Eq, Show)

applyMotion :: Movement -> Motion -> Buffer -> Range -> Range
applyMotion Move m b r = m b r
applyMotion Extend m b r = (m b r) {rangeAnchor = rangeAnchor r}

headMotion :: (Buffer -> Pos -> Pos) -> Motion
headMotion f b r = point (f b (rangeHead r))

charLeft, charRight :: Motion
charLeft = headMotion prevPos
charRight = headMotion nextPos

-- | Up and down with the default tab width (4).
lineUp, lineDown :: Motion
lineUp = lineBy 4 (-1)
lineDown = lineBy 4 1

-- | Vertical movement that remembers the /display/ column it started from,
-- so it stays visually aligned across tabs (every @tabWidth@ columns) and
-- wide characters.
lineBy :: Int -> Int -> Motion
lineBy tabWidth delta b r = Range p p (Just want)
  where
    Pos l c = rangeHead r
    want = fromMaybe (displayCol tabWidth (lineAt l b) c) (rangeWantCol r)
    l' = max 0 (min (lineCount b - 1) (l + delta))
    p = Pos l' (charIndexAtCol tabWidth (lineAt l' b) want)

lineStart, lineEnd, fileStart, lastLine :: Motion
lineStart _ r = point (Pos (posLine (rangeHead r)) 0)
lineEnd b r = let l = posLine (rangeHead r) in point (Pos l (max 0 (lineLength l b - 1)))
fileStart _ _ = point (Pos 0 0)
lastLine b _ = point (Pos (lineCount b - 1) 0)

-- | Go to a line, counting from 1 like the gutter. Out-of-range numbers go
-- to the first or last line.
gotoLine :: Int -> Motion
gotoLine n b _ = point (Pos (max 0 (min (lineCount b - 1) (n - 1))) 0)

data CharClass = Blank | LineEnd | WordChar | Punct
  deriving stock (Eq)

classAt :: Buffer -> Pos -> CharClass
classAt b p = case charAt p b of
  Nothing -> LineEnd
  Just '\n' -> LineEnd
  Just c
    | isSpace c -> Blank
    | isAlphaNum c || c == '_' -> WordChar
    | otherwise -> Punct

-- | If the head sits on the last character of a word (the next character is
-- of a different class), a word motion starts from the next character.
-- This is what makes repeated @w@ select successive words.
stepOffBoundary :: (Buffer -> Pos -> Pos) -> Buffer -> Pos -> Pos
stepOffBoundary step b p
  | classAt b (step b p) /= classAt b p = step b p
  | otherwise = p

-- | Advance while the /next/ position satisfies the predicate; returns the
-- last position that did.
extendWhile :: (Buffer -> Pos -> Pos) -> (CharClass -> Bool) -> Buffer -> Pos -> Pos
extendWhile step ok b = go
  where
    go p
      | p' /= p && ok (classAt b p') = go p'
      | otherwise = p
      where
        p' = step b p

-- | Advance while the /current/ position satisfies the predicate; returns
-- the first position that does not (or where movement stops).
skipWhile :: (Buffer -> Pos -> Pos) -> (CharClass -> Bool) -> Buffer -> Pos -> Pos
skipWhile step ok b = go
  where
    go p
      | ok (classAt b p) && step b p /= p = go (step b p)
      | otherwise = p

isGap :: CharClass -> Bool
isGap c = c == Blank || c == LineEnd

-- | @w@: select the next word and the blanks after it.
nextWordStart :: Motion
nextWordStart b r = Range start end Nothing
  where
    start = skipWhile nextPos (== LineEnd) b (stepOffBoundary nextPos b (rangeHead r))
    cls = classAt b start
    wordEnd = extendWhile nextPos (== cls) b start
    end = extendWhile nextPos (== Blank) b wordEnd

-- | @e@: select up to the end of the next word.
nextWordEnd :: Motion
nextWordEnd b r = Range start end Nothing
  where
    start = stepOffBoundary nextPos b (rangeHead r)
    wordStart = skipWhile nextPos isGap b start
    end = extendWhile nextPos (== classAt b wordStart) b wordStart

-- | @b@: select back to the start of the previous word.
prevWordStart :: Motion
prevWordStart b r = Range start end Nothing
  where
    start = stepOffBoundary prevPos b (rangeHead r)
    wordLast = skipWhile prevPos isGap b start
    end = extendWhile prevPos (== classAt b wordLast) b wordLast

-- | @x@: select the whole lines the range touches, including the last
-- line's end. If the range already covers whole lines, extend it by one
-- more line. The range keeps its direction.
selectLine :: Motion
selectLine b r
  | coversLines b r = lineBounds b r (min (lineCount b - 1) (el + 1))
  | otherwise = lineBounds b r el
  where
    Pos el _ = rangeEnd r

-- | @X@: select the whole lines the range touches, never adding one.
extendToLineBounds :: Motion
extendToLineBounds b r = lineBounds b r (posLine (rangeEnd r))

-- | Whether the range runs from a line's start to a line's end.
coversLines :: Buffer -> Range -> Bool
coversLines b r = sc == 0 && rangeEnd r == Pos el (lineLength el b)
  where
    Pos _ sc = rangeStart r
    Pos el _ = rangeEnd r

-- | From the start of the range's first line to the end of line @el@,
-- facing the way the range faced.
lineBounds :: Buffer -> Range -> Int -> Range
lineBounds b r el
  | rangeHead r < rangeAnchor r = Range end start Nothing
  | otherwise = Range start end Nothing
  where
    start = Pos (posLine (rangeStart r)) 0
    end = Pos el (lineLength el b)

-- | One range over the whole buffer.
selectAll :: Buffer -> Selection
selectAll b = single (Range (Pos 0 0) (endPos b) Nothing)

-- | Helix @C@: copy every range onto the next lines where it fits (the
-- same columns, as many lines down as the range is tall). The copy of the
-- primary range becomes primary.
copySelectionBelow :: Buffer -> Selection -> Selection
copySelectionBelow b sel = fromMaybe sel (fromRanges (rs <> map snd copies) prim)
  where
    rs = ranges sel
    copies = mapMaybe (\(i, r) -> (i,) <$> copyBelow b r) (zip [0 ..] rs)
    prim = maybe (primaryIndex sel) (length rs +) (lookup (primaryIndex sel) (zip (map fst copies) [0 ..]))

copyBelow :: Buffer -> Range -> Maybe Range
copyBelow b r = listToMaybe [moved d | d <- [height .. lineCount b - 1 - posLine (rangeEnd r)], fits d]
  where
    height = posLine (rangeEnd r) - posLine (rangeStart r) + 1
    shift d (Pos l c) = Pos (l + d) c
    moved d = Range (shift d (rangeAnchor r)) (shift d (rangeHead r)) Nothing
    -- Each end must land on a character (or on an empty line's start).
    fits d = all (\(Pos l c) -> c < max 1 (lineLength (l + d) b)) [rangeAnchor r, rangeHead r]

-- | Helix @A-s@: split every range into one range per line, without the
-- line breaks. Empty pieces are dropped (a range with only empty pieces
-- stays as it was).
splitOnNewlines :: Buffer -> Selection -> Selection
splitOnNewlines b sel = fromMaybe sel (fromRanges (concat pieces) prim)
  where
    pieces = [orSelf r (split r) | r <- ranges sel]
    orSelf r [] = [r]
    orSelf _ ps = ps
    split r =
      let Pos sl sc = rangeStart r
          Pos el ec = rangeEnd r
       in [ Range (Pos l s) (Pos l e) Nothing
          | l <- [sl .. el]
          , let s = if l == sl then sc else 0
                e = min (if l == el then ec else maxBound) (lineLength l b - 1)
          , e >= s
          ]
    -- The first piece of the old primary range becomes primary.
    prim = sum (map length (take (primaryIndex sel) pieces))

-- | Helix @f t F T@: select from the cursor to the @n@th next (or
-- previous) occurrence of a character, or up to just before it ("till").
-- The search crosses lines; a line's end counts as a @\\n@. Not found: no
-- change. When repeating (@skipAdjacent@), a "till" target right next to
-- the cursor is skipped, so the repeat moves on (Vim's @;@).
findChar :: Bool -> Bool -> Bool -> Char -> Int -> Motion
findChar skipAdjacent forward till ch n b r = case (if forward then forwardHits else backwardHits) of
  hits | (target : _) <- drop (max 1 n - 1) (filter useful hits) -> Range h (adjust target) Nothing
  _ -> r
  where
    h = rangeHead r
    -- Positions of the character after (before) the cursor, nearest first.
    forwardHits =
      [ Pos l c
      | l <- [posLine h .. lineCount b - 1]
      , let text = lineAt l b <> "\n"
            from = if l == posLine h then posCol h + 1 else 0
      , c <- [i + from | i <- indicesOf (T.drop from text)]
      ]
    backwardHits =
      [ Pos l c
      | l <- [posLine h, posLine h - 1 .. 0]
      , let text = lineAt l b <> "\n"
            upto = if l == posLine h then posCol h else T.length text
      , c <- reverse (indicesOf (T.take upto text))
      ]
    indicesOf t = [i | (i, c) <- zip [0 ..] (T.unpack t), c == ch]
    adjust p
      | till && forward = prevPos b p
      | till = nextPos b p
      | otherwise = p
    useful p = not (skipAdjacent && till) || adjust p /= h
