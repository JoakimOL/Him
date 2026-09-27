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
  , lineStart
  , lineEnd
  , fileStart
  , lastLine
  , nextWordStart
  , nextWordEnd
  , prevWordStart
  , selectLine
  ) where

import Data.Char (isAlphaNum, isSpace)
import Data.Maybe (fromMaybe)
import Him.Buffer
import Him.Position (Pos (..))
import Him.Selection

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

lineUp, lineDown :: Motion
lineUp = lineBy (-1)
lineDown = lineBy 1

-- | Vertical movement that remembers the column it started from.
lineBy :: Int -> Motion
lineBy delta b r = Range p p (Just want)
  where
    Pos l c = rangeHead r
    want = fromMaybe c (rangeWantCol r)
    l' = max 0 (min (lineCount b - 1) (l + delta))
    p = Pos l' (min want (lineLength l' b))

lineStart, lineEnd, fileStart, lastLine :: Motion
lineStart _ r = point (Pos (posLine (rangeHead r)) 0)
lineEnd b r = let l = posLine (rangeHead r) in point (Pos l (max 0 (lineLength l b - 1)))
fileStart _ _ = point (Pos 0 0)
lastLine b _ = point (Pos (lineCount b - 1) 0)

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

-- | @x@: select the whole line including its line end. If the range already
-- covers whole lines, extend it by one more line.
selectLine :: Motion
selectLine b r
  | coversLines = Range (Pos sl 0) (lineEndOf (min (lineCount b - 1) (el + 1))) Nothing
  | otherwise = Range (Pos sl 0) (lineEndOf el) Nothing
  where
    Pos sl sc = rangeStart r
    e@(Pos el _) = rangeEnd r
    lineEndOf l = Pos l (lineLength l b)
    coversLines = sc == 0 && e == lineEndOf el
