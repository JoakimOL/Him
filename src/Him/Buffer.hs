-- | Text storage. The representation is hidden behind this interface so it
-- can later be swapped for a rope without touching callers.
module Him.Buffer
  ( Buffer
  , empty
  , fromText
  , fromLines
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
  , textRange
  ) where

import Data.Foldable (toList)
import Data.Maybe (fromMaybe)
import Data.Sequence (Seq)
import Data.Sequence qualified as Seq
import Data.Text (Text)
import Data.Text qualified as T
import Him.Position (Pos (..))

-- | Lines of text without their terminators.
-- Invariant: there is always at least one line.
newtype Buffer = Buffer (Seq Text)
  deriving stock (Eq, Show)

empty :: Buffer
empty = Buffer (Seq.singleton T.empty)

fromLines :: [Text] -> Buffer
fromLines [] = empty
fromLines ls = Buffer (Seq.fromList ls)

-- | Split on @\\n@; a trailing newline produces a final empty line.
fromText :: Text -> Buffer
fromText = fromLines . T.splitOn "\n"

toLines :: Buffer -> [Text]
toLines (Buffer ls) = toList ls

toText :: Buffer -> Text
toText = T.intercalate "\n" . toLines

lineCount :: Buffer -> Int
lineCount (Buffer ls) = Seq.length ls

-- | The line at an index, or empty if out of range.
lineAt :: Int -> Buffer -> Text
lineAt i (Buffer ls) = fromMaybe T.empty (Seq.lookup i ls)

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

-- | Insert text (which may contain newlines) before the given position.
-- Returns the new buffer and the position just after the inserted text.
insertText :: Pos -> Text -> Buffer -> (Buffer, Pos)
insertText pos t b@(Buffer ls) =
  (Buffer (Seq.take l ls <> Seq.fromList newLines <> Seq.drop (l + 1) ls), Pos (l + breaks) endCol)
  where
    Pos l c = clampPos b pos
    (before, after) = T.splitAt c (lineAt l b)
    newLines = T.splitOn "\n" (before <> t <> after)
    breaks = T.count "\n" t
    endCol
      | breaks == 0 = c + T.length t
      | otherwise = T.length (T.takeWhileEnd (/= '\n') t)

-- | Delete the half-open range between two positions (in either order).
deleteRange :: Pos -> Pos -> Buffer -> Buffer
deleteRange p1 p2 b@(Buffer ls)
  | from >= to = b
  | otherwise = Buffer ((Seq.take l1 ls Seq.|> merged) <> Seq.drop (l2 + 1) ls)
  where
    from@(Pos l1 c1) = clampPos b (min p1 p2)
    to@(Pos l2 c2) = clampPos b (max p1 p2)
    merged = T.take c1 (lineAt l1 b) <> T.drop c2 (lineAt l2 b)

-- | The text in the half-open range between two positions.
textRange :: Pos -> Pos -> Buffer -> Text
textRange p1 p2 b
  | l1 == l2 = T.take (c2 - c1) (T.drop c1 (lineAt l1 b))
  | otherwise =
      T.intercalate "\n" $
        [T.drop c1 (lineAt l1 b)] <> [lineAt i b | i <- [l1 + 1 .. l2 - 1]] <> [T.take c2 (lineAt l2 b)]
  where
    Pos l1 c1 = clampPos b (min p1 p2)
    Pos l2 c2 = clampPos b (max p1 p2)
