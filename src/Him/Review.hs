-- | Reviewing the chat's proposed changes (ADR-43), pure: approving or
-- denying one change, finding the change at a line, and the rows a text
-- area shows for a document under review - its lines, with the removed
-- lines of each change and a header above it.
module Him.Review
  ( approveHunk
  , approveOnto
  , denyHunk
  , hunkAtLine
  , hunkLabel
  , DisplayRow (..)
  , displayRows
  , rowOfLine
  , addedLines
  ) where

import Data.List (find)
import Data.Text (Text)
import Data.Text qualified as T
import Him.Chat (Review (..))
import Him.Diff (Hunk (..), applyHunks, diffLines, mapLine)

-- | The base with one change applied (what is written when it is approved).
approveHunk :: Hunk -> [Text] -> [Text] -> [Text]
approveHunk h base current = applyHunks [h] base current

-- | The file's text with one change applied, when the file differs from the
-- base: the user changed the buffer by hand (unsaved) before the chat's
-- first change. The change is moved onto the file's text past the user's
-- own edits, which stay unsaved in the buffer. 'Nothing' when the change
-- overlaps one of them: then which lines are whose is not clear.
approveOnto :: Hunk -> [Text] -> [Text] -> [Text] -> Maybe [Text]
approveOnto h base current disk
  | disk == base = Just (approveHunk h base current)
  | any touches own = Nothing
  | otherwise = Just (take start disk <> added <> drop (start + hOldCount h) disk)
  where
    -- The user's own edits: base to file (old side: base lines).
    own = diffLines base disk
    start = mapLine own (hOldStart h)
    added = take (hNewCount h) (drop (hNewStart h) current)
    -- Line ranges [s, s + n) in the base overlap, or meet where one is an
    -- insertion (which could belong to either side).
    touches o = overlap (hOldStart h, hOldCount h) (hOldStart o, hOldCount o)
    overlap (a, n) (b, m)
      | n == 0 || m == 0 = a <= b + m && b <= a + n
      | otherwise = max a b < min (a + n) (b + m)

-- | The buffer's lines with one change undone (the base's lines back).
denyHunk :: Hunk -> [Text] -> [Text] -> [Text]
denyHunk h base current = applyHunks [Hunk (hNewStart h) (hNewCount h) (hOldStart h) (hOldCount h)] current base

-- | The change at a buffer line: its new lines cover the line, or (for a
-- removal) it sits just before the line, or at the end for the last line.
hunkAtLine :: Int -> Int -> [Hunk] -> Maybe (Int, Hunk)
hunkAtLine lineCount line hunks = find (covers . snd) (zip [0 ..] hunks)
  where
    covers h
      | hNewCount h > 0 = hNewStart h <= line && line < hNewStart h + hNewCount h
      | otherwise = line == hNewStart h || (hNewStart h >= lineCount && line == lineCount - 1)

-- | A change described for the model and the picker: where, and its size.
hunkLabel :: FilePath -> Hunk -> Text
hunkLabel path h =
  T.pack path <> ":" <> T.pack (show (hNewStart h + 1)) <> " (-" <> T.pack (show (hOldCount h)) <> " +" <> T.pack (show (hNewCount h)) <> ")"

-- | What a text area row shows.
data DisplayRow
  = -- | A buffer line.
    LineRow !Int
  | -- | A header above a change: its number of all, and its size.
    HeaderRow !Int !Int !Hunk
  | -- | A line the change removes (from the base).
    RemovedRow !Text
  | -- | Past the end of the buffer.
    EmptyRow
  deriving stock (Eq, Show)

-- | The rows from a top line: buffer lines, and before the first new line
-- of each change, its header and removed lines. Without a review, the
-- buffer lines alone.
displayRows :: Maybe Review -> Int -> Int -> Int -> [DisplayRow]
displayRows review lineCount top height = take height (go top)
  where
    hunks = maybe [] rvHunks review
    base = maybe [] rvBase review
    total = length hunks
    go l
      | l > lineCount = repeat EmptyRow
      | otherwise = before l <> (if l < lineCount then LineRow l : go (l + 1) else repeat EmptyRow)
    before l =
      concat
        [ HeaderRow (i + 1) total h : [RemovedRow t | t <- take (hOldCount h) (drop (hOldStart h) base)]
        | (i, h) <- zip [0 ..] hunks
        , hNewStart h == l
        ]

-- | The screen row (from the top of the area) a buffer line is drawn on.
rowOfLine :: [DisplayRow] -> Int -> Maybe Int
rowOfLine rows line = lookup (LineRow line) (zip rows [0 ..])

-- | The buffer lines a review's changes add (to highlight them).
addedLines :: Maybe Review -> [(Int, Int)]
addedLines = maybe [] (\r -> [(hNewStart h, hNewStart h + hNewCount h) | h <- rvHunks r, hNewCount h > 0])
