-- | Pure edits. Each takes the buffer and a range and returns the new buffer
-- and the range afterwards. Insert-mode edits treat the head as a gap
-- (insertion point) before the character it covers.
module Him.Edit
  ( Edit
  , applyEdits
  , insertAtHead
  , insertNewline
  , deleteBackward
  , deleteForward
  , deleteSelection
  , openLine
  , LineDirection (..)
  , selectionText
  , pasteAfter
  , pasteBefore
  , replaceWith
  , replaceChars
  , mapChars
  ) where

import Data.Char (isSpace)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Text qualified as T
import Him.Buffer
import Him.Position (Pos (..))
import Him.Selection

type Edit = Buffer -> Range -> (Buffer, Range)

-- | Which side of the head's line 'openLine' opens a line on.
data LineDirection = Above | Below
  deriving stock (Eq, Show)

-- | Apply an edit to every range of a selection; the edit is told the
-- range's index (e.g. to paste the matching register value).
--
-- The ranges are applied from the last to the first. An edit only changes
-- text around its own range, which lies before every range already
-- edited, so the results so far are kept as distances from the end of the
-- buffer (lines from the last line, characters from the end of their line),
-- which an edit further up does not change. That avoids mapping positions
-- through each change. Overlapping ranges are merged first.
applyEdits :: (Int -> Edit) -> Buffer -> Selection -> (Buffer, Selection)
applyEdits f b0 sel0 = case ranges sel of
  [r] -> let (b, r') = f 0 b0 r in (b, modifyPrimary (const r') sel)
  rs ->
    let step (b, done) (i, r) = let (b', r') = f i b r in (b', fromEnd b' r' : done)
        (b1, results) = foldl step (b0, []) (reverse (zip [0 ..] rs))
        rs' = map (toEnd b1) results
     in (b1, fromMaybe sel (fromRanges rs' (primaryIndex sel)))
  where
    sel = normalize sel0
    fromEnd b (Range a h w) = (distance b a, distance b h, w)
    toEnd b (a, h, w) = Range (position b a) (position b h) w
    distance b (Pos l c) = (lineCount b - 1 - l, lineLength l b - c)
    position b (dl, dc) =
      let l = max 0 (lineCount b - 1 - dl)
       in Pos l (max 0 (lineLength l b - dc))

insertAtHead :: Text -> Edit
insertAtHead t b r = let (b', p) = insertText (rangeHead r) t b in (b', point p)

-- | Split the line at the head, keeping the current line's indentation.
insertNewline :: Edit
insertNewline b r = insertAtHead ("\n" <> indentOf (posLine (rangeHead r)) b) b r

indentOf :: Int -> Buffer -> Text
indentOf l b = T.takeWhile (\c -> isSpace c && c /= '\n') (lineAt l b)

deleteBackward :: Edit
deleteBackward b r
  | p == h = (b, r)
  | otherwise = (deleteRange p h b, point p)
  where
    h = rangeHead r
    p = prevPos b h

deleteForward :: Edit
deleteForward b r = (deleteRange h (nextPos b h) b, point h)
  where
    h = rangeHead r

-- | Delete everything the range covers. When the range reaches the end of the
-- file and starts at a line start, the newline before it goes too, so that
-- deleting the last line(s) with @x d@ removes them completely.
deleteSelection :: Edit
deleteSelection b r
  | removesTrailingLines = (deleteRange (prevPos b s) to b, point (Pos (posLine s - 1) 0))
  | otherwise = let b' = deleteRange s to b in (b', point (clampPos b' s))
  where
    s = rangeStart r
    e = rangeEnd r
    to = nextPos b e
    removesTrailingLines = e == endPos b && posCol s == 0 && posLine s > 0

-- | @O@ / @o@: start a new line above or below the head's line, with the
-- same indentation; the head goes after that indentation.
openLine :: LineDirection -> Edit
openLine Above b r =
  let (b', _) = insertText (Pos l 0) (indent <> "\n") b
   in (b', point (Pos l (T.length indent)))
  where
    l = posLine (rangeHead r)
    indent = indentOf l b
openLine Below b r = insertNewline b (point (Pos l (lineLength l b)))
  where
    l = posLine (rangeHead r)

-- | The text a range covers (inclusive of the character at its end). Whole
-- lines at the end of the file get the newline they implicitly end with, so
-- yanking the last line with @x@ is linewise like any other line.
selectionText :: Buffer -> Range -> Text
selectionText b r
  | e == endPos b && posCol s == 0 = t <> "\n"
  | otherwise = t
  where
    s = rangeStart r
    e = rangeEnd r
    t = textRange s (nextPos b e) b

-- | Text ending in a newline (e.g. yanked with @x@) is pasted as whole lines.
isLinewise :: Text -> Bool
isLinewise = T.isSuffixOf "\n"

-- | @p@: paste after the selection (linewise text: below its last line).
-- The pasted text becomes the selection.
pasteAfter :: Text -> Edit
pasteAfter t b r
  | T.null t = (b, r)
  | isLinewise t && endLine + 1 < lineCount b = pasteAt (Pos (endLine + 1) 0) t b
  | isLinewise t = selectFrom (Pos (endLine + 1) 0) (insertText (endPos b) ("\n" <> T.dropEnd 1 t) b)
  | otherwise = pasteAt (nextPos b (rangeEnd r)) t b
  where
    endLine = posLine (rangeEnd r)

-- | @P@: paste before the selection (linewise text: above its first line).
pasteBefore :: Text -> Edit
pasteBefore t b r
  | T.null t = (b, r)
  | isLinewise t = pasteAt (Pos (posLine (rangeStart r)) 0) t b
  | otherwise = pasteAt (rangeStart r) t b

-- | @R@: replace what the range covers; the new text becomes the selection.
replaceWith :: Text -> Edit
replaceWith t b r
  | T.null t = (b', point (clampPos b' s))
  | otherwise = pasteAt s t b'
  where
    s = rangeStart r
    b' = deleteRange s (nextPos b (rangeEnd r)) b

-- | @r@: replace every character the range covers with one character, as in
-- Helix; line breaks stay. The range stays too, unless the character is
-- itself a line break (@r ret@), which moves what follows.
replaceChars :: Char -> Edit
replaceChars ch b r
  | ch == '\n' = pasteAt s new b'
  | otherwise = (fst (insertText s new b'), r)
  where
    s = rangeStart r
    to = nextPos b (rangeEnd r)
    new = T.map (\c -> if c == '\n' then c else ch) (textRange s to b)
    b' = deleteRange s to b

-- | Change every character the range covers, one character for one (a
-- case change), so the range stays.
mapChars :: (Char -> Char) -> Edit
mapChars f b r = (fst (insertText s (T.map f (textRange s to b)) (deleteRange s to b)), r)
  where
    s = rangeStart r
    to = nextPos b (rangeEnd r)

pasteAt :: Pos -> Text -> Buffer -> (Buffer, Range)
pasteAt p t b = selectFrom p (insertText p t b)

-- | Select from a start position up to the last inserted character.
selectFrom :: Pos -> (Buffer, Pos) -> (Buffer, Range)
selectFrom start (b', end) = (b', Range start (max start (prevPos b' end)) Nothing)
