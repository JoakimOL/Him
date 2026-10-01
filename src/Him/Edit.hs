-- | Pure edits. Each takes the buffer and a range and returns the new buffer
-- and the range afterwards. Insert-mode edits treat the head as a gap
-- (insertion point) before the character it covers.
module Him.Edit
  ( Edit
  , insertAtHead
  , insertNewline
  , deleteBackward
  , deleteForward
  , deleteSelection
  , openLineBelow
  , selectionText
  , pasteAfter
  , pasteBefore
  ) where

import Data.Char (isSpace)
import Data.Text (Text)
import Data.Text qualified as T
import Him.Buffer
import Him.Position (Pos (..))
import Him.Selection

type Edit = Buffer -> Range -> (Buffer, Range)

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

-- | @o@: start a new line below the head's line, with the same indentation.
openLineBelow :: Edit
openLineBelow b r = insertNewline b (point (Pos l (lineLength l b)))
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

pasteAt :: Pos -> Text -> Buffer -> (Buffer, Range)
pasteAt p t b = selectFrom p (insertText p t b)

-- | Select from a start position up to the last inserted character.
selectFrom :: Pos -> (Buffer, Pos) -> (Buffer, Range)
selectFrom start (b', end) = (b', Range start (max start (prevPos b' end)) Nothing)
