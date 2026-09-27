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
