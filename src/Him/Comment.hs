-- | Toggling comments (@C-c@): line comments on every line the selection
-- touches, or a block comment around each range.
module Him.Comment
  ( toggleLineComments
  , toggleBlockComments
  ) where

import Data.Char (isSpace)
import Data.IntMap.Strict qualified as IntMap
import Data.IntSet qualified as IntSet
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Text qualified as T
import Him.Buffer
import Him.Edit (applyEdits)
import Him.Position (Pos (..))
import Him.Selection

-- | Comment the lines the selection covers with a line comment token, or
-- uncomment them when every one is commented already. Blank lines are left
-- alone (and do not count). The token goes at the smallest indentation of
-- the lines, followed by a space; uncommenting removes that space too.
toggleLineComments :: Text -> Buffer -> Selection -> (Buffer, Selection)
toggleLineComments token b sel
  | null nonBlank = (b, sel)
  | otherwise = (foldl' change b (IntMap.toList changes), mapRanges move sel)
  where
    covered = IntSet.toAscList (IntSet.fromList (concat [[posLine (rangeStart r) .. posLine (rangeEnd r)] | r <- ranges sel]))
    nonBlank = [l | l <- covered, l < lineCount b, not (T.all isSpace (lineAt l b))]
    indent l = T.length (T.takeWhile isSpace (lineAt l b))
    commented l = token `T.isPrefixOf` T.stripStart (lineAt l b)
    uncommenting = all commented nonBlank
    col = minimum (map indent nonBlank)
    -- Per line: where the change is, and how many characters come (> 0) or go (< 0).
    changes = IntMap.fromList (map lineChange nonBlank)
    lineChange l
      | uncommenting =
          let i = indent l
              afterToken = T.drop (i + T.length token) (lineAt l b)
              n = T.length token + (if " " `T.isPrefixOf` afterToken then 1 else 0)
           in (l, (i, negate n))
      | otherwise = (l, (col, T.length token + 1))
    change buf (l, (c, delta))
      | delta > 0 = fst (insertText (Pos l c) (token <> " ") buf)
      | otherwise = deleteRange (Pos l c) (Pos l (c - delta)) buf
    move (Range a h _) = Range (shift a) (shift h) Nothing
    shift p@(Pos l c) = case IntMap.lookup l changes of
      Just (at, delta)
        | delta > 0 && c >= at -> Pos l (c + delta)
        | delta < 0 && c >= at - delta -> Pos l (c + delta)
        | delta < 0 && c > at -> Pos l at
      _ -> p

-- | Wrap each range in a block comment (inside its surrounding whitespace),
-- or unwrap them all when every range is one already. A range of one
-- character stands for its whole line.
toggleBlockComments :: (Text, Text) -> Buffer -> Selection -> (Buffer, Selection)
toggleBlockComments (open, close) b sel
  | null cores = (b, sel)
  | otherwise = applyEdits (const edit) b sel
  where
    cores = [core | r <- ranges sel, let (_, core, _) = parts b (widen b r), not (T.null core)]
    uncommenting = all isComment cores
    isComment core = T.length core >= T.length open + T.length close && open `T.isPrefixOf` core && close `T.isSuffixOf` core
    edit buf r0 =
      let r = widen buf r0
          (lead, core, trail) = parts buf r
          core'
            | T.null core = core
            | uncommenting = unwrap core
            | otherwise = open <> " " <> core <> " " <> close
          s = rangeStart r
          (buf', end) = insertText s (lead <> core' <> trail) (deleteRange s (nextPos buf (rangeEnd r)) buf)
       in if T.null core then (buf, r0) else (buf', Range s (max s (prevPos buf' end)) Nothing)
    unwrap core =
      let inner = T.dropEnd (T.length close) (T.drop (T.length open) core)
          inner' = fromMaybe inner (T.stripPrefix " " inner)
       in fromMaybe inner' (T.stripSuffix " " inner')

-- | One character widens to its line (up to and including its line break).
widen :: Buffer -> Range -> Range
widen b r
  | rangeStart r == rangeEnd r = let l = posLine (rangeHead r) in Range (Pos l 0) (Pos l (lineLength l b)) Nothing
  | otherwise = r

-- | A range's text split into leading whitespace, the rest, and trailing
-- whitespace.
parts :: Buffer -> Range -> (Text, Text, Text)
parts b r =
  let t = textRange (rangeStart r) (nextPos b (rangeEnd r)) b
      (lead, rest) = T.span isSpace t
      core = T.dropWhileEnd isSpace rest
   in (lead, core, T.drop (T.length core) rest)
