-- | How characters of a line map to terminal columns.
module Him.TextWidth
  ( charWidth
  , glyphs
  , layoutLine
  , displayCol
  , charIndexAtCol
  , isWide
  ) where

import Data.Char (chr, ord)
import Data.IntMap.Strict qualified as IntMap
import Data.List (mapAccumL)
import Data.Text (Text)
import Data.Text qualified as T

-- | Terminal columns a character takes, not counting tabs (whose width
-- depends on the column; see 'layoutLine'). Control characters are shown as
-- @^X@, so they take two columns.
charWidth :: Char -> Int
charWidth c
  | c >= ' ' && c < '\DEL' = 1 -- fast path: printable ASCII
  | isControl c = 2
  | isWide c = 2
  | otherwise = 1

-- | What to draw for a character that starts at a column of the given width:
-- tabs become spaces, control characters become @^X@. Wide characters are a
-- single glyph spanning two cells.
glyphs :: Char -> Int -> String
glyphs c w
  | c == '\t' = replicate w ' '
  | isControl c = ['^', if c == '\DEL' then '?' else chr (ord c + 64)]
  | otherwise = [c]

isControl :: Char -> Bool
isControl c = (c < ' ' && c /= '\t') || c == '\DEL'

-- | East Asian Wide / Fullwidth characters and wide emoji (a compact
-- approximation of Unicode's EastAsianWidth W/F classes).
isWide :: Char -> Bool
isWide c
  | n < 0x1100 = False -- fast path: nothing below Hangul Jamo is wide
  | otherwise = case IntMap.lookupLE n wideTable of
      Just (_, hi) -> n <= hi
      Nothing -> False
  where
    n = ord c

-- | 'wideRanges' keyed by their start, for O(log n) lookup.
wideTable :: IntMap.IntMap Int
wideTable = IntMap.fromList wideRanges

wideRanges :: [(Int, Int)]
wideRanges =
  [ (0x1100, 0x115F) -- Hangul Jamo
  , (0x231A, 0x231B)
  , (0x2329, 0x232A)
  , (0x23E9, 0x23EC)
  , (0x25FD, 0x25FE)
  , (0x2614, 0x2615)
  , (0x2648, 0x2653)
  , (0x26AA, 0x26AB)
  , (0x26BD, 0x26BE)
  , (0x26C4, 0x26C5)
  , (0x2705, 0x2705)
  , (0x270A, 0x270B)
  , (0x2728, 0x2728)
  , (0x274C, 0x274C)
  , (0x2753, 0x2755)
  , (0x2795, 0x2797)
  , (0x2B1B, 0x2B1C)
  , (0x2E80, 0x303E) -- CJK radicals, punctuation
  , (0x3041, 0x33FF) -- Hiragana, Katakana, CJK compatibility
  , (0x3400, 0x4DBF) -- CJK extension A
  , (0x4E00, 0x9FFF) -- CJK unified ideographs
  , (0xA000, 0xA4CF) -- Yi
  , (0xA960, 0xA97F)
  , (0xAC00, 0xD7A3) -- Hangul syllables
  , (0xF900, 0xFAFF) -- CJK compatibility ideographs
  , (0xFE10, 0xFE19)
  , (0xFE30, 0xFE6F)
  , (0xFF00, 0xFF60) -- Fullwidth forms
  , (0xFFE0, 0xFFE6)
  , (0x1F004, 0x1F004)
  , (0x1F0CF, 0x1F0CF)
  , (0x1F18E, 0x1F18E)
  , (0x1F191, 0x1F19A)
  , (0x1F200, 0x1F251)
  , (0x1F300, 0x1F64F) -- emoji
  , (0x1F680, 0x1F6FF)
  , (0x1F7E0, 0x1F7EB)
  , (0x1F90C, 0x1F9FF)
  , (0x1FA70, 0x1FAFF)
  , (0x20000, 0x2FFFD) -- CJK extensions
  , (0x30000, 0x3FFFD)
  ]

-- | @(charIndex, displayCol, width, char)@ for every character of a line,
-- with tab stops every @tabWidth@ columns.
layoutLine :: Int -> Text -> [(Int, Int, Int, Char)]
layoutLine tabWidth = snd . mapAccumL step 0 . zip [0 ..] . T.unpack
  where
    step col (i, c) =
      let w = if c == '\t' then tabWidth - col `mod` tabWidth else charWidth c
       in (col + w, (i, col, w, c))

-- | The display column where the character at a given index starts. Indices
-- at or past the end give the column just after the line.
displayCol :: Int -> Text -> Int -> Int
displayCol tw line i = case drop i (layoutLine tw line) of
  ((_, col, _, _) : _) -> col
  [] -> sum [w | (_, _, w, _) <- layoutLine tw line]

-- | The index of the character covering a display column (the inverse of
-- 'displayCol'). Columns past the end give the line length.
charIndexAtCol :: Int -> Text -> Int -> Int
charIndexAtCol tw line col = case [i | (i, start, w, _) <- layoutLine tw line, col < start + w] of
  (i : _) -> i
  [] -> T.length line
