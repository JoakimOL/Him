-- | How characters of a line map to terminal columns.
module Him.TextWidth
  ( tabWidth
  , charWidth
  , layoutLine
  , displayCol
  ) where

import Data.List (mapAccumL)
import Data.Text (Text)
import Data.Text qualified as T

tabWidth :: Int
tabWidth = 4

-- | Terminal columns taken by a character (tabs are handled by 'layoutLine').
charWidth :: Char -> Int
charWidth _ = 1

-- | @(charIndex, displayCol, width, char)@ for every character of a line.
layoutLine :: Text -> [(Int, Int, Int, Char)]
layoutLine = snd . mapAccumL step 0 . zip [0 ..] . T.unpack
  where
    step col (i, c) =
      let w = if c == '\t' then tabWidth - col `mod` tabWidth else charWidth c
       in (col + w, (i, col, w, c))

-- | The display column where the character at a given index starts. Indices
-- at or past the end give the column just after the line.
displayCol :: Text -> Int -> Int
displayCol line i = case drop i (layoutLine line) of
  ((_, col, _, _) : _) -> col
  [] -> sum [w | (_, _, w, _) <- layoutLine line]
