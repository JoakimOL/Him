-- | Pure builders for the ANSI / xterm escape sequences the editor uses.
-- Coordinates are 0-based; the conversion to the terminal's 1-based
-- coordinates happens here.
module Him.Terminal.Ansi
  ( moveCursor
  , clearScreen
  , clearToEndOfLine
  , hideCursor
  , showCursor
  , CursorShape (..)
  , cursorShape
  , Color (..)
  , Style (..)
  , defaultStyle
  , sgr
  ) where

import Data.ByteString.Builder (Builder, intDec)

csi :: Builder
csi = "\ESC["

-- | Move the cursor to @(row, col)@, both 0-based.
moveCursor :: Int -> Int -> Builder
moveCursor row col = csi <> intDec (row + 1) <> ";" <> intDec (col + 1) <> "H"

clearScreen :: Builder
clearScreen = csi <> "2J"

clearToEndOfLine :: Builder
clearToEndOfLine = csi <> "K"

hideCursor :: Builder
hideCursor = csi <> "?25l"

showCursor :: Builder
showCursor = csi <> "?25h"

data CursorShape = CursorBlock | CursorBar | CursorUnderline
  deriving stock (Eq, Show)

-- | DECSCUSR, steady (non-blinking) variants.
cursorShape :: CursorShape -> Builder
cursorShape = \case
  CursorBlock -> csi <> "2 q"
  CursorUnderline -> csi <> "4 q"
  CursorBar -> csi <> "6 q"

data Color
  = DefaultColor
  | -- | One of the 16 basic colours, 0-15.
    Ansi !Int
  | -- | 256-colour palette index.
    Indexed !Int
  | Rgb !Int !Int !Int
  deriving stock (Eq, Show)

data Style = Style
  { styleFg :: !Color
  , styleBg :: !Color
  , styleBold :: !Bool
  , styleItalic :: !Bool
  , styleUnderline :: !Bool
  , styleReverse :: !Bool
  }
  deriving stock (Eq, Show)

defaultStyle :: Style
defaultStyle = Style DefaultColor DefaultColor False False False False

-- | Select Graphic Rendition: reset, then apply the whole style. Emitting
-- the full style each time keeps the output independent of prior state.
sgr :: Style -> Builder
sgr s =
  csi
    <> "0"
    <> color 30 90 38 (styleFg s)
    <> color 40 100 48 (styleBg s)
    <> flag styleBold "1"
    <> flag styleItalic "3"
    <> flag styleUnderline "4"
    <> flag styleReverse "7"
    <> "m"
  where
    flag f code = if f s then ";" <> code else mempty
    color :: Int -> Int -> Int -> Color -> Builder
    color base brightBase extended = \case
      DefaultColor -> mempty
      Ansi n
        | n < 8 -> ";" <> intDec (base + n)
        | otherwise -> ";" <> intDec (brightBase + n - 8)
      Indexed n -> ";" <> intDec extended <> ";5;" <> intDec n
      Rgb r g b -> ";" <> intDec extended <> ";2;" <> intDec r <> ";" <> intDec g <> ";" <> intDec b
