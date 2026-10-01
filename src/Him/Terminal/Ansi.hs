-- | Pure builders for the ANSI / xterm escape sequences the editor uses.
-- Coordinates are 0-based; the conversion to the terminal's 1-based
-- coordinates happens here.
module Him.Terminal.Ansi
  ( moveCursor
  , clearScreen
  , clearToEndOfLine
  , setScrollRegion
  , resetScrollRegion
  , scrollUp
  , scrollDown
  , hideCursor
  , showCursor
  , CursorShape (..)
  , cursorShape
  , Color (..)
  , Style (..)
  , defaultStyle
  , sgr
  , PackedStyle
  , packStyle
  , unpackStyle
  , packedDefault
  , sgrPacked
  ) where

import Data.Bits (shiftL, shiftR, (.&.), (.|.))
import Data.ByteString.Builder (Builder, intDec)
import Data.Word (Word64)

csi :: Builder
csi = "\ESC["

-- | Move the cursor to @(row, col)@, both 0-based.
moveCursor :: Int -> Int -> Builder
moveCursor row col = csi <> intDec (row + 1) <> ";" <> intDec (col + 1) <> "H"

clearScreen :: Builder
clearScreen = csi <> "2J"

clearToEndOfLine :: Builder
clearToEndOfLine = csi <> "K"

-- | DECSTBM: scrolling affects rows @[top, top + height)@ only (0-based).
setScrollRegion :: Int -> Int -> Builder
setScrollRegion top height = csi <> intDec (top + 1) <> ";" <> intDec (top + height) <> "r"

-- | Back to the whole screen (this also moves the cursor home).
resetScrollRegion :: Builder
resetScrollRegion = csi <> "r"

-- | SU / SD: move the scroll region's contents up / down by n rows; the rows
-- that come in are blank.
scrollUp, scrollDown :: Int -> Builder
scrollUp n = csi <> intDec n <> "S"
scrollDown n = csi <> intDec n <> "T"

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

-- | A 'Style' packed into one machine word, for frames: two 26-bit colours
-- (2-bit tag, 24-bit payload) and four flag bits. Cells hold it unpacked,
-- so comparing two cells is two word comparisons instead of walking a
-- record of boxed fields.
newtype PackedStyle = PackedStyle Word64
  deriving stock (Eq, Ord)

instance Show PackedStyle where
  show = show . unpackStyle

packStyle :: Style -> PackedStyle
packStyle (Style fg bg b i u r) =
  PackedStyle $
    packColor fg
      .|. (packColor bg `shiftL` 26)
      .|. flag b 52
      .|. flag i 53
      .|. flag u 54
      .|. flag r 55
  where
    flag on bit = if on then 1 `shiftL` bit else 0

unpackStyle :: PackedStyle -> Style
unpackStyle (PackedStyle w) =
  Style (unpackColor w) (unpackColor (w `shiftR` 26)) (bit 52) (bit 53) (bit 54) (bit 55)
  where
    bit n = (w `shiftR` n) .&. 1 == 1

packedDefault :: PackedStyle
packedDefault = packStyle defaultStyle

sgrPacked :: PackedStyle -> Builder
sgrPacked = sgr . unpackStyle

packColor :: Color -> Word64
packColor = \case
  DefaultColor -> 0
  Ansi n -> 1 `shiftL` 24 .|. fromIntegral (n .&. 0xff)
  Indexed n -> 2 `shiftL` 24 .|. fromIntegral (n .&. 0xff)
  Rgb r g b -> 3 `shiftL` 24 .|. fromIntegral (r .&. 0xff) `shiftL` 16 .|. fromIntegral (g .&. 0xff) `shiftL` 8 .|. fromIntegral (b .&. 0xff)

unpackColor :: Word64 -> Color
unpackColor w = case (w `shiftR` 24) .&. 3 of
  0 -> DefaultColor
  1 -> Ansi (fromIntegral (w .&. 0xff))
  2 -> Indexed (fromIntegral (w .&. 0xff))
  _ -> Rgb (fromIntegral ((w `shiftR` 16) .&. 0xff)) (fromIntegral ((w `shiftR` 8) .&. 0xff)) (fromIntegral (w .&. 0xff))
