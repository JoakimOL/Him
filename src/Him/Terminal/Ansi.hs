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
  , setDefaultColors
  , resetDefaultColors
  , colorRgb
  , CursorShape (..)
  , cursorShape
  , Color (..)
  , Style (..)
  , Underline (..)
  , defaultStyle
  , patchStyle
  , sgr
  , PackedStyle
  , packStyle
  , unpackStyle
  , packedDefault
  , sgrPacked
  ) where

import Data.Bits (shiftL, shiftR, (.&.), (.|.))
import Data.ByteString.Builder (Builder, intDec, word8HexFixed)
import Data.Word (Word32, Word64)

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

-- | OSC 10 / 11: the terminal's default foreground and background, which
-- cells in the default style (and cleared areas) show. 'DefaultColor'
-- restores the terminal's own (OSC 110 / 111). Terminals that do not know
-- them ignore them.
setDefaultColors :: Color -> Color -> Builder
setDefaultColors fg bg = osc 10 fg <> osc 11 bg
  where
    osc :: Int -> Color -> Builder
    osc n c = case colorRgb c of
      Nothing -> "\ESC]" <> intDec (100 + n) <> "\ESC\\"
      Just (r, g, b) -> "\ESC]" <> intDec n <> ";rgb:" <> hex r <> "/" <> hex g <> "/" <> hex b <> "\ESC\\"
    hex v = word8HexFixed (fromIntegral v)

resetDefaultColors :: Builder
resetDefaultColors = setDefaultColors DefaultColor DefaultColor

-- | A colour's red, green and blue (xterm's palette for the indexed ones);
-- 'Nothing' for the terminal's default.
colorRgb :: Color -> Maybe (Int, Int, Int)
colorRgb = \case
  DefaultColor -> Nothing
  Rgb r g b -> Just (r, g, b)
  Ansi n -> Just (basic n)
  Indexed n
    | n < 16 -> Just (basic n)
    | n < 232 ->
        let i = n - 16
            level v = if v == 0 then 0 else 55 + 40 * v
         in Just (level (i `div` 36), level (i `div` 6 `mod` 6), level (i `mod` 6))
    | otherwise -> let v = 8 + 10 * (n - 232) in Just (v, v, v)
  where
    basic n = case n `mod` 16 of
      0 -> (0, 0, 0)
      1 -> (205, 0, 0)
      2 -> (0, 205, 0)
      3 -> (205, 205, 0)
      4 -> (0, 0, 238)
      5 -> (205, 0, 205)
      6 -> (0, 205, 205)
      7 -> (229, 229, 229)
      8 -> (127, 127, 127)
      9 -> (255, 0, 0)
      10 -> (0, 255, 0)
      11 -> (255, 255, 0)
      12 -> (92, 92, 255)
      13 -> (255, 0, 255)
      14 -> (0, 255, 255)
      _ -> (255, 255, 255)

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

-- | How text is underlined. Terminals that do not know the styled kinds
-- (curly, dotted, …) draw a plain line or none.
data Underline = NoUnderline | UnderlineLine | UnderlineCurl | UnderlineDouble | UnderlineDotted | UnderlineDashed
  deriving stock (Eq, Show, Enum, Bounded)

data Style = Style
  { styleFg :: !Color
  , styleBg :: !Color
  , styleBold :: !Bool
  , styleItalic :: !Bool
  , styleUnderline :: !Underline
  , styleUnderlineColor :: !Color
  , styleReverse :: !Bool
  , styleDim :: !Bool
  , styleStrike :: !Bool
  }
  deriving stock (Eq, Show)

defaultStyle :: Style
defaultStyle = Style DefaultColor DefaultColor False False NoUnderline DefaultColor False False False

-- | The second style laid over the first: its colours where it has them,
-- and the modifiers of both (Helix's @Style::patch@). A selection over
-- highlighted text keeps the text's colour when it only sets a background.
patchStyle :: Style -> Style -> Style
patchStyle under over =
  Style
    { styleFg = pick styleFg
    , styleBg = pick styleBg
    , styleBold = flag styleBold
    , styleItalic = flag styleItalic
    , styleUnderline = if styleUnderline over == NoUnderline then styleUnderline under else styleUnderline over
    , styleUnderlineColor = pick styleUnderlineColor
    , styleReverse = flag styleReverse
    , styleDim = flag styleDim
    , styleStrike = flag styleStrike
    }
  where
    pick f = if f over == DefaultColor then f under else f over
    flag f = f under || f over

-- | Select Graphic Rendition: reset, then apply the whole style. Emitting
-- the full style each time keeps the output independent of prior state.
sgr :: Style -> Builder
sgr s =
  csi
    <> "0"
    <> color 30 90 38 (styleFg s)
    <> color 40 100 48 (styleBg s)
    <> flag styleBold "1"
    <> flag styleDim "2"
    <> flag styleItalic "3"
    <> underline (styleUnderline s)
    <> flag styleReverse "7"
    <> flag styleStrike "9"
    <> (if styleUnderline s == NoUnderline then mempty else extendedOnly 58 (styleUnderlineColor s))
    <> "m"
  where
    flag f code = if f s then ";" <> code else mempty
    -- The styled kinds use the colon form (4:3), which terminals that do
    -- not know it ignore.
    underline = \case
      NoUnderline -> mempty
      UnderlineLine -> ";4"
      UnderlineDouble -> ";4:2"
      UnderlineCurl -> ";4:3"
      UnderlineDotted -> ";4:4"
      UnderlineDashed -> ";4:5"
    color :: Int -> Int -> Int -> Color -> Builder
    color base brightBase extended = \case
      DefaultColor -> mempty
      Ansi n
        | n < 8 -> ";" <> intDec (base + n)
        | otherwise -> ";" <> intDec (brightBase + n - 8)
      c -> extendedOnly extended c
    -- The underline colour (58) has only the extended forms.
    extendedOnly :: Int -> Color -> Builder
    extendedOnly extended = \case
      DefaultColor -> mempty
      Ansi n -> ";" <> intDec extended <> ";5;" <> intDec n
      Indexed n -> ";" <> intDec extended <> ";5;" <> intDec n
      Rgb r g b -> ";" <> intDec extended <> ";2;" <> intDec r <> ";" <> intDec g <> ";" <> intDec b

-- | A 'Style' packed into two machine words, for frames. The first holds
-- the two 26-bit colours (2-bit tag, 24-bit payload), five flags and the
-- underline kind; the second the underline colour. Cells hold it unpacked,
-- so comparing two cells is a few word comparisons instead of walking a
-- record of boxed fields.
data PackedStyle = PackedStyle {-# UNPACK #-} !Word64 {-# UNPACK #-} !Word32
  deriving stock (Eq, Ord)

instance Show PackedStyle where
  show = show . unpackStyle

packStyle :: Style -> PackedStyle
packStyle (Style fg bg b i u uc r d x) =
  PackedStyle
    ( packColor fg
        .|. (packColor bg `shiftL` 26)
        .|. flag b 52
        .|. flag i 53
        .|. flag r 54
        .|. flag d 55
        .|. flag x 56
        .|. (fromIntegral (fromEnum u) `shiftL` 57)
    )
    (fromIntegral (packColor uc))
  where
    flag on bit = if on then 1 `shiftL` bit else 0

unpackStyle :: PackedStyle -> Style
unpackStyle (PackedStyle w uc) =
  Style
    (unpackColor w)
    (unpackColor (w `shiftR` 26))
    (bit 52)
    (bit 53)
    (toEnum (fromIntegral ((w `shiftR` 57) .&. 7)))
    (unpackColor (fromIntegral uc))
    (bit 54)
    (bit 55)
    (bit 56)
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
