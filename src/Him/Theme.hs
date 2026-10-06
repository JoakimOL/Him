-- | Themes in Helix's format (ADR helix-themes): a TOML table from scopes to styles,
-- with an optional @[palette]@ of named colours and @inherits = "name"@.
--
-- @
-- inherits = "onedark"
-- "comment" = { fg = "gray", modifiers = ["italic"] }
-- "ui.selection" = { bg = "#3e4452" }
-- "diagnostic.error" = { underline = { color = "red", style = "curl" } }
-- "diff.plus" = "green"            # a string is the foreground
--
-- [palette]
-- gray = "#5c6370"
-- @
--
-- Colours are palette names, @#rrggbb@, a palette index (@"110"@), the
-- sixteen terminal colours (@red@, @light-red@, …, @white@), or @default@.
-- This module is pure: reading files and following @inherits@ is in
-- "Him.Theme.Load"; turning the styles into what rendering uses is
-- 'Him.Render.Theme.fromScopes'.
module Him.Theme
  ( ThemeFile (..)
  , parseThemeFile
  , mergeThemeFiles
  , resolveTheme
  , parseColor
  , downsample
  , defaultThemeText
  ) where

import Data.Char (isDigit, isHexDigit, digitToInt)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Text qualified as T
import Him.Json (Value (..), asObject)
import Him.Terminal.Ansi (Color (..), Style (..), Underline (..), defaultStyle)
import Him.Toml (parseToml)

-- | A theme file as written: its parent, palette, and scope entries (not
-- yet resolved, since a child's palette also colours its parent's entries).
data ThemeFile = ThemeFile
  { tfInherits :: !(Maybe Text)
  , tfPalette :: !(Map Text Text)
  , tfStyles :: !(Map Text Value)
  }
  deriving stock (Eq, Show)

parseThemeFile :: Text -> Either Text ThemeFile
parseThemeFile src = do
  doc <- parseToml src
  top <- maybe (Left "a theme must be a table") Right (asObject doc)
  let inherits = case lookup "inherits" top of
        Just (JString p) -> Just p
        _ -> Nothing
      palette = case lookup "palette" top >>= asObject of
        Just kvs -> Map.fromList [(k, c) | (k, JString c) <- kvs]
        Nothing -> Map.empty
      -- Helix's rainbow brackets (a list of styles) are not supported.
      styles = Map.fromList [(k, v) | (k, v) <- top, k `notElem` ["inherits", "palette", "rainbow"]]
  pure (ThemeFile inherits palette styles)

-- | A child over its parent, as Helix merges them: the child's entries
-- replace the parent's whole, palettes merge name by name.
mergeThemeFiles :: ThemeFile -> ThemeFile -> ThemeFile
mergeThemeFiles parent child =
  ThemeFile
    { tfInherits = tfInherits parent
    , tfPalette = tfPalette child <> tfPalette parent
    , tfStyles = tfStyles child <> tfStyles parent
    }

-- | The styles of every scope, and warnings about what was not understood
-- (those entries or parts are skipped, like Helix does).
resolveTheme :: ThemeFile -> (Map Text Style, [Text])
resolveTheme tf = (Map.fromList [(k, st) | (k, st, _) <- resolved], concat [ws | (_, _, ws) <- resolved])
  where
    resolved = [(k, st, map ((k <> ": ") <>) ws) | (k, v) <- Map.toList (tfStyles tf), let (st, ws) = style v]
    color = parseColor (tfPalette tf)
    style = \case
      JString c -> withColor c (\col -> defaultStyle {styleFg = col}) defaultStyle
      JObject kvs -> foldl' field (defaultStyle, []) kvs
      _ -> (defaultStyle, ["a style is a colour or a table"])
    withColor c f dflt = case color c of
      Just col -> (f col, [])
      Nothing -> (dflt, ["unknown colour " <> c])
    field (st, ws) = \case
      ("fg", JString c) -> add (withColor c (\col -> st {styleFg = col}) st)
      ("bg", JString c) -> add (withColor c (\col -> st {styleBg = col}) st)
      ("modifiers", JArray ms) -> (foldl' modifier st ms, ws <> ["unknown modifier " <> m | JString m <- ms, m `notElem` knownModifiers])
      ("underline", JObject u) -> (underline st u, ws <> underlineWarnings u)
      (k, _) -> (st, ws <> ["unknown style key " <> k])
      where
        add (st', w) = (st', ws <> w)
    modifier st = \case
      JString "bold" -> st {styleBold = True}
      JString "dim" -> st {styleDim = True}
      JString "italic" -> st {styleItalic = True}
      JString "underlined" -> st {styleUnderline = if styleUnderline st == NoUnderline then UnderlineLine else styleUnderline st}
      JString "reversed" -> st {styleReverse = True}
      JString "crossed_out" -> st {styleStrike = True}
      _ -> st
    knownModifiers = ["bold", "dim", "italic", "underlined", "reversed", "crossed_out", "slow_blink", "rapid_blink", "hidden", "normal"]
    underline st u =
      let kind = case lookup "style" u of
            Just (JString s) -> fromMaybe UnderlineLine (lookup s underlineKinds)
            _ -> if styleUnderline st == NoUnderline then UnderlineLine else styleUnderline st
          col = case lookup "color" u of
            Just (JString c) -> fromMaybe (styleUnderlineColor st) (color c)
            _ -> styleUnderlineColor st
       in st {styleUnderline = kind, styleUnderlineColor = col}
    underlineWarnings u =
      [ "unknown underline style " <> s | Just (JString s) <- [lookup "style" u], s `notElem` map fst underlineKinds]
        <> ["unknown colour " <> c | Just (JString c) <- [lookup "color" u], color c == Nothing]
    underlineKinds = [("line", UnderlineLine), ("curl", UnderlineCurl), ("double_line", UnderlineDouble), ("dotted", UnderlineDotted), ("dashed", UnderlineDashed)]

-- | A colour by name: the palette first, then @#rrggbb@, a palette index,
-- the terminal's sixteen colours (Helix's names), @default@ / @reset@.
parseColor :: Map Text Text -> Text -> Maybe Color
parseColor palette = go (3 :: Int)
  where
    go depth name = case Map.lookup name palette of
      -- A palette entry may name another (bounded, against loops).
      Just c | depth > 0, c /= name -> go (depth - 1) c
      _ -> literal name
    literal name
      | Just hex <- T.stripPrefix "#" name, T.length hex == 6, T.all isHexDigit hex =
          let byte i = digitToInt (T.index hex i) * 16 + digitToInt (T.index hex (i + 1))
           in Just (Rgb (byte 0) (byte 2) (byte 4))
      -- #rgb is #rrggbb.
      | Just hex <- T.stripPrefix "#" name, T.length hex == 3, T.all isHexDigit hex =
          let nibble i = digitToInt (T.index hex i) * 17
           in Just (Rgb (nibble 0) (nibble 1) (nibble 2))
      | not (T.null name), T.all isDigit name, T.length name <= 3, let n = read (T.unpack name), n < 256 = Just (Indexed n)
      | otherwise = lookup name named
    named =
      [ ("default", DefaultColor)
      , ("reset", DefaultColor)
      , ("black", Ansi 0)
      , ("red", Ansi 1)
      , ("green", Ansi 2)
      , ("yellow", Ansi 3)
      , ("blue", Ansi 4)
      , ("magenta", Ansi 5)
      , ("cyan", Ansi 6)
      , ("light-gray", Ansi 7)
      , ("gray", Ansi 8)
      , ("light-red", Ansi 9)
      , ("light-green", Ansi 10)
      , ("light-yellow", Ansi 11)
      , ("light-blue", Ansi 12)
      , ("light-magenta", Ansi 13)
      , ("light-cyan", Ansi 14)
      , ("white", Ansi 15)
      ]

-- | For terminals without 24-bit colour: every RGB colour becomes the
-- closest of the 256-colour palette (its 6×6×6 cube or grey ramp).
downsample :: Style -> Style
downsample st = st {styleFg = c (styleFg st), styleBg = c (styleBg st), styleUnderlineColor = c (styleUnderlineColor st)}
  where
    c = \case
      Rgb r g b -> Indexed (nearest256 r g b)
      other -> other

nearest256 :: Int -> Int -> Int -> Int
nearest256 r g b = snd (minimum [(dist rgb, i) | (i, rgb) <- candidates])
  where
    levels = [0, 95, 135, 175, 215, 255]
    level v = snd (minimum [(abs (v - l), i) | (i, l) <- zip [0 :: Int ..] levels])
    (ri, gi, bi) = (level r, level g, level b)
    cube = 16 + 36 * ri + 6 * gi + bi
    grey = let i = max 0 (min 23 ((((r + g + b) `div` 3) - 8 + 5) `div` 10)) in i
    candidates = [(cube, (levels !! ri, levels !! gi, levels !! bi)), (232 + grey, let v = 8 + 10 * grey in (v, v, v))]
    dist (r', g', b') = sq (r - r') + sq (g - g') + sq (b - b')
    sq x = x * x

-- | The built-in theme ("default"), written like any theme file. Its
-- colours are 256-colour palette indexes, so it looks the same on any
-- terminal.
defaultThemeText :: Text
defaultThemeText =
  T.unlines
    [ "# him's built-in theme. Any Helix theme works too: [editor] theme = \"onedark\"."
    , "\"ui.text\" = {}"
    , "\"ui.selection\" = { bg = \"selection\" }"
    , "\"ui.cursor\" = { modifiers = [\"reversed\"] }"
    , "\"ui.virtual\" = \"dim\""
    , "\"ui.linenr\" = \"dim\""
    , "\"ui.linenr.selected\" = \"250\""
    , "\"ui.statusline\" = { fg = \"252\", bg = \"bar\" }"
    , "\"ui.statusline.inactive\" = { fg = \"244\", bg = \"bar\" }"
    , "\"ui.window\" = \"dim\""
    , "\"ui.highlight\" = { bg = \"22\" }"
    , "\"ui.statusline.normal\" = { fg = \"235\", bg = \"blue\", modifiers = [\"bold\"] }"
    , "\"ui.statusline.insert\" = { fg = \"235\", bg = \"green\", modifiers = [\"bold\"] }"
    , "\"ui.statusline.select\" = { fg = \"235\", bg = \"sand\", modifiers = [\"bold\"] }"
    , "\"ui.statusline.command\" = { fg = \"235\", bg = \"purple\", modifiers = [\"bold\"] }"
    , "\"ui.statusline.picker\" = { fg = \"235\", bg = \"purple\", modifiers = [\"bold\"] }"
    , "\"ui.popup\" = { fg = \"252\", bg = \"bar\" }"
    , "\"ui.popup.key\" = { fg = \"blue\", modifiers = [\"bold\"] }"
    , "\"ui.menu.selected\" = { fg = \"255\", bg = \"selection\" }"
    , "\"ui.text.inactive\" = \"245\""
    , "\"ui.text.directory\" = { fg = \"blue\", modifiers = [\"bold\"] }"
    , "\"ui.text.focus\" = { fg = \"sand\", modifiers = [\"bold\"] }"
    , "\"error\" = \"light-red\""
    , "\"warning\" = \"yellow\""
    , "\"info\" = \"blue\""
    , "\"hint\" = \"245\""
    , "\"diagnostic.error\" = { underline = { color = \"red\", style = \"curl\" } }"
    , "\"diagnostic.warning\" = { underline = { color = \"yellow\", style = \"curl\" } }"
    , "\"diagnostic.info\" = { underline = { color = \"blue\", style = \"curl\" } }"
    , "\"diagnostic.hint\" = { underline = { color = \"245\", style = \"curl\" } }"
    , "\"diff.plus\" = \"green\""
    , "\"diff.delta\" = \"yellow\""
    , "\"diff.minus\" = \"167\""
    , "\"diff.plus.staged\" = \"65\""
    , "\"diff.delta.staged\" = \"101\""
    , "\"diff.minus.staged\" = \"95\""
    , ""
    , "\"keyword\" = \"purple\""
    , "\"keyword.operator\" = \"blue\""
    , "\"keyword.directive\" = \"pink\""
    , "\"function\" = \"blue\""
    , "\"function.macro\" = \"pink\""
    , "\"type\" = \"yellow\""
    , "\"constructor\" = \"yellow\""
    , "\"string\" = \"green\""
    , "\"string.special\" = \"orange\""
    , "\"string.regexp\" = \"orange\""
    , "\"comment\" = { fg = \"244\", modifiers = [\"italic\"] }"
    , "\"constant\" = \"209\""
    , "\"constant.character\" = \"green\""
    , "\"constant.character.escape\" = \"orange\""
    , "\"variable.builtin\" = \"pink\""
    , "\"variable.parameter\" = \"252\""
    , "\"variable.other.member\" = \"252\""
    , "\"operator\" = \"blue\""
    , "\"punctuation\" = \"248\""
    , "\"attribute\" = \"yellow\""
    , "\"namespace\" = \"sand\""
    , "\"module\" = \"sand\""
    , "\"label\" = \"purple\""
    , "\"tag\" = \"pink\""
    , "\"special\" = \"orange\""
    , "\"markup.heading\" = { fg = \"blue\", modifiers = [\"bold\"] }"
    , "\"markup.bold\" = { modifiers = [\"bold\"] }"
    , "\"markup.italic\" = { modifiers = [\"italic\"] }"
    , "\"markup.strikethrough\" = { modifiers = [\"crossed_out\"] }"
    , "\"markup.link\" = { fg = \"blue\", modifiers = [\"underlined\"] }"
    , "\"markup.raw\" = \"green\""
    , "\"markup.list\" = \"purple\""
    , "\"markup.quote\" = \"244\""
    , ""
    , "[palette]"
    , "blue = \"110\""
    , "green = \"114\""
    , "yellow = \"179\""
    , "red = \"203\""
    , "purple = \"176\""
    , "pink = \"174\""
    , "orange = \"173\""
    , "sand = \"180\""
    , "dim = \"240\""
    , "bar = \"236\""
    , "selection = \"24\""
    ]
