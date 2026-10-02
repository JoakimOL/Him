-- | Colours and styles used by the render components.
module Him.Render.Theme
  ( Theme (..)
  , defaultTheme
  ) where

import Him.Mode (Mode (..))
import Him.Terminal.Ansi

data Theme = Theme
  { themeText :: Style
  , themeSelection :: Style
  , themeCursor :: Style
  -- ^ Secondary cursors (the primary one is the terminal cursor).
  , themeTilde :: Style
  , themeGutter :: Style
  , themeGutterCurrent :: Style
  , themeStatusLine :: Style
  , themeMode :: Mode -> Style
  , themeInfo :: Style
  , themePopup :: Style
  , themePopupKey :: Style
  , themeError :: Style
  }

defaultTheme :: Theme
defaultTheme =
  Theme
    { themeText = defaultStyle
    , themeSelection = defaultStyle {styleBg = Indexed 24}
    , themeCursor = defaultStyle {styleReverse = True}
    , themeTilde = defaultStyle {styleFg = Indexed 240}
    , themeGutter = defaultStyle {styleFg = Indexed 240}
    , themeGutterCurrent = defaultStyle {styleFg = Indexed 250}
    , themeStatusLine = defaultStyle {styleBg = Indexed 236, styleFg = Indexed 252}
    , themeMode = \m -> defaultStyle {styleBold = True, styleFg = Indexed 235, styleBg = modeColor m}
    , themeInfo = defaultStyle
    , themePopup = defaultStyle {styleBg = Indexed 236, styleFg = Indexed 252}
    , themePopupKey = defaultStyle {styleBg = Indexed 236, styleFg = Indexed 110, styleBold = True}
    , themeError = defaultStyle {styleFg = Ansi 9}
    }
  where
    modeColor = \case
      Normal -> Indexed 110
      Insert -> Indexed 150
      Select -> Indexed 180
      CmdLine -> Indexed 176
