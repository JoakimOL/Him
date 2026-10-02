-- | Colours and styles used by the render components.
module Him.Render.Theme
  ( Theme (..)
  , defaultTheme
  ) where

import Him.GitState (SignKind (..))
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
  , themePopupSelected :: Style
  , themePopupDetail :: Style
  -- ^ Only its foreground is used, over the row's background.
  , themeGitSign :: SignKind -> Bool -> Style
  -- ^ Gutter signs by kind; the flag is "staged" (drawn dimmer).
  , themeDirectory :: Style
  -- ^ Directory entries in a listing.
  , themeDirectoryHeader :: Style
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
    , themePopupSelected = defaultStyle {styleBg = Indexed 24, styleFg = Indexed 255}
    , themePopupDetail = defaultStyle {styleFg = Indexed 245}
    , themeGitSign = \kind staged -> defaultStyle {styleFg = gitColor kind staged}
    , themeDirectory = defaultStyle {styleFg = Indexed 110, styleBold = True}
    , themeDirectoryHeader = defaultStyle {styleFg = Indexed 180, styleBold = True}
    , themeError = defaultStyle {styleFg = Ansi 9}
    }
  where
    gitColor kind staged = Indexed $ case (kind, staged) of
      (SignAdded, False) -> 114
      (SignChanged, False) -> 179
      (SignRemoved, False) -> 167
      (SignAdded, True) -> 65
      (SignChanged, True) -> 101
      (SignRemoved, True) -> 95
    modeColor = \case
      Normal -> Indexed 110
      Insert -> Indexed 150
      Select -> Indexed 180
      CmdLine -> Indexed 176
      Picking -> Indexed 176
      Directory -> Indexed 110
