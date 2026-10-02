-- | Colours and styles used by the render components.
module Him.Render.Theme
  ( Theme (..)
  , defaultTheme
  , scopeStyle
  ) where

import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Text (Text)
import Data.Text qualified as T
import Him.GitState (SignKind (..))
import Him.Lsp.Protocol (Severity (..))
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
  , themeDiagnostic :: Severity -> Style
  -- ^ Gutter signs and underlines of diagnostics.
  , themeScopes :: Map Text Style
  -- ^ Styles of syntax scopes (Helix's names); see 'scopeStyle'.
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
    , themeDiagnostic = \sev -> defaultStyle {styleFg = Indexed (diagColor sev)}
    , themeScopes = defaultScopes
    , themeDirectory = defaultStyle {styleFg = Indexed 110, styleBold = True}
    , themeDirectoryHeader = defaultStyle {styleFg = Indexed 180, styleBold = True}
    , themeError = defaultStyle {styleFg = Ansi 9}
    }
  where
    diagColor = \case
      SevError -> 203
      SevWarning -> 179
      SevInfo -> 110
      SevHint -> 245
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
      Completing -> Indexed 150

-- | The style for a scope, by its longest known prefix:
-- @keyword.control.import@, then @keyword.control@, then @keyword@.
scopeStyle :: Theme -> Text -> Maybe Style
scopeStyle theme = go
  where
    go scope = case Map.lookup scope (themeScopes theme) of
      Just st -> Just st
      Nothing
        | T.any (== '.') scope -> go (T.dropEnd 1 (T.dropWhileEnd (/= '.') scope))
        | otherwise -> Nothing

defaultScopes :: Map Text Style
defaultScopes =
  Map.fromList
    [ ("keyword", fg 176)
    , ("keyword.control", fg 176)
    , ("keyword.operator", fg 110)
    , ("keyword.directive", fg 174)
    , ("function", fg 110)
    , ("function.builtin", fg 110)
    , ("function.macro", fg 174)
    , ("type", fg 179)
    , ("type.builtin", fg 179)
    , ("constructor", fg 179)
    , ("string", fg 114)
    , ("string.special", fg 173)
    , ("string.regexp", fg 173)
    , ("comment", (fg 244) {styleItalic = True})
    , ("constant", fg 209)
    , ("constant.numeric", fg 209)
    , ("constant.character", fg 114)
    , ("constant.character.escape", fg 173)
    , ("variable.builtin", fg 174)
    , ("variable.parameter", fg 252)
    , ("variable.other.member", fg 252)
    , ("operator", fg 110)
    , ("punctuation", fg 248)
    , ("attribute", fg 179)
    , ("namespace", fg 180)
    , ("module", fg 180)
    , ("label", fg 176)
    , ("tag", fg 174)
    , ("special", fg 173)
    , ("markup.heading", (fg 110) {styleBold = True})
    , ("markup.bold", defaultStyle {styleBold = True})
    , ("markup.italic", defaultStyle {styleItalic = True})
    , ("markup.link", (fg 110) {styleUnderline = True})
    , ("markup.raw", fg 114)
    , ("markup.list", fg 176)
    , ("markup.quote", fg 244)
    , ("diff.plus", fg 114)
    , ("diff.minus", fg 167)
    , ("diff.delta", fg 179)
    ]
  where
    fg n = defaultStyle {styleFg = Indexed n}
