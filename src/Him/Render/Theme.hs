-- | Colours and styles used by the render components: a theme's scope
-- styles ("Him.Theme"), with the ones the UI uses looked up once
-- ('fromScopes') so drawing does not search the map per cell.
module Him.Render.Theme
  ( Theme (..)
  , fromScopes
  , defaultTheme
  , scopeStyle
  ) where

import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Text qualified as T
import Him.GitState (SignKind (..))
import Him.Lsp.Protocol (Severity (..))
import Him.Mode (Mode (..))
import Him.Terminal.Ansi
import Him.Theme (defaultThemeText, parseThemeFile, resolveTheme)

data Theme = Theme
  { themeName :: Text
  , themeScopes :: Map Text Style
  -- ^ Every scope's style (Helix's names); see 'scopeStyle'.
  , themeForeground :: Color
  , themeBackground :: Color
  -- ^ The terminal's default colours while him runs (@ui.text@'s
  -- foreground, @ui.background@'s background); 'DefaultColor' keeps the
  -- terminal's own.
  , themeText :: Style
  , themeSelection :: Style
  , themeSelectionPrimary :: Style
  , themeCursor :: Style
  -- ^ Secondary cursors (the primary one is the terminal cursor).
  , themeTilde :: Style
  , themeGutter :: Style
  , themeGutterCurrent :: Style
  , themeStatusLine :: Style
  , themeStatusLineInactive :: Style
  -- ^ The status lines of windows that are not focused.
  , themeWindow :: Style
  -- ^ The border between windows side by side.
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
  -- ^ Gutter signs of diagnostics.
  , themeDiagnosticText :: Severity -> Style
  -- ^ Laid over the text a diagnostic covers (an underline).
  , themeDirectory :: Style
  -- ^ Directory entries in a listing.
  , themeDirectoryHeader :: Style
  , themeError :: Style
  }

-- | The style for a scope, by its longest known prefix:
-- @keyword.control.import@, then @keyword.control@, then @keyword@.
scopeStyle :: Theme -> Text -> Maybe Style
scopeStyle theme = lookupScope (themeScopes theme)

lookupScope :: Map Text Style -> Text -> Maybe Style
lookupScope scopes = go
  where
    go scope = case Map.lookup scope scopes of
      Just st -> Just st
      Nothing
        | T.any (== '.') scope -> go (T.dropEnd 1 (T.dropWhileEnd (/= '.') scope))
        | otherwise -> Nothing

-- | A theme from its scope styles. The UI scopes are Helix's (@ui.text@,
-- @ui.statusline.insert@, @ui.menu.selected@, @diff.plus@, …), plus a few
-- of him's own that fall back to them (@ui.statusline.command@,
-- @ui.statusline.picker@, @ui.popup.key@, @diff.plus.staged@).
fromScopes :: Text -> Map Text Style -> Theme
fromScopes name scopes =
  Theme
    { themeName = name
    , themeScopes = scopes
    , themeForeground = styleFg text
    , themeBackground = styleBg (get "ui.background")
    , themeText = text
    , themeSelection = get "ui.selection"
    , themeSelectionPrimary = get "ui.selection.primary"
    , themeCursor = fromMaybe defaultStyle {styleReverse = True} (lookupScope scopes "ui.cursor")
    , themeTilde = fromMaybe (get "ui.linenr") (exact "ui.virtual")
    , themeGutter = get "ui.gutter" `patchStyle` get "ui.linenr"
    , themeGutterCurrent = get "ui.gutter.selected" `patchStyle` get "ui.linenr.selected"
    , themeStatusLine = statusLine
    , themeStatusLineInactive = statusLine `patchStyle` fromMaybe defaultStyle {styleDim = True} (exact "ui.statusline.inactive")
    , themeWindow = fromMaybe (get "ui.linenr") (exact "ui.window")
    , themeMode = modeStyle
    , themeInfo = text
    , themePopup = popup
    , themePopupKey = popup `patchStyle` fromMaybe (foreground (get "ui.text.focus")) {styleBold = True} (exact "ui.popup.key")
    , themePopupSelected = popup `patchStyle` get "ui.menu.selected"
    , themePopupDetail = foreground (fromMaybe (get "comment") (exact "ui.text.inactive"))
    , themeGitSign = gitSign
    , themeDiagnostic = get . severityName
    , themeDiagnosticText = diagnosticText
    , themeDirectory = fromMaybe text {styleBold = True} (exact "ui.text.directory")
    , themeDirectoryHeader = fromMaybe text {styleBold = True} (exact "ui.text.focus")
    , themeError = get "error"
    }
  where
    get = fromMaybe defaultStyle . lookupScope scopes
    exact = (`Map.lookup` scopes)
    text = get "ui.text"
    -- Only the colour of the text and its modifiers, not a background.
    foreground st = st {styleBg = DefaultColor}
    popup = text `patchStyle` get "ui.popup"
    statusLine = get "ui.statusline"
    modeStyle m =
      let (own, base) = case m of
            Normal -> ("normal", "normal")
            Insert -> ("insert", "insert")
            Select -> ("select", "select")
            CmdLine -> ("command", "normal")
            Picking -> ("picker", "normal")
            Directory -> ("directory", "normal")
            Completing -> ("completion", "insert")
       in statusLine `patchStyle` fromMaybe (get ("ui.statusline." <> base)) (exact ("ui.statusline." <> own))
    gitSign kind staged =
      let scope = case kind of
            SignAdded -> "diff.plus"
            SignChanged -> "diff.delta"
            SignRemoved -> "diff.minus"
          sign = fromMaybe (get scope) (exact (scope <> ".gutter"))
       in if staged then fromMaybe sign {styleDim = True} (exact (scope <> ".staged")) else sign
    severityName = \case
      SevError -> "error"
      SevWarning -> "warning"
      SevInfo -> "info"
      SevHint -> "hint"
    -- A diagnostic without an underline in the theme still gets one, in
    -- its gutter colour.
    diagnosticText sev =
      let st = get ("diagnostic." <> severityName sev)
       in if styleUnderline st == NoUnderline
            then st {styleUnderline = UnderlineLine, styleUnderlineColor = styleFg (get (severityName sev))}
            else st

-- | The built-in theme ('defaultThemeText').
defaultTheme :: Theme
defaultTheme = case parseThemeFile defaultThemeText of
  Right tf -> fromScopes "default" (fst (resolveTheme tf))
  Left e -> error ("the built-in theme does not parse: " <> T.unpack e)
