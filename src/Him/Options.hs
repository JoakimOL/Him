-- | The editor's settings (the @[editor]@ section of the config file,
-- ADR-34), and one table describing them: each setting's key, doc, type
-- and how it is read, so checking, applying and @--dump-default-config@
-- cannot drift apart.
module Him.Options
  ( Options (..)
  , defaultOptions
  , LineNumbers (..)
  , CursorKind (..)
  , cursorKindFor
  , OptionSpec (..)
  , optionSpecs
  , setOption
  , optionSections
  ) where

import Data.List (find)
import Data.Text (Text)
import Data.Text qualified as T
import Him.Json (Value (..))
import Him.Mode (Mode (..))

data LineNumbers = LineNumbersAbsolute | LineNumbersRelative | LineNumbersOff
  deriving stock (Eq, Show)

-- | A cursor's shape; the terminal frontend maps it to its escape codes.
data CursorKind = CursorKindBlock | CursorKindBar | CursorKindUnderline
  deriving stock (Eq, Show)

data Options = Options
  { optScrolloff :: !Int
  , optShowHidden :: !Bool
  -- ^ Dotfiles in directory listings (@g .@ toggles it).
  , optTabWidth :: !Int
  , optExpandTab :: !Bool
  -- ^ @tab@ in insert mode inserts spaces up to the next tab stop.
  , optLineNumbers :: !LineNumbers
  , optCursorNormal :: !CursorKind
  , optCursorInsert :: !CursorKind
  , optCursorSelect :: !CursorKind
  , optCursorCommand :: !CursorKind
  -- ^ On the @:@ line and in pickers.
  , optAutoCompletion :: !Bool
  , optCompletionTriggerLen :: !Int
  , optAutoSignatureHelp :: !Bool
  , optHoverLines :: !Int
  , optSmartCase :: !Bool
  , optWrapAround :: !Bool
  , optPickerHidden :: !Bool
  , optPickerGitIgnore :: !Bool
  , optPickerIgnore :: !Bool
  , optPickerFollowSymlinks :: !Bool
  , optPickerMaxFiles :: !Int
  , optPreview :: !Bool
  , optPreviewMinWidth :: !Int
  , optPreviewMaxSize :: !Int
  -- ^ In bytes; larger files are not read for the preview.
  , optEscapeTimeout :: !Int
  -- ^ Milliseconds to wait after @esc@ for the rest of an escape sequence.
  }
  deriving stock (Eq, Show)

defaultOptions :: Options
defaultOptions =
  Options
    { optScrolloff = 3
    , optShowHidden = False
    , optTabWidth = 4
    , optExpandTab = False
    , optLineNumbers = LineNumbersAbsolute
    , optCursorNormal = CursorKindBlock
    , optCursorInsert = CursorKindBar
    , optCursorSelect = CursorKindBlock
    , optCursorCommand = CursorKindBar
    , optAutoCompletion = True
    , optCompletionTriggerLen = 2
    , optAutoSignatureHelp = True
    , optHoverLines = 30
    , optSmartCase = True
    , optWrapAround = True
    , optPickerHidden = False
    , optPickerGitIgnore = True
    , optPickerIgnore = True
    , optPickerFollowSymlinks = True
    , optPickerMaxFiles = 500000
    , optPreview = True
    , optPreviewMinWidth = 60
    , optPreviewMaxSize = 20 * 1024 * 1024
    , optEscapeTimeout = 30
    }

-- | The cursor shape in a mode.
cursorKindFor :: Options -> Mode -> CursorKind
cursorKindFor o = \case
  Normal -> optCursorNormal o
  Directory -> optCursorNormal o
  Insert -> optCursorInsert o
  Completing -> optCursorInsert o
  Select -> optCursorSelect o
  CmdLine -> optCursorCommand o
  Picking -> optCursorCommand o

-- | One setting: its key below @[editor]@ (@"tab-width"@, or
-- @"search.smart-case"@ for a sub-table), what it does, how to read a
-- value, and its value in some options (as TOML, for the dump).
data OptionSpec = OptionSpec
  { osKey :: !Text
  , osDoc :: !Text
  , osSet :: Value -> Options -> Either Text Options
  , osShow :: Options -> Text
  }

-- | The sub-tables, in the order the dump writes them (@""@ is @[editor]@
-- itself).
optionSections :: [Text]
optionSections = ["", "cursor-shape", "lsp", "search", "file-picker", "picker"]

optionSpecs :: [OptionSpec]
optionSpecs =
  [ int "scrolloff" "lines kept visible above and below the cursor" 0 optScrolloff (\v o -> o {optScrolloff = v})
  , bool "show-hidden-files" "dotfiles in directory listings (g . toggles)" optShowHidden (\v o -> o {optShowHidden = v})
  , int "tab-width" "columns between tab stops" 1 optTabWidth (\v o -> o {optTabWidth = v})
  , bool "expand-tab" "tab inserts spaces instead of a tab character" optExpandTab (\v o -> o {optExpandTab = v})
  , choice "line-number" "absolute, relative or off" [("absolute", LineNumbersAbsolute), ("relative", LineNumbersRelative), ("off", LineNumbersOff)] optLineNumbers (\v o -> o {optLineNumbers = v})
  , int "escape-timeout" "milliseconds to wait after esc for a key sequence (read at startup)" 0 optEscapeTimeout (\v o -> o {optEscapeTimeout = v})
  , cursor "normal" optCursorNormal (\v o -> o {optCursorNormal = v})
  , cursor "insert" optCursorInsert (\v o -> o {optCursorInsert = v})
  , cursor "select" optCursorSelect (\v o -> o {optCursorSelect = v})
  , cursor "command" optCursorCommand (\v o -> o {optCursorCommand = v})
  , bool "lsp.auto-completion" "the completion menu opens while typing (C-x opens it anyway)" optAutoCompletion (\v o -> o {optAutoCompletion = v})
  , int "lsp.completion-trigger-len" "characters of a word typed before completion opens" 1 optCompletionTriggerLen (\v o -> o {optCompletionTriggerLen = v})
  , bool "lsp.auto-signature-help" "signature help after ( and ," optAutoSignatureHelp (\v o -> o {optAutoSignatureHelp = v})
  , int "lsp.hover-lines" "most lines a hover shows" 1 optHoverLines (\v o -> o {optHoverLines = v})
  , bool "search.smart-case" "ignore case unless the pattern has an upper-case letter" optSmartCase (\v o -> o {optSmartCase = v})
  , bool "search.wrap-around" "searches continue from the other end of the file" optWrapAround (\v o -> o {optWrapAround = v})
  , bool "file-picker.hidden" "list hidden files (dotfiles) too" optPickerHidden (\v o -> o {optPickerHidden = v})
  , bool "file-picker.git-ignore" "skip what .gitignore and .git/info/exclude ignore" optPickerGitIgnore (\v o -> o {optPickerGitIgnore = v})
  , bool "file-picker.ignore" "skip what .ignore files ignore" optPickerIgnore (\v o -> o {optPickerIgnore = v})
  , bool "file-picker.follow-symlinks" "enter linked directories" optPickerFollowSymlinks (\v o -> o {optPickerFollowSymlinks = v})
  , int "file-picker.max-files" "stop listing after this many files" 1 optPickerMaxFiles (\v o -> o {optPickerMaxFiles = v})
  , bool "picker.preview" "show the file beside the list" optPreview (\v o -> o {optPreview = v})
  , int "picker.preview-min-width" "narrowest picker that still shows the preview" 0 optPreviewMinWidth (\v o -> o {optPreviewMinWidth = v})
  , int "picker.preview-max-size" "bytes; larger files are not previewed" 0 optPreviewMaxSize (\v o -> o {optPreviewMaxSize = v})
  ]
  where
    int :: Text -> Text -> Int -> (Options -> Int) -> (Int -> Options -> Options) -> OptionSpec
    int key doc low get set =
      OptionSpec key (doc <> " (a number, at least " <> T.pack (show low) <> ")") (\v o -> case v of
        JInt n | n >= toInteger low, n <= toInteger (maxBound :: Int) -> Right (set (fromInteger n) o)
        _ -> Left ("editor." <> key <> " must be a whole number, at least " <> T.pack (show low))) (T.pack . show . get)
    bool :: Text -> Text -> (Options -> Bool) -> (Bool -> Options -> Options) -> OptionSpec
    bool key doc get set =
      OptionSpec key doc (\v o -> case v of
        JBool b -> Right (set b o)
        _ -> Left ("editor." <> key <> " must be true or false")) (\o -> if get o then "true" else "false")
    choice :: Eq a => Text -> Text -> [(Text, a)] -> (Options -> a) -> (a -> Options -> Options) -> OptionSpec
    choice key doc names get set =
      OptionSpec key doc (\v o -> case v of
        JString t | Just x <- lookup t names -> Right (set x o)
        _ -> Left ("editor." <> key <> " must be one of " <> T.intercalate ", " (map (quote . fst) names))) (\o -> maybe "?" (quote . fst) (find ((== get o) . snd) names))
    cursor mode = choice ("cursor-shape." <> mode) ("in " <> mode <> " mode: block, bar or underline" <> if mode == "command" then " (also pickers)" else "") [("block", CursorKindBlock), ("bar", CursorKindBar), ("underline", CursorKindUnderline)]
    quote t = "\"" <> t <> "\""

-- | Set one setting by its key; an unknown key is an error that lists
-- the known ones of its table.
setOption :: Text -> Value -> Options -> Either Text Options
setOption key v o = case find ((== key) . osKey) optionSpecs of
  Just spec -> osSet spec v o
  Nothing ->
    let (section, _) = T.breakOnEnd "." key
        known = [T.drop (T.length section) (osKey s) | s <- optionSpecs, section `T.isPrefixOf` osKey s, not (T.any (== '.') (T.drop (T.length section) (osKey s)))]
        table = if T.null section then "editor" else "editor." <> T.dropEnd 1 section
     in Left ("unknown setting editor." <> key <> " (known in [" <> table <> "]: " <> T.intercalate ", " known <> ")")
