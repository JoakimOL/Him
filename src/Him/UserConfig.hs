-- | The user's config file (ADR-32): @config.toml@, read with "Him.Toml".
--
-- @
-- [editor]
-- scrolloff = 3
-- theme = "onedark"
-- [editor.search]
-- smart-case = false        # every setting: "Him.Options"
--
-- [keys.normal]
-- "C-s" = "ex w"            # keys = an action invocation
-- "Q" = "no_op"             # unbind
--
-- [language-server.rust]
-- command = "rust-analyzer"
-- @
--
-- Bindings go on top of the defaults ('Him.Config.overrideBindings') and are
-- validated with them; everything else is checked here, and unknown
-- sections or settings are errors (a typo should not pass silently).
-- @him --dump-default-config@ prints every default ('defaultConfigText').
module Him.UserConfig
  ( UserConfig (..)
  , ServerOverride (..)
  , emptyUserConfig
  , parseUserConfig
  , loadUserConfig
  , configPath
  , applyUserConfig
  , applyEditorOptions
  , userOptions
  , defaultConfigText
  , modeSections
  ) where

import Control.Exception (IOException, try)
import Data.Either (partitionEithers)
import Data.List (sortOn)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.IO qualified as TIO
import Him.Action
import Him.Config (Bindings, Config (..))
import Him.Config.Default (allActions, configWith, defaultBindings)
import Him.Editor (Editor (..))
import Him.Json hiding (path)
import Him.Lsp.Config (ServerConfig (..), ServerTable, defaultServers)
import Him.Mode (Mode (..))
import Him.Options
import Him.Paths (configPath)
import Him.Toml (parseToml, quoteKey, quoteString)
import System.Directory (doesFileExist)

data UserConfig = UserConfig
  { ucBindings :: !Bindings
  , ucEditor :: ![(Text, Value)]
  -- ^ Settings by key ('Him.Options.optionSpecs'), already checked.
  , ucTheme :: !(Maybe Text)
  , ucServers :: !(Map Text ServerOverride)
  }
  deriving stock (Eq, Show)

-- | Changes to a language's server; a language without a built-in server
-- needs a command.
data ServerOverride = ServerOverride
  { soCommand :: !(Maybe FilePath)
  , soArgs :: !(Maybe [String])
  , soRoots :: !(Maybe [FilePath])
  , soLanguageId :: !(Maybe Text)
  , soEnabled :: !Bool
  }
  deriving stock (Eq, Show)

emptyUserConfig :: UserConfig
emptyUserConfig = UserConfig Map.empty [] Nothing Map.empty

-- | The sections of @[keys]@, by mode.
modeSections :: [(Text, Mode)]
modeSections =
  [ ("normal", Normal)
  , ("select", Select)
  , ("insert", Insert)
  , ("command", CmdLine)
  , ("picker", Picking)
  , ("directory", Directory)
  , ("completion", Completing)
  ]

-- | Read the config file; a missing file is an empty config.
loadUserConfig :: FilePath -> IO (Either [Text] UserConfig)
loadUserConfig path = do
  exists <- doesFileExist path
  if not exists
    then pure (Right emptyUserConfig)
    else
      try @IOException (TIO.readFile path) >>= \case
        Left e -> pure (Left [T.pack (show e)])
        Right src -> pure (parseUserConfig src)

-- | Check a config file's text; every problem is reported.
parseUserConfig :: Text -> Either [Text] UserConfig
parseUserConfig src = do
  doc <- either (\e -> Left [e]) Right (parseToml src)
  top <- maybe (Left ["the config must be a table"]) Right (asObject doc)
  let results = map section top
      (errs, parts) = partitionEithers results
  if null (concat errs) then Right (foldr ($) emptyUserConfig parts) else Left (concat errs)
  where
    section :: (Text, Value) -> Either [Text] (UserConfig -> UserConfig)
    section = \case
      ("editor", v) -> editor v
      ("keys", v) -> keys v
      ("language-server", v) -> servers v
      (other, _) -> Left ["unknown section [" <> other <> "] (known: editor, keys, language-server)"]
    editor v = do
      kvs <- table "[editor]" v
      fs <- collect (concatMap editorKey kvs)
      Right (foldr (.) id fs)
    -- Settings are checked against the option table; sub-tables
    -- ([editor.search]) give dotted keys.
    editorKey = \case
      ("theme", JString t) | not (T.null t) -> [Right (\c -> c {ucTheme = Just t})]
      ("theme", _) -> [Left ["editor.theme must be a theme's name in quotes, e.g. \"onedark\""]]
      (k, JObject sub) | k `elem` optionSections -> concatMap (\(k', v) -> editorKey (k <> "." <> k', v)) sub
      (k, v) -> case setOption k v defaultOptions of
        Left e -> [Left [e]]
        Right _ -> [Right (\c -> c {ucEditor = ucEditor c <> [(k, v)]})]
    keys v = do
      modes <- table "[keys]" v
      bindings <- collect (map modeKeys modes)
      Right (\c -> c {ucBindings = Map.fromListWith (flip (<>)) bindings})
    modeKeys (name, v) = case lookup name modeSections of
      Nothing -> Left ["unknown mode [keys." <> name <> "] (known: " <> T.intercalate ", " (map fst modeSections) <> ")"]
      Just mode -> do
        kvs <- table ("[keys." <> name <> "]") v
        pairs <- collect [binding name k x | (k, x) <- kvs]
        Right (mode, pairs)
    binding mode k = \case
      JString inv -> Right (k, inv)
      _ -> Left ["keys." <> mode <> "." <> quoteKey k <> ": the value must be an action in quotes, e.g. \"move_line_down\""]
    servers v = do
      langs <- table "[language-server]" v
      overrides <- collect (map server langs)
      Right (\c -> c {ucServers = Map.fromList overrides})
    server (lang, v) = do
      kvs <- table ("[language-server." <> lang <> "]") v
      fs <- collect (map (serverKey lang) kvs)
      Right (lang, foldr ($) (ServerOverride Nothing Nothing Nothing Nothing True) fs)
    serverKey lang = \case
      ("command", JString t) -> Right (\o -> o {soCommand = Just (T.unpack t)})
      ("args", JArray xs) | Just ss <- traverse asText xs -> Right (\o -> o {soArgs = Just (map T.unpack ss)})
      ("roots", JArray xs) | Just ss <- traverse asText xs -> Right (\o -> o {soRoots = Just (map T.unpack ss)})
      ("language-id", JString t) -> Right (\o -> o {soLanguageId = Just t})
      ("enabled", JBool b) -> Right (\o -> o {soEnabled = b})
      (k, _) -> Left ["language-server." <> lang <> "." <> k <> ": unknown setting or wrong type (command: string, args/roots: list of strings, language-id: string, enabled: true/false)"]
    table what v = maybe (Left [what <> " must be a table"]) Right (asObject v)
    collect :: [Either [Text] a] -> Either [Text] [a]
    collect xs = case partitionEithers xs of
      ([], ok) -> Right ok
      (errs, _) -> Left (concat errs)

-- | The configuration with the user's changes: bindings on top of the
-- defaults (validated), servers changed or added.
applyUserConfig :: UserConfig -> Either Text Config
applyUserConfig uc = do
  config <- configWith (ucBindings uc)
  servers <- applyServers (ucServers uc) defaultServers
  pure config {cfgServers = servers}

applyServers :: Map Text ServerOverride -> ServerTable -> Either Text ServerTable
applyServers overrides table0 = foldr step (Right table0) (Map.toList overrides)
  where
    step (lang, o) acc = do
      table <- acc
      if not (soEnabled o)
        then Right (Map.delete lang table)
        else case (Map.lookup lang table, soCommand o) of
          (Nothing, Nothing) -> Left ("language-server." <> lang <> ": no built-in server, so a command is needed")
          (base, cmd) ->
            let b = fromMaybe (ServerConfig "" [] [] lang) base
             in Right $
                  Map.insert
                    lang
                    b
                      { scCommand = fromMaybe (scCommand b) cmd
                      , scArgs = fromMaybe (scArgs b) (soArgs o)
                      , scRoots = fromMaybe (scRoots b) (soRoots o)
                      , scLanguageId = fromMaybe (scLanguageId b) (soLanguageId o)
                      }
                    table

-- | The editor settings of a config, applied to an editor.
applyEditorOptions :: UserConfig -> Editor -> Editor
applyEditorOptions uc ed =
  ed
    { edOptions = userOptions uc
    }

-- | The settings a config file makes (the rest are the defaults).
userOptions :: UserConfig -> Options
userOptions uc = foldl' (\o (k, v) -> either (const o) id (setOption k v o)) defaultOptions (ucEditor uc)

-- | Every default, as a config file (@him --dump-default-config@): editor
-- settings, all bindings per mode (with what they do), the language
-- servers, and a list of all actions.
defaultConfigText :: Text
defaultConfigText =
  T.unlines $
    [ "# him configuration: the defaults, as printed by `him --dump-default-config`."
    , "# Save as ~/.config/him/config.toml (or $XDG_CONFIG_HOME/him/config.toml, or"
    , "# point $HIM_CONFIG at it) and change what you like; :config-reload applies it."
    , "# Anything you leave out keeps its default, so deleting lines is fine."
    , "#"
    , "# Keys: a key or a chord (\"g g\"), with modifiers C- A- S- and names such as"
    , "# ret esc tab backspace space up down pageup pagedown F1. The value is an"
    , "# action with its arguments: \"move_line_down 5\", \"insert_text \\\"// \\\"\", \"ex w\"."
    , "# \"no_op\" unbinds a key. All actions are listed at the end."
    ]
      <> concatMap optionBlock optionSections
      <> concatMap modeBlock modeSections
      <> concatMap serverBlock (Map.toList defaultServers)
      <> [ ""
         , "# Every action, by group (<required> and [optional] arguments):"
         ]
      <> [ "#   " <> T.justifyLeft 34 ' ' (actName a <> signature a) <> " " <> actDoc a
         | a <- sortOn actGroup allActions
         ]
  where
    optionBlock section =
      [ ""
      , if T.null section then "[editor]" else "[editor." <> section <> "]"
      ]
        <> ["theme = \"default\"  # any Helix theme, or your own in themes/ next to this file (:theme)" | T.null section]
        <> [ T.justifyLeft 34 ' ' (name <> " = " <> osShow spec defaultOptions) <> " # " <> osDoc spec
           | spec <- optionSpecs
           , let (prefix, name) = T.breakOnEnd "." (osKey spec)
           , T.dropEnd 1 prefix == section
           ]
    modeBlock (name, mode) =
      [ ""
      , "[keys." <> name <> "]" <> modeNote mode
      ]
        <> [ T.justifyLeft 30 ' ' (quoteString keys <> " = " <> quoteString inv) <> describe inv
           | (keys, inv) <- Map.findWithDefault [] mode defaultBindings
           ]
    modeNote = \case
      Select -> "   # select mode also has every normal-mode key"
      Directory -> "   # in a directory listing; also every normal-mode key"
      Completing -> "   # insert mode with the completion menu open; also every insert-mode key"
      _ -> ""
    describe inv = case parseInvocation inv >>= \i -> maybe (Left "") Right (lookupAction (invAction i) registry) of
      Right a -> " # " <> actDoc a
      Left _ -> ""
    registry = either (error . T.unpack) id (mkActionRegistry allActions)
    signature a = T.concat [" " <> shape p | p <- actParams a]
    shape p = case paramDefault p of
      Nothing -> "<" <> paramName p <> ">"
      Just _ -> "[" <> paramName p <> "]"
    serverBlock (lang, sc) =
      [ ""
      , "[language-server." <> quoteKey lang <> "]"
      , "command = " <> quoteString (T.pack (scCommand sc))
      , "args = " <> list (scArgs sc)
      , "roots = " <> list (scRoots sc) <> "   # files that mark a project's root"
      , "language-id = " <> quoteString (scLanguageId sc)
      ]
    list xs = "[" <> T.intercalate ", " (map (quoteString . T.pack) xs) <> "]"
