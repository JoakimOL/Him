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
  , applyUserConfigWith
  , enabledPlugins
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
import Him.Config (Bindings, Config (..), Plugin (..))
import Data.Set qualified as Set
import Him.Config.Default (allActions, allPlugins, configWith, defaultBindings, pluginOf, plugins)
import Him.Editor (Editor (..))
import Him.Json hiding (path)
import Him.Lsp.Config (ServerConfig (..), ServerTable, defaultServers)
import Him.Mode (Mode (..))
import Him.Options
import Him.Paths (configPath)
import Him.Repl (ReplConfig (..), ReplTable, defaultRepls)
import Him.Chat (ChatConfig (..), defaultChatConfig)
import Him.Toml (parseToml, quoteKey, quoteString)
import System.Directory (doesFileExist)

data UserConfig = UserConfig
  { ucBindings :: !Bindings
  , ucEditor :: ![(Text, Value)]
  -- ^ Settings by key ('Him.Options.optionSpecs'), already checked.
  , ucTheme :: !(Maybe Text)
  , ucPlugins :: !(Map Text Bool)
  -- ^ Plugins switched on or off (@[plugins]@); the rest are on.
  , ucServers :: !(Map Text ServerOverride)
  , ucRepls :: !(Map Text ReplOverride)
  , ucChat :: !ChatConfig
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
emptyUserConfig = UserConfig Map.empty [] Nothing Map.empty Map.empty Map.empty defaultChatConfig

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
  , ("repl", Repl)
  , ("chat", Chat)
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
      ("plugins", v) -> pluginSection v
      ("repl", v) -> repls v
      ("chat", v) -> chat v
      (other, _) -> Left ["unknown section [" <> other <> "] (known: editor, keys, language-server, repl, chat, plugins)"]
    chat v = do
      kvs <- table "[chat]" v
      fs <- collect (map chatKey kvs)
      Right (\c -> c {ucChat = foldr ($) (ucChat c) fs})
    chatKey = \case
      ("provider", JString t) -> Right (\cc -> cc {ccProvider = t})
      ("model", JString t) -> Right (\cc -> cc {ccModel = t})
      ("effort", JString t) | t `elem` ["low", "medium", "high", "xhigh", "max"] -> Right (\cc -> cc {ccEffort = t})
      ("max-tokens", JInt n) | n > 0 -> Right (\cc -> cc {ccMaxTokens = fromInteger n})
      (k, _) -> Left ["chat." <> k <> ": unknown setting or wrong value (provider, model: strings; effort: low, medium, high, xhigh or max; max-tokens: a number)"]
    repls v = do
      langs <- table "[repl]" v
      overrides <- collect (map repl langs)
      Right (\c -> c {ucRepls = Map.fromList overrides})
    repl (lang, v) = do
      kvs <- table ("[repl." <> lang <> "]") v
      fs <- collect (map (replKey lang) kvs)
      Right (lang, foldr ($) (ReplOverride Nothing Nothing Nothing Nothing Nothing Nothing True) fs)
    replKey lang = \case
      ("command", JString t) -> Right (\o -> o {roCommand = Just (T.unpack t)})
      ("args", JArray xs) | Just ss <- traverse asText xs -> Right (\o -> o {roArgs = Just (map T.unpack ss)})
      ("roots", JArray xs) | Just ss <- traverse asText xs -> Right (\o -> o {roRoots = Just (map T.unpack ss)})
      ("multiline", JArray [JString a, JString b]) -> Right (\o -> o {roMultiline = Just (Just (a, b))})
      ("multiline", JArray []) -> Right (\o -> o {roMultiline = Just Nothing})
      ("reload", JString t) -> Right (\o -> o {roReload = Just (if T.null t then Nothing else Just t)})
      ("reload-on-save", JBool b) -> Right (\o -> o {roReloadOnSave = Just b})
      ("enabled", JBool b) -> Right (\o -> o {roEnabled = b})
      (k, _) -> Left ["repl." <> lang <> "." <> k <> ": unknown setting or wrong type (command: string, args/roots: list of strings, multiline: [start, end] or [], reload: string, reload-on-save/enabled: true/false)"]
    pluginSection v = do
      kvs <- table "[plugins]" v
      ps <- collect (map pluginKey kvs)
      Right (\c -> c {ucPlugins = Map.fromList ps})
    pluginKey = \case
      (name, JBool b) | Set.member name allPlugins -> Right (name, b)
      (name, _) | Set.member name allPlugins -> Left ["plugins." <> name <> " must be true or false"]
      (name, _) -> Left ["unknown plugin " <> name <> " (known: " <> T.intercalate ", " (Set.toList allPlugins) <> ")"]
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
applyUserConfig uc = applyUserConfigWith (enabledPlugins uc) uc

-- | The plugins a config switches on: all but those it turns off.
enabledPlugins :: UserConfig -> Set.Set Text
enabledPlugins uc = Set.filter (\name -> Map.findWithDefault True name (ucPlugins uc)) allPlugins

-- | The same with other plugins on (switching them while running).
applyUserConfigWith :: Set.Set Text -> UserConfig -> Either Text Config
applyUserConfigWith enabled uc = do
  config <- configWith enabled (ucBindings uc)
  servers <- applyServers (ucServers uc) defaultServers
  repls <- applyRepls (ucRepls uc) defaultRepls
  pure config {cfgServers = servers, cfgRepls = repls, cfgChat = ucChat uc}

-- | Changes to a language's REPL; a language without a built-in one needs
-- a command.
data ReplOverride = ReplOverride
  { roCommand :: !(Maybe FilePath)
  , roArgs :: !(Maybe [String])
  , roRoots :: !(Maybe [FilePath])
  , roMultiline :: !(Maybe (Maybe (Text, Text)))
  , roReload :: !(Maybe (Maybe Text))
  , roReloadOnSave :: !(Maybe Bool)
  , roEnabled :: !Bool
  }
  deriving stock (Eq, Show)

applyRepls :: Map Text ReplOverride -> ReplTable -> Either Text ReplTable
applyRepls overrides table0 = foldr step (Right table0) (Map.toList overrides)
  where
    step (lang, o) acc = do
      table <- acc
      if not (roEnabled o)
        then Right (Map.delete lang table)
        else case (Map.lookup lang table, roCommand o) of
          (Nothing, Nothing) -> Left ("repl." <> lang <> ": no built-in REPL, so a command is needed")
          (base, cmd) ->
            let b = fromMaybe (ReplConfig "" [] [] Nothing Nothing False) base
             in Right $
                  Map.insert
                    lang
                    ReplConfig
                      { rcCommand = fromMaybe (rcCommand b) cmd
                      , rcArgs = fromMaybe (rcArgs b) (roArgs o)
                      , rcRoots = fromMaybe (rcRoots b) (roRoots o)
                      , rcMultiline = fromMaybe (rcMultiline b) (roMultiline o)
                      , rcReload = fromMaybe (rcReload b) (roReload o)
                      , rcReloadOnSave = fromMaybe (rcReloadOnSave b) (roReloadOnSave o)
                      }
                    table

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
      <> [ ""
         , "[plugins]   # features that can be switched off (also :plugin-disable, :plugin-enable)"
         ]
      <> [T.justifyLeft 34 ' ' (plName p <> " = true") <> " # " <> plDoc p | p <- plugins]
      <> concatMap modeBlock modeSections
      <> concatMap serverBlock (Map.toList defaultServers)
      <> concatMap replBlock (Map.toList defaultRepls)
      <> [ ""
         , "[chat]   # the AI chat plugin (space c c)"
         , "# provider: \"claude-code\" (the claude program you are logged in to) or \"anthropic\" (the API; ANTHROPIC_API_KEY)"
         , "provider = " <> quoteString (ccProvider defaultChatConfig)
         , "model = " <> quoteString (ccModel defaultChatConfig)
         , "effort = " <> quoteString (ccEffort defaultChatConfig) <> "   # low, medium, high, xhigh or max"
         , "max-tokens = " <> T.pack (show (ccMaxTokens defaultChatConfig))
         ]
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
      Repl -> "   # insert mode in a REPL buffer; also every insert-mode key"
      Chat -> "   # insert mode in the chat buffer; also every insert-mode key"
      Completing -> "   # insert mode with the completion menu open; also every insert-mode key"
      _ -> ""
    describe inv = case parseInvocation inv >>= \i -> maybe (Left "") Right (lookupAction (invAction i) registry) of
      Right a -> " # " <> maybe "" (\p -> "(" <> plName p <> ") ") (pluginOf (actName a)) <> actDoc a
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
    replBlock (lang, rc) =
      [ ""
      , "[repl." <> quoteKey lang <> "]   # :repl, space e (send the selection), space E (reload)"
      , "command = " <> quoteString (T.pack (rcCommand rc))
      , "args = " <> list (rcArgs rc)
      , "roots = " <> list (rcRoots rc) <> "   # where it runs: the project's root"
      , "multiline = " <> maybe "[]" (\(a, b) -> "[" <> quoteString a <> ", " <> quoteString b <> "]") (rcMultiline rc) <> "   # put around code of several lines"
      , "reload = " <> quoteString (fromMaybe "" (rcReload rc))
      , "reload-on-save = " <> (if rcReloadOnSave rc then "true" else "false")
      ]
    list xs = "[" <> T.intercalate ", " (map (quoteString . T.pack) xs) <> "]"
