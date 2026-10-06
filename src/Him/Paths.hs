-- | Where him's files are: the config file, the user's themes, and the
-- runtime directories (grammars, queries, Helix's themes).
module Him.Paths
  ( configPath
  , runtimeDirs
  , ownRuntimeDirs
  , himRuntimeDir
  , themeDirs
  , stateDir
  ) where

import Control.Exception (IOException, try)
import System.Directory (getHomeDirectory)
import System.Environment (lookupEnv)
import System.FilePath (takeDirectory, (</>))

-- | Where the config file is: @$HIM_CONFIG@, @$XDG_CONFIG_HOME/him/config.toml@
-- or @~/.config/him/config.toml@.
configPath :: IO FilePath
configPath =
  lookupEnv "HIM_CONFIG" >>= \case
    Just p | not (null p) -> pure p
    _ ->
      lookupEnv "XDG_CONFIG_HOME" >>= \case
        Just xdg | not (null xdg) -> pure (xdg </> "him" </> "config.toml")
        _ -> (</> ".config/him/config.toml") <$> getHomeDirectory

-- | Where him keeps what it remembers between runs (plugins' files):
-- @$HIM_STATE@, @$XDG_STATE_HOME/him@ or @~/.local/state/him@.
stateDir :: IO FilePath
stateDir =
  lookupEnv "HIM_STATE" >>= \case
    Just p | not (null p) -> pure p
    _ ->
      lookupEnv "XDG_STATE_HOME" >>= \case
        Just xdg | not (null xdg) -> pure (xdg </> "him")
        _ -> (</> ".local/state/him") <$> getHomeDirectory

-- | Where @him --grammar@ puts grammars: @$HIM_RUNTIME@ or
-- @~/.config/him/runtime@.
himRuntimeDir :: IO FilePath
himRuntimeDir =
  lookupEnv "HIM_RUNTIME" >>= \case
    Just p | not (null p) -> pure p
    _ -> (</> ".config/him/runtime") <$> getHomeDirectory

-- | The runtime directories with files made for him (grammars it built,
-- queries the user put there), in order: @$HIM_RUNTIME@, then him's own.
ownRuntimeDirs :: IO [FilePath]
ownRuntimeDirs = do
  env <- lookupEnv "HIM_RUNTIME"
  home <- either (const "") id <$> try @IOException getHomeDirectory
  pure (filter (not . null) (maybe [] pure env <> [home </> ".config/him/runtime" | not (null home)]))

-- | The runtime directories, in order: @$HIM_RUNTIME@, him's own, then
-- Helix's (for its themes).
runtimeDirs :: IO [FilePath]
runtimeDirs = do
  env <- lookupEnv "HIM_RUNTIME"
  home <- either (const "") id <$> try @IOException getHomeDirectory
  pure $
    filter (not . null) $
      maybe [] pure env
        <> [home </> ".config/him/runtime", home </> ".config/helix/runtime", "/usr/lib/helix/runtime", "/usr/share/helix/runtime"]

-- | Where themes are looked for, first match wins: @themes/@ next to the
-- config file, then each runtime directory's @themes/@ (Helix's themes).
themeDirs :: IO [FilePath]
themeDirs = do
  config <- configPath
  runtime <- runtimeDirs
  pure ((takeDirectory config </> "themes") : map (</> "themes") runtime)
