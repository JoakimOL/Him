-- | Where him's files are: the config file, the user's themes, and the
-- runtime directories (grammars, queries, Helix's themes).
module Him.Paths
  ( configPath
  , runtimeDirs
  , themeDirs
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

-- | The runtime directories, in order: @$HIM_RUNTIME@, him's own, then
-- Helix's.
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
