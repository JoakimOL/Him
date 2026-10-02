-- | Finding and reading theme files ('Him.Paths.themeDirs'), following
-- @inherits@. The built-in theme is called @default@; a file of that name
-- replaces it.
module Him.Theme.Load
  ( loadTheme
  , themeNames
  , hasTrueColor
  ) where

import Control.Exception (IOException, try)
import Data.List (nub, sort)
import Data.Map.Strict qualified as Map
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.IO qualified as TIO
import Him.Paths (themeDirs)
import Him.Render.Theme (Theme, fromScopes)
import Him.Theme
import System.Directory (doesFileExist, listDirectory)
import System.Environment (lookupEnv)
import System.FilePath (dropExtension, takeExtension, (</>))

-- | Does the terminal say it shows 24-bit colour (@$COLORTERM@)? Otherwise
-- themes are reduced to the 256-colour palette.
hasTrueColor :: IO Bool
hasTrueColor = maybe False (`elem` ["truecolor", "24bit"]) <$> lookupEnv "COLORTERM"

-- | A theme by name, with warnings about what in it was skipped.
loadTheme :: Bool -> Text -> IO (Either Text (Theme, [Text]))
loadTheme trueColor name = do
  dirs <- themeDirs
  chain dirs (0 :: Int) 0 name >>= \case
    Left e -> pure (Left e)
    Right tf ->
      let (scopes, warnings) = resolveTheme tf
          scopes' = if trueColor then scopes else Map.map downsample scopes
       in pure (Right (fromScopes name scopes', warnings))
  where
    -- The file and its ancestors, merged. A theme inheriting its own name
    -- (a user's tweak of a Helix theme) gets the next one found.
    chain dirs depth from n
      | depth > 10 = pure (Left ("theme " <> name <> ": inherits too deep (a loop?)"))
      | otherwise =
          find dirs from n >>= \case
            Nothing
              | n == "default" -> pure (parseThemeFile defaultThemeText)
              | otherwise -> pure (Left ("no theme named " <> n))
            Just (i, path) ->
              try @IOException (TIO.readFile path) >>= \case
                Left e -> pure (Left (T.pack (show e)))
                Right src -> case parseThemeFile src of
                  Left e -> pure (Left (T.pack path <> ": " <> e))
                  Right tf -> case tfInherits tf of
                    Nothing -> pure (Right tf)
                    Just parent -> fmap (`mergeThemeFiles` tf) <$> chain dirs (depth + 1) (if parent == n then i + 1 else 0) parent
    find dirs from n = do
      let candidates = drop from (zip [0 ..] [d </> T.unpack n <> ".toml" | d <- dirs])
      found <- traverse (\(i, p) -> (\ok -> (i, p, ok)) <$> doesFileExist p) candidates
      pure (case [(i, p) | (i, p, True) <- found] of
        x : _ -> Just x
        [] -> Nothing)

-- | Every theme there is, for completion.
themeNames :: IO [Text]
themeNames = do
  dirs <- themeDirs
  names <- concat <$> traverse (\d -> either (const []) id <$> try @IOException (listDirectory d)) dirs
  pure (sort (nub ("default" : [T.pack (dropExtension f) | f <- names, takeExtension f == ".toml"])))
