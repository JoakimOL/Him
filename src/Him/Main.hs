-- | The @him@ program (ADR-52): its command line, for a build with these
-- plugins besides the built-in ones and contrib. @app/Main.hs@ is
-- @himMain []@; a personal build (@him --rebuild@, or the template
-- repository's CI) is @himMain [hostPlugin myPlugin, …]@.
module Him.Main
  ( himMain
  ) where

import Data.List (nub)
import Data.Text qualified as T
import Data.Text.IO qualified as TIO
import Him.App qualified as App
import Him.Config (Plugin (..))
import Him.Config.Default (plugins)
import Him.GrammarBuild (buildGrammars, defaultSourceDirs, himGrammarDir)
import Him.Mcp (runBridge)
import Him.Rebuild (personalBuild, rebuild)
import System.Posix.Process (executeFile)
import Him.UserConfig (defaultConfigTextFor)
import System.Directory (doesDirectoryExist)
import System.Environment (getArgs)
import System.Exit (die, exitFailure)
import System.IO (hPutStrLn, stderr)

-- | @him [FILE...]@, @him --dump-default-config@, @him --rebuild@,
-- @him --build-grammars [SOURCES] [NAME...]@, or @him --help@.
himMain :: [Plugin] -> IO ()
himMain extra = do
  let every = plugins <> extra
      names = map plName every
  if length (nub names) /= length names
    then die ("two plugins have the same name: " <> unwords [T.unpack n | n <- nub names, length (filter (== n) names) > 1])
    else
      getArgs >>= \case
        [flag] | flag `elem` ["--help", "-h"] -> putStr usage
        "--build-grammars" : rest -> buildCommand rest
        -- Started by Claude Code for the chat (see "Him.Mcp"), not by hand.
        ["--mcp-bridge", dir] -> runBridge dir
        ["--dump-default-config"] -> TIO.putStr (defaultConfigTextFor every)
        ["--rebuild"] -> rebuild
        files
          -- The released him starts a personal build instead (ADR-52).
          | null extra -> personalBuild >>= maybe (App.runWith every Nothing files) (either older (\built -> executeFile built False files Nothing))
          | otherwise -> App.runWith every Nothing files
          where
            older built = App.runWith every (Just (T.pack built <> " is older than this him: him --rebuild makes it again")) files

-- | Compile tree-sitter grammars for him (see "Him.GrammarBuild").
buildCommand :: [String] -> IO ()
buildCommand args = do
  (sources, names) <- case args of
    dir : more | '/' `elem` dir || dir == "." -> pure (Just dir, more)
    more -> do
      found <- filterM' doesDirectoryExist =<< defaultSourceDirs
      pure (case found of d : _ -> Just d; [] -> Nothing, more)
  out <- himGrammarDir
  case sources of
    Nothing -> do
      hPutStrLn stderr "no grammar sources found; run `hx --grammar fetch`, or pass a directory"
      exitFailure
    Just dir -> do
      putStrLn ("building grammars from " <> dir <> " into " <> out)
      results <- buildGrammars dir out (map T.pack names) TIO.putStrLn
      let failed = [n | (n, Left _) <- results]
      putStrLn (show (length results - length failed) <> " built, " <> show (length failed) <> " failed")
  where
    filterM' p = fmap concat . mapM (\x -> (\ok -> [x | ok]) <$> p x)

usage :: String
usage =
  unlines
    [ "him [FILE|DIRECTORY...]              edit files (a directory opens as a listing)"
    , "him --dump-default-config            print every default, as a config file"
    , "him --rebuild                        build your own him with the plugins in ~/.config/him/plugins.toml"
    , "him --build-grammars [SOURCES] [NAME...]"
    , "                                     compile tree-sitter grammars (sources: hx --grammar fetch)"
    , ""
    , "Config: ~/.config/him/config.toml ($HIM_CONFIG). In the editor, space ? lists every command."
    ]
