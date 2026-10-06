-- | The @him@ program (ADR personal-builds): its command line, for a build with these
-- plugins besides the built-in ones and contrib. @app/Main.hs@ is
-- @himMain []@; a personal build (@him --rebuild@, or the template
-- repository's CI) is @himMain [hostPlugin myPlugin, …]@.
module Him.Main
  ( himMain
  ) where

import Control.Monad (unless, when)
import Data.List (nub)
import Data.Maybe (mapMaybe)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.IO qualified as TIO
import Him.App qualified as App
import Him.Config (Plugin (..))
import Him.Config.Default (plugins)
import Him.GrammarBuild (buildGrammars, fetchGrammars)
import Him.GrammarList (GrammarSource (..), grammarList)
import Him.Language (grammarFor, languages)
import Him.Paths (himRuntimeDir)
import Him.Mcp (runBridge)
import Him.Rebuild (personalBuild, rebuild)
import System.Posix.Process (executeFile)
import Him.UserConfig (defaultConfigTextFor)
import System.Directory (findExecutable)
import System.FilePath ((</>))
import System.Environment (getArgs)
import System.Exit (die, exitFailure)

-- | @him [FILE...]@, @him --dump-default-config@, @him --rebuild@,
-- @him --grammar [fetch | build [--force]] [NAME...]@, or @him --help@.
himMain :: [Plugin] -> IO ()
himMain extra = do
  let every = plugins <> extra
      names = map plName every
  if length (nub names) /= length names
    then die ("two plugins have the same name: " <> unwords [T.unpack n | n <- nub names, length (filter (== n) names) > 1])
    else
      getArgs >>= \case
        [flag] | flag `elem` ["--help", "-h"] -> putStr usage
        "--grammar" : rest -> grammarCommand rest
        -- Started by Claude Code for the chat (see "Him.Mcp"), not by hand.
        ["--mcp-bridge", dir] -> runBridge dir
        ["--dump-default-config"] -> TIO.putStr (defaultConfigTextFor every)
        ["--rebuild"] -> rebuild
        files
          -- The released him starts a personal build instead (ADR personal-builds).
          | null extra -> personalBuild >>= maybe (App.runWith every Nothing files) (either older (\built -> executeFile built False files Nothing))
          | otherwise -> App.runWith every Nothing files
          where
            older built = App.runWith every (Just (T.pack built <> " is older than this him: him --rebuild makes it again")) files

-- | Fetch and build tree-sitter grammars (ADR grammar-setup): by default
-- those of the built-in languages, or the named ones.
grammarCommand :: [String] -> IO ()
grammarCommand args = do
  available <- either (die . ("the built-in grammar list is broken: " <>) . T.unpack) pure grammarList
  let (steps, rest) = case args of
        "fetch" : more -> ((True, False), more)
        "build" : more -> ((False, True), more)
        more -> ((True, True), more)
      force = "--force" `elem` rest
      names = [T.pack n | n <- rest, n /= "--force"]
      wanted = if null names then nub (mapMaybe (grammarFor "tree-sitter") languages) else names
      chosen = [g | g <- available, gsName g `elem` wanted]
      unknown = [n | n <- wanted, n `notElem` map gsName available]
  -- Only names given by hand can be unknown; the built-in table is tested.
  unless (null unknown || null names) $
    die ("no such grammar: " <> T.unpack (T.intercalate ", " unknown) <> " (him knows the grammars in runtime/grammars.toml)")
  runtime <- himRuntimeDir
  let sources = runtime </> "grammars" </> "sources"
      out = runtime </> "grammars"
  when (fst steps) $ do
    need "git" "to fetch grammar sources"
    putStrLn ("fetching " <> show (length chosen) <> " grammars into " <> sources)
    report "fetched" =<< fetchGrammars sources chosen TIO.putStrLn
  when (snd steps) $ do
    need "cc" "a C compiler, to build grammars"
    putStrLn ("building " <> show (length chosen) <> " grammars into " <> out)
    report "built" =<< buildGrammars sources out force chosen TIO.putStrLn
  where
    need program why =
      findExecutable program >>= \case
        Just _ -> pure ()
        Nothing -> die ("him --grammar needs " <> program <> " (" <> why <> ")")
    report :: String -> [(Text, Either Text Text)] -> IO ()
    report verb results = do
      let failed = [n | (n, Left _) <- results]
      putStrLn (show (length results - length failed) <> " " <> verb <> " or up to date, " <> show (length failed) <> " failed" <> if null failed then "" else ": " <> T.unpack (T.intercalate ", " failed))
      unless (null failed) exitFailure

usage :: String
usage =
  unlines
    [ "him [FILE|DIRECTORY...]              edit files (a directory opens as a listing)"
    , "him --dump-default-config            print every default, as a config file"
    , "him --rebuild                        build your own him with the plugins in ~/.config/him/plugins.toml"
    , "him --grammar [NAME...]              fetch and build the tree-sitter grammars (highlighting)"
    , "him --grammar fetch [NAME...]        only fetch their sources (needs git)"
    , "him --grammar build [--force] [NAME...]"
    , "                                     only build them (needs a C compiler)"
    , ""
    , "Config: ~/.config/him/config.toml ($HIM_CONFIG). In the editor, space ? lists every command."
    ]
