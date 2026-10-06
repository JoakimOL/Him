-- | Personal builds (ADR personal-builds): @him --rebuild@ builds a @him@ with the
-- plugins listed in @~/.config/him/plugins.toml@, as xmonad builds its
-- config. It writes a small stack project (a @Main.hs@ of
-- @himMain [hostPlugin …]@) and runs @stack build@, which needs GHC and
-- stack. The template repository (@templates/him-config@) does the same
-- in CI, for users without them.
--
-- @
-- [him]                      # optional: where him's source comes from
-- git = "https://github.com/JoakimOL/Him"
-- ref = "v0.1.0.0"           # default: the version you run
-- # path = "/home/me/src/him"
--
-- [plugins.harpoon]
-- git = "https://github.com/someone/him-harpoon"
-- ref = "v1.2"               # a tag or a commit
-- package = "him-harpoon"    # default: the name
-- module = "Harpoon"
-- spec = "harpoon"           # a PluginSpec in that module
-- # path = "../my-plugin"    # instead of git and ref
-- @
module Him.Rebuild
  ( PluginList (..)
  , Source (..)
  , ListedPlugin (..)
  , parsePluginList
  , projectFiles
  , himSnapshot
  , rebuild
  , personalBuild
  , pluginListPath
  ) where

import Control.Exception (IOException, try)
import Data.Char (isAlphaNum, isUpper)
import Data.Either (partitionEithers)
import Data.Map.Strict qualified as Map
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.IO qualified as TIO
import Data.Version (showVersion)
import Him.Json (Value (..), asObject)
import Him.Paths (configPath, stateDir)
import Him.Toml (parseToml)
import Paths_him (version)
import System.Directory (canonicalizePath, createDirectoryIfMissing, doesFileExist, getModificationTime, makeAbsolute)
import System.Environment (getExecutablePath)
import System.Exit (ExitCode (..))
import System.FilePath (takeDirectory, (</>))
import System.Process (CreateProcess (..), createProcess, proc, waitForProcess)

-- | The stack snapshot him is built with (as in its @stack.yaml@; a test
-- keeps them the same). Personal builds use it too, so the compiler is
-- the one him was tested with.
himSnapshot :: Text
himSnapshot = "lts-24.60"

data Source
  = FromGit !Text !Text
  | FromPath !FilePath
  deriving stock (Eq, Show)

data ListedPlugin = ListedPlugin
  { lpName :: !Text
  , lpSource :: !Source
  , lpPackage :: !Text
  , lpModule :: !Text
  , lpSpec :: !Text
  }
  deriving stock (Eq, Show)

data PluginList = PluginList
  { plHim :: !Source
  , plPlugins :: ![ListedPlugin]
  }
  deriving stock (Eq, Show)

-- | Where the list is: next to the config file.
pluginListPath :: IO FilePath
pluginListPath = (</> "plugins.toml") . takeDirectory <$> configPath

-- | Read a plugin list; every problem is reported.
parsePluginList :: Text -> Either [Text] PluginList
parsePluginList src = do
  doc <- either (\e -> Left [e]) Right (parseToml src)
  top <- maybe (Left ["the list must be a table"]) Right (asObject doc)
  let unknown = [k | (k, _) <- top, k `notElem` ["him", "plugins"]]
  him <- case lookup "him" top of
    Nothing -> Right defaultHim
    Just v -> source "him" defaultHim =<< table "[him]" v
  listed <- case lookup "plugins" top of
    Nothing -> Right []
    Just v -> table "[plugins]" v >>= collect . map plugin'
  if null unknown then Right (PluginList him listed) else Left ["unknown section [" <> k <> "] (known: him, plugins)" | k <- unknown]
  where
    defaultHim = FromGit "https://github.com/JoakimOL/Him" (T.pack ("v" <> showVersion version))
    table what v = maybe (Left [what <> " must be a table"]) Right (asObject v)
    text kvs k = case lookup k kvs of
      Just (JString t) | not (T.null t) -> Just t
      _ -> Nothing
    source what def kvs = case (text kvs "path", text kvs "git", text kvs "ref") of
      (Just p, Nothing, Nothing) -> Right (FromPath (T.unpack p))
      (Nothing, Just g, Just r) -> Right (FromGit g r)
      (Nothing, Nothing, Nothing) -> Right def
      _ -> Left [what <> ": give path, or git and ref"]
    plugin' (name, v) = do
      kvs <- table ("[plugins." <> name <> "]") v
      from <- source ("plugins." <> name) (FromPath "") kvs
      let bad = [k | (k, _) <- kvs, k `notElem` ["path", "git", "ref", "package", "module", "spec"]]
      case (from, text kvs "module", text kvs "spec", bad) of
        (_, _, _, k : _) -> Left ["plugins." <> name <> "." <> k <> ": unknown key (known: path, git, ref, package, module, spec)"]
        (FromPath "", _, _, _) -> Left ["plugins." <> name <> ": give path, or git and ref"]
        (_, Just m, Just s, _)
          | validModule m && validName s -> Right (ListedPlugin name from (maybe name id (text kvs "package")) m s)
          | otherwise -> Left ["plugins." <> name <> ": module must be a module name (Harpoon) and spec a name (harpoon)"]
        _ -> Left ["plugins." <> name <> ": module and spec are needed"]
    collect xs = case partitionEithers xs of
      ([], ok) -> Right ok
      (errs, _) -> Left (concat errs)
    validModule m = all (\part -> maybe False (isUpper . fst) (T.uncons part) && T.all (\c -> isAlphaNum c || c == '_') part) (T.splitOn "." m)
    validName s = maybe False (not . isUpper . fst) (T.uncons s) && T.all (\c -> isAlphaNum c || c == '_' || c == '\'') s

-- | The project's files: @stack.yaml@, the @.cabal@ file, @Main.hs@.
projectFiles :: PluginList -> [(FilePath, Text)]
projectFiles (PluginList him listed) =
  [ ( "stack.yaml"
    , T.unlines $
        ["# Written by him --rebuild; edit ~/.config/him/plugins.toml instead.", "snapshot: " <> himSnapshot, "packages:", "- .", "extra-deps:"]
          <> dep him
          <> concatMap (dep . lpSource) listed
    )
  , ( "him-personal.cabal"
    , T.unlines
        [ "cabal-version: 2.4"
        , "name: him-personal"
        , "version: 0"
        , "build-type: Simple"
        , ""
        , "executable him"
        , "  main-is: Main.hs"
        , "  default-language: GHC2021"
        , "  ghc-options: -threaded -rtsopts \"-with-rtsopts=-xn\""
        , "  build-depends: base, him" <> T.concat [", " <> lpPackage p | p <- listed]
        ]
    )
  , ( "Main.hs"
    , T.unlines $
        ["-- Written by him --rebuild; edit ~/.config/him/plugins.toml instead.", "module Main (main) where", "", "import Him.Main (himMain)", "import Him.Plugin (hostPlugin)"]
          <> ["import qualified " <> m | m <- uniq (map lpModule listed)]
          <> ["", "main :: IO ()", "main = himMain [" <> T.intercalate ", " ["hostPlugin " <> lpModule p <> "." <> lpSpec p | p <- listed] <> "]"]
    )
  ]
  where
    dep = \case
      FromGit g r -> ["- git: " <> g, "  commit: " <> r]
      FromPath p -> ["- " <> T.pack p]
    uniq = Map.keys . Map.fromList . map (,())

-- | @him --rebuild@: write the project and build it; the result goes to
-- @<state dir>/bin/him@, which the released @him@ then starts instead
-- ('personalBuild').
rebuild :: IO ()
rebuild = do
  listPath <- pluginListPath
  exists <- doesFileExist listPath
  src <- if exists then TIO.readFile listPath else pure ""
  case parsePluginList src of
    Left errs -> mapM_ (TIO.putStrLn . ((T.pack listPath <> ": ") <>)) errs
    -- Nothing to add to the released him: nothing to build.
    Right list
      | null (plPlugins list) -> putStrLn ("no plugins listed in " <> listPath <> "; the released him has the built-in and contrib ones (:plugins)")
    Right list -> do
      state <- stateDir
      let dir = state </> "build"
          bin = state </> "bin"
      createDirectoryIfMissing True dir
      -- Paths in the list are relative to the list's directory.
      absolute <- absolutePaths (takeDirectory listPath) list
      mapM_ (\(f, t) -> TIO.writeFile (dir </> f) t) (projectFiles absolute)
      putStrLn ("building in " <> dir <> " (" <> show (length (plPlugins list)) <> " plugins); stack's output follows")
      started <- try @IOException (createProcess (proc "stack" ["build", "--copy-bins", "--local-bin-path", bin]) {cwd = Just dir})
      case started of
        Left e -> putStrLn ("could not run stack (" <> show e <> "); install it, or use the template repository (templates/him-config)")
        Right (_, _, _, ph) ->
          waitForProcess ph >>= \case
            ExitSuccess -> putStrLn ("built " <> (bin </> "him") <> "; him starts it from now on")
            ExitFailure n -> putStrLn ("stack failed (" <> show n <> "); the build is in " <> dir)
  where
    absolutePaths base (PluginList him listed) = do
      him' <- absoluteSource base him
      listed' <- mapM (\p -> (\s -> p {lpSource = s}) <$> absoluteSource base (lpSource p)) listed
      pure (PluginList him' listed')
    absoluteSource base = \case
      FromPath p -> FromPath <$> makeAbsolute (base </> p)
      other -> pure other

-- | The personal build, if there is one and this is not it: 'Right' when
-- it is to be started instead of this program, 'Left' when it is older
-- than this one (built before an update: run @him --rebuild@).
personalBuild :: IO (Maybe (Either FilePath FilePath))
personalBuild = do
  built <- (</> ("bin" </> "him")) <$> stateDir
  exists <- doesFileExist built
  if not exists
    then pure Nothing
    else do
      canonical <- canonicalizePath built
      self <- canonicalizePath =<< getExecutablePath
      if canonical == self
        then pure Nothing
        else do
          newer <- (>=) <$> getModificationTime built <*> getModificationTime self
          pure (Just (if newer then Right built else Left built))
