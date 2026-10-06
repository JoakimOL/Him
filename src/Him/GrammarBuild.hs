{-# LANGUAGE MultiWayIf #-}

-- | Fetching and building tree-sitter grammars for him (ADR grammar-setup):
-- @him --grammar fetch@ and @him --grammar build@.
--
-- Fetching clones each grammar's repository at the revision in the grammar
-- list ("Him.GrammarList") into @RUNTIME/grammars/sources/NAME@, the way
-- Helix does: one shallow fetch of that commit. A source already at the
-- revision is left alone.
--
-- Building compiles the sources into @RUNTIME/grammars/NAME.so@ with the
-- system's C compiler; a grammar built from the same revision before is
-- skipped. Prebuilt grammars from other editors are not loaded (ADR
-- tree-sitter). Many were generated with an old @tree_sitter/array.h@ whose
-- @array_push@ writes through a pointer cast that strict aliasing lets the
-- compiler move past the reallocation; built with optimisation (Helix uses
-- -O3), the external scanner then writes into freed memory. Seen with
-- Helix's @haskell.so@ and GCC 16: heap corruption on ordinary files.
-- Building with @-fno-strict-aliasing@ removes that, and building ourselves
-- ties the grammars to the runtime we ship.
module Him.GrammarBuild
  ( fetchGrammars
  , buildGrammars
  ) where

import Control.Concurrent (forkIO)
import Control.Concurrent.MVar (newEmptyMVar, putMVar, takeMVar)
import Control.Concurrent.STM (atomically, newTQueueIO, tryReadTQueue, writeTQueue)
import Control.Exception (IOException, try)
import Control.Monad (forM_, replicateM, replicateM_, when)
import Data.IORef (atomicModifyIORef', newIORef, readIORef)
import Data.List (isSuffixOf, sort)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding (decodeUtf8Lenient)
import Data.Text.IO qualified as TIO
import GHC.Conc (getNumProcessors)
import Him.GrammarList (GrammarSource (..))
import Him.Process (ProcessResult (..), runProcess, runProcessEnv)
import System.Directory (createDirectoryIfMissing, doesDirectoryExist, doesFileExist, listDirectory, removeFile)
import System.Exit (ExitCode (..))
import System.FilePath (takeFileName, (</>))

-- | Fetch each grammar's source into the sources directory. Reports each
-- result through @progress@; 'Right' says what was done.
fetchGrammars :: FilePath -> [GrammarSource] -> (Text -> IO ()) -> IO [(Text, Either Text Text)]
fetchGrammars sources grammars progress = do
  createDirectoryIfMissing True sources
  inParallel grammars progress (fetchOne sources)

fetchOne :: FilePath -> GrammarSource -> IO (Either Text Text)
fetchOne sources g = do
  let dir = sources </> T.unpack (gsName g)
  current <- headRev dir
  if current == Just (gsRev g)
    then pure (Right "up to date")
    else do
      createDirectoryIfMissing True dir
      isRepo <- doesDirectoryExist (dir </> ".git")
      steps dir $
        [["init", "--quiet"] | not isRepo]
          <> [ ["remote", "remove", "origin"] | isRepo]
          <> [ ["remote", "add", "origin", T.unpack (gsGit g)]
             , ["fetch", "--quiet", "--depth", "1", "origin", T.unpack (gsRev g)]
             , ["checkout", "--quiet", "--force", "FETCH_HEAD"]
             ]
  where
    steps _ [] = pure (Right "fetched")
    steps dir (args : rest) =
      git dir args >>= \case
        -- A source without an origin yet is fine to remove from.
        Left e | take 2 args /= ["remote", "remove"] -> pure (Left e)
        _ -> steps dir rest

-- | Build each grammar from the sources directory into the output
-- directory (unless built from the same revision before, or @force@).
-- Reports each result through @progress@.
buildGrammars :: FilePath -> FilePath -> Bool -> [GrammarSource] -> (Text -> IO ()) -> IO [(Text, Either Text Text)]
buildGrammars sources out force grammars progress = do
  createDirectoryIfMissing True out
  inParallel grammars progress (buildOne sources out force)

buildOne :: FilePath -> FilePath -> Bool -> GrammarSource -> IO (Either Text Text)
buildOne sources out force g = do
  let name = T.unpack (gsName g)
      dir = sources </> name
      output = out </> name <> ".so"
      stamp = out </> name <> ".rev"
  fetched <- doesDirectoryExist dir
  rev <- headRev dir
  built <- doesFileExist output
  previous <- either (const Nothing) (Just . T.strip) <$> try @IOException (TIO.readFile stamp)
  if
    | not fetched -> pure (Left "not fetched (him --grammar fetch)")
    | not force && built && rev /= Nothing && previous == rev -> pure (Right "up to date")
    | otherwise -> do
        src <- case gsSubpath g of
          Just sub -> do
            let d = dir </> T.unpack sub </> "src"
            ok <- doesFileExist (d </> "parser.c")
            pure (if ok then Just d else Nothing)
          Nothing -> findSourceDir dir name
        case src of
          Nothing -> pure (Left "no src/parser.c")
          Just d -> do
            r <- compileGrammar d output
            when (r == Right ()) (mapM_ (TIO.writeFile stamp) rev)
            pure ("built" <$ r)

compileGrammar :: FilePath -> FilePath -> IO (Either Text ())
compileGrammar src output = do
  hasC <- doesFileExist (src </> "scanner.c")
  hasCC <- doesFileExist (src </> "scanner.cc")
  let common = ["-O2", "-fno-strict-aliasing", "-fPIC", "-I", src]
  -- C files are compiled to objects, so a C++ scanner can join them.
  parser <- compile "cc" (common <> ["-c", src </> "parser.c", "-o", output <> ".parser.o"])
  scanner
    <- if hasC
      then compile "cc" (common <> ["-c", src </> "scanner.c", "-o", output <> ".scanner.o"])
      else
        if hasCC
          then compile "c++" (common <> ["-c", src </> "scanner.cc", "-o", output <> ".scanner.o"])
          else pure (Right ())
  let objects = [output <> ".parser.o"] <> [output <> ".scanner.o" | hasC || hasCC]
  linked <- case (parser, scanner) of
    (Right (), Right ()) -> compile (if hasCC then "c++" else "cc") (["-shared", "-o", output] <> objects)
    (Left e, _) -> pure (Left e)
    (_, Left e) -> pure (Left e)
  forM_ objects (try @IOException . removeFile)
  pure linked
  where
    compile cc args =
      runProcess cc args Nothing "" >>= \case
        Left e -> pure (Left (T.pack cc <> ": " <> e))
        Right r
          | prExit r == ExitSuccess -> pure (Right ())
          | otherwise -> pure (Left (T.take 300 (decodeUtf8Lenient (prStderr r))))

-- | The commit a fetched source is at; 'Nothing' if it is not a repository
-- of its own (a parent directory's repository does not count).
headRev :: FilePath -> IO (Maybe Text)
headRev dir = do
  isRepo <- doesDirectoryExist (dir </> ".git")
  if not isRepo
    then pure Nothing
    else either (const Nothing) Just <$> git dir ["rev-parse", "HEAD"]

-- | Run git in a directory: its output, or what went wrong. It never asks
-- for a password (a repository that moved would otherwise prompt).
git :: FilePath -> [String] -> IO (Either Text Text)
git dir args =
  runProcessEnv [("GIT_TERMINAL_PROMPT", "0")] "git" args (Just dir) "" >>= \case
    Left e -> pure (Left ("git: " <> e))
    Right r
      | prExit r == ExitSuccess -> pure (Right (T.strip (decodeUtf8Lenient (prStdout r))))
      | otherwise -> pure (Left (T.take 300 (T.strip (decodeUtf8Lenient (prStderr r)))))

-- | Run a job for each grammar, as many at once as there are cores (each
-- mostly waits for git or a compiler), reporting each result as it comes.
-- The results are sorted by name.
inParallel :: [GrammarSource] -> (Text -> IO ()) -> (GrammarSource -> IO (Either Text Text)) -> IO [(Text, Either Text Text)]
inParallel grammars progress job = do
  queue <- newTQueueIO
  atomically (mapM_ (writeTQueue queue) grammars)
  results <- newIORef []
  workers <- max 1 <$> getNumProcessors
  done <- newEmptyMVar
  let worker =
        atomically (tryReadTQueue queue) >>= \case
          Nothing -> putMVar done ()
          Just g -> do
            r <- job g
            progress (gsName g <> ": " <> either id id r)
            atomicModifyIORef' results (\rs -> ((gsName g, r) : rs, ()))
            worker
  replicateM_ workers (forkIO worker)
  _ <- replicateM workers (takeMVar done)
  sort <$> readIORef results

-- | A grammar's @src@ directory when the list gives no subpath: @DIR/src@,
-- or for repositories with several grammars a @src@ below a subdirectory
-- named like the grammar (@typescript/tsx/src@).
findSourceDir :: FilePath -> String -> IO (Maybe FilePath)
findSourceDir dir name = do
  direct <- doesFileExist (dir </> "src" </> "parser.c")
  if direct
    then pure (Just (dir </> "src"))
    else do
      -- Subdirectories two levels down at most (ocaml/grammars/ocaml/src).
      level1 <- subdirs dir
      level2 <- concat <$> mapM (\s -> map (s </>) <$> subdirs (dir </> s)) level1
      candidates <- filterIO (\s -> doesFileExist (dir </> s </> "src" </> "parser.c")) (level1 <> level2)
      let norm = map (\c -> if c == '_' then '-' else c)
          related a b = a `isSuffixOf` b || b `isSuffixOf` a
          matching = [s | s <- candidates, norm name `related` norm (takeFileName s)]
      pure $ case (matching, candidates) of
        (s : _, _) -> Just (dir </> s </> "src")
        ([], [s]) -> Just (dir </> s </> "src")
        _ -> Nothing
  where
    filterIO p = fmap concat . mapM (\x -> (\ok -> [x | ok]) <$> p x)
    subdirs d = do
      entries <- either (const []) id <$> try @IOException (listDirectory d)
      filterIO (\e -> doesDirectoryExist (d </> e)) [e | e <- entries, take 1 e /= "."]
