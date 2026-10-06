-- | Building tree-sitter grammars for him (ADR tree-sitter): @him --build-grammars@.
--
-- Prebuilt grammars from other editors are not loaded. Many were generated
-- with an old @tree_sitter/array.h@ whose @array_push@ writes through a
-- pointer cast that strict aliasing lets the compiler move past the
-- reallocation; built with optimisation (Helix uses -O3), the external
-- scanner then writes into freed memory. Seen with Helix's @haskell.so@
-- and GCC 16: heap corruption on ordinary files. Building with
-- @-fno-strict-aliasing@ removes that, and building ourselves ties the
-- grammars to the runtime we ship.
module Him.GrammarBuild
  ( buildGrammars
  , defaultSourceDirs
  , himGrammarDir
  , findSourceDir
  ) where

import Control.Concurrent (forkIO)
import GHC.Conc (getNumProcessors)
import Control.Concurrent.MVar (newEmptyMVar, putMVar, takeMVar)
import Control.Concurrent.STM (atomically, newTQueueIO, tryReadTQueue, writeTQueue)
import Control.Exception (IOException, try)
import Control.Monad (forM_, replicateM, replicateM_)
import Data.IORef (atomicModifyIORef', newIORef, readIORef)
import Data.List (isSuffixOf, sort)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding (decodeUtf8Lenient)
import Him.Process (ProcessResult (..), runProcess)
import System.Directory (createDirectoryIfMissing, doesDirectoryExist, doesFileExist, getHomeDirectory, listDirectory, removeFile)
import System.Exit (ExitCode (..))
import System.FilePath (takeFileName, (</>))

-- | Where him keeps the grammars it built.
himGrammarDir :: IO FilePath
himGrammarDir = (</> ".config/him/runtime/grammars") <$> getHomeDirectory

-- | Where grammar sources usually are: Helix fetches them with
-- @hx --grammar fetch@.
defaultSourceDirs :: IO [FilePath]
defaultSourceDirs = do
  home <- getHomeDirectory
  pure [home </> ".config/helix/runtime/grammars/sources", "/usr/lib/helix/runtime/grammars/sources"]

-- | Build every grammar under a sources directory (one subdirectory per
-- grammar, as Helix lays them out), or only the named ones, into the
-- output directory. Reports each result through @progress@.
buildGrammars :: FilePath -> FilePath -> [Text] -> (Text -> IO ()) -> IO [(Text, Either Text ())]
buildGrammars sources out only progress = do
  createDirectoryIfMissing True out
  names <- sort . filter (\n -> null only || T.pack n `elem` only) <$> listDirectory sources
  queue <- newTQueueIO
  atomically (mapM_ (writeTQueue queue) names)
  results <- newIORef []
  -- Each worker mostly waits for a compiler process, so use every core.
  workers <- max 1 <$> getNumProcessors
  done <- newEmptyMVar
  let worker =
        atomically (tryReadTQueue queue) >>= \case
          Nothing -> putMVar done ()
          Just name -> do
            r <- buildOne (sources </> name) (out </> name <> ".so") name
            progress (T.pack name <> ": " <> either id (const "ok") r)
            atomicModifyIORef' results (\rs -> ((T.pack name, r) : rs, ()))
            worker
  replicateM_ workers (forkIO worker)
  _ <- replicateM workers (takeMVar done)
  sort <$> readIORef results

buildOne :: FilePath -> FilePath -> String -> IO (Either Text ())
buildOne dir output name =
  findSourceDir dir name >>= \case
    Nothing -> pure (Left "no src/parser.c")
    Just src -> do
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
        Left e -> pure (Left e)
        Right r
          | prExit r == ExitSuccess -> pure (Right ())
          | otherwise -> pure (Left (T.take 300 (decodeUtf8Lenient (prStderr r))))

-- | A grammar's @src@ directory: @DIR/src@, or for repositories with
-- several grammars a @src@ below a subdirectory named like the grammar
-- (@typescript/tsx/src@, @markdown/tree-sitter-markdown-inline/src@).
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
