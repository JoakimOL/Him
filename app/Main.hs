module Main (main) where

import Him.App qualified as App
import Data.Text qualified as T
import Data.Text.IO qualified as TIO
import Him.GrammarBuild (buildGrammars, defaultSourceDirs, himGrammarDir)
import System.Directory (doesDirectoryExist)
import System.Environment (getArgs)
import System.Exit (exitFailure)
import System.IO (hPutStrLn, stderr)

-- | @him [FILE...]@, or @him --build-grammars [SOURCES] [NAME...]@.
main :: IO ()
main =
  getArgs >>= \case
    "--build-grammars" : rest -> buildCommand rest
    files -> App.run files

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
