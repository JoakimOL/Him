-- | Listing the files below a directory for the file picker, honouring
-- @.gitignore@ and @.ignore@ files ("Him.Ignore").
module Him.FileTree
  ( listFiles
  , rootIgnorer
  ) where

import Control.Exception (IOException, try)
import Data.ByteString qualified as BS
import Data.List (isPrefixOf, sort)
import Data.Text.Encoding (decodeUtf8Lenient)
import Him.Ignore
import System.Directory (canonicalizePath, doesDirectoryExist, doesPathExist, listDirectory)
import System.FilePath (makeRelative, takeDirectory, (</>))

-- | Files below a directory, sorted, relative to it. Hidden entries
-- (@.git@, dotfiles) and ignored ones are skipped, and an ignored directory
-- is not entered. The walk is breadth first and stops after @limit@ files.
listFiles :: Int -> FilePath -> IO [FilePath]
listFiles limit root = do
  outer <- rootIgnorer root
  sort . take limit <$> walk limit [("", outer)]
  where
    walk _ [] = pure []
    walk n _ | n <= 0 = pure []
    walk n ((dir, ignorer0) : rest) = do
      own <- readRules (root `join` dir)
      let ignorer = ignorer0 <> [(Below dir, rules) | rules <- own]
      entries <- either (const []) sort <$> (try (listDirectory (root `join` dir)) :: IO (Either IOException [FilePath]))
      kinds <- traverse (\e -> (dir `join` e,) <$> doesDirectoryExist (root `join` (dir `join` e))) [e | e <- entries, not ("." `isPrefixOf` e)]
      let kept = [(p, isDir) | (p, isDir) <- kinds, not (isIgnored ignorer p isDir)]
          files = [p | (p, False) <- kept]
          dirs = [(p, ignorer) | (p, True) <- kept]
      (files <>) <$> walk (n - length files) (rest <> dirs)
    join d "" = d
    join "" p = p
    join "." p = p
    join d p = d <> "/" <> p

-- | Rules from outside the walk that still apply to it: the ignore files of
-- the root's ancestors up to the enclosing git repository's root, and that
-- repository's @.git/info/exclude@. Outside a repository there are none.
rootIgnorer :: FilePath -> IO Ignorer
rootIgnorer root = do
  start <- either (const root) id <$> (try (canonicalizePath root) :: IO (Either IOException FilePath))
  findRepo start >>= \case
    Nothing -> pure []
    Just repo -> do
      let -- From the repository root down to the root's parent; the
          -- root's own files are read by the walk.
          below = takeWhile (/= repo) (iterate takeDirectory start)
          ancestors = if start == repo then [] else repo : reverse (drop 1 below)
          scope dir = if dir == start then Below "" else Above (makeRelative dir start)
      exclude <- readRulesFile (repo </> ".git" </> "info" </> "exclude")
      fromAncestors <- traverse (\dir -> map (scope dir,) <$> readRules dir) ancestors
      pure ([(scope repo, rules) | Just rules <- [exclude]] <> concat fromAncestors)

-- | The nearest directory at or above a path that contains @.git@.
findRepo :: FilePath -> IO (Maybe FilePath)
findRepo dir = do
  here <- doesPathExist (dir </> ".git")
  let parent = takeDirectory dir
  if here
    then pure (Just dir)
    else if parent == dir then pure Nothing else findRepo parent

-- | The rule sets of a directory's ignore files, in precedence order.
readRules :: FilePath -> IO [[Rule]]
readRules dir = do
  found <- traverse (readRulesFile . (dir </>)) ignoreFileNames
  pure [rules | Just rules <- found]

readRulesFile :: FilePath -> IO (Maybe [Rule])
readRulesFile path =
  (try (BS.readFile path) :: IO (Either IOException BS.ByteString)) >>= \case
    Left _ -> pure Nothing
    Right bytes -> pure (Just (parseIgnore (decodeUtf8Lenient bytes)))
