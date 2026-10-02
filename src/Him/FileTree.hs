-- | Listing the files below a directory for the file picker, honouring
-- @.gitignore@ and @.ignore@ files ("Him.Ignore").
module Him.FileTree
  ( listFiles
  , WalkOptions (..)
  , defaultWalk
  , walkFiles
  , rootIgnorer
  ) where

import Control.Concurrent (forkIO, getNumCapabilities, killThread)
import Control.Concurrent.MVar (newEmptyMVar, putMVar, takeMVar)
import Control.Concurrent.STM (TVar, atomically, modifyTVar', newTQueueIO, newTVarIO, orElse, readTQueue, readTVar, retry, writeTQueue, writeTVar)
import Control.Exception (IOException, SomeException, bracket, finally, onException, try)
import Control.Monad (replicateM, replicateM_, when)
import Data.ByteString qualified as BS
import Data.IORef (atomicModifyIORef', newIORef, readIORef)
import Data.List (isPrefixOf, sort)
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text.Encoding (decodeUtf8Lenient)
import GHC.Foreign qualified as GHC
import GHC.IO.Encoding (getFileSystemEncoding)
import Him.Ignore
import System.Directory (canonicalizePath, doesPathExist)
import System.FilePath (makeRelative, takeDirectory, (</>))
import System.Posix.Directory (closeDirStream, openDirStream)
import System.Posix.Directory.Internals (DirType, dirEntName, dirEntType, isDirectoryType, isRegularFileType, isSymbolicLinkType, isUnknownType, readDirStreamWith)
import System.Posix.Files (getFileStatus, getSymbolicLinkStatus, isDirectory, isRegularFile, isSymbolicLink)

-- | What a walk lists (@[editor.file-picker]@ in the config file).
data WalkOptions = WalkOptions
  { woHidden :: !Bool
  -- ^ Dotfiles and dot-directories too (never @.git@).
  , woGitIgnore :: !Bool
  -- ^ Honour @.gitignore@ and @.git/info/exclude@.
  , woIgnore :: !Bool
  -- ^ Honour @.ignore@.
  , woFollowLinks :: !Bool
  -- ^ Enter linked directories.
  , woLimit :: !Int
  -- ^ Stop after this many files.
  }
  deriving stock (Eq, Show)

-- | Hidden entries skipped, both ignore files honoured, links followed.
defaultWalk :: Int -> WalkOptions
defaultWalk = WalkOptions False True True True

-- | Files below a directory, sorted, relative to it (see 'walkFiles').
listFiles :: WalkOptions -> FilePath -> IO [FilePath]
listFiles opts root = do
  found <- newIORef []
  _ <- walkFiles opts root (\fs -> atomicModifyIORef' found (\acc -> (fs <> acc, ())))
  sort <$> readIORef found

-- | Walk the files below a directory, relative to it, handing them to
-- @emit@ a directory at a time (from several threads; @emit@ must be
-- thread safe). Hidden entries (dotfiles; @.git@ always) and ignored ones
-- are skipped, as the options say, and an ignored directory is not
-- entered. Links to directories are followed, as Helix does, but each
-- target only once and never one that contains the link (a cycle). It
-- stops after the limit and returns how many files it found.
--
-- Directories are read by a pool of worker threads (ADR-24), and entry
-- types come from @readdir@ itself, so most entries cost no @stat@.
-- Killing the calling thread stops the workers too.
walkFiles :: WalkOptions -> FilePath -> ([FilePath] -> IO ()) -> IO Int
walkFiles opts root emit = do
  outer <- if woGitIgnore opts || woIgnore opts then rootIgnorer names root else pure []
  queue <- newTQueueIO
  pending <- newTVarIO (1 :: Int)
  count <- newIORef 0
  atomically (writeTQueue queue ("", outer))
  workers <- max 1 . min 8 <$> getNumCapabilities
  finished <- newEmptyMVar
  followed <- newTVarIO Set.empty
  let next = (Just <$> readTQueue queue) `orElse` (readTVar pending >>= \n -> if n == 0 then pure Nothing else retry)
      worker =
        atomically next >>= \case
          Nothing -> pure ()
          Just job -> do
            _ <- try @SomeException (visit job)
            atomically (modifyTVar' pending (subtract 1))
            worker
      visit (dir, ignorer0) = do
        own <- readRules names (root `join` dir)
        let ignorer = ignorer0 <> [(Below dir, rules) | rules <- own]
        entries <- either (const []) id <$> try @IOException (readEntries (root `join` dir))
        kinds <- traverse (\(name, kind) -> (dir `join` name,) <$> resolve (woFollowLinks opts) followed (root `join` (dir `join` name)) kind) [e | e@(name, _) <- entries, shown name]
        let kept = [(p, k) | (p, Just k) <- kinds, not (isIgnored ignorer p (k == Dir))]
            files = [p | (p, File) <- kept]
            dirs = [(p, ignorer) | (p, Dir) <- kept]
        before <- atomicModifyIORef' count (\c -> (c + length files, c))
        let room = limit - before
        if room <= 0
          then pure ()
          else do
            let taken = take room files
            if null taken then pure () else emit taken
            when (length files < room) $
              atomically (mapM_ (writeTQueue queue) dirs >> modifyTVar' pending (+ length dirs))
  tids <- replicateM workers (forkIO (worker `finally` putMVar finished ()))
  replicateM_ workers (takeMVar finished) `onException` mapM_ killThread tids
  min limit <$> readIORef count
  where
    limit = woLimit opts
    names = [".gitignore" | woGitIgnore opts] <> [".ignore" | woIgnore opts]
    shown name = name /= ".git" && (woHidden opts || not ("." `isPrefixOf` name))
    join d "" = d
    join "" p = p
    join "." p = p
    join d p = d <> "/" <> p

data Kind = File | Dir
  deriving stock (Eq)

-- | Entries of a directory with their @readdir@ types.
readEntries :: FilePath -> IO [(FilePath, DirType)]
readEntries dir = bracket (openDirStream dir) closeDirStream (go [])
  where
    go acc ds =
      readDirStreamWith (\de -> (,) <$> (dirEntName de >>= peekName) <*> dirEntType de) ds >>= \case
        Nothing -> pure acc
        Just (name, kind)
          | name == "." || name == ".." -> go acc ds
          | otherwise -> go ((name, kind) : acc) ds
    peekName cstr = getFileSystemEncoding >>= \enc -> GHC.peekCString enc cstr

-- | A file or a directory to enter; 'Nothing' for anything else. Only
-- links and file systems that do not report types need a @stat@.
resolve :: Bool -> TVar (Set FilePath) -> FilePath -> DirType -> IO (Maybe Kind)
resolve follow followed path kind
  | isDirectoryType kind = pure (Just Dir)
  | isRegularFileType kind = pure (Just File)
  | isSymbolicLinkType kind = link
  | isUnknownType kind =
      try @IOException (getSymbolicLinkStatus path) >>= \case
        Right st | isDirectory st -> pure (Just Dir)
        Right st | isSymbolicLink st -> link
        Right st | isRegularFile st -> pure (Just File)
        _ -> pure Nothing
  | otherwise = pure Nothing
  where
    link =
      try @IOException (getFileStatus path) >>= \case
        Right st | isRegularFile st -> pure (Just File)
        Right st | isDirectory st, follow -> linkedDirectory
        _ -> pure Nothing
    -- Enter a linked directory unless it contains the link (a cycle) or
    -- another link to it was entered already.
    linkedDirectory = do
      target <- canonicalizePath path
      here <- canonicalizePath (takeDirectory path)
      let cycle' = (target <> "/") `isPrefixOf` (here <> "/")
      fresh <- atomically $ do
        seen <- readTVar followed
        if Set.member target seen then pure False else True <$ writeTVar followed (Set.insert target seen)
      pure (if fresh && not cycle' then Just Dir else Nothing)

-- | Rules from outside the walk that still apply to it: the ignore files of
-- the root's ancestors up to the enclosing git repository's root, and that
-- repository's @.git/info/exclude@ (with @.gitignore@ among the names).
-- Outside a repository there are none.
rootIgnorer :: [FilePath] -> FilePath -> IO Ignorer
rootIgnorer names root = do
  start <- either (const root) id <$> (try (canonicalizePath root) :: IO (Either IOException FilePath))
  findRepo start >>= \case
    Nothing -> pure []
    Just repo -> do
      let -- From the repository root down to the root's parent; the
          -- root's own files are read by the walk.
          below = takeWhile (/= repo) (iterate takeDirectory start)
          ancestors = if start == repo then [] else repo : reverse (drop 1 below)
          scope dir = if dir == start then Below "" else Above (makeRelative dir start)
      exclude <- if ".gitignore" `elem` names then readRulesFile (repo </> ".git" </> "info" </> "exclude") else pure Nothing
      fromAncestors <- traverse (\dir -> map (scope dir,) <$> readRules names dir) ancestors
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
readRules :: [FilePath] -> FilePath -> IO [[Rule]]
readRules names dir = do
  found <- traverse (readRulesFile . (dir </>)) names
  pure [rules | Just rules <- found]

readRulesFile :: FilePath -> IO (Maybe [Rule])
readRulesFile path =
  (try (BS.readFile path) :: IO (Either IOException BS.ByteString)) >>= \case
    Left _ -> pure Nothing
    Right bytes -> pure (Just (parseIgnore (decodeUtf8Lenient bytes)))
