-- | The runtime (ADR-23): runs the background jobs actions ask for
-- ("Him.Effect"), each on its own thread, and posts their results to the
-- main loop's event channel. At most one job per 'JobKey' runs; starting
-- another cancels the old one.
module Him.Runtime
  ( Runtime
  , newRuntime
  , perform
  , shutdown
  ) where

import Control.Concurrent (ThreadId, forkIO, killThread)
import Control.Concurrent.MVar (MVar, modifyMVar, modifyMVar_, newEmptyMVar, newMVar, putMVar, readMVar, tryReadMVar)
import Control.Monad (when)
import Data.Text (Text)
import Data.Text qualified as T
import Him.Lsp.Config (ServerConfig (..), serverFor)
import Him.Lsp.Server (Server, findRoot, sendMessage, startServer, stopServer)
import Him.Lsp.State (ServerInfo)
import Control.Exception (IOException, try)
import Data.ByteString qualified as BS
import Him.Document (Document (..))
import Him.File (loadDocument)
import System.Directory (getFileSize, makeAbsolute)
import System.IO (IOMode (..), withBinaryFile)
import Control.Exception (evaluate)
import Data.Foldable (toList)
import Data.IORef (atomicModifyIORef', newIORef)
import Data.List (sort)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import GHC.Clock (getMonotonicTime)
import Him.Effect
import Him.Event (Event (..))
import Him.FileTree (walkFiles)
import Him.Buffer qualified as Buffer
import Him.Diff (Hunk (..), diffLines, mapLine)
import Him.Git (loadBase, removeFromIndex, writeIndex)
import Him.GitState (GitBase (..))
import Data.IntMap.Strict qualified as IntMap
import Him.Picker (rank)
import Him.Syntax (SyntaxProvider, SyntaxSession (..), startSyntax)

data Runtime = Runtime
  { rtPost :: Event -> IO ()
  , rtJobs :: MVar (Map JobKey ThreadId)
  , rtProviders :: [SyntaxProvider]
  , rtSessions :: MVar (Map Int SyntaxSession)
  -- ^ Highlighters by document id. The editor only sees their results.
  , rtServers :: MVar (Map Text (MVar (Either Text (Server, ServerInfo))))
  -- ^ Language servers by key; the inner variable is filled once the
  -- server has started (or failed to).
  }

newRuntime :: [SyntaxProvider] -> (Event -> IO ()) -> IO Runtime
newRuntime providers post = do
  jobs <- newMVar Map.empty
  sessions <- newMVar Map.empty
  servers <- newMVar Map.empty
  pure (Runtime post jobs providers sessions servers)

-- | Carry out an effect that needs the runtime; others are ignored (the
-- main loop handles them before).
perform :: Runtime -> Effect -> IO ()
perform rt = \case
  StartJob job -> modifyMVar_ (rtJobs rt) $ \jobs -> do
    mapM_ killThread (Map.lookup (jobKey job) jobs)
    tid <- forkIO (runJob rt job)
    pure (Map.insert (jobKey job) tid jobs)
  CancelJob key -> modifyMVar_ (rtJobs rt) $ \jobs -> do
    mapM_ killThread (Map.lookup key jobs)
    pure (Map.delete key jobs)
  LspSend key msg -> do
    servers <- readMVar (rtServers rt)
    case Map.lookup key servers of
      Just started ->
        tryReadMVar started >>= \case
          Just (Right (server, _)) -> sendMessage server msg
          _ -> pure ()
      Nothing -> pure ()
  LspStop key -> do
    -- Forget it first, so a restart starts a new one.
    stopped <- modifyMVar (rtServers rt) (\servers -> pure (Map.delete key servers, Map.lookup key servers))
    mapM_ (\started -> tryReadMVar started >>= mapM_ (either (const (pure ())) (stopServer . fst))) stopped
  _ -> pure ()

-- | Stop every language server (when the editor quits).
shutdown :: Runtime -> IO ()
shutdown rt = do
  servers <- readMVar (rtServers rt)
  mapM_ (\started -> tryReadMVar started >>= mapM_ (either (const (pure ())) (stopServer . fst))) (Map.elems servers)

-- | Most files a picker lists (a memory guard; the scan streams).
maxFiles :: Int
maxFiles = 500000

runJob :: Runtime -> Job -> IO ()
runJob rt = \case
  ScanFiles gen root -> do
    -- Files are sent in batches: every 5000 files or 100 ms, whichever
    -- comes first, so the picker fills quickly without an event per file.
    start <- getMonotonicTime
    pending <- newIORef ([], 0 :: Int, start)
    let send fs = if null fs then pure () else post (EvJob (FilesFound gen (sort fs)))
        emit fs = do
          now <- getMonotonicTime
          due <- atomicModifyIORef' pending $ \(acc, n, lastSent) ->
            let acc' = fs <> acc
                n' = n + length fs
             in if n' >= 5000 || now - lastSent >= 0.1 then (([], 0, now), acc') else ((acc', n', lastSent), [])
          send due
    _ <- walkFiles maxFiles root emit
    rest <- atomicModifyIORef' pending (\(acc, _, t) -> (([], 0, t), acc))
    send rest
    post (EvJob (ScanFinished gen))
  FilterPicker gen query items -> do
    let (best, total) = rank query (toList items)
    _ <- evaluate (length best + total)
    post (EvJob (PickerFiltered gen query best total))
  GitLoad doc path -> do
    base <- loadBase path
    post (EvJob (GitLoaded doc base))
  GitDiff doc version base buffer -> do
    -- Unstaged: index -> buffer. Staged: HEAD -> index, moved onto buffer
    -- lines through the unstaged hunks.
    let current = Buffer.toLines buffer
        unstaged = diffLines (gbIndex base) current
        staged = [h {hNewStart = mapLine unstaged (hNewStart h)} | h <- diffLines (gbHead base) (gbIndex base)]
    _ <- evaluate (length unstaged + length staged)
    post (EvJob (GitDiffed doc version unstaged staged))
  GitWriteIndex doc base ending new -> do
    result <- maybe (removeFromIndex base) (writeIndex base ending) new
    post (EvJob (GitWritten doc result))
  SyntaxStart doc language -> do
    started <- startSyntax (rtProviders rt) language
    modifyMVar_ (rtSessions rt) $ \sessions -> do
      mapM_ ssClose (Map.lookup doc sessions)
      pure (maybe (Map.delete doc sessions) (\(_, session) -> Map.insert doc session sessions) started)
    post (EvJob (SyntaxStarted doc (fst <$> started)))
  LspEnsure doc language file -> case serverFor language of
    Nothing -> post (EvJob (LspUnavailable doc "no language server configured"))
    Just config -> do
      absolute <- makeAbsolute file
      root <- findRoot (scRoots config) absolute
      let key = T.pack (scCommand config <> " " <> root)
      -- The first document for a key starts the server; others wait for it.
      (started, fresh) <- modifyMVar (rtServers rt) $ \servers -> case Map.lookup key servers of
        Just v -> pure (servers, (v, False))
        Nothing -> do
          v <- newEmptyMVar
          pure (Map.insert key v servers, (v, True))
      when fresh $ do
        result <- startServer config root (post . EvJob . LspMessage key) $ do
          -- Only report the exit of the server still registered under the
          -- key (not one that was stopped or replaced by a restart).
          current <- modifyMVar (rtServers rt) $ \servers ->
            if Map.lookup key servers == Just started
              then pure (Map.delete key servers, True)
              else pure (servers, False)
          when current (post (EvJob (LspExited key)))
        putMVar started result
      readMVar started >>= \case
        Right (_, info) -> post (EvJob (LspReady doc key absolute (scLanguageId config) info))
        Left e -> post (EvJob (LspUnavailable doc e))
  LoadPreview file -> do
    -- Binary and very large files are not shown.
    size <- try @IOException (getFileSize file)
    head' <- try @IOException (withBinaryFile file ReadMode (`BS.hGet` 8192))
    result <- case (size, head') of
      (Left e, _) -> pure (Left (T.pack (show e)))
      (Right n, _) | n > 20 * 1024 * 1024 -> pure (Left "file too large to preview")
      (_, Right bytes) | BS.elem 0 bytes -> pure (Left "binary file")
      _ -> either Left (Right . docBuffer) <$> loadDocument file
    post (EvJob (PreviewLoaded file result))
  Highlight doc version buffer from to -> do
    session <- Map.lookup doc <$> readMVar (rtSessions rt)
    case session of
      Nothing -> post (EvJob (SyntaxStarted doc Nothing))
      Just s -> do
        ssUpdate s version buffer []
        spans <- ssHighlight s from to
        _ <- evaluate (IntMap.size spans)
        post (EvJob (Highlighted doc version from to spans))
  where
    post = rtPost rt
