-- | The runtime (ADR effects-and-runtime): runs the background jobs actions ask for
-- ("Him.Effect"), each on its own thread, and posts their results to the
-- main loop's event channel. At most one job per 'JobKey' runs; starting
-- another cancels the old one.
module Him.Runtime
  ( Runtime
  , newRuntime
  , perform
  , shutdown
  , reconfigure
  ) where

import Control.Concurrent (ThreadId, forkIO, killThread, threadDelay)
import Control.Concurrent.MVar (MVar, modifyMVar, modifyMVar_, newEmptyMVar, newMVar, putMVar, readMVar, tryReadMVar)
import Control.Monad (when)
import Data.Text (Text)
import Data.Text qualified as T
import Him.Lsp.Config (ServerConfig (..), serverFor)
import Him.Lsp.Server (Server, findRoot, sendMessage, startServer, stopServer)
import Him.Repl (ReplConfig (..))
import Him.Chat (ChatConfig (..), ChatEvent (..), ChatProvider (..), ChatSession (..))
import Him.Config (Config (..))
import Him.Repl.Process (ReplProcess, interruptRepl, sendRepl, startRepl, stopRepl)
import Him.Lsp.State (ServerInfo)
import Control.Exception (IOException, try)
import Data.ByteString qualified as BS
import Him.Document (Document (..))
import Him.File (loadDocument)
import System.Directory (getCurrentDirectory, getFileSize, makeAbsolute)
import System.IO (IOMode (..), withBinaryFile)
import Control.Exception (evaluate)
import Data.Foldable (toList)
import Data.IORef (IORef, atomicModifyIORef', newIORef, readIORef, writeIORef)
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
import Him.Picker (PickTarget (..), matchLimit, pickerItem, rank)
import Him.Grep (Hit (..), grepFile)
import Him.Search (compileNeedle)
import Data.Foldable (for_)
import Him.Syntax (SyntaxProvider, SyntaxSession (..), startSyntax)
import Him.Language (detectLanguage, languages)
import Data.Unique (Unique, newUnique)
import Him.Spawn (Spawned, sendSpawned, spawnLines, stopSpawned)

data Runtime = Runtime
  { rtPost :: Event -> IO ()
  , rtJobs :: MVar (Map JobKey ThreadId)
  , rtProviders :: [SyntaxProvider]
  , rtSessions :: MVar (Map Int SyntaxSession)
  -- ^ Highlighters by document id. The editor only sees their results.
  , rtConfig :: IORef Config
  -- ^ The tables it reads (servers, REPLs, the chat provider); replaced
  -- when the config is reloaded ('reconfigure').
  , rtRepls :: MVar (Map Int ReplProcess)
  -- ^ Running REPLs, by the id of their buffer.
  , rtChats :: MVar (Map Int ChatEntry)
  -- ^ Chat sessions, by chat buffer.
  , rtServers :: MVar (Map Text (MVar (Either Text (Server, ServerInfo))))
  -- ^ Language servers by key; the inner variable is filled once the
  -- server has started (or failed to).
  , rtProcesses :: MVar (Map Text (Unique, Spawned))
  -- ^ Plugin processes, by key (@plugin:name@).
  }

newRuntime :: Config -> (Event -> IO ()) -> IO Runtime
newRuntime config post = do
  jobs <- newMVar Map.empty
  sessions <- newMVar Map.empty
  configRef <- newIORef config
  replProcesses <- newMVar Map.empty
  chats <- newMVar Map.empty
  servers <- newMVar Map.empty
  processes <- newMVar Map.empty
  pure (Runtime post jobs (cfgSyntaxProviders config) sessions configRef replProcesses chats servers processes)

-- | Use another config's tables from now on (after a reload; running
-- servers and REPLs keep running).
reconfigure :: Runtime -> Config -> IO ()
reconfigure rt = writeIORef (rtConfig rt)

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
  ReplStart doc language file -> do
    -- Started at once (not as a job), so text sent right after reaches it.
    table <- cfgRepls <$> readIORef (rtConfig rt)
    case Map.lookup language table of
      Nothing -> rtPost rt (EvJob (ReplExited doc ("no REPL for " <> language <> " (add [repl." <> language <> "] to the config)")))
      Just config -> do
        absolute <- if null file then getCurrentDirectory else makeAbsolute file
        root <- if null file then pure absolute else findRoot (rcRoots config) absolute
        stopped <- modifyMVar (rtRepls rt) (\m -> pure (Map.delete doc m, Map.lookup doc m))
        mapM_ stopRepl stopped
        let post = rtPost rt . EvJob
        startRepl config root (post . ReplOutput doc) (\why -> modifyMVar_ (rtRepls rt) (pure . Map.delete doc) >> post (ReplExited doc why)) >>= \case
          Left e -> post (ReplExited doc e)
          Right rp -> do
            modifyMVar_ (rtRepls rt) (pure . Map.insert doc rp)
            post (ReplStarted doc config root)
  ChatSend doc req -> do
    config <- readIORef (rtConfig rt)
    let cc = cfgChat config
        post = rtPost rt . EvJob . ChatReply doc
    case [p | p <- cfgChatProviders config, cpName p == ccProvider cc] of
      [] -> post (ChatFailed ("no chat provider named " <> ccProvider cc))
      provider : _ -> do
        -- The session is kept across turns (and replaced when the configured
        -- provider changes). A finished turn is not cancelled: for Claude
        -- Code that would end the process (ADR claude-code-provider); only ChatCancel does.
        session <- modifyMVar (rtChats rt) $ \m -> case Map.lookup doc m of
          Just e | ceProvider e == cpName provider -> pure (m, ceSession e)
          old -> do
            mapM_ (sessClose . ceSession) old
            s <- cpStart provider
            pure (Map.insert doc (ChatEntry (cpName provider) s Nothing) m, s)
        cancel <- sessSend session cc req post
        modifyMVar_ (rtChats rt) (pure . Map.adjust (\e -> e {ceCancel = Just cancel}) doc)
  ChatCancel doc -> cancelChatIn rt doc
  ChatAnswer doc callId isError result ->
    Map.lookup doc <$> readMVar (rtChats rt) >>= mapM_ (\e -> sessAnswer (ceSession e) callId isError result)
  ReplSend doc asCode text -> withRepl rt doc (\rp -> sendRepl rp asCode text)
  ReplInterrupt doc -> withRepl rt doc interruptRepl
  ReplStop doc -> modifyMVar (rtRepls rt) (\m -> pure (Map.delete doc m, Map.lookup doc m)) >>= mapM_ stopRepl
  LspStopAll -> do
    stopped <- modifyMVar (rtServers rt) (\servers -> pure (Map.empty, Map.elems servers))
    mapM_ (\started -> tryReadMVar started >>= mapM_ (either (const (pure ())) (stopServer . fst))) stopped
  ProcessStart key command args dir -> do
    takeProcesses rt (== key) >>= mapM_ stopSpawned
    me <- newUnique
    registered <- newEmptyMVar
    let post = rtPost rt . EvJob
        -- A process stopped or replaced under its key says nothing more;
        -- the end waits until the process is in the table.
        exited code = do
          readMVar registered
          current <- modifyMVar (rtProcesses rt) $ \m -> case Map.lookup key m of
            Just (u, _) | u == me -> pure (Map.delete key m, True)
            _ -> pure (m, False)
          when current (post (ProcessDone key code))
    spawnLines command args dir (post . ProcessLine key) exited >>= \case
      Left e -> post (ProcessLine key e) >> post (ProcessDone key (-1))
      Right sp -> modifyMVar_ (rtProcesses rt) (pure . Map.insert key (me, sp)) >> putMVar registered ()
  ProcessSend key text -> Map.lookup key <$> readMVar (rtProcesses rt) >>= mapM_ ((`sendSpawned` text) . snd)
  ProcessStop key -> takeProcesses rt (== key) >>= mapM_ stopSpawned
  ProcessStopAll owner -> takeProcesses rt ((== owner) . T.takeWhile (/= ':')) >>= mapM_ stopSpawned
  _ -> pure ()

-- | Take the processes whose keys match out of the table.
takeProcesses :: Runtime -> (Text -> Bool) -> IO [Spawned]
takeProcesses rt match = modifyMVar (rtProcesses rt) $ \m ->
  let (taken, kept) = Map.partitionWithKey (\k _ -> match k) m
   in pure (kept, map snd (Map.elems taken))

-- | A chat buffer's session: which provider started it, and how to cancel
-- the turn in flight.
data ChatEntry = ChatEntry
  { ceProvider :: Text
  , ceSession :: ChatSession
  , ceCancel :: Maybe (IO ())
  }

-- | Cancel a chat's turn, if one runs.
cancelChatIn :: Runtime -> Int -> IO ()
cancelChatIn rt doc =
  modifyMVar (rtChats rt) (\m -> pure (Map.adjust (\e -> e {ceCancel = Nothing}) doc m, Map.lookup doc m >>= ceCancel)) >>= sequence_

-- | Run something with a REPL, if it is running.
withRepl :: Runtime -> Int -> (ReplProcess -> IO ()) -> IO ()
withRepl rt doc k = Map.lookup doc <$> readMVar (rtRepls rt) >>= mapM_ k

-- | Stop every language server and REPL (when the editor quits).
shutdown :: Runtime -> IO ()
shutdown rt = do
  readMVar (rtRepls rt) >>= mapM_ stopRepl
  readMVar (rtProcesses rt) >>= mapM_ (stopSpawned . snd)
  readMVar (rtChats rt) >>= mapM_ (sessClose . ceSession)
  servers <- readMVar (rtServers rt)
  mapM_ (\started -> tryReadMVar started >>= mapM_ (either (const (pure ())) (stopServer . fst))) (Map.elems servers)

-- | A preview highlights at most this many lines from the top (a search
-- hit further down shows plain).
previewHighlightLines :: Int
previewHighlightLines = 20000

runJob :: Runtime -> Job -> IO ()
runJob rt = \case
  ScanFiles gen opts root -> do
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
    _ <- walkFiles opts root emit
    rest <- atomicModifyIORef' pending (\(acc, _, t) -> (([], 0, t), acc))
    send rest
    post (EvJob (ScanFinished gen))
  GrepFiles gen query opts root -> do
    -- Debounce: the search for the next key replaces this one (same key)
    -- while it waits, so typing a word searches once.
    threadDelay 80000
    for_ (compileNeedle True query) $ \needle -> do
      -- The walk calls emit from its worker threads, so files are searched
      -- in parallel. Hits are sent in batches (every 50 ms, or sooner for
      -- many), and only until the picker holds matchLimit of them; after
      -- that only the count grows.
      start <- getMonotonicTime
      pending <- newIORef ([], 0 :: Int, 0 :: Int, start)
      let send (items, n) = when (n > 0) (post (EvJob (GrepFound gen query items n)))
          path f = if root == "." then f else root <> "/" <> f
          emit files = do
            found <- concat <$> mapM (\f -> map (item f) <$> grepFile needle (path f)) (sort files)
            let n = length found
            now <- getMonotonicTime
            due <- atomicModifyIORef' pending $ \(acc, count, kept, lastSent) ->
              let new = take (matchLimit - kept) found
                  acc' = acc <> new
                  count' = count + n
                  kept' = kept + length new
               in if length acc' >= 500 || now - lastSent >= 0.05
                    then (([], 0, kept', now), (acc', count'))
                    else ((acc', count', kept', lastSent), ([], 0))
            send due
          item f h = pickerItem (T.pack (f <> ":" <> show (hitLine h + 1))) (PickPosition (path f) (hitLine h) (hitColumn h) Nothing) (T.strip (hitText h))
      _ <- walkFiles opts root emit
      rest <- atomicModifyIORef' pending (\(acc, count, kept, t) -> (([], 0, kept, t), (acc, count)))
      send rest
    post (EvJob (GrepFinished gen query))
  FilterPicker gen query items -> do
    let (best, total) = rank query (toList items)
    _ <- evaluate (length best + total)
    post (EvJob (PickerFiltered gen query best total))
  GitLoad doc path -> do
    base <- loadBase path
    post (EvJob (GitLoaded doc base))
  GitDiff doc version base buffer -> do
    -- Debounce: a newer version's job replaces this one (same key) while
    -- it waits, so a burst of typing is diffed once, after it.
    threadDelay 50000
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
  LspEnsure doc language file -> (cfgServers <$> readIORef (rtConfig rt)) >>= \table -> case serverFor table language of
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
  LoadPreview maxSize file -> do
    -- Binary and very large files are not shown.
    size <- try @IOException (getFileSize file)
    head' <- try @IOException (withBinaryFile file ReadMode (`BS.hGet` 8192))
    result <- case (size, head') of
      (Left e, _) -> pure (Left (T.pack (show e)))
      (Right n, _) | n > toInteger maxSize -> pure (Left "file too large to preview")
      (_, Right bytes) | BS.elem 0 bytes -> pure (Left "binary file")
      _ -> either Left (Right . docBuffer) <$> loadDocument file
    post (EvJob (PreviewLoaded file result))
    -- Then its highlighting, from a session of its own (closed after).
    for_ result $ \buffer -> for_ (detectLanguage languages file (Buffer.lineAt 0 buffer)) $ \language ->
      startSyntax (rtProviders rt) language >>= \case
        Nothing -> pure ()
        Just (_, s) -> do
          ssUpdate s 0 buffer []
          spans <- ssHighlight s 0 (min (Buffer.lineCount buffer) previewHighlightLines - 1)
          ssClose s
          _ <- evaluate (IntMap.size spans)
          post (EvJob (PreviewHighlighted file spans))
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
