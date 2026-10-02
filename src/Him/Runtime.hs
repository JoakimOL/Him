-- | The runtime (ADR-23): runs the background jobs actions ask for
-- ("Him.Effect"), each on its own thread, and posts their results to the
-- main loop's event channel. At most one job per 'JobKey' runs; starting
-- another cancels the old one.
module Him.Runtime
  ( Runtime
  , newRuntime
  , perform
  ) where

import Control.Concurrent (ThreadId, forkIO, killThread)
import Control.Concurrent.MVar (MVar, modifyMVar_, newMVar)
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
import Him.Picker (rank)

data Runtime = Runtime
  { rtPost :: Event -> IO ()
  , rtJobs :: MVar (Map JobKey ThreadId)
  }

newRuntime :: (Event -> IO ()) -> IO Runtime
newRuntime post = Runtime post <$> newMVar Map.empty

-- | Carry out an effect that needs the runtime; others are ignored (the
-- main loop handles them before).
perform :: Runtime -> Effect -> IO ()
perform rt = \case
  StartJob job -> modifyMVar_ (rtJobs rt) $ \jobs -> do
    mapM_ killThread (Map.lookup (jobKey job) jobs)
    tid <- forkIO (runJob (rtPost rt) job)
    pure (Map.insert (jobKey job) tid jobs)
  CancelJob key -> modifyMVar_ (rtJobs rt) $ \jobs -> do
    mapM_ killThread (Map.lookup key jobs)
    pure (Map.delete key jobs)
  _ -> pure ()

-- | Most files a picker lists (a memory guard; the scan streams).
maxFiles :: Int
maxFiles = 500000

runJob :: (Event -> IO ()) -> Job -> IO ()
runJob post = \case
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
