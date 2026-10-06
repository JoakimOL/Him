-- | Processes plugins start (ADR plugin-building-blocks): output and errors in one stream,
-- read on a thread and handed on a line at a time; input written on
-- request. Like the REPL's ("Him.Repl.Process"), for any program.
module Him.Spawn
  ( Spawned
  , spawnLines
  , sendSpawned
  , stopSpawned
  ) where

import Control.Concurrent (forkIO)
import Control.Exception (IOException, try)
import Control.Monad (void)
import Data.ByteString qualified as BS
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding (encodeUtf8)
import Data.Text.Encoding qualified as TE
import Data.Text.Encoding.Error (lenientDecode)
import System.Exit (ExitCode (..))
import System.IO (Handle, hClose, hFlush, hSetBinaryMode)
import System.Posix.IO (FdOption (..), createPipe, fdToHandle, setFdOption)
import System.Process (CreateProcess (..), ProcessHandle, StdStream (..), createProcess, proc, terminateProcess, waitForProcess)

data Spawned = Spawned
  { spInput :: !Handle
  , spHandle :: !ProcessHandle
  }

-- | Start a program (in a directory, if given). Each line of output goes
-- to @line@ (without its line break; a last line without one too), then
-- @exited@ gets the exit code.
spawnLines :: FilePath -> [String] -> Maybe FilePath -> (Text -> IO ()) -> (Int -> IO ()) -> IO (Either Text Spawned)
spawnLines command args dir line exited = do
  started <- try @IOException $ do
    -- Both ends close on exec, so other processes do not inherit them
    -- (the output would never end).
    (r, w) <- createPipe
    mapM_ (\fd -> setFdOption fd CloseOnExec True) [r, w]
    readEnd <- fdToHandle r
    writeEnd <- fdToHandle w
    (Just hin, _, _, ph) <-
      createProcess (proc command args) {cwd = dir, std_in = CreatePipe, std_out = UseHandle writeEnd, std_err = UseHandle writeEnd}
    pure (readEnd, hin, ph)
  case started of
    Left e -> pure (Left (T.pack command <> ": " <> T.pack (show e)))
    Right (readEnd, hin, ph) -> do
      hSetBinaryMode readEnd True
      hSetBinaryMode hin True
      let loop decode pending = do
            chunk <- either (const BS.empty) id <$> try @IOException (BS.hGetSome readEnd 4096)
            if BS.null chunk
              then do
                if T.null pending then pure () else line pending
                code <- waitForProcess ph
                exited (case code of ExitSuccess -> 0; ExitFailure n -> n)
              else do
                let TE.Some text _ decode' = decode chunk
                    parts = T.splitOn "\n" (pending <> text)
                mapM_ (line . T.dropWhileEnd (== '\r')) (init parts)
                loop decode' (last parts)
      _ <- forkIO (loop (TE.streamDecodeUtf8With lenientDecode) "")
      pure (Right (Spawned hin ph))

-- | Write text to its input; a process that is gone is ignored.
sendSpawned :: Spawned -> Text -> IO ()
sendSpawned sp text = void (try @IOException (BS.hPut (spInput sp) (encodeUtf8 text) >> hFlush (spInput sp)))

stopSpawned :: Spawned -> IO ()
stopSpawned sp = do
  void (try @IOException (hClose (spInput sp)))
  terminateProcess (spHandle sp)
