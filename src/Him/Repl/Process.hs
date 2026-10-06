-- | A REPL process (ADR repl): its output and errors in one stream, read on
-- a thread and handed on as text; input written as typed or sent.
module Him.Repl.Process
  ( ReplProcess
  , rpConfig
  , startRepl
  , sendRepl
  , interruptRepl
  , stopRepl
  ) where

import Control.Concurrent (forkIO, threadDelay)
import Control.Exception (IOException, SomeException, try)
import Control.Monad (void)
import Data.ByteString qualified as BS
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding (encodeUtf8)
import Data.Text.Encoding qualified as TE
import Data.Text.Encoding.Error (lenientDecode)
import Him.Repl (ReplConfig (..), wrapCode)
import System.Environment (getEnvironment)
import System.IO (Handle, hClose, hFlush, hSetBinaryMode)
import System.Posix.IO (FdOption (..), createPipe, fdToHandle, setFdOption)
import System.Process (CreateProcess (..), ProcessHandle, StdStream (..), createProcess, interruptProcessGroupOf, proc, terminateProcess, waitForProcess)

data ReplProcess = ReplProcess
  { rpConfig :: !ReplConfig
  , rpInput :: !Handle
  , rpHandle :: !ProcessHandle
  }

-- | Start a REPL in a directory. Output goes to @output@ as it comes;
-- @exited@ gets the reason once the process ends. The process gets a dumb
-- terminal type (no colours) and its own process group (so an interrupt
-- reaches what it runs, not the editor).
startRepl :: ReplConfig -> FilePath -> (Text -> IO ()) -> (Text -> IO ()) -> IO (Either Text ReplProcess)
startRepl config dir output exited = do
  env <- getEnvironment
  started <- try @IOException $ do
    -- Both ends close on exec: a process started meanwhile (a language
    -- server) must not inherit the write end, or the output never ends.
    (r, w) <- createPipe
    mapM_ (\fd -> setFdOption fd CloseOnExec True) [r, w]
    readEnd <- fdToHandle r
    writeEnd <- fdToHandle w
    (Just hin, _, _, ph) <-
      createProcess
        (proc (rcCommand config) (rcArgs config))
          { cwd = Just dir
          , std_in = CreatePipe
          , std_out = UseHandle writeEnd
          , std_err = UseHandle writeEnd
          , create_group = True
          , env = Just (("TERM", "dumb") : [kv | kv@(k, _) <- env, k /= "TERM"])
          }
    pure (readEnd, hin, ph)
  case started of
    Left e -> pure (Left (T.pack (rcCommand config) <> ": " <> T.pack (show e)))
    Right (readEnd, hin, ph) -> do
      hSetBinaryMode readEnd True
      hSetBinaryMode hin True
      let -- Bytes may split a character; the decoder keeps the rest.
          loop decode = do
            chunk <- either (const BS.empty) id <$> try @IOException (BS.hGetSome readEnd 4096)
            if BS.null chunk
              then do
                code <- waitForProcess ph
                exited (T.pack (show code))
              else do
                let TE.Some text _ decode' = decode chunk
                if T.null text then pure () else output text
                loop decode'
      _ <- forkIO (loop (TE.streamDecodeUtf8With lenientDecode))
      pure (Right (ReplProcess config hin ph))

-- | Send text; with @asCode@, several lines are wrapped first
-- ('wrapCode'). A REPL that is gone is ignored.
sendRepl :: ReplProcess -> Bool -> Text -> IO ()
sendRepl rp asCode text =
  void . try @SomeException $ do
    BS.hPut (rpInput rp) (encodeUtf8 (if asCode then wrapCode (Just (rpConfig rp)) text else text))
    hFlush (rpInput rp)

-- | Ctrl-C for the REPL: stop what it is evaluating.
interruptRepl :: ReplProcess -> IO ()
interruptRepl rp = void (try @SomeException (interruptProcessGroupOf (rpHandle rp)))

-- | Close its input (most REPLs then quit), and end it if it is still
-- there a moment later.
stopRepl :: ReplProcess -> IO ()
stopRepl rp = do
  void (try @SomeException (hClose (rpInput rp)))
  void . forkIO $ do
    threadDelay 500000
    void (try @SomeException (terminateProcess (rpHandle rp)))
