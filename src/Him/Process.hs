-- | Running external programs (git; ADR-23): feed stdin, collect stdout and
-- stderr as bytes. Both outputs are read concurrently, so a program that
-- writes a lot to one of them cannot block on a full pipe.
module Him.Process
  ( ProcessResult (..)
  , runProcess
  ) where

import Control.Concurrent (forkIO)
import Control.Concurrent.MVar (newEmptyMVar, putMVar, takeMVar)
import Control.Exception (IOException, evaluate, try)
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.Text (Text)
import Data.Text qualified as T
import System.Exit (ExitCode)
import System.IO (hClose)
import System.Process (CreateProcess (..), StdStream (..), createProcess, proc, waitForProcess)

data ProcessResult = ProcessResult
  { prExit :: !ExitCode
  , prStdout :: !ByteString
  , prStderr :: !ByteString
  }
  deriving stock (Eq, Show)

-- | Run a program with arguments in a directory (or the current one),
-- giving it @input@ on stdin. 'Left' when it could not be started.
runProcess :: FilePath -> [String] -> Maybe FilePath -> ByteString -> IO (Either Text ProcessResult)
runProcess cmd args cwd input =
  fmap (either (Left . T.pack . show @IOException) Right) . try $ do
    (Just hin, Just hout, Just herr, ph) <-
      createProcess (proc cmd args) {cwd = cwd, std_in = CreatePipe, std_out = CreatePipe, std_err = CreatePipe}
    errVar <- newEmptyMVar
    _ <- forkIO (BS.hGetContents herr >>= evaluate >>= putMVar errVar)
    outVar <- newEmptyMVar
    _ <- forkIO (BS.hGetContents hout >>= evaluate >>= putMVar outVar)
    -- A program that exits without reading its input closes the pipe.
    _ <- try @IOException (BS.hPut hin input)
    _ <- try @IOException (hClose hin)
    out <- takeMVar outVar
    err <- takeMVar errVar
    code <- waitForProcess ph
    pure (ProcessResult code out err)
