-- | Entry point of the editor. Grows into the main event loop
-- (read event -> resolve key -> run command -> render) in later milestones.
module Him.App
  ( run
  ) where

import Data.ByteString qualified as B
import Data.ByteString.Char8 qualified as BC
import Him.Log (logMsg)
import Him.Terminal.Raw (withRawTerminal)
import System.IO (hFlush, stdout)
import System.Posix.IO (stdInput)
import System.Posix.IO.ByteString (fdRead)

-- | Run the editor, optionally opening the given file.
run :: Maybe FilePath -> IO ()
run file = do
  logMsg ("starting, file = " <> show file)
  withRawTerminal $ do
    say "raw mode - press keys to see their bytes, q to quit\r\n"
    loop
  where
    loop = do
      bytes <- fdRead stdInput 64
      say (show (B.unpack bytes) <> "\r\n")
      if bytes == "q" then pure () else loop
    say s = BC.hPut stdout (BC.pack s) >> hFlush stdout
