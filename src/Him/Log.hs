-- | Debug logging. Stdout is owned by the renderer while the terminal is in
-- raw mode, so diagnostics go to a file instead: set @HIM_LOG=/path/to/file@.
-- Without the variable, logging is a no-op.
module Him.Log
  ( logMsg
  ) where

import System.Environment (lookupEnv)

-- | Append one line to the log file named by @HIM_LOG@, if set.
logMsg :: String -> IO ()
logMsg msg =
  lookupEnv "HIM_LOG" >>= \case
    Nothing -> pure ()
    Just path -> appendFile path (msg <> "\n")
