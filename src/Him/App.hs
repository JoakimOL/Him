-- | Entry point of the editor. Grows into the main event loop
-- (read event -> resolve key -> run command -> render) in later milestones.
module Him.App
  ( run
  ) where

import Him.Log (logMsg)
import Him.Terminal.Size (getWindowSize)

-- | Run the editor, optionally opening the given file.
run :: Maybe FilePath -> IO ()
run file = do
  logMsg ("starting, file = " <> show file)
  size <- getWindowSize
  putStrLn ("him 0.1.0.0 - terminal size: " <> maybe "unknown (not a tty)" showSize size)
  where
    showSize (rows, cols) = show cols <> "x" <> show rows
