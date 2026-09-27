-- | The only place that writes to the terminal.
module Him.Terminal.Output
  ( writeOutput
  ) where

import Data.ByteString.Builder (Builder, hPutBuilder)
import System.IO (hFlush, stdout)

-- | Write a whole frame's worth of output at once, then flush, so the
-- terminal never shows a half-drawn screen.
writeOutput :: Builder -> IO ()
writeOutput b = hPutBuilder stdout b >> hFlush stdout
