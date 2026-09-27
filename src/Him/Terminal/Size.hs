-- | Terminal dimensions, queried through a tiny C shim (@cbits/winsize.c@)
-- because the @unix@ package does not expose @TIOCGWINSZ@.
module Him.Terminal.Size
  ( getWindowSize
  , onResize
  ) where

import Control.Monad (void)
import Foreign.C.Types (CInt (..))
import Foreign.Marshal.Alloc (alloca)
import Foreign.Ptr (Ptr)
import Foreign.Storable (peek)
import System.Posix.Signals (Handler (..), installHandler)
import System.Posix.Signals.Exts (windowChange)

foreign import ccall unsafe "him_get_winsize"
  c_getWinsize :: Ptr CInt -> Ptr CInt -> IO CInt

-- | @(rows, cols)@ of the terminal on stdout, or 'Nothing' if stdout is not a tty.
getWindowSize :: IO (Maybe (Int, Int))
getWindowSize =
  alloca $ \rowsPtr ->
    alloca $ \colsPtr -> do
      rc <- c_getWinsize rowsPtr colsPtr
      if rc /= 0
        then pure Nothing
        else do
          rows <- peek rowsPtr
          cols <- peek colsPtr
          pure (Just (fromIntegral rows, fromIntegral cols))

-- | Call the handler with the new @(rows, cols)@ whenever the terminal is
-- resized (SIGWINCH).
onResize :: ((Int, Int) -> IO ()) -> IO ()
onResize handler =
  void $ installHandler windowChange (Catch (getWindowSize >>= mapM_ handler)) Nothing
