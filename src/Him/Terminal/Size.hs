-- | Terminal dimensions, queried through a tiny C shim (@cbits/winsize.c@)
-- because the @unix@ package does not expose @TIOCGWINSZ@.
module Him.Terminal.Size
  ( getWindowSize
  ) where

import Foreign.C.Types (CInt (..))
import Foreign.Marshal.Alloc (alloca)
import Foreign.Ptr (Ptr)
import Foreign.Storable (peek)

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
