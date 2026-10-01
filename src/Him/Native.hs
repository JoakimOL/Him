{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE UnliftedFFITypes #-}

-- | Fast byte-level operations on 'Text' (UTF-8 in text >= 2), implemented
-- in @cbits/text.c@. The Text's array is passed to C directly; the calls are
-- @unsafe@, so the GC cannot move it in the meantime.
module Him.Native
  ( Offsets
  , offsetAt
  , lineStarts
  , countNewlines
  , findForward
  , findBackward
  ) where

import Data.Text.Array qualified as A
import Data.Text.Internal (Text (..))

import Foreign.C.Types (CInt (..), CPtrdiff (..), CSize (..), CUChar (..))
import GHC.Exts
import GHC.IO (IO (..), unsafeDupablePerformIO)
import GHC.Word (Word32 (..))

-- | An immutable array of 32-bit byte offsets.
data Offsets = Offsets ByteArray#

foreign import ccall unsafe "him_count_byte"
  c_countByte :: ByteArray# -> CSize -> CSize -> CUChar -> CSize

foreign import ccall unsafe "him_line_starts"
  c_lineStarts :: ByteArray# -> CSize -> CSize -> MutableByteArray# RealWorld -> IO ()

foreign import ccall unsafe "him_find_forward"
  c_findForward :: ByteArray# -> CSize -> CSize -> ByteArray# -> CSize -> CSize -> CInt -> CPtrdiff

foreign import ccall unsafe "him_find_backward"
  c_findBackward :: ByteArray# -> CSize -> CSize -> ByteArray# -> CSize -> CSize -> CInt -> CPtrdiff

offsetAt :: Offsets -> Int -> Int
offsetAt (Offsets ba) (I# i) = fromIntegral (W32# (indexWord32Array# ba i))
{-# INLINE offsetAt #-}

countNewlines :: Text -> Int
countNewlines (Text (A.ByteArray arr) off len) =
  fromIntegral (c_countByte arr (fromIntegral off) (fromIntegral len) 10)

-- | Start offsets (in bytes, relative to the text) of every line of a text,
-- followed by @length + 1@: @lines + 1@ entries for @countNewlines + 1@
-- lines.
lineStarts :: Text -> Offsets
lineStarts t@(Text (A.ByteArray arr) off len) = unsafeDupablePerformIO $ IO $ \s0 ->
  let !(I# bytes) = 4 * (countNewlines t + 2)
   in case newByteArray# bytes s0 of
        (# s1, mba #) -> case c_lineStarts arr (fromIntegral off) (fromIntegral len) mba of
          IO run -> case run s1 of
            (# s2, () #) -> case unsafeFreezeByteArray# mba s2 of
              (# s3, ba #) -> (# s3, Offsets ba #)

-- | Byte offset of the first occurrence of a needle in a text. With
-- @fold@, the needle must be lower-case and ASCII letters match either case.
findForward :: Bool -> Text -> Text -> Maybe Int
findForward fold (Text (A.ByteArray narr) noff nlen) (Text (A.ByteArray harr) hoff hlen) =
  result (c_findForward harr (fromIntegral hoff) (fromIntegral hlen) narr (fromIntegral noff) (fromIntegral nlen) (if fold then 1 else 0))

-- | Byte offset of the last occurrence.
findBackward :: Bool -> Text -> Text -> Maybe Int
findBackward fold (Text (A.ByteArray narr) noff nlen) (Text (A.ByteArray harr) hoff hlen) =
  result (c_findBackward harr (fromIntegral hoff) (fromIntegral hlen) narr (fromIntegral noff) (fromIntegral nlen) (if fold then 1 else 0))

result :: CPtrdiff -> Maybe Int
result r = if r < 0 then Nothing else Just (fromIntegral r)

