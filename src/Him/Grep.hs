-- | Searching the files of a project for text, for the global search
-- picker (@space /@, ADR global-search). The pattern is a "Him.Search" needle
-- (literal, smart case); the scan runs in C over the file's bytes
-- ("Him.Native"), one hit per line, and only the hit lines are decoded. The hits of a search are kept in a few
-- arrays ('Hits'), so a picker can hold a million of them.
module Him.Grep
  ( Hit (..)
  , grepText
  , grepFiles
  , maxGrepSize
  , maxHitText
  , Hits
  , HitChunk
  , hitChunk
  , chunkSize
  , noHits
  , addChunks
  , hitCount
  , hitsLabelWidth
  , hitAt
  ) where

import Control.Exception (IOException, bracket, evaluate, try)
import Data.ByteString.Unsafe (unsafePackCStringLen)
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import Foreign.Marshal.Alloc (free, mallocBytes)
import Foreign.Ptr (Ptr, castPtr, nullPtr, plusPtr)
import System.Posix.Files (fileSize, getFdStatus)
import System.Posix.IO (OpenMode (..), closeFd, defaultFileFlags, fdReadBuf, openFd)
import Data.Word (Word8)
import Data.Array (Array, listArray, (!))
import Data.Array.Unboxed (UArray)
import Data.Array.Unboxed qualified as U
import Data.Int (Int32)
import Data.IntMap.Strict (IntMap)
import Data.IntMap.Strict qualified as IntMap
import Data.ByteString qualified as BS
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding (decodeUtf8Lenient)
import Data.Text.Unsafe (dropWord8, lengthWord8, takeWord8)
import Him.Native qualified as Native
import Him.Search (Needle, needleFold, needleText)

-- | A line that contains the pattern.
data Hit = Hit
  { hitLine :: !Int
  -- ^ From 0.
  , hitColumn :: !Int
  -- ^ The first match on the line, in characters.
  , hitText :: !Text
  -- ^ The line without its line break, at most 'maxHitText' characters (a
  -- slice of the searched text: 'hitChunk' copies it).
  }
  deriving stock (Eq, Show)

-- | The lines of a text that contain the needle, in order.
grepText :: Needle -> Text -> [Hit]
grepText n t = map hit (Native.grepLines (needleFold n) (needleText n) t)
  where
    slice from to = takeWord8 (to - from) (dropWord8 from t)
    hit (Native.LineHit line start match end) =
      let text = slice start end
          text' = if T.isSuffixOf "\r" text then T.dropEnd 1 text else text
       in Hit line (T.length (slice start match)) (T.take maxHitText text')

-- | Longer lines are cut (minified files).
maxHitText :: Int
maxHitText = 300

-- | Files larger than this are not searched.
maxGrepSize :: Int
maxGrepSize = 20 * 1024 * 1024

-- | The hits in files; none for a file that cannot be read, is too large,
-- or looks binary (a NUL in its first 8 KB, as for the preview). The files
-- are read one at a time into one buffer, so a file with no hits leaves
-- nothing for the garbage collector.
grepFiles :: Needle -> [FilePath] -> IO [(FilePath, [Hit])]
grepFiles n paths = bracket (newIORef (nullPtr, 0)) (\buf -> readIORef buf >>= free . fst) $ \buf ->
  mapM (\path -> (path,) . either (const []) id <$> try @IOException (grepOne buf path)) paths
  where
    grepOne buf path = bracket (openFd path ReadOnly defaultFileFlags) closeFd $ \fd -> do
      size <- fromIntegral . fileSize <$> getFdStatus fd
      if size > maxGrepSize
        then pure []
        else do
          ptr <- room buf size
          got <- readAll fd ptr size
          head' <- unsafePackCStringLen (castPtr ptr, min 8192 got)
          if BS.elem 0 head'
            then pure []
            else Native.grepLinesAt (needleFold n) (needleText n) ptr got >>= mapM (hit ptr)
    -- A line is decoded (copied) from the buffer, at most as many bytes as
    -- 'maxHitText' characters can take.
    hit ptr (Native.LineHit line start match end) = do
      before <- unsafePackCStringLen (castPtr ptr `plusPtr` start, match - start)
      bytes <- unsafePackCStringLen (castPtr ptr `plusPtr` start, min (end - start) (4 * maxHitText))
      let text = T.take maxHitText (decodeUtf8Lenient bytes)
          text' = if T.isSuffixOf "\r" text then T.dropEnd 1 text else text
      evaluate (Hit line (T.length (decodeUtf8Lenient before)) text')
    readAll fd ptr size = fill 0
      where
        fill done
          | done >= size = pure done
          | otherwise = do
              k <- fromIntegral <$> fdReadBuf fd (ptr `plusPtr` done) (fromIntegral (size - done))
              if k == 0 then pure done else fill (done + k)

-- | The buffer, grown to hold at least n bytes.
room :: IORef (Ptr Word8, Int) -> Int -> IO (Ptr Word8)
room buf n =
  readIORef buf >>= \case
    (ptr, cap) | cap >= n && ptr /= nullPtr -> pure ptr
    (ptr, _) -> do
      free ptr
      let cap = max n (64 * 1024)
      new <- mallocBytes cap
      writeIORef buf (new, cap)
      pure new

-- | The hits of several files, packed: a file index, line, column and text
-- end per hit in unboxed arrays, and their lines in one text.
data HitChunk = HitChunk
  { hcFiles :: !(Array Int FilePath)
  , hcFile :: !(UArray Int Int32)
  , hcLine :: !(UArray Int Int32)
  , hcColumn :: !(UArray Int Int32)
  , hcEnd :: !(UArray Int Int32)
  -- ^ Where each hit's line ends in 'hcText', in bytes.
  , hcText :: !Text
  , hcLabelWidth :: !Int
  -- ^ The longest @path:line@ label.
  }
  deriving stock (Eq, Show)

-- | Pack files' hits ('Nothing' if there are none). The lines are copied,
-- so the chunk does not keep the files' texts alive.
hitChunk :: [(FilePath, [Hit])] -> Maybe HitChunk
hitChunk found
  | n == 0 = Nothing
  | otherwise =
      Just
        HitChunk
          { hcFiles = listArray (0, length files - 1) (map fst files)
          , hcFile = arr [fromIntegral i | (i, (_, hs)) <- zip [0 :: Int ..] files, _ <- hs]
          , hcLine = arr (map (fromIntegral . hitLine) hits)
          , hcColumn = arr (map (fromIntegral . hitColumn) hits)
          , hcEnd = arr (map fromIntegral (drop 1 (scanl (+) 0 (map (lengthWord8 . hitText) hits))))
          , hcText = T.concat (map hitText hits)
          , hcLabelWidth = maximum [length f + 1 + length (show (hitLine h + 1)) | (f, hs) <- files, h <- hs]
          }
  where
    files = filter (not . null . snd) found
    hits = concatMap snd files
    n = length hits
    arr :: [Int32] -> UArray Int Int32
    arr = U.listArray (0, n - 1)

chunkSize :: HitChunk -> Int
chunkSize c = let (lo, hi) = U.bounds (hcLine c) in hi - lo + 1

-- | A search's hits, in the order they arrived: chunks by the index of
-- their first hit.
data Hits = Hits !Int !Int !(IntMap HitChunk)
  deriving stock (Eq, Show)

noHits :: Hits
noHits = Hits 0 0 IntMap.empty

addChunks :: [HitChunk] -> Hits -> Hits
addChunks cs hs = foldl' add hs cs
  where
    add (Hits n w m) c = Hits (n + chunkSize c) (max w (hcLabelWidth c)) (IntMap.insert n c m)

hitCount :: Hits -> Int
hitCount (Hits n _ _) = n

-- | The longest @path:line@ label among the hits.
hitsLabelWidth :: Hits -> Int
hitsLabelWidth (Hits _ w _) = w

-- | The i-th hit and its file.
hitAt :: Hits -> Int -> Maybe (FilePath, Hit)
hitAt (Hits n _ m) i
  | i < 0 || i >= n = Nothing
  | otherwise = do
      (start, c) <- IntMap.lookupLE i m
      let j = i - start
          end = fromIntegral (hcEnd c U.! j)
          begin = if j == 0 then 0 else fromIntegral (hcEnd c U.! (j - 1))
          text = takeWord8 (end - begin) (dropWord8 begin (hcText c))
      pure (hcFiles c ! fromIntegral (hcFile c U.! j), Hit (fromIntegral (hcLine c U.! j)) (fromIntegral (hcColumn c U.! j)) text)
