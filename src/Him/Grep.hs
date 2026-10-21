-- | Searching the files of a project for text, for the global search
-- picker (@space /@, ADR global-search). The pattern is a "Him.Search" needle
-- (literal, smart case); the scan runs in C over the whole file
-- ("Him.Native"), one hit per line. The hits of a search are kept in a few
-- arrays ('Hits'), so a picker can hold a million of them.
module Him.Grep
  ( Hit (..)
  , grepText
  , grepFile
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

import Control.Exception (IOException, try)
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
import System.IO (IOMode (..), hFileSize, withBinaryFile)

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

-- | The hits in a file; none for a file that cannot be read, is too large,
-- or looks binary (a NUL in its first 8 KB, as for the preview).
grepFile :: Needle -> FilePath -> IO [Hit]
grepFile n path =
  try @IOException read' >>= \case
    Right (Just bytes) | not (BS.elem 0 (BS.take 8192 bytes)) -> pure (grepText n (decodeUtf8Lenient bytes))
    _ -> pure []
  where
    read' = withBinaryFile path ReadMode $ \h -> do
      size <- hFileSize h
      if size > toInteger maxGrepSize then pure Nothing else Just <$> BS.hGet h (fromInteger size)

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
