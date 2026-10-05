-- | Searching the files of a project for text, for the global search
-- picker (@space /@, ADR-46). The pattern is a "Him.Search" needle
-- (literal, smart case); the scan runs in C over the whole file
-- ("Him.Native"), one hit per line.
module Him.Grep
  ( Hit (..)
  , grepText
  , grepFile
  , maxGrepSize
  , maxHitText
  ) where

import Control.Exception (IOException, try)
import Data.ByteString qualified as BS
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding (decodeUtf8Lenient)
import Data.Text.Unsafe (dropWord8, takeWord8)
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
  -- ^ The line without its line break, at most 'maxHitText' characters
  -- (copied, so a hit does not keep its file's text alive).
  }
  deriving stock (Eq, Show)

-- | The lines of a text that contain the needle, in order.
grepText :: Needle -> Text -> [Hit]
grepText n = go 0
  where
    go line t = case Native.findForward (needleFold n) (needleText n) t of
      Nothing -> []
      Just off ->
        let before = takeWord8 off t
            after = dropWord8 off t
            line' = line + Native.countNewlines before
            start = T.takeWhileEnd (/= '\n') before
            (end, rest) = T.break (== '\n') after
            text = start <> (if T.isSuffixOf "\r" end then T.dropEnd 1 end else end)
         in Hit line' (T.length start) (T.copy (T.take maxHitText text)) : go (line' + 1) (T.drop 1 rest)

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
