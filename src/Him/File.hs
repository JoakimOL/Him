{-# LANGUAGE MagicHash #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE UnboxedTuples #-}

-- | Loading and saving documents.
--
-- Both directions stream: files are read and decoded in chunks, and every
-- line stays a slice of its chunk's text (no per-line copies), and saving
-- writes the lines straight to the file. Peak memory is therefore about the
-- size of the text itself, not several copies of the whole file.
module Him.File
  ( loadDocument
  , loadDocumentChunked
  , saveDocument
  , decodeDocument
  , decodeChunks
  , encodeDocument
  , encodeBuilder
  ) where

import Control.Exception (IOException, evaluate, try)
import Data.Bits ((.&.))
import Data.ByteString.Unsafe (unsafePackCStringLen)
import Data.Word (Word8)
import Foreign.Marshal.Alloc (allocaBytes)
import Foreign.Marshal.Utils (copyBytes)
import Data.Array.Byte qualified as BA
import Data.Text.Array qualified as A
import Data.Text.Internal (Text (..))
import Data.Text.Internal.Validate (isValidUtf8ByteArray)
import Foreign.Ptr (Ptr, castPtr, plusPtr)
import GHC.Exts (Int (..), MutableByteArray#, Ptr (..), RealWorld, byteArrayContents#, isByteArrayPinned#, isTrue#, keepAlive#, mutableByteArrayContents#, newPinnedByteArray#, touch#, unsafeFreezeByteArray#)
import GHC.IO (IO (..), unIO)
import Data.ByteString (ByteString)
import Data.ByteString qualified as B
import Data.ByteString.Builder (Builder, hPutBuilder, toLazyByteString)
import Data.ByteString.Lazy qualified as BL
import Data.List (intersperse)
import Data.Maybe (fromMaybe)
import Data.Text qualified as T
import Data.Text.Encoding (Decoding (..), decodeUtf8Lenient, encodeUtf8Builder, streamDecodeUtf8With)
import Data.Text.Encoding.Error (lenientDecode)
import Him.Buffer qualified as Buffer
import Him.Document
import System.Directory (doesDirectoryExist, doesFileExist)
import System.IO (BufferMode (..), Handle, IOMode (..), hFileSize, hGetBuf, hIsEOF, hPutBuf, hSetBuffering, withBinaryFile)

-- | Load a file. A file that does not exist yet gives an empty document bound
-- to that path (it is created on the first save).
loadDocument :: FilePath -> IO (Either Text Document)
loadDocument = loadWith readFileLines

-- | 'loadDocument' using only the chunked reader (what pipes get); exposed
-- so tests can check it on ordinary files.
loadDocumentChunked :: FilePath -> IO (Either Text Document)
loadDocumentChunked = loadWith readLines

loadWith :: (Handle -> IO Lines) -> FilePath -> IO (Either Text Document)
loadWith reader path = do
  isFile <- doesFileExist path
  isDir <- doesDirectoryExist path
  if
    | isDir -> pure (Left (T.pack path <> " is a directory"))
    | not isFile -> pure (Right (newDocument (Just path) Buffer.empty))
    | otherwise ->
        try (withBinaryFile path ReadMode reader) >>= \case
          Left e -> pure (Left (T.pack (show (e :: IOException))))
          Right acc -> pure (Right (finish (Just path) acc))

chunkSize :: Int
chunkSize = 1024 * 1024

-- | Regular files are read whole, straight into one pinned array the size
-- of the file. If the bytes are valid UTF-8 (checked in place), that array
-- /is/ the text: no decoding copy at all. Otherwise, and for files whose
-- size is unknown (pipes), fall back to the chunked reader.
readFileLines :: Handle -> IO Lines
readFileLines h = do
  size <- try (hFileSize h)
  case size of
    Right n | n > 0 && n < fromIntegral (maxBound :: Int) -> do
      text <- readWhole h (fromIntegral n)
      eof <- hIsEOF h
      let acc = addChunk emptyLines text
      -- The file grew while we read it: read the rest in chunks.
      if eof then pure acc else readLinesFrom acc h
    -- Size 0 is also what pipes report: read until EOF.
    Right _ -> readLines h
    Left (_ :: IOException) -> readLines h

data MutableBytes = MutableBytes (MutableByteArray# RealWorld)

readWhole :: Handle -> Int -> IO Text
readWhole h size@(I# size#) = do
  MutableBytes mba <- IO $ \s -> case newPinnedByteArray# size# s of
    (# s', m #) -> (# s', MutableBytes m #)
  let ptr = Ptr (mutableByteArrayContents# mba) :: Ptr Word8
      fill done
        | done >= size = pure done
        | otherwise = do
            n <- hGetBuf h (ptr `plusPtr` done) (size - done)
            if n == 0 then pure done else fill (done + n)
  got <- fill 0
  IO $ \s -> case unsafeFreezeByteArray# mba s of
    (# s', ba #)
      | isValidUtf8ByteArray (BA.ByteArray ba) 0 got -> (# s', Text (A.ByteArray ba) 0 got #)
      | otherwise -> case unIO (decodeUtf8Lenient <$> B.packCStringLen (castPtr ptr, got)) s' of
          (# s'', t #) -> case touch# mba s'' of s''' -> (# s''', t #)

-- | Read a file in chunks into one reused buffer. Each chunk's complete
-- UTF-8 prefix is decoded (copied once, into the text that stays alive);
-- an incomplete character at the end is moved to the front of the buffer
-- for the next read. No garbage proportional to the file size is created.
readLines :: Handle -> IO Lines
readLines = readLinesFrom emptyLines

readLinesFrom :: Lines -> Handle -> IO Lines
readLinesFrom start h = allocaBytes chunkSize $ \buf -> loop buf 0 start
  where
    loop :: Ptr Word8 -> Int -> Lines -> IO Lines
    loop buf pending acc = do
      n <- hGetBuf h (buf `plusPtr` pending) (chunkSize - pending)
      let total = pending + n
      bytes <- unsafePackCStringLen (castPtr buf, total)
      if n == 0
        then pure (if total == 0 then acc else addChunk acc (decodeUtf8Lenient (B.copy bytes)))
        else do
          let cut = completePrefix bytes
          !text <- evaluate (decodeUtf8Lenient (B.take cut bytes))
          !acc' <- evaluate (addChunk acc text)
          copyBytes buf (buf `plusPtr` cut) (total - cut)
          loop buf (total - cut) acc'

-- | Length of the longest prefix that does not end inside a UTF-8 sequence.
completePrefix :: ByteString -> Int
completePrefix bs = case [i | i <- [len - 1, len - 2 .. max 0 (len - 4)], B.index bs i .&. 0xC0 /= 0x80] of
  (i : _) | i + seqLen (B.index bs i) > len -> i
  _ -> len
  where
    len = B.length bs
    seqLen b
      | b .&. 0x80 == 0 = 1
      | b .&. 0xE0 == 0xC0 = 2
      | b .&. 0xF0 == 0xE0 = 3
      | b .&. 0xF8 == 0xF0 = 4
      | otherwise = 1

-- | Write the document to a path. Returns the number of bytes written.
saveDocument :: FilePath -> Document -> IO (Either Text Int)
saveDocument path doc =
  try (withBinaryFile path WriteMode write) >>= \case
    Left e -> pure (Left (T.pack (show (e :: IOException))))
    Right n -> pure (Right (fromIntegral n))
  where
    write h = do
      hSetBuffering h (BlockBuffering (Just (64 * 1024)))
      writeDocument h doc
      hFileSize h

-- | Like @hPutBuilder h (encodeBuilder doc)@, but a large region held in a
-- pinned array (a loaded file) is handed to 'hPutBuf' directly instead of
-- being copied into the builder's buffer first.
writeDocument :: Handle -> Document -> IO ()
writeDocument h doc
  | Buffer.lineCount buf == 1 && T.null (Buffer.lineAt 0 buf) = pure ()
  | otherwise = do
      sequence_ (intersperse (hPutBuilder h sep) (map region (Buffer.regions buf)))
      if docTrailingNewline doc then hPutBuilder h sep else pure ()
  where
    buf = docBuffer doc
    crlf = docLineEnding doc == CRLF
    sep = if crlf then "\r\n" else "\n"
    region r
      | Buffer.regionCR r == crlf = putText (Buffer.regionText r)
      | otherwise = hPutBuilder h (mconcat (intersperse sep (map encodeUtf8Builder (Buffer.regionLines r))))
    putText t@(Text (A.ByteArray arr) off len)
      | len >= 64 * 1024 && isTrue# (isByteArrayPinned# arr) =
          IO $ \s -> keepAlive# arr s (unIO (hPutBuf h (Ptr (byteArrayContents# arr) `plusPtr` off) len))
      | otherwise = hPutBuilder h (encodeUtf8Builder t)

-- | Regions of complete lines read so far (newest first), the text after the
-- last newline seen, and whether lines end in CRLF (decided by the first
-- complete line).
data Lines = Lines ![(Bool, Text)] !Text !(Maybe Bool)

emptyLines :: Lines
emptyLines = Lines [] T.empty Nothing

-- | Add decoded text. Everything up to the chunk's last newline becomes one
-- region (a slice of the chunk: no copy). The line that started in the
-- previous chunk is completed with a small copy and stored on its own.
addChunk :: Lines -> Text -> Lines
addChunk (Lines regions carry cr) text
  | T.null rest = Lines regions (carry <> text) cr
  | otherwise = Lines (maybe id (\m -> ((isCR, m) :)) middle ((isCR, firstLine) : regions)) after cr'
  where
    (first, rest) = T.breakOn "\n" text
    firstLine = carry <> first
    cr' = Just (fromMaybe ("\r" `T.isSuffixOf` firstLine) cr)
    isCR = cr' == Just True
    -- Lines between the first and the last newline of the chunk. (Not
    -- 'T.breakOnEnd': it reverses, i.e. copies, the whole chunk twice;
    -- these scan from the end and return slices.)
    body = T.drop 1 rest
    upToLast = T.dropWhileEnd (/= '\n') body
    after = T.takeWhileEnd (/= '\n') body
    middle = if T.null upToLast then Nothing else Just (T.dropEnd 1 upToLast)

-- | Build the document. The text after the last newline is the final line
-- (empty if the file ends with a newline).
finish :: Maybe FilePath -> Lines -> Document
finish path (Lines regions carry cr) =
  (newDocument path (Buffer.fromRegions (reverse finalRegions)))
    { docLineEnding = if cr == Just True then CRLF else LF
    , docTrailingNewline = T.null carry
    }
  where
    finalRegions
      | T.null carry && not (null regions) = regions
      | otherwise = (False, carry) : regions

-- | Decode complete file contents (see 'loadDocument').
decodeDocument :: Maybe FilePath -> ByteString -> Document
decodeDocument path = finish path . addChunk emptyLines . decodeUtf8Lenient

-- | Decode file contents that arrive in chunks, exactly as 'loadDocument'
-- does (exposed for testing chunk boundaries).
decodeChunks :: Maybe FilePath -> [ByteString] -> Document
decodeChunks path = finish path . go (streamDecodeUtf8With lenientDecode) emptyLines
  where
    go _ acc [] = acc
    go decode acc (c : cs) = let Some text _ next = decode c in go next (addChunk acc text) cs

encodeDocument :: Document -> ByteString
encodeDocument = BL.toStrict . toLazyByteString . encodeBuilder

-- | The file contents, as a builder. Regions of the buffer whose line
-- endings already match the file's are copied in one piece (for an
-- unedited file that is nearly everything); others are written line by line.
encodeBuilder :: Document -> Builder
encodeBuilder doc
  | Buffer.lineCount buf == 1 && T.null (Buffer.lineAt 0 buf) = mempty
  | otherwise = mconcat (intersperse sep (map region (Buffer.regions buf))) <> final
  where
    buf = docBuffer doc
    crlf = docLineEnding doc == CRLF
    sep = if crlf then "\r\n" else "\n"
    final = if docTrailingNewline doc then sep else mempty
    region r
      | Buffer.regionCR r == crlf = encodeUtf8Builder (Buffer.regionText r)
      | otherwise = mconcat (intersperse sep (map encodeUtf8Builder (Buffer.regionLines r)))
