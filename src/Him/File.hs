{-# LANGUAGE MultiWayIf #-}

-- | Loading and saving documents.
module Him.File
  ( loadDocument
  , saveDocument
  , decodeDocument
  , encodeDocument
  ) where

import Control.Exception (IOException, try)
import Data.ByteString (ByteString)
import Data.ByteString qualified as B
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding (decodeUtf8Lenient, encodeUtf8)
import Him.Buffer qualified as Buffer
import Him.Document
import System.Directory (doesDirectoryExist, doesFileExist)

-- | Load a file. A file that does not exist yet gives an empty document bound
-- to that path (it is created on the first save).
loadDocument :: FilePath -> IO (Either Text Document)
loadDocument path = do
  isFile <- doesFileExist path
  isDir <- doesDirectoryExist path
  if
    | isDir -> pure (Left (T.pack path <> " is a directory"))
    | not isFile -> pure (Right (newDocument (Just path) Buffer.empty))
    | otherwise ->
        try (B.readFile path) >>= \case
          Left e -> pure (Left (T.pack (show (e :: IOException))))
          Right bytes -> pure (Right (decodeDocument (Just path) bytes))

-- | Write the document to a path. Returns the number of bytes written.
saveDocument :: FilePath -> Document -> IO (Either Text Int)
saveDocument path doc = do
  let bytes = encodeDocument doc
  try (B.writeFile path bytes) >>= \case
    Left e -> pure (Left (T.pack (show (e :: IOException))))
    Right () -> pure (Right (B.length bytes))

-- | Decode file contents: UTF-8 (invalid bytes become U+FFFD), detecting
-- CRLF line endings and whether there is a final newline.
decodeDocument :: Maybe FilePath -> ByteString -> Document
decodeDocument path bytes =
  (newDocument path (Buffer.fromText body))
    { docLineEnding = ending
    , docTrailingNewline = T.null normalized || hasFinalNewline
    }
  where
    text = decodeUtf8Lenient bytes
    ending = if "\r\n" `T.isInfixOf` text then CRLF else LF
    normalized = if ending == CRLF then T.replace "\r\n" "\n" text else text
    hasFinalNewline = "\n" `T.isSuffixOf` normalized
    body = if hasFinalNewline then T.dropEnd 1 normalized else normalized

encodeDocument :: Document -> ByteString
encodeDocument doc
  | Buffer.toLines buf == [""] = B.empty
  | otherwise = encodeUtf8 (T.intercalate sep (Buffer.toLines buf) <> final)
  where
    buf = docBuffer doc
    sep = case docLineEnding doc of
      LF -> "\n"
      CRLF -> "\r\n"
    final = if docTrailingNewline doc then sep else ""
