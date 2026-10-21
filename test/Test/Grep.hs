-- | The global search over files on disk ("Him.Grep.grepFiles").
module Test.Grep
  ( grepTests
  ) where

import Control.Exception (IOException, try)
import Data.ByteString qualified as BS
import Data.Text qualified as T
import Data.Text.Encoding (decodeUtf8Lenient, encodeUtf8)
import Him.Grep (Hit (..), grepFiles, grepText, maxHitText)
import Him.Search (compileNeedle)
import System.Directory (createDirectoryIfMissing, getTemporaryDirectory, removeDirectoryRecursive)
import Test.Harness

grepTests :: IO [Test]
grepTests = do
  tmp <- getTemporaryDirectory
  let root = tmp <> "/him-test-grep"
      files =
        [ ("plain", encodeUtf8 "the cat\nnothing\n  The end")
        , ("crlf", encodeUtf8 "a the\r\nb\r\nthe\r\n")
        , ("invalid", BS.pack [0xff, 0xfe, 0x20] <> encodeUtf8 "the\n" <> BS.pack [0xc3] <> encodeUtf8 " x the")
        , ("long", encodeUtf8 (T.replicate 900 "漢" <> " the " <> T.replicate 2000 "y" <> "\nthe"))
        , ("empty", "")
        , ("binary", "the\0")
        ]
  _ <- try @IOException (removeDirectoryRecursive root)
  createDirectoryIfMissing True root
  mapM_ (\(name, bytes) -> BS.writeFile (root <> "/" <> name) bytes) files
  let Just needle = compileNeedle True "the"
      paths = map ((root <>) . ("/" <>) . fst) files <> [root <> "/missing"]
  found <- grepFiles needle paths
  removeDirectoryRecursive root
  let expected = [grepText needle (decodeUtf8Lenient bytes) | (name, bytes) <- files, name /= "binary"] <> [[], []]
  pure
    [ test "files match the search in memory (CRLF, invalid UTF-8, long lines); binary and missing files have none" $
        assertEqual (paths, expected) (map fst found, map snd found)
    , test "a long line is cut, and its column counts characters" $
        assertEqual [(0, 901, maxHitText), (1, 0, 3)] [(hitLine h, hitColumn h, T.length (hitText h)) | (p, hs) <- found, p == root <> "/long", h <- hs]
    ]
