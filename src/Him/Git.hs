-- | Talking to git (ADR-25), through the @git@ program. These run in
-- background jobs ("Him.Runtime").
module Him.Git
  ( loadBase
  , writeIndex
  , removeFromIndex
  , splitBlob
  ) where

import Control.Exception (IOException, try)
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding (decodeUtf8Lenient)
import Him.Buffer qualified as Buffer
import Him.Document (Document (..), LineEnding (..), newDocument)
import Him.File (decodeDocument, encodeDocument)
import Him.GitState (GitBase (..))
import Him.Process (ProcessResult (..), runProcess)
import System.Directory (canonicalizePath)
import System.Exit (ExitCode (..))
import System.FilePath (makeRelative, takeDirectory)

-- | Run git in a directory; the output when it succeeded.
git :: FilePath -> [String] -> ByteString -> IO (Either Text ByteString)
git dir args input =
  runProcess "git" args (Just dir) input >>= \case
    Left e -> pure (Left e)
    Right r
      | prExit r == ExitSuccess -> pure (Right (prStdout r))
      | otherwise -> pure (Left (T.strip (decodeUtf8Lenient (prStderr r))))

-- | The file's versions in the index and in HEAD. 'Nothing' when it is not
-- inside a work tree (or git is missing). An untracked file has an empty
-- index version and no mode.
loadBase :: FilePath -> IO (Maybe GitBase)
loadBase path = do
  canon <- either (const path) id <$> try @IOException (canonicalizePath path)
  git (takeDirectory canon) ["rev-parse", "--show-toplevel"] "" >>= \case
    Left _ -> pure Nothing
    Right out -> do
      let root = T.unpack (T.strip (decodeUtf8Lenient out))
          rel = makeRelative root canon
      staged <- git root ["ls-files", "--stage", "--", rel] ""
      let mode = case staged of
            Right o | (m : _) <- T.words (decodeUtf8Lenient o) -> m
            _ -> ""
      (index, newline) <-
        if T.null mode
          then pure ([], True)
          else either (const ([], True)) splitBlob <$> git root ["show", ":" <> rel] ""
      headVersion <- git root ["show", "HEAD:" <> rel] ""
      pure . Just $
        GitBase
          { gbRoot = root
          , gbPath = rel
          , gbMode = mode
          , gbIndex = index
          , gbIndexNewline = newline
          , gbHead = either (const []) (fst . splitBlob) headVersion
          , gbInHead = either (const False) (const True) headVersion
          }

-- | A blob as lines, split the way documents are ("Him.File"), and whether
-- it ends with a line break.
splitBlob :: ByteString -> ([Text], Bool)
splitBlob bytes =
  let d = decodeDocument Nothing bytes
   in (Buffer.toLines (docBuffer d), BS.null bytes || docTrailingNewline d)

-- | Make these lines the file's index version (staging), written with the
-- given line ending.
writeIndex :: GitBase -> LineEnding -> [Text] -> IO (Either Text ())
writeIndex base ending ls = do
  let doc = (newDocument Nothing (Buffer.fromLines ls)) {docLineEnding = ending, docTrailingNewline = gbIndexNewline base}
      blob = if null ls || ls == [""] then "" else encodeDocument doc
      root = gbRoot base
      rel = gbPath base
  git root ["hash-object", "-w", "--stdin", "--path=" <> rel] blob >>= \case
    Left e -> pure (Left e)
    Right out -> do
      let sha = T.unpack (T.strip (decodeUtf8Lenient out))
          mode = if T.null (gbMode base) then "100644" else T.unpack (gbMode base)
          add = ["--add" | T.null (gbMode base)]
      fmap (const ()) <$> git root (["update-index"] <> add <> ["--cacheinfo", mode <> "," <> sha <> "," <> rel]) ""

-- | Take the file out of the index (unstaging a file that is not in HEAD).
removeFromIndex :: GitBase -> IO (Either Text ())
removeFromIndex base = fmap (const ()) <$> git (gbRoot base) ["update-index", "--force-remove", "--", gbPath base] ""
