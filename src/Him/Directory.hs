-- | Directory listings (ADR-22): a directory opens as a read-only document
-- whose lines are its entries, like Emacs's dired. The ordinary motions
-- and search work on it; the keys of the 'Him.Mode.Directory' layer open
-- entries ("Him.Commands.Directory").
--
-- Line 0 is the directory's path, line 1 is @../@, then subdirectories and
-- files, each sorted by name. Subdirectories end in @/@.
module Him.Directory
  ( loadPath
  , loadDirectory
  , listingDocument
  , listingDir
  , entryAt
  , selectEntry
  ) where

import Control.Exception (IOException, try)
import Data.List (findIndex, sortOn)
import Data.Text (Text)
import Data.Text qualified as T
import Him.Buffer qualified as Buffer
import Him.Document
import Him.File (loadDocument)
import Him.Position (Pos (..))
import Him.Selection (point, single)
import System.Directory (canonicalizePath, doesDirectoryExist, listDirectory)
import System.FilePath ((</>))

-- | Open a path: a directory as a listing, anything else as a file.
loadPath :: FilePath -> IO (Either Text Document)
loadPath path = do
  isDir <- doesDirectoryExist path
  if isDir then loadDirectory path else loadDocument path

-- | List a directory. The cursor starts on its first entry.
loadDirectory :: FilePath -> IO (Either Text Document)
loadDirectory path = do
  result <- try $ do
    dir <- canonicalizePath path
    names <- listDirectory dir
    entries <- traverse (\n -> DirEntry n <$> doesDirectoryExist (dir </> n)) names
    pure (listingDocument dir entries)
  pure $ case result of
    Left e -> Left (T.pack (show (e :: IOException)))
    Right doc -> Right doc

-- | The listing of a directory with these entries (in any order).
listingDocument :: FilePath -> [DirEntry] -> Document
listingDocument dir entries =
  (newDocument (Just dir) (Buffer.fromLines (header : map label shown)))
    { docKind = DirectoryDoc shown
    , docSelection = single (point (Pos (if length shown > 1 then 2 else 1) 0))
    }
  where
    shown = DirEntry ".." True : sortOn (\e -> (not (deIsDir e), deName e)) entries
    header = T.pack dir <> ":"
    -- A name with a line break would shift every line below it.
    label e = T.map (\c -> if c == '\n' || c == '\r' then '?' else c) (T.pack (deName e)) <> if deIsDir e then "/" else ""

-- | The directory a listing shows.
listingDir :: Document -> Maybe FilePath
listingDir d = case docKind d of
  DirectoryDoc _ -> docPath d
  TextDoc -> Nothing

-- | The entry on a line of a listing.
entryAt :: Int -> Document -> Maybe DirEntry
entryAt line d = case docKind d of
  DirectoryDoc es | line >= 1 -> case drop (line - 1) es of
    e : _ -> Just e
    [] -> Nothing
  _ -> Nothing

-- | Put the cursor on the entry with this name, if the listing has one.
selectEntry :: FilePath -> Document -> Document
selectEntry name d = case docKind d of
  DirectoryDoc es
    | Just i <- findIndex ((== name) . deName) es ->
        d {docSelection = single (point (Pos (i + 1) 0))}
  _ -> d
