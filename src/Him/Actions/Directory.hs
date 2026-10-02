-- | Actions for directory listings ("Him.Directory"): open the entry under
-- the cursor, go up, refresh, open a listing from anywhere, and create,
-- rename and delete files (the names are asked for on the command line,
-- see 'runFileAction').
module Him.Actions.Directory
  ( actions
  , runFileAction
  ) where

import Control.Exception (IOException, try)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.State.Strict (gets, modify')
import Data.ByteString qualified as BS
import Data.Containers.ListUtils (nubOrd)
import Data.List (isPrefixOf)
import Data.Text qualified as T
import Him.Action
import Him.EditorM
import Him.Actions.File (openFile)
import Him.Directory
import Him.Document (DirEntry (..), Document (..))
import Him.Options (Options (..))
import Him.Editor
import Him.Mode (Mode (..))
import Him.Position (Pos (..))
import Him.Selection (primary, rangeEnd, rangeHead, rangeStart, ranges)
import Him.View (initialView)
import System.Directory
  ( createDirectoryIfMissing
  , doesDirectoryExist
  , doesPathExist
  , getCurrentDirectory
  , pathIsSymbolicLink
  , removeDirectoryRecursive
  , removeFile
  , renamePath
  )
import System.FilePath (takeDirectory, takeFileName, (</>))

actions :: [Action]
actions =
  [ simple "directory_open" GBuffers "Open the file or enter the directory under the cursor" $ do
      d <- getDoc
      case (listingDir d, entryAt (posLine (rangeHead (primary (docSelection d)))) d) of
        (Just dir, Just e)
          | deName e == ".." -> goUp dir
          | deIsDir e -> showListing (dir </> deName e) Nothing
          | otherwise -> openFile (dir </> deName e)
        (Just _, Nothing) -> failWith "no entry on this line"
        (Nothing, _) -> failWith "not a directory listing"
  , simple "directory_parent" GBuffers "Go to the parent directory" $
      listingDir <$> getDoc >>= \case
        Just dir -> goUp dir
        Nothing -> failWith "not a directory listing"
  , simple "directory_refresh" GBuffers "List the directory again" $ do
      d <- getDoc
      case listingDir d of
        Just dir -> showListing dir (deName <$> entryAt (posLine (rangeHead (primary (docSelection d)))) d)
        Nothing -> failWith "not a directory listing"
  , simple "directory_of_buffer" GBuffers "Open the directory of the current file" $ do
      d <- getDoc
      cwd <- liftIO getCurrentDirectory
      case (listingDir d, docPath d) of
        (Just dir, _) -> openListing (takeDirectory dir) (Just (takeFileName dir))
        (Nothing, Just path) -> openListing (takeDirectory (cwd </> path)) (Just (takeFileName path))
        (Nothing, Nothing) -> openListing cwd Nothing
  , simple "directory_of_cwd" GBuffers "Open the working directory" $
      liftIO getCurrentDirectory >>= \cwd -> openListing cwd Nothing
  , simple "directory_toggle_hidden" GBuffers "Show or hide dotfiles in listings" $ do
      modify' (\e -> e {edOptions = (edOptions e) {optShowHidden = not (optShowHidden (edOptions e))}})
      shown <- gets (optShowHidden . edOptions)
      d <- getDoc
      mapM_ (\dir -> showListing dir (deName <$> entryAt (cursorLine d) d)) (listingDir d)
      info (if shown then "showing dotfiles" else "hiding dotfiles")
  , simple "directory_new_file" GBuffers "Create a file (a name ending in / makes a directory)" $
      withListing $ \dir _ -> prompt (NewFile dir) ""
  , simple "directory_new_directory" GBuffers "Create a directory" $
      withListing $ \dir _ -> prompt (NewDirectory dir) ""
  , simple "directory_rename" GBuffers "Rename or move the entry under the cursor" $
      withListing $ \dir d -> case entryAt (cursorLine d) d of
        Just e | deName e /= ".." -> prompt (RenameEntry dir (deName e)) (T.pack (deName e))
        _ -> failWith "no entry to rename on this line"
  , simple "directory_delete" GBuffers "Delete the selected entries (asks first)" $
      withListing $ \dir d ->
        let names = nubOrd [deName e | r <- ranges (docSelection d), e <- entriesIn (posLine (rangeStart r)) (posLine (rangeEnd r)) d]
         in if null names then failWith "no entries selected" else prompt (DeleteEntries dir names) ""
  ]
  where
    withListing k = do
      d <- getDoc
      case listingDir d of
        Just dir -> k dir d
        Nothing -> failWith "not a directory listing"
    prompt act initial = do
      modify' (\e -> e {edPrompt = FilePrompt act, edCmdLine = initial, edPreviewPending = False})
      setMode CmdLine

cursorLine :: Document -> Int
cursorLine d = posLine (rangeHead (primary (docSelection d)))

-- | Run a file operation with what was typed, then list the directory again
-- with the cursor on the result.
runFileAction :: FileAction -> T.Text -> EditorM ()
runFileAction act typed = case act of
  NewFile dir
    | T.null name -> pure ()
    | "/" `T.isSuffixOf` name -> makeDirectory dir
    | otherwise -> do
        let path = dir </> T.unpack name
        exists <- liftIO (doesPathExist path)
        if exists
          then failWith (name <> " already exists")
          else attempt (createDirectoryIfMissing True (takeDirectory path) >> BS.writeFile path "") $
            done dir ("created " <> name) (firstComponent name)
  NewDirectory dir
    | T.null name -> pure ()
    | otherwise -> makeDirectory dir
  RenameEntry dir old
    | T.null name || name == T.pack old -> pure ()
    | otherwise -> do
        let from = dir </> old
            to = dir </> T.unpack name
        exists <- liftIO (doesPathExist to)
        if exists
          then failWith (name <> " already exists")
          else attempt (createDirectoryIfMissing True (takeDirectory to) >> renamePath from to) $ do
            modify' (renameBuffers from to)
            done dir ("renamed to " <> name) (firstComponent name)
  DeleteEntries dir names
    | T.toLower name `elem` ["y", "yes"] -> do
        line <- cursorLine <$> getDoc
        attempt (mapM_ (remove . (dir </>)) names) $ do
          showListing dir Nothing
          modifyDoc (selectLine line)
          info ("deleted " <> T.pack (show (length names)) <> " entr" <> (if length names == 1 then "y" else "ies"))
    | otherwise -> info "nothing deleted"
  where
    name = T.strip typed
    makeDirectory dir =
      attempt (createDirectoryIfMissing True (dir </> T.unpack name)) $
        done dir ("created " <> name) (firstComponent name)
    firstComponent = T.unpack . T.takeWhile (/= '/')
    done dir msg focus = showListing dir (Just focus) >> info msg
    remove path = do
      isDir <- doesDirectoryExist path
      isLink <- pathIsSymbolicLink path
      -- A link to a directory is removed, not followed.
      if isDir && not isLink then removeDirectoryRecursive path else removeFile path
    attempt io next =
      liftIO (try io) >>= \case
        Left e -> failWith (T.pack (show (e :: IOException)))
        Right () -> next

-- | Buffers showing a renamed file, or something inside a renamed
-- directory, follow it.
renameBuffers :: FilePath -> FilePath -> Editor -> Editor
renameBuffers from to ed = setBuffers (map move bs) i ed
  where
    (bs, i) = buffers ed
    move b = b {bufDoc = (bufDoc b) {docPath = moved <$> docPath (bufDoc b)}}
    moved p
      | p == from = to
      | (from <> "/") `isPrefixOf` p = to <> drop (length from) p
      | otherwise = p

-- | Show the parent, with the cursor on the directory we came from.
goUp :: FilePath -> EditorM ()
goUp dir = showListing (takeDirectory dir) (Just (takeFileName dir))

-- | Replace the current listing with another directory's (dired-style
-- navigation stays in one buffer).
showListing :: FilePath -> Maybe FilePath -> EditorM ()
showListing dir focus = do
  showHidden <- gets (optShowHidden . edOptions)
  liftIO (loadDirectory showHidden dir) >>= \case
    Left e -> failWith ("could not list " <> T.pack dir <> ": " <> e)
    Right doc -> do
      replaceText (maybe id selectEntry focus doc)
      modify' (\e -> e {edView = initialView})

-- | Open a listing as a buffer (or switch to an open one), with the cursor
-- on an entry.
openListing :: FilePath -> Maybe FilePath -> EditorM ()
openListing dir focus = do
  openFile dir
  mapM_ (\name -> modifyDoc (selectEntry name)) focus
