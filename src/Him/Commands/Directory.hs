-- | Actions for directory listings ("Him.Directory"): open the entry under
-- the cursor, go up, refresh, and open a listing from anywhere.
module Him.Commands.Directory
  ( actions
  ) where

import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.State.Strict (modify')
import Data.Text qualified as T
import Him.Action
import Him.Command
import Him.Commands.File (openFile)
import Him.Directory
import Him.Document (DirEntry (..), Document (..))
import Him.Editor (Editor (..))
import Him.Position (Pos (..))
import Him.Selection (primary, rangeHead)
import Him.View (initialView)
import System.Directory (getCurrentDirectory)
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
  ]

-- | Show the parent, with the cursor on the directory we came from.
goUp :: FilePath -> EditorM ()
goUp dir = showListing (takeDirectory dir) (Just (takeFileName dir))

-- | Replace the current listing with another directory's (dired-style
-- navigation stays in one buffer).
showListing :: FilePath -> Maybe FilePath -> EditorM ()
showListing dir focus =
  liftIO (loadDirectory dir) >>= \case
    Left e -> failWith ("could not list " <> T.pack dir <> ": " <> e)
    Right doc -> modify' (\e -> e {edDoc = maybe id selectEntry focus doc, edView = initialView})

-- | Open a listing as a buffer (or switch to an open one), with the cursor
-- on an entry.
openListing :: FilePath -> Maybe FilePath -> EditorM ()
openListing dir focus = do
  openFile dir
  mapM_ (\name -> modifyDoc (selectEntry name)) focus
