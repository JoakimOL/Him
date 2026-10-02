-- | @:@ commands for files, buffers and quitting.
module Him.Commands.File
  ( exCommands
  , actions
  , openFile
  ) where

import Control.Exception (IOException, try)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.State.Strict (gets, modify')
import Data.Text qualified as T
import Him.Action
import Him.Effect (Effect (..))
import Him.Buffer (lineCount)
import Him.Buffer qualified as Buffer
import Him.Command
import Him.Document (Document (..), changeDocument, clampSelection, displayName, isReadOnly, newDocument)
import Him.Options (Options (..))
import Him.Editor
import Him.Ex (ExArgs (..), ExCommand (..))
import Him.Commands.Git (markGitReload)
import Him.Directory (listingDir, loadPath)
import Him.Lsp.Sync (closeEffects)
import Him.File (loadDocument, saveDocument)
import System.Directory (canonicalizePath, doesFileExist, getCurrentDirectory, setCurrentDirectory)

actions :: [Action]
actions =
  [ simple "buffer_next" GBuffers "Go to the next buffer" (modify' (switchBuffer 1))
  , simple "buffer_previous" GBuffers "Go to the previous buffer" (modify' (switchBuffer (-1)))
  ]

exCommands :: [ExCommand]
exCommands =
  [ ExCommand ["write", "w"] "Write the file, optionally to a new path" PathArgs $ \args ->
      () <$ write args
  , ExCommand ["quit", "q"] "Quit (refuses with unsaved changes in any buffer)" NoArgs $ \_ -> quitChecked
  , ExCommand ["quit!", "q!"] "Quit, discarding unsaved changes" NoArgs $ \_ -> quit
  , ExCommand ["quit-all", "qa"] "Quit (refuses with unsaved changes in any buffer)" NoArgs $ \_ -> quitChecked
  , ExCommand ["quit-all!", "qa!"] "Quit, discarding unsaved changes" NoArgs $ \_ -> quit
  , ExCommand ["write-quit", "wq", "x"] "Write the file and quit" PathArgs $ \args -> do
      ok <- write args
      if ok then quitChecked else pure ()
  , ExCommand ["write-all", "wa"] "Write every modified buffer" NoArgs $ \_ -> () <$ writeAll
  , ExCommand ["write-quit-all", "wqa", "xa"] "Write every modified buffer and quit" NoArgs $ \_ -> do
      ok <- writeAll
      if ok then quit else pure ()
  , ExCommand ["open", "o", "edit", "e"] "Open files (switches to one already open)" PathArgs $ \case
      [] -> failWith ":open needs a path"
      paths -> mapM_ (openFile . T.unpack) paths
  , ExCommand ["new", "n"] "Open a new scratch buffer" NoArgs $ \_ ->
      modify' (openBuffer (newDocument Nothing Buffer.empty))
  , ExCommand ["buffer-close", "bc", "bclose"] "Close the buffer (refuses with unsaved changes)" NoArgs $ \_ -> do
      dirty <- docDirty <$> getDoc
      if dirty
        then failWith "unsaved changes (use :bc! to discard them)"
        else closeCurrent
  , ExCommand ["buffer-close!", "bc!", "bclose!"] "Close the buffer, discarding unsaved changes" NoArgs $ \_ ->
      closeCurrent
  , ExCommand ["change-current-directory", "cd"] "Change the working directory (default: the listed one)" PathArgs $ \args -> do
      listed <- listingDir <$> getDoc
      case (args, listed) of
        ([dir], _) -> changeDirectory (T.unpack dir)
        ([], Just dir) -> changeDirectory dir
        ([], Nothing) -> failWith ":cd needs a directory"
        _ -> failWith ":cd takes one directory"
  , ExCommand ["action"] "Run an action by name, with arguments (e.g. :action goto_line 12)" NoArgs $ \args ->
      case parseInvocation (T.unwords args) of
        Left e -> failWith e
        Right inv -> request (RunAction inv)
  , ExCommand ["show-directory", "pwd"] "Show the working directory" NoArgs $ \_ ->
      liftIO getCurrentDirectory >>= info . T.pack
  , ExCommand ["reload", "rl"] "Load the file again from disk (refuses with unsaved changes)" NoArgs $ \_ -> reloadCurrent False
  , ExCommand ["reload!", "rl!"] "Load the file again from disk, discarding unsaved changes" NoArgs $ \_ -> reloadCurrent True
  , ExCommand ["reload-all", "rla"] "Load every unmodified buffer again from disk" NoArgs $ \_ -> reloadAll
  , ExCommand ["config-open"] "Open the config file (with the defaults if it does not exist yet)" NoArgs $ \_ -> request OpenConfig
  , ExCommand ["config-reload"] "Read the config file again and apply it" NoArgs $ \_ -> request ReloadConfig
  , ExCommand ["theme"] "Use a theme (Helix's, or one in themes/ next to the config file); without a name, show the current one" ThemeArgs $ \case
      [] -> request (ChangeTheme Nothing)
      [name] -> request (ChangeTheme (Just name))
      _ -> failWith "usage: :theme [name]"
  , ExCommand ["buffer-next", "bn", "bnext"] "Go to the next buffer" NoArgs $ \_ -> modify' (switchBuffer 1)
  , ExCommand ["buffer-previous", "bp", "bprev"] "Go to the previous buffer" NoArgs $ \_ -> modify' (switchBuffer (-1))
  ]

-- | Read a document's file again: 'Left' with why not.
reloadDocument :: Bool -> Document -> EditorM (Either T.Text (Maybe Document))
reloadDocument force d = case (isReadOnly d, docPath d) of
  (True, _) -> pure (Left "a directory listing is refreshed with g r")
  (_, Nothing) -> pure (Left "no file to reload")
  (_, Just path)
    | docDirty d && not force -> pure (Left "unsaved changes (use :reload! to discard them)")
    | otherwise -> do
        exists <- liftIO (doesFileExist path)
        if not exists
          then pure (Left (T.pack path <> " no longer exists"))
          else
            liftIO (loadDocument path) >>= \case
              Left e -> pure (Left e)
              Right fresh
                | docBuffer fresh == docBuffer d && not (docDirty d) -> pure (Right Nothing)
                | otherwise -> pure (Right (Just (reloaded fresh d)))

-- | The document with its file's new text: one undoable change; the
-- cursors stay where they were (clamped), and the git state is read again.
-- Its identity stays, so highlighting and the language server follow
-- through the version bump.
reloaded :: Document -> Document -> Document
reloaded fresh d =
  let buf = docBuffer fresh
   in markGitReload
        (changeDocument buf (clampSelection buf (docSelection d)) d)
          { docDirty = False
          , docSavedBuffer = buf
          , docLineEnding = docLineEnding fresh
          , docTrailingNewline = docTrailingNewline fresh
          }

reloadCurrent :: Bool -> EditorM ()
reloadCurrent force =
  getDoc >>= reloadDocument force >>= \case
    Left e -> failWith e
    Right Nothing -> info "already up to date"
    Right (Just d) -> do
      modifyDoc (const d)
      info ("reloaded " <> displayName d)

-- | Reload every buffer with a file and no unsaved changes.
reloadAll :: EditorM ()
reloadAll = do
  (bs, _) <- gets buffers
  results <- mapM (\b -> (docId (bufDoc b),) <$> reloadDocument False (bufDoc b)) [b | b <- bs, not (isReadOnly (bufDoc b)), docPath (bufDoc b) /= Nothing]
  mapM_ (\(i, r) -> either (const (pure ())) (mapM_ (\d -> modify' (modifyDocument i (const d)))) r) results
  let changed = length [() | (_, Right (Just _)) <- results]
      skipped = length [() | (_, Left _) <- results]
  info $
    "reloaded " <> T.pack (show changed) <> " buffer(s)"
      <> if skipped > 0 then ", skipped " <> T.pack (show skipped) <> " (unsaved changes or missing files)" else ""

-- | Close the current buffer, telling its language server.
closeCurrent :: EditorM ()
closeCurrent = do
  getDoc >>= mapM_ request . closeEffects
  modify' closeBuffer

-- | Quit unless a buffer has unsaved changes.
quitChecked :: EditorM ()
quitChecked = do
  dirty <- gets (filter docDirty . map bufDoc . fst . buffers)
  case dirty of
    [] -> quit
    [d] -> failWith ("unsaved changes in " <> displayName d <> " (use :q! to discard them, or :wq to save)")
    ds -> failWith (T.pack (show (length ds)) <> " buffers have unsaved changes (use :q! to discard them, or :wa to save)")

changeDirectory :: FilePath -> EditorM ()
changeDirectory dir =
  liftIO (try (setCurrentDirectory dir >> getCurrentDirectory)) >>= \case
    Left e -> failWith ("could not change directory: " <> T.pack (show (e :: IOException)))
    Right now -> info ("working directory: " <> T.pack now)

-- | Show a file or directory: switch to its buffer if it is open, otherwise
-- load it (a missing file opens as an empty buffer that :w creates; a
-- directory opens as a listing).
openFile :: FilePath -> EditorM ()
openFile path = do
  want <- liftIO (canonical path)
  (bs, _) <- gets buffers
  open <- liftIO (traverse (traverse canonical . docPath . bufDoc) bs)
  case lookup (Just want) (zip open [0 ..]) of
    Just i -> modify' (gotoBuffer i)
    Nothing -> do
      showHidden <- gets (optShowHidden . edOptions)
      liftIO (loadPath showHidden path) >>= \case
        Left e -> failWith ("could not open " <> T.pack path <> ": " <> e)
        Right doc -> modify' (openBuffer doc)
  where
    canonical p = either (const p) id <$> (try (canonicalizePath p) :: IO (Either IOException FilePath))

-- | Write every modified buffer that has a path. False if any could not be
-- written.
writeAll :: EditorM Bool
writeAll = do
  (bs, cur) <- gets buffers
  results <- traverse (\(i, b) -> saveBuffer i (bufDoc b)) (zip [0 ..] bs)
  modify' (gotoBuffer cur)
  let failed = length (filter (== Just False) results)
      written = length (filter (== Just True) results)
  if failed == 0
    then True <$ info ("wrote " <> T.pack (show written) <> " buffer(s)")
    else False <$ failWith (T.pack (show failed) <> " buffer(s) could not be written")
  where
    saveBuffer i doc
      | not (docDirty doc) = pure Nothing
      | otherwise = do
          modify' (gotoBuffer i)
          Just <$> write []

-- | Save to the given path (and remember it), or to the document's path.
write :: [T.Text] -> EditorM Bool
write args = do
  doc <- getDoc
  case (args, docPath doc) of
    _ | isReadOnly doc -> False <$ failWith "a directory listing cannot be written"
    ([path], _) -> saveTo (T.unpack path)
    ([], Just path) -> saveTo path
    ([], Nothing) -> False <$ failWith "no file name (use :w <path>)"
    _ -> False <$ failWith ":write takes at most one path"
  where
    saveTo path = do
      doc <- getDoc
      liftIO (saveDocument path doc) >>= \case
        Left e -> False <$ failWith ("could not write " <> T.pack path <> ": " <> e)
        Right bytes -> do
          modifyDoc (\d -> markGitReload d {docPath = Just path, docDirty = False, docSavedBuffer = docBuffer doc, docSaves = docSaves d + 1})
          info $
            "\"" <> T.pack path <> "\" written, "
              <> T.pack (show (lineCount (docBuffer doc)))
              <> "L, "
              <> T.pack (show bytes)
              <> "B"
          pure True
