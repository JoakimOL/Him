-- | @:@ commands for files and quitting.
module Him.Commands.File
  ( exCommands
  ) where

import Control.Monad.IO.Class (liftIO)
import Data.Text qualified as T
import Him.Buffer (lineCount)
import Him.Command
import Him.Document (Document (..))
import Him.Ex (ExCommand (..))
import Him.File (saveDocument)

exCommands :: [ExCommand]
exCommands =
  [ ExCommand ["write", "w"] "Write the file, optionally to a new path" $ \args ->
      () <$ write args
  , ExCommand ["quit", "q"] "Quit (refuses with unsaved changes)" $ \_ -> do
      dirty <- docDirty <$> getDoc
      if dirty
        then failWith "unsaved changes (use :q! to discard them, or :wq to save)"
        else quit
  , ExCommand ["quit!", "q!"] "Quit, discarding unsaved changes" $ \_ -> quit
  , ExCommand ["write-quit", "wq", "x"] "Write the file and quit" $ \args -> do
      ok <- write args
      if ok then quit else pure ()
  ]

-- | Save to the given path (and remember it), or to the document's path.
write :: [T.Text] -> EditorM Bool
write args = do
  doc <- getDoc
  case (args, docPath doc) of
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
          modifyDoc (\d -> d {docPath = Just path, docDirty = False})
          info $
            "\"" <> T.pack path <> "\" written, "
              <> T.pack (show (lineCount (docBuffer doc)))
              <> "L, "
              <> T.pack (show bytes)
              <> "B"
          pure True
