-- | Pickers: @space f@ (files under the working directory) and @space b@
-- (open buffers), and the keys that drive an open picker.
module Him.Commands.Picker
  ( actions
  , pickerInsert
  ) where

import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.State.Strict (gets, modify')
import Data.Text qualified as T
import Him.Action
import Him.Command
import Him.Commands.File (openFile)
import Him.Document (displayName)
import Him.Editor
import Him.FileTree (listFiles)
import Him.Mode (Mode (..))
import Him.Picker

actions :: [Action]
actions =
  [ simple "file_picker" GBuffers "Open a file from the working directory" $ do
      files <- liftIO (listFiles maxFiles ".")
      open (newPicker "files" [PickerItem (T.pack f) (PickFile f) | f <- files])
  , simple "buffer_picker" GBuffers "Switch to an open buffer" $ do
      (bs, cur) <- gets buffers
      let label i b = T.pack (show (i + 1)) <> (if i == cur then " * " else "   ") <> displayName (bufDoc b)
      open ((newPicker "buffers" [PickerItem (label i b) (PickBuffer i) | (i, b) <- zip [0 ..] bs]) {pkSelected = cur})
  , simple "picker_close" GPrompt "Close the picker" close
  , simple "picker_accept" GPrompt "Open the selected item" $
      gets (fmap selectedItem . edPicker) >>= \case
        Just (Just item) -> do
          close
          case piTarget item of
            PickFile path -> openFile path
            PickBuffer i -> modify' (gotoBuffer i)
        _ -> close
  , simple "picker_next" GPrompt "Select the next item" (onPicker (moveSelection 1))
  , simple "picker_previous" GPrompt "Select the previous item" (onPicker (moveSelection (-1)))
  , simple "picker_backspace" GPrompt "Delete the last character of the query" $
      onPicker (\p -> setQuery (T.dropEnd 1 (pkQuery p)) p)
  ]
  where
    open p = modify' (\e -> e {edPicker = Just p, edMode = Picking})
    close = modify' (\e -> e {edPicker = Nothing, edMode = Normal})

-- | Typing narrows the picker.
pickerInsert :: Char -> EditorM ()
pickerInsert c = onPicker (\p -> setQuery (T.snoc (pkQuery p) c) p)

onPicker :: (Picker -> Picker) -> EditorM ()
onPicker f = modify' (\e -> e {edPicker = f <$> edPicker e})

-- | The picker lists at most this many files.
maxFiles :: Int
maxFiles = 50000
