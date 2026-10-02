-- | Pickers: @space f@ (files under the working directory) and @space b@
-- (open buffers), and the keys that drive an open picker.
module Him.Commands.Picker
  ( actions
  , pickerInsert
  , applyJobResult
  ) where

import Control.Monad.Trans.State.Strict (gets, modify')
import Data.Sequence qualified as Seq
import Data.Text qualified as T
import Him.Action
import Him.Effect (Effect (..), Job (..), JobKey (..), JobResult (..))
import Him.Command
import Him.Commands.File (openFile)
import Him.Document (displayName)
import Him.Editor
import Him.Mode (Mode (..))
import Him.Picker

actions :: [Action]
actions =
  [ simple "file_picker" GBuffers "Open a file from the working directory" $ do
      -- The picker opens at once and fills as the scan streams in (ADR-24).
      gen <- gets edNextId
      modify' (\e -> e {edNextId = gen + 1})
      open (newPicker "files" []) {pkGeneration = gen, pkLoading = True}
      request (StartJob (ScanFiles gen "."))
  , simple "buffer_picker" GBuffers "Switch to an open buffer" $ do
      (bs, cur) <- gets buffers
      let label i b = T.pack (show (i + 1)) <> (if i == cur then " * " else "   ") <> displayName (bufDoc b)
      open ((newPicker "buffers" [pickerItem (label i b) (PickBuffer i) "" | (i, b) <- zip [0 ..] bs]) {pkSelected = cur})
  , simple "command_palette" GPrompt "List every action with its keys, and run one" (request OpenPalette)
  , simple "picker_close" GPrompt "Close the picker" close
  , simple "picker_accept" GPrompt "Open the selected item" $
      gets (fmap selectedItem . edPicker) >>= \case
        Just (Just item) -> do
          close
          case piTarget item of
            PickFile path -> openFile path
            PickBuffer i -> modify' (gotoBuffer i)
            PickAction name needsArgs
              | needsArgs -> do
                  modify' (\e -> e {edPrompt = ExPrompt, edCmdLine = "action " <> name <> " ", edCompletions = []})
                  setMode CmdLine
              | otherwise -> request (RunAction (Invocation name []))
        _ -> close
  , simple "picker_next" GPrompt "Select the next item" (onPicker (moveSelection 1))
  , simple "picker_previous" GPrompt "Select the previous item" (onPicker (moveSelection (-1)))
  , simple "picker_backspace" GPrompt "Delete the last character of the query" $
      changeQuery (T.dropEnd 1)
  ]
  where
    open p = modify' (\e -> e {edPicker = Just p, edMode = Picking})
    close = do
      modify' (\e -> e {edPicker = Nothing, edMode = Normal})
      request (CancelJob ScanJob)
      request (CancelJob FilterJob)

-- | Typing narrows the picker.
pickerInsert :: Char -> EditorM ()
pickerInsert c = changeQuery (`T.snoc` c)

onPicker :: (Picker -> Picker) -> EditorM ()
onPicker f = modify' (\e -> e {edPicker = f <$> edPicker e})

-- | Change the query. A large picker keeps showing its last matches
-- (marked stale) while a background job ranks the items (ADR-24).
changeQuery :: (T.Text -> T.Text) -> EditorM ()
changeQuery f =
  gets edPicker >>= \case
    Nothing -> pure ()
    Just p -> refresh p {pkQuery = f (pkQuery p), pkSelected = 0}

-- | Bring a picker's matches up to date with its query and items: at once
-- when it is small or the query is empty, otherwise in a job.
refresh :: Picker -> EditorM ()
refresh p
  | T.null (pkQuery p) || Seq.length (pkItems p) < syncLimit = setPicker (setQuery (pkQuery p) p) {pkSelected = pkSelected p}
  | otherwise = do
      setPicker p {pkStale = True}
      request (StartJob (FilterPicker (pkGeneration p) (pkQuery p) (pkItems p)))
  where
    setPicker p' = modify' (\e -> e {edPicker = Just p'})

-- | A background job reported back; results for another picker, or for an
-- older query, are dropped.
applyJobResult :: JobResult -> EditorM ()
applyJobResult result =
  gets edPicker >>= \case
    Just p | Just p' <- apply p -> p'
    _ -> pure ()
  where
    apply p = case result of
      FilesFound gen files
        | gen == pkGeneration p ->
            Just (refresh p {pkItems = pkItems p <> Seq.fromList [pickerItem (T.pack f) (PickFile f) "" | f <- files]})
      ScanFinished gen
        | gen == pkGeneration p -> Just (modify' (\e -> e {edPicker = Just p {pkLoading = False}}))
      PickerFiltered gen query best total
        | gen == pkGeneration p && query == pkQuery p ->
            Just $ modify' $ \e ->
              e {edPicker = Just p {pkMatches = best, pkMatchCount = total, pkStale = False, pkSelected = min (pkSelected p) (max 0 (length best - 1))}}
      _ -> Nothing
