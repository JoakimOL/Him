-- | Pickers: @space f@ (files under the working directory) and @space b@
-- (open buffers), and the keys that drive an open picker.
module Him.Commands.Picker
  ( actions
  , pickerInsert
  , applyJobResult
  , pickerHousekeeping
  ) where

import Control.Monad.Trans.State.Strict (get, gets, modify')
import Data.Map.Strict qualified as Map
import Data.Sequence qualified as Seq
import Data.Text qualified as T
import Him.Action
import Him.Effect (Effect (..), Job (..), JobKey (..), JobResult (..))
import Him.Command
import Him.Commands.File (openFile)
import Him.Commands.Lsp qualified as Lsp
import Him.Lsp.Protocol (Encoding (..), fromLspColumn)
import Him.Buffer qualified as Buffer
import Him.Document (Document (..), displayName)
import Him.Position (Pos (..))
import Him.Selection (point, single)
import Him.Editor
import Him.Mode (Mode (..))
import Him.Picker
import Him.FileTree (WalkOptions (..))
import Him.Options (Options (..))

actions :: [Action]
actions =
  [ simple "file_picker" GBuffers "Open a file from the working directory" $ do
      -- The picker opens at once and fills as the scan streams in (ADR-24).
      gen <- gets edNextId
      modify' (\e -> e {edNextId = gen + 1})
      open (newPicker "files" []) {pkGeneration = gen, pkLoading = True}
      o <- gets edOptions
      request (StartJob (ScanFiles gen (WalkOptions (optPickerHidden o) (optPickerGitIgnore o) (optPickerIgnore o) (optPickerFollowSymlinks o) (optPickerMaxFiles o)) "."))
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
            PickPosition path line col encoding -> do
              openFile path
              modifyDoc $ \d ->
                let l = max 0 (min line (Buffer.lineCount (docBuffer d) - 1))
                    lineText = Buffer.lineAt l (docBuffer d)
                    -- A language server's column, converted on the line.
                    c = case encoding of
                      Just "utf-8" -> fromLspColumn Utf8 lineText col
                      Just "utf-16" -> fromLspColumn Utf16 lineText col
                      Just "utf-32" -> fromLspColumn Utf32 lineText col
                      _ -> max 0 (min col (Buffer.lineLength l (docBuffer d)))
                 in d {docSelection = single (point (Pos l c))}
            PickCodeAction act -> Lsp.runCodeAction act
        _ -> close
  , simple "picker_next" GPrompt "Select the next item" (onPicker (moveSelection 1))
  , simple "picker_previous" GPrompt "Select the previous item" (onPicker (moveSelection (-1)))
  , simple "picker_backspace" GPrompt "Delete the last character of the query" $
      changeQuery (T.dropEnd 1)
  ]
  where
    open p = modify' (\e -> e {edPicker = Just p, edMode = Picking})
    close = do
      modify' (\e -> e {edPicker = Nothing, edMode = Normal, edPreviews = Map.empty})
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
    Just p -> case pkSource p of
      StaticItems -> refresh p {pkQuery = f (pkQuery p), pkSelected = 0}
      -- The server filters: ask again, keep showing the last answer.
      ServerQuery server -> do
        let query = f (pkQuery p)
        modify' (\e -> e {edPicker = Just p {pkQuery = query, pkStale = True}})
        Lsp.queryWorkspaceSymbols server (pkGeneration p) query

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
            let new = [pickerItem (T.pack f) (PickFile f) "" | f <- files]
             in Just (refresh p {pkItems = pkItems p <> Seq.fromList new, pkLabelWidth = max (pkLabelWidth p) (labelWidth new)})
      ScanFinished gen
        | gen == pkGeneration p -> Just (modify' (\e -> e {edPicker = Just p {pkLoading = False}}))
      PreviewLoaded file loaded ->
        Just $ modify' $ \e ->
          e {edPreviews = capped (Map.insert file (either PreviewNone PreviewText loaded) (edPreviews e))}
      PickerFiltered gen query best total
        | gen == pkGeneration p && query == pkQuery p ->
            Just $ modify' $ \e ->
              e {edPicker = Just p {pkMatches = best, pkMatchCount = total, pkStale = False, pkSelected = min (pkSelected p) (max 0 (length best - 1))}}
      _ -> Nothing

-- | After every event: read the file the selected item points at, for the
-- preview, unless it is open or read already.
pickerHousekeeping :: EditorM ()
pickerHousekeeping = do
  ed <- get
  case edPicker ed >>= selectedItem of
    Just item
      | optPreview (edOptions ed)
      , Just file <- fileOf (piTarget item)
      , Just (_, Left _) <- previewFor ed (piTarget item)
      , Map.notMember file (edPreviews ed) -> do
          modify' (\e -> e {edPreviews = Map.insert file PreviewLoading (edPreviews e)})
          request (StartJob (LoadPreview (optPreviewMaxSize (edOptions ed)) file))
    _ -> pure ()
  where
    fileOf = \case
      PickFile file -> Just file
      PickPosition file _ _ _ -> Just file
      _ -> Nothing

-- | Keep the preview cache small (the most recent reads matter).
capped :: Map.Map FilePath Preview -> Map.Map FilePath Preview
capped m = if Map.size m > 64 then Map.fromList (take 32 (Map.toList m)) else m
