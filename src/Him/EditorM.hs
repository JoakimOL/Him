-- | The monad actions run in, and helpers for writing actions. Keys are
-- bound to actions (see "Him.Action" and "Him.Keymap").
module Him.EditorM
  ( EditorM
    -- * Helpers for writing actions
  , getDoc
  , modifyDoc
  , setMode
  , info
  , failWith
  , edit
  , editEach
  , motion
  , quit
  , getRegister
  , setRegister
  , request
  , keyHint
  , keyHints
  , openPicker
  , replaceText
  , transcriptKept
  , transcriptMessage
  ) where

import Control.Monad.Trans.State.Strict (StateT, gets, modify')
import Data.Map.Strict qualified as Map
import Data.Text (Text)
import Him.Document (DocKind (..), Document (..), changeDocument, inputPos, isReadOnly)
import Him.Buffer qualified as Buffer
import Control.Monad (when)
import Him.Edit (Edit, applyEdits)
import Him.Effect (Effect)
import Him.Editor
import Him.Mode (Mode (..))
import Him.KeyHints (HintScope (..), hintLine, keyOr)
import Him.Motion (Motion, Movement (..), applyMotion)
import Him.Picker (Picker)
import Him.Selection (Range (..), mapRanges, normalize, point)

-- | Actions run with access to the editor state and IO (for files etc.).
-- Keep the actual logic in pure modules ("Him.Motion", "Him.Edit") where
-- possible, and use these actions as thin wrappers.
type EditorM = StateT Editor IO

getDoc :: EditorM Document
getDoc = gets edDoc

modifyDoc :: (Document -> Document) -> EditorM ()
modifyDoc f = modify' (\e -> e {edDoc = f (edDoc e)})

-- | Switch modes. Insert mode is refused in a read-only document.
setMode :: Mode -> EditorM ()
setMode m = do
  readOnly <- isReadOnly <$> getDoc
  if m == Insert && readOnly
    then readOnlyMessage >>= failWith
    else do
      modify' (\e -> e {edMode = m})
      -- In a REPL or chat buffer, typing goes to the input: a cursor up in
      -- the transcript moves to its end (ADR transcripts).
      when (m == Insert) $ modifyDoc $ \d -> case inputPos d of
        Just p ->
          let end = Buffer.endPos (docBuffer d)
           in d {docSelection = mapRanges (\r -> if rangeHead r < p then point end else r) (docSelection d)}
        Nothing -> d

-- | Why a read-only buffer refuses an edit; a listing says how to use it.
readOnlyMessage :: EditorM Text
readOnlyMessage =
  getDoc >>= \d -> case docKind d of
    DirectoryDoc _ -> do
      open <- keyHint Directory "directory_open"
      up <- keyHint Directory "directory_parent"
      pure ("a directory listing is read-only (" <> open <> " opens an entry, " <> up <> " goes up)")
    _ -> pure "this buffer is read-only"

info :: Text -> EditorM ()
info t = modify' (\e -> e {edStatus = Just (Status Info t)})

failWith :: Text -> EditorM ()
failWith t = modify' (\e -> e {edStatus = Just (Status Error t)})

-- | A register's values (one per range it was yanked from); empty when unset.
getRegister :: Char -> EditorM [Text]
getRegister c = gets (Map.findWithDefault [] c . edRegisters)

setRegister :: Char -> [Text] -> EditorM ()
setRegister c vs = modify' (\e -> e {edRegisters = Map.insert c vs (edRegisters e)})

-- | Queue an effect for the main loop (see "Him.Effect").
-- | Show a picker; its keys take over until it closes (ADR picker-actions: what
-- choosing does is the picker's 'Him.Picker.pkPrimary' and 'Him.Picker.pkSecondary').
openPicker :: Picker -> EditorM ()
openPicker p = modify' (\e -> e {edPicker = Just p, edMode = Picking})

request :: Effect -> EditorM ()
request eff = modify' (\e -> e {edEffects = edEffects e <> [eff]})

-- | Replace the current document's text and selection wholesale (undo,
-- redo, a new directory listing), keeping its identity and bumping its
-- version.
replaceText :: Document -> EditorM ()
replaceText new = modifyDoc (\d -> new {docId = docId d, docVersion = docVersion d + 1})

quit :: EditorM ()
quit = modify' (\e -> e {edQuit = True})

-- | Apply a pure edit to every range and mark the document modified.
-- The state before the edit is recorded for undo (see "Him.History"; the
-- main loop commits it once the editor is out of insert mode).
edit :: Edit -> EditorM ()
edit f = editEach (const f)

-- | Like 'edit', but the edit is told the index of the range it works on.
editEach :: (Int -> Edit) -> EditorM ()
editEach f = do
  readOnly <- isReadOnly <$> getDoc
  if readOnly then readOnlyMessage >>= failWith else editAll f

editAll :: (Int -> Edit) -> EditorM ()
editAll f = do
  d <- getDoc
  let (buf, sel) = applyEdits f (docBuffer d) (docSelection d)
  if transcriptKept d buf
    then modifyDoc (const (changeDocument buf sel d) {docDirty = True})
    else failWith transcriptMessage

-- | In a REPL or chat buffer only the input after the prompt may change;
-- the transcript before it is read-only (ADR transcripts). The first difference
-- between the texts ('Buffer.changeBetween', cheap) must not be before
-- the input.
transcriptKept :: Document -> Buffer.Buffer -> Bool
transcriptKept d buf = case inputPos d of
  Nothing -> True
  Just p -> case Buffer.changeBetween (docBuffer d) buf of
    Nothing -> True
    Just (start, _, _) -> start >= p

transcriptMessage :: Text
transcriptMessage = "only the input after the prompt can be changed (select and y copy from anywhere)"

-- | Apply a motion to every range. In select mode the ranges are extended.
motion :: Motion -> EditorM ()
motion m = do
  mode <- gets edMode
  let movement = if mode == Select then Extend else Move
  modifyDoc $ \d ->
    d {docSelection = normalize (mapRanges (applyMotion movement m (docBuffer d)) (docSelection d))}

-- | The keys that run an action in a mode, as the config binds them
-- (@:action name@ when nothing does), for texts that tell what to press.
keyHint :: Mode -> Text -> EditorM Text
keyHint m inv = gets (\e -> keyOr (edKeyHints e) (InMode m) inv)

-- | A line of hints in a mode (@"ret send · A-ret new line"@); unbound
-- actions are left out.
keyHints :: Mode -> [(Text, Text)] -> EditorM Text
keyHints m pairs = gets (\e -> hintLine (edKeyHints e) (InMode m) pairs)
