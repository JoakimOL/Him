-- | The monad actions run in, and helpers for writing actions. Keys are
-- bound to actions (see "Him.Action" and "Him.Keymap").
module Him.Command
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
  ) where

import Control.Monad.Trans.State.Strict (StateT, gets, modify')
import Data.Map.Strict qualified as Map
import Data.Text (Text)
import Him.Document (Document (..), isReadOnly)
import Him.Edit (Edit, applyEdits)
import Him.Editor
import Him.History (Snapshot (..), beginChange)
import Him.Mode (Mode (..))
import Him.Motion (Motion, Movement (..), applyMotion)
import Him.Selection (mapRanges, normalize)

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
    then failWith readOnlyMessage
    else modify' (\e -> e {edMode = m})

readOnlyMessage :: Text
readOnlyMessage = "a directory listing is read-only (ret opens an entry, - goes up)"

info :: Text -> EditorM ()
info t = modify' (\e -> e {edStatus = Just (Status Info t)})

failWith :: Text -> EditorM ()
failWith t = modify' (\e -> e {edStatus = Just (Status Error t)})

-- | A register's values (one per range it was yanked from); empty when unset.
getRegister :: Char -> EditorM [Text]
getRegister c = gets (Map.findWithDefault [] c . edRegisters)

setRegister :: Char -> [Text] -> EditorM ()
setRegister c vs = modify' (\e -> e {edRegisters = Map.insert c vs (edRegisters e)})

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
  if readOnly then failWith readOnlyMessage else editAll f

editAll :: (Int -> Edit) -> EditorM ()
editAll f = modifyDoc $ \d ->
  let (buf, sel) = applyEdits f (docBuffer d) (docSelection d)
   in d
        { docBuffer = buf
        , docSelection = sel
        , docDirty = True
        , docHistory = beginChange (Snapshot (docBuffer d) (docSelection d)) (docHistory d)
        }

-- | Apply a motion to every range. In select mode the ranges are extended.
motion :: Motion -> EditorM ()
motion m = do
  mode <- gets edMode
  let movement = if mode == Select then Extend else Move
  modifyDoc $ \d ->
    d {docSelection = normalize (mapRanges (applyMotion movement m (docBuffer d)) (docSelection d))}
