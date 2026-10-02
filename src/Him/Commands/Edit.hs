-- | Actions that change modes or text.
module Him.Commands.Edit
  ( actions
  , insertChar
  ) where

import Control.Monad.Trans.State.Strict (gets, modify')
import Data.Map.Strict qualified as Map
import Data.Text qualified as T
import Him.Action
import Him.Buffer (nextPos)
import Him.Command
import Him.Document (Document (..))
import Him.Edit
import Him.History (History, Snapshot (..), redo, undo)
import Him.Editor (Editor (..))
import Him.Mode (Mode (..))
import Him.Selection

actions :: [Action]
actions =
  [ simple "normal_mode" GModes "Return to normal mode" (setMode Normal)
  , simple "insert_mode" GModes "Insert before the selection" $ do
      modifySelection (\_ r -> point (rangeStart r))
      setMode Insert
  , simple "append_mode" GModes "Insert after the selection" $ do
      modifySelection (\d r -> point (nextPos (docBuffer d) (rangeEnd r)))
      setMode Insert
  , simple "open_below" GEditing "Open a new line below and insert" $ do
      edit openLineBelow
      setMode Insert
  , simple "select_mode" GModes "Toggle select (extend) mode" $
      gets edMode >>= \m -> setMode (if m == Select then Normal else Select)
  , simple "insert_newline" GEditing "Insert a line break" (edit insertNewline)
  , simple "insert_tab" GEditing "Insert a tab character" (insertChar '\t')
  , simple "delete_char_backward" GEditing "Delete the character before the cursor" (edit deleteBackward)
  , simple "delete_char_forward" GEditing "Delete the character under the cursor" (edit deleteForward)
  , simple "delete_selection" GEditing "Delete the selection (and yank it)" $ do
      yank
      edit deleteSelection
      setMode Normal
  , simple "change_selection" GEditing "Delete the selection (and yank it), then insert" $ do
      yank
      edit deleteSelection
      setMode Insert
  , simple "yank" GClipboard "Copy the selection into the register" $ do
      yank
      n <- T.length <$> register
      info ("yanked " <> T.pack (show n) <> " characters")
  , simple "paste_after" GClipboard "Paste after the selection" (register >>= edit . pasteAfter)
  , simple "paste_before" GClipboard "Paste before the selection" (register >>= edit . pasteBefore)
  , simple "undo" GHistory "Undo the last change" (history "nothing to undo" undo)
  , simple "redo" GHistory "Redo the last undone change" (history "nothing to redo" redo)
  , action "insert_text" GEditing "Insert text before the selection" (text "text") (edit . insertAtHead)
  , action "set_mode" GModes "Switch to a mode" (choice "mode" [("normal", Normal), ("insert", Insert), ("select", Select)]) setMode
  , simple "no_op" GMisc "Do nothing (bind a key to this to disable it)" (pure ())
  ]

defaultRegister :: Char
defaultRegister = '"'

register :: EditorM T.Text
register = gets (Map.findWithDefault "" defaultRegister . edRegisters)

yank :: EditorM ()
yank = do
  d <- getDoc
  let t = selectionText (docBuffer d) (primary (docSelection d))
  modify' (\e -> e {edRegisters = Map.insert defaultRegister t (edRegisters e)})

-- | Undo or redo: swap the current state with one from the history.
history :: T.Text -> (Snapshot -> History -> Maybe (Snapshot, History)) -> EditorM ()
history nothingMsg step = do
  d <- getDoc
  case step (Snapshot (docBuffer d) (docSelection d)) (docHistory d) of
    Nothing -> info nothingMsg
    Just (Snapshot buf sel, h) ->
      modifyDoc $ \doc ->
        doc
          { docBuffer = buf
          , docSelection = sel
          , docHistory = h
          , docDirty = buf /= docSavedBuffer doc
          }

insertChar :: Char -> EditorM ()
insertChar c = edit (insertAtHead (T.singleton c))

modifySelection :: (Document -> Range -> Range) -> EditorM ()
modifySelection f = modifyDoc (\d -> d {docSelection = mapRanges (f d) (docSelection d)})
