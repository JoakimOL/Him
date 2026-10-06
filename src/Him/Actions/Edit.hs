-- | Actions that change modes or text.
module Him.Actions.Edit
  ( actions
  , insertChar
  ) where

import Control.Monad.Trans.State.Strict (gets, modify')
import Data.Text qualified as T
import Him.Action
import Him.Buffer (nextPos)
import Him.Buffer qualified as Buffer
import Him.Position (Pos (..))
import Him.EditorM
import Him.Effect (Effect (..))
import Him.Document (Document (..))
import Him.Edit
import Him.Actions.Register (selectedRegister, yank)
import Him.History (History, Snapshot (..), redo, undo)
import Him.Editor (Await (..), Editor (..))
import Him.Options (Options (..))
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
  , simple "insert_at_line_start" GModes "Insert at the start of the line (after its indentation)" $ do
      modifySelection $ \d r ->
        let l = posLine (rangeHead r)
         in point (Pos l (T.length (T.takeWhile (\c -> c == ' ' || c == '\t') (Buffer.lineAt l (docBuffer d)))))
      setMode Insert
  , simple "insert_at_line_end" GModes "Insert at the end of the line" $ do
      modifySelection (\d r -> let l = posLine (rangeHead r) in point (Pos l (Buffer.lineLength l (docBuffer d))))
      setMode Insert
  , simple "open_below" GEditing "Open a new line below and insert" $ do
      edit (openLine Below)
      setMode Insert
  , simple "open_above" GEditing "Open a new line above and insert" $ do
      edit (openLine Above)
      setMode Insert
  , simple "replace" GEditing "Replace each selected character with the next key" $
      modify' (\e -> e {edAwait = Just AwaitReplaceChar})
  , simple "select_mode" GModes "Toggle select (extend) mode" $
      gets edMode >>= \m -> setMode (if m == Select then Normal else Select)
  , simple "insert_newline" GEditing "Insert a line break" (edit insertNewline)
  , simple "insert_tab" GEditing "Insert a tab, or tab-width spaces with expand-tab" $ do
      o <- gets edOptions
      if optExpandTab o then edit (insertAtHead (T.replicate (optTabWidth o) " ")) else insertChar '\t'
  , simple "delete_char_backward" GEditing "Delete the character before the cursor" (edit deleteBackward)
  , simple "delete_char_forward" GEditing "Delete the character under the cursor" (edit deleteForward)
  , simple "delete_selection" GEditing "Delete the selection (and yank it)" $ do
      yank =<< selectedRegister
      edit deleteSelection
      setMode Normal
  , simple "change_selection" GEditing "Delete the selection (and yank it), then insert" $ do
      yank =<< selectedRegister
      edit deleteSelection
      setMode Insert
  , simple "undo" GHistory "Undo the last change" (history "nothing to undo" undo)
  , simple "redo" GHistory "Redo the last undone change" (history "nothing to redo" redo)
  , action "insert_text" GEditing "Insert text before the selection" (text "text") (edit . insertAtHead)
  , action "set_mode" GModes "Switch to a mode" (choice "mode" [("normal", Normal), ("insert", Insert), ("select", Select)]) setMode
  , simple "no_op" GMisc "Do nothing (bind a key to this to disable it)" (pure ())
  , simple "suspend" GMisc "Suspend the editor (fg in the shell brings it back)" (request Suspend)
  ]

-- | Undo or redo: swap the current state with one from the history.
history :: T.Text -> (Snapshot -> History -> Maybe (Snapshot, History)) -> EditorM ()
history nothingMsg step = do
  d <- getDoc
  case step (Snapshot (docBuffer d) (docSelection d)) (docHistory d) of
    Nothing -> info nothingMsg
    -- In a REPL or chat buffer, undo may change the input only (output
    -- that came since is not undone away, ADR transcripts).
    Just (Snapshot buf _, _) | not (transcriptKept d buf) -> failWith transcriptMessage
    Just (Snapshot buf sel, h) ->
      modifyDoc $ \doc ->
        doc
          { docBuffer = buf
          , docSelection = sel
          , docHistory = h
          , docDirty = buf /= docSavedBuffer doc
          , docVersion = docVersion doc + 1
          }

insertChar :: Char -> EditorM ()
insertChar c = edit (insertAtHead (T.singleton c))

modifySelection :: (Document -> Range -> Range) -> EditorM ()
modifySelection f = modifyDoc (\d -> d {docSelection = mapRanges (f d) (docSelection d)})
