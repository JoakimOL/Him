-- | Commands that change modes or text.
module Him.Commands.Edit
  ( commands
  , insertChar
  ) where

import Control.Monad.Trans.State.Strict (gets, modify')
import Data.Map.Strict qualified as Map
import Data.Text qualified as T
import Him.Buffer (nextPos)
import Him.Command
import Him.Document (Document (..))
import Him.Edit
import Him.History (History, Snapshot (..), redo, undo)
import Him.Editor (Editor (..))
import Him.Mode (Mode (..))
import Him.Selection

commands :: [Command]
commands =
  [ Command "normal_mode" "Return to normal mode" (setMode Normal)
  , Command "insert_mode" "Insert before the selection" $ do
      modifySelection (\_ r -> point (rangeStart r))
      setMode Insert
  , Command "append_mode" "Insert after the selection" $ do
      modifySelection (\d r -> point (nextPos (docBuffer d) (rangeEnd r)))
      setMode Insert
  , Command "open_below" "Open a new line below and insert" $ do
      edit openLineBelow
      setMode Insert
  , Command "select_mode" "Toggle select (extend) mode" $
      gets edMode >>= \m -> setMode (if m == Select then Normal else Select)
  , Command "insert_newline" "Insert a line break" (edit insertNewline)
  , Command "insert_tab" "Insert a tab character" (insertChar '\t')
  , Command "delete_char_backward" "Delete the character before the cursor" (edit deleteBackward)
  , Command "delete_char_forward" "Delete the character under the cursor" (edit deleteForward)
  , Command "delete_selection" "Delete the selection (and yank it)" $ do
      yank
      edit deleteSelection
      setMode Normal
  , Command "change_selection" "Delete the selection (and yank it), then insert" $ do
      yank
      edit deleteSelection
      setMode Insert
  , Command "yank" "Copy the selection into the register" $ do
      yank
      n <- T.length <$> register
      info ("yanked " <> T.pack (show n) <> " characters")
  , Command "paste_after" "Paste after the selection" (register >>= edit . pasteAfter)
  , Command "paste_before" "Paste before the selection" (register >>= edit . pasteBefore)
  , Command "undo" "Undo the last change" (history "nothing to undo" undo)
  , Command "redo" "Redo the last undone change" (history "nothing to redo" redo)
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
