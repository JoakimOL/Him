-- | Commands that change modes or text.
module Him.Commands.Edit
  ( commands
  , insertChar
  ) where

import Control.Monad.Trans.State.Strict (gets)
import Data.Text qualified as T
import Him.Buffer (nextPos)
import Him.Command
import Him.Document (Document (..))
import Him.Edit
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
  , Command "delete_selection" "Delete the selection" $ do
      edit deleteSelection
      setMode Normal
  , Command "change_selection" "Delete the selection and insert" $ do
      edit deleteSelection
      setMode Insert
  ]

insertChar :: Char -> EditorM ()
insertChar c = edit (insertAtHead (T.singleton c))

modifySelection :: (Document -> Range -> Range) -> EditorM ()
modifySelection f = modifyDoc (\d -> d {docSelection = mapRanges (f d) (docSelection d)})
