-- | Actions that move or reshape the selection.
module Him.Commands.Motion
  ( actions
  ) where

import Control.Monad (replicateM_)
import Data.Text (Text)
import Him.Action
import Him.Command
import Him.Document (Document (..))
import Him.Motion
import Him.Selection (collapse, mapRanges)

actions :: [Action]
actions =
  [ counted "move_char_left" "Move left" (\n -> replicateM_ n (motion charLeft))
  , counted "move_char_right" "Move right" (\n -> replicateM_ n (motion charRight))
  , counted "move_line_up" "Move up" (motion . lineBy . negate)
  , counted "move_line_down" "Move down" (motion . lineBy)
  , simple "goto_line_start" GMovement "Go to the start of the line" (motion lineStart)
  , simple "goto_line_end" GMovement "Go to the last character of the line" (motion lineEnd)
  , simple "goto_file_start" GMovement "Go to the first line" (motion fileStart)
  , simple "goto_last_line" GMovement "Go to the last line" (motion lastLine)
  , action "goto_line" GMovement "Go to a line (counting from 1)" (int "line") (motion . gotoLine)
  , simple "move_next_word_start" GSelection "Select to the start of the next word" (motion nextWordStart)
  , simple "move_prev_word_start" GSelection "Select back to the start of the previous word" (motion prevWordStart)
  , simple "move_next_word_end" GSelection "Select to the end of the next word" (motion nextWordEnd)
  , simple "select_line" GSelection "Select the whole line (repeat to extend)" (motion selectLine)
  , simple "collapse_selection" GSelection "Reduce the selection to the cursor" $
      modifyDoc (\d -> d {docSelection = mapRanges collapse (docSelection d)})
  ]

-- | A movement with an optional repeat count (at least 1).
counted :: Text -> Text -> (Int -> EditorM ()) -> Action
counted name doc run = action name GMovement doc (optional "1" 1 (int "count")) (run . max 1)
