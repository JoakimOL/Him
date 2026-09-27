-- | Commands that move or reshape the selection.
module Him.Commands.Motion
  ( commands
  ) where

import Him.Command
import Him.Motion
import Him.Selection (collapse, mapRanges)
import Him.Document (Document (..))

commands :: [Command]
commands =
  [ Command "move_char_left" "Move left" (motion charLeft)
  , Command "move_char_right" "Move right" (motion charRight)
  , Command "move_line_up" "Move up" (motion lineUp)
  , Command "move_line_down" "Move down" (motion lineDown)
  , Command "goto_line_start" "Go to the start of the line" (motion lineStart)
  , Command "goto_line_end" "Go to the last character of the line" (motion lineEnd)
  , Command "goto_file_start" "Go to the first line" (motion fileStart)
  , Command "goto_last_line" "Go to the last line" (motion lastLine)
  , Command "move_next_word_start" "Select to the start of the next word" (motion nextWordStart)
  , Command "move_prev_word_start" "Select back to the start of the previous word" (motion prevWordStart)
  , Command "move_next_word_end" "Select to the end of the next word" (motion nextWordEnd)
  , Command "select_line" "Select the whole line (repeat to extend)" (motion selectLine)
  , Command "collapse_selection" "Reduce the selection to the cursor" $
      modifyDoc (\d -> d {docSelection = mapRanges collapse (docSelection d)})
  ]
