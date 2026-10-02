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
import Him.Buffer (Buffer)
import Him.Selection (Selection, collapse, keepPrimary, mapRanges, removePrimary, rotatePrimary)

actions :: [Action]
actions =
  [ repeated "move_char_left" GMovement "Move left" charLeft
  , repeated "move_char_right" GMovement "Move right" charRight
  , action "move_line_up" GMovement "Move up" count (motion . lineBy . negate)
  , action "move_line_down" GMovement "Move down" count (motion . lineBy)
  , simple "goto_line_start" GMovement "Go to the start of the line" (motion lineStart)
  , simple "goto_line_end" GMovement "Go to the last character of the line" (motion lineEnd)
  , simple "goto_file_start" GMovement "Go to the first line" (motion fileStart)
  , simple "goto_last_line" GMovement "Go to the last line" (motion lastLine)
  , action "goto_line" GMovement "Go to a line (counting from 1)" (int "line") (motion . gotoLine)
  , repeated "move_next_word_start" GSelection "Select to the start of the next word" nextWordStart
  , repeated "move_prev_word_start" GSelection "Select back to the start of the previous word" prevWordStart
  , repeated "move_next_word_end" GSelection "Select to the end of the next word" nextWordEnd
  , repeated "select_line" GSelection "Select the whole line (repeat to extend)" selectLine
  , simple "collapse_selection" GSelection "Reduce the selection to the cursor" $
      modifyDoc (\d -> d {docSelection = mapRanges collapse (docSelection d)})
  , simple "select_all" GSelection "Select the whole file" (withBuffer (const . selectAll))
  , simple "keep_primary_selection" GSelection "Keep only the primary selection" (withBuffer (const keepPrimary))
  , simple "remove_primary_selection" GSelection "Remove the primary selection" (withBuffer (const removePrimary))
  , simple "rotate_selections_forward" GSelection "Make the next selection primary" (withBuffer (const (rotatePrimary 1)))
  , simple "rotate_selections_backward" GSelection "Make the previous selection primary" (withBuffer (const (rotatePrimary (-1))))
  , action "copy_selection_on_next_line" GSelection "Copy each selection onto the next line" count $ \n ->
      replicateM_ n (withBuffer copySelectionBelow)
  , simple "split_selection_on_newline" GSelection "Split each selection into its lines" (withBuffer splitOnNewlines)
  ]

-- | Reshape the whole selection, given the buffer.
withBuffer :: (Buffer -> Selection -> Selection) -> EditorM ()
withBuffer f = modifyDoc (\d -> d {docSelection = f (docBuffer d) (docSelection d)})

-- | An optional repeat count, at least 1. A count typed before the key
-- (@5 j@) fills it.
count :: ArgSpec Int
count = max 1 <$> optional "1" 1 (int "count")

-- | A motion repeated by an optional count.
repeated :: Text -> ActionGroup -> Text -> Motion -> Action
repeated name grp doc m = action name grp doc count (\n -> replicateM_ n (motion m))
