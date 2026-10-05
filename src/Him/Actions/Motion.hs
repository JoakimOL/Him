-- | Actions that move or reshape the selection.
module Him.Actions.Motion
  ( actions
  , awaitedKey
  , vertical
  ) where

import Control.Monad (replicateM_)
import Control.Monad.Trans.State.Strict (get, gets, modify')
import Data.Set qualified as Set
import Him.Buffer qualified as Buffer
import Him.Editor (Await (..), Editor (..), focusedTextHeight)
import Him.Key (Key (..), KeyCode (..), Modifier (..))
import Him.View (View (..))
import Data.Text (Text)
import Him.Action
import Him.EditorM
import Him.Actions.Jump (jumping)
import Him.Document (Document (..))
import Him.Motion
import Him.Actions.Match (awaitedMatchKey)
import Him.Options (Options (..))
import Him.Buffer (Buffer)
import Him.Selection (Selection, collapse, keepPrimary, mapRanges, removePrimary, rotatePrimary)

actions :: [Action]
actions =
  [ repeated "move_char_left" GMovement "Move left" charLeft
  , repeated "move_char_right" GMovement "Move right" charRight
  , action "move_line_up" GMovement "Move up" count (vertical . negate)
  , action "move_line_down" GMovement "Move down" count vertical
  , simple "goto_line_start" GMovement "Go to the start of the line" (motion lineStart)
  , simple "goto_line_end" GMovement "Go to the last character of the line" (motion lineEnd)
  , action "goto_file_start" GMovement "Go to the first line, or to line <count> (5 g g)" (optional "-" 0 (int "count")) $ \n ->
      jumping (motion (if n > 0 then gotoLine n else fileStart))
  , simple "goto_last_line" GMovement "Go to the last line" (jumping (motion lastLine))
  , action "goto_line" GMovement "Go to a line (counting from 1)" (int "line") (jumping . motion . gotoLine)
  , repeated "move_next_word_start" GSelection "Select to the start of the next word" nextWordStart
  , repeated "move_prev_word_start" GSelection "Select back to the start of the previous word" prevWordStart
  , repeated "move_next_word_end" GSelection "Select to the end of the next word" nextWordEnd
  , repeated "select_line" GSelection "Select the whole line (repeat to extend)" selectLine
  , simple "collapse_selection" GSelection "Reduce the selection to the cursor" $
      modifyDoc (\d -> d {docSelection = mapRanges collapse (docSelection d)})
  , action "page_down" GMovement "Move down a page" count (page 1 1)
  , action "page_up" GMovement "Move up a page" count (page (-1) 1)
  , action "half_page_down" GMovement "Move down half a page" count (page 1 2)
  , action "half_page_up" GMovement "Move up half a page" count (page (-1) 2)
  , action "find_next_char" GSelection "Select to the next occurrence of a character (f)" count (await True False)
  , action "find_till_char" GSelection "Select up to the next occurrence of a character (t)" count (await True True)
  , action "find_prev_char" GSelection "Select back to the previous occurrence of a character (F)" count (await False False)
  , action "till_prev_char" GSelection "Select back up to the previous occurrence of a character (T)" count (await False True)
  , simple "repeat_last_find" GSelection "Repeat the last f, t, F or T" $
      gets edLastFind >>= \case
        Just (forward, till, ch) -> motion (findChar True forward till ch 1)
        Nothing -> info "no character search to repeat"
  , simple "select_all" GSelection "Select the whole file" (jumping (withBuffer (const . selectAll)))
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

-- | Move the cursor and the view together by pages (@direction@ 1 or -1),
-- or by half pages (@parts@ 2).
page :: Int -> Int -> Int -> EditorM ()
page direction parts n = do
  ed <- get
  let height = focusedTextHeight ed
      delta = direction * n * max 1 (height `div` parts)
      lines' = Buffer.lineCount (docBuffer (edDoc ed))
  vertical delta
  modify' $ \e ->
    e {edView = (edView e) {viewTop = max 0 (min (lines' - 1) (viewTop (edView e) + delta))}}

-- | Move lines up (negative) or down, keeping the display column.
vertical :: Int -> EditorM ()
vertical delta = do
  tw <- gets (optTabWidth . edOptions)
  motion (lineBy tw delta)

-- | Wait for the character to find (see 'awaitedKey').
await :: Bool -> Bool -> Int -> EditorM ()
await forward till n = modify' (\e -> e {edAwait = Just (AwaitFind forward till n)})

-- | The key after @f t F T@ (the character to find; @ret@ is a line
-- break) or after a match-mode command (see "Him.Actions.Match"); anything
-- else cancels. 'False' when nothing was waiting for a key.
awaitedKey :: Key -> EditorM Bool
awaitedKey key =
  gets edAwait >>= \case
    Nothing -> pure False
    Just (AwaitFind forward till n) -> do
      modify' (\e -> e {edAwait = Nothing})
      case keyChar key of
        Just ch -> do
          modify' (\e -> e {edLastFind = Just (forward, till, ch)})
          motion (findChar False forward till ch n)
        Nothing -> pure ()
      pure True
    Just waiting -> do
      modify' (\e -> e {edAwait = Nothing})
      mapM_ (awaitedMatchKey waiting) (keyChar key)
      pure True
  where
    keyChar (Key code mods)
      | Set.null (Set.delete Shift mods) = case code of
          KChar c -> Just c
          KEnter -> Just '\n'
          KTab -> Just '\t'
          _ -> Nothing
      | otherwise = Nothing
