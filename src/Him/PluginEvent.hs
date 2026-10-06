-- | Events for plugins (ADR plugin-building-blocks): buffers opened, changed, saved, closed,
-- entered, mode changes, and the cursor moving. They are found by comparing the editor with
-- what was seen after the last event, so no code path that opens, edits
-- or saves has to remember to raise them. Pure.
module Him.PluginEvent
  ( Event (..)
  , Seen (..)
  , unseen
  , detectEvents
  ) where

import Data.IntMap.Strict (IntMap)
import Data.IntMap.Strict qualified as IntMap
import Data.Text (Text)
import Him.Document (Document (..))
import Him.Mode (Mode)
import Him.Position (Pos)

data Event
  = -- | A document was opened (at startup, every document is).
    BufferOpened !Int
  | BufferClosed !Int
  | -- | The focused window shows another document.
    BufferEntered !Int
  | -- | A document's text changed; its new version.
    BufferChanged !Int !Int
  | BufferSaved !Int
  | -- | From one mode to another.
    ModeChanged !Mode !Mode
  | -- | The cursor of the focused window moved (or it shows another
    -- buffer): the buffer, and where the cursor is now.
    CursorMoved !Int !Pos
  | -- | A line of output from a process the plugin started (by its key).
    ProcessOutput !Text !Text
  | -- | The process ended, with its exit code.
    ProcessExited !Text !Int
  deriving stock (Eq, Show)

-- | What was seen after the last event: each document's version and
-- number of saves, the mode, the focused document, its cursor.
data Seen = Seen
  { seenDocs :: !(IntMap (Int, Int))
  , seenMode :: !(Maybe Mode)
  , seenFocus :: !(Maybe Int)
  , seenCursor :: !(Maybe Pos)
  }
  deriving stock (Eq, Show)

-- | Before the first event: everything is new.
unseen :: Seen
unseen = Seen IntMap.empty Nothing Nothing Nothing

-- | The events since the last look, in a stable order: closed, opened,
-- changed, saved, entered, the mode, then the cursor (not at the first
-- look).
detectEvents :: [Document] -> Int -> Pos -> Mode -> Seen -> ([Event], Seen)
detectEvents docs focus cur mode seen = (events, Seen now (Just mode) (Just focus) (Just cur))
  where
    now = IntMap.fromList [(docId d, (docVersion d, docSaves d)) | d <- docs]
    before = seenDocs seen
    events =
      [BufferClosed i | i <- IntMap.keys (IntMap.difference before now)]
        <> [BufferOpened i | i <- IntMap.keys (IntMap.difference now before)]
        <> [BufferChanged i v | (i, (v, _)) <- IntMap.toList now, Just (v', _) <- [IntMap.lookup i before], v /= v']
        <> [BufferSaved i | (i, (_, s)) <- IntMap.toList now, Just (_, s') <- [IntMap.lookup i before], s /= s']
        <> [BufferEntered focus | seenFocus seen /= Just focus]
        <> [ModeChanged old mode | Just old <- [seenMode seen], old /= mode]
        <> [CursorMoved focus cur | Just old <- [seenCursor seen], old /= cur || seenFocus seen /= Just focus]
