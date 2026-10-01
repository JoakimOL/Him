-- | Undo/redo history of a document, as snapshots.
--
-- The buffer is a persistent structure, so a snapshot shares almost all of
-- its memory with the current state; keeping whole snapshots is cheap and
-- simple. A change set representation (and an undo tree) can replace this
-- later behind the same interface.
module Him.History
  ( History
  , Snapshot (..)
  , emptyHistory
  , beginChange
  , commit
  , undo
  , redo
  ) where

import Him.Buffer (Buffer)
import Him.Selection (Selection)

data Snapshot = Snapshot
  { snapBuffer :: !Buffer
  , snapSelection :: !Selection
  }
  deriving stock (Eq, Show)

data History = History
  { histUndo :: ![Snapshot]
  , histRedo :: ![Snapshot]
  , histPending :: !(Maybe Snapshot)
  -- ^ The state before the change currently in progress (e.g. an insert
  -- session), not yet an undo step.
  }
  deriving stock (Eq, Show)

-- | Maximum number of undo steps kept.
maxUndo :: Int
maxUndo = 1000

emptyHistory :: History
emptyHistory = History [] [] Nothing

-- | Record the state before an edit. Only the first edit of a change counts,
-- so everything up to the next 'commit' is undone in one step.
beginChange :: Snapshot -> History -> History
beginChange s h = case histPending h of
  Nothing -> h {histPending = Just s}
  Just _ -> h

-- | Turn the change in progress into an undo step.
commit :: History -> History
commit h = case histPending h of
  Nothing -> h
  Just s -> History (take maxUndo (s : histUndo h)) [] Nothing

-- | Given the current state, the state to go back to and the new history.
undo :: Snapshot -> History -> Maybe (Snapshot, History)
undo current h = case histUndo (commit h) of
  [] -> Nothing
  (s : rest) -> Just (s, History rest (current : histRedo (commit h)) Nothing)

redo :: Snapshot -> History -> Maybe (Snapshot, History)
redo current h = case histRedo h of
  [] -> Nothing
  (s : rest) -> Just (s, h {histUndo = current : histUndo h, histRedo = rest})
