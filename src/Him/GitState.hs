-- | What a document knows about its file in git (ADR git): the versions in
-- the index and in HEAD, the hunks between them and the buffer, and the
-- staging edits computed from those (the signs are drawn by the git
-- plugin, "Him.Actions.Git"). Pure; the git
-- commands are in "Him.Git" and run as background jobs.
module Him.GitState
  ( GitBase (..)
  , GitInfo (..)
  , GitTracking (..)
  , SignKind (..)
  , tracking
  , changeStarts
  , applySelected
  , invertHunk
  ) where

import Data.Text (Text)
import Him.Diff

-- | The file's versions in git, as lines (split like the buffer).
data GitBase = GitBase
  { gbRoot :: !FilePath
  , gbPath :: !FilePath
  -- ^ Relative to the repository root.
  , gbMode :: !Text
  -- ^ The index entry's mode (@100644@); empty when the file is untracked.
  , gbIndex :: ![Text]
  , gbIndexNewline :: !Bool
  -- ^ Whether the index version ends with a line break.
  , gbHead :: ![Text]
  , gbInHead :: !Bool
  , gbBranch :: !Text
  -- ^ The checked-out branch when the base was loaded (empty before the
  -- first commit).
  }
  deriving stock (Eq, Show)

data GitInfo
  = -- | Not looked up yet (or to be looked up again, e.g. after a save).
    GitUnknown
  | GitLoading
  | -- | Not in a repository, or no file.
    GitOutside
  | GitTracked !GitTracking
  deriving stock (Eq, Show)

data GitTracking = GitTracking
  { gtBase :: !GitBase
  , gtUnstaged :: ![Hunk]
  -- ^ Index → buffer.
  , gtStaged :: ![Hunk]
  -- ^ HEAD → index, with the new side moved to buffer lines.
  , gtVersion :: !Int
  -- ^ The document version the hunks are for (-1: none yet).
  , gtRequested :: !Int
  -- ^ The version a diff was last asked for (-1: none). A newer version
  -- asks again; the job waits a moment first and is replaced by a newer
  -- one, so typing diffs once it pauses (ADR per-key-work).
  , gtReload :: !Bool
  -- ^ The base should be loaded again (after a save or staging).
  }
  deriving stock (Eq, Show)

tracking :: GitInfo -> Maybe GitTracking
tracking = \case
  GitTracked t -> Just t
  _ -> Nothing

data SignKind = SignAdded | SignChanged | SignRemoved
  deriving stock (Eq, Show)

-- | The buffer lines where changes start, sorted (for @] g@ / @[ g@).
changeStarts :: GitTracking -> [Int]
changeStarts t = mergeSorted (map start (gtUnstaged t)) (map start (gtStaged t))
  where
    start h = if hunkKind h == Removed then max 0 (hNewStart h - 1) else hNewStart h
    mergeSorted (a : as) (b : bs)
      | a < b = a : mergeSorted as (b : bs)
      | a > b = b : mergeSorted (a : as) bs
      | otherwise = a : mergeSorted as bs
    mergeSorted as [] = as
    mergeSorted [] bs = bs

-- | The old lines with the selected changes towards the new lines applied
-- (@hunks@ = 'diffLines' old new; @selected@ is about new lines). This is
-- staging. Unstaging and resetting revert the selected changes, which is
-- the same as applying the unselected ones.
--
-- Within a changed hunk, removed and added lines are paired by position,
-- so single lines can be taken; the lines left over are pure additions
-- (taken when selected) or removals (attached to the hunk's last new
-- line, or the line above a pure removal).
applySelected :: [Text] -> [Text] -> [Hunk] -> (Int -> Bool) -> [Text]
applySelected old new hunks selected = go 0 hunks old
  where
    go _ [] rest = rest
    go pos (h : hs) rest =
      let (keep, rest') = splitAt (hOldStart h - pos) rest
       in keep <> piece h <> go (hOldStart h + hOldCount h) hs (drop (hOldCount h) rest')
    piece h =
      let os = take (hOldCount h) (drop (hOldStart h) old)
          ns = take (hNewCount h) (drop (hNewStart h) new)
          k = min (hOldCount h) (hNewCount h)
          paired = [if selected (hNewStart h + i) then n else o | (i, o, n) <- zip3 [0 ..] os ns]
          added = [n | (j, n) <- zip [k ..] (drop k ns), selected (hNewStart h + j)]
          anchor = if hNewCount h > 0 then hNewStart h + hNewCount h - 1 else max 0 (hNewStart h - 1)
          removed = if selected anchor then [] else drop k os
       in paired <> added <> removed

-- | The same hunk seen from the other side.
invertHunk :: Hunk -> Hunk
invertHunk (Hunk os oc ns nc) = Hunk ns nc os oc
