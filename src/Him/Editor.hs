-- | The complete, pure editor state.
module Him.Editor
  ( Editor (..)
  , Status (..)
  , Severity (..)
  , PromptKind (..)
  , newEditor
  ) where

import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Text (Text)
import Him.Document (Document)
import Him.Key (Key)
import Him.Mode (Mode (..))
import Him.Search (Direction)
import Him.Selection (Selection)
import Him.View (View, initialView)

data Severity = Info | Error
  deriving stock (Eq, Show)

-- | A message shown in the bottom row until the next key press.
data Status = Status !Severity !Text
  deriving stock (Eq, Show)

-- | What the command line is being used for.
data PromptKind
  = -- | A @:@ command.
    ExPrompt
  | -- | A search, with the selection to search from and to restore when
    -- the search is cancelled.
    SearchPrompt !Direction !Selection
  deriving stock (Eq, Show)

-- | Only one document for now; this becomes a list of documents plus a
-- "current" index when multiple buffers are added.
data Editor = Editor
  { edDoc :: !Document
  , edMode :: !Mode
  , edView :: !View
  , edSize :: !(Int, Int)
  -- ^ Terminal @(rows, cols)@.
  , edStatus :: !(Maybe Status)
  , edPending :: ![Key]
  -- ^ Keys of an unfinished key sequence (e.g. the @g@ of @g g@).
  , edCount :: !(Maybe Int)
  -- ^ A count typed before a key, e.g. the @5@ of @5 j@.
  , edCmdLine :: !Text
  -- ^ Text typed on the command line.
  , edPrompt :: !PromptKind
  , edPreviewPending :: !Bool
  -- ^ The search text changed; the incremental search preview is computed
  -- once before the next render, not for every key of a burst.
  , edRegisters :: !(Map Char Text)
  -- ^ Yanked text. Only the default register @\"@ is used so far.
  , edQuit :: !Bool
  }
  deriving stock (Eq, Show)

newEditor :: (Int, Int) -> Document -> Editor
newEditor size doc =
  Editor
    { edDoc = doc
    , edMode = Normal
    , edView = initialView
    , edSize = size
    , edStatus = Nothing
    , edPending = []
    , edCount = Nothing
    , edCmdLine = ""
    , edPrompt = ExPrompt
    , edPreviewPending = False
    , edRegisters = Map.empty
    , edQuit = False
    }
