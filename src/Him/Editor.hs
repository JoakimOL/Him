-- | The complete, pure editor state.
module Him.Editor
  ( Editor (..)
  , Status (..)
  , Severity (..)
  , newEditor
  ) where

import Data.Text (Text)
import Him.Document (Document)
import Him.Key (Key)
import Him.Mode (Mode (..))
import Him.View (View, initialView)

data Severity = Info | Error
  deriving stock (Eq, Show)

-- | A message shown in the bottom row until the next key press.
data Status = Status !Severity !Text
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
  , edCmdLine :: !Text
  -- ^ Text typed after @:@ in command mode.
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
    , edCmdLine = ""
    , edQuit = False
    }
