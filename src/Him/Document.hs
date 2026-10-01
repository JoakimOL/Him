-- | A buffer together with everything that belongs to it: its selection,
-- the file it came from, and how that file was formatted.
module Him.Document
  ( Document (..)
  , LineEnding (..)
  , newDocument
  , displayName
  ) where

import Data.Text (Text)
import Data.Text qualified as T
import Him.Buffer (Buffer)
import Him.History (History, emptyHistory)
import Him.Position (Pos (..))
import Him.Selection (Selection, point, single)

data LineEnding = LF | CRLF
  deriving stock (Eq, Show)

data Document = Document
  { docBuffer :: !Buffer
  , docSelection :: !Selection
  , docPath :: !(Maybe FilePath)
  , docDirty :: !Bool
  -- ^ Modified since the last load or save.
  , docLineEnding :: !LineEnding
  , docTrailingNewline :: !Bool
  -- ^ Whether the file ends with a line ending (written back on save).
  , docHistory :: !History
  , docSavedBuffer :: !Buffer
  -- ^ The text as last loaded or saved, to recompute 'docDirty' after
  -- undo/redo.
  }
  deriving stock (Eq, Show)

newDocument :: Maybe FilePath -> Buffer -> Document
newDocument path buf =
  Document
    { docBuffer = buf
    , docSelection = single (point (Pos 0 0))
    , docPath = path
    , docDirty = False
    , docLineEnding = LF
    , docTrailingNewline = True
    , docHistory = emptyHistory
    , docSavedBuffer = buf
    }

displayName :: Document -> Text
displayName = maybe "[scratch]" T.pack . docPath
