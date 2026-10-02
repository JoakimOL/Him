-- | A buffer together with everything that belongs to it: its selection,
-- the file it came from, and how that file was formatted.
module Him.Document
  ( Document (..)
  , DocKind (..)
  , DirEntry (..)
  , isReadOnly
  , LineEnding (..)
  , newDocument
  , displayName
  ) where

import Data.Text (Text)
import Data.Text qualified as T
import Him.Buffer (Buffer)
import Him.GitState (GitInfo (..))
import Him.History (History, emptyHistory)
import Him.Syntax (SyntaxInfo, noSyntax)
import Him.Position (Pos (..))
import Him.Selection (Selection, point, single)

data LineEnding = LF | CRLF
  deriving stock (Eq, Show)

-- | What a document shows.
data DocKind
  = TextDoc
  | -- | A directory listing (see "Him.Directory"): the entry shown on each
    -- line, starting at line 1 (line 0 is the header).
    DirectoryDoc ![DirEntry]
  deriving stock (Eq, Show)

data DirEntry = DirEntry
  { deName :: !FilePath
  , deIsDir :: !Bool
  }
  deriving stock (Eq, Show)

-- | Directory listings cannot be edited or written.
isReadOnly :: Document -> Bool
isReadOnly d = case docKind d of
  DirectoryDoc _ -> True
  TextDoc -> False

data Document = Document
  { docId :: !Int
  -- ^ Identifies the buffer while it is open (assigned by "Him.Editor"), so
  -- results of background work find their document.
  , docVersion :: !Int
  -- ^ Bumped on every change of the text, so stale results are dropped.
  , docBuffer :: !Buffer
  , docKind :: !DocKind
  , docSelection :: !Selection
  , docPath :: !(Maybe FilePath)
  , docDirty :: !Bool
  -- ^ Modified since the last load or save.
  , docLineEnding :: !LineEnding
  , docTrailingNewline :: !Bool
  -- ^ Whether the file ends with a line ending (written back on save).
  , docHistory :: !History
  , docSyntax :: !SyntaxInfo
  -- ^ Highlighting ("Him.Syntax").
  , docGit :: !GitInfo
  -- ^ The file's state in git ("Him.GitState"), for gutter signs and staging.
  , docSavedBuffer :: !Buffer
  -- ^ The text as last loaded or saved, to recompute 'docDirty' after
  -- undo/redo.
  }
  deriving stock (Eq, Show)

newDocument :: Maybe FilePath -> Buffer -> Document
newDocument path buf =
  Document
    { docId = 0
    , docVersion = 0
    , docBuffer = buf
    , docKind = TextDoc
    , docSelection = single (point (Pos 0 0))
    , docPath = path
    , docDirty = False
    , docLineEnding = LF
    , docTrailingNewline = True
    , docHistory = emptyHistory
    , docSyntax = noSyntax
    , docGit = GitUnknown
    , docSavedBuffer = buf
    }

displayName :: Document -> Text
displayName = maybe "[scratch]" T.pack . docPath
