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
  , changeDocument
  , replaceBuffer
  , clampSelection
  , inputPos
  , unsaved
  , setInputPos
  ) where

import Data.Maybe (isNothing)
import Data.Text (Text)
import Data.Text qualified as T
import Him.Buffer (Buffer, clampPos)
import Him.GitState (GitInfo (..))
import Him.History (History, Snapshot (..), beginChange, emptyHistory)
import Him.Lsp.State (DocLsp (..))
import Him.Repl (ReplState (..))
import Him.Chat (ChatState (..))
import Him.Syntax (SyntaxInfo, noSyntax)
import Him.Position (Pos (..))
import Him.Selection (Range (..), Selection, mapRanges, point, single)

data LineEnding = LF | CRLF
  deriving stock (Eq, Show)

-- | What a document shows.
data DocKind
  = TextDoc
  | -- | A directory listing (see "Him.Directory"): the entry shown on each
    -- line, starting at line 1 (line 0 is the header).
    DirectoryDoc ![DirEntry]
  | -- | A REPL's transcript and input (see "Him.Repl").
    ReplDoc !ReplState
  | -- | An AI chat's transcript and input (see "Him.Chat").
    ChatDoc !ChatState
  | -- | Read-only text a plugin shows, by name (ADR-51).
    ScratchDoc !Text
  deriving stock (Eq, Show)

data DirEntry = DirEntry
  { deName :: !FilePath
  , deIsDir :: !Bool
  }
  deriving stock (Eq, Show)

-- | Directory listings and plugins' scratch buffers cannot be edited or
-- written.
isReadOnly :: Document -> Bool
isReadOnly d = case docKind d of
  DirectoryDoc _ -> True
  ScratchDoc _ -> True
  TextDoc -> False
  ReplDoc _ -> False
  ChatDoc _ -> False

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
  , docSaves :: !Int
  -- ^ How often it was saved (language servers hear about saves).
  , docLsp :: !DocLsp
  -- ^ The document's language server, if any ("Him.Lsp.State").
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
    , docSaves = 0
    , docLsp = LspUnknown
    , docGit = GitUnknown
    , docSavedBuffer = buf
    }

displayName :: Document -> Text
displayName d = case docKind d of
  ReplDoc rs -> "[repl: " <> rsLanguage rs <> "]"
  ChatDoc _ -> "[chat]"
  ScratchDoc name -> "[" <> name <> "]"
  _ -> maybe "[scratch]" T.pack (docPath d)

-- | A new text and selection as one undoable change: the old ones are kept
-- for undo, and the version goes up (so highlighting, git and the language
-- server notice). The dirty flag is the caller's ('replaceBuffer' compares).
changeDocument :: Buffer -> Selection -> Document -> Document
changeDocument buf sel d =
  d
    { docBuffer = buf
    , docSelection = sel
    , docVersion = docVersion d + 1
    , docHistory = beginChange (Snapshot (docBuffer d) (docSelection d)) (docHistory d)
    }

-- | 'changeDocument' for a whole new text (a reset, a server's edits): the
-- document is dirty unless the text is what was saved.
replaceBuffer :: Buffer -> Selection -> Document -> Document
replaceBuffer buf sel d = (changeDocument buf sel d) {docDirty = buf /= docSavedBuffer d}

-- | Keep every range inside a (changed) text.
clampSelection :: Buffer -> Selection -> Selection
clampSelection buf = mapRanges (\r -> r {rangeAnchor = clampPos buf (rangeAnchor r), rangeHead = clampPos buf (rangeHead r)})

-- | Where a transcript's input starts (a REPL or chat buffer).
inputPos :: Document -> Maybe Pos
inputPos d = case docKind d of
  ReplDoc rs -> Just (rsInput rs)
  ChatDoc cs -> Just (csInput cs)
  _ -> Nothing

setInputPos :: Pos -> Document -> Document
setInputPos p d = case docKind d of
  ReplDoc rs -> d {docKind = ReplDoc rs {rsInput = p}}
  ChatDoc cs -> d {docKind = ChatDoc cs {csInput = p}}
  _ -> d

-- | Changes that would be lost: a modified document, but not a REPL or
-- chat transcript (those are never saved).
unsaved :: Document -> Bool
unsaved d = docDirty d && isNothing (inputPos d)
