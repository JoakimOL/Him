-- | What the editor knows about language servers (ADR-29): pure data, kept
-- in the 'Him.Editor.Editor' and each document; the processes themselves
-- live in the runtime.
module Him.Lsp.State
  ( LspState (..)
  , emptyLsp
  , ServerInfo (..)
  , Sync (..)
  , DocLsp (..)
  , Attachment (..)
  , Pending (..)
  , ShownDiagnostic (..)
  , Completion (..)
  , shownDiagnostics
  ) where

import Data.IntMap.Strict (IntMap)
import Data.IntMap.Strict qualified as IntMap
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.List (sortOn)
import Data.Text (Text)
import Data.Text qualified as T
import Him.Buffer (Buffer, lineAt, lineCount)
import Him.Json (Value)
import Him.Lsp.Protocol (CompletionItem, Diagnostic (..), Encoding (..), Severity, fromLspColumn)

-- | What the editor needs to know about a running server.
data ServerInfo = ServerInfo
  { siEncoding :: !Encoding
  , siTriggers :: ![Text]
  -- ^ Characters that start completion.
  , siSignatureTriggers :: ![Text]
  -- ^ Characters that ask for signature help.
  , siSync :: !Sync
  , siName :: !Text
  -- ^ The command, for messages.
  , siRoot :: !FilePath
  , siCapabilities :: !Value
  }
  deriving stock (Eq, Show)

-- | How the server wants text changes: not at all, whole, or as edits.
data Sync = SyncNone | SyncFull | SyncIncremental
  deriving stock (Eq, Show)

-- | A document's link to a server.
data DocLsp
  = LspUnknown
  | LspStarting
  | -- | No server for this document (no language, none configured, or it
    -- failed).
    LspNone
  | LspAttached !Attachment
  deriving stock (Eq, Show)

data Attachment = Attachment
  { atServer :: !Text
  , atPath :: !FilePath
  -- ^ Absolute, as the server knows it.
  , atLanguageId :: !Text
  , atSent :: !Int
  -- ^ The version last sent (@-1@: not opened yet).
  , atSentText :: !(Maybe Buffer)
  -- ^ The text last sent, to compute the next change from.
  , atSaves :: !Int
  -- ^ The document's save count last reported (@didSave@).
  }
  deriving stock (Eq, Show)

-- | A request waiting for its reply, and what to do with it.
data Pending
  = PendingHover
  | -- | Definitions, references, …: the picker's title.
    PendingLocations !Text
  | -- | Document, its version, and where the completed word starts.
    PendingCompletion !Int !Int !(Int, Int)
  | PendingRename
  | -- | Document and version formatted.
    PendingFormat !Int !Int
  | PendingCodeActions
  | -- | A code action asked to be completed (@codeAction/resolve@).
    PendingResolve
  | PendingSignature
  | -- | The document's path.
    PendingSymbols !FilePath
  | -- | An accepted completion's details (imports): document and the
    -- version right after the insertion.
    PendingCompletionResolve !Int !Int
  | -- | Workspace symbols for the picker of this generation and query.
    PendingWorkspaceSymbols !Int !Text
  | -- | An answer nothing waits for (e.g. @workspace/executeCommand@).
    PendingIgnore
  deriving stock (Eq, Show)

data LspState = LspState
  { lsServers :: !(Map Text ServerInfo)
  , lsPending :: !(IntMap Pending)
  -- ^ By request id.
  , lsDiagnostics :: !(Map FilePath [Diagnostic])
  -- ^ By absolute path, as the servers last published them.
  , lsAutoVersion :: !Int
  -- ^ The document version last looked at for automatic completion.
  , lsSignatureVersion :: !Int
  -- ^ The same, for signature help.
  }
  deriving stock (Eq, Show)

emptyLsp :: LspState
emptyLsp = LspState Map.empty IntMap.empty Map.empty (-1) (-1)

-- | The completion menu in insert mode.
data Completion = Completion
  { cmDoc :: !Int
  , cmStart :: !(Int, Int)
  -- ^ Where the completed word starts (line, character column).
  , cmItems :: ![CompletionItem]
  -- ^ All the server offered.
  , cmShown :: ![CompletionItem]
  -- ^ Those matching what is typed now, best first.
  , cmSelected :: !Int
  }
  deriving stock (Eq, Show)

-- | A diagnostic placed in the buffer: line, character columns, severity,
-- message.
data ShownDiagnostic = ShownDiagnostic
  { sdLine :: !Int
  , sdStart :: !Int
  , sdEnd :: !Int
  , sdSeverity :: !Severity
  , sdMessage :: !Text
  }
  deriving stock (Eq, Show)

-- | A document's diagnostics in character columns, one per line they
-- cover, sorted by position. Columns are converted against the current
-- text (positions after an edit may be off until the server republishes).
shownDiagnostics :: LspState -> DocLsp -> Buffer -> [ShownDiagnostic]
shownDiagnostics st doc buf = case doc of
  LspAttached (Attachment server path _ _ _ _) ->
    let enc = maybe Utf16 siEncoding (Map.lookup server (lsServers st))
        n = lineCount buf
     in sortOn (\sd -> (sdLine sd, sdStart sd)) $
          [ ShownDiagnostic l s e (diagSeverity d) (diagMessage d)
          | d <- Map.findWithDefault [] path (lsDiagnostics st)
          , let (sl, sc) = diagStart d
                (el, ec) = diagEnd d
          , l <- [max 0 sl .. min el (n - 1)]
          , let text = lineAt l buf
                s = if l == sl then fromLspColumn enc text sc else 0
                e0 = if l == el then fromLspColumn enc text ec else T.length text
                -- At least one character wide, so an empty range shows.
                e = max (s + 1) e0
          ]
  _ -> []
