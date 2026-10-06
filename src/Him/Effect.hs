-- | Effects: what an action asks for beyond changing the editor state
-- (ADR effects-and-runtime). Actions only queue them ('Him.EditorM.request'), so they stay
-- state changes that tests can inspect; the main loop carries them out.
--
-- Some are handled right after the key with the config at hand
-- ('RunAction', 'OpenPalette'); jobs run on background threads
-- ("Him.Runtime") and report back as 'JobResult' events.
module Him.Effect
  ( Effect (..)
  , Job (..)
  , JobKey (..)
  , jobKey
  , JobResult (..)
  , RegisterUse (..)
  ) where

import Data.Sequence (Seq)
import Data.Text (Text)
import Data.IntMap.Strict (IntMap)
import Him.Buffer (Buffer)
import Him.Language (Language)
import Him.Syntax.Span (LineSpan)
import Him.Diff (Hunk)
import Him.Document (LineEnding)
import Him.GitState (GitBase)
import Him.Invocation (Invocation)
import Him.Json (Value)
import Him.Lsp.State (ServerInfo)
import Him.Picker (PickerItem)
import Him.FileTree (WalkOptions)
import Him.Repl (ReplConfig)
import Him.Chat (ChatEvent, ChatRequest)

data Effect
  = -- | Run another action, e.g. the one chosen in the command palette.
    RunAction !Invocation
  | -- | Open the command palette (it lists the configured actions and keys).
    OpenPalette
  | -- | Start a background job; one already running with the same key is
    -- cancelled first.
    StartJob !Job
  | CancelJob !JobKey
  | -- | Send a message to a language server (by its key); queued, never
    -- blocks.
    LspSend !Text !Value
  | -- | Stop a language server (by its key).
    LspStop !Text
  | -- | Suspend the editor (Ctrl-Z): give the terminal back to the shell
    -- until it is continued (@fg@).
    Suspend
  | -- | Read the config file again and use it.
    ReloadConfig
  | -- | Open the config file (with the defaults in it if it does not exist).
    OpenConfig
  | -- | Use the theme of this name; 'Nothing' shows which one is used.
    ChangeTheme !(Maybe Text)
  | -- | Switch a plugin on or off (ADR git-and-lsp-as-plugins); 'Nothing' lists them.
    PluginCommand !(Maybe (Text, Bool))
  | -- | Stop every language server (the LSP plugin was switched off).
    LspStopAll
  | -- | Start the REPL of a language for a REPL buffer (by document id),
    -- in the project of a file.
    ReplStart !Int !Text !FilePath
  | -- | Send text to a REPL; 'True' wraps code of several lines first.
    ReplSend !Int !Bool !Text
  | ReplInterrupt !Int
  | ReplStop !Int
  | -- | Send a chat buffer's conversation to the chat provider (a request
    -- already running for it is cancelled).
    ChatSend !Int !ChatRequest
  | ChatCancel !Int
  | -- | Answer a tool call the model waits for (chat buffer, call id, is it
    -- an error, the result).
    ChatAnswer !Int !Text !Bool !Text
  | -- | Copy a register's values to the system clipboard (@+@) or primary
    -- selection (@*@).
    ClipboardSet !Char ![Text]
  | -- | Read the clipboard into its register, then use it.
    ClipboardGet !Char !RegisterUse
  | -- | Start a program for a plugin (ADR plugin-building-blocks): key (@plugin:name@; one
    -- running with the same key is stopped first), command, arguments,
    -- directory. Its lines come back as 'ProcessLine'.
    ProcessStart !Text !FilePath ![String] !(Maybe FilePath)
  | -- | Write to a plugin process's input.
    ProcessSend !Text !Text
  | ProcessStop !Text
  | -- | Stop every process of a plugin (it was switched off).
    ProcessStopAll !Text
  deriving stock (Eq, Show)

-- | What a register read from the clipboard is for.
data RegisterUse = UsePasteAfter | UsePasteBefore | UseReplace | UseInsert | UseShowRegisters | UseRefresh
  deriving stock (Eq, Show)

-- | Background work. Results carry the generation (and query) they were
-- started for, so an answer that arrives too late is recognised and
-- dropped.
data Job
  = -- | List the files below a directory for the picker of this generation.
    ScanFiles !Int !WalkOptions !FilePath
  | -- | Search the files below a directory for a query (literal, smart
    -- case), for the global search picker of this generation.
    GrepFiles !Int !Text !WalkOptions !FilePath
  | -- | Rank a large picker's items for a query.
    FilterPicker !Int !Text !(Seq PickerItem)
  | -- | Look up a document's file in git (by document id and path).
    GitLoad !Int !FilePath
  | -- | Diff a document version against its git base.
    GitDiff !Int !Int !GitBase !Buffer
  | -- | Write a new index version of a document's file ('Nothing' takes the
    -- file out of the index).
    GitWriteIndex !Int !GitBase !LineEnding !(Maybe [Text])
  | -- | Find a syntax provider for a document's language.
    SyntaxStart !Int !Language
  | -- | Highlight lines @[from, to]@ of a document version.
    Highlight !Int !Int !Buffer !Int !Int
  | -- | Make sure a language server runs for a document (id, language,
    -- path), starting it if needed.
    LspEnsure !Int !Language !FilePath
  | -- | Read a file for the picker's preview (unless larger than so many
    -- bytes).
    LoadPreview !Int !FilePath
  deriving stock (Eq, Show)

data JobKey = ScanJob | FilterJob | GrepJob | GitLoadJob !Int | GitDiffJob !Int | GitWriteJob !Int | SyntaxJob !Int | LspStartJob !Int | PreviewJob !FilePath
  deriving stock (Eq, Ord, Show)

jobKey :: Job -> JobKey
jobKey = \case
  ScanFiles {} -> ScanJob
  FilterPicker {} -> FilterJob
  GrepFiles {} -> GrepJob
  GitLoad d _ -> GitLoadJob d
  GitDiff d _ _ _ -> GitDiffJob d
  GitWriteIndex d _ _ _ -> GitWriteJob d
  SyntaxStart d _ -> SyntaxJob d
  Highlight d _ _ _ _ -> SyntaxJob d
  LspEnsure d _ _ -> LspStartJob d
  LoadPreview _ file -> PreviewJob file

data JobResult
  = FilesFound !Int ![FilePath]
  | ScanFinished !Int
  | -- | Generation, query, more matching lines (until the picker holds
    -- 'Him.Picker.matchLimit'), and how many matching lines were found
    -- since the last batch (those included).
    GrepFound !Int !Text ![PickerItem] !Int
  | GrepFinished !Int !Text
  | -- | Generation, query, best matches, total number of matches.
    PickerFiltered !Int !Text ![PickerItem] !Int
  | -- | Document id, and its git base ('Nothing': not in a repository).
    GitLoaded !Int !(Maybe GitBase)
  | -- | Document id, version, unstaged and staged hunks (buffer lines).
    GitDiffed !Int !Int ![Hunk] ![Hunk]
  | GitWritten !Int !(Either Text ())
  | -- | Document id, and the provider now highlighting it (if any).
    SyntaxStarted !Int !(Maybe Text)
  | -- | Document id, version, the lines covered, and their spans.
    Highlighted !Int !Int !Int !Int !(IntMap [LineSpan])
  | -- | A server serves the document: id, server key, absolute path,
    -- language id, what the server can do.
    LspReady !Int !Text !FilePath !Text !ServerInfo
  | -- | No server for the document, and why.
    LspUnavailable !Int !Text
  | -- | A message from a server (a reply or a notification).
    LspMessage !Text !Value
  | LspExited !Text
  | -- | A REPL runs: buffer id, how it was started, where.
    ReplStarted !Int !ReplConfig !FilePath
  | ReplOutput !Int !Text
  | -- | Something from the chat provider, for a chat buffer.
    ChatReply !Int !ChatEvent
  | -- | It stopped (or could not start), and why.
    ReplExited !Int !Text
  | -- | A file read for the preview, or why not.
    PreviewLoaded !FilePath !(Either Text Buffer)
  | -- | A line of output from a plugin process (by key).
    ProcessLine !Text !Text
  | -- | It ended: the exit code (-1: it could not start; the reason came
    -- as a line before).
    ProcessDone !Text !Int
  deriving stock (Eq, Show)
