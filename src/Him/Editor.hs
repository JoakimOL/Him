-- | The complete, pure editor state.
module Him.Editor
  ( Editor (..)
  , Status (..)
  , Severity (..)
  , PromptKind (..)
  , FileAction (..)
  , InfoBox (..)
  , Preview (..)
  , Await (..)
  , InfoPlace (..)
  , newEditor
  , keymapMode
    -- * Buffers
  , Buffered (..)
  , buffers
  , setBuffers
  , bufferIndex
  , switchBuffer
  , gotoBuffer
  , openBuffer
  , closeBuffer
  , modifyDocument
  , previewFor
  , mapDocuments
  ) where

import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Text (Text)
import Him.Key (Key)
import Him.Options (Options, defaultOptions)
import Him.Mode (Mode (..))
import Him.Effect (Effect)
import Data.Text qualified as T
import Him.Lsp.State (Attachment (..), Completion, DocLsp (..), LspState, emptyLsp)
import Him.Position (Pos (..))
import Him.Picker (PickTarget (..), Picker)
import Him.Search (Direction)
import Him.Selection (Selection, primary, rangeHead)
import Him.Buffer (Buffer)
import Him.Buffer qualified as Buffer
import Him.Document (DocKind (..), Document (..), newDocument)
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
  | -- | Select the matches of a pattern inside the selection (Helix @s@),
    -- with the selection to restore when cancelled.
    SelectPrompt !Selection
  | -- | A file operation in a directory listing, waiting for a name or a
    -- confirmation.
    FilePrompt !FileAction
  | -- | The new name for the symbol at this position (language server).
    RenamePrompt !(Int, Int)
  deriving stock (Eq, Show)

-- | File operations in a directory listing ("Him.Commands.Directory").
-- The first field is always the listed directory.
data FileAction
  = NewFile !FilePath
  | NewDirectory !FilePath
  | -- | The entry's current name.
    RenameEntry !FilePath !FilePath
  | -- | The entries to delete; the prompt asks for @y@.
    DeleteEntries !FilePath ![FilePath]
  deriving stock (Eq, Show)

-- | A popup listing what can be typed next: the keys after a prefix such
-- as @g@, or the @:@ commands matching what is typed. Computed after every
-- key ("Him.Info"), drawn by "Him.Render.Info".
data InfoBox = InfoBox
  { infoTitle :: !Text
  , infoRows :: ![(Text, Text)]
  -- ^ A key or name, and its description.
  , infoPlace :: !InfoPlace
  }
  deriving stock (Eq, Show)

-- | What a command waiting for a key will do with it.
data Await
  = -- | Find a character: forward?, till?, count.
    AwaitFind !Bool !Bool !Int
  deriving stock (Eq, Show)

-- | A file's text for the picker's preview.
data Preview
  = PreviewLoading
  | PreviewText !Buffer
  | -- | Why there is nothing to show (binary, too large, unreadable).
    PreviewNone !Text
  deriving stock (Eq, Show)

-- | Where the box sits: a corner of the text area, or next to the cursor.
data InfoPlace = BottomLeft | BottomRight | AtCursor | AboveCursor
  deriving stock (Eq, Show)

-- | The open documents form a zipper: the current one ('edDoc', with
-- 'edView'), and the others before and after it in buffer order. Code that
-- works on the current document never sees the others.
data Editor = Editor
  { edDoc :: !Document
  , edBefore :: ![Buffered]
  -- ^ Buffers before the current one, nearest first.
  , edAfter :: ![Buffered]
  -- ^ Buffers after the current one, in order.
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
  , edAwait :: !(Maybe Await)
  -- ^ A command waiting for the next key (the character of @f@).
  , edLastFind :: !(Maybe (Bool, Bool, Char))
  -- ^ The last @f t F T@: forward, till, character (repeated by @A-.@).
  , edRepaint :: !Bool
  -- ^ The terminal lost its contents (after a suspend): draw everything.
  , edPreviews :: !(Map FilePath Preview)
  -- ^ Files read for the picker's preview (while a picker is open).
  , edPicker :: !(Maybe Picker)
  -- ^ The open picker, shown in 'Picking' mode.
  , edEffects :: ![Effect]
  -- ^ Effects requested by actions, oldest first ("Him.Effect").
  , edNextId :: !Int
  -- ^ The id the next opened document gets ('docId').
  , edLsp :: !LspState
  -- ^ Language servers, pending requests, diagnostics ("Him.Lsp.State").
  , edCompletion :: !(Maybe Completion)
  -- ^ The completion menu, in insert mode.
  , edPopup :: !(Maybe InfoBox)
  -- ^ A box shown until the next key (e.g. hover documentation).
  , edOptions :: !Options
  , edSignLane :: !Bool
  -- ^ The gutter has a sign lane (an enabled plugin draws there).
  -- ^ The settings (@[editor]@ in the config file).
  , edInfo :: !(Maybe InfoBox)
  , edCompletions :: ![Text]
  -- ^ Candidates from the last @tab@ on the command line, shown until the
  -- line changes.
  , edRegisters :: !(Map Char [Text])
  -- ^ Registers: @\"@ (yanked text, one value per range) and @/@ (the
  -- last search).
  , edQuit :: !Bool
  }
  deriving stock (Eq, Show)

newEditor :: (Int, Int) -> Document -> Editor
newEditor size doc =
  Editor
    { edDoc = doc {docId = 1}
    , edBefore = []
    , edAfter = []
    , edMode = Normal
    , edView = initialView
    , edSize = size
    , edStatus = Nothing
    , edPending = []
    , edCount = Nothing
    , edCmdLine = ""
    , edPrompt = ExPrompt
    , edPreviewPending = False
    , edAwait = Nothing
    , edLastFind = Nothing
    , edRepaint = False
    , edPreviews = Map.empty
    , edPicker = Nothing
    , edEffects = []
    , edNextId = 2
    , edLsp = emptyLsp
    , edCompletion = Nothing
    , edPopup = Nothing
    , edOptions = defaultOptions
    , edSignLane = True
    , edInfo = Nothing
    , edCompletions = []
    , edRegisters = Map.empty
    , edQuit = False
    }

-- | The mode whose keymap applies: normal mode in a directory listing uses
-- the 'Directory' layer, insert mode with the completion menu open the
-- 'Completing' one.
keymapMode :: Editor -> Mode
keymapMode ed = case (edMode ed, docKind (edDoc ed)) of
  (Normal, DirectoryDoc _) -> Directory
  (Insert, _) | Just _ <- edCompletion ed -> Completing
  (m, _) -> m

-- | A document that is open but not shown, with its scroll position.
data Buffered = Buffered
  { bufDoc :: !Document
  , bufView :: !View
  }
  deriving stock (Eq, Show)

-- | All buffers in order, and the index of the current one.
buffers :: Editor -> ([Buffered], Int)
buffers ed = (reverse (edBefore ed) <> [Buffered (edDoc ed) (edView ed)] <> edAfter ed, length (edBefore ed))

-- | Replace all buffers and make the @i@th current (clamped). An empty list
-- leaves a new scratch buffer.
setBuffers :: [Buffered] -> Int -> Editor -> Editor
setBuffers bs i ed = case splitAt j bs of
  (before, cur : after) ->
    ed {edBefore = reverse before, edDoc = bufDoc cur, edView = bufView cur, edAfter = after}
  _ -> scratch ed {edBefore = [], edAfter = []}
  where
    j = max 0 (min (length bs - 1) i)

-- | @(index, count)@ of the current buffer, counting from 0.
bufferIndex :: Editor -> (Int, Int)
bufferIndex ed = (length (edBefore ed), length (edBefore ed) + 1 + length (edAfter ed))

-- | Move to the buffer @n@ steps away, wrapping around.
switchBuffer :: Int -> Editor -> Editor
switchBuffer n ed = let (bs, i) = buffers ed in setBuffers bs ((i + n) `mod` length bs) ed

gotoBuffer :: Int -> Editor -> Editor
gotoBuffer i ed = setBuffers (fst (buffers ed)) i ed

-- | Open a document as a new buffer right after the current one, and show it.
openBuffer :: Document -> Editor -> Editor
openBuffer doc ed =
  ed
    { edBefore = Buffered (edDoc ed) (edView ed) : edBefore ed
    , edDoc = doc {docId = edNextId ed}
    , edView = initialView
    , edNextId = edNextId ed + 1
    }

-- | A new scratch document with a fresh id.
scratch :: Editor -> Editor
scratch ed = ed {edDoc = (newDocument Nothing Buffer.empty) {docId = edNextId ed}, edView = initialView, edNextId = edNextId ed + 1}

-- | Close the current buffer and show the next one (or the previous one if
-- it was the last). Closing the only buffer leaves a new scratch buffer.
closeBuffer :: Editor -> Editor
closeBuffer ed = case (edAfter ed, edBefore ed) of
  (next : after, _) -> ed {edDoc = bufDoc next, edView = bufView next, edAfter = after}
  ([], prev : before) -> ed {edDoc = bufDoc prev, edView = bufView prev, edBefore = before}
  ([], []) -> scratch ed

-- | Change the open document with this id, wherever it is in the buffers.
modifyDocument :: Int -> (Document -> Document) -> Editor -> Editor
modifyDocument i f ed
  | docId (edDoc ed) == i = ed {edDoc = f (edDoc ed)}
  | otherwise = ed {edBefore = map g (edBefore ed), edAfter = map g (edAfter ed)}
  where
    g b = if docId (bufDoc b) == i then b {bufDoc = f (bufDoc b)} else b

-- | Change every open document.
mapDocuments :: (Document -> Document) -> Editor -> Editor
mapDocuments f ed =
  ed
    { edDoc = f (edDoc ed)
    , edBefore = map g (edBefore ed)
    , edAfter = map g (edAfter ed)
    }
  where
    g b = b {bufDoc = f (bufDoc b)}

-- | What the picker's preview shows for an item: its file's title, and the
-- text with the line to centre on, or why there is none. 'Nothing' for
-- items that are not places (actions, code actions). An open buffer's own
-- text is used (with unsaved changes); other files come from the cache
-- ('edPreviews').
previewFor :: Editor -> PickTarget -> Maybe (Text, Either Text (Buffer, Int))
previewFor ed = \case
  PickBuffer i -> case drop i (fst (buffers ed)) of
    b : _ ->
      let d = bufDoc b
       in Just (maybe "[scratch]" T.pack (docPath d), Right (docBuffer d, posLine (rangeHead (primary (docSelection d)))))
    [] -> Nothing
  PickFile file -> place file 0
  PickPosition file line _ _ -> place file line
  _ -> Nothing
  where
    place file line = Just (T.pack file, maybe cached (\d -> Right (docBuffer d, line)) (openDoc file))
      where
        cached = case Map.lookup file (edPreviews ed) of
          Just (PreviewText b) -> Right (b, line)
          Just (PreviewNone why) -> Left why
          _ -> Left "loading…"
    openDoc file =
      case [d | b <- fst (buffers ed), let d = bufDoc b, docPath d == Just file || attachedTo file d] of
        d : _ -> Just d
        [] -> Nothing
    attachedTo file d = case docLsp d of
      LspAttached at -> atPath at == file
      _ -> False
