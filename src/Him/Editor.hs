-- | The complete, pure editor state.
module Him.Editor
  ( Editor (..)
  , Status (..)
  , Severity (..)
  , PromptKind (..)
  , FileAction (..)
  , InfoBox (..)
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
  ) where

import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Text (Text)
import Him.Key (Key)
import Him.Mode (Mode (..))
import Him.Picker (Picker)
import Him.Search (Direction)
import Him.Selection (Selection)
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

-- | Which corner of the text area the box sits in.
data InfoPlace = BottomLeft | BottomRight
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
  , edPicker :: !(Maybe Picker)
  -- ^ The open picker, shown in 'Picking' mode.
  , edShowHidden :: !Bool
  -- ^ Directory listings show dotfiles (@g .@ toggles).
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
    { edDoc = doc
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
    , edPicker = Nothing
    , edShowHidden = False
    , edInfo = Nothing
    , edCompletions = []
    , edRegisters = Map.empty
    , edQuit = False
    }

-- | The mode whose keymap applies: normal mode in a directory listing
-- uses the 'Directory' layer.
keymapMode :: Editor -> Mode
keymapMode ed = case (edMode ed, docKind (edDoc ed)) of
  (Normal, DirectoryDoc _) -> Directory
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
  _ -> ed {edBefore = [], edDoc = newDocument Nothing Buffer.empty, edView = initialView, edAfter = []}
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
    , edDoc = doc
    , edView = initialView
    }

-- | Close the current buffer and show the next one (or the previous one if
-- it was the last). Closing the only buffer leaves a new scratch buffer.
closeBuffer :: Editor -> Editor
closeBuffer ed = case (edAfter ed, edBefore ed) of
  (next : after, _) -> ed {edDoc = bufDoc next, edView = bufView next, edAfter = after}
  ([], prev : before) -> ed {edDoc = bufDoc prev, edView = bufView prev, edBefore = before}
  ([], []) -> ed {edDoc = newDocument Nothing Buffer.empty, edView = initialView}
