-- | The complete, pure editor state.
module Him.Editor
  ( Editor (..)
  , Status (..)
  , Severity (..)
  , PromptKind (..)
  , FileAction (..)
  , InfoBox (..)
  , CmdCompletions (..)
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
  , modifyPluginUI
    -- * Windows
  , windowBoxes
  , focusedTextHeight
  , focusWindow
  , splitWindow
  , closeWindow
  , onlyWindow
  , swapWindow
  , windowEditor
  , windowShowing
  , reviewFor
  , allDocuments
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
import Him.PluginEvent (Seen, unseen)
import Him.PluginUI (OpenCanvas, PluginUI, emptyPluginUI)
import Him.PluginState (PluginStates, noStates)
import Him.Json (Value)
import Him.Jumplist (Jump (..), Jumplist (..))
import Data.Sequence qualified as Seq
import Him.Search (Direction)
import Him.KeyHints (KeyHints, noHints)
import Him.Syntax (SyntaxInfo (..))
import Him.Syntax.Span (LineSpan)
import Him.Selection (Selection, primary, rangeHead)
import Him.Buffer (Buffer)
import Him.Buffer qualified as Buffer
import Him.Document (DocKind (..), Document (..), clampSelection, displayName, newDocument)
import Him.Chat (ChatState (..), Review (..))
import Him.View (View, initialView)
import Him.Window
import Data.IntMap.Strict (IntMap)
import Data.IntMap.Strict qualified as IntMap
import Data.List (find, findIndex)
import Data.Maybe (fromMaybe)

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

-- | File operations in a directory listing ("Him.Actions.Directory").
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
  , infoSelected :: !(Maybe Int)
  -- ^ The highlighted row, if any.
  }
  deriving stock (Eq, Show)

-- | The candidates of a @tab@ on the command line. Further @tab@s (and
-- @S-tab@) cycle through them, as in Helix.
data CmdCompletions = CmdCompletions
  { ccBefore :: !Text
  -- ^ The line before the completed word.
  , ccCandidates :: ![Text]
  -- ^ What each candidate puts after 'ccBefore'.
  , ccShown :: ![Text]
  -- ^ The candidates as listed (a path's last component).
  , ccSelected :: !(Maybe Int)
  -- ^ The candidate on the line; none before the first cycle.
  }
  deriving stock (Eq, Show)

-- | What a command waiting for a key will do with it.
data Await
  = -- | Find a character: forward?, till?, count.
    AwaitFind !Bool !Bool !Int
  | -- | Match mode (@m@): the character of @m s@, @m d@, @m r@ (its
    -- first and, with that given, its second), or the object of @m i@ /
    -- @m a@ (inside?).
    AwaitSurround
  | AwaitDeleteSurround
  | AwaitReplaceSurround
  | AwaitReplaceSurroundWith !Char
  | AwaitObject !Bool
  | -- | The character @r@ replaces each selected one with.
    AwaitReplaceChar
  | -- | The register for the next command (@\"@), or to insert (@C-r@ in
    -- insert mode).
    AwaitRegister
  | AwaitInsertRegister
  deriving stock (Eq, Show)

-- | A file's text for the picker's preview.
data Preview
  = PreviewLoading
  | -- | The text, and its highlighting once a provider made it.
    PreviewText !Buffer !(IntMap [LineSpan])
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
  -- ^ The settings (@[editor]@ in the config file).
  , edSignLane :: !Bool
  -- ^ The gutter has a sign lane (an enabled plugin draws there).
  , edLayout :: !Layout
  -- ^ How the screen is split into windows (ADR window-splits).
  , edFocus :: !Int
  -- ^ The focused window: it shows 'edDoc' through 'edView'.
  , edWindows :: !(IntMap Window)
  -- ^ The other windows.
  , edInfo :: !(Maybe InfoBox)
  , edCompletions :: !(Maybe CmdCompletions)
  -- ^ Candidates from the last @tab@ on the command line, shown until the
  -- line is edited.
  , edRegisters :: !(Map Char [Text])
  -- ^ Registers, one value per range: @\"@ (the default), @/@ (the last
  -- search), any other character yanked to (@\"a y@), and the last
  -- values copied to or pasted from @+@ and @*@ (the clipboard,
  -- "Him.Actions.Register").
  , edSelectedRegister :: !(Maybe Char)
  -- ^ Chosen with @\"@ for the next command only.
  , edJumps :: !(IntMap Jumplist)
  -- ^ Each window's jumplist, by window id (ADR jumplist).
  , edJumpTexts :: !(IntMap (Int, Buffer))
  -- ^ For each document with jumps: the version and text their positions
  -- refer to, so they can follow later edits.
  , edPluginUI :: !(Map Text PluginUI)
  -- ^ What each plugin shows, by plugin name (ADR plugin-building-blocks).
  , edKeyHints :: !KeyHints
  -- ^ Which keys run which action in the running config, for texts that
  -- name keys ("Him.KeyHints"; set by "Him.Session").
  , edCanvas :: !(Maybe OpenCanvas)
  -- ^ A plugin's canvas over everything; it has the keys (ADR plugin-canvas).
  , edPluginOptions :: !(Map Text (Map Text Value))
  -- ^ Plugins' settings (@[plugins.<name>]@), by plugin (ADR plugin-api).
  , edPluginStates :: !PluginStates
  -- ^ Each plugin's own state ("Him.PluginState", ADR plugin-api).
  , edSeen :: !Seen
  -- ^ What plugin events have been raised for ("Him.PluginEvent").
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
    , edLayout = Leaf 0
    , edFocus = 0
    , edWindows = IntMap.empty
    , edInfo = Nothing
    , edCompletions = Nothing
    , edRegisters = Map.empty
    , edSelectedRegister = Nothing
    , edJumps = IntMap.empty
    , edJumpTexts = IntMap.empty
    , edPluginUI = Map.empty
    , edCanvas = Nothing
    , edKeyHints = noHints
    , edPluginOptions = Map.empty
    , edPluginStates = noStates
    , edSeen = unseen
    , edQuit = False
    }

-- | The mode whose keymap applies: normal mode in a directory listing uses
-- the 'Directory' layer, insert mode with the completion menu open the
-- 'Completing' one.
keymapMode :: Editor -> Mode
keymapMode ed = case (edMode ed, docKind (edDoc ed)) of
  (Normal, DirectoryDoc _) -> Directory
  (Insert, _) | Just _ <- edCompletion ed -> Completing
  (Insert, ReplDoc _) -> Repl
  (Insert, ChatDoc _) -> Chat
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
-- text with the line to centre on and its highlighting (what is known of
-- it), or why there is none. 'Nothing' for
-- items that are not places (actions, code actions). An open buffer's own
-- text is used (with unsaved changes); other files come from the cache
-- ('edPreviews').
previewFor :: Editor -> PickTarget -> Maybe (Text, Either Text (Buffer, Int, IntMap [LineSpan]))
previewFor ed = \case
  PickBuffer i -> case drop i (fst (buffers ed)) of
    b : _ ->
      let d = bufDoc b
       in Just (maybe "[scratch]" T.pack (docPath d), Right (docBuffer d, posLine (rangeHead (primary (docSelection d))), spansOf d))
    [] -> Nothing
  PickFile file -> place file 0
  PickPosition file line _ _ -> place file line
  PickJump i -> do
    j <- IntMap.lookup (edFocus ed) (edJumps ed) >>= Seq.lookup i . jlJumps
    d <- find ((== jumpDoc j) . docId) (allDocuments ed)
    pure (displayName d, Right (docBuffer d, posLine (rangeHead (primary (jumpSelection j))), spansOf d))
  _ -> Nothing
  where
    spansOf d = siSpans (docSyntax d)
    place file line = Just (T.pack file, maybe cached (\d -> Right (docBuffer d, line, spansOf d)) (openDoc file))
      where
        cached = case Map.lookup file (edPreviews ed) of
          Just (PreviewText b spans) -> Right (b, line, spans)
          Just (PreviewNone why) -> Left why
          _ -> Left "loading…"
    openDoc file =
      case [d | b <- fst (buffers ed), let d = bufDoc b, docPath d == Just file || attachedTo file d] of
        d : _ -> Just d
        [] -> Nothing
    attachedTo file d = case docLsp d of
      LspAttached at -> atPath at == file
      _ -> False

-- * Windows (ADR window-splits)

-- | Every open document, the current one included.
allDocuments :: Editor -> [Document]
allDocuments = map bufDoc . fst . buffers

-- | Where each window is: the screen above the command line, divided.
windowBoxes :: Editor -> [(Int, Box)]
windowBoxes ed = boxes (Box 0 0 (max 0 (rows - 1)) cols) (edLayout ed)
  where
    (rows, cols) = edSize ed

-- | Text rows of the focused window (its box less the status line).
focusedTextHeight :: Editor -> Int
focusedTextHeight ed = case lookup (edFocus ed) (windowBoxes ed) of
  Just b -> max 1 (boxHeight b - 1)
  Nothing -> max 1 (fst (edSize ed) - 2)

-- | Focus a window: the focused one is put away with its view and
-- selection, and the other's document becomes current, scrolled and
-- selected as that window left it.
focusWindow :: Int -> Editor -> Editor
focusWindow w ed = case IntMap.lookup w (edWindows ed) of
  Just win | w /= edFocus ed -> showWindow win ed {edFocus = w, edWindows = IntMap.delete w (IntMap.insert (edFocus ed) (current ed) (edWindows ed))}
  _ -> ed

-- | The focused window as a 'Window'.
current :: Editor -> Window
current ed = Window (docId (edDoc ed)) (edView ed) (docSelection (edDoc ed))

-- | Make a window's document current (if it is still open), with its view
-- and selection.
showWindow :: Window -> Editor -> Editor
showWindow win ed = case findIndex ((== winDoc win) . docId) (allDocuments ed) of
  Nothing -> ed
  Just i ->
    let ed' = gotoBuffer i ed
        d = edDoc ed'
     in ed' {edDoc = d {docSelection = clampSelection (docBuffer d) (winSelection win)}, edView = winView win}

-- | Split the focused window: the new one shows the same document, in the
-- same place, and gets the focus.
splitWindow :: Axis -> Editor -> Editor
splitWindow axis ed =
  ed
    { edLayout = insertBeside axis (edFocus ed) new (edLayout ed)
    , edWindows = IntMap.insert (edFocus ed) (current ed) (edWindows ed)
    , edFocus = new
    , edNextId = new + 1
    }
  where
    new = edNextId ed

-- | Close the focused window and focus the one before it (or after it,
-- for the first). 'Nothing' when it is the only one.
closeWindow :: Editor -> Maybe Editor
closeWindow ed = case break (== edFocus ed) (leaves (edLayout ed)) of
  (before, _ : after) -> case reverse before <> after of
    next : _ -> Just (closeTo next)
    [] -> Nothing
  _ -> Nothing
  where
    closeTo next =
      let ed' = showWindow (fromMaybe (current ed) (IntMap.lookup next (edWindows ed))) ed
       in ed'
            { edLayout = removeWindow (edFocus ed) (edLayout ed)
            , edFocus = next
            , edWindows = IntMap.delete next (edWindows ed)
            }

-- | Close every window but the focused one.
onlyWindow :: Editor -> Editor
onlyWindow ed = ed {edLayout = Leaf (edFocus ed), edWindows = IntMap.empty}

-- | Swap the focused window with its neighbour on a side (the focus moves
-- with it).
swapWindow :: Side -> Editor -> Editor
swapWindow side ed = case neighbour side (edFocus ed) (windowBoxes ed) of
  Just other -> ed {edLayout = swapWindows (edFocus ed) other (edLayout ed)}
  Nothing -> ed

-- | The editor as an unfocused window shows it: its document, view and
-- selection, in normal mode, without popups. For drawing it with the
-- same components as the focused one.
windowEditor :: Editor -> Int -> Editor
windowEditor ed w = case IntMap.lookup w (edWindows ed) of
  Nothing -> ed
  Just win ->
    let shown = maybe ed (`gotoBuffer` ed) (findIndex ((== winDoc win) . docId) (allDocuments ed))
        d = edDoc shown
     in shown
          { edDoc = d {docSelection = clampSelection (docBuffer d) (winSelection win)}
          , edView = winView win
          , edMode = Normal
          , edPicker = Nothing
          , edInfo = Nothing
          , edPopup = Nothing
          , edCompletion = Nothing
          , edPending = []
          , edCount = Nothing
          , edStatus = Nothing
          , edFocus = w
          }

-- | The window showing a document, if any (the focused one first).
windowShowing :: Int -> Editor -> Maybe Int
windowShowing i ed
  | docId (edDoc ed) == i = Just (edFocus ed)
  | otherwise = case [w | (w, win) <- IntMap.toList (edWindows ed), winDoc win == i] of
      w : _ -> Just w
      [] -> Nothing

-- | A document's review of proposed chat changes, if it has one (ADR change-review).
reviewFor :: Editor -> Int -> Maybe Review
reviewFor ed i = case [rv | d <- allDocuments ed, ChatDoc cs <- [docKind d], rv <- csReviews cs, rvDoc rv == i] of
  rv : _ -> Just rv
  [] -> Nothing

-- | Change what a plugin shows (ADR plugin-building-blocks).
modifyPluginUI :: Text -> (PluginUI -> PluginUI) -> Editor -> Editor
modifyPluginUI name f e = e {edPluginUI = Map.insert name (f (Map.findWithDefault emptyPluginUI name (edPluginUI e))) (edPluginUI e)}
