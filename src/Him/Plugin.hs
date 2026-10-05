-- | The plugin API (ADR-51): the one module a plugin imports. A plugin is
-- a 'PluginSpec': its actions, @:@ commands and keys, and what it does
-- when something happens ('Event'). Its code runs in 'PluginM', which can
--
-- * look at the editor: 'buffers', 'currentBuffer', 'bufferText',
--   'cursor', 'mode', 'windows', 'diagnostics', …
-- * change it: 'openFile', 'replaceRange', 'setCursor', 'runAction',
--   'notify', …
-- * show things: status line 'Segment's, gutter signs, 'Annotation's,
--   pickers, popups, scratch buffers. These are data the editor draws;
--   setting them again replaces them.
-- * run programs: 'spawn' (their output arrives as 'ProcessOutput').
-- * keep state of any type: 'getState', 'putState'.
--
-- "Him.Contrib.WordCount" and "Him.Contrib.RecentFiles" are examples.
module Him.Plugin
  ( -- * Plugins
    PluginSpec (..)
  , pluginSpec
  , hostPlugin
  , PluginM
  , apiVersion
  , pluginName
    -- * Actions and commands
  , PluginAction
  , action
  , actionWith
  , PluginCommand
  , command
  , ExArgs (..)
  , ArgSpec
  , int
  , text
  , choice
  , optional
    -- * Events
  , Event (..)
    -- * Settings
  , option
  , optionText
  , optionInt
  , optionBool
    -- * State
  , liftIO
  , stateFile
  , getState
  , putState
  , modifyState
    -- * Looking at the editor
  , BufferId
  , BufferInfo (..)
  , BufferKind (..)
  , WindowInfo (..)
  , buffers
  , currentBuffer
  , bufferInfo
  , bufferText
  , bufferLine
  , cursor
  , selections
  , mode
  , windows
  , options
  , Options (..)
  , Diagnostic
  , ShownDiagnostic (..)
  , Severity (..)
  , diagnostics
  , Mode (..)
  , Pos (..)
    -- * Changing it
  , notify
  , warn
  , openFile
  , focusBuffer
  , setCursor
  , replaceRange
  , runAction
    -- * Programs
  , spawn
  , sendInput
  , stopProcess
    -- * What it shows
  , Face (..)
  , face
  , Segment (..)
  , Side (..)
  , segment
  , setSegments
  , GutterSign (..)
  , SignSpan (..)
  , setSigns
  , Annotation (..)
  , setAnnotations
  , showPopup
  , openScratch
    -- * Pickers
  , PickerSpec (..)
  , Item (..)
  , Target (..)
  , openPicker
  , chosenItems
  , closePicker
  ) where

import Control.Monad.Trans.State.Strict (get, gets, modify')
import Data.IntMap.Strict qualified as IntMap
import Data.List (find, findIndex)
import Data.Map.Strict qualified as Map
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Typeable (Typeable)
import Him.Action (ArgSpec, choice, int, optional, text)
import Him.Action qualified as Action
import Him.Actions.File qualified as File
import Him.Actions.Jump (jumping)
import Him.Buffer qualified as Buffer
import Him.Document (DocKind (..), Document (..), clampSelection, changeDocument, displayName, isReadOnly, newDocument)
import Him.Editor hiding (Severity (..), buffers)
import Him.Editor qualified as Ed
import Him.EditorM qualified as EditorM
import Him.Effect (Effect (ProcessSend, ProcessStart, ProcessStop, RunAction))
import Him.Ex (ExArgs (..), ExCommand (..))
import Him.Invocation (parseInvocation)
import Him.Json (Value, asBool, asInt, asText)
import Control.Monad ((<=<))
import Control.Monad.IO.Class (liftIO)
import Data.Text qualified as T
import Him.Paths (stateDir)
import System.Directory (createDirectoryIfMissing)
import System.FilePath ((</>))
import Him.Jumplist (mapThroughChange)
import Him.Lsp.Protocol (Severity (..))
import Him.Lsp.State (ShownDiagnostic (..), shownDiagnosticsIn)
import Him.Mode (Mode (..))
import Him.Options (Options (..))
import Him.Picker qualified as Picker
import Him.Plugin.Internal (askCtx, liftEditor)
import Him.Plugin.Host (hostPlugin)
import Him.Plugin.Types
import Him.PluginEvent (Event (..))
import Him.PluginState (insertState, lookupState)
import Him.PluginUI
import Him.Position (Pos (..))
import Him.Selection (Range (..), point, primary, rangeHead, ranges, single)
import Him.Syntax (SyntaxInfo (..))
import Him.Window (Window (..))

-- | The name of the plugin the code runs for.
pluginName :: PluginM s Text
pluginName = ctxName <$> askCtx

-- * Actions and commands

-- | An action without arguments (bind keys to it by its name).
action :: Text -> Text -> PluginM s () -> PluginAction s
action name doc run = PluginAction $ \ctx -> Action.simple name Action.GPlugins doc (runPluginM ctx run)

-- | An action with arguments, parsed by an 'ArgSpec' (@int "count"@,
-- @text "name"@, …).
actionWith :: Text -> Text -> ArgSpec a -> (a -> PluginM s ()) -> PluginAction s
actionWith name doc spec run = PluginAction $ \ctx -> Action.action name Action.GPlugins doc spec (runPluginM ctx . run)

-- | A @:@ command: its names (the full one first), what it does, how its
-- arguments complete, and the code, given the arguments.
command :: [Text] -> Text -> ExArgs -> ([Text] -> PluginM s ()) -> PluginCommand s
command names doc args run = PluginCommand $ \ctx -> ExCommand names doc args (runPluginM ctx . run)

-- * Settings
-- | A setting from the plugin's @[plugins.<name>]@ table, as read.
option :: Text -> PluginM s (Maybe Value)
option key = do
  name <- pluginName
  liftEditor (gets (Map.lookup key <=< Map.lookup name . edPluginOptions))

-- | A setting, or a default when it is missing or of another type.
optionText :: Text -> Text -> PluginM s Text
optionText key def = fromMaybe def . (>>= asText) <$> option key

optionInt :: Text -> Int -> PluginM s Int
optionInt key def = fromMaybe def . (>>= asInt) <$> option key

optionBool :: Text -> Bool -> PluginM s Bool
optionBool key def = fromMaybe def . (>>= asBool) <$> option key

-- * State

-- | A file of the plugin's to keep things in between runs (the
-- directory is made; the file is the plugin's to read and write):
-- @<state dir>/plugins/<plugin>/<name>@ (see 'Him.Paths.stateDir').
stateFile :: FilePath -> PluginM s FilePath
stateFile file = do
  name <- pluginName
  liftIO $ do
    dir <- (</> ("plugins" </> T.unpack name)) <$> stateDir
    createDirectoryIfMissing True dir
    pure (dir </> file)

-- | The plugin's state ('psInitial' until it is put).
getState :: Typeable s => PluginM s s
getState = do
  ctx <- askCtx
  liftEditor (gets (fromMaybe (ctxInitial ctx) . lookupState (ctxName ctx) . edPluginStates))

putState :: Typeable s => s -> PluginM s ()
putState s = do
  name <- pluginName
  liftEditor (modify' (\e -> e {edPluginStates = insertState name s (edPluginStates e)}))

modifyState :: Typeable s => (s -> s) -> PluginM s ()
modifyState f = getState >>= putState . f

-- * Looking at the editor

info :: Document -> BufferInfo
info d =
  BufferInfo
    { biId = docId d
    , biPath = docPath d
    , biName = displayName d
    , biKind = case docKind d of
        TextDoc -> TextBuffer
        DirectoryDoc _ -> DirectoryBuffer
        ReplDoc _ -> ReplBuffer
        ChatDoc _ -> ChatBuffer
        ScratchDoc _ -> ScratchBuffer
    , biLanguage = siLanguage (docSyntax d)
    , biDirty = docDirty d
    , biLineCount = Buffer.lineCount (docBuffer d)
    , biVersion = docVersion d
    }

-- | The open buffers, in buffer order.
buffers :: PluginM s [BufferInfo]
buffers = map info <$> liftEditor (gets allDocuments)

-- | The buffer of the focused window.
currentBuffer :: PluginM s BufferInfo
currentBuffer = info <$> liftEditor EditorM.getDoc

bufferInfo :: BufferId -> PluginM s (Maybe BufferInfo)
bufferInfo i = fmap info <$> document i

document :: BufferId -> PluginM s (Maybe Document)
document i = liftEditor (gets (find ((== i) . docId) . allDocuments))

-- | A buffer's whole text ('Nothing': no such buffer).
bufferText :: BufferId -> PluginM s (Maybe Text)
bufferText i = fmap (Buffer.toText . docBuffer) <$> document i

-- | One line of a buffer (from 0), without its line break.
bufferLine :: BufferId -> Int -> PluginM s (Maybe Text)
bufferLine i l = (>>= lineOf) <$> document i
  where
    lineOf d
      | l >= 0 && l < Buffer.lineCount (docBuffer d) = Just (Buffer.lineAt l (docBuffer d))
      | otherwise = Nothing

-- | Where the cursor is in the focused window.
cursor :: PluginM s Pos
cursor = rangeHead . primary . docSelection <$> liftEditor EditorM.getDoc

-- | The focused window's selections: anchor and cursor of each.
selections :: PluginM s [(Pos, Pos)]
selections = map (\r -> (rangeAnchor r, rangeHead r)) . ranges . docSelection <$> liftEditor EditorM.getDoc

-- | The mode, as keys see it.
mode :: PluginM s Mode
mode = liftEditor (gets edMode)

windows :: PluginM s [WindowInfo]
windows = liftEditor $ do
  ed <- get
  pure (WindowInfo (edFocus ed) (docId (edDoc ed)) True : [WindowInfo w (winDoc win) False | (w, win) <- IntMap.toList (edWindows ed)])

-- | The settings (@[editor]@ in the config file).
options :: PluginM s Options
options = liftEditor (gets edOptions)

type Diagnostic = ShownDiagnostic

-- | A buffer's diagnostics from its language server, one per line they
-- cover, in character columns.
diagnostics :: BufferId -> PluginM s [Diagnostic]
diagnostics i =
  document i >>= \case
    Nothing -> pure []
    Just d -> do
      lsp <- liftEditor (gets edLsp)
      pure (shownDiagnosticsIn lsp (docLsp d) (docBuffer d) 0 (Buffer.lineCount (docBuffer d)))

-- * Changing it

-- | A message in the bottom row, until the next key.
notify :: Text -> PluginM s ()
notify = liftEditor . EditorM.info

-- | An error message in the bottom row.
warn :: Text -> PluginM s ()
warn = liftEditor . EditorM.failWith

-- | Open a file (or go to its buffer), as a jump.
openFile :: FilePath -> PluginM s ()
openFile = liftEditor . jumping . File.openFile

-- | Show a buffer in the focused window.
focusBuffer :: BufferId -> PluginM s ()
focusBuffer i = liftEditor $ do
  (bs, _) <- gets Ed.buffers
  mapM_ (modify' . gotoBuffer) (findIndex ((== i) . docId . bufDoc) bs)

-- | Put the cursor (one, no selection) somewhere in the focused window.
setCursor :: Pos -> PluginM s ()
setCursor p = liftEditor (EditorM.modifyDoc (\d -> d {docSelection = clampSelection (docBuffer d) (single (point p))}))

-- | Replace the text between two positions of a buffer (half open) with
-- other text: one change for undo. Its selections move with the text.
replaceRange :: BufferId -> Pos -> Pos -> Text -> PluginM s ()
replaceRange i from to new =
  document i >>= \case
    Nothing -> warn "no such buffer"
    Just d
      | isReadOnly d -> warn "that buffer is read-only"
      | otherwise ->
          let (a, b) = (min from to, max from to)
              old = docBuffer d
              buf = fst (Buffer.insertText a new (Buffer.deleteRange a b old))
              sel = clampSelection buf (mapThroughChange (a, b, new) (docSelection d))
           in liftEditor (modify' (modifyDocument i (\doc -> (changeDocument buf sel doc) {docDirty = True})))

-- | Run any action by its invocation (@"goto_line 10"@), after this code.
runAction :: Text -> PluginM s ()
runAction inv = case parseInvocation inv of
  Left e -> warn e
  Right parsed -> liftEditor (EditorM.request (RunAction parsed))

-- * Programs

-- | Start a program (by name, for this plugin; one running under the
-- name is stopped first): command, arguments, directory. Each line it
-- prints arrives as 'ProcessOutput', then 'ProcessExited'.
spawn :: Text -> FilePath -> [String] -> Maybe FilePath -> PluginM s ()
spawn name cmd args dir = do
  key <- processKey name
  liftEditor (EditorM.request (ProcessStart key cmd args dir))

sendInput :: Text -> Text -> PluginM s ()
sendInput name t = processKey name >>= \key -> liftEditor (EditorM.request (ProcessSend key t))

stopProcess :: Text -> PluginM s ()
stopProcess name = processKey name >>= liftEditor . EditorM.request . ProcessStop

processKey :: Text -> PluginM s Text
processKey name = (\p -> p <> ":" <> name) <$> pluginName

-- * What it shows

ui :: (PluginUI -> PluginUI) -> PluginM s ()
ui f = pluginName >>= \name -> liftEditor (modify' (modifyPluginUI name f))

-- | The plugin's status line segments (replacing the ones it had).
setSegments :: [Segment] -> PluginM s ()
setSegments segs = ui (\u -> u {puSegments = segs})

-- | The plugin's gutter signs in a buffer (set 'psSigns' too).
setSigns :: BufferId -> [SignSpan] -> PluginM s ()
setSigns i spans = ui (\u -> u {puSigns = if null spans then IntMap.delete i (puSigns u) else IntMap.insert i spans (puSigns u)})

-- | The plugin's annotations in a buffer: text after the ends of lines.
setAnnotations :: BufferId -> [Annotation] -> PluginM s ()
setAnnotations i anns = ui (\u -> u {puAnnotations = if null anns then IntMap.delete i (puAnnotations u) else IntMap.insert i anns (puAnnotations u)})

-- | A box with a title and rows (a name and a description), until the
-- next key.
showPopup :: Text -> [(Text, Text)] -> PluginM s ()
showPopup title rows = liftEditor (modify' (\e -> e {edPopup = Just (InfoBox title rows BottomRight Nothing)}))

-- | Show text in a read-only buffer of this name (made the first time,
-- its text replaced after), in the focused window.
openScratch :: Text -> Text -> PluginM s BufferId
openScratch name t = liftEditor $ do
  existing <- gets (find ((== ScratchDoc name) . docKind) . allDocuments)
  case existing of
    Just d -> do
      modify' (modifyDocument (docId d) (\doc -> changeDocument (Buffer.fromText t) (single (point (Pos 0 0))) doc))
      (bs, _) <- gets Ed.buffers
      mapM_ (modify' . gotoBuffer) (findIndex ((== docId d) . docId . bufDoc) bs)
      pure (docId d)
    Nothing -> do
      modify' (openBuffer (newDocument Nothing (Buffer.fromText t)) {docKind = ScratchDoc name})
      gets (docId . edDoc)

-- * Pickers

-- | Show a picker; its actions get the chosen items with 'chosenItems'.
openPicker :: PickerSpec -> PluginM s ()
openPicker spec =
  liftEditor . EditorM.openPicker $
    (Picker.newPicker (pickerTitle spec) [Picker.pickerItem (itemLabel it) (target (itemTarget it)) (itemDetail it) | it <- pickerItems spec])
      { Picker.pkPrimary = pickerPrimary spec
      , Picker.pkSecondary = pickerSecondary spec
      }
  where
    target = \case
      TargetValue v -> Picker.PickValue v
      TargetFile f -> Picker.PickFile f
      TargetPosition f l c -> Picker.PickPosition f l c Nothing

-- | The open picker's marked items, or else its selected one (empty when
-- no picker is open).
chosenItems :: PluginM s [Item]
chosenItems = liftEditor (gets (maybe [] (concatMap item . Picker.chosenItems) . edPicker))
  where
    item it = case Picker.piTarget it of
      Picker.PickValue v -> [Item (Picker.piLabel it) (Picker.piDetail it) (TargetValue v)]
      Picker.PickFile f -> [Item (Picker.piLabel it) (Picker.piDetail it) (TargetFile f)]
      Picker.PickPosition f l c _ -> [Item (Picker.piLabel it) (Picker.piDetail it) (TargetPosition f l c)]
      _ -> []

closePicker :: PluginM s ()
closePicker = liftEditor (modify' (\e -> e {edPicker = Nothing, edMode = Normal, edPreviews = Map.empty}))
