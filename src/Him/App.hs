-- | The main loop: read an event, resolve it through the keymap of the
-- current mode, run the bound action, render, repeat.
module Him.App
  ( run
  , handleEvent
  ) where

import Control.Concurrent.STM (TChan, atomically, newTChanIO, readTChan, tryReadTChan, writeTChan)
import Control.Exception (SomeException, try)
import Control.Monad (unless, when)
import Control.Monad.Trans.State.Strict (execStateT, get, gets, modify')
import Data.Map.Strict qualified as Map
import Data.Char (digitToInt, isDigit)
import Data.Maybe (fromMaybe, isJust)
import Data.Set qualified as Set
import Data.Text qualified as T
import Him.Buffer qualified as Buffer
import Him.Action (Bound (..))
import Him.Command (failWith)
import Him.Command qualified as Command
import Him.Config (Config (..))
import Him.Commands.Search (refreshSearchPreview)
import Him.Config.Default (defaultConfig)
import Him.Document (Document (..), newDocument)
import Him.History qualified as History
import Him.Mode (Mode (..))
import Him.Editor
import Him.Event (Event (..))
import Him.File (loadDocument)
import Him.Key (Key (..), KeyCode (..))
import Him.Keymap (Keymap, Resolved (..), emptyKeymap, resolve)
import Him.Log (logMsg)
import Him.Render (ensureCursorVisible, render)
import Him.Render.Diff (diffFrames)
import Him.Render.Frame (Frame)
import Him.Render.Theme (defaultTheme)
import Him.Terminal.Input (startInputReader)
import Him.Terminal.Output (writeOutput)
import Him.Terminal.Raw (withRawTerminal)
import Him.Terminal.Size (getWindowSize, onResize)
import System.Exit (die)

-- | Run the editor, opening the given files (the first one is shown).
run :: [FilePath] -> IO ()
run files = do
  config <- either (die . T.unpack) pure defaultConfig
  -- Load before entering raw mode, so errors print normally.
  docs <- traverse (\path -> loadDocument path >>= either (die . T.unpack) pure) files
  logMsg ("starting, files = " <> show files)
  withRawTerminal $ do
    events <- newTChanIO
    size <- fromMaybe (24, 80) <$> getWindowSize
    onResize (atomically . writeTChan events . uncurry EvResize)
    startInputReader events
    eventLoop config events (openAll size docs)

-- | An editor showing the first document, with the others open behind it.
openAll :: (Int, Int) -> [Document] -> Editor
openAll size docs = case docs of
  [] -> newEditor size (newDocument Nothing Buffer.empty)
  d : ds -> gotoBuffer 0 (foldl (flip openBuffer) (newEditor size d) ds)

-- | Most events that can be handled before one render. Typeahead (a paste,
-- key repeat, a fast typist) is handled first and drawn once, the way Vim
-- does. The limit keeps the screen updating during a very long burst.
maxBatch :: Int
maxBatch = 512

eventLoop :: Config -> TChan Event -> Editor -> IO ()
eventLoop config events = go Nothing
  where
    go :: Maybe Frame -> Editor -> IO ()
    go prev ed0 = do
      let ed = ensureCursorVisible (refreshSearchPreview ed0)
          frame = render defaultTheme prev ed
      writeOutput (diffFrames prev frame)
      next <- atomically (readTChan events) >>= batch maxBatch ed
      unless (edQuit next) (go (Just frame) next)

    -- Handle an event, then whatever else is already queued.
    batch :: Int -> Editor -> Event -> IO Editor
    batch n ed ev = do
      ed' <- step ed ev
      if edQuit ed' || n <= 1
        then pure ed'
        else
          atomically (tryReadTChan events) >>= \case
            Nothing -> pure ed'
            Just ev' -> batch (n - 1) (ensureCursorVisible ed') ev'

    step :: Editor -> Event -> IO Editor
    step ed ev =
      -- A bug in a command should not take the editor (and unsaved work)
      -- down with it.
      try (execStateT (handleEvent config ev) ed) >>= \case
        Right ed' -> pure ed'
        Left e -> do
          logMsg ("command failed: " <> show (e :: SomeException))
          execStateT (failWith ("internal error: " <> T.pack (show e))) ed

handleEvent :: Config -> Event -> Command.EditorM ()
handleEvent _ (EvResize rows cols) = modify' (\e -> e {edSize = (rows, cols)})
handleEvent config (EvKey key) = do
  ed <- get
  let pending = edPending ed
      keys = pending <> [key]
      keymap = Map.findWithDefault emptyKeymap (edMode ed) (cfgKeymaps config)
      setPending ks = modify' (\e -> e {edPending = ks})
      clearCount = modify' (\e -> e {edCount = Nothing})
  when (null pending) $ modify' (\e -> e {edStatus = Nothing})
  case countDigit ed keymap key of
    Just n -> modify' (\e -> e {edCount = Just n})
    Nothing -> case resolve keymap keys of
      NeedMore -> setPending keys
      Found bound -> do
        setPending []
        clearCount
        case (edCount ed, boundCounted bound) of
          (Just n, Just counted) -> counted n
          _ -> boundRun bound
      NoMatch -> do
        setPending []
        clearCount
        -- Only a key typed on its own falls back (a failed chord is dropped).
        when (null pending) $ sequence_ (cfgFallback config (edMode ed) key)
  commitOutsideInsert

-- | A digit typed before a key sequence, in normal or select mode, adds to
-- the count (@1 2 j@ moves 12 lines). @0@ only continues a count, and a
-- digit the keymap binds keeps its binding.
countDigit :: Editor -> Keymap Bound -> Key -> Maybe Int
countDigit ed keymap key = case key of
  Key (KChar c) mods
    | Set.null mods
    , isDigit c
    , null (edPending ed)
    , edMode ed `elem` [Normal, Select]
    , c /= '0' || isJust (edCount ed)
    , NoMatch <- resolve keymap [key] ->
        Just (min maxCount (maybe 0 (* 10) (edCount ed) + digitToInt c))
  _ -> Nothing
  where
    maxCount = 1000000

-- | Once the editor is out of insert mode, the edits made since the last
-- commit become one undo step. A whole insert session therefore undoes at
-- once.
commitOutsideInsert :: Command.EditorM ()
commitOutsideInsert = do
  mode <- gets edMode
  unless (mode == Insert) $
    Command.modifyDoc (\d -> d {docHistory = History.commit (docHistory d)})
