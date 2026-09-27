-- | The main loop: read an event, resolve it through the keymap of the
-- current mode, run the command, render, repeat.
module Him.App
  ( run
  , handleEvent
  ) where

import Control.Concurrent (Chan, newChan, readChan, writeChan)
import Control.Exception (SomeException, try)
import Control.Monad (unless, when)
import Control.Monad.Trans.State.Strict (execStateT, get, modify')
import Data.Map.Strict qualified as Map
import Data.Maybe (fromMaybe)
import Data.Text qualified as T
import Him.Buffer qualified as Buffer
import Him.Command (cmdRun, failWith)
import Him.Command qualified as Command
import Him.Config (Config (..))
import Him.Config.Default (defaultConfig)
import Him.Document (newDocument)
import Him.Editor
import Him.Event (Event (..))
import Him.File (loadDocument)
import Him.Keymap (Resolved (..), emptyKeymap, resolve)
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

-- | Run the editor, optionally opening the given file.
run :: Maybe FilePath -> IO ()
run file = do
  config <- either (die . T.unpack) pure defaultConfig
  -- Load before entering raw mode, so errors print normally.
  doc <- case file of
    Nothing -> pure (newDocument Nothing Buffer.empty)
    Just path -> loadDocument path >>= either (die . T.unpack) pure
  logMsg ("starting, file = " <> show file)
  withRawTerminal $ do
    events <- newChan
    size <- fromMaybe (24, 80) <$> getWindowSize
    onResize (writeChan events . uncurry EvResize)
    startInputReader events
    eventLoop config events (newEditor size doc)

eventLoop :: Config -> Chan Event -> Editor -> IO ()
eventLoop config events = go Nothing
  where
    go :: Maybe Frame -> Editor -> IO ()
    go prev ed0 = do
      let ed = ensureCursorVisible ed0
          frame = render defaultTheme ed
      writeOutput (diffFrames prev frame)
      ev <- readChan events
      -- A bug in a command should not take the editor (and unsaved work)
      -- down with it.
      next <-
        try (execStateT (handleEvent config ev) ed) >>= \case
          Right ed' -> pure ed'
          Left e -> do
            logMsg ("command failed: " <> show (e :: SomeException))
            execStateT (failWith ("internal error: " <> T.pack (show e))) ed
      unless (edQuit next) (go (Just frame) next)

handleEvent :: Config -> Event -> Command.EditorM ()
handleEvent _ (EvResize rows cols) = modify' (\e -> e {edSize = (rows, cols)})
handleEvent config (EvKey key) = do
  ed <- get
  let pending = edPending ed
      keys = pending <> [key]
      keymap = Map.findWithDefault emptyKeymap (edMode ed) (cfgKeymaps config)
      setPending ks = modify' (\e -> e {edPending = ks})
  when (null pending) $ modify' (\e -> e {edStatus = Nothing})
  case resolve keymap keys of
    NeedMore -> setPending keys
    Found name -> do
      setPending []
      maybe (failWith ("unknown command: " <> name)) cmdRun (Map.lookup name (cfgRegistry config))
    NoMatch -> do
      setPending []
      -- Only a key typed on its own falls back (a failed chord is dropped).
      when (null pending) $ sequence_ (cfgFallback config (edMode ed) key)
