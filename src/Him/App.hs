-- | The terminal frontend: raw mode, input, the main loop (batching events,
-- rendering, the effects only the loop can carry out) and the theme. The
-- event handling itself is "Him.Session".
module Him.App
  ( run
  , runWith
  , handleEvent
  ) where

import Control.Concurrent.STM (TChan, atomically, newTChanIO, readTChan, tryReadTChan, writeTChan)
import Control.Exception (SomeException, try)
import Control.Monad (foldM, unless)
import Control.Monad.Trans.State.Strict (execStateT)
import Data.Maybe (fromMaybe)
import Data.Set qualified as Set
import Data.Text qualified as T
import Him.Effect (Effect (..))
import Him.Ex (previewedTheme)
import Him.EditorM (failWith)
import Him.Config (Config (..), Plugin (..))
import Him.Actions.Search (refreshSearchPreview)
import Him.Config.Default (plugins)
import Him.UserConfig (UserConfig (..), applyEditorOptions, applyUserConfigWithIn, userOptions)
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import Him.Runtime (Runtime, newRuntime)
import Him.Runtime qualified as Runtime
import Him.Options (Options (..))
import Him.Editor
import Him.Event (Event (..))
import Him.Directory (loadPath)
import Him.Log (logMsg)
import Him.Render (ensureCursorVisible, render)
import Him.Render.Diff (diffFrames)
import Him.Render.Frame (Frame)
import Him.Render.Theme (Theme (..), defaultTheme)
import Him.Theme.Load (hasTrueColor, loadTheme)
import Control.Applicative ((<|>))
import Him.Terminal.Input (startInputReader)
import Him.Terminal.Output (writeOutput)
import Him.Terminal.Raw (withRawTerminal)
import Him.Terminal.Size (getWindowSize, onResize)
import System.Exit (die)
import Him.Session

-- | Run the editor, opening the given files (the first one is shown).
run :: [FilePath] -> IO ()
run = runWith plugins Nothing

-- | The same, for a build with these plugins: the built-in ones and
-- contrib, then a personal build's own ("Him.Main", ADR personal-builds); with a
-- message to show at the start.
runWith :: [Plugin] -> Maybe T.Text -> [FilePath] -> IO ()
runWith every notice files = do
  -- The user's config; a broken one starts the defaults and says why.
  (userConfig, config, problem) <- loadConfigWith every
  trueColor <- hasTrueColor
  (theme, themeProblem) <- either (\e -> (defaultTheme, Just e)) (\t -> (t, Nothing)) <$> themeOf trueColor userConfig
  -- Load before entering raw mode, so errors print normally.
  docs <- traverse (\path -> loadPath False path >>= either (die . T.unpack) pure) files
  logMsg ("starting, files = " <> show files)
  withRawTerminal $ \suspend -> do
    events <- newTChanIO
    size <- fromMaybe (24, 80) <$> getWindowSize
    onResize (atomically . writeTChan events . uncurry EvResize)
    -- The escape timeout is read once, here (a reload does not change it).
    startInputReader (optEscapeTimeout (userOptions userConfig)) events
    runtime <- newRuntime config (atomically . writeTChan events)
    -- Start what the first document needs (its git state) before any key.
    let opened = applyEditorOptions userConfig (openAll size docs)
    start <- execStateT (housekeeping config) (withPlugins config opened) {edStatus = Status Error <$> (problem <|> themeProblem <|> notice)}
    mapM_ (Runtime.perform runtime) (edEffects start)
    configRef <- newIORef config
    userRef <- newIORef userConfig
    themeRef <- newIORef theme
    previewRef <- newIORef (Nothing, Nothing)
    shownRef <- newIORef Nothing
    eventLoop (Loop configRef userRef themeRef trueColor previewRef shownRef every) runtime suspend events start {edEffects = []}
    Runtime.shutdown runtime

-- | The theme a config names (@[editor] theme@), or the built-in one.
-- Warnings about parts of the theme that were skipped are logged.
themeOf :: Bool -> UserConfig -> IO (Either T.Text Theme)
themeOf trueColor uc = loadNamedTheme trueColor (fromMaybe "default" (ucTheme uc))

loadNamedTheme :: Bool -> T.Text -> IO (Either T.Text Theme)
loadNamedTheme trueColor name =
  loadTheme trueColor name >>= \case
    Left e -> pure (Left ("theme: " <> e))
    Right (theme, warnings) -> do
      mapM_ (\w -> logMsg ("theme " <> T.unpack name <> ": " <> T.unpack w)) warnings
      pure (Right theme)

-- | What the loop keeps besides the editor: the config, the user's config
-- it was made from (to make it again with other plugins), the theme (all
-- replaced by :config-reload, :plugin-*, :theme), and whether the terminal
-- shows 24-bit colour. Then the theme previewed on the : line (the name
-- and, if it loaded, the theme), the name of the theme last drawn, and
-- every plugin of this build.
data Loop = Loop (IORef Config) (IORef UserConfig) (IORef Theme) Bool (IORef (Maybe T.Text, Maybe Theme)) (IORef (Maybe T.Text)) [Plugin]

-- | Most events that can be handled before one render. Typeahead (a paste,
-- key repeat, a fast typist) is handled first and drawn once, the way Vim
-- does. The limit keeps the screen updating during a very long burst.
maxBatch :: Int
maxBatch = 512

eventLoop :: Loop -> Runtime -> IO () -> TChan Event -> Editor -> IO ()
eventLoop (Loop configRef userRef themeRef trueColor previewRef shownRef every) runtime suspend events = go Nothing
  where
    go :: Maybe Frame -> Editor -> IO ()
    go prev ed0 = do
      -- Once per batch, e.g. hand the language server the text as it is now.
      config <- readIORef configRef
      flushed <- execStateT (mapM_ plBeforeRender (cfgPlugins config)) ed0
      mapM_ (Runtime.perform runtime) (edEffects flushed)
      committed <- readIORef themeRef
      let ed = ensureCursorVisible (refreshSearchPreview flushed {edEffects = [], edRepaint = False})
      (theme, previewChanged) <- themePreview config ed committed
      -- After a suspend the terminal was cleared: draw everything. A
      -- change of colours is drawn everywhere too.
      let previous = if edRepaint flushed || previewChanged then Nothing else prev
          frame = render theme previous ed
      writeOutput (diffFrames previous frame)
      next <- atomically (readTChan events) >>= batch maxBatch ed
      unless (edQuit next) (go (Just frame) next)

    -- While the : line reads ":theme <name>", draw in that theme if it
    -- loads; otherwise (esc, ret, other text) in the one in use. The last
    -- preview is kept, so typing elsewhere on the line loads nothing.
    -- Whether the colours differ from the previous frame's is returned too.
    themePreview config ed committed = do
      let wanted = previewedTheme (cfgExCommands config) ed
      (shown, loaded) <- readIORef previewRef
      preview <-
        if wanted == shown
          then pure loaded
          else do
            loaded' <- case wanted of
              Nothing -> pure Nothing
              Just name -> either (const Nothing) Just <$> loadNamedTheme trueColor name
            writeIORef previewRef (wanted, loaded')
            pure loaded'
      let theme = fromMaybe committed preview
      changed <- (/= Just (themeName theme)) <$> readIORef shownRef
      writeIORef shownRef (Just (themeName theme))
      pure (theme, changed)

    -- Handle an event, then whatever else is already queued.
    -- :config-reload: a new config for the loop, the runtime's server table
    -- and the editor's settings; a broken file keeps the current config.
    reloadConfig ed = do
      (uc, config, problem) <- loadConfigWith every
      case problem of
        Just e -> pure ed {edStatus = Just (Status Error e)}
        Nothing -> do
          old <- readIORef configRef
          writeIORef configRef config
          writeIORef userRef uc
          Runtime.reconfigure runtime config
          switched <- execStateT (switchPlugins old config) ed
          -- The theme too (its file may have changed); everything is
          -- drawn again in its colours.
          themed <-
            themeOf trueColor uc >>= \case
              Left e -> pure (Just (Status Error e))
              Right theme -> Nothing <$ writeIORef themeRef theme
          pure (applyEditorOptions uc switched) {edStatus = Just (fromMaybe (Status Info "config reloaded") themed), edRepaint = True}

    -- :plugin-enable / :plugin-disable: the config made again with the
    -- plugin on or off, and the plugin told.
    setPlugin ed name on = do
      old <- readIORef configRef
      uc <- readIORef userRef
      let enabled = (if on then Set.insert else Set.delete) name (Set.fromList (map plName (cfgPlugins old)))
      if name `Set.notMember` Set.fromList (map plName every)
        then pure ed {edStatus = Just (Status Error ("unknown plugin " <> name))}
        else case applyUserConfigWithIn every enabled uc of
          Left e -> pure ed {edStatus = Just (Status Error e)}
          Right config -> do
            writeIORef configRef config
            switched <- execStateT (switchPlugins old config) ed
            pure switched {edStatus = Just (Status Info (name <> (if on then " on" else " off"))), edRepaint = True}

    loopEffect ed = \case
      ReloadConfig -> reloadConfig ed
      ChangeTheme t -> changeTheme ed t
      PluginCommand (Just (name, on)) -> setPlugin ed name on
      Suspend -> do
        suspend
        -- The window may have changed meanwhile.
        size <- fromMaybe (edSize ed) <$> getWindowSize
        pure ed {edRepaint = True, edSize = size}
      _ -> pure ed

    -- :theme: switch, or say which theme is used.
    changeTheme ed = \case
      Nothing -> do
        theme <- readIORef themeRef
        pure ed {edStatus = Just (Status Info ("theme: " <> themeName theme))}
      Just name ->
        loadNamedTheme trueColor name >>= \case
          Left e -> pure ed {edStatus = Just (Status Error e)}
          Right theme -> do
            writeIORef themeRef theme
            pure ed {edStatus = Just (Status Info ("theme: " <> name)), edRepaint = True}

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
    step ed ev = readIORef configRef >>= \config ->
      -- A bug in a command should not take the editor (and unsaved work)
      -- down with it.
      try (execStateT (handleEvent config ev) ed) >>= \case
        Right ed' -> do
          -- Start or cancel the background jobs the event asked for.
          mapM_ (Runtime.perform runtime) (edEffects ed')
          -- Then the effects only the loop can carry out, in order.
          foldM loopEffect ed' {edEffects = []} (edEffects ed')
        Left e -> do
          logMsg ("command failed: " <> show (e :: SomeException))
          execStateT (failWith ("internal error: " <> T.pack (show e))) ed
