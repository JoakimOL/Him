-- | The editor session without a frontend: turning events into state
-- changes (keys through the keymap, job results), the effects that need
-- the config, housekeeping after each event, and plugins coming and going.
-- The terminal loop ("Him.App") drives it; another frontend could too.
module Him.Session
  ( loadConfig
  , openAll
  , handleEvent
  , housekeeping
  , withPlugins
  , switchPlugins
  ) where

import Control.Monad (unless, when)
import Control.Monad.Trans.State.Strict (get, gets, modify')
import Data.Map.Strict qualified as Map
import Data.Char (digitToInt, isDigit)
import Data.List (partition)
import Data.Maybe (isJust)
import Data.Set qualified as Set
import Data.Text qualified as T
import Him.Buffer qualified as Buffer
import Him.Action (Bound (..), bindInvocation)
import Him.Effect (Effect (..))
import Him.EditorM (failWith)
import Him.EditorM qualified as Command
import Him.Config (Config (..), Plugin (..))
import Him.Config.Default (defaultConfig, plugins)
import Him.Document (Document (..), newDocument)
import Him.History qualified as History
import Him.Actions.File qualified as File
import Him.Actions.Register qualified as Register
import Him.Actions.Motion qualified as Motion
import Him.Actions.Picker qualified as Picker
import Him.Actions.Jump qualified as Jump
import Him.Actions.Syntax qualified as Syntax
import Him.Info (refreshInfo)
import Him.UserConfig (UserConfig (..), applyUserConfig, configPath, defaultConfigText, emptyUserConfig, loadUserConfig)
import Control.Monad.IO.Class (liftIO)
import System.Directory (doesFileExist)
import Him.Palette (paletteItems)
import Him.Picker (newPicker)
import Him.Mode (Mode (..))
import Him.Editor
import Him.Event (Event (..))
import Him.Key (Key (..), KeyCode (..))
import Him.Keymap (Keymap, Resolved (..), emptyKeymap, resolve)
import Him.Log (logMsg)
import Him.Render (ensureCursorVisible)
import System.Exit (die)

-- | The config: the user's file on top of the defaults. On a problem, the
-- defaults and a message naming the first one (all are logged).
loadConfig :: IO (UserConfig, Config, Maybe T.Text)
loadConfig = do
  path <- configPath
  defaults <- either (die . T.unpack) pure defaultConfig
  loadUserConfig path >>= \case
    Left errs -> broken path defaults errs
    Right uc -> case applyUserConfig uc of
      Left e -> broken path defaults (T.lines e)
      Right config -> pure (uc, config, Nothing)
  where
    broken path defaults errs = do
      logMsg ("config " <> path <> ": " <> T.unpack (T.unlines errs))
      let first = case errs of
            e : _ -> e
            [] -> "?"
          more = if length errs > 1 then " (and " <> T.pack (show (length errs - 1)) <> " more)" else ""
      pure (emptyUserConfig, defaults, Just ("config: " <> first <> more <> "; using the defaults (:config-open)"))

-- | An editor showing the first document, with the others open behind it.
openAll :: (Int, Int) -> [Document] -> Editor
openAll size docs = case docs of
  [] -> newEditor size (newDocument Nothing Buffer.empty)
  d : ds -> gotoBuffer 0 (foldl (flip openBuffer) (newEditor size d) ds)

handleEvent :: Config -> Event -> Command.EditorM ()
handleEvent config (EvResize rows cols) = do
  modify' (\e -> e {edSize = (rows, cols)})
  -- More lines may be visible now; they need highlighting.
  housekeeping config
handleEvent config (EvJob result) = do
  Picker.applyJobResult result
  Syntax.applySyntaxResult result
  mapM_ (`plJobResult` result) (cfgPlugins config)
  runEffects config
  housekeeping config
handleEvent config (EvKey key) = do
  -- A key some command waits for (the character after f) is that
  -- command's, not the keymap's.
  awaited <- Motion.awaitedKey key
  if awaited then afterKey config else keyThroughKeymap config key

keyThroughKeymap :: Config -> Key -> Command.EditorM ()
keyThroughKeymap config key = do
  -- A popup (hover) lasts until the next key; signature help stays while
  -- typing in insert mode (it closes at ')' or when insert mode ends).
  modify' $ \e ->
    let keep = edMode e == Insert && fmap infoTitle (edPopup e) == Just "signature"
     in e {edPopup = if keep then edPopup e else Nothing}
  ed <- get
  let pending = edPending ed
      keys = pending <> [key]
      keymap = Map.findWithDefault emptyKeymap (keymapMode ed) (cfgKeymaps config)
      setPending ks = modify' (\e -> e {edPending = ks})
      clearCount = modify' (\e -> e {edCount = Nothing})
  when (null pending) $ modify' (\e -> e {edStatus = Nothing})
  replay <- case countDigit ed keymap key of
    Just n -> False <$ modify' (\e -> e {edCount = Just n})
    Nothing -> case resolve keymap keys of
      NeedMore -> False <$ setPending keys
      Found bound -> do
        setPending []
        clearCount
        case (edCount ed, boundCounted bound) of
          (Just n, Just counted) -> counted n
          _ -> boundRun bound
        -- A register chosen with " is for this one command.
        when (isJust (edSelectedRegister ed)) $ modify' (\e -> e {edSelectedRegister = Nothing})
        pure False
      NoMatch
        -- A started sequence that does not go on, of keys that mean
        -- something on their own (the first j of a "j j" bound in insert
        -- mode): they act as typed, and this key starts afresh.
        | not (null pending)
        , Just typed <- traverse (cfgFallback config (edMode ed)) pending -> do
            setPending []
            sequence_ typed
            pure True
        | otherwise -> do
            setPending []
            clearCount
            modify' (\e -> e {edSelectedRegister = Nothing})
            -- Only a key typed on its own falls back (a failed chord is dropped).
            when (null pending) $ sequence_ (cfgFallback config (edMode ed) key)
            pure False
  if replay then keyThroughKeymap config key else afterKey config

-- | What follows every key: effects, undo grouping, background state, the
-- info box.
afterKey :: Config -> Command.EditorM ()
afterKey config = do
  runEffects config
  commitOutsideInsert
  housekeeping config
  modify' (refreshInfo config)

-- | Keep the current document's background state current (highlighting,
-- and whatever the plugins track): it asks for jobs when something
-- changed.
housekeeping :: Config -> Command.EditorM ()
housekeeping config = do
  -- The view moves with the cursor before rendering; follow it here too,
  -- so highlighting asks for the lines that will be shown.
  modify' ensureCursorVisible
  Syntax.syntaxHousekeeping
  mapM_ plHousekeeping (cfgPlugins config)
  Picker.pickerHousekeeping
  modify' Jump.syncJumps

-- | What the editor needs to know about the enabled plugins.
withPlugins :: Config -> Editor -> Editor
withPlugins config ed = ed {edSignLane = any plSigns (cfgPlugins config)}

-- | Going from one config's plugins to another's: the ones switched off
-- clear up, the ones switched on start, and the gutter follows.
switchPlugins :: Config -> Config -> Command.EditorM ()
switchPlugins old new = do
  let names = Set.fromList . map plName . cfgPlugins
      gone = [p | p <- cfgPlugins old, plName p `Set.notMember` names new]
      added = [p | p <- cfgPlugins new, plName p `Set.notMember` names old]
  mapM_ plDisable gone
  mapM_ plEnable added
  modify' (withPlugins new)
  housekeeping new

-- | Carry out the effects the key's action requested that need the config
-- (ADR-23). An action run this way may request more; a chain is cut off
-- after a few rounds so a loop cannot hang the editor.
runEffects :: Config -> Command.EditorM ()
runEffects config = go (8 :: Int)
  where
    go 0 = modify' (\e -> e {edEffects = filter (not . immediate) (edEffects e)})
    go n = do
      (now, later) <- gets (partition immediate . edEffects)
      if null now
        then pure ()
        else do
          modify' (\e -> e {edEffects = later})
          mapM_ perform now
          go (n - 1)
    -- Jobs are left for the main loop, which owns the runtime.
    immediate = \case
      RunAction _ -> True
      OpenPalette -> True
      OpenConfig -> True
      PluginCommand Nothing -> True
      ClipboardSet {} -> True
      ClipboardGet {} -> True
      _ -> False
    perform = \case
      StartJob _ -> pure ()
      CancelJob _ -> pure ()
      LspSend _ _ -> pure ()
      LspStop _ -> pure ()
      Suspend -> pure ()
      ReloadConfig -> pure ()
      ChangeTheme _ -> pure ()
      LspStopAll -> pure ()
      ReplStart {} -> pure ()
      ReplSend {} -> pure ()
      ReplInterrupt _ -> pure ()
      ReplStop _ -> pure ()
      ChatSend {} -> pure ()
      ChatCancel _ -> pure ()
      ChatAnswer {} -> pure ()
      PluginCommand (Just _) -> pure ()
      ClipboardSet c vs -> Register.clipboardSet (cfgClipboardProviders config) c vs
      ClipboardGet c use -> Register.clipboardGet (cfgClipboardProviders config) c use
      PluginCommand Nothing ->
        let on = map plName (cfgPlugins config)
            describe p = plName p <> (if plName p `elem` on then " (on)" else " (off)")
         in Command.info ("plugins: " <> T.intercalate ", " (map describe plugins))
      OpenConfig -> do
        path <- liftIO configPath
        exists <- liftIO (doesFileExist path)
        if exists
          then File.openFile path
          else do
            -- A new file, with the defaults to start from (saved with :w).
            modify' (openBuffer ((newDocument (Just path) (Buffer.fromText defaultConfigText)) {docDirty = True}))
            Command.info "a new config file with the defaults: change what you like, then :w and :config-reload"
      RunAction inv -> either failWith boundRun (bindInvocation (cfgActions config) inv)
      OpenPalette -> modify' $ \e ->
        e {edPicker = Just (newPicker "commands" (paletteItems config (keymapMode e))), edMode = Picking}

-- | A digit typed before a key sequence, in normal or select mode, adds to
-- the count (@1 2 j@ moves 12 lines). @0@ only continues a count, and a
-- digit the keymap binds keeps its binding.
countDigit :: Editor -> Keymap Bound -> Key -> Maybe Int
countDigit ed keymap key = case key of
  Key (KChar c) mods
    | Set.null mods
    , isDigit c
    , null (edPending ed)
    , keymapMode ed `elem` [Normal, Select, Directory]
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
