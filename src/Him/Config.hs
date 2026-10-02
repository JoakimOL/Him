-- | What the main loop needs to turn keys into actions, and how it is built
-- from bindings written as text (see ADR-17 in docs/PLAN.md).
--
-- A config file parser only has to produce 'Bindings': it can then call
-- @'buildConfig' actions ('overrideBindings' user defaults)@ and report the
-- errors that come back.
module Him.Config
  ( Config (..)
  , Plugin (..)
  , plugin
  , Bindings
  , buildConfig
  , overrideBindings
  , inheritsFrom
  ) where

import Data.Either (lefts, partitionEithers)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Text (Text)
import Data.Text qualified as T
import Him.Action
import Him.Command (EditorM)
import Him.Ex (ExCommand)
import Him.Effect (JobResult)
import Him.Lsp.Config (ServerTable, defaultServers)
import Him.Syntax (SyntaxProvider)
import Him.Key (Key)
import Him.Keymap (Keymap, fromBindings, unionKeymap)
import Him.Mode (Mode (..))

data Config = Config
  { cfgActions :: ActionRegistry
  , cfgKeymaps :: Map Mode (Keymap Bound)
  , cfgFallback :: Mode -> Key -> Maybe (EditorM ())
  -- ^ What to do with a key that no binding matches, e.g. insert the typed
  -- character in insert mode.
  , cfgExCommands :: [ExCommand]
  -- ^ For completing and describing @:@ commands.
  , cfgPrefixNames :: Map [Key] Text
  -- ^ Titles for key prefixes in the info box, e.g. @g@ = "goto".
  , cfgSyntaxProviders :: [SyntaxProvider]
  -- ^ Highlighters, tried in order for each document's language
  -- ("Him.Syntax").
  , cfgServers :: ServerTable
  -- ^ Language servers by language.
  , cfgPlugins :: [Plugin]
  -- ^ The enabled plugins, in order (their actions, keys and commands are
  -- in the fields above already).
  }

-- | A feature that can be switched off (ADR-35): git signs and staging,
-- the language-server client. Everything it adds to the editor is named
-- here; a disabled plugin adds nothing, and its hooks do not run. Its
-- state lives in the editor and documents as before (so tests and
-- rendering need no plugin machinery), and 'plDisable' clears it.
data Plugin = Plugin
  { plName :: Text
  , plDoc :: Text
  , plActions :: [Action]
  , plBindings :: Bindings
  -- ^ Default keys, added to the core's (the user's go on top of both).
  , plExCommands :: [ExCommand]
  , plPrefixNames :: [([Key], Text)]
  , plSigns :: Bool
  -- ^ It draws in the gutter's sign lane (the lane is left out when no
  -- enabled plugin does).
  , plHousekeeping :: EditorM ()
  -- ^ After every event: notice what changed and start jobs.
  , plBeforeRender :: EditorM ()
  -- ^ Once per batch of input, before drawing.
  , plJobResult :: JobResult -> EditorM ()
  -- ^ Every background job result (it picks out its own).
  , plEnable :: EditorM ()
  -- ^ Switched on while running: e.g. look every document up again.
  , plDisable :: EditorM ()
  -- ^ Switched off while running: stop its work and clear its state.
  }

-- | A plugin that adds nothing yet; fill in what it has.
plugin :: Text -> Text -> Plugin
plugin name doc = Plugin name doc [] Map.empty [] [] False (pure ()) (pure ()) (const (pure ())) (pure ()) (pure ())

-- | Per mode, @(keys, action invocation)@ pairs such as
-- @("g g", "goto_file_start")@ or @("C-d", "move_line_down 20")@. Within a
-- mode, a later pair for the same keys wins.
type Bindings = Map Mode [(Text, Text)]

-- | Put the first bindings on top of the second (user bindings over the
-- defaults). Bind a key to @no_op@ to disable it.
overrideBindings :: Bindings -> Bindings -> Bindings
overrideBindings user defaults = Map.unionWith (flip (<>)) user defaults

-- | A mode that also uses another mode's bindings, under its own: select
-- mode and the directory layer are normal mode with a few overrides.
inheritsFrom :: Mode -> Maybe Mode
inheritsFrom = \case
  Select -> Just Normal
  Directory -> Just Normal
  Completing -> Just Insert
  _ -> Nothing

-- | Validate every binding against the actions and build the keymaps.
-- All errors are reported, one per line.
buildConfig :: [Action] -> Bindings -> (Mode -> Key -> Maybe (EditorM ())) -> Either Text Config
buildConfig actions bindings fallback = do
  registry <- mkActionRegistry actions
  let bindPair mode (keys, inv) = case bindText registry inv of
        Left e -> Left (T.pack (show mode) <> " mode, " <> keys <> ": " <> e)
        Right b -> Right (keys, b)
      buildMode mode pairs = case partitionEithers (map (bindPair mode) pairs) of
        ([], ok) -> either (Left . pure) Right (fromBindings ok)
        (errs, _) -> Left errs
      compiled = Map.mapWithKey buildMode bindings
      own = Map.mapMaybe (either (const Nothing) Just) compiled
      withParent mode km = case inheritsFrom mode >>= (`Map.lookup` own) of
        Just parent -> unionKeymap km parent
        Nothing -> km
  case concat (lefts (Map.elems compiled)) of
    [] ->
      Right
        Config
          { cfgActions = registry
          , cfgKeymaps = Map.mapWithKey withParent own
          , cfgFallback = fallback
          , cfgExCommands = []
          , cfgPrefixNames = Map.empty
          , cfgSyntaxProviders = []
          , cfgServers = defaultServers
          , cfgPlugins = []
          }
    errs -> Left (T.intercalate "\n" errs)
