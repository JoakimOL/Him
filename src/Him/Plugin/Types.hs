{-# LANGUAGE GeneralizedNewtypeDeriving #-}

-- | The types of the plugin API (ADR plugin-api): a plugin's spec, the monad its
-- code runs in, and the views of the editor it gets. Plugins import
-- "Him.Plugin", which re-exports these.
module Him.Plugin.Types
  ( PluginSpec (..)
  , pluginSpec
  , PluginM (..)
  , Ctx (..)
  , runPluginM
  , PluginAction (..)
  , PluginCommand (..)
  , BufferId
  , BufferInfo (..)
  , BufferKind (..)
  , WindowInfo (..)
  , Item (..)
  , Target (..)
  , PickerSpec (..)
  , apiVersion
  ) where

import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Trans.Reader (ReaderT (..))
import Data.Text (Text)
import Him.Action (Action)
import Him.EditorM (EditorM)
import Him.Ex (ExCommand)
import Him.Mode (Mode)
import Him.PluginEvent (Event)

-- | The plugin API's version: raised when it changes in a way plugins
-- notice.
apiVersion :: Int
apiVersion = 1

-- | A plugin: what it adds, and what it does when things happen. @s@ is
-- its own state (any type), which starts as 'psInitial' and goes back to
-- it when the plugin is switched off.
data PluginSpec s = PluginSpec
  { psName :: Text
  -- ^ Lower case, as in @[plugins]@ and @:plugin-enable@.
  , psDoc :: Text
  , psInitial :: s
  , psDefaultOn :: Bool
  -- ^ On unless the config says otherwise (contrib plugins: 'False').
  , psActions :: [PluginAction s]
  , psCommands :: [PluginCommand s]
  , psBindings :: [(Mode, Text, Text)]
  -- ^ Default keys: mode, keys (@"space x"@), action invocation. The
  -- user's keys win.
  , psPrefixNames :: [(Text, Text)]
  -- ^ Titles of key prefixes (@("space x", "my plugin")@).
  , psOptions :: [(Text, Text)]
  -- ^ Its settings under @[plugins.<name>]@: key and what it does
  -- (others are refused when the config is read).
  , psSigns :: Bool
  -- ^ It shows gutter signs (the gutter keeps a lane for them).
  , psOnEvent :: Event -> PluginM s ()
  , psStart :: PluginM s ()
  -- ^ When it is switched on while the editor runs.
  , psStop :: PluginM s ()
  -- ^ When it is switched off (its UI, processes and state go anyway).
  }

-- | A plugin that adds nothing yet: fill in what it has.
pluginSpec :: Text -> Text -> s -> PluginSpec s
pluginSpec name doc initial = PluginSpec name doc initial True [] [] [] [] [] False (const (pure ())) (pure ()) (pure ())

-- | What plugin code knows: its name and its initial state.
data Ctx s = Ctx
  { ctxName :: Text
  , ctxInitial :: s
  }

-- | Plugin code: the editor monad, knowing which plugin it runs for.
newtype PluginM s a = PluginM (ReaderT (Ctx s) EditorM a)
  deriving newtype (Functor, Applicative, Monad, MonadIO)

runPluginM :: Ctx s -> PluginM s a -> EditorM a
runPluginM ctx (PluginM m) = runReaderT m ctx

-- | An action of the plugin, made with 'Him.Plugin.action' or
-- 'Him.Plugin.actionWith'.
newtype PluginAction s = PluginAction (Ctx s -> Action)

-- | A @:@ command of the plugin ('Him.Plugin.command').
newtype PluginCommand s = PluginCommand (Ctx s -> ExCommand)

-- | Documents are known by their id.
type BufferId = Int

data BufferKind = TextBuffer | DirectoryBuffer | ReplBuffer | ChatBuffer | ScratchBuffer
  deriving stock (Eq, Show)

-- | What a plugin sees of a buffer.
data BufferInfo = BufferInfo
  { biId :: !BufferId
  , biPath :: !(Maybe FilePath)
  , biName :: !Text
  -- ^ As the status line shows it.
  , biKind :: !BufferKind
  , biLanguage :: !(Maybe Text)
  , biDirty :: !Bool
  , biLineCount :: !Int
  , biVersion :: !Int
  -- ^ Goes up with every change.
  }
  deriving stock (Eq, Show)

data WindowInfo = WindowInfo
  { wiId :: !Int
  , wiBuffer :: !BufferId
  , wiFocused :: !Bool
  }
  deriving stock (Eq, Show)

-- | What a picker item stands for. A file or a position gets a preview,
-- and @picker_open@ goes there.
data Target
  = TargetValue !Text
  | TargetFile !FilePath
  | -- | A file, a line and a column (from 0).
    TargetPosition !FilePath !Int !Int
  deriving stock (Eq, Show)

data Item = Item
  { itemLabel :: !Text
  , itemDetail :: !Text
  -- ^ Shown dimmed after the label, and matched too.
  , itemTarget :: !Target
  }
  deriving stock (Eq, Show)

-- | A picker: @ret@ runs the primary action, @del@ the secondary one, on
-- the chosen items ('Him.Plugin.chosenItems'); @tab@ marks items.
data PickerSpec = PickerSpec
  { pickerTitle :: !Text
  , pickerItems :: ![Item]
  , pickerPrimary :: !Text
  -- ^ An action name: the plugin's own, or @picker_open@ (go to the
  -- chosen files and positions).
  , pickerSecondary :: !(Maybe Text)
  }
  deriving stock (Eq, Show)
