-- | Plugins' own state (ADR plugin-api), kept in the editor so each editor (and
-- each test) has its own. A plugin's state can be of any type; it is
-- stored as a 'Dynamic' under the plugin's name.
module Him.PluginState
  ( PluginStates
  , noStates
  , lookupState
  , insertState
  , deleteState
  ) where

import Data.Dynamic (Dynamic, fromDynamic, toDyn)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Text (Text)
import Data.Typeable (Typeable)

newtype PluginStates = PluginStates (Map Text Dynamic)

-- | States cannot be compared or shown (they are of any type); only which
-- plugins have one is.
instance Eq PluginStates where
  PluginStates a == PluginStates b = Map.keys a == Map.keys b

instance Show PluginStates where
  show (PluginStates m) = "PluginStates " <> show (Map.keys m)

noStates :: PluginStates
noStates = PluginStates Map.empty

lookupState :: Typeable s => Text -> PluginStates -> Maybe s
lookupState name (PluginStates m) = Map.lookup name m >>= fromDynamic

insertState :: Typeable s => Text -> s -> PluginStates -> PluginStates
insertState name s (PluginStates m) = PluginStates (Map.insert name (toDyn s) m)

deleteState :: Text -> PluginStates -> PluginStates
deleteState name (PluginStates m) = PluginStates (Map.delete name m)
