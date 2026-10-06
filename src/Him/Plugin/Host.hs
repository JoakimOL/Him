-- | Turning a 'PluginSpec' into the editor's 'Plugin' record (ADR plugin-api):
-- its actions, commands, keys and hooks, run for it by name.
module Him.Plugin.Host
  ( hostPlugin
  ) where

import Control.Monad.Trans.State.Strict (modify')
import Data.Map.Strict qualified as Map
import Data.Maybe (mapMaybe)
import Him.Config (Plugin (..), plugin)
import Him.Editor (Editor (..))
import Him.Key (parseKeys)
import Him.Plugin.Types
import Him.PluginState (deleteState)

hostPlugin :: PluginSpec s -> Plugin
hostPlugin spec =
  (plugin (psName spec) (psDoc spec))
    { plActions = [mk ctx | PluginAction mk <- psActions spec]
    , plExCommands = [mk ctx | PluginCommand mk <- psCommands spec]
    , plBindings = Map.fromListWith (flip (<>)) [(m, [(keys, inv)]) | (m, keys, inv) <- psBindings spec]
    , plPrefixNames = mapMaybe (\(keys, name) -> (,name) <$> parseKeys keys) (psPrefixNames spec)
    , plSigns = psSigns spec
    , plOptions = psOptions spec
    , plDefaultOn = psDefaultOn spec
    , plEvent = run . psOnEvent spec
    , plEnable = run (psStart spec)
    , plDisable = do
        run (psStop spec)
        modify' (\e -> e {edPluginStates = deleteState (psName spec) (edPluginStates e)})
    }
  where
    ctx = Ctx (psName spec) (psInitial spec)
    run = runPluginM ctx
