-- | The plugin API's escape hatch (ADR-51): plugin code that needs the
-- editor's internals. Built-in plugins may use it while they move to the
-- API; contrib plugins should not (review keeps them to "Him.Plugin").
module Him.Plugin.Internal
  ( liftEditor
  , askCtx
  ) where

import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Reader (ask)
import Him.EditorM (EditorM)
import Him.Plugin.Types (Ctx, PluginM (..))

liftEditor :: EditorM a -> PluginM s a
liftEditor = PluginM . lift

askCtx :: PluginM s (Ctx s)
askCtx = PluginM ask
