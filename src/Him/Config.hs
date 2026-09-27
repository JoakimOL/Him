-- | What the main loop needs to turn keys into actions.
module Him.Config
  ( Config (..)
  ) where

import Data.Map.Strict (Map)
import Him.Command (EditorM, Registry)
import Him.Key (Key)
import Him.Keymap (Keymap)
import Him.Mode (Mode)

data Config = Config
  { cfgRegistry :: Registry
  , cfgKeymaps :: Map Mode Keymap
  , cfgFallback :: Mode -> Key -> Maybe (EditorM ())
  -- ^ What to do with a key that no binding matches, e.g. insert the typed
  -- character in insert mode.
  }
