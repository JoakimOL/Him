-- | Effects: what an action asks for beyond changing the editor state
-- (ADR-23). Actions only queue them ('Him.Command.request'), so they stay
-- state changes that tests can inspect; the main loop carries them out.
module Him.Effect
  ( Effect (..)
  ) where

import Him.Invocation (Invocation)

data Effect
  = -- | Run another action, e.g. the one chosen in the command palette.
    -- Handled right after the current key, with the config at hand.
    RunAction !Invocation
  | -- | Open the command palette (it lists the configured actions and keys).
    OpenPalette
  deriving stock (Eq, Show)
