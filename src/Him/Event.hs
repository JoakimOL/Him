-- | Everything the main loop reacts to.
module Him.Event
  ( Event (..)
  ) where

import Him.Effect (JobResult)
import Him.Key (Key)

data Event
  = -- | A decoded key press.
    EvKey Key
  | -- | The terminal was resized to @rows cols@.
    EvResize Int Int
  | -- | A background job reported back ("Him.Runtime").
    EvJob JobResult
  deriving stock (Eq, Show)
