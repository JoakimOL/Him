-- | Which keys run an action, in the config that is running: for texts
-- that tell the user what to press (a header, a placeholder, a message), so
-- they follow the user's bindings instead of naming the defaults. Built
-- from the config's keymaps ("Him.Session" keeps the editor's copy
-- current); looked up by an action's invocation (@chat_next_change@,
-- @command_mode_with "magit-commit "@). Pure.
module Him.KeyHints
  ( KeyHints
  , HintScope (..)
  , noHints
  , buildHints
  , keyFor
  , keyOr
  , hintLine
  ) where

import Data.List (sortOn)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Text (Text)
import Data.Text qualified as T
import Him.Key (Key, showKeys)
import Him.Mode (Mode)

-- | Where keys are pressed: a mode (its keymap, inherited keys included),
-- or a plugin's keymap by full name (@magit:status@).
data HintScope = InMode !Mode | InKeymap !Text
  deriving stock (Eq, Ord, Show)

-- | The key sequences of each invocation in each scope, best first.
newtype KeyHints = KeyHints (Map (HintScope, Text) [Text])
  deriving stock (Eq, Show)

noHints :: KeyHints
noHints = KeyHints Map.empty

-- | From every binding: its scope, keys and invocation. The best keys are
-- the fewest presses, then the shortest to read (@C-w v@ over
-- @space w C-v@).
buildHints :: [(HintScope, [Key], Text)] -> KeyHints
buildHints bindings =
  KeyHints (Map.map (map snd . sortOn fst) (Map.fromListWith (<>) [((scope, inv), [((length keys, T.length shown), shown)]) | (scope, keys, inv) <- bindings, let shown = showKeys keys]))

-- | The best keys for an invocation in a scope, if it has any.
keyFor :: KeyHints -> HintScope -> Text -> Maybe Text
keyFor (KeyHints m) scope inv = case Map.lookup (scope, inv) m of
  Just (k : _) -> Just k
  _ -> Nothing

-- | The keys, or how to run it without (@:action name@) when nothing is
-- bound to it.
keyOr :: KeyHints -> HintScope -> Text -> Text
keyOr hints scope inv = maybe (":action " <> inv) id (keyFor hints scope inv)

-- | A line of hints, @"ret send · A-ret new line"@: each bound invocation
-- with what it does; unbound ones are left out.
hintLine :: KeyHints -> HintScope -> [(Text, Text)] -> Text
hintLine hints scope pairs = T.intercalate " · " [k <> " " <> what | (inv, what) <- pairs, Just k <- [keyFor hints scope inv]]
