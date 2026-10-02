-- | Keymaps: tries from key sequences to bindings. A keymap is built from
-- text (@Keymap Text@, the action invocations), validated, and then mapped
-- to runnable actions (@Keymap Bound@, see "Him.Action").
module Him.Keymap
  ( Keymap
  , Resolved (..)
  , emptyKeymap
  , fromBindings
  , resolve
  , unionKeymap
  , keymapBindings
  , lookupPrefix
  , children
  ) where

import Control.Monad (foldM)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Text (Text)
import Him.Key (Key, parseKeys)

newtype Keymap a = Keymap (Map Key (Node a))
  deriving stock (Eq, Show, Functor, Foldable, Traversable)

data Node a
  = Leaf a
  | -- | A key that starts a longer sequence, e.g. @g@ in @g g@.
    Prefix (Keymap a)
  deriving stock (Eq, Show, Functor, Foldable, Traversable)

data Resolved a
  = Found a
  | -- | The keys so far are a prefix of at least one binding.
    NeedMore
  | NoMatch
  deriving stock (Eq, Show)

emptyKeymap :: Keymap a
emptyKeymap = Keymap Map.empty

-- | Build a keymap from @(keys, binding)@ pairs such as @("g g",
-- "goto_file_start")@, using the syntax of 'Him.Key.parseKeys'. Later
-- bindings override earlier ones.
fromBindings :: [(Text, a)] -> Either Text (Keymap a)
fromBindings = foldM add emptyKeymap
  where
    add km (keysText, b) = case parseKeys keysText of
      Just ks@(_ : _) -> Right (insert ks b km)
      _ -> Left ("invalid key sequence: " <> keysText)

insert :: [Key] -> a -> Keymap a -> Keymap a
insert [] _ km = km
insert [k] b (Keymap m) = Keymap (Map.insert k (Leaf b) m)
insert (k : ks) b (Keymap m) = Keymap (Map.insert k (Prefix (insert ks b sub)) m)
  where
    sub = case Map.lookup k m of
      Just (Prefix s) -> s
      _ -> emptyKeymap

resolve :: Keymap a -> [Key] -> Resolved a
resolve _ [] = NeedMore
resolve (Keymap m) (k : ks) = case Map.lookup k m of
  Nothing -> NoMatch
  Just (Leaf b)
    | null ks -> Found b
    | otherwise -> NoMatch
  Just (Prefix sub)
    | null ks -> NeedMore
    | otherwise -> resolve sub ks

-- | Left-biased union that merges prefix nodes, so a mode can override a
-- few bindings of another.
unionKeymap :: Keymap a -> Keymap a -> Keymap a
unionKeymap (Keymap a) (Keymap b) = Keymap (Map.unionWith merge a b)
  where
    merge (Prefix x) (Prefix y) = Prefix (unionKeymap x y)
    merge x _ = x

-- | Every binding with its key sequence, e.g. for help or for checking a
-- config.
keymapBindings :: Keymap a -> [([Key], a)]
keymapBindings (Keymap m) = concatMap go (Map.toList m)
  where
    go (k, Leaf b) = [([k], b)]
    go (k, Prefix sub) = [(k : ks, b) | (ks, b) <- keymapBindings sub]

-- | The keymap below a prefix (@g@ in @g g@).
lookupPrefix :: Keymap a -> [Key] -> Maybe (Keymap a)
lookupPrefix km [] = Just km
lookupPrefix (Keymap m) (k : ks) = case Map.lookup k m of
  Just (Prefix sub) -> lookupPrefix sub ks
  _ -> Nothing

-- | The keys a keymap binds directly: a binding, or 'Nothing' for a key
-- that starts a longer sequence.
children :: Keymap a -> [(Key, Maybe a)]
children (Keymap m) = [(k, leaf n) | (k, n) <- Map.toList m]
  where
    leaf (Leaf b) = Just b
    leaf (Prefix _) = Nothing
