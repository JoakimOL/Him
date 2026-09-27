-- | Keymaps: tries from key sequences to command names.
module Him.Keymap
  ( Keymap
  , Resolved (..)
  , emptyKeymap
  , fromBindings
  , resolve
  , unionKeymap
  , boundCommands
  ) where

import Control.Monad (foldM)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Text (Text)
import Him.Key (Key, parseKeys)

newtype Keymap = Keymap (Map Key Node)
  deriving stock (Eq, Show)

data Node
  = Leaf Text
  | -- | A key that starts a longer sequence, e.g. @g@ in @g g@.
    Prefix Keymap
  deriving stock (Eq, Show)

data Resolved
  = Found Text
  | -- | The keys so far are a prefix of at least one binding.
    NeedMore
  | NoMatch
  deriving stock (Eq, Show)

emptyKeymap :: Keymap
emptyKeymap = Keymap Map.empty

-- | Build a keymap from @(keys, command)@ pairs such as @("g g",
-- "goto_file_start")@, using the syntax of 'Him.Key.parseKeys'. Later
-- bindings override earlier ones.
fromBindings :: [(Text, Text)] -> Either Text Keymap
fromBindings = foldM add emptyKeymap
  where
    add km (keysText, name) = case parseKeys keysText of
      Just ks@(_ : _) -> Right (insert ks name km)
      _ -> Left ("invalid key sequence: " <> keysText)

insert :: [Key] -> Text -> Keymap -> Keymap
insert [] _ km = km
insert [k] name (Keymap m) = Keymap (Map.insert k (Leaf name) m)
insert (k : ks) name (Keymap m) = Keymap (Map.insert k (Prefix (insert ks name sub)) m)
  where
    sub = case Map.lookup k m of
      Just (Prefix s) -> s
      _ -> emptyKeymap

resolve :: Keymap -> [Key] -> Resolved
resolve _ [] = NeedMore
resolve (Keymap m) (k : ks) = case Map.lookup k m of
  Nothing -> NoMatch
  Just (Leaf name)
    | null ks -> Found name
    | otherwise -> NoMatch
  Just (Prefix sub)
    | null ks -> NeedMore
    | otherwise -> resolve sub ks

-- | Left-biased union that merges prefix nodes, so a mode can override a
-- few bindings of another.
unionKeymap :: Keymap -> Keymap -> Keymap
unionKeymap (Keymap a) (Keymap b) = Keymap (Map.unionWith merge a b)
  where
    merge (Prefix x) (Prefix y) = Prefix (unionKeymap x y)
    merge x _ = x

-- | All command names a keymap refers to.
boundCommands :: Keymap -> [Text]
boundCommands (Keymap m) = concatMap go (Map.elems m)
  where
    go (Leaf name) = [name]
    go (Prefix km) = boundCommands km
