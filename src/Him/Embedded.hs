{-# LANGUAGE TemplateHaskell #-}

-- | What him carries of @runtime/@ (ADR grammar-setup), so a new install
-- needs no other editor's files: the grammar list (which repository and
-- revision each tree-sitter grammar is built from) and every language's
-- @highlights.scm@. Both come from Helix (MPL-2.0, @runtime/queries/LICENSE@)
-- through @dev/sync-helix-runtime.py@.
module Him.Embedded
  ( grammarListSource
  , embeddedQueries
  ) where

import Data.Map.Lazy (Map)
import Data.Map.Lazy qualified as Map
import Data.Text (Text)
import Data.Text qualified as T
import Him.Embedded.TH (embedQueries, embedText)

-- | @runtime/grammars.toml@.
grammarListSource :: Text
grammarListSource = T.pack $(embedText "runtime/grammars.toml")

-- | Each language's @highlights.scm@, by the language's query directory name.
-- Lazy: a lookup packs only the query it finds (a strict map packed all
-- 3 MB of them when the first file was highlighted).
embeddedQueries :: Map Text Text
embeddedQueries = Map.fromList [(T.pack lang, T.pack q) | (lang, q) <- $(embedQueries "runtime/queries")]
