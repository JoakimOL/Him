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

import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Text (Text)
import Data.Text qualified as T
import Him.Embedded.TH (embedQueries, embedText)

-- | @runtime/grammars.toml@.
grammarListSource :: Text
grammarListSource = T.pack $(embedText "runtime/grammars.toml")

-- | Each language's @highlights.scm@, by the language's query directory name.
embeddedQueries :: Map Text Text
embeddedQueries = Map.fromList [(T.pack lang, T.pack q) | (lang, q) <- $(embedQueries "runtime/queries")]
