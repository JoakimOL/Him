-- | The tree-sitter grammars him can fetch and build (ADR grammar-setup):
-- for each one, the git repository, the revision the queries are written
-- for, and where in the repository the grammar is. Read from
-- @runtime/grammars.toml@, which is built into him ("Him.Embedded").
-- Pure.
module Him.GrammarList
  ( GrammarSource (..)
  , grammarList
  , parseGrammarList
  ) where

import Data.Text (Text)
import Him.Embedded (grammarListSource)
import Him.Json (Value (..))
import Him.Toml (parseToml)

data GrammarSource = GrammarSource
  { gsName :: !Text
  , gsGit :: !Text
  , gsRev :: !Text
  , gsSubpath :: !(Maybe Text)
  -- ^ The grammar's directory in a repository holding several.
  }
  deriving stock (Eq, Show)

-- | The list built into him.
grammarList :: Either Text [GrammarSource]
grammarList = parseGrammarList grammarListSource

-- | One table per grammar: @git@, @rev@ and an optional @subpath@.
parseGrammarList :: Text -> Either Text [GrammarSource]
parseGrammarList src =
  parseToml src >>= \case
    JObject tables -> mapM entry tables
    _ -> Left "the grammar list is not a table"
  where
    entry (name, JObject fields) =
      GrammarSource name <$> required "git" <*> required "rev" <*> pure (field "subpath")
      where
        field k = case lookup k fields of
          Just (JString s) -> Just s
          _ -> Nothing
        required k = maybe (Left ("grammar " <> name <> ": no " <> k)) Right (field k)
    entry (name, _) = Left ("grammar " <> name <> ": not a table")
