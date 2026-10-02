-- | Syntax highlighting: one interface for every kind of highlighter
-- (ADR-26).
--
-- The editor knows only this module. A provider (tree-sitter, TextMate,
-- or a fake one in the tests) is a value of 'SyntaxProvider', listed in
-- the config ('Him.Config.cfgSyntaxProviders'); the first one that can
-- highlight a document's language is used. No provider type appears
-- anywhere else, so adding one is a new module plus an entry in that list.
module Him.Syntax
  ( SyntaxProvider (..)
  , SyntaxSession (..)
  , TextChange (..)
  , LineSpan (..)
  , SyntaxInfo (..)
  , SyntaxStatus (..)
  , noSyntax
  , startSyntax
  , flatten
  ) where

import Data.IntMap.Strict (IntMap)
import Data.IntMap.Strict qualified as IntMap
import Data.List (sortOn)
import Data.Text (Text)
import Him.Buffer (Buffer)
import Him.Language (Language)
import Him.Syntax.Span

-- | Something that can highlight some languages.
data SyntaxProvider = SyntaxProvider
  { spName :: !Text
  , spStart :: Language -> IO (Maybe SyntaxSession)
  -- ^ 'Nothing' when it has no definition for the language.
  }

-- | A running highlighter for one document. Its state (a parse tree, a
-- TextMate rule stack per line, …) stays inside.
data SyntaxSession = SyntaxSession
  { ssUpdate :: Int -> Buffer -> [TextChange] -> IO ()
  -- ^ A new version of the text, and the edits since the last one if
  -- known (empty: start over, e.g. after an undo).
  , ssHighlight :: Int -> Int -> IO (IntMap [LineSpan])
  -- ^ Spans for lines @[from, to]@, per line, sorted and not overlapping.
  , ssClose :: IO ()
  }

-- | An edit, for providers that can update incrementally: the text between
-- the two positions (line, column) was replaced by the new text.
data TextChange = TextChange
  { tcFrom :: !(Int, Int)
  , tcTo :: !(Int, Int)
  , tcText :: !Text
  }
  deriving stock (Eq, Show)

data SyntaxStatus
  = -- | Not looked at yet.
    SyntaxUnknown
  | SyntaxStarting
  | -- | No language, or no provider for it.
    SyntaxNone
  | -- | Highlighted by this provider.
    SyntaxActive !Text
  deriving stock (Eq, Show)

-- | A document's highlighting, as far as the editor knows it.
data SyntaxInfo = SyntaxInfo
  { siStatus :: !SyntaxStatus
  , siLanguage :: !(Maybe Text)
  , siSpans :: !(IntMap [LineSpan])
  , siVersion :: !Int
  -- ^ The document version the spans are for (-1: none).
  , siFrom :: !Int
  , siTo :: !Int
  -- ^ The lines the spans cover.
  , siPending :: !Bool
  -- ^ A highlight job is running.
  }
  deriving stock (Eq, Show)

noSyntax :: SyntaxInfo
noSyntax = SyntaxInfo SyntaxUnknown Nothing IntMap.empty (-1) 0 (-1) False

-- | Try the providers in order.
startSyntax :: [SyntaxProvider] -> Language -> IO (Maybe (Text, SyntaxSession))
startSyntax [] _ = pure Nothing
startSyntax (p : ps) lang =
  spStart p lang >>= \case
    Just session -> pure (Just (spName p, session))
    Nothing -> startSyntax ps lang

-- | Make spans of one line sorted and non-overlapping. Where spans
-- overlap, the earlier one in the list wins (providers list their most
-- important matches first).
flatten :: [LineSpan] -> [LineSpan]
flatten spans = merge (sortOn lsStart (go [] spans))
  where
    -- Cut each span around the ones already taken.
    go taken [] = taken
    go taken (s : rest) = go (taken <> cut s taken) rest
    cut s [] = [s | lsStart s < lsEnd s]
    cut s (t : ts)
      | lsEnd s <= lsStart t || lsEnd t <= lsStart s = cut s ts
      | otherwise =
          concatMap (`cut` ts) [s {lsEnd = lsStart t}, s {lsStart = lsEnd t}]
    merge (a : b : rest)
      | lsEnd a == lsStart b && lsScope a == lsScope b = merge (a {lsEnd = lsEnd b} : rest)
      | otherwise = a : merge (b : rest)
    merge xs = xs
