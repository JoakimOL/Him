-- | A highlighted stretch of one line: what syntax providers produce and
-- what the renderer draws. Kept apart from "Him.Syntax" so low-level
-- rendering modules can use it.
module Him.Syntax.Span
  ( LineSpan (..)
  ) where

import Data.Text (Text)

-- | Characters @[lsStart, lsEnd)@ of a line belong to a scope: a dotted
-- name such as @keyword.control.import@ (tree-sitter capture names and
-- TextMate scopes have this form; the theme resolves it by its longest
-- known prefix).
data LineSpan = LineSpan
  { lsStart :: !Int
  , lsEnd :: !Int
  , lsScope :: !Text
  }
  deriving stock (Eq, Show)
