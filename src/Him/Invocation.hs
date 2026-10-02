-- | Invocations: an action name plus its arguments, as written in a
-- binding (@move_line_down 5@, @insert_text "// "@). Pure and dependency
-- free, so that editor state ("Him.Effect") can hold one; "Him.Action"
-- binds them to actions.
module Him.Invocation
  ( Invocation (..)
  , parseInvocation
  , renderInvocation
  , quoteArg
  ) where

import Data.Char (isDigit, isSpace)
import Data.Text (Text)
import Data.Text qualified as T

-- | An action name plus its arguments as text: what a binding says.
data Invocation = Invocation
  { invAction :: !Text
  , invArgs :: ![Text]
  }
  deriving stock (Eq, Show)

-- | Parse @name arg ...@. Arguments are separated by spaces; a
-- double-quoted argument may contain spaces and the escapes @\\\"@, @\\\\@,
-- @\\n@ and @\\t@.
parseInvocation :: Text -> Either Text Invocation
parseInvocation src = tokens (T.unpack src) >>= \case
  [] -> Left "empty action"
  (name : args)
    | validName name -> Right (Invocation name args)
    | otherwise -> Left ("invalid action name: " <> quoteArg name)
  where
    validName n = not (T.null n) && T.all (\c -> c == '_' || isDigit c || (c >= 'a' && c <= 'z')) n

tokens :: String -> Either Text [Text]
tokens s = case dropWhile isSpace s of
  [] -> Right []
  '"' : rest -> quoted "" rest
  rest -> let (w, rest') = break isSpace rest in (T.pack w :) <$> tokens rest'
  where
    quoted acc = \case
      '"' : rest -> (T.pack (reverse acc) :) <$> tokens rest
      '\\' : c : rest -> case lookup c [('"', '"'), ('\\', '\\'), ('n', '\n'), ('t', '\t')] of
        Just e -> quoted (e : acc) rest
        Nothing -> Left ("unknown escape \\" <> T.singleton c)
      c : rest -> quoted (c : acc) rest
      [] -> Left "unterminated string"

-- | The inverse of 'parseInvocation'.
renderInvocation :: Invocation -> Text
renderInvocation (Invocation name args) = T.unwords (name : map quoteArg args)

quoteArg :: Text -> Text
quoteArg t
  | not (T.null t) && T.all (\c -> not (isSpace c) && c /= '"' && c /= '\\') t = t
  | otherwise = "\"" <> T.concatMap esc t <> "\""
  where
    esc = \case
      '"' -> "\\\""
      '\\' -> "\\\\"
      '\n' -> "\\n"
      '\t' -> "\\t"
      c -> T.singleton c

