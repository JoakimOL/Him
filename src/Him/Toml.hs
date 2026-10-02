-- | A small TOML reader for the config file (ADR-32). It reads the part of
-- TOML a config needs, into the JSON value type ("Him.Json"): tables
-- become objects (keys in file order), arrays arrays.
--
-- Supported: comments, @[table]@ and @[dotted.\"quoted\".table]@ headers,
-- bare and quoted keys (dotted keys on the left too), basic strings with
-- escapes, literal strings, integers, booleans, arrays and inline tables
-- (both also over several lines, as Helix's themes write them). Not
-- supported: dates, floats, multi-line strings, arrays of tables. Errors
-- name the line.
module Him.Toml
  ( parseToml
  , quoteKey
  , quoteString
  ) where

import Control.Monad (foldM)
import Data.Char (chr, digitToInt, isAlphaNum, isDigit, isHexDigit)
import Data.Text (Text)
import Data.Text qualified as T
import Him.Json (Value (..))

-- | Parse a document into a table.
parseToml :: Text -> Either Text Value
parseToml src = do
  (table, _) <- foldM step (JObject [], []) (logicalLines (zip [1 :: Int ..] (T.lines src)))
  pure table
  where
    step (doc, current) (n, line) =
      let t = T.strip (stripComment line)
          located e = Left ("line " <> T.pack (show n) <> ": " <> e)
       in if T.null t
            then Right (doc, current)
            else case T.uncons t of
              Just ('[', rest)
                | "[" `T.isPrefixOf` rest -> located "arrays of tables ([[…]]) are not supported"
                | Just inner <- T.stripSuffix "]" rest -> do
                    path <- either located Right (keyPath (T.strip inner))
                    doc' <- either located Right (ensureTable path doc)
                    Right (doc', path)
                | otherwise -> located "a table header needs a closing ]"
              _ -> do
                (keys, rest) <- either located Right (keyThenEquals t)
                (v, after) <- either located Right (value (T.strip rest))
                if not (T.null (T.strip (stripComment after)))
                  then located ("unexpected text after the value: " <> T.strip after)
                  else do
                    doc' <- either located Right (insertAt (current <> keys) v doc)
                    Right (doc', current)

-- | Join lines that continue an array or inline table (an open @[@ or @{@
-- in a value) into one (comments are dropped first, so one cannot swallow
-- the rest).
logicalLines :: [(Int, Text)] -> [(Int, Text)]
logicalLines = go . map (fmap stripComment)
  where
    go [] = []
    go ((n, l) : rest)
      | d > 0 && not (isHeader l) =
          let (more, rest') = collect d rest
           in (n, T.intercalate " " (l : more)) : go rest'
      | otherwise = (n, l) : go rest
      where
        d = depth l
    collect _ [] = ([], [])
    collect d ((_, l) : rest)
      | d + depth l <= 0 = ([l], rest)
      | otherwise = let (more, rest') = collect (d + depth l) rest in (l : more, rest')
    isHeader l = "[" `T.isPrefixOf` T.stripStart l
    -- Open minus closed brackets outside strings (comments are gone).
    -- Most lines have none, and are not walked.
    depth l
      | T.any (\c -> c == '[' || c == ']' || c == '{' || c == '}') l = count (0 :: Int) False False (T.unpack l)
      | otherwise = 0
    count acc _ _ [] = acc
    count acc inStr esc (c : cs)
      | inStr = if esc then count acc True False cs else if c == '\\' then count acc True True cs else count acc (c /= '"') False cs
      | c == '"' = count acc True False cs
      | c == '\'' = count acc False False (drop 1 (dropWhile (/= '\'') cs))
      | c == '[' || c == '{' = count (acc + 1) False False cs
      | c == ']' || c == '}' = count (acc - 1) False False cs
      | otherwise = count acc False False cs

-- | Drop a comment, minding strings. A line without @#@ is left alone.
stripComment :: Text -> Text
stripComment t
  | T.any (== '#') t = T.pack (go False False (T.unpack t))
  | otherwise = t
  where
    go _ _ [] = []
    go inStr esc (c : cs)
      | inStr = c : if esc then go True False cs else if c == '\\' then go True True cs else go (c /= '"') False cs
      | c == '"' = c : go True False cs
      | c == '\'' = let (lit, rest) = break (== '\'') cs in c : lit <> take 1 rest <> go False False (drop 1 rest)
      | c == '#' = []
      | otherwise = c : go False False cs

-- | A dotted key, then @=@.
keyThenEquals :: Text -> Either Text ([Text], Text)
keyThenEquals t = do
  (keys, rest) <- keys' t
  case T.uncons (T.stripStart rest) of
    Just ('=', after) -> Right (keys, after)
    _ -> Left "expected = after the key"

keyPath :: Text -> Either Text [Text]
keyPath t = do
  (ks, rest) <- keys' t
  if T.null (T.strip rest) then Right ks else Left ("unexpected text in the table header: " <> rest)

-- | One or more keys separated by dots.
keys' :: Text -> Either Text ([Text], Text)
keys' t = do
  (k, rest) <- oneKey (T.stripStart t)
  case T.uncons (T.stripStart rest) of
    Just ('.', after) -> do
      (ks, rest') <- keys' after
      Right (k : ks, rest')
    _ -> Right ([k], rest)

oneKey :: Text -> Either Text (Text, Text)
oneKey t = case T.uncons t of
  Just ('"', _) -> basicString t
  Just ('\'', rest) -> let (lit, after) = T.break (== '\'') rest in if T.null after then Left "unterminated key" else Right (lit, T.drop 1 after)
  _ ->
    let (k, rest) = T.span (\c -> isAlphaNum c || c == '_' || c == '-') t
     in if T.null k then Left ("expected a key, got: " <> T.take 20 t) else Right (k, rest)

-- | A value, and what follows it.
value :: Text -> Either Text (Value, Text)
value t = case T.uncons t of
  Just ('"', _) -> (\(s, rest) -> (JString s, rest)) <$> basicString t
  Just ('\'', rest) -> let (lit, after) = T.break (== '\'') rest in if T.null after then Left "unterminated string" else Right (JString lit, T.drop 1 after)
  Just ('[', rest) -> array (T.stripStart rest) []
  Just ('{', rest) -> inline (T.stripStart rest) (JObject [])
  _
    | Just rest <- T.stripPrefix "true" t -> Right (JBool True, rest)
    | Just rest <- T.stripPrefix "false" t -> Right (JBool False, rest)
    | otherwise ->
        let (num, rest) = T.span (\c -> isDigit c || c == '-' || c == '+' || c == '_') t
            digits = T.filter (/= '_') (T.dropWhile (== '+') num)
         in case reads (T.unpack digits) of
              [(n, "")] -> Right (JInt n, rest)
              _ -> Left ("expected a value (a string, number, boolean, array or table), got: " <> T.take 20 t)
  where
    array s acc = case T.uncons s of
      Just (']', rest) -> Right (JArray (reverse acc), rest)
      _ -> do
        (v, rest) <- value s
        let rest' = T.stripStart rest
        case T.uncons rest' of
          Just (',', after) -> array (T.stripStart after) (v : acc)
          Just (']', after) -> Right (JArray (reverse (v : acc)), after)
          _ -> Left "expected , or ] in the array"
    -- An inline table: @{ key = value, a.b = value }@ (a trailing comma
    -- is allowed, as TOML 1.1 does).
    inline s acc = case T.uncons s of
      Just ('}', rest) -> Right (acc, rest)
      _ -> do
        (keys, rest) <- keyThenEquals s
        (v, after) <- value (T.strip rest)
        acc' <- insertAt keys v acc
        case T.uncons (T.stripStart after) of
          Just (',', more) -> inline (T.stripStart more) acc'
          Just ('}', more) -> Right (acc', more)
          _ -> Left "expected , or } in the inline table"

-- | A double-quoted string with escapes. One without escapes (the usual
-- case) is a slice.
basicString :: Text -> Either Text (Text, Text)
basicString t = case T.uncons stop of
  Just ('"', after) -> Right (plain, after)
  _ -> go [] (T.unpack body)
  where
    body = T.drop 1 t
    (plain, stop) = T.break (\c -> c == '"' || c == '\\') body
    go acc = \case
      '"' : rest -> Right (T.pack (reverse acc), T.pack rest)
      '\\' : c : rest -> case c of
        'n' -> go ('\n' : acc) rest
        't' -> go ('\t' : acc) rest
        'r' -> go ('\r' : acc) rest
        '"' -> go ('"' : acc) rest
        '\\' -> go ('\\' : acc) rest
        'u' | (hex, rest') <- splitAt 4 rest, length hex == 4, all isHexDigit hex -> go (chr (foldl (\n d -> n * 16 + digitToInt d) 0 hex) : acc) rest'
        _ -> Left ("unknown escape \\" <> T.singleton c)
      c : rest -> go (c : acc) rest
      [] -> Left "unterminated string"

-- | Make sure a table exists at a path (creating the missing ones).
ensureTable :: [Text] -> Value -> Either Text Value
ensureTable [] v = Right v
ensureTable (k : ks) (JObject kvs) = case lookup k kvs of
  Just sub@(JObject _) -> (\sub' -> JObject (replace k sub' kvs)) <$> ensureTable ks sub
  Just _ -> Left (k <> " is not a table")
  Nothing -> (\sub' -> JObject (kvs <> [(k, sub')])) <$> ensureTable ks (JObject [])
ensureTable _ _ = Left "not a table"

-- | Set a key (at a path) to a value; setting a key twice is an error.
insertAt :: [Text] -> Value -> Value -> Either Text Value
insertAt [] _ _ = Left "empty key"
insertAt [k] v (JObject kvs) = case lookup k kvs of
  Just _ -> Left (quoteKey k <> " is set twice")
  Nothing -> Right (JObject (kvs <> [(k, v)]))
insertAt (k : ks) v (JObject kvs) = case lookup k kvs of
  Just sub@(JObject _) -> (\sub' -> JObject (replace k sub' kvs)) <$> insertAt ks v sub
  Just _ -> Left (k <> " is not a table")
  Nothing -> (\sub' -> JObject (kvs <> [(k, sub')])) <$> insertAt ks v (JObject [])
insertAt _ _ _ = Left "not a table"

replace :: Text -> Value -> [(Text, Value)] -> [(Text, Value)]
replace k v = map (\(k', v') -> if k' == k then (k, v) else (k', v'))

-- | A key as TOML writes it: bare when it can be, quoted otherwise.
quoteKey :: Text -> Text
quoteKey k
  | not (T.null k) && T.all (\c -> isAlphaNum c || c == '_' || c == '-') k = k
  | otherwise = quoteString k

-- | A basic string.
quoteString :: Text -> Text
quoteString s = "\"" <> T.concatMap esc s <> "\""
  where
    esc = \case
      '"' -> "\\\""
      '\\' -> "\\\\"
      '\n' -> "\\n"
      '\t' -> "\\t"
      c
        | c < ' ' -> T.pack ("\\u" <> pad (hex (fromEnum c)))
        | otherwise -> T.singleton c
    hex n = let (q, r) = n `divMod` 16 in (if q > 0 then hex q else "") <> ["0123456789ABCDEF" !! r]
    pad h = replicate (4 - length h) '0' <> h
