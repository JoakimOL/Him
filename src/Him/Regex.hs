-- | A small backtracking regular expression engine (ADR-28), enough for
-- tree-sitter query predicates (@#match?@, @#lua-match?@) and, later,
-- regex search. There is no regex library among GHC's boot libraries.
--
-- Supported: literals and escapes, @.@, classes @[a-z]@ / @[^…]@ with
-- @\\d \\w \\s@ (and their negations), anchors @^ $ \\b@, groups (capturing
-- or @(?:…)@), alternation, and @* + ? {m} {m,} {m,n}@, greedy or lazy
-- (a trailing @?@). Not supported: backreferences and lookaround.
module Him.Regex
  ( Regex
  , compileRegex
  , compileLua
  , matchesRegex
  , findRegex
  ) where

import Data.Char (isAlphaNum, isDigit, isSpace, isUpper, toLower)
import Data.Text (Text)
import Data.Text qualified as T

data Regex = Regex !Node
  deriving stock (Show)

data Node
  = Empty
  | Lit !Char
  | AnyChar
  | Set !Bool ![Item]
  -- ^ Negated?, items.
  | LineStart
  | LineEnd
  | WordBoundary
  | Seq ![Node]
  | Alt ![Node]
  | Repeat !Int !(Maybe Int) !Bool !Node
  -- ^ Minimum, maximum, greedy.
  deriving stock (Show)

data Item = Range !Char !Char | Predicate !Char
  -- ^ A class letter: d w s D W S (and Lua's a l u p x c).
  deriving stock (Show)

-- * Parsing

compileRegex :: Text -> Either Text Regex
compileRegex src = case alternation (T.unpack src) of
  Right (node, []) -> Right (Regex node)
  Right (_, rest) -> Left ("regex: unexpected " <> T.pack (take 10 rest))
  Left e -> Left e

type P a = String -> Either Text (a, String)

alternation :: P Node
alternation s = do
  (first, rest) <- sequence' s
  case rest of
    '|' : rest' -> do
      (other, rest'') <- alternation rest'
      let others = case other of
            Alt ns -> ns
            n -> [n]
      pure (Alt (first : others), rest'')
    _ -> pure (first, rest)

sequence' :: P Node
sequence' = go []
  where
    go acc s = case s of
      [] -> done acc s
      c : _ | c == '|' || c == ')' -> done acc s
      _ -> do
        (atom, rest) <- atomP s
        (node, rest') <- quantifier atom rest
        go (node : acc) rest'
    done acc s = Right (Seq (reverse acc), s)

quantifier :: Node -> P Node
quantifier atom s = case s of
  '*' : rest -> lazy 0 Nothing rest
  '+' : rest -> lazy 1 Nothing rest
  '?' : rest -> lazy 0 (Just 1) rest
  '{' : rest
    | (digits, rest1) <- span isDigit rest
    , not (null digits) ->
        let lo = read digits
         in case rest1 of
              '}' : rest2 -> lazy lo (Just lo) rest2
              ',' : '}' : rest2 -> lazy lo Nothing rest2
              ',' : rest2
                | (digits2, '}' : rest3) <- span isDigit rest2
                , not (null digits2) ->
                    lazy lo (Just (read digits2)) rest3
              _ -> Right (atom, s)
  _ -> Right (atom, s)
  where
    lazy lo hi rest = case rest of
      '?' : rest' -> Right (Repeat lo hi False atom, rest')
      _ -> Right (Repeat lo hi True atom, rest)

atomP :: P Node
atomP = \case
  '(' : '?' : ':' : rest -> group rest
  '(' : rest -> group rest
  '[' : rest -> setP rest
  '.' : rest -> Right (AnyChar, rest)
  '^' : rest -> Right (LineStart, rest)
  '$' : rest -> Right (LineEnd, rest)
  '\\' : 'b' : rest -> Right (WordBoundary, rest)
  '\\' : c : rest
    | c `elem` ("dwsDWS" :: String) -> Right (Set False [Predicate c], rest)
    | otherwise -> Right (Lit (escaped c), rest)
  ['\\'] -> Left "regex: trailing backslash"
  c : rest -> Right (Lit c, rest)
  [] -> Right (Empty, [])
  where
    group s = do
      (node, rest) <- alternation s
      case rest of
        ')' : rest' -> Right (node, rest')
        _ -> Left "regex: missing )"

escaped :: Char -> Char
escaped = \case
  'n' -> '\n'
  't' -> '\t'
  'r' -> '\r'
  c -> c

setP :: P Node
setP s0 =
  let (neg, s1) = case s0 of
        '^' : r -> (True, r)
        _ -> (False, s0)
   in go neg [] True s1
  where
    go neg acc first = \case
      ']' : rest | not first -> Right (Set neg (reverse acc), rest)
      '\\' : c : rest
        | c `elem` ("dwsDWS" :: String) -> go neg (Predicate c : acc) False rest
        | otherwise -> range neg acc (escaped c) rest
      c : rest -> range neg acc c rest
      [] -> Left "regex: missing ]"
    range neg acc a = \case
      '-' : b : rest | b /= ']' -> go neg (Range a b : acc) False rest
      rest -> go neg (Range a a : acc) False rest

-- | A Lua pattern (tree-sitter's @#lua-match?@), translated: @%a %d %l %s
-- %u %w %p %x %c@ classes (upper case negates), @%@ escapes, and @-@ for a
-- lazy @*@.
compileLua :: Text -> Either Text Regex
compileLua = compileRegex . T.pack . go False . T.unpack
  where
    go inSet = \case
      '%' : c : rest
        | Just (neg, cls) <- luaClass c ->
            (if inSet then cls else "[" <> (if neg then "^" else "") <> cls <> "]") <> go inSet rest
        | otherwise -> '\\' : c : go inSet rest
      '[' : rest | not inSet -> '[' : go True rest
      ']' : rest | inSet -> ']' : go False rest
      '-' : rest | not inSet -> "*?" <> go inSet rest
      c : rest
        | not inSet && c `elem` ("(){}|" :: String) -> '\\' : c : go inSet rest
        | otherwise -> c : go inSet rest
      [] -> []
    -- Lua classes as set contents (ASCII), and whether upper case negated
    -- them (only honoured outside a set).
    luaClass c =
      (\cls -> (isUpper c, cls)) <$> lookup (toLower c) classes
    classes =
      [ ('a', "a-zA-Z")
      , ('d', "0-9")
      , ('l', "a-z")
      , ('u', "A-Z")
      , ('s', " \\t\\n\\r\f\v")
      , ('w', "a-zA-Z0-9")
      , ('x', "0-9a-fA-F")
      , ('p', "!-/:-@\\[-`{-~")
      , ('c', "\0-\31")
      ]

-- * Matching

-- | Does the regex match anywhere in the text?
matchesRegex :: Regex -> Text -> Bool
matchesRegex re t = case findRegex re t of
  Just _ -> True
  Nothing -> False

-- | The first match: start and end (character offsets).
findRegex :: Regex -> Text -> Maybe (Int, Int)
findRegex (Regex node) t = go 0 Nothing (T.unpack t)
  where
    go i prev rest = case run node i prev rest (\j _ -> Just j) of
      Just j -> Just (i, j)
      Nothing -> case rest of
        c : rest' -> go (i + 1) (Just c) rest'
        [] -> Nothing

-- | Continuation-passing matcher: position, previous character, input,
-- continuation (gets the end position and the rest).
run :: Node -> Int -> Maybe Char -> String -> (Int -> String -> Maybe Int) -> Maybe Int
run node i prev input k = case node of
  Empty -> k i input
  Lit c -> one (== c)
  AnyChar -> one (/= '\n')
  Set neg items -> one (\c -> any (itemMatches c) items /= neg)
  LineStart -> if prev == Nothing || prev == Just '\n' then k i input else Nothing
  LineEnd -> case input of
    [] -> k i input
    '\n' : _ -> k i input
    _ -> Nothing
  WordBoundary ->
    let before = maybe False isWord prev
        after = case input of
          c : _ -> isWord c
          [] -> False
     in if before /= after then k i input else Nothing
  Seq ns -> seqRun ns i prev input k
  Alt ns -> firstJust [run n i prev input k | n <- ns]
  Repeat lo hi greedy n -> repeatRun lo hi greedy n i prev input k
  where
    one p = case input of
      c : rest | p c -> k (i + 1) rest
      _ -> Nothing

seqRun :: [Node] -> Int -> Maybe Char -> String -> (Int -> String -> Maybe Int) -> Maybe Int
seqRun [] i _ input k = k i input
seqRun (n : ns) i prev input k =
  run n i prev input (\j rest -> seqRun ns j (lastChar i j prev input) rest k)

-- | The character before position j, having consumed input from i.
lastChar :: Int -> Int -> Maybe Char -> String -> Maybe Char
lastChar i j prev input = if j == i then prev else Just (input !! (j - i - 1))

repeatRun :: Int -> Maybe Int -> Bool -> Node -> Int -> Maybe Char -> String -> (Int -> String -> Maybe Int) -> Maybe Int
repeatRun lo hi greedy n = go 0
  where
    go count i prev input k
      | count < lo = run n i prev input (\j rest -> if j == i then Nothing else go (count + 1) j (lastChar i j prev input) rest k)
      | Just h <- hi, count >= h = k i input
      | greedy = more `orTry` k i input
      | otherwise = k i input `orTry` more
      where
        -- Each further repetition must consume something (no empty loops).
        more = run n i prev input (\j rest -> if j == i then Nothing else go (count + 1) j (lastChar i j prev input) rest k)
    orTry (Just x) _ = Just x
    orTry Nothing y = y

firstJust :: [Maybe a] -> Maybe a
firstJust = \case
  Just x : _ -> Just x
  Nothing : rest -> firstJust rest
  [] -> Nothing

itemMatches :: Char -> Item -> Bool
itemMatches c = \case
  Range a b -> a <= c && c <= b
  Predicate p -> case p of
    'd' -> isDigit c
    'w' -> isWord c
    's' -> isSpace c
    'D' -> not (isDigit c)
    'W' -> not (isWord c)
    'S' -> not (isSpace c)
    _ -> False

isWord :: Char -> Bool
isWord c = isAlphaNum c || c == '_'
