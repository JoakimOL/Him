-- | @.gitignore@ and @.ignore@ files (see ADR gitignore-matcher in docs/PLAN.md).
--
-- The syntax is git's: blank lines and @#@ comments are skipped, @!@
-- re-includes, a trailing @/@ matches directories only, and a pattern with
-- a @/@ at the start or in the middle is anchored to the directory of its
-- file; otherwise it matches a name at any depth. Globs: @*@ and @?@ (not
-- across @/@), @[a-z]@ / @[!a-z]@, and @**@ (@**/x@, @x/**@, @a/**/b@).
--
-- Several files apply at once ('Ignorer'): the last matching rule of the
-- most specific file wins, and @.ignore@ beats @.gitignore@ in the same
-- directory. A directory that is ignored is not entered, so, as in git,
-- nothing inside it can be re-included.
module Him.Ignore
  ( Rule (..)
  , Ignorer
  , Scope (..)
  , parseIgnore
  , matchRules
  , isIgnored
  , ignoreFileNames
  ) where

import Data.Char (isSpace)
import Data.List (isPrefixOf, tails)
import Data.Maybe (listToMaybe, mapMaybe)
import Data.Text (Text)
import Data.Text qualified as T

-- | Read in this order, so later files take precedence.
ignoreFileNames :: [FilePath]
ignoreFileNames = [".gitignore", ".ignore"]

data Rule = Rule
  { ruleNegated :: !Bool
  , ruleDirOnly :: !Bool
  , ruleGlob :: ![Tok]
  -- ^ Matched against the path relative to the ignore file's directory.
  }
  deriving stock (Eq, Show)

data Tok
  = Lit !Char
  | -- | @?@: one character other than @/@.
    One
  | -- | @*@: any run without @/@.
    Star
  | -- | @**@ not followed by @/@: anything.
    AnyAll
  | -- | @**/@: nothing, or any directories ending in @/@.
    Dirs
  | -- | @[...]@: negated?, ranges.
    Class !Bool ![(Char, Char)]
  deriving stock (Eq, Show)

parseIgnore :: Text -> [Rule]
parseIgnore = mapMaybe (parseLine . T.unpack) . T.lines

parseLine :: String -> Maybe Rule
parseLine raw = case trimEnd (filter (/= '\r') raw) of
  "" -> Nothing
  '#' : _ -> Nothing
  '!' : rest -> rule True rest
  line -> rule False line
  where
    rule neg body =
      let dirOnly = "/" `isSuffix` body
          body' = if dirOnly then init body else body
          anchored = '/' `elem` body'
          body'' = dropWhile (== '/') body'
          toks = compile body''
       in if null body''
            then Nothing
            else Just (Rule neg dirOnly (if anchored then toks else Dirs : toks))
    isSuffix s x = reverse s `isPrefixOf` reverse x
    -- Trailing spaces are dropped unless escaped with a backslash.
    trimEnd s = case span isSpace (reverse s) of
      (sp, '\\' : r) | not (null sp) -> reverse r <> " "
      (_, r) -> reverse r

compile :: String -> [Tok]
compile = \case
  [] -> []
  '*' : '*' : '/' : rest -> Dirs : compile rest
  '*' : '*' : rest -> AnyAll : compile (dropWhile (== '*') rest)
  '*' : rest -> Star : compile rest
  '?' : rest -> One : compile rest
  '\\' : c : rest -> Lit c : compile rest
  '[' : rest | Just (cls, rest') <- charClass rest -> cls : compile rest'
  c : rest -> Lit c : compile rest

-- | The inside of @[...]@, if it is closed.
charClass :: String -> Maybe (Tok, String)
charClass s0 =
  let (neg, s1) = case s0 of
        c : r | c == '!' || c == '^' -> (True, r)
        _ -> (False, s0)
   in go neg [] True s1
  where
    go neg acc first = \case
      ']' : rest | not first -> Just (Class neg (reverse acc), rest)
      a : '-' : b : rest | b /= ']' -> go neg ((a, b) : acc) False rest
      a : rest -> go neg ((a, a) : acc) False rest
      [] -> Nothing

-- | Does the glob match the whole path?
glob :: [Tok] -> String -> Bool
glob toks path = case (toks, path) of
  ([], s) -> null s
  (Lit c : ts, x : xs) -> c == x && glob ts xs
  (One : ts, x : xs) -> x /= '/' && glob ts xs
  (Class neg rs : ts, x : xs) -> x /= '/' && any (\(a, b) -> a <= x && x <= b) rs /= neg && glob ts xs
  (Star : ts, s) -> any (glob ts) (s : [drop n s | n <- [1 .. length (takeWhile (/= '/') s)]])
  (AnyAll : ts, s) -> any (glob ts) (tails s)
  (Dirs : ts, s) -> glob ts s || or [glob ts rest | ('/' : rest) <- tails s]
  _ -> False

-- | The verdict of one file's rules for a path relative to its directory:
-- @Just True@ ignored, @Just False@ re-included, 'Nothing' no rule matched.
matchRules :: [Rule] -> FilePath -> Bool -> Maybe Bool
matchRules rules path isDir =
  listToMaybe [not (ruleNegated r) | r <- reverse rules, isDir || not (ruleDirOnly r), glob (ruleGlob r) path]

-- | Where a rule set's file is, relative to the walk's root.
data Scope
  = -- | In this directory below the root (@""@: the root itself).
    Below !FilePath
  | -- | In an ancestor of the root; the root is at this path inside it
    -- (e.g. @"src/Him"@ when the walk starts two levels below).
    Above !FilePath
  deriving stock (Eq, Show)

-- | Rule sets, least specific first.
type Ignorer = [(Scope, [Rule])]

-- | Is a path (relative to the walk's root) ignored?
isIgnored :: Ignorer -> FilePath -> Bool -> Bool
isIgnored ignorer path isDir =
  case mapMaybe verdict (reverse ignorer) of
    v : _ -> v
    [] -> False
  where
    verdict (scope, rules) = do
      rel <- case scope of
        Below base -> relativeTo base path
        Above prefix -> Just (prefix <> "/" <> path)
      matchRules rules rel isDir
    relativeTo "" p = Just p
    relativeTo base p
      | (base <> "/") `isPrefixOf` p = Just (drop (length base + 1) p)
      | otherwise = Nothing
