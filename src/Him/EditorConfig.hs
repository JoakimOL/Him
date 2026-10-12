-- | A project's indentation from its @.editorconfig@ files
-- (https://editorconfig.org): @indent_style@, @indent_size@ and
-- @tab_width@. The other properties are not read.
module Him.EditorConfig
  ( editorConfigIndent
  , parseEditorConfig
  , sectionMatches
  , indentHint
  , EditorConfig (..)
  ) where

import Control.Exception (IOException, try)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.IO qualified as TIO
import Data.Text.Read qualified as TR
import Him.Indent (IndentHint (..))
import System.Directory (doesFileExist, makeAbsolute)
import System.FilePath (makeRelative, takeDirectory, (</>))

data EditorConfig = EditorConfig
  { ecRoot :: !Bool
  -- ^ @root = true@: files further up are not read.
  , ecSections :: ![(Text, [(Text, Text)])]
  -- ^ Each section's glob and its properties (names and values lower case).
  }
  deriving stock (Eq, Show)

-- | The indentation the @.editorconfig@ files above a file give it.
editorConfigIndent :: FilePath -> IO IndentHint
editorConfigIndent file = do
  path <- makeAbsolute file
  configs <- collect (takeDirectory path)
  -- The nearest file's sections come last, so they win.
  pure (indentHint [kv | (dir, ec) <- configs, (glob, kvs) <- ecSections ec, sectionMatches glob (makeRelative dir path), kv <- kvs])
  where
    collect dir = do
      let file' = dir </> ".editorconfig"
      exists <- doesFileExist file'
      found <- if exists then either (const Nothing) (Just . parseEditorConfig) <$> try @IOException (TIO.readFile file') else pure Nothing
      let parent = takeDirectory dir
          up = if parent == dir then pure [] else collect parent
      case found of
        Just ec | ecRoot ec -> pure [(dir, ec)]
        Just ec -> (<> [(dir, ec)]) <$> up
        Nothing -> up

-- | The indentation that properties give, later ones winning.
indentHint :: [(Text, Text)] -> IndentHint
indentHint kvs =
  IndentHint
    { hintTabs = case final "indent_style" of
        Just "tab" -> Just True
        Just "space" -> Just False
        _ -> Nothing
    , hintWidth = case final "indent_size" of
        Just "tab" -> number =<< final "tab_width"
        Just n -> number n
        Nothing -> number =<< final "tab_width"
    }
  where
    final k = case [v | (k', v) <- kvs, k' == k] of
      [] -> Nothing
      vs -> Just (last vs)
    number t = case TR.decimal t of
      Right (n, rest) | T.null rest, n > 0 -> Just n
      _ -> Nothing

parseEditorConfig :: Text -> EditorConfig
parseEditorConfig src = EditorConfig root (reverse (map finish sections))
  where
    (preamble, sections) = foldl step ([], []) (map T.strip (T.lines src))
    root = lookup "root" preamble == Just "true"
    finish (glob, kvs) = (glob, reverse kvs)
    step (pre, secs) line
      | T.null line || T.head line `elem` ['#', ';'] = (pre, secs)
      | "[" `T.isPrefixOf` line && "]" `T.isSuffixOf` line = (pre, (T.drop 1 (T.dropEnd 1 line), []) : secs)
      | otherwise = case T.breakOn "=" line of
          (_, "") -> (pre, secs)
          (k, v) ->
            let kv = (T.toLower (T.strip k), T.toLower (T.strip (T.drop 1 v)))
             in case secs of
                  [] -> (kv : pre, secs)
                  (g, kvs) : rest -> (pre, (g, kv : kvs) : rest)

-- | Whether a section's glob matches a path relative to the directory of
-- its file. A glob without a slash matches a file name in any directory.
sectionMatches :: Text -> FilePath -> Bool
sectionMatches glob path = any (\g -> match (T.unpack g) path) (expand anchored)
  where
    anchored
      | "/" `T.isPrefixOf` glob = T.drop 1 glob
      | T.any (== '/') glob = glob
      | otherwise = "**/" <> glob

-- | @{a,b}@ alternatives, expanded (nested ones too); @{1..3}@ ranges are
-- not supported.
expand :: Text -> [Text]
expand g = case T.breakOn "{" g of
  (_, "") -> [g]
  (before, rest) -> case closing (T.drop 1 rest) of
    Nothing -> [g]
    Just (inside, after) -> [before <> alt <> tl | alt <- concatMap expand (splitTop inside), tl <- expand after]
  where
    closing t = go (0 :: Int) 0 t
      where
        go depth i s = case T.uncons s of
          Nothing -> Nothing
          Just ('}', _) | depth == 0 -> Just (T.take i t, T.drop (i + 1) t)
          Just ('}', s') -> go (depth - 1) (i + 1) s'
          Just ('{', s') -> go (depth + 1) (i + 1) s'
          Just (_, s') -> go depth (i + 1) s'
    splitTop t = go (0 :: Int) "" (T.unpack t)
      where
        go _ acc [] = [T.pack (reverse acc)]
        go 0 acc (',' : cs) = T.pack (reverse acc) : go 0 "" cs
        go d acc (c : cs) = go (if c == '{' then d + 1 else if c == '}' then d - 1 else d) (c : acc) cs

-- | Glob matching: @*@ (not across @/@), @**@ (across), @?@, @[abc]@,
-- @[!abc]@ and @[a-z]@. @**/@ also matches no directory at all.
match :: String -> String -> Bool
match pat str = case pat of
  [] -> null str
  '*' : '*' : '/' : rest -> match rest str || any (match rest) (afterSlashes str)
  '*' : '*' : rest -> any (match rest) (suffixes str)
  '*' : rest -> any (\i -> match rest (drop i str)) [0 .. length (takeWhile (/= '/') str)]
  '?' : rest -> case str of
    c : cs | c /= '/' -> match rest cs
    _ -> False
  '[' : rest | Just (set, rest') <- charClass rest -> case str of
    c : cs | c /= '/' && set c -> match rest' cs
    _ -> False
  p : rest -> case str of
    c : cs -> p == c && match rest cs
    [] -> False
  where
    suffixes s = [drop i s | i <- [0 .. length s]]
    afterSlashes s = [drop 1 r | r <- suffixes s, take 1 r == "/"]

-- | A @[...]@ class (the opening bracket already read): its test and what
-- follows the closing bracket.
charClass :: String -> Maybe (Char -> Bool, String)
charClass s0 =
  let (negated, s) = case s0 of
        '!' : r -> (True, r)
        r -> (False, r)
   in case break (== ']') s of
        (_, []) -> Nothing
        (body, _ : rest) ->
          let test c = inClass c body
           in Just (if negated then not . test else test, rest)
  where
    inClass c = \case
      a : '-' : b : more | b /= ']' -> (c >= a && c <= b) || inClass c more
      x : more -> c == x || inClass c more
      [] -> False
