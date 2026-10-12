-- | Languages: how a file's language is recognised, and what each
-- syntax provider calls it (ADR syntax-providers). The table is built in; a config file
-- can extend it later.
module Him.Language
  ( Language (..)
  , languages
  , detectLanguage
  , grammarFor
  ) where

import Control.Applicative ((<|>))
import Data.List (find)
import Data.Text (Text)
import Data.Text qualified as T
import System.FilePath (takeExtension, takeFileName)

data Language = Language
  { langName :: !Text
  , langExtensions :: ![Text]
  -- ^ Without the dot.
  , langFileNames :: ![Text]
  , langShebangs :: ![Text]
  -- ^ Interpreter names, e.g. @python3@.
  , langGrammars :: ![(Text, Text)]
  -- ^ Per provider (@"tree-sitter"@, later @"textmate"@): its name for
  -- the grammar.
  , langLineComment :: !(Maybe Text)
  -- ^ What starts a comment to the end of the line (@--@).
  , langBlockComment :: !(Maybe (Text, Text))
  -- ^ What opens and closes a block comment (@{-@, @-}@).
  }
  deriving stock (Eq, Show)

-- | What a provider calls a language's grammar, if it has one.
grammarFor :: Text -> Language -> Maybe Text
grammarFor provider = lookup provider . langGrammars

lang :: Text -> [Text] -> [Text] -> [Text] -> Text -> Maybe Text -> Maybe (Text, Text) -> Language
lang name exts files shebangs grammar line block =
  Language name exts files shebangs [("tree-sitter", grammar)] line block

cStyle :: Maybe (Text, Text)
cStyle = Just ("/*", "*/")

markup :: Maybe (Text, Text)
markup = Just ("<!--", "-->")

-- | The built-in languages. Tree-sitter grammar names are Helix's.
languages :: [Language]
languages =
  [ lang "haskell" ["hs", "hs-boot", "hsc"] [] ["runhaskell", "runghc"] "haskell" (Just "--") (Just ("{-", "-}"))
  , lang "rust" ["rs"] [] [] "rust" (Just "//") cStyle
  , lang "c" ["c", "h"] [] [] "c" (Just "//") cStyle
  , lang "cpp" ["cc", "cpp", "cxx", "hpp", "hh", "hxx", "c++"] [] [] "cpp" (Just "//") cStyle
  , lang "python" ["py", "pyi"] [] ["python", "python3"] "python" (Just "#") Nothing
  , lang "javascript" ["js", "mjs", "cjs"] [] ["node"] "javascript" (Just "//") cStyle
  , lang "typescript" ["ts", "mts", "cts"] [] [] "typescript" (Just "//") cStyle
  , lang "tsx" ["tsx"] [] [] "tsx" (Just "//") cStyle
  , lang "go" ["go"] [] [] "go" (Just "//") cStyle
  , lang "java" ["java"] [] [] "java" (Just "//") cStyle
  , lang "json" ["json", "jsonc"] [] [] "json" Nothing Nothing
  , lang "toml" ["toml"] ["Cargo.lock"] [] "toml" (Just "#") Nothing
  , lang "yaml" ["yaml", "yml"] [] [] "yaml" (Just "#") Nothing
  , lang "markdown" ["md", "markdown"] [] [] "markdown" Nothing markup
  , lang "bash" ["sh", "bash", "zsh"] [".bashrc", ".zshrc", ".profile", "PKGBUILD"] ["sh", "bash", "zsh"] "bash" (Just "#") Nothing
  , lang "lua" ["lua"] [] ["lua"] "lua" (Just "--") (Just ("--[[", "]]"))
  , lang "nix" ["nix"] [] [] "nix" (Just "#") cStyle
  , lang "html" ["html", "htm"] [] [] "html" Nothing markup
  , lang "css" ["css"] [] [] "css" Nothing cStyle
  , lang "make" ["mk", "mak"] ["Makefile", "makefile", "GNUmakefile"] [] "make" (Just "#") Nothing
  , lang "cabal" ["cabal"] [] [] "cabal" (Just "--") Nothing
  , lang "ruby" ["rb"] ["Gemfile", "Rakefile"] ["ruby"] "ruby" (Just "#") Nothing
  , lang "zig" ["zig"] [] [] "zig" (Just "//") Nothing
  , lang "ocaml" ["ml", "mli"] [] [] "ocaml" Nothing (Just ("(*", "*)"))
  , lang "elixir" ["ex", "exs"] [] ["elixir"] "elixir" (Just "#") Nothing
  , lang "sql" ["sql"] [] [] "sql" (Just "--") cStyle
  , lang "dockerfile" ["dockerfile"] ["Dockerfile", "Containerfile"] [] "dockerfile" (Just "#") Nothing
  , lang "git-commit" [] ["COMMIT_EDITMSG"] [] "gitcommit" (Just "#") Nothing
  ]

-- | A file's language, from its name, else from a @#!@ first line.
detectLanguage :: [Language] -> FilePath -> Text -> Maybe Language
detectLanguage table path firstLine =
  byName <|> byExtension <|> byShebang
  where
    name = T.pack (takeFileName path)
    ext = T.toLower (T.drop 1 (T.pack (takeExtension path)))
    byName = find ((name `elem`) . langFileNames) table
    byExtension = if T.null ext then Nothing else find ((ext `elem`) . langExtensions) table
    byShebang = do
      rest <- T.stripPrefix "#!" firstLine
      let ws = T.words rest
          interpreter = case ws of
            (env : arg : _) | "env" `T.isSuffixOf` env -> arg
            (prog : _) -> T.takeWhileEnd (/= '/') prog
            [] -> ""
          base = T.dropWhileEnd (\c -> c == '.' || (c >= '0' && c <= '9')) interpreter
      find (\l -> interpreter `elem` langShebangs l || base `elem` langShebangs l) table
