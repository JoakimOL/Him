-- | Languages: how a file's language is recognised, and what each
-- syntax provider calls it (ADR-26). The table is built in; a config file
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
  , langComment :: !(Maybe Text)
  }
  deriving stock (Eq, Show)

-- | What a provider calls a language's grammar, if it has one.
grammarFor :: Text -> Language -> Maybe Text
grammarFor provider = lookup provider . langGrammars

lang :: Text -> [Text] -> [Text] -> [Text] -> Text -> Maybe Text -> Language
lang name exts files shebangs grammar comment =
  Language name exts files shebangs [("tree-sitter", grammar)] comment

-- | The built-in languages. Tree-sitter grammar names are Helix's.
languages :: [Language]
languages =
  [ lang "haskell" ["hs", "hs-boot", "hsc"] [] ["runhaskell", "runghc"] "haskell" (Just "--")
  , lang "rust" ["rs"] [] [] "rust" (Just "//")
  , lang "c" ["c", "h"] [] [] "c" (Just "//")
  , lang "cpp" ["cc", "cpp", "cxx", "hpp", "hh", "hxx", "c++"] [] [] "cpp" (Just "//")
  , lang "python" ["py", "pyi"] [] ["python", "python3"] "python" (Just "#")
  , lang "javascript" ["js", "mjs", "cjs"] [] ["node"] "javascript" (Just "//")
  , lang "typescript" ["ts", "mts", "cts"] [] [] "typescript" (Just "//")
  , lang "tsx" ["tsx"] [] [] "tsx" (Just "//")
  , lang "go" ["go"] [] [] "go" (Just "//")
  , lang "java" ["java"] [] [] "java" (Just "//")
  , lang "json" ["json", "jsonc"] [] [] "json" Nothing
  , lang "toml" ["toml"] ["Cargo.lock"] [] "toml" (Just "#")
  , lang "yaml" ["yaml", "yml"] [] [] "yaml" (Just "#")
  , lang "markdown" ["md", "markdown"] [] [] "markdown" Nothing
  , lang "bash" ["sh", "bash", "zsh"] [".bashrc", ".zshrc", ".profile", "PKGBUILD"] ["sh", "bash", "zsh"] "bash" (Just "#")
  , lang "lua" ["lua"] [] ["lua"] "lua" (Just "--")
  , lang "nix" ["nix"] [] [] "nix" (Just "#")
  , lang "html" ["html", "htm"] [] [] "html" Nothing
  , lang "css" ["css"] [] [] "css" Nothing
  , lang "make" ["mk", "mak"] ["Makefile", "makefile", "GNUmakefile"] [] "make" (Just "#")
  , lang "cabal" ["cabal"] [] [] "cabal" (Just "--")
  , lang "ruby" ["rb"] ["Gemfile", "Rakefile"] ["ruby"] "ruby" (Just "#")
  , lang "zig" ["zig"] [] [] "zig" (Just "//")
  , lang "ocaml" ["ml", "mli"] [] [] "ocaml" Nothing
  , lang "elixir" ["ex", "exs"] [] ["elixir"] "elixir" (Just "#")
  , lang "sql" ["sql"] [] [] "sql" (Just "--")
  , lang "dockerfile" ["dockerfile"] ["Dockerfile", "Containerfile"] [] "dockerfile" (Just "#")
  , lang "git-commit" [] ["COMMIT_EDITMSG"] [] "git-commit" (Just "#")
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
