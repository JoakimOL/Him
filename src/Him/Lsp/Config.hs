-- | Which language server serves a language, and how to find a project's
-- root (ADR-29). Built in for now; a config file can extend it later.
module Him.Lsp.Config
  ( ServerConfig (..)
  , serverFor
  ) where

import Data.Text (Text)
import Him.Language (Language (..))

data ServerConfig = ServerConfig
  { scCommand :: !FilePath
  , scArgs :: ![String]
  , scRoots :: ![FilePath]
  -- ^ Files that mark a project root (the nearest directory with one wins).
  , scLanguageId :: !Text
  -- ^ The LSP language id for @didOpen@.
  }
  deriving stock (Eq, Show)

serverFor :: Language -> Maybe ServerConfig
serverFor language = case langName language of
  "haskell" -> Just (ServerConfig "haskell-language-server-wrapper" ["--lsp"] ["hie.yaml", "stack.yaml", "cabal.project", "package.yaml"] "haskell")
  "rust" -> Just (ServerConfig "rust-analyzer" [] ["Cargo.toml"] "rust")
  "c" -> Just (ServerConfig "clangd" [] ["compile_commands.json", ".clangd", "compile_flags.txt", "CMakeLists.txt", "Makefile"] "c")
  "cpp" -> Just (ServerConfig "clangd" [] ["compile_commands.json", ".clangd", "compile_flags.txt", "CMakeLists.txt", "Makefile"] "cpp")
  "typescript" -> ts "typescript"
  "tsx" -> ts "typescriptreact"
  "javascript" -> ts "javascript"
  "python" -> Just (ServerConfig "pylsp" [] ["pyproject.toml", "setup.py", "setup.cfg"] "python")
  "go" -> Just (ServerConfig "gopls" [] ["go.mod"] "go")
  _ -> Nothing
  where
    ts lid = Just (ServerConfig "typescript-language-server" ["--stdio"] ["tsconfig.json", "jsconfig.json", "package.json"] lid)
