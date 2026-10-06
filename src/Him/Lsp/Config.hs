-- | Which language server serves a language, and how to find a project's
-- root (ADR lsp-client). Built in for now; a config file can extend it later.
module Him.Lsp.Config
  ( ServerConfig (..)
  , ServerTable
  , defaultServers
  , serverFor
  ) where

import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
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

-- | Servers by language name (the config file can change it).
type ServerTable = Map Text ServerConfig

serverFor :: ServerTable -> Language -> Maybe ServerConfig
serverFor table language = Map.lookup (langName language) table

-- | The built-in servers.
defaultServers :: ServerTable
defaultServers =
  Map.fromList
    [ ("haskell", ServerConfig "haskell-language-server-wrapper" ["--lsp"] ["hie.yaml", "stack.yaml", "cabal.project", "package.yaml"] "haskell")
    , ("rust", ServerConfig "rust-analyzer" [] ["Cargo.toml"] "rust")
    , ("c", clangd "c")
    , ("cpp", clangd "cpp")
    , ("typescript", ts "typescript")
    , ("tsx", ts "typescriptreact")
    , ("javascript", ts "javascript")
    , ("python", ServerConfig "pylsp" [] ["pyproject.toml", "setup.py", "setup.cfg"] "python")
    , ("go", ServerConfig "gopls" [] ["go.mod"] "go")
    ]
  where
    clangd = ServerConfig "clangd" [] ["compile_commands.json", ".clangd", "compile_flags.txt", "CMakeLists.txt", "Makefile"]
    ts = ServerConfig "typescript-language-server" ["--stdio"] ["tsconfig.json", "jsconfig.json", "package.json"]
