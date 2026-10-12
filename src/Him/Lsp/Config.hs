-- | Which language server serves a language, and how to find a project's
-- root (ADR lsp-client), and the settings it gets. Built in; the config
-- file's @[language-server.<language>]@ changes them ("Him.UserConfig").
module Him.Lsp.Config
  ( ServerConfig (..)
  , ServerTable
  , defaultServers
  , serverFor
  , server
  , settingsFor
  ) where

import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Text qualified as T
import Him.Json (Value (..), key)
import Him.Language (Language (..))

data ServerConfig = ServerConfig
  { scCommand :: !FilePath
  , scArgs :: ![String]
  , scRoots :: ![FilePath]
  -- ^ Files that mark a project root (the nearest directory with one wins).
  , scLanguageId :: !Text
  -- ^ The LSP language id for @didOpen@.
  , scSettings :: !Value
  -- ^ The server's settings (an object, e.g. @{"haskell": {"formattingProvider":
  -- "fourmolu"}}@): its answer to @workspace/configuration@, and sent with
  -- @workspace/didChangeConfiguration@ when it starts.
  , scInitOptions :: !(Maybe Value)
  -- ^ @initializationOptions@ for @initialize@.
  }
  deriving stock (Eq, Show)

-- | Servers by language name (the config file can change it).
type ServerTable = Map Text ServerConfig

serverFor :: ServerTable -> Language -> Maybe ServerConfig
serverFor table language = Map.lookup (langName language) table

-- | A server with no settings.
server :: FilePath -> [String] -> [FilePath] -> Text -> ServerConfig
server command args roots languageId = ServerConfig command args roots languageId (JObject []) Nothing

-- | The answer to a @workspace/configuration@ item: the settings under its
-- dotted section (all of them without one), or null.
settingsFor :: Value -> Maybe Text -> Value
settingsFor settings = \case
  Nothing -> settings
  Just section -> fromMaybe JNull (foldl (\v k -> v >>= key k) (Just settings) (T.splitOn "." section))

-- | The built-in servers.
defaultServers :: ServerTable
defaultServers =
  Map.fromList
    [ ("haskell", server "haskell-language-server-wrapper" ["--lsp"] ["hie.yaml", "stack.yaml", "cabal.project", "package.yaml"] "haskell")
    , ("rust", server "rust-analyzer" [] ["Cargo.toml"] "rust")
    , ("c", clangd "c")
    , ("cpp", clangd "cpp")
    , ("typescript", ts "typescript")
    , ("tsx", ts "typescriptreact")
    , ("javascript", ts "javascript")
    , ("python", server "pylsp" [] ["pyproject.toml", "setup.py", "setup.cfg"] "python")
    , ("go", server "gopls" [] ["go.mod"] "go")
    ]
  where
    clangd = server "clangd" [] ["compile_commands.json", ".clangd", "compile_flags.txt", "CMakeLists.txt", "Makefile"]
    ts = server "typescript-language-server" ["--stdio"] ["tsconfig.json", "jsconfig.json", "package.json"]
