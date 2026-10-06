-- | REPLs (ADR repl): what starts one for a language, and the state of a REPL
-- buffer. Pure; the transcript operations are in "Him.Transcript",
-- the process in "Him.Repl.Process", the actions in "Him.Actions.Repl".
module Him.Repl
  ( ReplConfig (..)
  , ReplTable
  , defaultRepls
  , ReplState (..)
  , ReplStatus (..)
  , newReplState
  , wrapCode
  ) where

import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Text (Text)
import Data.Text qualified as T
import Him.Position (Pos (..))

-- | How to run a language's REPL (@[repl.<language>]@ in the config).
data ReplConfig = ReplConfig
  { rcCommand :: !FilePath
  , rcArgs :: ![String]
  , rcRoots :: ![FilePath]
  -- ^ Files that mark the project's root, where the REPL runs (so
  -- @stack ghci@ loads the project); without one, the file's directory.
  , rcMultiline :: !(Maybe (Text, Text))
  -- ^ Lines put around code of several lines (ghci's @:{@ and @:}@).
  , rcReload :: !(Maybe Text)
  -- ^ What reloads the project (ghci's @:reload@).
  , rcReloadOnSave :: !Bool
  -- ^ Send 'rcReload' after a file of the language is saved.
  }
  deriving stock (Eq, Show)

type ReplTable = Map Text ReplConfig

-- | The built-in REPLs, by language name ("Him.Language").
defaultRepls :: ReplTable
defaultRepls =
  Map.fromList
    [ ("haskell", ReplConfig "stack" ["ghci"] ["stack.yaml", "cabal.project"] (Just (":{", ":}")) (Just ":reload") True)
    , ("python", ReplConfig "python3" ["-i", "-q", "-u"] ["pyproject.toml", "setup.py", ".git"] (Just ("", "")) Nothing False)
    , ("javascript", ReplConfig "node" ["-i"] ["package.json"] Nothing Nothing False)
    ]

data ReplStatus
  = ReplStarting
  | ReplRunning
  | -- | It exited (or never started), and why.
    ReplStopped !Text
  deriving stock (Eq, Show)

-- | A REPL buffer: the transcript is the document's text; what is typed
-- after 'rsInput' is the next input.
data ReplState = ReplState
  { rsLanguage :: !Text
  , rsInput :: !Pos
  , rsStatus :: !ReplStatus
  , rsConfig :: !(Maybe ReplConfig)
  -- ^ Known once it runs (the runtime picks it from the table).
  , rsSeenSaves :: !Int
  -- ^ Saves of the language's files already followed by a reload.
  }
  deriving stock (Eq, Show)

newReplState :: Text -> ReplState
newReplState language = ReplState language (Pos 0 0) ReplStarting Nothing 0

-- | Code as it is sent: several lines are wrapped in the REPL's multi-line
-- markers, if it has them; one line is sent as it is.
wrapCode :: Maybe ReplConfig -> Text -> Text
wrapCode config code = case rcMultiline =<< config of
  Just (open, close) | T.any (== '\n') body -> open <> "\n" <> body <> "\n" <> close <> "\n"
  _ -> body <> "\n"
  where
    body = T.dropWhileEnd (== '\n') code
