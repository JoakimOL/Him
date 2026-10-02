-- | @:@ commands ("ex commands"), such as @:w file@ or @:q!@.
module Him.Ex
  ( ExCommand (..)
  , ExArgs (..)
  , parseExLine
  , runExLine
  ) where

import Data.List (find)
import Data.Text (Text)
import Data.Text qualified as T
import Him.EditorM (EditorM, failWith)

data ExCommand = ExCommand
  { exNames :: ![Text]
  -- ^ The full name first, then aliases: @["write", "w"]@.
  , exDoc :: !Text
  , exArgs :: !ExArgs
  , exRun :: [Text] -> EditorM ()
  -- ^ Receives the whitespace-separated arguments.
  }

-- | What the arguments are, for completion: paths, theme names, or one of
-- a fixed list of names.
data ExArgs = NoArgs | PathArgs | ThemeArgs | NameArgs [Text]
  deriving stock (Eq, Show)

-- | Split a command line into a command name and its arguments.
parseExLine :: Text -> Maybe (Text, [Text])
parseExLine line = case T.words line of
  (name : args) -> Just (name, args)
  [] -> Nothing

runExLine :: [ExCommand] -> Text -> EditorM ()
runExLine table line = case parseExLine line of
  Nothing -> pure ()
  Just (name, args) -> case find ((name `elem`) . exNames) table of
    Nothing -> failWith ("unknown command: " <> name)
    Just c -> exRun c args
