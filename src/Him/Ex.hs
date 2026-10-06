-- | @:@ commands ("ex commands"), such as @:w file@ or @:q!@.
module Him.Ex
  ( ExCommand (..)
  , ExArgs (..)
  , parseExLine
  , runExLine
  , previewedTheme
  ) where

import Data.List (find)
import Data.Text (Text)
import Data.Text qualified as T
import Him.Editor (Editor (..), PromptKind (..))
import Him.EditorM (EditorM, failWith)
import Him.Mode (Mode (..))

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

-- | The theme to preview while the @:@ line names one, as in Helix: the
-- argument of a command taking a theme ('ThemeArgs').
previewedTheme :: [ExCommand] -> Editor -> Maybe Text
previewedTheme table ed = case (edMode ed, edPrompt ed, parseExLine (edCmdLine ed)) of
  (CmdLine, ExPrompt, Just (name, [arg]))
    | Just c <- find ((name `elem`) . exNames) table
    , exArgs c == ThemeArgs ->
        Just arg
  _ -> Nothing
