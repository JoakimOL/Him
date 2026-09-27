-- | The @:@ command line: entering it, editing it, and running it.
module Him.Commands.CommandLine
  ( commands
  , cmdlineInsert
  ) where

import Control.Monad.Trans.State.Strict (gets, modify')
import Data.Text qualified as T
import Him.Command
import Him.Editor (Editor (..))
import Him.Ex (ExCommand, runExLine)
import Him.Mode (Mode (..))

commands :: [ExCommand] -> [Command]
commands exTable =
  [ Command "command_mode" "Enter a : command" $ do
      setCmdLine ""
      setMode CmdLine
  , Command "cmdline_cancel" "Leave the command line" $ do
      setCmdLine ""
      setMode Normal
  , Command "cmdline_backspace" "Delete the last character (leave if empty)" $
      gets edCmdLine >>= \case
        t | T.null t -> setMode Normal
        t -> setCmdLine (T.dropEnd 1 t)
  , Command "cmdline_execute" "Run the typed command" $ do
      line <- gets edCmdLine
      setCmdLine ""
      setMode Normal
      runExLine exTable line
  ]

cmdlineInsert :: Char -> EditorM ()
cmdlineInsert c = modify' (\e -> e {edCmdLine = T.snoc (edCmdLine e) c})

setCmdLine :: T.Text -> EditorM ()
setCmdLine t = modify' (\e -> e {edCmdLine = t})
