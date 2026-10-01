-- | The @:@ command line: entering it, editing it, and running it.
module Him.Commands.CommandLine
  ( commands
  , cmdlineInsert
  ) where

import Control.Monad.Trans.State.Strict (gets, modify')
import Data.Text qualified as T
import Him.Command
import Him.Commands.Search (cancelSearch, executeSearch)
import Him.Editor (Editor (..), PromptKind (..))
import Him.Ex (ExCommand, runExLine)
import Him.Mode (Mode (..))

commands :: [ExCommand] -> [Command]
commands exTable =
  [ Command "command_mode" "Enter a : command" $ do
      modify' (\e -> e {edPrompt = ExPrompt})
      setCmdLine ""
      setMode CmdLine
  , Command "cmdline_cancel" "Leave the command line" cancel
  , Command "cmdline_backspace" "Delete the last character (leave if empty)" $
      gets edCmdLine >>= \case
        t | T.null t -> cancel
        t -> setCmdLine (T.dropEnd 1 t)
  , Command "cmdline_execute" "Run the typed command" $ do
      line <- gets edCmdLine
      prompt <- gets edPrompt
      setCmdLine ""
      setMode Normal
      case prompt of
        ExPrompt -> runExLine exTable line
        SearchPrompt dir origin -> executeSearch dir origin line
  ]

cancel :: EditorM ()
cancel = do
  prompt <- gets edPrompt
  setCmdLine ""
  setMode Normal
  case prompt of
    SearchPrompt _ origin -> cancelSearch origin
    ExPrompt -> pure ()

cmdlineInsert :: Char -> EditorM ()
cmdlineInsert c = setCmdLine . (`T.snoc` c) =<< gets edCmdLine

-- | Changing a search's text schedules the incremental preview.
setCmdLine :: T.Text -> EditorM ()
setCmdLine t = modify' $ \e ->
  e
    { edCmdLine = t
    , edPreviewPending = case edPrompt e of
        SearchPrompt {} -> True
        ExPrompt -> False
    }
