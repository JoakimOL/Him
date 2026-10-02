-- | The @:@ command line: entering it, editing it, and running it.
module Him.Commands.CommandLine
  ( actions
  , cmdlineInsert
  ) where

import Control.Monad.Trans.State.Strict (gets, modify')
import Data.Text qualified as T
import Him.Action
import Him.Command
import Him.Commands.Search (cancelSearch, executeSearch, executeSelect)
import Him.Editor (Editor (..), PromptKind (..))
import Him.Ex (ExCommand, runExLine)
import Him.Mode (Mode (..))

actions :: [ExCommand] -> [Action]
actions exTable =
  [ simple "command_mode" GPrompt "Enter a : command" $ do
      modify' (\e -> e {edPrompt = ExPrompt})
      setCmdLine ""
      setMode CmdLine
  , simple "cmdline_cancel" GPrompt "Leave the command line" cancel
  , simple "cmdline_backspace" GPrompt "Delete the last character (leave if empty)" $
      gets edCmdLine >>= \case
        t | T.null t -> cancel
        t -> setCmdLine (T.dropEnd 1 t)
  , simple "cmdline_execute" GPrompt "Run the typed command" $ do
      line <- gets edCmdLine
      prompt <- gets edPrompt
      setCmdLine ""
      setMode Normal
      case prompt of
        ExPrompt -> runExLine exTable line
        SearchPrompt dir origin -> executeSearch dir origin line
        SelectPrompt origin -> executeSelect origin line
  , action "ex" GPrompt "Run a : command, e.g. ex \"w\"" (text "command") (runExLine exTable)
  ]

cancel :: EditorM ()
cancel = do
  prompt <- gets edPrompt
  setCmdLine ""
  setMode Normal
  case prompt of
    SearchPrompt _ origin -> cancelSearch origin
    SelectPrompt origin -> cancelSearch origin
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
        SelectPrompt {} -> True
        ExPrompt -> False
    }
