-- | REPLs: the transcript, wrapping code, and a real process (@cat@
-- standing in for a REPL: it answers with what it is sent).
module Test.Repl
  ( replTests
  ) where

import Control.Monad.Trans.State.Strict (execStateT)
import Data.Foldable (foldlM)
import Data.List (find)
import Data.Map.Strict qualified as Map
import Data.Maybe (fromMaybe)
import Data.Text qualified as T
import Him.Buffer qualified as B
import Him.Config (Config (..))
import Him.Config.Default (defaultConfig)
import Him.Document
import Him.Editor
import Him.Event (Event (..))
import Him.Key (parseKeys)
import Him.Position (Pos (..))
import Him.Repl
import Him.Transcript
import Him.Selection (point, primary, rangeHead, single)
import Him.Session (handleEvent)
import Test.Harness
import Test.Util

replTests :: IO [Test]
replTests = do
  defaults <- either (fail . show) pure defaultConfig
  let config = defaults {cfgRepls = Map.singleton "python" (ReplConfig "cat" [] [] (Just ("<<", ">>")) (Just "again") False)}
      file = (newDocument (Just "t.py") (buf "one\ntwo")) {docId = 1}
      typeKeys ks ed = foldlM (\e k -> execStateT (handleEvent config (EvKey k)) e) ed (fromMaybe (error ("bad keys: " <> show ks)) (parseKeys ks))
      transcript ed = maybe "" (B.toText . docBuffer) (find ((/= Nothing) . replState) (allDocuments ed))
  rt <- testRuntime config
  let settleRepl = settleUntil config rt 5000
  opened <- settleRepl (T.isInfixOf "[cat" . transcript) =<< typeKeys ": r e p l ret" (newEditor (20, 80) file)
  typed <- settleRepl (T.isInfixOf "hi\nhi\n" . transcript) =<< typeKeys "i h i ret" opened
  -- Back in the file: the line, then two lines (wrapped).
  sentLine <- settleRepl (T.isInfixOf "one\none\n" . transcript) =<< typeKeys "esc C-w h space e" typed
  sentBlock <- settleRepl (T.isInfixOf "<<\none\ntwo\n>>\n" . transcript) =<< typeKeys "x x space e" sentLine
  reloaded <- settleRepl (T.isInfixOf "again\nagain\n" . transcript) =<< typeKeys ": r e p l - r e l o a d ret" sentBlock
  stopTyped <- typeKeys ": r e p l - s t o p ret" reloaded
  stopped <- settleRepl (T.isInfixOf "[exited" . transcript) stopTyped
  let replDoc = (newDocument Nothing (buf "> ")) {docKind = ReplDoc (newReplState "x") {rsInput = Pos 0 2}}
      typing = replDoc {docBuffer = buf "> 1+", docSelection = single (point (Pos 0 4))}
      withOutput = insertOutput "out\n" typing
  -- A REPL buffer: a line of output, then the prompt, the input after it.
  let transcriptDoc = (newDocument Nothing (buf "out\n> ")) {docKind = ReplDoc (newReplState "x") {rsInput = Pos 1 2}}
      inRepl = newEditor (20, 80) transcriptDoc
      text ed = B.toText (docBuffer (edDoc ed))
  deleteOutput <- typeKeys "x d" inRepl
  typedFromTop <- typeKeys "i a b esc" inRepl
  backIntoPrompt <- typeKeys "i a backspace backspace esc" inRepl
  yanked <- typeKeys "x y" inRepl
  undone <- typeKeys "i a b esc u" inRepl
  pure
    [ test "the transcript cannot be deleted (only the input after the prompt)" $
        assertEqual ("out\n> ", True) (text deleteOutput, maybe False (\(Status _ m) -> "only the input" `T.isInfixOf` m) (edStatus deleteOutput))
    , test "insert mode from up in the transcript types into the input" (assertEqual "out\n> ab" (text typedFromTop))
    , test "backspace stops at the prompt" (assertEqual "out\n> " (text backIntoPrompt))
    , test "the transcript can be selected and yanked" (assertEqual (Just ["out\n"]) (Map.lookup '"' (edRegisters yanked)))
    , test "undo takes back typing in the input" (assertEqual "out\n> " (text undone))
    , test "output goes before the input, and the cursor moves with the input" $
        assertEqual ("> out\n1+", Pos 1 2) (B.toText (docBuffer withOutput), rangeHead (primary (docSelection withOutput)))
    , test "ret takes what was typed after the output" $
        assertEqual (Just ("1+", "> 1+\n")) (fmap (\(i, d) -> (i, B.toText (docBuffer d))) (takeInput typing))
    , test "output is not an edit (no undo step, not dirty)" $
        assertEqual (False, docHistory typing) (docDirty withOutput, docHistory withOutput)
    , test "code of several lines is wrapped; one line is not" $
        assertEqual [":{\na\nb\n:}\n", "a\n", "a\nb\n"] [wrapCode (Just ghci) "a\nb\n", wrapCode (Just ghci) "a", wrapCode Nothing "a\nb"]
    , test "a transcript typed in is never unsaved (quitting does not ask)" $
        assertEqual (False, True) (unsaved replDoc {docDirty = True}, unsaved (newDocument Nothing (buf "x")) {docDirty = True})
    , test "escape sequences and carriage returns are dropped from output" $
        assertEqual "red\nline\n" (cleanOutput "\ESC[31mred\ESC[0m\r\n\ESC]0;title\aline\n")
    , test ":repl opens a REPL beside the file and starts it" $
        assertEqual (True, 2) ("[cat" `T.isInfixOf` transcript opened, length (allDocuments opened))
    , test "typing in the REPL buffer and ret sends the line" (assertEqual True (T.isInfixOf "hi\nhi\n" (transcript typed)))
    , test "space e sends the line, or the selection wrapped" $
        assertEqual (True, True) (T.isInfixOf "one\none\n" (transcript sentLine), T.isInfixOf "<<\none\ntwo\n>>\n" (transcript sentBlock))
    , test "space e keeps the focus in the file" (assertEqual (Just "t.py") (docPath (edDoc sentBlock)))
    , test ":repl-reload sends the reload command" (assertEqual True (T.isInfixOf "again\nagain\n" (transcript reloaded)))
    , test ":repl-stop ends it" (assertEqual True (T.isInfixOf "[exited" (transcript stopped)))
    ]
  where
    ghci = ReplConfig "ghci" [] [] (Just (":{", ":}")) Nothing False

