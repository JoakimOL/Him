-- | The @:@ command line: entering it, editing it, and running it.
module Him.Actions.CommandLine
  ( actions
  , cmdlineInsert
  ) where

import Control.Exception (IOException, try)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.State.Strict (gets, modify')
import Data.List (find, isPrefixOf, sort)
import Data.Text qualified as T
import System.Directory (doesDirectoryExist, listDirectory)
import Him.Action
import Him.EditorM
import Him.Actions.Directory (runFileAction)
import Him.Actions.Lsp qualified as Lsp
import Him.Actions.Search (cancelSearch, executeSearch, executeSelect)
import Him.Editor (CmdCompletions (..), Editor (..), PromptKind (..))
import Him.Ex (ExArgs (..), ExCommand (..), runExLine)
import Him.Theme.Load (themeNames)
import Him.Mode (Mode (..))

actions :: [ExCommand] -> [Action]
actions exTable =
  [ simple "command_mode" GPrompt "Enter a : command" $ do
      modify' (\e -> e {edPrompt = ExPrompt})
      setCmdLine ""
      setMode CmdLine
  , action "command_mode_with" GPrompt "Enter a : command that starts with this text (command_mode_with \"magit-commit \")" (text "text") $ \t -> do
      modify' (\e -> e {edPrompt = ExPrompt})
      setCmdLine t
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
        FilePrompt act -> runFileAction act line
        RenamePrompt at -> Lsp.renameTo at line
  , simple "cmdline_complete" GPrompt "Complete the command name or path; again: the next candidate" (complete exTable 1)
  , simple "cmdline_complete_previous" GPrompt "Complete; again: the previous candidate" (complete exTable (-1))
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
    FilePrompt _ -> pure ()
    RenamePrompt _ -> pure ()
    ExPrompt -> pure ()

cmdlineInsert :: Char -> EditorM ()
cmdlineInsert c = setCmdLine . (`T.snoc` c) =<< gets edCmdLine

-- | Changing a search's text schedules the incremental preview.
setCmdLine :: T.Text -> EditorM ()
setCmdLine t = modify' $ \e ->
  e
    { edCmdLine = t
    , edCompletions = Nothing
    , edPreviewPending = case edPrompt e of
        SearchPrompt {} -> True
        SelectPrompt {} -> True
        FilePrompt {} -> False
        RenamePrompt {} -> False
        ExPrompt -> False
    }

-- | @tab@ on the @:@ line: complete the command name, or the last argument
-- when the command takes paths. With several candidates, the line is
-- extended to their common prefix and the candidates are listed; when that
-- adds nothing, or the candidates are already listed, the line steps to
-- the next candidate (the previous one with @step = -1@), as in Helix.
complete :: [ExCommand] -> Int -> EditorM ()
complete exTable step =
  gets edCompletions >>= \case
    Just cc -> cycleTo cc
    Nothing -> do
      prompt <- gets edPrompt
      line <- gets edCmdLine
      let (name, rest) = T.break (== ' ') line
          (before, arg) = T.breakOnEnd " " line
          command = find ((name `elem`) . exNames) exTable
      case prompt of
        ExPrompt
          | T.null rest -> offer line "" [n <> " " | c <- exTable, n <- exNames c, name `T.isPrefixOf` n]
          | Just c <- command, exArgs c == PathArgs ->
              offer line before =<< liftIO (completePath (T.unpack arg))
          | Just c <- command, NameArgs names <- exArgs c ->
              offer line before [n | n <- names, arg `T.isPrefixOf` n]
          | Just c <- command, exArgs c == ThemeArgs -> do
              names <- liftIO themeNames
              offer line before [n | n <- names, arg `T.isPrefixOf` n]
        _ -> pure ()
  where
    offer _ _ [] = pure ()
    offer _ before [c] = setCmdLine (before <> c)
    offer line before cs = do
      let extended = before <> commonPrefix cs
          cc = CmdCompletions before cs (map shortName cs) Nothing
      if extended /= line
        then setCmdLine extended >> modify' (\e -> e {edCompletions = Just cc})
        else cycleTo cc
    cycleTo cc = do
      let n = length (ccCandidates cc)
          i = case ccSelected cc of
            Nothing -> if step > 0 then 0 else n - 1
            Just j -> (j + step) `mod` n
      setCmdLine (ccBefore cc <> ccCandidates cc !! i)
      modify' (\e -> e {edCompletions = Just cc {ccSelected = Just i}})
    -- A command name without its trailing space; a path's last component.
    shortName c =
      let t = T.strip c
          dir = "/" `T.isSuffixOf` t
          base = T.takeWhileEnd (/= '/') (T.dropWhileEnd (== '/') t)
       in base <> if dir then "/" else ""

commonPrefix :: [T.Text] -> T.Text
commonPrefix [] = ""
commonPrefix (c : cs) = foldl' (\p x -> maybe "" (\(q, _, _) -> q) (T.commonPrefixes p x)) c cs

-- | Paths starting with the typed text; directories end in @/@.
completePath :: FilePath -> IO [T.Text]
completePath typed = do
  let (dir, base) = splitPath typed
      listDir = if null dir then "." else dir
  entries <- either (const []) id <$> (try (listDirectory listDir) :: IO (Either IOException [FilePath]))
  let hidden e = "." `isPrefixOf` e && not ("." `isPrefixOf` base)
      matches = sort [e | e <- entries, base `isPrefixOf` e, not (hidden e)]
  traverse (\e -> do
    isDir <- doesDirectoryExist (dir <> e)
    pure (T.pack (dir <> e <> if isDir then "/" else ""))) matches
  where
    splitPath p = let (b, d) = break (== '/') (reverse p) in (reverse d, reverse b)
