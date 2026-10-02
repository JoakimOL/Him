-- | The REPL plugin (ADR-38): a REPL for the file's language in a window
-- beside it. Type into it like any buffer (@ret@ in insert mode sends the
-- line), or send the selection from a file (@space e@). @space E@ reloads
-- the project, which also happens after saving a file of its language
-- (ghci's @:reload@), so a REPL started in the project (@stack ghci@)
-- follows the code as it is written.
module Him.Actions.Repl
  ( replPlugin
  ) where

import Control.Monad.Trans.State.Strict (get, gets, modify')
import Data.IntMap.Strict qualified as IntMap
import Data.List (find, findIndex)
import Data.Map.Strict qualified as Map
import Data.Text (Text)
import Data.Text qualified as T
import Him.Action
import Him.Buffer qualified as Buffer
import Him.Config (Plugin (..), plugin)
import Him.Document (DocKind (..), Document (..), newDocument)
import Him.Edit (selectionText)
import Him.Editor
import Him.EditorM
import Him.Effect (Effect (..), JobResult (..))
import Him.Ex (ExArgs (..), ExCommand (..))
import Him.Language (detectLanguage, langName, languages)
import Him.Mode (Mode (..))
import Him.Position (Pos (..))
import Him.Repl
import Him.Transcript
import Him.Selection (Range (..), point, primary, rangeEnd, rangeStart, ranges, single)
import Him.Syntax (SyntaxInfo (..))
import Him.View (View (..))
import Him.Window (Axis (..), Box (..), Window (..))

replPlugin :: Plugin
replPlugin =
  (plugin "repl" "A REPL beside the file (:repl): type into it, or send the selection (space e)")
    { plActions = actions
    , plBindings =
        Map.fromList
          [ (Normal, [("space e", "repl_send"), ("space E", "repl_reload")])
          , (Repl, [("ret", "repl_submit"), ("C-c", "repl_interrupt")])
          ]
    , plExCommands = exCommands
    , plHousekeeping = reloadAfterSaves
    , plJobResult = applyReplResult
    , plDisable = do
        ids <- gets (map docId . filter isRepl . allDocuments)
        mapM_ (request . ReplStop) ids
        mapM_ (\i -> modify' (modifyDocument i (setStatus (ReplStopped "the repl plugin was switched off")))) ids
    }

actions :: [Action]
actions =
  [ simple "repl_open" GMisc "Open the REPL of this file's language beside it, and go there" (openRepl True)
  , simple "repl_send" GMisc "Send the selection (or the line) to the REPL of this file's language" sendSelection
  , simple "repl_reload" GMisc "Reload the project in the REPL (ghci's :reload)" reload
  , simple "repl_submit" GMisc "Send what was typed in the REPL buffer" submit
  , simple "repl_interrupt" GMisc "Interrupt the REPL (Ctrl-C)" (withCurrentRepl (request . ReplInterrupt))
  ]

exCommands :: [ExCommand]
exCommands =
  [ ExCommand ["repl"] "Open the REPL of this file's language (or of the language named) beside it" NoArgs $ \case
      [] -> openRepl True
      [lang] -> () <$ ensureRepl True lang ""
      _ -> failWith "usage: :repl [language]"
  , ExCommand ["repl-send"] "Send text to the REPL of this file's language (e.g. :repl-send main)" NoArgs $ \args ->
      withLanguage $ \lang file -> sendCode lang file (T.unwords args)
  , ExCommand ["repl-reload"] "Reload the project in the REPL" NoArgs $ \_ -> reload
  , ExCommand ["repl-interrupt"] "Interrupt the REPL" NoArgs $ \_ -> withCurrentRepl (request . ReplInterrupt)
  , ExCommand ["repl-stop"] "Stop the REPL" NoArgs $ \_ -> withCurrentRepl $ \i -> do
      request (ReplStop i)
      modify' (modifyDocument i (setStatus (ReplStopped "stopped")))
  , ExCommand ["repl-restart"] "Restart the REPL" NoArgs $ \_ -> withLanguage $ \lang file -> do
      found <- gets (findRepl lang)
      case found of
        Just d -> restart d file
        Nothing -> () <$ ensureRepl False lang file
  ]

isRepl :: Document -> Bool
isRepl d = case docKind d of
  ReplDoc _ -> True
  _ -> False

setStatus :: ReplStatus -> Document -> Document
setStatus st d = case docKind d of
  ReplDoc rs -> d {docKind = ReplDoc rs {rsStatus = st}}
  _ -> d

-- | The REPL buffer of a language.
findRepl :: Text -> Editor -> Maybe Document
findRepl lang ed = find (\d -> (rsLanguage <$> replState d) == Just lang) (allDocuments ed)

-- | The current file's language and path (a REPL buffer's own language).
withLanguage :: (Text -> FilePath -> EditorM ()) -> EditorM ()
withLanguage k = do
  d <- getDoc
  case replState d of
    Just rs -> k (rsLanguage rs) ""
    Nothing -> case docPath d >>= \p -> (,p) . langName <$> detectLanguage languages p (Buffer.lineAt 0 (docBuffer d)) of
      Just (lang, path) -> k lang path
      Nothing -> failWith "no language for this buffer (:repl <language> opens one)"

-- | The REPL of the current file's language, if one is open.
withCurrentRepl :: (Int -> EditorM ()) -> EditorM ()
withCurrentRepl k = withLanguage $ \lang _ ->
  gets (findRepl lang) >>= \case
    Just d -> k (docId d)
    Nothing -> failWith ("no REPL for " <> lang <> " (:repl opens one)")

openRepl :: Bool -> EditorM ()
openRepl focus = withLanguage $ \lang file -> () <$ ensureRepl focus lang file

-- | Make sure the language's REPL runs and is shown in a window (a new one
-- beside the current, if needed). With @focus@ the REPL's window is
-- focused; otherwise the focus stays. Its buffer's id.
ensureRepl :: Bool -> Text -> FilePath -> EditorM Int
ensureRepl focus lang file = do
  from <- gets edFocus
  existing <- gets (findRepl lang)
  i <- case existing of
    Just d -> do
      shown <- gets (windowShowing (docId d))
      case shown of
        Just w -> modify' (focusWindow w)
        Nothing -> modify' (showBuffer (docId d) . splitWindow Beside)
      case replState d of
        Just rs | ReplStopped _ <- rsStatus rs -> restart d file
        _ -> pure ()
      pure (docId d)
    Nothing -> do
      saves <- gets (savesOf lang)
      let doc = (newDocument Nothing Buffer.empty) {docKind = ReplDoc (newReplState lang) {rsSeenSaves = saves}}
      modify' (openBuffer doc . splitWindow Beside)
      i <- gets (docId . edDoc)
      request (ReplStart i lang file)
      info ("starting the " <> lang <> " REPL")
      pure i
  if focus then pure () else modify' (focusWindow from)
  pure i
  where
    showBuffer i ed = maybe ed (`gotoBuffer` ed) (findIndex ((== i) . docId) (allDocuments ed))

restart :: Document -> FilePath -> EditorM ()
restart d file = do
  modify' (modifyDocument (docId d) (setStatus ReplStarting))
  request (ReplStart (docId d) (maybe "" rsLanguage (replState d)) file)

-- | Send code to the language's REPL, shown in its buffer as if typed.
sendCode :: Text -> FilePath -> Text -> EditorM ()
sendCode lang file code = do
  i <- ensureRepl False lang file
  modify' (modifyDocument i (insertOutput (T.dropWhileEnd (== '\n') code <> "\n")))
  request (ReplSend i True code)
  modify' followOutput'
  where
    followOutput' ed = maybe ed (\d -> followOutput (docId d) ed) (findRepl lang ed)

-- | @space e@: the selection, or the line when only one character is
-- selected.
sendSelection :: EditorM ()
sendSelection = do
  d <- getDoc
  let buf = docBuffer d
      prim = primary (docSelection d)
      code
        | rangeStart prim == rangeEnd prim = Buffer.lineAt (posLine (rangeHead prim)) buf
        | otherwise = T.intercalate "\n" (map (selectionText buf) (ranges (docSelection d)))
  if T.null (T.strip code)
    then failWith "nothing to send"
    else withLanguage $ \lang file -> sendCode lang file code

reload :: EditorM ()
reload = withCurrentRepl $ \i -> do
  ed <- get
  case find ((== i) . docId) (allDocuments ed) >>= replState >>= rsConfig >>= rcReload of
    Nothing -> failWith "this REPL has no reload command (set reload in [repl.<language>])"
    Just cmd -> sendTo i cmd

-- | Send a command to a REPL buffer, echoed.
sendTo :: Int -> Text -> EditorM ()
sendTo i cmd = do
  modify' (modifyDocument i (insertOutput (cmd <> "\n")))
  request (ReplSend i False (cmd <> "\n"))
  modify' (followOutput i)

-- | @ret@ in a REPL buffer.
submit :: EditorM ()
submit = do
  d <- getDoc
  case takeInput d of
    Nothing -> pure ()
    Just (input, d') -> do
      modifyDoc (const d')
      case rsStatus <$> replState d of
        Just (ReplStopped why) -> failWith ("the REPL is not running (" <> why <> "; :repl-restart)")
        _ -> request (ReplSend (docId d) False (input <> "\n"))

-- | Saves of the language's files (as far as highlighting has named their
-- language).
savesOf :: Text -> Editor -> Int
savesOf lang ed = sum [docSaves d | d <- allDocuments ed, siLanguage (docSyntax d) == Just lang]

-- | After every event: a REPL whose language's files were saved since it
-- last reloaded reloads (when its config says so).
reloadAfterSaves :: EditorM ()
reloadAfterSaves = do
  ed <- get
  sequence_
    [ do
        modify' (modifyDocument (docId d) (\doc -> doc {docKind = ReplDoc rs {rsSeenSaves = saves}}))
        sendTo (docId d) cmd
    | d <- allDocuments ed
    , Just rs <- [replState d]
    , rsStatus rs == ReplRunning
    , Just config <- [rsConfig rs]
    , rcReloadOnSave config
    , Just cmd <- [rcReload config]
    , let saves = savesOf (rsLanguage rs) ed
    , saves > rsSeenSaves rs
    ]

applyReplResult :: JobResult -> EditorM ()
applyReplResult = \case
  ReplStarted i config root -> do
    modify' $ modifyDocument i $ \d -> case docKind d of
      ReplDoc rs -> insertOutput ("[" <> T.pack (unwords (rcCommand config : rcArgs config)) <> " in " <> T.pack root <> "]\n") d {docKind = ReplDoc rs {rsStatus = ReplRunning, rsConfig = Just config}}
      _ -> d
    modify' (followOutput i)
  ReplOutput i out -> modify' (followOutput i . modifyDocument i (insertOutput (cleanOutput out)))
  ReplExited i why -> modify' (followOutput i . modifyDocument i (insertOutput ("\n[exited: " <> why <> "]\n") . setStatus (ReplStopped why)))
  _ -> pure ()

-- | Windows that show a REPL buffer but are not focused follow its end.
followOutput :: Int -> Editor -> Editor
followOutput i ed = case find ((== i) . docId) (allDocuments ed) of
  Nothing -> ed
  Just d ->
    let end = Buffer.endPos (docBuffer d)
        height w = maybe 10 (\b -> max 1 (boxHeight b - 1)) (lookup w (windowBoxes ed))
        follow w win
          | winDoc win == i = win {winView = (winView win) {viewTop = max 0 (posLine end - height w + 1)}, winSelection = single (point end)}
          | otherwise = win
     in ed {edWindows = IntMap.mapWithKey follow (edWindows ed)}
