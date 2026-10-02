-- | The AI chat plugin (ADR-41). @space c c@ opens the chat in a window
-- beside the code; type a message and @ret@ sends it (@A-ret@ is a line
-- break). The model reads files on its own; its edits are applied to the
-- files' buffers in the editor window, highlighted, and wait for you:
-- @space c a@ approves the next one (kept and saved), @space c d@ denies it
-- (the old lines come back), @space c A@ / @space c D@ do all of them. When
-- every edit of a turn is decided, the conversation goes on.
module Him.Actions.Chat
  ( chatPlugin
  ) where

import Control.Monad (forM, when)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.State.Strict (get, gets, modify')
import Data.List (find, findIndex)
import Data.Map.Strict qualified as Map
import Data.Maybe (isJust, isNothing)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.IO qualified as TIO
import Control.Exception (IOException, try)
import Him.Action hiding (text)
import Him.Actions.File (openFile)
import Him.Buffer qualified as Buffer
import Him.Chat
import Him.Chat.Tools
import Him.Config (Plugin (..), plugin)
import Him.Document (DocKind (..), Document (..), newDocument)
import Him.Edit (selectionText)
import Him.Editor
import Him.EditorM
import Him.Effect (Effect (..), JobResult (..))
import Him.Ex (ExArgs (..), ExCommand (..))
import Him.File (saveDocument)
import Him.FileTree (defaultWalk, listFiles)
import Him.Json (Value (..), object)
import Him.Mode (Mode (..))
import Him.Key (KeyCode (..), plain)
import Him.Position (Pos (..))
import Him.Selection (Range (..), point, primary, rangeHead, rangeStart, ranges, single)
import Him.Transcript (insertOutput, takeInput)
import Him.View (View (..))
import Him.Window (Axis (..), Box (..), Window (..))
import Data.IntMap.Strict qualified as IntMap
import System.Directory (doesFileExist, getCurrentDirectory)
import System.FilePath (isAbsolute, makeRelative, normalise, splitDirectories)

chatPlugin :: Plugin
chatPlugin =
  (plugin "chat" "An AI chat beside the code (space c c); its edits wait for your approval (space c a / d)")
    { plActions = actions
    , plBindings =
        Map.fromList
          [ ( Normal
            ,
              [ ("space c c", "chat_open")
              , ("space c a", "chat_approve")
              , ("space c d", "chat_deny")
              , ("space c A", "chat_approve_all")
              , ("space c D", "chat_deny_all")
              , ("space c s", "chat_add_selection")
              ]
            )
          , (Chat, [("ret", "chat_submit"), ("A-ret", "insert_newline"), ("C-c", "chat_cancel")])
          ]
    , plPrefixNames = [([plain (KChar ' '), plain (KChar 'c')], "chat")]
    , plExCommands = exCommands
    , plJobResult = applyChatResult
    , plDisable = do
        ids <- gets (map docId . filter isChat . allDocuments)
        mapM_ (request . ChatCancel) ids
        mapM_ (\i -> modify' (modifyChat i (\cs -> cs {csStatus = ChatIdle}))) ids
    }

actions :: [Action]
actions =
  [ simple "chat_open" GMisc "Open the AI chat beside the code, and go there" (openChat True)
  , simple "chat_submit" GMisc "Send what was typed in the chat" submit
  , simple "chat_cancel" GMisc "Stop the chat's answer" cancel
  , simple "chat_approve" GMisc "Approve the chat's next pending edit (keep and save it)" (decideNext True)
  , simple "chat_deny" GMisc "Deny the chat's next pending edit (put the old lines back)" (decideNext False)
  , simple "chat_approve_all" GMisc "Approve every pending edit of the chat" (decideAll True)
  , simple "chat_deny_all" GMisc "Deny every pending edit of the chat" (decideAll False)
  , simple "chat_add_selection" GMisc "Put the selection into the chat's message, with its file and line" addSelection
  ]

exCommands :: [ExCommand]
exCommands =
  [ ExCommand ["chat"] "Open the AI chat beside the code" NoArgs $ \_ -> openChat True
  , ExCommand ["chat-new"] "Start a new conversation (the chat buffer is cleared)" NoArgs $ \_ -> newConversation
  , ExCommand ["chat-approve"] "Approve the next pending edit (:chat-approve all: every one)" NoArgs $ \args -> if args == ["all"] then decideAll True else decideNext True
  , ExCommand ["chat-deny"] "Deny the next pending edit (:chat-deny all: every one)" NoArgs $ \args -> if args == ["all"] then decideAll False else decideNext False
  ]

-- * The chat buffer

isChat :: Document -> Bool
isChat d = case docKind d of
  ChatDoc _ -> True
  _ -> False

chatOf :: Editor -> Maybe (Document, ChatState)
chatOf ed = case [(d, cs) | d <- allDocuments ed, ChatDoc cs <- [docKind d]] of
  x : _ -> Just x
  [] -> Nothing

modifyChat :: Int -> (ChatState -> ChatState) -> Editor -> Editor
modifyChat i f = modifyDocument i $ \d -> case docKind d of
  ChatDoc cs -> d {docKind = ChatDoc (f cs)}
  _ -> d

say :: Int -> Text -> EditorM ()
say i t = modify' (followEnd i . modifyDocument i (insertOutput t))

prompt :: Text
prompt = "you> "

-- | Show the chat (beside the current window if it is not shown), and
-- with @focus@ go there to type. Its buffer's id.
openChat :: Bool -> EditorM ()
openChat focus = do
  i <- ensureChat
  when focus $ do
    w <- gets (windowShowing i)
    mapM_ (modify' . focusWindow) w
    modifyDoc (\d -> d {docSelection = single (point (Buffer.endPos (docBuffer d)))})
    setMode Insert

ensureChat :: EditorM Int
ensureChat = do
  from <- gets edFocus
  existing <- gets chatOf
  i <- case existing of
    Just (d, _) -> do
      shown <- gets (windowShowing (docId d))
      when (isNothing shown) $ modify' (showBuffer (docId d) . splitWindow Beside)
      pure (docId d)
    Nothing -> do
      let doc = (newDocument Nothing (Buffer.fromText prompt)) {docKind = ChatDoc newChatState {csInput = Pos 0 (T.length prompt)}}
      modify' (openBuffer doc . splitWindow Beside)
      gets (docId . edDoc)
  modify' (focusWindow from)
  pure i
  where
    showBuffer i ed = maybe ed (`gotoBuffer` ed) (findIndex ((== i) . docId) (allDocuments ed))

newConversation :: EditorM ()
newConversation = do
  existing <- gets chatOf
  case existing of
    Nothing -> openChat True
    Just (d, _) -> do
      request (ChatCancel (docId d))
      modify' $ modifyDocument (docId d) $ \doc ->
        doc
          { docBuffer = Buffer.fromText prompt
          , docSelection = single (point (Pos 0 (T.length prompt)))
          , docKind = ChatDoc newChatState {csInput = Pos 0 (T.length prompt)}
          , docVersion = docVersion doc + 1
          }

-- | Windows on the chat that are not focused follow its end.
followEnd :: Int -> Editor -> Editor
followEnd i ed = case find ((== i) . docId) (allDocuments ed) of
  Nothing -> ed
  Just d ->
    let end = Buffer.endPos (docBuffer d)
        height w = maybe 10 (\b -> max 1 (boxHeight b - 1)) (lookup w (windowBoxes ed))
        follow w win
          | winDoc win == i = win {winView = (winView win) {viewTop = max 0 (posLine end - height w + 1)}, winSelection = single (point end)}
          | otherwise = win
     in ed {edWindows = IntMap.mapWithKey follow (edWindows ed)}

-- * Sending

submit :: EditorM ()
submit = do
  d <- getDoc
  case docKind d of
    ChatDoc cs -> case csStatus cs of
      ChatWaiting -> failWith "the answer is still coming (C-c stops it)"
      ChatDeciding -> failWith "decide the pending edits first: space c a / space c d (A / D: all)"
      ChatIdle -> case takeInput d of
        Just (text, d') | not (T.null (T.strip text)) -> do
          modifyDoc (const d')
          context <- editorContext
          let message = object [("role", JString "user"), ("content", JArray [object [("type", JString "text"), ("text", JString (context <> T.strip text))]])]
          modify' (modifyChat (docId d) (\c -> c {csHistory = csHistory c <> [message]}))
          continue (docId d)
        _ -> pure ()
    _ -> pure ()

-- | Ask the model to go on from the history as it is now.
continue :: Int -> EditorM ()
continue i = do
  root <- liftIO getCurrentDirectory
  modify' (modifyChat i (\c -> c {csStatus = ChatWaiting}))
  say i "\nclaude> "
  history <- gets (maybe [] (csHistory . snd) . chatOf)
  request (ChatSend i (ChatRequest (systemPrompt root) history chatTools))

-- | What the user is looking at, said before their message.
editorContext :: EditorM Text
editorContext = do
  ed <- get
  pure $ case editorDocument ed of
    Just d | Just p <- docPath d -> "[I am looking at " <> T.pack p <> ", line " <> T.pack (show (posLine (rangeHead (primary (docSelection d))) + 1)) <> ".]\n\n"
    _ -> ""

-- | The document the user is looking at: the focused one, or (with the
-- chat focused) the one in another window.
editorDocument :: Editor -> Maybe Document
editorDocument ed
  | not (isChat (edDoc ed)) = Just (edDoc ed)
  | otherwise = case [d | (_, win) <- IntMap.toList (edWindows ed), d <- allDocuments ed, docId d == winDoc win, not (isChat d)] of
      d : _ -> Just d
      [] -> Nothing

cancel :: EditorM ()
cancel =
  gets chatOf >>= \case
    Just (d, cs) | csStatus cs == ChatWaiting -> do
      request (ChatCancel (docId d))
      modify' (modifyChat (docId d) (\c -> c {csStatus = ChatIdle}))
      say (docId d) ("\n[stopped]\n\n" <> prompt)
    _ -> pure ()

-- | @space c s@: the selection, with its file and line, into the message.
addSelection :: EditorM ()
addSelection = do
  d <- getDoc
  let sel = docSelection d
      code = T.intercalate "\n" (map (selectionText (docBuffer d)) (ranges sel))
      line = posLine (rangeStart (primary sel)) + 1
      header = maybe "" (\p -> T.pack p <> ":" <> T.pack (show line) <> "\n") (docPath d)
  i <- ensureChat
  modify' $ modifyDocument i $ \c ->
    let (buf, end) = Buffer.insertText (Buffer.endPos (docBuffer c)) (header <> "```\n" <> T.dropWhileEnd (== '\n') code <> "\n```\n") (docBuffer c)
     in c {docBuffer = buf, docSelection = single (point end), docVersion = docVersion c + 1}
  openChat True

-- * Answers

applyChatResult :: JobResult -> EditorM ()
applyChatResult = \case
  ChatReply i ev -> case ev of
    ChatText t -> say i t
    ChatFailed e -> do
      modify' (modifyChat i (\c -> c {csStatus = ChatIdle}))
      say i ("\n[error: " <> e <> "]\n\n" <> prompt)
    ChatFinished stop message calls -> do
      modify' (modifyChat i (\c -> c {csHistory = csHistory c <> [message]}))
      finished i stop calls
  _ -> pure ()
  where
    finished i stop calls
      | null calls = do
            modify' (modifyChat i (\c -> c {csStatus = ChatIdle}))
            say i ("\n\n" <> prompt)
      | stop == "refusal" || stop == "max_tokens" = do
            -- Tools of a cut-off or declined turn never run; the history
            -- still needs their results.
            let why = if stop == "refusal" then "the request was declined" else "the answer was cut off"
            appendResults i [toolResult (tcId c) True ("not run: " <> why) | c <- calls]
            modify' (modifyChat i (\c -> c {csStatus = ChatIdle}))
            say i ("\n[" <> why <> "]\n\n" <> prompt)
      | otherwise = runCalls i calls

appendResults :: Int -> [Value] -> EditorM ()
appendResults i results =
  modify' (modifyChat i (\c -> c {csHistory = csHistory c <> [object [("role", JString "user"), ("content", JArray results)]], csResults = [], csEdits = []}))

-- | Run a turn's tool calls: reads at once, edits as pending edits.
runCalls :: Int -> [ToolCall] -> EditorM ()
runCalls i calls = do
  root <- liftIO getCurrentDirectory
  results <- forM calls $ \call -> case parseToolCall call of
    Left e -> pure (tcId call, Just (toolResult (tcId call) True e))
    Right req -> case req of
      ListFiles -> do
        files <- liftIO (listFiles (defaultWalk 2000) root)
        pure (tcId call, Just (toolResult (tcId call) False (T.unlines (map T.pack files))))
      ReadFile p -> withPath root p call $ \path -> do
        text <- readProjectFile path
        pure (tcId call, Just (either (toolResult (tcId call) True) (toolResult (tcId call) False) text))
      EditFile p old new -> withPath root p call $ \path -> do
        target <- openTarget path
        number <- nextNumber i
        d <- gets (find ((== target) . docId) . allDocuments)
        case d >>= either (const Nothing) Just . proposeEdit number (tcId call) path old new of
          Just (pe, d') -> pending i pe d'
          Nothing ->
            pure (tcId call, Just (toolResult (tcId call) True (either id (const "") (maybe (Left "the file could not be opened") (proposeEdit number (tcId call) path old new) d))))
      WriteFile p content -> withPath root p call $ \path -> do
        target <- openTarget path
        number <- nextNumber i
        gets (find ((== target) . docId) . allDocuments) >>= \case
          Just d -> let (pe, d') = proposeWrite number (tcId call) path content d in pending i pe d'
          Nothing -> pure (tcId call, Just (toolResult (tcId call) True "the file could not be opened"))
  modify' (modifyChat i (\c -> c {csResults = results}))
  if all (isJust . snd) results
    then finishTurn i
    else do
      modify' (modifyChat i (\c -> c {csStatus = ChatDeciding}))
      say i "[space c a: approve the next edit, space c d: deny it; A / D: all of them]\n"
      showNextEdit i
  where
    withPath root p call k = case projectPath root p of
      Right path -> k path
      Left e -> pure (tcId call, Just (toolResult (tcId call) True e))

nextNumber :: Int -> EditorM Int
nextNumber i = do
  n <- gets (maybe 1 (csNextEdit . snd) . chatOf)
  modify' (modifyChat i (\c -> c {csNextEdit = n + 1}))
  pure n

-- | Record a pending edit: the document changed, the chat shows the diff.
pending :: Int -> PendingEdit -> Document -> EditorM (Text, Maybe Value)
pending i pe d' = do
  modify' (modifyDocument (docId d') (const d'))
  modify' (modifyChat i (\c -> c {csEdits = csEdits c <> [pe]}))
  say i ("\n" <> editSummary pe)
  pure (peToolId pe, Nothing)

-- | A path the model gave, inside the project (relative to it).
projectPath :: FilePath -> FilePath -> Either Text FilePath
projectPath root p
  | ".." `elem` splitDirectories rel = Left ("outside the project: " <> T.pack p)
  | isAbsolute rel = Left ("outside the project: " <> T.pack p)
  | otherwise = Right rel
  where
    rel = normalise (if isAbsolute p then makeRelative root p else p)

-- | A file's text as the editor has it (unsaved changes included).
readProjectFile :: FilePath -> EditorM (Either Text Text)
readProjectFile path = do
  open <- gets (find ((== Just path) . docPath) . allDocuments)
  case open of
    Just d -> pure (Right (Buffer.toText (docBuffer d)))
    Nothing ->
      liftIO (try @IOException (TIO.readFile path)) >>= \case
        Right t | T.length t > 400000 -> pure (Left "the file is too large to read whole")
        Right t -> pure (Right t)
        Left e -> pure (Left (T.pack (show e)))

-- | Open a file in the editor window (not the chat's), creating an empty
-- buffer for a new file; the focus stays where it was. Its buffer's id.
openTarget :: FilePath -> EditorM Int
openTarget path = do
  from <- gets edFocus
  ed <- get
  chat <- gets (fmap (docId . fst) . chatOf)
  let others = [w | (w, win) <- IntMap.toList (edWindows ed), Just (winDoc win) /= chat]
  if Just (docId (edDoc ed)) /= chat
    then pure ()
    else case others of
      w : _ -> modify' (focusWindow w)
      [] -> modify' (splitWindow Beside)
  exists <- liftIO (doesFileExist path)
  if exists then openFile path else modify' (openBuffer (newDocument (Just path) Buffer.empty))
  i <- gets (docId . edDoc)
  modify' (focusWindow from)
  pure i

-- | All of a turn's results are there: send them and go on.
finishTurn :: Int -> EditorM ()
finishTurn i = do
  results <- gets (maybe [] (csResults . snd) . chatOf)
  appendResults i [r | (_, Just r) <- results]
  continue i

-- * Deciding

decideNext :: Bool -> EditorM ()
decideNext approve =
  gets chatOf >>= \case
    Just (d, cs) | Just pe <- find ((== Undecided) . peDecision) (csEdits cs) -> do
      decide (docId d) approve pe
      afterDecision (docId d)
    _ -> failWith "no pending edits"

decideAll :: Bool -> EditorM ()
decideAll approve =
  gets chatOf >>= \case
    Just (d, cs) | any ((== Undecided) . peDecision) (csEdits cs) -> do
      mapM_ (decide (docId d) approve) [pe | pe <- csEdits cs, peDecision pe == Undecided]
      afterDecision (docId d)
    _ -> failWith "no pending edits"

-- | Keep (and save) or undo one edit, and record its tool result.
decide :: Int -> Bool -> PendingEdit -> EditorM ()
decide i approve pe = do
  result <-
    if approve
      then
        gets (find ((== peDoc pe) . docId) . allDocuments) >>= \case
          Nothing -> pure (toolResult (peToolId pe) True "the file's buffer was closed before the edit was approved")
          Just d ->
            liftIO (saveDocument (pePath pe) d) >>= \case
              Left e -> pure (toolResult (peToolId pe) True ("approved, but saving failed: " <> e))
              Right _ -> do
                modify' (modifyDocument (peDoc pe) (\doc -> doc {docDirty = False, docSavedBuffer = docBuffer doc, docSaves = docSaves doc + 1}))
                pure (toolResult (peToolId pe) False ("The user approved the edit; " <> T.pack (pePath pe) <> " is saved."))
      else do
        modify' (modifyDocument (peDoc pe) (revertEdit pe))
        pure (toolResult (peToolId pe) False "The user rejected this edit; the file is unchanged.")
  modify' $ modifyChat i $ \c ->
    c
      { csEdits = [if peNumber e == peNumber pe then e {peDecision = if approve then Approved else Denied} else e | e <- csEdits c]
      , csResults = [(k, if k == peToolId pe then Just result else r) | (k, r) <- csResults c]
      }
  say i ("[#" <> T.pack (show (peNumber pe)) <> (if approve then " approved]\n" else " denied]\n"))

afterDecision :: Int -> EditorM ()
afterDecision i = do
  cs <- gets (fmap snd . chatOf)
  case cs of
    Just c | all (isJust . snd) (csResults c) -> finishTurn i
    _ -> showNextEdit i

-- | Put the editor window on the next undecided edit.
showNextEdit :: Int -> EditorM ()
showNextEdit _ =
  gets chatOf >>= \case
    Just (_, cs) | Just pe <- find ((== Undecided) . peDecision) (csEdits cs) -> do
      let lines' = max 1 (length (peNew pe))
          sel = single (Range (Pos (peLine pe) 0) (Pos (peLine pe + lines' - 1) 0) Nothing)
      ed <- get
      if docId (edDoc ed) == peDoc pe
        then modifyDoc (\d -> d {docSelection = sel})
        else modify' $ \e ->
          e {edWindows = fmap (\win -> if winDoc win == peDoc pe then win {winSelection = sel, winView = (winView win) {viewTop = max 0 (peLine pe - 3)}} else win) (edWindows e)}
    _ -> pure ()
