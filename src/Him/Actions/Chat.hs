-- | The AI chat plugin (ADR ai-chat, ADR change-review, ADR chat-panel). @space c c@ opens the
-- chat in a window beside the code; type a message in the box at the
-- bottom and @ret@ sends it (@A-ret@ is a line break, @up@ / @down@ recall
-- earlier messages). The transcript shows your messages, the answers, and
-- what the model did. The model reads files on its own and proposes
-- changes, all in one go: they appear in the files' buffers, each with a
-- header and the lines it removes, and nothing is written. Review them in
-- any order with the cursor on one: @space c a@ keeps it (written to the
-- file), @space c d@ discards it (the old lines come back); @space c A@ /
-- @D@ do all; @] c@ / @[ c@ move between them, @space c l@ lists them. The
-- decisions go to the model with the next message.
module Him.Actions.Chat
  ( chatPlugin
  ) where

import Control.Monad (forM, when)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.State.Strict (get, gets, modify')
import Data.List (find, findIndex)
import Data.Map.Strict qualified as Map
import Data.Maybe (isNothing)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.IO qualified as TIO
import Control.Exception (IOException, try)
import Him.Action hiding (text)
import Him.Actions.File (openFile)
import Him.Actions.Motion (vertical)
import Him.Chat.Transcript
import Him.Buffer qualified as Buffer
import Him.Chat
import Him.Chat.Tools
import Him.Config (Plugin (..), plugin)
import Him.Document (DocKind (..), Document (..), clampSelection, newDocument, replaceBuffer)
import Him.Diff (Hunk (..), diffLines)
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
import Him.View (View (..))
import Him.Window (Axis (..), Box (..), Window (..))
import Data.Maybe (fromMaybe)
import Him.Review (approveHunk, approveOnto, denyHunk, hunkAtLine, hunkLabel)
import Him.Picker (PickTarget (..), newPicker, pickerItem)
import Data.IntMap.Strict qualified as IntMap
import System.Directory (doesFileExist, getCurrentDirectory)
import System.FilePath (isAbsolute, makeRelative, normalise, splitDirectories)

chatPlugin :: Plugin
chatPlugin =
  (plugin "chat" "An AI chat beside the code (space c c); its edits wait for you to keep or discard them (space c a / d)")
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
              , ("space c l", "chat_changes")
              , ("space c s", "chat_add_selection")
              , ("space c y", "chat_copy_code")
              , ("space c n", "chat_new")
              , ("] c", "chat_next_change")
              , ("[ c", "chat_prev_change")
              ]
            )
          , (Chat, [("ret", "chat_submit"), ("A-ret", "insert_newline"), ("C-c", "chat_cancel"), ("up", "chat_history_previous"), ("down", "chat_history_next"), ("C-l", "chat_new")])
          ]
    , plPrefixNames = [([plain (KChar ' '), plain (KChar 'c')], "chat")]
    , plExCommands = exCommands
    , plJobResult = applyChatResult
    , plHousekeeping = refreshReviews
    , plSigns = True
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
  , simple "chat_approve" GMisc "Keep the proposed change under the cursor (it is written to the file)" (decideAtCursor True)
  , simple "chat_deny" GMisc "Discard the proposed change under the cursor (the old lines come back)" (decideAtCursor False)
  , simple "chat_approve_all" GMisc "Keep every proposed change" (decideAll True)
  , simple "chat_deny_all" GMisc "Discard every proposed change" (decideAll False)
  , simple "chat_next_change" GMovement "Go to the next proposed change" (jumpChange True)
  , simple "chat_prev_change" GMovement "Go to the previous proposed change" (jumpChange False)
  , simple "chat_changes" GMisc "List every proposed change" listChanges
  , simple "chat_add_selection" GMisc "Put the selection into the chat's message, with its file and line" addSelection
  , simple "chat_new" GMisc "Start a new conversation (proposed changes stay under review)" newConversation
  , simple "chat_copy_code" GClipboard "Copy a code block of the chat: the one under the cursor, or the last one" copyCode
  , simple "chat_history_previous" GMisc "In the chat's input: the message sent before (or up a line)" (recall True)
  , simple "chat_history_next" GMisc "In the chat's input: the message sent after (or down a line)" (recall False)
  ]

exCommands :: [ExCommand]
exCommands =
  [ ExCommand ["chat"] "Open the AI chat beside the code" NoArgs $ \_ -> openChat True
  , ExCommand ["chat-new"] "Start a new conversation (the chat buffer is cleared)" NoArgs $ \_ -> newConversation
  , ExCommand ["chat-approve", "chat-keep"] "Keep the proposed change under the cursor (:chat-keep all: every one)" NoArgs $ \args -> if args == ["all"] then decideAll True else decideAtCursor True
  , ExCommand ["chat-deny", "chat-discard"] "Discard the proposed change under the cursor (:chat-discard all: every one)" NoArgs $ \args -> if args == ["all"] then decideAll False else decideAtCursor False
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

-- | The model's text, at the end of the transcript. After what it did (a
-- tool line), its text starts after a blank line.
say :: Int -> Text -> EditorM ()
say i t = do
  d <- gets (find ((== i) . docId) . allDocuments)
  case d of
    Just doc | openLineEmpty doc -> case T.dropWhile (== '\n') t of
      "" -> pure ()
      t' -> do
        when (fmap fst (lastMark doc) `elem` map (Just . Just) [MarkTool, MarkChange, MarkNote]) $ gap i
        withWidth i (\w -> appendModel w t')
    _ -> withWidth i (\w -> appendModel w t)

-- | Lines of the editor's own (a tool line, a note), after the model's
-- prose with a blank line between.
sayLines :: Int -> ChatMark -> [Text] -> EditorM ()
sayLines i mark ls = do
  d <- gets (find ((== i) . docId) . allDocuments)
  let afterProse = case d of
        Just doc -> not (openLineEmpty doc) || fmap fst (lastMark doc) == Just Nothing
        Nothing -> False
  when afterProse $ gap i
  withWidth i (\w -> appendLines w mark ls)

gap :: Int -> EditorM ()
gap i = withWidth i appendGap

-- | The end of the transcript for now: its last line finished, so the
-- empty line above the input separates them.
settle :: Int -> EditorM ()
settle i = do
  d <- gets (find ((== i) . docId) . allDocuments)
  when (maybe False (not . openLineEmpty) d) $ withWidth i (`appendModel` "\n")

-- | Change the chat's document, laid out for the width of its window.
withWidth :: Int -> (Int -> Document -> Document) -> EditorM ()
withWidth i f = do
  ed <- get
  let width = case [boxWidth b | Just w <- [windowShowing i ed], (w', b) <- windowBoxes ed, w' == w] of
        bw : _ -> max 20 (bw - 2)
        [] -> 78
  modify' (followEnd i . modifyDocument i (f width))

-- | What a new chat says before the first message.
welcome :: Int -> EditorM ()
welcome i =
  sayLines
    i
    MarkWelcome
    [ "Ask Claude about your code. It sees which file you are in, reads the project, and proposes changes that you keep or discard in the editor."
    , ""
    , "ret send · A-ret new line · up/down earlier messages · C-c stop · C-l new chat"
    , "In a file: space c s puts the selection in your message."
    ]

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
      modify' (openBuffer newChatDocument . splitWindow Beside)
      i <- gets (docId . edDoc)
      welcome i
      pure i
  modify' (focusWindow from)
  pure i
  where
    showBuffer i ed = maybe ed (`gotoBuffer` ed) (findIndex ((== i) . docId) (allDocuments ed))

newConversation :: EditorM ()
newConversation = do
  existing <- gets chatOf
  case existing of
    Nothing -> openChat True
    Just (d, cs) -> do
      request (ChatCancel (docId d))
      modify' $ modifyDocument (docId d) $ \doc ->
        doc
          { docBuffer = docBuffer newChatDocument
          , docSelection = docSelection newChatDocument
          -- Proposed changes stay under review: they are in the buffers;
          -- earlier messages can still be recalled.
          , docKind = ChatDoc (fromMaybe newChatState (chatState newChatDocument)) {csReviews = csReviews cs, csSent = csSent cs}
          , docVersion = docVersion doc + 1
          }
      welcome (docId d)
      info "a new conversation"


-- | Windows on the chat follow its end: those not focused always, the
-- focused one while its cursor is in the input (not while reading above).
followEnd :: Int -> Editor -> Editor
followEnd i ed = case find ((== i) . docId) (allDocuments ed) of
  Nothing -> ed
  Just d ->
    let end = Buffer.endPos (docBuffer d)
        height w = maybe 10 (\b -> max 1 (boxHeight b - 1)) (lookup w (windowBoxes ed))
        follow w win
          | winDoc win == i = win {winView = (winView win) {viewTop = max 0 (posLine end - height w + 1)}, winSelection = single (point end)}
          | otherwise = win
        inInput = maybe False (\cs -> posLine (rangeHead (primary (docSelection d))) >= posLine (csInput cs)) (chatState d)
        focused
          | docId (edDoc ed) == i && inInput = ed {edView = (edView ed) {viewTop = max (viewTop (edView ed)) (posLine end - height (edFocus ed) + 1)}}
          | otherwise = ed
     in focused {edWindows = IntMap.mapWithKey follow (edWindows ed)}

-- * Sending

submit :: EditorM ()
submit = do
  d <- getDoc
  case docKind d of
    ChatDoc cs -> case csStatus cs of
      ChatWaiting -> failWith "the answer is still coming (C-c stops it)"
      ChatIdle -> case takeMessage d of
        Just (text, d') | not (T.null (T.strip text)) -> do
          let i = docId d
          modifyDoc (const d')
          context <- editorContext
          review <- reviewNote
          let message = object [("role", JString "user"), ("content", JArray [object [("type", JString "text"), ("text", JString (review <> maybe "" fst context <> T.strip text))]])]
          modify' (modifyChat i (\c -> c {csHistory = csHistory c <> [message], csDecisions = [], csSent = T.strip text : filter (/= T.strip text) (csSent c)}))
          -- The message as a block of the transcript, then the answer's.
          gap i
          sayLines i MarkUser ["You"]
          withWidth i (\w -> appendUser w (T.strip text))
          mapM_ (\(_, label) -> sayLines i MarkContext ["↳ " <> label]) context
          gap i
          sayLines i MarkClaude ["Claude"]
          continue i
        _ -> pure ()
    _ -> pure ()

-- | Ask the model to go on from the history as it is now.
continue :: Int -> EditorM ()
continue i = do
  root <- liftIO getCurrentDirectory
  modify' (modifyChat i (\c -> c {csStatus = ChatWaiting}))
  history <- gets (maybe [] (csHistory . snd) . chatOf)
  request (ChatSend i (ChatRequest (systemPrompt root) history chatTools))

-- | What became of the model's proposed changes since its last turn, and
-- which still wait, said before the user's message.
reviewNote :: EditorM Text
reviewNote = do
  ed <- get
  let decided = maybe [] (csDecisions . snd) (chatOf ed)
      waiting = [hunkLabel (rvPath rv) h | Just (_, cs) <- [chatOf ed], rv <- csReviews cs, h <- rvHunks rv]
  pure $
    T.concat
      [ "[The user reviewed your proposed changes: " <> T.intercalate "; " decided <> ".]\n" | not (null decided)]
      <> T.concat ["[Still waiting for review: " <> T.intercalate ", " waiting <> ".]\n" | not (null waiting)]
      <> (if null decided && null waiting then "" else "\n")

-- | What the user is looking at, said before their message, and how it is
-- shown under the message.
editorContext :: EditorM (Maybe (Text, Text))
editorContext = do
  ed <- get
  pure $ case editorDocument ed of
    Just d | Just p <- docPath d ->
      let line = T.pack (show (posLine (rangeHead (primary (docSelection d))) + 1))
       in Just ("[I am looking at " <> T.pack p <> ", line " <> line <> ".]\n\n", T.pack p <> ":" <> line)
    _ -> Nothing

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
    Just (d, cs) | csStatus cs /= ChatIdle -> do
      request (ChatCancel (docId d))
      modify' (modifyChat (docId d) (\c -> c {csStatus = ChatIdle}))
      sayLines (docId d) MarkNote ["Stopped."]
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
    ChatActivity t -> sayLines i MarkTool ["◦ " <> t]
    -- A tool Claude Code waits for (ADR claude-code-provider): answered at once, an edit as
    -- proposed.
    ChatToolCall call -> runCall i call >>= \(isError, text) -> request (ChatAnswer i (tcId call) isError text)
    ChatFailed e -> do
      modify' (modifyChat i (\c -> c {csStatus = ChatIdle}))
      sayLines i MarkError ["Error: " <> e]
      endOfTurn i
    ChatFinished stop message calls -> do
      modify' (modifyChat i (\c -> c {csHistory = csHistory c <> [message]}))
      finished i stop calls
  _ -> pure ()
  where
    finished i stop calls
      | null calls = do
          modify' (modifyChat i (\c -> c {csStatus = ChatIdle}))
          endOfTurn i
      | stop == "refusal" || stop == "max_tokens" = do
          -- Tools of a cut-off or declined turn never run; the history
          -- still needs their results.
          let why = if stop == "refusal" then "the request was declined" else "the answer was cut off"
          appendResults i [toolResult (tcId c) True ("not run: " <> why) | c <- calls]
          modify' (modifyChat i (\c -> c {csStatus = ChatIdle}))
          sayLines i MarkNote [T.toUpper (T.take 1 why) <> T.drop 1 why <> "."]
          endOfTurn i
      | otherwise = do
          -- The API provider: the calls came with the reply; the results
          -- go back with the next request, at once.
          results <- forM calls $ \call -> (\(isError, text) -> toolResult (tcId call) isError text) <$> runCall i call
          appendResults i results
          continue i

appendResults :: Int -> [Value] -> EditorM ()
appendResults i results =
  modify' (modifyChat i (\c -> c {csHistory = csHistory c <> [object [("role", JString "user"), ("content", JArray results)]]}))

-- | The turn is over. With changes proposed: a summary of them, and the
-- editor on the first (in normal mode, to review).
endOfTurn :: Int -> EditorM ()
endOfTurn i = do
  refreshReviews
  reviews <- gets (maybe [] (csReviews . snd) . chatOf)
  let n = sum (map (length . rvHunks) reviews)
      counts rv = let hs = rvHunks rv in " (+" <> T.pack (show (sum (map hNewCount hs))) <> " −" <> T.pack (show (sum (map hOldCount hs))) <> ")"
  if n == 0
    then settle i
    else do
      gap i
      sayLines
        i
        MarkReview
        ( [T.pack (show n) <> " change" <> (if n == 1 then "" else "s") <> " to review"]
            <> ["  " <> T.pack (rvPath rv) <> counts rv | rv <- reviews]
            <> ["In the editor, cursor on a change: space c a keep · space c d discard · ] c next · space c A / D all · space c l list"]
        )
      case reviews of
        rv : _ | h : _ <- rvHunks rv -> showChange rv h
        _ -> pure ()

-- | Run one tool call: whether it failed, and what to tell the model.
runCall :: Int -> ToolCall -> EditorM (Bool, Text)
runCall i call = do
  root <- liftIO getCurrentDirectory
  case parseToolCall call of
    Left e -> pure (True, e)
    Right req -> case req of
      ListFiles -> do
        files <- liftIO (listFiles (defaultWalk 2000) root)
        sayLines i MarkTool ["◦ Listed the project's files"]
        pure (False, T.unlines (map T.pack files))
      ReadFile p -> withPath root p $ \path -> do
        sayLines i MarkTool ["◦ Read " <> T.pack path]
        either (True,) (False,) <$> readProjectFile path
      EditFile p old new -> withPath root p $ \path -> do
        target <- openTarget path
        d <- gets (find ((== target) . docId) . allDocuments)
        case maybe (Left "the file could not be opened") (editBuffer path old new . docBuffer) d of
          Right buf -> propose i target path buf
          Left e -> pure (True, e)
      WriteFile p content -> withPath root p $ \path -> do
        target <- openTarget path
        propose i target path (writeBuffer content)
  where
    withPath root p k = case projectPath root p of
      Right path -> k path
      Left e -> pure (True, e)

-- | A proposed change: the buffer takes the new text (an undoable change),
-- and the document is under review from its text before the first one.
propose :: Int -> Int -> FilePath -> Buffer.Buffer -> EditorM (Bool, Text)
propose i target path buf = do
  d <- gets (find ((== target) . docId) . allDocuments)
  case d of
    Nothing -> pure (True, "the file could not be opened")
    Just doc -> do
      known <- gets (\ed -> reviewFor ed target)
      when (isNothing known) $
        modify' (modifyChat i (\c -> c {csReviews = csReviews c <> [Review target path (Buffer.toLines (docBuffer doc)) [] (-1)]}))
      modify' (modifyDocument target (replaceBuffer buf (clampSelection buf (docSelection doc))))
      refreshReviews
      let hunks = diffLines (Buffer.toLines (docBuffer doc)) (Buffer.toLines buf)
      sayLines i MarkChange ["✎ " <> T.pack path <> "  +" <> T.pack (show (sum (map hNewCount hunks))) <> " −" <> T.pack (show (sum (map hOldCount hunks)))]
      pure (False, proposedResult path)

-- | After every event: each review's changes are the diff from its base to
-- the buffer as it is now; a review without changes left is done.
refreshReviews :: EditorM ()
refreshReviews =
  gets chatOf >>= \case
    Just (chat, cs) | not (null (csReviews cs)) -> do
      ed <- get
      let refresh rv = case find ((== rvDoc rv) . docId) (allDocuments ed) of
            Nothing -> Nothing
            Just d
              | docVersion d == rvVersion rv -> Just rv
              | otherwise -> Just rv {rvHunks = diffLines (rvBase rv) (Buffer.toLines (docBuffer d)), rvVersion = docVersion d}
          reviews = [rv | Just rv <- map refresh (csReviews cs), not (null (rvHunks rv))]
      when (reviews /= csReviews cs) $ modify' (modifyChat (docId chat) (\c -> c {csReviews = reviews}))
    _ -> pure ()

-- | A path the model gave, inside the project (relative to it).
projectPath :: FilePath -> FilePath -> Either Text FilePath
projectPath root p
  | ".." `elem` splitDirectories rel = Left ("outside the project: " <> T.pack p)
  | isAbsolute rel = Left ("outside the project: " <> T.pack p)
  | otherwise = Right rel
  where
    rel = normalise (if isAbsolute p then makeRelative root p else p)

-- | A file's text as the editor has it (proposed changes included).
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
  focusEditorWindow
  exists <- liftIO (doesFileExist path)
  open <- gets (find ((== Just path) . docPath) . allDocuments)
  case open of
    Just d -> modify' (\ed -> maybe ed (`gotoBuffer` ed) (findIndex ((== docId d) . docId) (allDocuments ed)))
    Nothing -> if exists then openFile path else modify' (openBuffer (newDocument (Just path) Buffer.empty))
  i <- gets (docId . edDoc)
  modify' (focusWindow from)
  pure i

-- | Focus a window that is not the chat's (splitting one off if needed).
focusEditorWindow :: EditorM ()
focusEditorWindow = do
  ed <- get
  chat <- gets (fmap (docId . fst) . chatOf)
  let others = [w | (w, win) <- IntMap.toList (edWindows ed), Just (winDoc win) /= chat]
  when (Just (docId (edDoc ed)) == chat) $ case others of
    w : _ -> modify' (focusWindow w)
    [] -> modify' (splitWindow Beside)

-- * Reviewing

-- | Approve or deny the change under the cursor.
decideAtCursor :: Bool -> EditorM ()
decideAtCursor approve = do
  refreshReviews
  ed <- get
  let d = edDoc ed
      line = posLine (rangeHead (primary (docSelection d)))
  case reviewFor ed (docId d) of
    Nothing -> failWith "no proposed changes in this buffer (space c l lists them)"
    Just rv -> case hunkAtLine (Buffer.lineCount (docBuffer d)) line (rvHunks rv) of
      Nothing -> failWith "the cursor is not on a proposed change (] c goes to the next)"
      Just (_, h) -> do
        ok <- decide approve rv h
        left <- gets (\e -> maybe 0 (length . rvHunks) (reviewFor e (docId d)))
        when ok $ info ((if approve then "kept" else "discarded") <> (if left == 0 then "; no changes left here" else "; " <> T.pack (show left) <> " left here (] c: next)"))

-- | Approve (write to the file) or deny (put back) one change; 'False' when
-- it could not be approved.
--
-- Approving writes the file as it is on disk with this change applied. If
-- the buffer had unsaved edits by hand before the chat's first change, the
-- file differs from the review's base: the change is moved onto the file's
-- text past them, and they stay unsaved ('approveOnto'). A change that
-- overlaps one of them is refused (ADR change-review).
decide :: Bool -> Review -> Hunk -> EditorM Bool
decide approve rv h = do
  chat <- gets (fmap (docId . fst) . chatOf)
  ed <- get
  case (chat, find ((== rvDoc rv) . docId) (allDocuments ed)) of
    (Just i, Just d) -> do
      let current = Buffer.toLines (docBuffer d)
          label = hunkLabel (rvPath rv) h
      ok <-
        if approve
          then case approveOnto h (rvBase rv) current (Buffer.toLines (docSavedBuffer d)) of
            Nothing ->
              False <$ failWith "this change overlaps your own unsaved edit; save it (:w) or undo it first"
            Just written -> do
              ok <- writeBase d (rvPath rv) written
              when ok $ do
                setBase i rv (approveHunk h (rvBase rv) current)
                note i ("approved " <> label)
              pure ok
          else do
            let current' = Buffer.fromLines (denyHunk h (rvBase rv) current)
            modify' (modifyDocument (docId d) (\doc -> replaceBuffer current' (clampSelection current' (docSelection doc)) doc))
            note i ("rejected " <> label <> ", its old lines are back")
            pure True
      refreshReviews
      pure ok
    _ -> pure False

-- | Approve or deny every proposed change.
decideAll :: Bool -> EditorM ()
decideAll approve =
  gets chatOf >>= \case
    Just (chat, cs) | not (null (csReviews cs)) -> do
      ed <- get
      ok <-
        if approve
          then -- One change at a time, from the last (the earlier ones keep
          -- their places), each checked like a single approval.
            and <$> mapM (approveAllOf . rvDoc) (csReviews cs)
          else do
            sequence_
              [ do
                  let base = Buffer.fromLines (rvBase rv)
                  modify' (modifyDocument (docId d) (\doc -> replaceBuffer base (clampSelection base (docSelection doc)) doc))
                  note (docId chat) ("rejected all changes to " <> T.pack (rvPath rv))
              | rv <- csReviews cs
              , Just d <- [find ((== rvDoc rv) . docId) (allDocuments ed)]
              ]
            pure True
      refreshReviews
      when ok $ info (if approve then "kept every proposed change" else "discarded every proposed change")
    _ -> failWith "no proposed changes"

-- | Approve a document's changes, last first, until none are left or one
-- cannot be approved.
approveAllOf :: Int -> EditorM Bool
approveAllOf doc = do
  refreshReviews
  gets (`reviewFor` doc) >>= \case
    Just rv | h : _ <- reverse (rvHunks rv) -> do
      ok <- decide True rv h
      if ok then approveAllOf doc else pure False
    _ -> pure True

-- | Write a document's file with these lines (its line endings kept); the
-- document stays as it is (other changes may still be proposed).
writeBase :: Document -> FilePath -> [Text] -> EditorM Bool
writeBase d path lines' = do
  let saved = Buffer.fromLines lines'
  liftIO (saveDocument path d {docBuffer = saved}) >>= \case
    Left e -> False <$ failWith ("could not write " <> T.pack path <> ": " <> e)
    Right _ -> do
      modify' (modifyDocument (docId d) (\doc -> doc {docSavedBuffer = saved, docDirty = docBuffer doc /= saved, docSaves = docSaves doc + 1}))
      pure True

setBase :: Int -> Review -> [Text] -> EditorM ()
setBase i rv base' = modify' (modifyChat i (\c -> c {csReviews = [if rvDoc r == rvDoc rv then r {rvBase = base', rvVersion = -1} else r | r <- csReviews c]}))

-- | A decision, for the model's next message.
note :: Int -> Text -> EditorM ()
note i t = modify' (modifyChat i (\c -> c {csDecisions = csDecisions c <> [t]}))

-- | @] c@ / @[ c@: the next or previous change in this buffer (wrapping).
jumpChange :: Bool -> EditorM ()
jumpChange forward = do
  refreshReviews
  ed <- get
  let d = edDoc ed
      line = posLine (rangeHead (primary (docSelection d)))
      starts = maybe [] (map hNewStart . rvHunks) (reviewFor ed (docId d))
      target
        | forward = case filter (> line) starts of
            l : _ -> Just l
            [] -> case starts of l : _ -> Just l; [] -> Nothing
        | otherwise = case reverse (filter (< line) starts) of
            l : _ -> Just l
            [] -> case reverse starts of l : _ -> Just l; [] -> Nothing
  case target of
    Nothing -> failWith "no proposed changes in this buffer (space c l lists them)"
    Just l -> motion (\b _ -> point (Buffer.clampPos b (Pos (min l (Buffer.lineCount b - 1)) 0)))

-- | Put the editor on a change: focus its window, cursor on its first line.
showChange :: Review -> Hunk -> EditorM ()
showChange rv h = do
  focusEditorWindow
  modify' (\ed -> maybe ed (`gotoBuffer` ed) (findIndex ((== rvDoc rv) . docId) (allDocuments ed)))
  modifyDoc (\d -> d {docSelection = single (point (Buffer.clampPos (docBuffer d) (Pos (min (hNewStart h) (Buffer.lineCount (docBuffer d) - 1)) 0)))})
  setMode Normal

-- | @space c l@: every proposed change, in a picker (with a preview).
listChanges :: EditorM ()
listChanges = do
  refreshReviews
  reviews <- gets (maybe [] (csReviews . snd) . chatOf)
  case [pickerItem (hunkLabel (rvPath rv) h) (PickPosition (rvPath rv) (hNewStart h) 0 Nothing) "" | rv <- reviews, h <- rvHunks rv] of
    [] -> failWith "no proposed changes"
    items -> do
      focusEditorWindow
      openPicker (newPicker "proposed changes" items)

-- * The input

-- | @up@ / @down@ in the chat's input: on its first (last) line, the
-- message sent before (after) the one shown; elsewhere, a line up (down).
recall :: Bool -> EditorM ()
recall back = do
  d <- getDoc
  case chatState d of
    Just cs
      | onEdge cs d -> do
          let n = csRecall cs + (if back then 1 else -1)
              sent = csSent cs
          case drop n sent of
            _ | n < 0 -> do
              modifyDoc (setMessage "")
              setRecall (-1)
            m : _ -> do
              modifyDoc (setMessage m)
              setRecall n
            [] -> pure ()
    _ -> vertical (if back then -1 else 1)
  where
    line d = posLine (rangeHead (primary (docSelection d)))
    onEdge cs d
      | back = line d == posLine (csInput cs)
      | otherwise = line d == Buffer.lineCount (docBuffer d) - 1
    setRecall n = modifyDoc (\doc -> maybe doc (\cs -> doc {docKind = ChatDoc cs {csRecall = n}}) (chatState doc))

-- | @space c y@: a code block of the chat into the register.
copyCode :: EditorM ()
copyCode = do
  ed <- get
  let here = edDoc ed
      line = if isChat here then posLine (rangeHead (primary (docSelection here))) else maxBound
  case chatOf ed >>= codeBlockAt line . fst of
    Nothing -> failWith "no code block in the chat"
    Just code -> do
      setRegister '"' [code <> "\n"]
      info ("copied a code block (" <> T.pack (show (length (T.lines code))) <> " lines); p pastes it")
