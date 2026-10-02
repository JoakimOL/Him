-- | The language-server client in the editor (ADR-29): attaching documents
-- to servers, keeping the server's copy of the text current, asking
-- questions (hover, definition, references) and applying the answers, and
-- diagnostics. Messages are built and read as pure data; the runtime only
-- moves them ("Him.Lsp.Server").
module Him.Commands.Lsp
  ( actions
  , lspHousekeeping
  , lspFlush
  , applyLspResult
  , completionHousekeeping
  , exCommands
  ) where

import Control.Monad.Trans.State.Strict (get, gets, modify')
import Data.IntMap.Strict qualified as IntMap
import Data.Char (isAlphaNum)
import Data.List (find, sortOn)
import Data.Map.Strict qualified as Map
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Text qualified as T
import Him.Action hiding (text)
import Him.Buffer qualified as Buffer
import Him.Command
import Him.Commands.File (openFile)
import Him.Document (DocKind (..), Document (..))
import Him.Effect (Effect (..), Job (..), JobResult (..))
import Him.Editor hiding (Severity (..))
import Him.Json hiding (path)
import Him.Language (detectLanguage, languages)
import Him.Lsp.Protocol hiding (request)
import Him.Lsp.Protocol qualified as P
import Him.Lsp.State
import Him.Lsp.Sync (syncMessages)
import Him.Ex (ExArgs (..), ExCommand (..))
import Him.Mode (Mode (..))
import Him.Picker (PickTarget (..), fuzzyScore, newPicker, pickerItem)
import Him.Position (Pos (..))
import Him.Selection (point, primary, rangeHead, single)

actions :: [Action]
actions =
  [ simple "lsp_hover" GLsp "Show documentation for the symbol under the cursor" $
      ask PendingHover "textDocument/hover" []
  , simple "goto_definition" GLsp "Go to the definition of the symbol under the cursor" $
      ask PendingDefinition "textDocument/definition" []
  , simple "goto_references" GLsp "List the references to the symbol under the cursor" $
      ask PendingReferences "textDocument/references" [("context", object [("includeDeclaration", JBool True)])]
  , simple "diagnostics_picker" GLsp "List the diagnostics of this file" $ do
      ed <- get
      let d = edDoc ed
          path = fromMaybe "" (docPath d)
          items =
            [ pickerItem
                (T.pack (show (sdLine sd + 1) <> ":" <> show (sdStart sd + 1)) <> "  " <> severityName (sdSeverity sd))
                (PickPosition path (sdLine sd) (sdStart sd))
                (T.takeWhile (/= '\n') (sdMessage sd))
            | sd <- dedupe (shownDiagnostics (edLsp ed) (docLsp d) (docBuffer d))
            ]
      if null items
        then info "no diagnostics"
        else modify' (\e -> e {edPicker = Just (newPicker "diagnostics" items), edMode = Picking})
  , simple "completion" GLsp "Ask the language server to complete the word at the cursor" requestCompletion
  , simple "completion_next" GLsp "Select the next completion" (moveCompletion 1)
  , simple "completion_previous" GLsp "Select the previous completion" (moveCompletion (-1))
  , simple "completion_accept" GLsp "Insert the selected completion" acceptCompletion
  , simple "completion_cancel" GLsp "Close the completion menu and leave insert mode" $ do
      modify' (\e -> e {edCompletion = Nothing})
      setMode Normal
  , simple "goto_next_diagnostic" GLsp "Go to the next diagnostic" (jumpDiagnostic True)
  , simple "goto_prev_diagnostic" GLsp "Go to the previous diagnostic" (jumpDiagnostic False)
  ]
  where
    -- One entry per diagnostic, not per line it covers.
    dedupe = go Nothing
      where
        go _ [] = []
        go prev (sd : rest)
          | Just (sdMessage sd) == prev = go prev rest
          | otherwise = sd : go (Just (sdMessage sd)) rest

severityName :: Severity -> Text
severityName = \case
  SevError -> "error"
  SevWarning -> "warning"
  SevInfo -> "info"
  SevHint -> "hint"

-- | Send a request about the cursor position, remembering what to do with
-- the answer.
ask :: Pending -> Text -> [(Text, Value)] -> EditorM ()
ask pending method extra = do
  ed <- get
  let d = edDoc ed
  case docLsp d of
    LspAttached at -> do
      -- The server must have the current text before the question.
      lspFlush
      i <- gets edNextId
      let Pos l c = rangeHead (primary (docSelection d))
          enc = encodingOf ed at
          character = toLspColumn enc (Buffer.lineAt l (docBuffer d)) c
          params =
            object $
              [ ("textDocument", object [("uri", JString (pathToUri (atPath at)))])
              , ("position", object [("line", JInt (fromIntegral l)), ("character", JInt (fromIntegral character))])
              ]
                <> extra
      modify' $ \e ->
        e {edNextId = i + 1, edLsp = (edLsp e) {lsPending = IntMap.insert i pending (lsPending (edLsp e))}}
      request (LspSend (atServer at) (P.request i method params))
    LspStarting -> info "the language server is still starting"
    _ -> failWith "no language server for this file"

encodingOf :: Editor -> Attachment -> Encoding
encodingOf ed at = maybe Utf16 siEncoding (Map.lookup (atServer at) (lsServers (edLsp ed)))

-- | After every event: attach the current document to its server.
lspHousekeeping :: EditorM ()
lspHousekeeping = do
  d <- getDoc
  case (docLsp d, docKind d, docPath d) of
    (LspUnknown, TextDoc, Just path)
      | Just language <- detectLanguage languages path (Buffer.lineAt 0 (docBuffer d)) -> do
          modifyDoc (\doc -> doc {docLsp = LspStarting})
          request (StartJob (LspEnsure (docId d) language path))
    (LspUnknown, _, _) -> modifyDoc (\doc -> doc {docLsp = LspNone})
    _ -> pure ()

-- | Once per batch of input, before drawing: give the server the current
-- text of the current document (whole, as one change).
lspFlush :: EditorM ()
lspFlush = do
  ed <- get
  let d = edDoc ed
  case docLsp d of
    LspAttached at -> do
      let (messages, at') = syncMessages (Map.lookup (atServer at) (lsServers (edLsp ed))) at d
      mapM_ (request . LspSend (atServer at)) messages
      if at' /= at then modifyDoc (\doc -> doc {docLsp = LspAttached at'}) else pure ()
    _ -> pure ()

-- | A server event or answer arrived.
applyLspResult :: JobResult -> EditorM ()
applyLspResult = \case
  LspReady doc server path languageId serverInfo -> do
    modify' (\e -> e {edLsp = (edLsp e) {lsServers = Map.insert server serverInfo (lsServers (edLsp e))}})
    modify' (modifyDocument doc (\d -> d {docLsp = LspAttached (Attachment server path languageId (-1) Nothing (docSaves d))}))
  LspUnavailable doc reason -> do
    modify' (modifyDocument doc (\d -> d {docLsp = LspNone}))
    current <- gets (docId . edDoc)
    if current == doc && reason /= "no language server configured"
      then info ("language server: " <> T.take 120 reason)
      else pure ()
  LspExited server -> do
    modify' (\e -> e {edLsp = (edLsp e) {lsServers = Map.delete server (lsServers (edLsp e))}})
    modify' (mapDocuments (\d -> case docLsp d of LspAttached at | atServer at == server -> d {docLsp = LspNone}; _ -> d))
    info "language server exited"
  LspMessage _ message -> case classify message of
    Reply i result -> do
      pending <- gets (IntMap.lookup i . lsPending . edLsp)
      modify' (\e -> e {edLsp = (edLsp e) {lsPending = IntMap.delete i (lsPending (edLsp e))}})
      case (pending, result) of
        (Just p, Right value) -> answered p value
        (Just _, Left e) -> failWith ("language server: " <> e)
        (Nothing, _) -> pure ()
    Notification "textDocument/publishDiagnostics" params
      | Just (path, ds) <- parseDiagnostics params ->
          modify' (\e -> e {edLsp = (edLsp e) {lsDiagnostics = Map.insert path ds (lsDiagnostics (edLsp e))}})
    Notification "window/showMessage" params
      | Just text <- key "message" params >>= asText -> info (T.take 200 text)
    _ -> pure ()
  _ -> pure ()

-- | What to do with an answer.
answered :: Pending -> Value -> EditorM ()
answered pending value = case pending of
  PendingHover -> case parseHover value of
    [] -> info "no documentation here"
    ls -> modify' (\e -> e {edPopup = Just (InfoBox "hover" [(l, "") | l <- take 30 ls] AtCursor)})
  PendingDefinition -> goToLocations "definitions" (parseLocations value)
  PendingReferences -> goToLocations "references" (parseLocations value)
  PendingCompletion doc _ start -> do
    ed <- get
    let Pos l c = rangeHead (primary (docSelection (edDoc ed)))
        items = parseCompletion value
    -- Still typing that word in that document?
    if docId (edDoc ed) == doc && edMode ed == Insert && l == fst start && c >= snd start && not (null items)
      then showCompletion (Completion doc start items [] 0)
      else pure ()

-- | One location: go there. Several: pick one.
goToLocations :: Text -> [Location] -> EditorM ()
goToLocations title = \case
  [] -> info ("no " <> title)
  [loc] -> openAt loc
  locs ->
    modify' $ \e ->
      e
        { edPicker =
            Just
              ( newPicker
                  title
                  [ pickerItem (T.pack (locPath l <> ":" <> show (fst (locStart l) + 1))) (PickPosition (locPath l) (fst (locStart l)) (snd (locStart l))) ""
                  | l <- locs
                  ]
              )
        , edMode = Picking
        }

-- | Open a location's file and put the cursor there (the column is in the
-- server's units: approximated as characters).
openAt :: Location -> EditorM ()
openAt loc = do
  openFile (locPath loc)
  let (l, c) = locStart loc
  modifyDoc $ \d ->
    let line = min l (Buffer.lineCount (docBuffer d) - 1)
        col = min c (Buffer.lineLength line (docBuffer d))
     in d {docSelection = single (point (Pos (max 0 line) (max 0 col)))}

jumpDiagnostic :: Bool -> EditorM ()
jumpDiagnostic forward = do
  ed <- get
  let d = edDoc ed
      here = rangeHead (primary (docSelection d))
      positions = [Pos (sdLine sd) (sdStart sd) | sd <- shownDiagnostics (edLsp ed) (docLsp d) (docBuffer d)]
      target
        | forward = find (> here) positions <|> headMaybe positions
        | otherwise = find (< here) (reverse positions) <|> headMaybe (reverse positions)
  case target of
    Nothing -> info "no diagnostics"
    Just p -> modifyDoc (\doc -> doc {docSelection = single (point p)})
  where
    headMaybe = \case
      x : _ -> Just x
      [] -> Nothing
    a <|> b = maybe b Just a

-- * Completion

-- | Characters that make up a word to complete.
isWordChar :: Char -> Bool
isWordChar c = isAlphaNum c || c == '_'

-- | Where the word before the cursor starts, and the word.
wordBeforeCursor :: Editor -> ((Int, Int), Text)
wordBeforeCursor ed =
  let d = edDoc ed
      Pos l c = rangeHead (primary (docSelection d))
      before = T.take c (Buffer.lineAt l (docBuffer d))
      word = T.takeWhileEnd isWordChar before
   in ((l, c - T.length word), word)

requestCompletion :: EditorM ()
requestCompletion = do
  ed <- get
  let (start, _) = wordBeforeCursor ed
      d = edDoc ed
  ask (PendingCompletion (docId d) (docVersion d) start) "textDocument/completion" []

-- | Show the menu filtered by what is typed now; closed when nothing
-- matches.
showCompletion :: Completion -> EditorM ()
showCompletion c = do
  ed <- get
  let d = edDoc ed
      Pos l col = rangeHead (primary (docSelection d))
      (sl, sc) = cmStart c
      typed = T.take (col - sc) (T.drop sc (Buffer.lineAt l (docBuffer d)))
      shown = take 100 (filterCompletion typed (cmItems c))
  modify' $ \e ->
    e
      { edCompletion =
          if l /= sl || col < sc || null shown
            then Nothing
            else Just c {cmShown = shown, cmSelected = min (cmSelected c) (length shown - 1)}
      }

-- | Items matching the typed text (fuzzy, on the filter text), best
-- first; the server's order breaks ties.
filterCompletion :: Text -> [CompletionItem] -> [CompletionItem]
filterCompletion typed items
  | T.null typed = sortOn ciSort items
  | otherwise =
      map snd . sortOn fst $
        [((score, ciSort i), i) | i <- items, Just score <- [fuzzyScore typed (ciFilter i)]]

moveCompletion :: Int -> EditorM ()
moveCompletion n = modify' $ \e ->
  e {edCompletion = (\c -> c {cmSelected = (cmSelected c + n) `mod` max 1 (length (cmShown c))}) <$> edCompletion e}

-- | Replace the typed word (at every cursor) with the selected item.
acceptCompletion :: EditorM ()
acceptCompletion =
  gets edCompletion >>= \case
    Nothing -> pure ()
    Just c -> case drop (cmSelected c) (cmShown c) of
      [] -> modify' (\e -> e {edCompletion = Nothing})
      item : _ -> do
        ed <- get
        let Pos _ col = rangeHead (primary (docSelection (edDoc ed)))
            typedLength = col - snd (cmStart c)
        modify' (\e -> e {edCompletion = Nothing})
        edit $ \b r ->
          let Pos hl hc = rangeHead r
              from = Pos hl (max 0 (hc - typedLength))
              (b', end) = Buffer.insertText from (ciInsert item) (Buffer.deleteRange from (Pos hl hc) b)
           in (b', point end)

-- | After every event in insert mode: keep the menu in step with the
-- typing, or open it on its own after a trigger character or the second
-- character of a word.
completionHousekeeping :: EditorM ()
completionHousekeeping = do
  ed <- get
  let d = edDoc ed
  case (edCompletion ed, edMode ed) of
    (Just c, Insert) | cmDoc c == docId d -> showCompletion c
    (Just _, _) -> modify' (\e -> e {edCompletion = Nothing})
    (Nothing, Insert)
      | LspAttached at <- docLsp d
      , docVersion d /= lsAutoVersion (edLsp ed)
      , not (any isCompletion (IntMap.elems (lsPending (edLsp ed)))) -> do
          modify' (\e -> e {edLsp = (edLsp e) {lsAutoVersion = docVersion d}})
          let (_, word) = wordBeforeCursor ed
              Pos l c = rangeHead (primary (docSelection d))
              previous = T.takeEnd 1 (T.take c (Buffer.lineAt l (docBuffer d)))
              triggers = maybe [] siTriggers (Map.lookup (atServer at) (lsServers (edLsp ed)))
          if (not (T.null previous) && previous `elem` triggers) || T.length word >= 2
            then requestCompletion
            else pure ()
    _ -> pure ()
  where
    isCompletion = \case
      PendingCompletion {} -> True
      _ -> False

-- * Commands

exCommands :: [ExCommand]
exCommands =
  [ ExCommand ["lsp-info"] "Show the language server of this file" NoArgs $ \_ -> do
      ed <- get
      info $ case docLsp (edDoc ed) of
        LspAttached at -> case Map.lookup (atServer at) (lsServers (edLsp ed)) of
          Just si ->
            siName si <> " in " <> T.pack (siRoot si) <> " (" <> encodingName (siEncoding si) <> ", " <> syncName (siSync si) <> " sync)"
          Nothing -> atServer at
        LspStarting -> "the language server is starting"
        _ -> "no language server for this file"
  , ExCommand ["lsp-stop"] "Stop the language server of this file" NoArgs $ \_ ->
      withServer $ \server -> do
        stopServer server LspNone
        info "language server stopped (:lsp-start starts it again)"
  , ExCommand ["lsp-start"] "Start the language server of this file" NoArgs $ \_ -> do
      d <- getDoc
      case docLsp d of
        LspAttached _ -> info "the language server is running (:lsp-restart restarts it)"
        LspStarting -> info "the language server is starting"
        _ -> modifyDoc (\doc -> doc {docLsp = LspUnknown})
  , ExCommand ["lsp-restart"] "Restart the language server of this file" NoArgs $ \_ ->
      withServer $ \server -> do
        -- Every document it served attaches again (the current one now,
        -- the others when they are shown).
        stopServer server LspUnknown
        info "restarting the language server"
  ]
  where
    withServer k =
      docLsp <$> getDoc >>= \case
        LspAttached at -> k (atServer at)
        _ -> failWith "no language server for this file"
    syncName = \case
      SyncNone -> "no"
      SyncFull -> "full"
      SyncIncremental -> "incremental"

-- | Stop a server and forget it; its documents get the given state, and
-- its requests and diagnostics are dropped.
stopServer :: Text -> DocLsp -> EditorM ()
stopServer server after = do
  ed <- get
  let served = [p | d <- allDocs ed, LspAttached at <- [docLsp d], atServer at == server, let p = atPath at]
  request (LspStop server)
  modify' $ \e ->
    (mapDocuments (\d -> case docLsp d of LspAttached at | atServer at == server -> d {docLsp = after}; _ -> d) e)
      { edLsp =
          (edLsp e)
            { lsServers = Map.delete server (lsServers (edLsp e))
            , lsDiagnostics = foldr Map.delete (lsDiagnostics (edLsp e)) served
            , lsPending = IntMap.empty
            }
      , edCompletion = Nothing
      }
  where
    allDocs ed = map bufDoc (fst (buffers ed))
