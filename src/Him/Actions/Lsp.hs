-- | The language-server client in the editor (ADR-29): attaching documents
-- to servers, keeping the server's copy of the text current, asking
-- questions (hover, definition, references) and applying the answers, and
-- diagnostics. Messages are built and read as pure data; the runtime only
-- moves them ("Him.Lsp.Server").
module Him.Actions.Lsp
  ( lspPlugin
  , actions
  , lspHousekeeping
  , lspFlush
  , applyLspResult
  , completionHousekeeping
  , exCommands
  , runCodeAction
  , renameTo
  , queryWorkspaceSymbols
  ) where

import Control.Monad.Trans.State.Strict (get, gets, modify')
import Data.IntMap.Strict qualified as IntMap
import Data.Map.Strict qualified as Map
import Data.Maybe (fromMaybe)
import Data.Text qualified as T
import Him.Action hiding (text)
import Him.EditorM
import Him.Document (Document (..))
import Him.Effect (Effect (..), JobResult (..))
import Him.Options (Options (..))
import Him.Editor hiding (Severity (..))
import Him.Json hiding (path)
import Him.Lsp.Protocol hiding (request)
import Him.Lsp.State
import Him.Ex (ExArgs (..), ExCommand (..))
import Him.Mode (Mode (..))
import Him.Config (Plugin (..), plugin)
import Him.Picker (PickTarget (..), Picker (..), PickerSource (..), newPicker, pickerItem)
import Him.Position (Pos (..))
import Him.Lsp.Edit
import Him.Selection (Range (..), primary, rangeHead)
import Him.Actions.Lsp.Core
import Him.Actions.Lsp.Navigation
import Him.Actions.Lsp.Edits
import Him.Actions.Lsp.Completion

-- | The language-server client (ADR-29) as a plugin (ADR-35).
lspPlugin :: Plugin
lspPlugin =
  (plugin "lsp" "Language servers: diagnostics, hover, go to, completion, rename, format, code actions")
    { plActions = actions
    , plBindings =
        Map.fromList
          [ ( Normal
            ,
              [ ("space k", "lsp_hover")
              , ("space x", "diagnostics_picker")
              , ("g d", "goto_definition")
              , ("g r", "goto_references")
              , ("g y", "goto_type_definition")
              , ("g i", "goto_implementation")
              , ("space r", "rename_symbol")
              , ("space a", "code_action")
              , ("space s", "document_symbols")
              , ("space S", "workspace_symbols")
              , ("] d", "goto_next_diagnostic")
              , ("[ d", "goto_prev_diagnostic")
              ]
            )
          , (Insert, [("C-x", "completion")])
          , -- Insert mode with the completion menu open is insert mode with these.
            ( Completing
            ,
              [ ("tab", "completion_next")
              , ("C-n", "completion_next")
              , ("down", "completion_next")
              , ("S-tab", "completion_previous")
              , ("C-p", "completion_previous")
              , ("up", "completion_previous")
              , ("ret", "completion_accept")
              , ("esc", "completion_cancel")
              ]
            )
          ]
    , plExCommands = exCommands
    , plSigns = True
    , plHousekeeping = lspHousekeeping >> completionHousekeeping
    , plBeforeRender = lspFlush
    , plJobResult = applyLspResult
    , -- Documents attach again as they are shown.
      plEnable = modify' (mapDocuments (\d -> d {docLsp = LspUnknown}))
    , plDisable = do
        request LspStopAll
        modify' $ \e ->
          (mapDocuments (\d -> d {docLsp = LspUnknown}) e)
            { edLsp = emptyLsp
            , edCompletion = Nothing
            , edPopup = Nothing
            }
    }

actions :: [Action]
actions =
  [ simple "lsp_hover" GLsp "Show documentation for the symbol under the cursor" $
      ask PendingHover "textDocument/hover" []
  , simple "goto_definition" GLsp "Go to the definition of the symbol under the cursor" $
      ask (PendingLocations "definitions") "textDocument/definition" []
  , simple "goto_type_definition" GLsp "Go to the definition of the type of the symbol under the cursor" $
      ask (PendingLocations "type definitions") "textDocument/typeDefinition" []
  , simple "goto_implementation" GLsp "Go to the implementations of the symbol under the cursor" $
      ask (PendingLocations "implementations") "textDocument/implementation" []
  , simple "goto_references" GLsp "List the references to the symbol under the cursor" $
      ask (PendingLocations "references") "textDocument/references" [("context", object [("includeDeclaration", JBool True)])]
  , simple "rename_symbol" GLsp "Rename the symbol under the cursor everywhere" $ do
      ed <- get
      case docLsp (edDoc ed) of
        LspAttached _ -> do
          let ((l, c), word) = wordAroundCursor ed
          modify' (\e -> e {edPrompt = RenamePrompt (l, c), edCmdLine = word, edCompletions = Nothing})
          setMode CmdLine
        _ -> failWith "no language server for this file"
  , simple "format_document" GLsp "Format the file with the language server" formatDocument
  , simple "code_action" GLsp "List the language server's actions for the selection" codeActions
  , simple "workspace_symbols" GLsp "Search the symbols of the whole project (as you type)" $ do
      d <- getDoc
      case docLsp d of
        LspAttached at -> do
          gen <- gets edNextId
          modify' $ \e ->
            e
              { edNextId = gen + 1
              , edPicker = Just (newPicker "workspace symbols" []) {pkGeneration = gen, pkSource = ServerQuery (atServer at), pkLoading = True}
              , edMode = Picking
              }
          queryWorkspaceSymbols (atServer at) gen ""
        _ -> failWith "no language server for this file"
  , simple "document_symbols" GLsp "List the symbols of this file" $ do
      d <- getDoc
      case docLsp d of
        LspAttached at ->
          sendRequest (atServer at) (PendingSymbols (atPath at)) "textDocument/documentSymbol" (object [("textDocument", docIdentifier at)])
        _ -> failWith "no language server for this file"
  , simple "diagnostics_picker" GLsp "List the diagnostics of this file" $ do
      ed <- get
      let d = edDoc ed
          path = fromMaybe "" (docPath d)
          items =
            [ pickerItem
                (T.pack (show (sdLine sd + 1) <> ":" <> show (sdStart sd + 1)) <> "  " <> severityName (sdSeverity sd))
                (PickPosition path (sdLine sd) (sdStart sd) Nothing)
                (T.takeWhile (/= '\n') (sdMessage sd))
            | sd <- dedupe (shownDiagnostics (edLsp ed) (docLsp d) (docBuffer d))
            ]
      if null items
        then info "no diagnostics"
        else openPicker (newPicker "diagnostics" items)
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
    ServerRequest _ "workspace/applyEdit" params
      | Just e <- key "edit" params -> () <$ applyWorkspaceEdit (parseWorkspaceEdit e)
    Notification "window/showMessage" params
      | Just text <- key "message" params >>= asText -> info (T.take 200 text)
    _ -> pure ()
  _ -> pure ()

-- | What to do with an answer.
answered :: Pending -> Value -> EditorM ()
answered pending value = case pending of
  PendingHover -> case parseHover value of
    [] -> info "no documentation here"
    ls -> modify' (\e -> e {edPopup = Just (InfoBox "hover" [(l, "") | l <- take (optHoverLines (edOptions e)) ls] AtCursor Nothing)})
  PendingLocations title -> goToLocations title (parseLocations value)
  PendingCompletion doc _ start -> do
    ed <- get
    let Pos l c = rangeHead (primary (docSelection (edDoc ed)))
        items = parseCompletion value
    -- Still typing that word in that document?
    if docId (edDoc ed) == doc && edMode ed == Insert && l == fst start && c >= snd start && not (null items)
      then showCompletion (Completion doc start items [] 0)
      else pure ()
  PendingRename -> case parseWorkspaceEdit value of
    [] -> info "nothing to rename"
    changes -> do
      n <- applyWorkspaceEdit changes
      info ("renamed in " <> T.pack (show n) <> " file" <> (if n == 1 then "" else "s"))
  PendingFormat doc version -> do
    d <- getDoc
    case parseTextEdits value of
      [] -> info "already formatted"
      edits
        | docId d == doc && docVersion d == version -> do
            enc <- currentEncoding
            modifyDoc (applyToDocument enc edits)
            info "formatted"
        | otherwise -> info "the file changed while formatting; try again"
  PendingCodeActions -> case fromMaybe [] (asArray value) of
    [] -> info "no code actions here"
    acts ->
      modify' $ \e ->
        e
          { edPicker =
              Just
                ( newPicker
                    "code actions"
                    [ pickerItem title (PickCodeAction a) (fromMaybe "" (key "kind" a >>= asText))
                    | a <- acts
                    , Just title <- [key "title" a >>= asText]
                    ]
                )
          , edMode = Picking
          }
  PendingResolve -> runCodeAction value
  PendingSignature -> case signatureLines value of
    [] -> pure ()
    ls -> modify' (\e -> e {edPopup = Just (InfoBox "signature" [(l, "") | l <- ls] AboveCursor Nothing)})
  PendingSymbols file -> do
    enc <- currentEncoding
    case symbols value of
      [] -> info "no symbols"
      found ->
        modify' $ \e ->
          e
            { edPicker =
                Just
                  ( newPicker
                      "symbols"
                      [ pickerItem (T.replicate (2 * depth) " " <> name) (PickPosition file l c (Just (encodingName enc))) (kind <> "  " <> T.pack (show (l + 1)))
                      | (depth, name, kind, (l, c)) <- found
                      ]
                  )
            , edMode = Picking
            }
  PendingCompletionResolve doc version -> do
    d <- getDoc
    enc <- currentEncoding
    case maybe [] parseTextEdits (key "additionalTextEdits" value) of
      edits@(_ : _)
        | docId d == doc && docVersion d == version -> modifyDoc (applyAdditional enc edits)
      _ -> pure ()
  PendingWorkspaceSymbols gen query -> workspaceSymbolsArrived gen query value
  PendingIgnore -> pure ()

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
  , ExCommand ["format", "fmt"] "Format the file with the language server" NoArgs $ \_ -> formatDocument
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
