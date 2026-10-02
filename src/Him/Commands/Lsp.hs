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
  , runCodeAction
  , renameTo
  , queryWorkspaceSymbols
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
import Him.Options (Options (..))
import Him.Editor hiding (Severity (..))
import Him.Json hiding (path)
import Him.Json qualified as J
import Him.Language (detectLanguage, languages)
import Him.Lsp.Protocol hiding (request)
import Him.Lsp.Protocol qualified as P
import Him.Lsp.State
import Him.Lsp.Sync (syncMessages)
import Him.Ex (ExArgs (..), ExCommand (..))
import Him.Mode (Mode (..))
import Data.Sequence qualified as Seq
import Him.Picker (PickTarget (..), Picker (..), PickerSource (..), fuzzyScore, labelWidth, matchLimit, newPicker, pickerItem)
import System.FilePath (makeRelative)
import Him.Position (Pos (..))
import Control.Monad.IO.Class (liftIO)
import Him.History (Snapshot (..), beginChange)
import Him.Lsp.Edit
import Him.Selection (Range (..), mapRanges, point, primary, rangeEnd, rangeHead, rangeStart, single)
import System.Directory (makeAbsolute)

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
          modify' (\e -> e {edPrompt = RenamePrompt (l, c), edCmdLine = word, edCompletions = []})
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
    LspAttached at ->
      sendRequest (atServer at) pending method $
        object ([("textDocument", docIdentifier at), ("position", cursorPosition ed at)] <> extra)
    LspStarting -> info "the language server is still starting"
    _ -> failWith "no language server for this file"

-- | Send a request (after bringing the server's text up to date),
-- remembering what to do with the answer.
sendRequest :: Text -> Pending -> Text -> Value -> EditorM ()
sendRequest server pending method params = do
  lspFlush
  i <- gets edNextId
  modify' $ \e ->
    e {edNextId = i + 1, edLsp = (edLsp e) {lsPending = IntMap.insert i pending (lsPending (edLsp e))}}
  request (LspSend server (P.request i method params))

docIdentifier :: Attachment -> Value
docIdentifier at = object [("uri", JString (pathToUri (atPath at)))]

-- | The cursor as an LSP position.
cursorPosition :: Editor -> Attachment -> Value
cursorPosition ed at =
  let d = edDoc ed
      Pos l c = rangeHead (primary (docSelection d))
   in lspPosition (encodingOf ed at) (docBuffer d) l c

lspPosition :: Encoding -> Buffer.Buffer -> Int -> Int -> Value
lspPosition enc buf l c =
  object [("line", JInt (fromIntegral l)), ("character", JInt (fromIntegral (toLspColumn enc (Buffer.lineAt l buf) c)))]

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
    ls -> modify' (\e -> e {edPopup = Just (InfoBox "hover" [(l, "") | l <- take (optHoverLines (edOptions e)) ls] AtCursor)})
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
    ls -> modify' (\e -> e {edPopup = Just (InfoBox "signature" [(l, "") | l <- ls] AboveCursor)})
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

-- | The active signature, and the first line of its documentation.
signatureLines :: Value -> [Text]
signatureLines v = case key "signatures" v >>= asArray of
  Just sigs@(_ : _) ->
    let active = fromMaybe 0 (key "activeSignature" v >>= asInt)
        sig = sigs !! max 0 (min active (length sigs - 1))
        label = fromMaybe "" (key "label" sig >>= asText)
        doc = case key "documentation" sig of
          Just (JString t) -> t
          Just o -> fromMaybe "" (key "value" o >>= asText)
          Nothing -> ""
     in label : take 1 (filter (not . T.null) (T.lines doc))
  _ -> []

-- | Document symbols, flattened: depth, name, kind, position (server
-- units). Both the nested and the flat form are read.
symbols :: Value -> [(Int, Text, Text, (Int, Int))]
symbols = go 0 . fromMaybe [] . asArray
  where
    go depth = concatMap (one depth)
    one depth s = case key "name" s >>= asText of
      Nothing -> []
      Just name ->
        let at' = (key "selectionRange" s <|> key "range" s <|> (key "location" s >>= key "range")) >>= key "start" >>= position'
            kind = maybe "" kindName (key "kind" s >>= asInt)
            children = maybe [] (go (depth + 1)) (key "children" s >>= asArray)
         in [(depth, name, kind, p) | Just p <- [at']] <> children
    position' p = (,) <$> (key "line" p >>= asInt) <*> (key "character" p >>= asInt)
    a <|> b = maybe b Just a
    kindName = symbolKindName

-- | The protocol's symbol kinds, by number.
symbolKindName :: Int -> Text
symbolKindName k =
  fromMaybe "" . lookup k . zip [1 ..] $
    ["file", "module", "namespace", "package", "class", "method", "property", "field", "constructor", "enum", "interface", "function", "variable", "constant", "string", "number", "boolean", "array", "object", "key", "null", "enum member", "struct", "event", "operator", "type parameter"]

-- | The project root of the current document's server (for short paths).
currentRoot :: EditorM FilePath
currentRoot = do
  ed <- get
  pure $ case docLsp (edDoc ed) of
    LspAttached at -> maybe "" siRoot (Map.lookup (atServer at) (lsServers (edLsp ed)))
    _ -> ""

currentEncoding :: EditorM Encoding
currentEncoding = do
  ed <- get
  pure $ case docLsp (edDoc ed) of
    LspAttached at -> encodingOf ed at
    _ -> Utf16

-- | Apply a server's edits to a document as one undoable change.
applyToDocument :: Encoding -> [TextEdit] -> Document -> Document
applyToDocument enc edits d =
  let buf = applyTextEdits enc edits (docBuffer d)
   in d
        { docBuffer = buf
        , docSelection = mapRanges (\r -> r {rangeAnchor = Buffer.clampPos buf (rangeAnchor r), rangeHead = Buffer.clampPos buf (rangeHead r)}) (docSelection d)
        , docDirty = buf /= docSavedBuffer d
        , docVersion = docVersion d + 1
        , docHistory = beginChange (Snapshot (docBuffer d) (docSelection d)) (docHistory d)
        }

-- | Apply edits to several files: open buffers are changed in place, other
-- files are opened (and left modified, to be saved). The current buffer
-- stays current. Returns how many files changed.
applyWorkspaceEdit :: [(FilePath, [TextEdit])] -> EditorM Int
applyWorkspaceEdit changes = do
  enc <- currentEncoding
  origin <- gets (docId . edDoc)
  mapM_ (applyFile enc) changes
  -- Back to where we were.
  (bs, _) <- gets buffers
  case [i | (i, b) <- zip [0 ..] bs, docId (bufDoc b) == origin] of
    i : _ -> modify' (gotoBuffer i)
    [] -> pure ()
  pure (length changes)
  where
    applyFile enc (file, edits) = do
      (bs, _) <- gets buffers
      absolute <- liftIO (traverse (traverse makeAbsolute . docPath . bufDoc) bs)
      let matches =
            [ docId d
            | (b, abs') <- zip bs absolute
            , let d = bufDoc b
            , case docLsp d of
                LspAttached at -> atPath at == file
                _ -> abs' == Just file
            ]
      case matches of
        doc : _ -> modify' (modifyDocument doc (applyToDocument enc edits))
        [] -> do
          openFile file
          modifyDoc (applyToDocument enc edits)

-- | Run a code action picked from the list: apply its edit and run its
-- command; one that comes without either is asked for in full first.
runCodeAction :: Value -> EditorM ()
runCodeAction act = do
  d <- getDoc
  case docLsp d of
    LspAttached at -> do
      let edit' = key "edit" act
          command = case key "command" act of
            Just c@(JObject _) -> Just c
            Just (JString _) -> Just act
            _ -> Nothing
      case (edit', command) of
        (Nothing, Nothing)
          | Just _ <- key "data" act -> sendRequest (atServer at) PendingResolve "codeAction/resolve" act
          | otherwise -> info "this action does nothing"
        _ -> do
          mapM_ (\e -> applyWorkspaceEdit (parseWorkspaceEdit e)) edit'
          mapM_ (execute at) command
    _ -> failWith "no language server for this file"
  where
    execute at c = case key "command" c >>= asText of
      Just name ->
        sendRequest (atServer at) PendingIgnore "workspace/executeCommand" $
          object [("command", JString name), ("arguments", fromMaybe (JArray []) (key "arguments" c))]
      Nothing -> pure ()

codeActions :: EditorM ()
codeActions = do
  ed <- get
  let d = edDoc ed
  case docLsp d of
    LspAttached at -> do
      let r = primary (docSelection d)
          enc = encodingOf ed at
          Pos sl sc = rangeStart r
          Pos el ec = rangeEnd r
          buf = docBuffer d
          -- The diagnostics on the selected lines, as the server sent them.
          diagnostics =
            [ diagRaw dg
            | dg <- Map.findWithDefault [] (atPath at) (lsDiagnostics (edLsp ed))
            , fst (diagStart dg) <= el
            , fst (diagEnd dg) >= sl
            ]
      sendRequest (atServer at) PendingCodeActions "textDocument/codeAction" $
        object
          [ ("textDocument", docIdentifier at)
          , ("range", object [("start", lspPosition enc buf sl sc), ("end", lspPosition enc buf el (min (ec + 1) (Buffer.lineLength el buf)))])
          , ("context", object [("diagnostics", JArray diagnostics)])
          ]
    _ -> failWith "no language server for this file"

formatDocument :: EditorM ()
formatDocument = do
  d <- getDoc
  o <- gets edOptions
  case docLsp d of
    LspAttached at ->
      sendRequest (atServer at) (PendingFormat (docId d) (docVersion d)) "textDocument/formatting" $
        object [("textDocument", docIdentifier at), ("options", object [("tabSize", JInt (toInteger (optTabWidth o))), ("insertSpaces", JBool (optExpandTab o))])]
    _ -> failWith "no language server for this file"

-- | Enter on the rename prompt.
renameTo :: (Int, Int) -> Text -> EditorM ()
renameTo (l, c) newName
  | T.null (T.strip newName) = pure ()
  | otherwise = do
      ed <- get
      case docLsp (edDoc ed) of
        LspAttached at ->
          sendRequest (atServer at) PendingRename "textDocument/rename" $
            object
              [ ("textDocument", docIdentifier at)
              , ("position", lspPosition (encodingOf ed at) (docBuffer (edDoc ed)) l c)
              , ("newName", JString (T.strip newName))
              ]
        _ -> failWith "no language server for this file"

-- | The word under (or just before) the cursor, and where the cursor is.
wordAroundCursor :: Editor -> ((Int, Int), Text)
wordAroundCursor ed =
  let d = edDoc ed
      Pos l c = rangeHead (primary (docSelection d))
      line = Buffer.lineAt l (docBuffer d)
      before = T.takeWhileEnd isWordChar (T.take c line)
      after = T.takeWhile isWordChar (T.drop c line)
   in ((l, c), before <> after)

-- | One location: go there. Several: pick one.
goToLocations :: Text -> [Location] -> EditorM ()
goToLocations title locations = do
  enc <- currentEncoding
  root <- currentRoot
  case locations of
    [] -> info ("no " <> title)
    [loc] -> openAt enc loc
    locs ->
      modify' $ \e ->
        e
          { edPicker =
              Just
                ( newPicker
                    title
                    [ pickerItem (T.pack (makeRelative root (locPath l) <> ":" <> show (fst (locStart l) + 1))) (PickPosition (locPath l) (fst (locStart l)) (snd (locStart l)) (Just (encodingName enc))) ""
                    | l <- locs
                    ]
                )
          , edMode = Picking
          }

-- | Open a location's file and put the cursor there, converting the
-- server's column against the line.
openAt :: Encoding -> Location -> EditorM ()
openAt enc loc = do
  openFile (locPath loc)
  let (l, c) = locStart loc
  modifyDoc $ \d ->
    let line = max 0 (min l (Buffer.lineCount (docBuffer d) - 1))
        col = fromLspColumn enc (Buffer.lineAt line (docBuffer d)) c
     in d {docSelection = single (point (Pos line col))}

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
        -- Imports and other edits elsewhere: given with the item, or asked
        -- for now if the server fills them in on request.
        enc <- currentEncoding
        d <- getDoc
        case (ciAdditional item, docLsp d) of
          (edits@(_ : _), _) -> modifyDoc (applyAdditional enc edits)
          ([], LspAttached at)
            | resolvable ed at ->
                sendRequest (atServer at) (PendingCompletionResolve (docId d) (docVersion d)) "completionItem/resolve" (ciRaw item)
          _ -> pure ()
  where
    resolvable ed at =
      maybe False (\si -> J.path ["completionProvider", "resolveProvider"] (siCapabilities si) == Just (JBool True)) (Map.lookup (atServer at) (lsServers (edLsp ed)))

-- | Edits that come with a completion (an import at the top): applied to the
-- text, with the cursors moved down by the lines added above them.
applyAdditional :: Encoding -> [TextEdit] -> Document -> Document
applyAdditional enc edits d =
  let buf = applyTextEdits enc edits (docBuffer d)
      shift (Pos l c) = Pos (l + sum [T.count "\n" (teText e) - (fst (teEnd e) - fst (teStart e)) | e <- edits, fst (teEnd e) < l]) c
   in d
        { docBuffer = buf
        , docSelection = mapRanges (\r -> r {rangeAnchor = Buffer.clampPos buf (shift (rangeAnchor r)), rangeHead = Buffer.clampPos buf (shift (rangeHead r))}) (docSelection d)
        , docDirty = True
        , docVersion = docVersion d + 1
        }

-- | After every event in insert mode: keep the completion menu in step
-- with the typing, or open it on its own after a trigger character or the
-- second character of a word; and ask for signature help after its
-- trigger characters (closing it at @)@).
completionHousekeeping :: EditorM ()
completionHousekeeping = menuHousekeeping >> signatureHousekeeping

signatureHousekeeping :: EditorM ()
signatureHousekeeping = do
  ed <- get
  let d = edDoc ed
  case (edMode ed, docLsp d) of
    (Insert, LspAttached at)
      | optAutoSignatureHelp (edOptions ed)
      , docVersion d /= lsSignatureVersion (edLsp ed) -> do
          modify' (\e -> e {edLsp = (edLsp e) {lsSignatureVersion = docVersion d}})
          let Pos l c = rangeHead (primary (docSelection d))
              previous = T.takeEnd 1 (T.take c (Buffer.lineAt l (docBuffer d)))
              triggers = maybe [] siSignatureTriggers (Map.lookup (atServer at) (lsServers (edLsp ed)))
          if previous == ")"
            then modify' (\e -> e {edPopup = Nothing})
            else
              if not (T.null previous) && previous `elem` triggers
                then ask PendingSignature "textDocument/signatureHelp" []
                else pure ()
    _ -> pure ()

menuHousekeeping :: EditorM ()
menuHousekeeping = do
  ed <- get
  let d = edDoc ed
  case (edCompletion ed, edMode ed) of
    (Just c, Insert) | cmDoc c == docId d -> showCompletion c
    (Just _, _) -> modify' (\e -> e {edCompletion = Nothing})
    (Nothing, Insert)
      | LspAttached at <- docLsp d
      , optAutoCompletion (edOptions ed)
      , docVersion d /= lsAutoVersion (edLsp ed)
      , not (any isCompletion (IntMap.elems (lsPending (edLsp ed)))) -> do
          modify' (\e -> e {edLsp = (edLsp e) {lsAutoVersion = docVersion d}})
          let (_, word) = wordBeforeCursor ed
              Pos l c = rangeHead (primary (docSelection d))
              previous = T.takeEnd 1 (T.take c (Buffer.lineAt l (docBuffer d)))
              triggers = maybe [] siTriggers (Map.lookup (atServer at) (lsServers (edLsp ed)))
          if (not (T.null previous) && previous `elem` triggers) || T.length word >= optCompletionTriggerLen (edOptions ed)
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

-- * Workspace symbols

-- | Ask for the symbols matching a query, for the picker of a generation.
queryWorkspaceSymbols :: Text -> Int -> Text -> EditorM ()
queryWorkspaceSymbols server gen query =
  sendRequest server (PendingWorkspaceSymbols gen query) "workspace/symbol" (object [("query", JString query)])

-- | The answer for a picker's query: shown in the server's order, if the
-- picker is still open and its query unchanged.
workspaceSymbolsArrived :: Int -> Text -> Value -> EditorM ()
workspaceSymbolsArrived gen query value = do
  ed <- get
  case edPicker ed of
    Just p
      | pkGeneration p == gen && pkQuery p == query -> do
          enc <- currentEncoding
          let root = case pkSource p of
                ServerQuery server -> maybe "" siRoot (Map.lookup server (lsServers (edLsp ed)))
                StaticItems -> ""
              items =
                [ pickerItem name (PickPosition file l c (Just (encodingName enc))) (T.intercalate "  " (filter (not . T.null) [kind, container, T.pack (makeRelative root file <> ":" <> show (l + 1))]))
                | s <- fromMaybe [] (asArray value)
                , Just name <- [key "name" s >>= asText]
                , Just loc <- [key "location" s]
                , Just file <- [key "uri" loc >>= asText >>= uriToPath]
                , let (l, c) = fromMaybe (0, 0) (key "range" loc >>= key "start" >>= \p' -> (,) <$> (key "line" p' >>= asInt) <*> (key "character" p' >>= asInt))
                      kind = maybe "" symbolKindName (key "kind" s >>= asInt)
                      container = fromMaybe "" (key "containerName" s >>= asText)
                ]
          modify' $ \e ->
            e
              { edPicker =
                  Just
                    p
                      { pkItems = Seq.fromList items
                      , pkLabelWidth = labelWidth items
                      , pkMatches = take matchLimit items
                      , pkMatchCount = length items
                      , pkSelected = 0
                      , pkStale = False
                      , pkLoading = False
                      }
              }
    _ -> pure ()
