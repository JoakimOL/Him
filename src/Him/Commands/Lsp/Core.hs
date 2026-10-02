-- | The LSP client's plumbing: sending requests about the cursor, keeping
-- documents attached and the server's text current, positions and
-- encodings, and stopping servers.
module Him.Commands.Lsp.Core
  ( severityName
  , ask
  , sendRequest
  , docIdentifier
  , cursorPosition
  , lspPosition
  , encodingOf
  , lspHousekeeping
  , lspFlush
  , currentRoot
  , currentEncoding
  , stopServer
  , isWordChar
  ) where

import Control.Monad.Trans.State.Strict (get, gets, modify')
import Data.IntMap.Strict qualified as IntMap
import Data.Char (isAlphaNum)
import Data.Map.Strict qualified as Map
import Data.Text (Text)
import Him.Buffer qualified as Buffer
import Him.Command
import Him.Document (DocKind (..), Document (..))
import Him.Effect (Effect (..), Job (..))
import Him.Editor hiding (Severity (..))
import Him.Json hiding (path)
import Him.Language (detectLanguage, languages)
import Him.Lsp.Protocol hiding (request)
import Him.Lsp.Protocol qualified as P
import Him.Lsp.State
import Him.Lsp.Sync (syncMessages)
import Him.Position (Pos (..))
import Him.Selection (Range (..), primary, rangeHead)

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

-- | Characters that make up a word to complete.
isWordChar :: Char -> Bool
isWordChar c = isAlphaNum c || c == '_'
