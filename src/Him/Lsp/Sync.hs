-- | Keeping a server's copy of a document current (ADR-29): the messages
-- for opening, changing (incrementally when the server allows it), saving
-- and closing a document.
--
-- The server's copy is the buffer's text plus the final line break the
-- file has. Edits never touch that line break, so changes computed on the
-- buffer ('Him.Buffer.changeBetween') apply to the server's copy as they
-- are.
module Him.Lsp.Sync
  ( syncMessages
  , closeEffects
  ) where

import Him.Buffer qualified as Buffer
import Him.Document (Document (..))
import Him.Effect (Effect (..))
import Him.Json
import Him.Lsp.Protocol
import Him.Lsp.State
import Him.Position (Pos (..))

-- | What to send so the server has the document as it is now, and the
-- attachment afterwards. Nothing when it is current.
syncMessages :: Maybe ServerInfo -> Attachment -> Document -> ([Value], Attachment)
syncMessages info at d = (opened <> saved, at')
  where
    uri = JString (pathToUri (atPath at))
    version = JInt (fromIntegral (docVersion d))
    fullText = Buffer.toText (docBuffer d) <> if docTrailingNewline d then "\n" else ""
    sync = maybe SyncFull siSync info
    enc = maybe Utf16 siEncoding info
    opened = case atSentText at of
      _ | atSent at == docVersion d -> []
      Nothing ->
        [ notification "textDocument/didOpen" $
            object [("textDocument", object [("uri", uri), ("languageId", JString (atLanguageId at)), ("version", version), ("text", JString fullText)])]
        ]
      Just previous -> case sync of
        SyncNone -> []
        SyncFull -> [change [object [("text", JString fullText)]]]
        SyncIncremental -> case Buffer.changeBetween previous (docBuffer d) of
          Nothing -> []
          Just (from, to, text) ->
            [change [object [("range", object [("start", position previous from), ("end", position previous to)]), ("text", JString text)]]]
    change events = notification "textDocument/didChange" $ object [("textDocument", object [("uri", uri), ("version", version)]), ("contentChanges", JArray events)]
    position buf (Pos l c) = object [("line", JInt (fromIntegral l)), ("character", JInt (fromIntegral (toLspColumn enc (Buffer.lineAt l buf) c)))]
    saved =
      [ notification "textDocument/didSave" (object [("textDocument", object [("uri", uri)])])
      | atSaves at /= docSaves d
      ]
    at' = at {atSent = docVersion d, atSentText = Just (docBuffer d), atSaves = docSaves d}

-- | Tell a document's server that it is closed.
closeEffects :: Document -> [Effect]
closeEffects d = case docLsp d of
  LspAttached at | atSent at >= 0 ->
    [LspSend (atServer at) (notification "textDocument/didClose" (object [("textDocument", object [("uri", JString (pathToUri (atPath at)))])]))]
  _ -> []
