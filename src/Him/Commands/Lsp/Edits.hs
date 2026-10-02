-- | Changing text with the language server: its edits to one document or
-- the workspace, code actions, formatting, renaming.
module Him.Commands.Lsp.Edits
  ( applyToDocument
  , applyWorkspaceEdit
  , runCodeAction
  , codeActions
  , formatDocument
  , renameTo
  , wordAroundCursor
  ) where

import Control.Monad.Trans.State.Strict (get, gets, modify')
import Data.Map.Strict qualified as Map
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Text qualified as T
import Him.Buffer qualified as Buffer
import Him.Command
import Him.Commands.File (openFile)
import Him.Document (Document (..), clampSelection, replaceBuffer)
import Him.Options (Options (..))
import Him.Editor hiding (Severity (..))
import Him.Json hiding (path)
import Him.Lsp.Protocol hiding (request)
import Him.Lsp.State
import Him.Position (Pos (..))
import Control.Monad.IO.Class (liftIO)
import Him.Lsp.Edit
import Him.Selection (Range (..), primary, rangeEnd, rangeHead, rangeStart)
import System.Directory (makeAbsolute)
import Him.Commands.Lsp.Core

-- | Apply a server's edits to a document as one undoable change.
applyToDocument :: Encoding -> [TextEdit] -> Document -> Document
applyToDocument enc edits d =
  let buf = applyTextEdits enc edits (docBuffer d)
   in replaceBuffer buf (clampSelection buf (docSelection d)) d

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
