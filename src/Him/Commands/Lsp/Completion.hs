-- | Completion while typing (the menu, accepting an item and its imports)
-- and signature help.
module Him.Commands.Lsp.Completion
  ( signatureLines
  , wordBeforeCursor
  , requestCompletion
  , showCompletion
  , filterCompletion
  , moveCompletion
  , acceptCompletion
  , applyAdditional
  , completionHousekeeping
  , signatureHousekeeping
  , menuHousekeeping
  ) where

import Control.Monad.Trans.State.Strict (get, gets, modify')
import Data.IntMap.Strict qualified as IntMap
import Data.List (sortOn)
import Data.Map.Strict qualified as Map
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Text qualified as T
import Him.Buffer qualified as Buffer
import Him.Command
import Him.Document (Document (..))
import Him.Options (Options (..))
import Him.Editor hiding (Severity (..))
import Him.Json hiding (path)
import Him.Json qualified as J
import Him.Lsp.Protocol hiding (request)
import Him.Lsp.State
import Him.Mode (Mode (..))
import Him.Picker (fuzzyScore)
import Him.Position (Pos (..))
import Him.Lsp.Edit
import Him.Selection (Range (..), mapRanges, point, primary, rangeHead)
import Him.Commands.Lsp.Core

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
