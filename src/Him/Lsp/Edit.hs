-- | Text edits from language servers (ADR lsp-client): formatting results, renames
-- and code actions all arrive as edits, per file.
module Him.Lsp.Edit
  ( TextEdit (..)
  , parseTextEdits
  , parseWorkspaceEdit
  , applyTextEdits
  ) where

import Data.List (sortOn)
import Data.Ord (Down (..))
import Him.Buffer (Buffer)
import Him.Buffer qualified as Buffer
import Him.Json
import Him.Lsp.Protocol (Encoding, TextEdit (..), fromLspColumn, parseTextEdits, uriToPath)
import Him.Position (Pos (..))

-- | The edits of a @WorkspaceEdit@, per file: its @changes@ map, or its
-- @documentChanges@ (file creations, renames and deletions are skipped).
parseWorkspaceEdit :: Value -> [(FilePath, [TextEdit])]
parseWorkspaceEdit v = case (key "documentChanges" v >>= asArray, key "changes" v >>= asObject) of
  (Just changes, _) ->
    [ (file, parseTextEdits edits)
    | c <- changes
    , Just uri <- [key "textDocument" c >>= key "uri" >>= asText]
    , Just file <- [uriToPath uri]
    , Just edits <- [key "edits" c]
    ]
  (_, Just byUri) -> [(file, parseTextEdits edits) | (uri, edits) <- byUri, Just file <- [uriToPath uri]]
  _ -> []

-- | Apply edits (all made against the same text, not overlapping) from the
-- last to the first, so earlier positions stay valid; each position's
-- column is converted against its line as it is then. Insertions at the
-- same place end up in the order given (the protocol's rule).
applyTextEdits :: Encoding -> [TextEdit] -> Buffer -> Buffer
applyTextEdits enc edits buf0 = foldl apply buf0 (map snd (sortOn (\(i, e) -> Down (teStart e, i)) (zip [0 :: Int ..] edits)))
  where
    apply buf e =
      let from = toPos buf (teStart e)
          to = max from (toPos buf (teEnd e))
       in fst (Buffer.insertText from (teText e) (Buffer.deleteRange from to buf))
    toPos buf (l, c)
      | l >= Buffer.lineCount buf = Buffer.endPos buf
      | otherwise = Pos (max 0 l) (fromLspColumn enc (Buffer.lineAt l buf) c)
