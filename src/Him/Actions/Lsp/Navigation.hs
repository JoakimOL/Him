-- | Going places with the language server: definitions and references,
-- document and workspace symbols, diagnostics.
module Him.Actions.Lsp.Navigation
  ( symbols
  , symbolKindName
  , goToLocations
  , openAt
  , jumpDiagnostic
  , queryWorkspaceSymbols
  , workspaceSymbolsArrived
  ) where

import Control.Monad.Trans.State.Strict (get, modify')
import Data.List (find)
import Data.Map.Strict qualified as Map
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Text qualified as T
import Him.Buffer qualified as Buffer
import Him.EditorM
import Him.Actions.Jump (jumping)
import Him.Actions.File (openFile)
import Him.Document (Document (..))
import Him.Editor hiding (Severity (..))
import Him.Json hiding (path)
import Him.Lsp.Protocol hiding (request)
import Him.Lsp.State
import Him.Mode (Mode (..))
import Data.Sequence qualified as Seq
import Him.Picker (PickTarget (..), Picker (..), PickerSource (..), labelWidth, matchLimit, newPicker, pickerItem)
import System.FilePath (makeRelative)
import Him.Position (Pos (..))
import Him.Selection (Range (..), point, primary, rangeHead, single)
import Him.Actions.Lsp.Core

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
openAt enc loc = jumping $ do
  openFile (locPath loc)
  let (l, c) = locStart loc
  modifyDoc $ \d ->
    let line = max 0 (min l (Buffer.lineCount (docBuffer d) - 1))
        col = fromLspColumn enc (Buffer.lineAt line (docBuffer d)) c
     in d {docSelection = single (point (Pos line col))}

jumpDiagnostic :: Bool -> EditorM ()
jumpDiagnostic forward = jumping $ do
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
                _ -> ""
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
