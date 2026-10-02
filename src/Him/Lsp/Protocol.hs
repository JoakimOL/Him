-- | The pure part of the LSP client (ADR-29): message framing, file URIs,
-- position encodings, and building and reading the messages him uses.
module Him.Lsp.Protocol
  ( -- * Framing
    Framer
  , emptyFramer
  , feedFramer
  , frameMessage
    -- * URIs
  , pathToUri
  , uriToPath
    -- * Positions
  , Encoding (..)
  , encodingName
  , toLspColumn
  , fromLspColumn
    -- * Messages
  , request
  , notification
  , response
  , Incoming (..)
  , classify
    -- * Reading results
  , Diagnostic (..)
  , Severity (..)
  , parseDiagnostics
  , Location (..)
  , parseLocations
  , parseHover
  , TextEdit (..)
  , parseTextEdits
  , CompletionItem (..)
  , parseCompletion
  , stripSnippet
  ) where

import Control.Applicative ((<|>))
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.ByteString.Char8 qualified as BC
import Data.Char (chr, digitToInt, isAlphaNum, isAscii, isHexDigit, ord)
import Data.Maybe (fromMaybe, mapMaybe)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding (decodeUtf8Lenient, encodeUtf8)
import Him.Json hiding (path)
import Numeric (showHex)

-- * Framing

-- | Bytes received so far that do not make a whole message yet.
newtype Framer = Framer ByteString
  deriving stock (Eq, Show)

emptyFramer :: Framer
emptyFramer = Framer BS.empty

-- | Add received bytes; get the complete message bodies, in order.
-- Messages are @Content-Length: N@ headers, a blank line, and N bytes.
feedFramer :: Framer -> ByteString -> ([ByteString], Framer)
feedFramer (Framer pending) new = go [] (pending <> new)
  where
    go acc buf = case BS.breakSubstring "\r\n\r\n" buf of
      (headers, rest)
        | not (BS.null rest)
        , Just len <- contentLength headers
        , BS.length rest - 4 >= len ->
            let body = BS.take len (BS.drop 4 rest)
             in go (body : acc) (BS.drop (4 + len) rest)
      _ -> (reverse acc, Framer buf)
    contentLength headers =
      case [v | line <- BC.lines headers, let (k, v) = BC.break (== ':') line, BC.map lower (BC.strip k) == "content-length"] of
        v : _ | Just (n, _) <- BC.readInt (BC.strip (BC.drop 1 v)) -> Just n
        _ -> Nothing
    lower c = if c >= 'A' && c <= 'Z' then chr (ord c + 32) else c

frameMessage :: Value -> ByteString
frameMessage v =
  let body = renderJson v
   in "Content-Length: " <> BC.pack (show (BS.length body)) <> "\r\n\r\n" <> body

-- * URIs

-- | @file://@ URI of an absolute path, percent-encoding what is not safe.
pathToUri :: FilePath -> Text
pathToUri path = "file://" <> T.pack (concatMap enc (BC.unpack (encodeUtf8 (T.pack path))))
  where
    enc c
      | isAscii c && isAlphaNum c || c `elem` ("/-_.~" :: String) = [c]
      | otherwise = '%' : pad (showHex (ord c) "")
    pad h = if length h == 1 then '0' : map upper h else map upper h
    upper c = if c >= 'a' && c <= 'f' then chr (ord c - 32) else c

uriToPath :: Text -> Maybe FilePath
uriToPath uri = T.unpack . decodeUtf8Lenient . BS.pack . map (fromIntegral . ord) . decode . T.unpack <$> T.stripPrefix "file://" uri
  where
    decode = \case
      '%' : a : b : rest | isHexDigit a && isHexDigit b -> chr (digitToInt a * 16 + digitToInt b) : decode rest
      c : rest -> c : decode rest
      [] -> []

-- * Positions

-- | How a server counts columns.
data Encoding = Utf8 | Utf16 | Utf32
  deriving stock (Eq, Show)

encodingName :: Encoding -> Text
encodingName = \case
  Utf8 -> "utf-8"
  Utf16 -> "utf-16"
  Utf32 -> "utf-32"

-- | A character column in a line, in the server's units.
toLspColumn :: Encoding -> Text -> Int -> Int
toLspColumn enc line col = sum (map (units enc) (T.unpack (T.take col line)))

-- | The character column at a column in the server's units.
fromLspColumn :: Encoding -> Text -> Int -> Int
fromLspColumn enc line target = go 0 0 (T.unpack line)
  where
    go chars acc rest
      | acc >= target = chars
      | otherwise = case rest of
          c : cs -> go (chars + 1) (acc + units enc c) cs
          [] -> chars

units :: Encoding -> Char -> Int
units enc c = case enc of
  Utf32 -> 1
  Utf16 -> if ord c >= 0x10000 then 2 else 1
  Utf8
    | ord c < 0x80 -> 1
    | ord c < 0x800 -> 2
    | ord c < 0x10000 -> 3
    | otherwise -> 4

-- * Messages

request :: Int -> Text -> Value -> Value
request i method params =
  object [("jsonrpc", JString "2.0"), ("id", JInt (fromIntegral i)), ("method", JString method), ("params", params)]

notification :: Text -> Value -> Value
notification method params =
  object [("jsonrpc", JString "2.0"), ("method", JString method), ("params", params)]

response :: Value -> Value -> Value
response i result = object [("jsonrpc", JString "2.0"), ("id", i), ("result", result)]

data Incoming
  = -- | A reply to one of our requests: id, and the result or the error.
    Reply !Int !(Either Text Value)
  | -- | The server asks something (id, method, params).
    ServerRequest !Value !Text !Value
  | Notification !Text !Value
  | Malformed
  deriving stock (Eq, Show)

classify :: Value -> Incoming
classify v = case (key "id" v, key "method" v >>= asText) of
  (Just i, Just method) -> ServerRequest i method (fromMaybe JNull (key "params" v))
  (Just i, Nothing)
    | Just n <- asInt i ->
        Reply n $ case key "error" v of
          Just e -> Left (fromMaybe "error" (key "message" e >>= asText))
          Nothing -> Right (fromMaybe JNull (key "result" v))
  (Nothing, Just method) -> Notification method (fromMaybe JNull (key "params" v))
  _ -> Malformed

-- * Reading results

data Severity = SevError | SevWarning | SevInfo | SevHint
  deriving stock (Eq, Ord, Show)

-- | A diagnostic, positions still in the server's units (converted when
-- shown, against the line text then current).
data Diagnostic = Diagnostic
  { diagStart :: !(Int, Int)
  , diagEnd :: !(Int, Int)
  , diagSeverity :: !Severity
  , diagMessage :: !Text
  , diagSource :: !Text
  , diagRaw :: !Value
  -- ^ As the server sent it (code-action requests send it back).
  }
  deriving stock (Eq, Show)

-- | @textDocument/publishDiagnostics@: the file and its diagnostics.
parseDiagnostics :: Value -> Maybe (FilePath, [Diagnostic])
parseDiagnostics params = do
  path <- key "uri" params >>= asText >>= uriToPath
  ds <- key "diagnostics" params >>= asArray
  pure (path, mapMaybe diagnostic ds)
  where
    diagnostic d = do
      (s, e) <- key "range" d >>= range
      msg <- key "message" d >>= asText
      let sev = case key "severity" d >>= asInt of
            Just 2 -> SevWarning
            Just 3 -> SevInfo
            Just 4 -> SevHint
            _ -> SevError
          source = fromMaybe "" (key "source" d >>= asText)
      pure (Diagnostic s e sev msg source d)

range :: Value -> Maybe ((Int, Int), (Int, Int))
range r = (,) <$> (key "start" r >>= position) <*> (key "end" r >>= position)

position :: Value -> Maybe (Int, Int)
position p = (,) <$> (key "line" p >>= asInt) <*> (key "character" p >>= asInt)

data Location = Location
  { locPath :: !FilePath
  , locStart :: !(Int, Int)
  -- ^ Line, and column in the server's units.
  }
  deriving stock (Eq, Show)

-- | A @Location@, a list of them, or @LocationLink@s.
parseLocations :: Value -> [Location]
parseLocations v = case v of
  JArray vs -> mapMaybe one vs
  JNull -> []
  _ -> mapMaybe one [v]
  where
    one l = case (key "uri" l, key "targetUri" l) of
      (Just u, _) -> Location <$> (asText u >>= uriToPath) <*> (key "range" l >>= fmap fst . range)
      (_, Just u) -> Location <$> (asText u >>= uriToPath) <*> ((key "targetSelectionRange" l >>= fmap fst . range) <|> (key "targetRange" l >>= fmap fst . range))
      _ -> Nothing

-- | Hover contents as plain lines (Markdown is shown as it is).
parseHover :: Value -> [Text]
parseHover v = case key "contents" v of
  Nothing -> []
  Just c -> trimBlank (concatMap T.lines (contents c))
  where
    contents = \case
      JString t -> [t]
      JArray vs -> concatMap contents vs
      o@(JObject _)
        | Just t <- key "value" o >>= asText -> [t]
        | otherwise -> []
      _ -> []
    trimBlank = reverse . dropWhile T.null . reverse . dropWhile T.null

-- | Replace the text between two positions (line, column in the server's
-- units).
data TextEdit = TextEdit
  { teStart :: !(Int, Int)
  , teEnd :: !(Int, Int)
  , teText :: !Text
  }
  deriving stock (Eq, Show)

parseTextEdits :: Value -> [TextEdit]
parseTextEdits = mapMaybe one . fromMaybe [] . asArray
  where
    one e = do
      r <- key "range" e
      s <- key "start" r >>= pos
      t <- key "end" r >>= pos
      text <- key "newText" e >>= asText
      pure (TextEdit s t text)
    pos p = (,) <$> (key "line" p >>= asInt) <*> (key "character" p >>= asInt)

data CompletionItem = CompletionItem
  { ciLabel :: !Text
  , ciDetail :: !Text
  , ciInsert :: !Text
  -- ^ The text to insert (snippets reduced to plain text).
  , ciReplace :: !(Maybe ((Int, Int), (Int, Int)))
  -- ^ The range a text edit replaces, in the server's units.
  , ciFilter :: !Text
  , ciSort :: !Text
  , ciAdditional :: ![TextEdit]
  -- ^ Edits elsewhere, made with the insertion (e.g. an import).
  , ciRaw :: !Value
  -- ^ As the server sent it, for @completionItem/resolve@.
  }
  deriving stock (Eq, Show)

-- | A completion result: a list, or a @CompletionList@.
parseCompletion :: Value -> [CompletionItem]
parseCompletion v = mapMaybe item (fromMaybe [] (asArray v <|> (key "items" v >>= asArray)))
  where
    item i = do
      label <- key "label" i >>= asText
      let snippet = (key "insertTextFormat" i >>= asInt) == Just 2
          plain t = if snippet then stripSnippet t else t
          edit = key "textEdit" i
          newText = edit >>= key "newText" >>= asText
          replace = edit >>= \e -> (key "range" e <|> key "replace" e) >>= range
          insert = fromMaybe label (newText <|> (key "insertText" i >>= asText))
      pure
        CompletionItem
          { ciLabel = label
          , ciDetail = fromMaybe "" (key "detail" i >>= asText)
          , ciInsert = plain insert
          , ciReplace = replace
          , ciFilter = fromMaybe label (key "filterText" i >>= asText)
          , ciSort = fromMaybe label (key "sortText" i >>= asText)
          , ciAdditional = maybe [] parseTextEdits (key "additionalTextEdits" i)
          , ciRaw = i
          }

-- | A snippet as the text it shows: @${1:x}@ becomes @x@, @$1@ and @$0@
-- disappear, @\\$@ is a dollar.
stripSnippet :: Text -> Text
stripSnippet = T.pack . go . T.unpack
  where
    go = \case
      '\\' : c : rest -> c : go rest
      '$' : '{' : rest -> let (inner, rest') = braced 0 rest in go (dropPlaceholder inner) <> go rest'
      '$' : rest | (ds@(_ : _), rest') <- span (`elem` ['0' .. '9']) rest, not (null ds) -> go rest'
      c : rest -> c : go rest
      [] -> []
    -- The text up to the matching close brace.
    braced :: Int -> String -> (String, String)
    braced depth = \case
      '}' : rest | depth == 0 -> ([], rest)
      c : rest ->
        let depth' = case c of
              '{' -> depth + 1
              '}' -> depth - 1
              _ -> depth
            (inner, rest') = braced depth' rest
         in (c : inner, rest')
      [] -> ([], [])
    -- "1:default" -> "default"; "1|a,b|" -> "a"; "1" -> "".
    dropPlaceholder s = case span (`elem` ['0' .. '9']) s of
      (_, ':' : def) -> def
      (_, '|' : choices) -> takeWhile (\c -> c /= ',' && c /= '|') choices
      _ -> ""
