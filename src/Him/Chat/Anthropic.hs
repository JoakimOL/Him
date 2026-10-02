-- | The chat provider for the Claude API (ADR-41). There is no Anthropic
-- SDK for Haskell and only boot libraries are allowed, so the request is
-- raw HTTP through @curl@: the request (headers with the key, and the
-- body) goes to curl's standard input as a config file, so the key never
-- shows in the process list and no temporary file holds it. The reply
-- streams (server-sent events) and is parsed here ('streamStep', pure).
--
-- Credentials, first found: @$ANTHROPIC_API_KEY@ (@x-api-key@),
-- @$ANTHROPIC_AUTH_TOKEN@, or the token @ant auth print-credentials@ gives
-- (both @Authorization: Bearer@ with the OAuth beta header).
module Him.Chat.Anthropic
  ( anthropicProvider
  , requestBody
  , StreamState
  , newStream
  , streamStep
  , streamEnd
  ) where

import Control.Concurrent (forkIO)
import Control.Exception (IOException, SomeException, try)
import Control.Monad (void)
import Data.ByteString qualified as BS
import Data.ByteString.Char8 qualified as BC
import Data.IntMap.Strict (IntMap)
import Data.IntMap.Strict qualified as IntMap
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding (decodeUtf8Lenient, encodeUtf8)
import Him.Chat
import Him.Json
import System.Environment (lookupEnv)
import System.Exit (ExitCode (..))
import System.IO (hClose, hSetBinaryMode)
import System.Process (CreateProcess (..), StdStream (..), createProcess, proc, readProcessWithExitCode, terminateProcess, waitForProcess)

-- | Stateless: every turn sends the whole history, and tool calls come with
-- the finished reply.
anthropicProvider :: ChatProvider
anthropicProvider = ChatProvider "anthropic" (pure (ChatSession send (\_ _ _ -> pure ()) (pure ())))

-- | The request body: streaming, adaptive thinking at the configured
-- effort, the server-side fallback for refusals, and the tools with
-- @eager_input_streaming@ (their input is checked here before use).
requestBody :: ChatConfig -> ChatRequest -> Value
requestBody cc req =
  object
    [ ("model", JString (ccModel cc))
    , ("max_tokens", JInt (toInteger (ccMaxTokens cc)))
    , ("stream", JBool True)
    , ("system", JString (crSystem req))
    , ("messages", JArray (crMessages req))
    , ("tools", JArray (map eager (crTools req)))
    , ("thinking", object [("type", JString "adaptive")])
    , ("output_config", object [("effort", JString (ccEffort cc))])
    , ("fallbacks", JString "default")
    ]
  where
    eager = \case
      JObject kvs -> JObject (kvs <> [("eager_input_streaming", JBool True)])
      v -> v

send :: ChatConfig -> ChatRequest -> (ChatEvent -> IO ()) -> IO (IO ())
send cc req emit =
  credentials >>= \case
    Nothing -> do
      emit (ChatFailed "no credentials: set ANTHROPIC_API_KEY (or run `ant auth login`)")
      pure (pure ())
    Just auth -> do
      base <- fromMaybe "https://api.anthropic.com" <$> lookupEnv "ANTHROPIC_BASE_URL"
      let config =
            T.unlines $
              [ "url = " <> quote (T.pack base <> "/v1/messages")
              , "silent"
              , "show-error"
              , "no-buffer"
              , "header = \"content-type: application/json\""
              , "header = \"anthropic-version: 2023-06-01\""
              , "header = " <> quote ("anthropic-beta: " <> T.intercalate "," ("server-side-fallback-2026-07-01" : snd auth))
              , "header = " <> quote (fst auth)
              , "data-binary = " <> quote (decodeUtf8Lenient (renderJson (requestBody cc req)))
              ]
      started <- try @IOException (createProcess (proc "curl" ["--config", "-"]) {std_in = CreatePipe, std_out = CreatePipe, std_err = CreatePipe})
      case started of
        Left e -> do
          emit (ChatFailed ("curl: " <> T.pack (show e)))
          pure (pure ())
        Right (Just hin, Just hout, Just herr, ph) -> do
          mapM_ (`hSetBinaryMode` True) [hin, hout, herr]
          BS.hPut hin (encodeUtf8 config) >> hClose hin
          _ <- forkIO $ do
            let loop st =
                  try @IOException (BC.hGetLine hout) >>= \case
                    Right line -> do
                      let (events, st') = streamStep (decodeUtf8Lenient line) st
                      mapM_ emit events
                      loop st'
                    Left _ -> pure st
            st <- loop newStream
            errText <- either (const "") decodeUtf8Lenient <$> try @IOException (BS.hGetContents herr)
            code <- waitForProcess ph
            emit $ case (streamEnd st, code) of
              (Just done, _) -> done
              (Nothing, ExitSuccess) -> ChatFailed "the reply ended early"
              (Nothing, ExitFailure n) -> ChatFailed ("curl exited with " <> T.pack (show n) <> ": " <> T.strip errText)
          pure (void (try @SomeException (terminateProcess ph)))
        Right _ -> pure (pure ())
  where
    -- A curl config string: quotes and backslashes escaped (the JSON body
    -- has no raw line breaks).
    quote t = "\"" <> T.concatMap (\c -> if c == '"' || c == '\\' then T.pack ['\\', c] else T.singleton c) t <> "\""

-- | The auth header, and extra beta flags it needs.
credentials :: IO (Maybe (Text, [Text]))
credentials =
  lookupEnv "ANTHROPIC_API_KEY" >>= \case
    Just k | not (null k) -> pure (Just ("x-api-key: " <> T.pack k, []))
    _ ->
      lookupEnv "ANTHROPIC_AUTH_TOKEN" >>= \case
        Just t | not (null t) -> pure (Just (bearer (T.pack t)))
        _ ->
          try @IOException (readProcessWithExitCode "ant" ["auth", "print-credentials", "--access-token"] "") >>= \case
            Right (ExitSuccess, out, _) | not (null (words out)) -> pure (Just (bearer (T.strip (T.pack out))))
            _ -> pure Nothing
  where
    bearer t = ("authorization: Bearer " <> t, ["oauth-2025-04-20"])

-- | The reply being read: each content block as it grows (by index), the
-- stop reason, and an error the server sent.
data StreamState = StreamState
  { ssBlocks :: !(IntMap Block)
  , ssStop :: !(Maybe Text)
  , ssDone :: !Bool
  , ssError :: !(Maybe Text)
  , ssBody :: ![Text]
  -- ^ Lines that were not events (an error reply that did not stream).
  }

-- | A block: what @content_block_start@ gave, plus the deltas.
data Block = Block
  { bBase :: !Value
  , bText :: !Text
  , bThinking :: !Text
  , bSignature :: !(Maybe Text)
  , bJson :: !Text
  }

newStream :: StreamState
newStream = StreamState IntMap.empty Nothing False Nothing []

-- | One line of the event stream: the events to show now, and the state.
streamStep :: Text -> StreamState -> ([ChatEvent], StreamState)
streamStep line st = case T.stripPrefix "data:" (T.stripEnd line) of
  Nothing
    | "event:" `T.isPrefixOf` line || T.null (T.strip line) -> ([], st)
    | otherwise -> ([], st {ssBody = ssBody st <> [line]})
  Just payload -> case parseJson (encodeUtf8 (T.strip payload)) of
    Left _ -> ([], st)
    Right v -> event v
  where
    event v = case key "type" v >>= asText of
      Just "content_block_start"
        | Just i <- index v, Just b <- key "content_block" v ->
            ([], st {ssBlocks = IntMap.insert i (Block b "" "" Nothing "") (ssBlocks st)})
      Just "content_block_delta"
        | Just i <- index v, Just d <- key "delta" v -> delta i d
      Just "message_delta" -> ([], st {ssStop = (path ["delta", "stop_reason"] v >>= asText) <> ssStop st})
      Just "message_stop" -> ([], st {ssDone = True})
      Just "error" -> ([], st {ssError = Just (fromMaybe "error" (path ["error", "message"] v >>= asText))})
      _ -> ([], st)
    index v = key "index" v >>= asInt
    delta i d = case key "type" d >>= asText of
      Just "text_delta" | Just t <- key "text" d >>= asText -> ([ChatText t], update i (\b -> b {bText = bText b <> t}))
      Just "thinking_delta" | Just t <- key "thinking" d >>= asText -> ([], update i (\b -> b {bThinking = bThinking b <> t}))
      Just "signature_delta" | Just t <- key "signature" d >>= asText -> ([], update i (\b -> b {bSignature = Just t}))
      Just "input_json_delta" | Just t <- key "partial_json" d >>= asText -> ([], update i (\b -> b {bJson = bJson b <> t}))
      _ -> ([], st)
    update i f = st {ssBlocks = IntMap.adjust f i (ssBlocks st)}

-- | The final event once the stream ended: the assistant message and its
-- tool calls, or the failure. 'Nothing' if nothing conclusive arrived.
streamEnd :: StreamState -> Maybe ChatEvent
streamEnd st
  | Just e <- ssError st = Just (ChatFailed e)
  | ssDone st =
      let blocks = map finish (IntMap.elems (ssBlocks st))
          message = object [("role", JString "assistant"), ("content", JArray (map fst blocks))]
       in Just (ChatFinished (fromMaybe "end_turn" (ssStop st)) message [c | (_, Just c) <- blocks])
  | not (null (ssBody st)) =
      let body = T.unlines (ssBody st)
       in Just (ChatFailed (fromMaybe (T.strip body) (either (const Nothing) (\v -> path ["error", "message"] v >>= asText) (parseJson (encodeUtf8 body)))))
  | otherwise = Nothing
  where
    finish b = case key "type" (bBase b) >>= asText of
      Just "text" -> (set "text" (JString (bText b)) (bBase b), Nothing)
      Just "thinking" -> (maybe id (set "signature" . JString) (bSignature b) (set "thinking" (JString (bThinking b)) (bBase b)), Nothing)
      Just "tool_use" ->
        let raw = if T.null (bJson b) then "{}" else bJson b
            parsed = case parseJson (encodeUtf8 raw) of
              Right v@(JObject _) -> Right v
              _ -> Left raw
            -- The history needs an object; an unparseable input becomes {}
            -- (its tool result says why).
            echoed = either (const (JObject [])) id parsed
            callId = fromMaybe "" (key "id" (bBase b) >>= asText)
            name = fromMaybe "" (key "name" (bBase b) >>= asText)
         in (set "input" echoed (bBase b), Just (ToolCall callId name parsed))
      _ -> (bBase b, Nothing)
    set k v = \case
      JObject kvs -> JObject ([(k', v') | (k', v') <- kvs, k' /= k] <> [(k, v)])
      other -> other
