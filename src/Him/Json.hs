-- | JSON: values, a parser and an encoder (the LSP client, the chat, the
-- config file; ADR effects-and-runtime). There is no JSON library among GHC's boot libraries,
-- so this is a small one: strict, UTF-8, RFC 8259.
module Him.Json
  ( Value (..)
  , parseJson
  , encodeJson
  , renderJson
    -- * Building and reading
  , object
  , key
  , path
  , asText
  , asInt
  , asBool
  , asArray
  , asObject
  ) where

import Data.Bits (shiftL, (.&.), (.|.))
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.ByteString.Builder (Builder)
import Data.ByteString.Builder qualified as B
import Data.ByteString.Lazy qualified as BL
import Data.Char (chr, ord)
import Data.Foldable (foldlM)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding (decodeUtf8Lenient, encodeUtf8Builder)
import Data.Word (Word8)

data Value
  = JNull
  | JBool !Bool
  | -- | A number without fraction or exponent; kept exact (ids, positions).
    JInt !Integer
  | JDouble !Double
  | JString !Text
  | JArray ![Value]
  | -- | Members in their order; lookups take the first match.
    JObject ![(Text, Value)]
  deriving stock (Eq, Show)

-- * Parsing

-- | Parse one JSON value (surrounding whitespace allowed).
parseJson :: ByteString -> Either Text Value
parseJson bs = do
  (v, i) <- value bs (skipWs bs 0)
  let j = skipWs bs i
  if j == BS.length bs then Right v else Left (failure j "trailing input")

failure :: Int -> Text -> Text
failure i msg = "JSON: " <> msg <> " at byte " <> T.pack (show i)

at :: ByteString -> Int -> Maybe Word8
at bs i = if i < BS.length bs then Just (BS.index bs i) else Nothing

skipWs :: ByteString -> Int -> Int
skipWs bs i = case at bs i of
  Just c | c == 32 || c == 9 || c == 10 || c == 13 -> skipWs bs (i + 1)
  _ -> i

value :: ByteString -> Int -> Either Text (Value, Int)
value bs i = case at bs i of
  Just 123 -> objectAt bs (skipWs bs (i + 1))
  Just 91 -> arrayAt bs (skipWs bs (i + 1))
  Just 34 -> (\(t, j) -> (JString t, j)) <$> stringAt bs (i + 1)
  Just 116 -> literal "true" (JBool True)
  Just 102 -> literal "false" (JBool False)
  Just 110 -> literal "null" JNull
  Just c | c == 45 || (c >= 48 && c <= 57) -> numberAt bs i
  Just _ -> Left (failure i "unexpected character")
  Nothing -> Left (failure i "unexpected end")
  where
    literal word v
      | word `BS.isPrefixOf` BS.drop i bs = Right (v, i + BS.length word)
      | otherwise = Left (failure i "bad literal")

objectAt :: ByteString -> Int -> Either Text (Value, Int)
objectAt bs i0 = case at bs i0 of
  Just 125 -> Right (JObject [], i0 + 1)
  _ -> go [] i0
  where
    go acc i = case at bs i of
      Just 34 -> do
        (k, j) <- stringAt bs (i + 1)
        let j' = skipWs bs j
        case at bs j' of
          Just 58 -> do
            (v, l) <- value bs (skipWs bs (j' + 1))
            let l' = skipWs bs l
                acc' = (k, v) : acc
            case at bs l' of
              Just 44 -> go acc' (skipWs bs (l' + 1))
              Just 125 -> Right (JObject (reverse acc'), l' + 1)
              _ -> Left (failure l' "expected , or }")
          _ -> Left (failure j' "expected :")
      _ -> Left (failure i "expected a string key")

arrayAt :: ByteString -> Int -> Either Text (Value, Int)
arrayAt bs i0 = case at bs i0 of
  Just 93 -> Right (JArray [], i0 + 1)
  _ -> go [] i0
  where
    go acc i = do
      (v, j) <- value bs i
      let j' = skipWs bs j
      case at bs j' of
        Just 44 -> go (v : acc) (skipWs bs (j' + 1))
        Just 93 -> Right (JArray (reverse (v : acc)), j' + 1)
        _ -> Left (failure j' "expected , or ]")

numberAt :: ByteString -> Int -> Either Text (Value, Int)
numberAt bs i =
  let len = BS.length (BS.takeWhile isNumByte (BS.drop i bs))
      raw = BS.take len (BS.drop i bs)
      str = map (chr . fromIntegral) (BS.unpack raw)
      isFrac = BS.any (\c -> c == 46 || c == 101 || c == 69) raw
   in if isFrac
        then case reads str of
          [(d, "")] -> Right (JDouble d, i + len)
          _ -> Left (failure i "bad number")
        else case reads str of
          [(n, "")] -> Right (JInt n, i + len)
          _ -> Left (failure i "bad number")
  where
    isNumByte c = (c >= 48 && c <= 57) || c == 45 || c == 43 || c == 46 || c == 101 || c == 69

-- | A string body (after the opening quote). Runs without escapes are
-- decoded as one slice.
stringAt :: ByteString -> Int -> Either Text (Text, Int)
stringAt bs start = go [] start start
  where
    slice a b = decodeUtf8Lenient (BS.take (b - a) (BS.drop a bs))
    go acc runStart i = case at bs i of
      Nothing -> Left (failure i "unterminated string")
      Just 34 -> Right (T.concat (reverse (slice runStart i : acc)), i + 1)
      Just 92 -> case at bs (i + 1) of
        Just e | Just c <- lookup e simpleEscapes -> go (T.singleton c : slice runStart i : acc) (i + 2) (i + 2)
        Just 117 -> do
          (c, j) <- unicodeEscape (i + 2)
          go (T.singleton c : slice runStart i : acc) j j
        _ -> Left (failure i "bad escape")
      Just c
        | c < 32 -> Left (failure i "control character in string")
        | otherwise -> go acc runStart (i + 1)
    simpleEscapes = [(34, '"'), (92, '\\'), (47, '/'), (98, '\b'), (102, '\f'), (110, '\n'), (114, '\r'), (116, '\t')]
    hex4 j = do
      digits <- traverse (\k -> maybe (Left (failure k "bad \\u escape")) Right (at bs k >>= hexDigit)) [j .. j + 3]
      foldlM (\a d -> Right (a * 16 + d)) 0 digits
    -- A surrogate pair is joined; a lone surrogate becomes U+FFFD.
    unicodeEscape j = do
      hi <- hex4 j
      if hi >= 0xD800 && hi <= 0xDBFF && at bs (j + 4) == Just 92 && at bs (j + 5) == Just 117
        then do
          lo <- hex4 (j + 6)
          if lo >= 0xDC00 && lo <= 0xDFFF
            then Right (chr (0x10000 + ((hi - 0xD800) `shiftL` 10 .|. (lo - 0xDC00))), j + 10)
            else Right ('\xFFFD', j + 4)
        else Right (if hi >= 0xD800 && hi <= 0xDFFF then '\xFFFD' else chr hi, j + 4)

hexDigit :: Word8 -> Maybe Int
hexDigit c
  | c >= 48 && c <= 57 = Just (fromIntegral c - 48)
  | c >= 97 && c <= 102 = Just (fromIntegral c - 87)
  | c >= 65 && c <= 70 = Just (fromIntegral c - 55)
  | otherwise = Nothing

-- * Encoding

encodeJson :: Value -> Builder
encodeJson = \case
  JNull -> "null"
  JBool b -> if b then "true" else "false"
  JInt n -> B.integerDec n
  JDouble d
    | isNaN d || isInfinite d -> "null"
    | otherwise -> B.doubleDec d
  JString t -> encodeString t
  JArray vs -> "[" <> commaSep (map encodeJson vs) <> "]"
  JObject kvs -> "{" <> commaSep [encodeString k <> ":" <> encodeJson v | (k, v) <- kvs] <> "}"
  where
    commaSep = \case
      [] -> mempty
      b : bs -> b <> foldMap ("," <>) bs

encodeString :: Text -> Builder
encodeString t = "\"" <> foldMap piece (chunks t) <> "\""
  where
    piece (Left c) = escape c
    piece (Right run) = encodeUtf8Builder run

-- | Runs that need no escaping, and the characters between them.
chunks :: Text -> [Either Char Text]
chunks t
  | T.null t = []
  | otherwise =
      let (run, rest) = T.break needsEscape t
       in [Right run | not (T.null run)] <> case T.uncons rest of
            Just (c, rest') -> Left c : chunks rest'
            Nothing -> []
  where
    needsEscape c = c == '"' || c == '\\' || c < ' '

escape :: Char -> Builder
escape = \case
  '"' -> "\\\""
  '\\' -> "\\\\"
  '\n' -> "\\n"
  '\r' -> "\\r"
  '\t' -> "\\t"
  c -> "\\u00" <> B.word8HexFixed (fromIntegral (ord c) .&. 0xFF)

renderJson :: Value -> ByteString
renderJson = BL.toStrict . B.toLazyByteString . encodeJson

-- * Building and reading

object :: [(Text, Value)] -> Value
object = JObject

-- | A member of an object.
key :: Text -> Value -> Maybe Value
key k = \case
  JObject kvs -> lookup k kvs
  _ -> Nothing

-- | Nested members: @path ["result", "contents"]@.
path :: [Text] -> Value -> Maybe Value
path ks v = foldlM (flip key) v ks

asText :: Value -> Maybe Text
asText = \case
  JString t -> Just t
  _ -> Nothing

asInt :: Value -> Maybe Int
asInt = \case
  JInt n -> Just (fromInteger n)
  JDouble d | d == fromIntegral (round d :: Integer) -> Just (round d)
  _ -> Nothing

asBool :: Value -> Maybe Bool
asBool = \case
  JBool b -> Just b
  _ -> Nothing

asArray :: Value -> Maybe [Value]
asArray = \case
  JArray vs -> Just vs
  _ -> Nothing

asObject :: Value -> Maybe [(Text, Value)]
asObject = \case
  JObject kvs -> Just kvs
  _ -> Nothing
