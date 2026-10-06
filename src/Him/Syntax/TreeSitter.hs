-- | The tree-sitter syntax provider (ADR tree-sitter). It exports one value,
-- 'treeSitter', of the common provider type ("Him.Syntax"); nothing else
-- in the editor knows tree-sitter is involved.
--
-- The runtime is vendored C (@cbits/tree-sitter@). Grammars are compiled
-- shared objects loaded at run time, with their @highlights.scm@ queries,
-- from a runtime directory laid out like Helix's (@grammars/NAME.so@,
-- @queries/LANG/highlights.scm@): @$HIM_RUNTIME@, @~/.config/him/runtime@,
-- @~/.config/helix/runtime@, @/usr/lib/helix/runtime@ or
-- @/usr/share/helix/runtime@.
module Him.Syntax.TreeSitter
  ( treeSitter
    -- * For tests
  , findRuntime
  , readQuery
  ) where

import Control.Exception (IOException, SomeException, try)
import Him.Paths (runtimeDirs)
import Control.Monad (forM, when)
import Data.Array.Unboxed (UArray, bounds, listArray, (!))
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.ByteString.Unsafe (unsafeUseAsCStringLen)
import Data.IORef (newIORef, readIORef, writeIORef)
import Data.IntMap.Strict (IntMap)
import Data.IntMap.Strict qualified as IntMap
import Data.List (groupBy, nub, sortOn)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding (decodeUtf8Lenient, encodeUtf8)
import Data.Text.IO qualified as TIO
import Data.Text.Unsafe (takeWord8)
import Data.Word (Word32)
import Foreign.C.String (CString, peekCStringLen)
import Foreign.C.Types (CBool (..), CInt (..))
import Foreign.Marshal.Alloc (alloca)
import Foreign.Marshal.Array (allocaArray, peekArray)
import Foreign.Ptr (FunPtr, Ptr, nullPtr)
import Foreign.Storable (peek, peekByteOff)
import GHC.Int (Int32)
import Him.Buffer (Buffer)
import Him.Buffer qualified as Buffer
import Him.Language (Language (..), grammarFor)
import Him.Log (logMsg)
import Him.Regex (Regex, compileLua, compileRegex, matchesRegex)
import Him.Syntax
import System.Directory (doesFileExist, getHomeDirectory)
import System.Environment (lookupEnv)
import System.FilePath ((</>))
import System.Posix.DynamicLinker (RTLDFlags (..), dlopen, dlsym)

data TSParser
data TSTree
data TSQuery
data TSQueryCursor
data TSLanguage

foreign import ccall unsafe "ts_parser_new" c_parser_new :: IO (Ptr TSParser)
foreign import ccall unsafe "ts_parser_delete" c_parser_delete :: Ptr TSParser -> IO ()
foreign import ccall unsafe "ts_parser_set_language" c_parser_set_language :: Ptr TSParser -> Ptr TSLanguage -> IO CBool
foreign import ccall safe "him_ts_parse" c_parse :: Ptr TSParser -> CString -> Word32 -> IO (Ptr TSTree)
foreign import ccall unsafe "ts_tree_delete" c_tree_delete :: Ptr TSTree -> IO ()
foreign import ccall unsafe "ts_language_abi_version" c_abi_version :: Ptr TSLanguage -> IO Word32
foreign import ccall safe "ts_query_new" c_query_new :: Ptr TSLanguage -> CString -> Word32 -> Ptr Word32 -> Ptr CInt -> IO (Ptr TSQuery)
foreign import ccall unsafe "ts_query_pattern_count" c_pattern_count :: Ptr TSQuery -> IO Word32
foreign import ccall unsafe "ts_query_capture_count" c_capture_count :: Ptr TSQuery -> IO Word32
foreign import ccall unsafe "ts_query_capture_name_for_id" c_capture_name :: Ptr TSQuery -> Word32 -> Ptr Word32 -> IO CString
foreign import ccall unsafe "ts_query_string_value_for_id" c_string_value :: Ptr TSQuery -> Word32 -> Ptr Word32 -> IO CString
foreign import ccall unsafe "ts_query_predicates_for_pattern" c_predicates :: Ptr TSQuery -> Word32 -> Ptr Word32 -> IO (Ptr ())
foreign import ccall unsafe "ts_query_cursor_new" c_cursor_new :: IO (Ptr TSQueryCursor)
foreign import ccall unsafe "ts_query_cursor_delete" c_cursor_delete :: Ptr TSQueryCursor -> IO ()
foreign import ccall safe "him_ts_query" c_query :: Ptr TSQueryCursor -> Ptr TSQuery -> Ptr TSTree -> Word32 -> Word32 -> Ptr Word32 -> Word32 -> IO Int32
foreign import ccall "dynamic" callLanguage :: FunPtr (IO (Ptr TSLanguage)) -> IO (Ptr TSLanguage)

-- | The grammar ABI versions this runtime reads (tree-sitter 0.26).
abiRange :: (Word32, Word32)
abiRange = (13, 15)

treeSitter :: SyntaxProvider
treeSitter = SyntaxProvider "tree-sitter" start

-- | A loaded grammar with its compiled highlight query.
data Grammar = Grammar
  { gLanguage :: !(Ptr TSLanguage)
  , gQuery :: !(Ptr TSQuery)
  , gCaptures :: !(IntMap Text)
  -- ^ Capture names by id.
  , gPredicates :: !(IntMap [Predicate])
  -- ^ By pattern index.
  }

data Predicate
  = Equal !Bool !Int !(Either Int Text)
  -- ^ Negated?, capture, another capture or a string.
  | Match !Bool !Int !Regex
  | AnyOf !Bool !Int ![Text]
  | -- | A predicate this provider does not evaluate: the match is dropped.
    Unsupported
  deriving stock (Show)

start :: Language -> IO (Maybe SyntaxSession)
start language = case grammarFor "tree-sitter" language of
  Nothing -> pure Nothing
  Just name -> do
    found <- findRuntime name
    case found of
      Nothing -> Nothing <$ logMsg ("tree-sitter: no grammar " <> T.unpack name)
      Just runtime ->
        loadGrammar runtime name (langName language) >>= \case
          Left e -> Nothing <$ logMsg ("tree-sitter: " <> T.unpack e)
          Right grammar -> Just <$> newSession grammar

-- | The directory with the grammar's shared object. Only grammars built
-- for him are loaded (@$HIM_RUNTIME@ or @~/.config/him/runtime@, filled by
-- @him --build-grammars@): prebuilt ones from other editors may be
-- miscompiled (see "Him.GrammarBuild").
findRuntime :: Text -> IO (Maybe FilePath)
findRuntime name = do
  dirs <- runtimeDirs
  env <- lookupEnv "HIM_RUNTIME"
  home <- either (const "") id <$> try @IOException getHomeDirectory
  let own = maybe [] pure env <> [home </> ".config/him/runtime"]
  firstWith [d | d <- dirs, d `elem` own] ("grammars" </> T.unpack name <> ".so")

firstWith :: [FilePath] -> FilePath -> IO (Maybe FilePath)
firstWith [] _ = pure Nothing
firstWith (d : ds) file = do
  ok <- doesFileExist (d </> file)
  if ok then pure (Just d) else firstWith ds file

loadGrammar :: FilePath -> Text -> Text -> IO (Either Text Grammar)
loadGrammar runtime name langDir = do
  loaded <- try @SomeException $ do
    dl <- dlopen (runtime </> "grammars" </> T.unpack name <> ".so") [RTLD_NOW, RTLD_LOCAL]
    sym <- dlsym dl ("tree_sitter_" <> map (\c -> if c == '-' then '_' else c) (T.unpack name))
    callLanguage sym
  case loaded of
    Left e -> pure (Left ("cannot load grammar " <> name <> ": " <> T.pack (show e)))
    Right lang -> do
      abi <- c_abi_version lang
      if abi < fst abiRange || abi > snd abiRange
        then pure (Left ("grammar " <> name <> " has ABI " <> T.pack (show abi)))
        else do
          -- Queries may come from any runtime directory (Helix's included);
          -- the language's own first, then the grammar's.
          dirs <- runtimeDirs
          let search = nub (runtime : dirs)
              queryIn n = firstWith search ("queries" </> T.unpack n </> "highlights.scm") >>= \case
                Just d -> readQuery (d </> "queries") n
                Nothing -> pure Nothing
          src' <- queryIn langDir >>= maybe (queryIn name) (pure . Just)
          case src' of
            Nothing -> pure (Left ("no highlights.scm for " <> langDir))
            Just source -> compileQuery lang source

-- | A language's @highlights.scm@, with @; inherits: a,b@ lines replaced by
-- those languages' queries (as Helix does).
readQuery :: FilePath -> Text -> IO (Maybe Text)
readQuery dir = go []
  where
    go seen lang
      | lang `elem` seen = pure (Just "")
      | otherwise =
          try @IOException (TIO.readFile (dir </> T.unpack lang </> "highlights.scm")) >>= \case
            Left _ -> pure Nothing
            Right t -> Just . T.unlines <$> mapM (expand (lang : seen)) (T.lines t)
    expand seen line = case T.stripPrefix "; inherits:" (T.strip line) of
      Just names -> T.unlines . map (fromMaybe "") <$> mapM (go seen . T.strip) (T.splitOn "," names)
      Nothing -> pure line

compileQuery :: Ptr TSLanguage -> Text -> IO (Either Text Grammar)
compileQuery lang source = do
  let bytes = encodeUtf8 source
  query <- unsafeUseAsCStringLen bytes $ \(ptr, len) ->
    alloca $ \offPtr -> alloca $ \typePtr -> do
      q <- c_query_new lang ptr (fromIntegral len) offPtr typePtr
      if q == nullPtr
        then do
          off <- peek offPtr
          kind <- peek typePtr
          pure (Left ("query error " <> T.pack (show kind) <> " at byte " <> T.pack (show off)))
        else pure (Right q)
  case query of
    Left e -> pure (Left e)
    Right q -> do
      -- Counts are unsigned: an empty range must not wrap around.
      captures <- fromIntegral <$> c_capture_count q
      names <- forM (takeWhile (< captures) [0 :: Int ..]) $ \i -> (i,) <$> withLength (c_capture_name q (fromIntegral i))
      patterns <- fromIntegral <$> c_pattern_count q
      preds <- forM (takeWhile (< patterns) [0 :: Int ..]) $ \p -> (p,) <$> predicatesOf q (fromIntegral p)
      pure (Right (Grammar lang q (IntMap.fromList names) (IntMap.fromList preds)))

-- | A C string with its length written through a pointer.
withLength :: (Ptr Word32 -> IO CString) -> IO Text
withLength f = alloca $ \lenPtr -> do
  s <- f lenPtr
  len <- peek lenPtr
  T.pack <$> peekCStringLen (s, fromIntegral len)

-- | A pattern's predicates. Steps are (type, value id) pairs of 32-bit
-- numbers: type 0 ends a predicate, 1 is a capture, 2 is a string.
predicatesOf :: Ptr TSQuery -> Word32 -> IO [Predicate]
predicatesOf q pattern = alloca $ \countPtr -> do
  steps <- c_predicates q pattern countPtr
  count <- peek countPtr
  raw <- forM (takeWhile (< fromIntegral count) [0 :: Int ..]) $ \i -> do
    kind <- peekByteOff steps (i * 8) :: IO Word32
    value <- peekByteOff steps (i * 8 + 4) :: IO Word32
    pure (kind, value)
  args <- forM raw $ \(kind, value) -> case kind of
    1 -> pure (Just (Left (fromIntegral value)))
    2 -> Just . Right <$> withLength (c_string_value q value)
    _ -> pure Nothing
  pure (map toPredicate (splitDone args))
  where
    splitDone xs = case break (== Nothing) xs of
      ([], []) -> []
      (p, rest) -> [[a | Just a <- p] | not (null p)] <> splitDone (drop 1 rest)
    toPredicate = \case
      Right op : Left cap : rest -> case (op, rest) of
        ("eq?", [arg]) -> Equal False cap arg
        ("not-eq?", [arg]) -> Equal True cap arg
        ("match?", [Right re]) -> regex False cap compileRegex re
        ("not-match?", [Right re]) -> regex True cap compileRegex re
        ("lua-match?", [Right re]) -> regex False cap compileLua re
        ("not-lua-match?", [Right re]) -> regex True cap compileLua re
        ("any-of?", strs) -> AnyOf False cap [s | Right s <- strs]
        ("not-any-of?", strs) -> AnyOf True cap [s | Right s <- strs]
        _ -> ignoredOr op
      Right op : _ -> ignoredOr op
      _ -> Unsupported
    regex neg cap compile re = either (const Unsupported) (Match neg cap) (compile re)
    -- Directives (#set!, …) and Helix's local-variable checks (#is?,
    -- #is-not?) do not decide whether a capture is shown here.
    ignoredOr op
      | "!" `T.isSuffixOf` op || op `elem` ["is?", "is-not?"] = AnyOf True (-1) []
      | otherwise = Unsupported

-- | Per document: a parser, the current tree, and the text it was parsed
-- from (bytes, line start offsets, and the buffer for line texts).
newSession :: Grammar -> IO SyntaxSession
newSession grammar = do
  parser <- c_parser_new
  _ <- c_parser_set_language parser (gLanguage grammar)
  cursor <- c_cursor_new
  state <- newIORef Nothing
  let update version buffer _changes = do
        current <- readIORef state
        when (fmap (\(v, _, _, _, _) -> v) current /= Just version) $ do
          let bytes = encodeUtf8 (Buffer.toText buffer)
              starts = lineStarts bytes
          tree <- unsafeUseAsCStringLen bytes $ \(ptr, len) -> c_parse parser ptr (fromIntegral len)
          mapM_ (\(_, _, _, _, old) -> when (old /= nullPtr) (c_tree_delete old)) current
          writeIORef state (Just (version, bytes, starts, buffer, tree))
      highlight from to =
        readIORef state >>= \case
          Just (_, bytes, starts, buffer, tree) | tree /= nullPtr -> highlightLines grammar cursor tree bytes starts buffer from to
          _ -> pure IntMap.empty
      close = do
        readIORef state >>= mapM_ (\(_, _, _, _, tree) -> when (tree /= nullPtr) (c_tree_delete tree))
        c_cursor_delete cursor
        c_parser_delete parser
  pure (SyntaxSession update highlight close)

-- | Byte offset of each line's start.
lineStarts :: ByteString -> UArray Int Int
lineStarts bytes =
  let starts = 0 : map (+ 1) (BS.elemIndices 10 bytes)
   in listArray (0, length starts - 1) starts

data Capture = Capture
  { cMatch :: !Word32
  , cPattern :: !Int
  , cIndex :: !Int
  , cStart :: !Int
  , cEnd :: !Int
  , cStartRow :: !Int
  , cStartCol :: !Int
  , cEndRow :: !Int
  , cEndCol :: !Int
  }

highlightLines :: Grammar -> Ptr TSQueryCursor -> Ptr TSTree -> ByteString -> UArray Int Int -> Buffer -> Int -> Int -> IO (IntMap [LineSpan])
highlightLines grammar cursor tree bytes starts buffer from to = do
  captures <- runQuery 4096
  let kept = concat [cs | cs@(c : _) <- groupBy sameMatch captures, all (holds cs) (IntMap.findWithDefault [] (cPattern c) (gPredicates grammar))]
      named = [(c, name) | c <- kept, Just name <- [IntMap.lookup (cIndex c) (gCaptures grammar)], shown name]
      -- Inner (shorter) nodes win over the nodes around them; on the same
      -- node, the last matching pattern wins. That is Helix's rule ("the
      -- last matching pattern (and the innermost node) wins"), which its
      -- queries are written for: general fallbacks such as
      -- (identifier) @variable come first.
      ordered = sortOn (\(c, _) -> (cEnd c - cStart c, negate (cPattern c))) named
      pieces = IntMap.fromListWith (flip (<>)) [(row, [sp]) | (c, name) <- ordered, (row, sp) <- perLine c name, row >= from', row <= to']
  pure (IntMap.union (IntMap.map flatten pieces) (IntMap.fromList [(l, []) | l <- [from' .. to']]))
  where
    (_, lastLine) = bounds starts
    from' = max 0 (min from lastLine)
    to' = max from' (min to lastLine)
    startByte = starts ! from'
    endByte = if to' >= lastLine then BS.length bytes else starts ! (to' + 1)
    -- The capture buffer grows until the window's captures fit.
    runQuery cap = do
      result <- allocaArray (cap * 9) $ \out -> do
        n <- c_query cursor (gQuery grammar) tree (fromIntegral startByte) (fromIntegral endByte) out (fromIntegral cap)
        if n < 0 then pure Nothing else Just <$> peekArray (fromIntegral n * 9) out
      case result of
        Nothing -> runQuery (cap * 4)
        Just ws -> pure (records ws)
    records (a : b : c : d : e : f : g : h : i : rest) =
      Capture a (fromIntegral b) (fromIntegral c) (fromIntegral d) (fromIntegral e) (fromIntegral f) (fromIntegral g) (fromIntegral h) (fromIntegral i) : records rest
    records _ = []
    sameMatch a b = cMatch a == cMatch b
    nodeText c = decodeUtf8Lenient (BS.take (cEnd c - cStart c) (BS.drop (cStart c) bytes))
    textOf cs idx = [nodeText c | c <- cs, cIndex c == idx]
    holds cs = \case
      Equal neg cap arg ->
        let left = textOf cs cap
            right = either (textOf cs) pure arg
         in (not (null left) && left == right) /= neg
      Match neg cap re -> all (\t -> matchesRegex re t /= neg) (textOf cs cap)
      AnyOf neg cap options
        | cap < 0 -> True
        | otherwise -> all (\t -> (t `elem` options) /= neg) (textOf cs cap)
      Unsupported -> False
    -- Captures that are not highlights.
    shown name = not ("_" `T.isPrefixOf` name) && name `notElem` ["spell", "nospell", "conceal", "none"] && not ("local." `T.isPrefixOf` name)
    perLine c name =
      [ (row, LineSpan s e name)
      | row <- [cStartRow c .. cEndRow c]
      , let line = Buffer.lineAt row buffer
            s = if row == cStartRow c then charCol line (cStartCol c) else 0
            e = if row == cEndRow c then charCol line (cEndCol c) else T.length line
      , s < e
      ]
    charCol line byteCol = T.length (takeWord8 byteCol line)
