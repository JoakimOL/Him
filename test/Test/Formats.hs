-- | Keys and key decoding, keymaps, actions, : commands, pickers, files, JSON, TOML, ignore files.
module Test.Formats
  ( keyTests
  , decodeTests
  , keymapTests
  , actionTests
  , exTests
  , pickerTests
  , fileTests
  , jsonTests
  , tomlTests
  , ignoreTests
  ) where

import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Him.Action
import Him.Buffer qualified as B
import Him.Config.Default (allActions, defaultConfig)
import Him.Document
import Him.Ex (parseExLine)
import Him.File (decodeChunks, decodeDocument, encodeDocument)
import Data.Text.Encoding qualified as TE
import Him.Picker
import Him.Ignore
import Him.Toml (parseToml)
import Him.Json hiding (path)
import Him.Json qualified as J
import Him.Key
import Him.Keymap
import Him.Position (Pos (..))
import Him.Terminal.Input (decodeKeys)
import Data.Text qualified as T
import Test.Harness
import Test.Util

keyTests :: [Test]
keyTests =
  [ test "parse plain char" (assertEqual (Just (plain (KChar 'a'))) (parseKey "a"))
  , test "parse ctrl" (assertEqual (Just (ctrl 's')) (parseKey "C-s"))
  , test "parse named" (assertEqual (Just (plain KEnter)) (parseKey "ret"))
  , test "parse F-key" (assertEqual (Just (plain (KF 5))) (parseKey "F5"))
  , test "parse sequence" (assertEqual (Just [plain (KChar 'g'), plain (KChar 'g')]) (parseKeys "g g"))
  , test "lone C is the letter" (assertEqual (Just (plain (KChar 'C'))) (parseKey "C"))
  , test "C-- is ctrl minus" (assertEqual (Just (ctrl '-')) (parseKey "C--"))
  , test "unknown name fails" (assertEqual Nothing (parseKey "nope"))
  , group "show/parse round trip" [roundTrip t | t <- ["a", "C-s", "A-S-left", "space", "ret", "F12", "C-A-x", "minus"]]
  ]
  where
    roundTrip :: Text -> Test
    roundTrip t = test (show t) (assertEqual (Just t) (showKey <$> parseKey t))

decodeTests :: [Test]
decodeTests =
  [ decodes "ascii" "ab" [ch 'a', ch 'b']
  , decodes "enter / tab / backspace" "\r\t\DEL" [plain KEnter, plain KTab, plain KBackspace]
  , decodes "ctrl letters" "\SOH\DC3" [ctrl 'a', ctrl 's']
  , decodes "arrows" "\ESC[A\ESC[B\ESC[C\ESC[D" [plain KUp, plain KDown, plain KRight, plain KLeft]
  , decodes "ss3 arrows" "\ESCOA" [plain KUp]
  , decodes "ctrl-right" "\ESC[1;5C" [withMod Ctrl (plain KRight)]
  , decodes "shift-alt-up" "\ESC[1;4A" [withMod Shift (withMod Alt (plain KUp))]
  , decodes "delete, pgup" "\ESC[3~\ESC[5~" [plain KDelete, plain KPageUp]
  , decodes "F5" "\ESC[15~" [plain (KF 5)]
  , decodes "shift-tab" "\ESC[Z" [withMod Shift (plain KTab)]
  , decodes "alt-x" "\ESCx" [alt 'x']
  , decodes "utf-8" "\195\166\226\130\172" [ch 'æ', ch '€']
  , decodes "unknown CSI is dropped" "\ESC[99xa" [ch 'a']
  , test "lone ESC waits for more" (assertEqual ([], "\ESC") (decodeKeys False "\ESC"))
  , test "lone ESC on timeout is Esc" (assertEqual ([plain KEsc], "") (decodeKeys True "\ESC"))
  , test "partial CSI waits" (assertEqual ([ch 'a'], "\ESC[1;") (decodeKeys False "a\ESC[1;"))
  , test "partial utf-8 waits" (assertEqual ([], "\226\130") (decodeKeys False "\226\130"))
  ]
  where
    ch = plain . KChar
    decodes :: String -> ByteString -> [Key] -> Test
    decodes name input expected = test name (assertEqual (expected, "") (decodeKeys False input))

keymapTests :: [Test]
keymapTests =
  [ test "single key" (assertEqual (Found "one") (resolve km (keys "a")))
  , test "prefix needs more" (assertEqual NeedMore (resolve km (keys "g")))
  , test "chord" (assertEqual (Found "top") (resolve km (keys "g g")))
  , test "unknown" (assertEqual NoMatch (resolve km (keys "z")))
  , test "unknown after prefix" (assertEqual NoMatch (resolve km (keys "g z")))
  , test "union overrides and merges prefixes" $
      let over = either (error . show) id (fromBindings [("g e", "end"), ("a", "other")])
          u = unionKeymap over km
       in assertEqual [Found "other", Found "top", Found "end"] (map (resolve u . keys) ["a", "g g", "g e"])
  , test "invalid key sequence is an error" (assertEqual (Left "invalid key sequence: nope") (() <$ fromBindings [("nope", "x" :: Text)]))
  , test "default config is valid" (assertEqual (Right ()) (() <$ defaultConfig))
  ]
  where
    km = either (error . show) id (fromBindings [("a", "one" :: Text), ("g g", "top")])
    keys = fromMaybe [] . parseKeys


actionTests :: [Test]
actionTests =
  [ test "a bare name" (assertEqual (Right (Invocation "undo" [])) (parseInvocation "undo"))
  , test "arguments" (assertEqual (Right (Invocation "goto_line" ["12"])) (parseInvocation "  goto_line   12 "))
  , test "quoted argument with escapes" $
      assertEqual (Right (Invocation "insert_text" ["a \"b\"\n\\"])) (parseInvocation "insert_text \"a \\\"b\\\"\\n\\\\\"")
  , test "empty quoted argument" (assertEqual (Right (Invocation "insert_text" [""])) (parseInvocation "insert_text \"\""))
  , test "unterminated string" (assertEqual (Left "unterminated string") (parseInvocation "insert_text \"abc"))
  , test "unknown escape" (assertEqual (Left "unknown escape \\q") (parseInvocation "insert_text \"\\q\""))
  , test "empty binding" (assertEqual (Left "empty action") (parseInvocation "  "))
  , test "invalid name" (assertEqual (Left "invalid action name: Undo") (parseInvocation "Undo"))
  , test "render round-trips" $
      let invs = [Invocation "insert_text" ["a b", "", "q\"\\\n\t"], Invocation "goto_line" ["3"], Invocation "undo" []]
       in assertEqual (map Right invs) (map (parseInvocation . renderInvocation) invs)
  , test "unknown action" (assertEqual (Left "unknown action: fly") (bindErr "fly"))
  , test "missing argument" (assertEqual (Left "goto_line: missing argument <line>") (bindErr "goto_line"))
  , test "not a number" (assertEqual (Left "goto_line: <line> must be a number, got x") (bindErr "goto_line x"))
  , test "too many arguments" (assertEqual (Left "undo: unexpected argument 1 \"a b\"") (bindErr "undo 1 \"a b\""))
  , test "optional argument may be left out" (assertEqual (Right ()) (bindErr "move_line_down"))
  , test "optional argument may be given" (assertEqual (Right ()) (bindErr "move_line_down -3"))
  , test "bad choice" $
      assertEqual (Left "set_mode: <mode> must be one of normal, insert, select, got command") (bindErr "set_mode command")
  , test "duplicate action names are rejected" $
      assertEqual (Left "duplicate action names: a") (() <$ mkActionRegistry [simple "a" GMisc "" (pure ()), simple "a" GMisc "" (pure ())])
  , test "parameters describe themselves" $
      assertEqual
        (Just [Param "count" PInt (Just "1")])
        (actParams <$> lookupAction "move_line_down" registry)
  , test "every action name is a valid binding target" $
      assertEqual [] [actName a | a <- allActions, (invAction <$> parseInvocation (actName a)) /= Right (actName a)]
  , test "registry lists actions by group" $
      let groups = map actGroup (registryActions registry)
       in assertEqual True (and (zipWith (<=) groups (drop 1 groups)))
  ]
  where
    registry = either (error . T.unpack) id (mkActionRegistry allActions)
    bindErr t = () <$ bindText registry t

exTests :: [Test]
exTests =
  [ test "name and args" (assertEqual (Just ("w", ["file.txt"])) (parseExLine "w file.txt"))
  , test "blank line" (assertEqual Nothing (parseExLine "   "))
  ]

pickerTests :: [Test]
pickerTests =
  [ test "fuzzy: characters in order" (assertEqual (Just 0, Just 2, Nothing) (fuzzyScore "ab" "xaby", fuzzyScore "ab" "axxb", fuzzyScore "ba" "ab"))
  , test "fuzzy: the best start wins" (assertEqual (Just 0) (fuzzyScore "ab" "a_xab"))
  , test "fuzzy: case is ignored" (assertEqual (Just 0) (fuzzyScore "SPEC" "test/Spec.hs"))
  , test "matches: best first, shorter on ties" $
      assertEqual ["src/ab.hs", "a/b.hs", "src/a/long/b.hs"] (map piLabel (matches "ab" (items ["src/a/long/b.hs", "a/b.hs", "src/ab.hs", "xyz"])))
  , test "an empty query keeps the order" (assertEqual ["b", "a"] (map piLabel (matches "" (items ["b", "a"]))))
  , test "moving wraps around" $
      let p = newPicker "t" (items ["a", "b", "c"])
       in assertEqual [1, 0, 2] (map (pkSelected . ($ p)) [moveSelection 1, moveSelection 3, moveSelection (-1)])
  , test "an exact first word or file name wins a tie" $
      assertEqual ["goto_line <line>", "x/b.hs"] (map (piLabel . head' . matches' (items ["goto_line_end", "goto_line <line>", "goto_line_start"])) ["goto_line"] <> map (piLabel . head' . matches' (items ["x/ab.hs", "x/b.hs.bak", "x/b.hs"])) ["b.hs"])
  , test "a label match beats a detail match" $
      assertEqual ["xy", "other"] (map piLabel (matches "xy" [pickerItem "other" (PickFile "") "xy here", pickerItem "xy" (PickFile "") ""]))
  , test "a new query selects the best match" $
      assertEqual (Just "b") (piLabel <$> selectedItem (setQuery "b" (moveSelection 2 (newPicker "t" (items ["a", "b", "c"])))))
  ]
  where
    items = map (\l -> pickerItem l (PickFile (T.unpack l)) "")
    matches' xs q = matches q xs
    head' = \case
      x : _ -> x
      [] -> pickerItem "" (PickFile "") ""

fileTests :: [Test]
fileTests =
  [ test "trailing newline is remembered" (assertEqual "a\nb\n" (roundTrip "a\nb\n"))
  , test "missing trailing newline is kept" (assertEqual "a\nb" (roundTrip "a\nb"))
  , test "CRLF is kept" (assertEqual "a\r\nb\r\n" (roundTrip "a\r\nb\r\n"))
  , test "CRLF lines are split" (assertEqual ["a", "b"] (B.toLines (docBuffer (decodeDocument Nothing "a\r\nb\r\n"))))
  , test "empty file stays empty" (assertEqual "" (roundTrip ""))
  , test "edited CRLF file saves CRLF everywhere" (assertEqual "a\r\nXb\r\nc\r\n" (editThenSave "a\r\nb\r\nc\r\n"))
  , test "edited LF file" (assertEqual "a\nXb\nc" (editThenSave "a\nb\nc"))
  , test "edited last line keeps no final newline" (assertEqual "a\nb\nXc" (encodeDocument (edit (Pos 2 0) (decodeDocument Nothing "a\nb\nc"))))
  , test "no final newline is remembered" (assertEqual False (docTrailingNewline (decodeDocument Nothing "a\nb")))
  , test "mixed endings follow the first line" (assertEqual ["a", "b\r", "c"] (B.toLines (docBuffer (decodeDocument Nothing "a\nb\r\nc"))))
  , test "chunks split anywhere decode the same" $
      let whole = "first line\r\nsecond æøå 漢字\r\n\r\nlast" :: ByteString
          expected = docBuffer (decodeDocument Nothing whole)
          splits = [docBuffer (decodeChunks Nothing [BS.take i whole, BS.drop i whole]) | i <- [0 .. BS.length whole]]
       in assertEqual [] [i | (i, b) <- zip [0 :: Int ..] splits, b /= expected]
  , test "many small chunks" $
      let whole = "a\nbb\nccc\n" :: ByteString
       in assertEqual (docBuffer (decodeDocument Nothing whole)) (docBuffer (decodeChunks Nothing [BS.singleton w | w <- BS.unpack whole]))
  ]
  where
    roundTrip :: ByteString -> ByteString
    roundTrip = encodeDocument . decodeDocument Nothing
    edit p d = d {docBuffer = fst (B.insertText p "X" (docBuffer d))}
    editThenSave = encodeDocument . edit (Pos 1 0) . decodeDocument Nothing

jsonTests :: [Test]
jsonTests =
  [ test "objects, arrays and literals" $
      assertEqual
        (Right (JObject [("a", JArray [JInt 1, JBool True, JNull]), ("b", JObject [])]))
        (parseJson " { \"a\" : [1, true, null], \"b\": {} } ")
  , test "numbers" $
      assertEqual (Right [JInt 0, JInt (-12), JDouble 1.5, JDouble 1000, JInt 123456789012345678901234567890])
        (traverse parseJson ["0", "-12", "1.5", "1e3", "123456789012345678901234567890"])
  , test "escapes" (assertEqual (Right (JString "\"\\/\b\f\n\r\t\233")) (parseJson "\"\\\"\\\\\\/\\b\\f\\n\\r\\t\\u00e9\""))
  , test "surrogate pairs join; a lone surrogate is replaced" $
      assertEqual (Right [JString "\128512", JString "\65533x"]) (traverse parseJson ["\"\\ud83d\\ude00\"", "\"\\ud83dx\""])
  , test "raw UTF-8 in strings" (assertEqual (Right (JString "æ漢")) (parseJson (TE.encodeUtf8 "\"æ漢\"")))
  , test "malformed input is an error" $
      assertEqual [True, True, True, True, True, True]
        (map (isLeft . parseJson) ["{\"a\" 1}", "[1,]", "\"open", "tru", "1 2", "\"a\nb\""])
  , test "accessors" $
      let v = JObject [("result", JObject [("n", JInt 3), ("s", JString "x")])]
       in assertEqual (Just 3, Just "x", Nothing) (J.path ["result", "n"] v >>= asInt, J.path ["result", "s"] v >>= asText, J.path ["result", "z"] v)
  , test "random values round-trip through render and parse" $
      let vals = take 300 (map (fst . genValue 3) (chunksOf 40 (randoms 23)))
       in assertEqual [] [v | v <- vals, parseJson (renderJson v) /= Right v]
  ]
  where
    isLeft = either (const True) (const False)
    chunksOf n xs = let (a, b) = splitAt n xs in a : chunksOf n b
    -- A random value from a supply of random numbers.
    genValue :: Int -> [Int] -> (Value, [Int])
    genValue depth (r : rs) = case r `mod` (if depth <= 0 then 5 else 7) of
      0 -> (JNull, rs)
      1 -> (JBool (even (r `div` 7)), rs)
      2 -> (JInt (fromIntegral (r `div` 7) - 2 ^ (40 :: Int)), rs)
      3 -> (JDouble (fromIntegral (r `mod` 100000) / 64), rs)
      4 -> (JString (T.pack (take (r `mod` 6) (map (pickChar . (`div` 3)) rs))), drop 6 rs)
      5 -> let (vs, rest) = many (r `mod` 4) (depth - 1) rs in (JArray vs, rest)
      _ -> let (vs, rest) = many (r `mod` 4) (depth - 1) rs in (JObject (zip ["k", "é", "\"q", "\n"] vs), rest)
    genValue _ [] = (JNull, [])
    many 0 _ rs = ([], rs)
    many n d rs = let (v, rs') = genValue d rs; (vs, rs'') = many (n - 1 :: Int) d rs' in (v : vs, rs'')
    pickChar n = let pool = "aZ \"\\\n\t\0é漢😀\DEL" in pool !! (n `mod` length pool)

tomlTests :: [Test]
tomlTests =
  [ test "tables, keys and values" $
      assertEqual
        (Right (JObject [("a", JInt 1), ("t", JObject [("s", JString "x\ty"), ("lit", JString "c:\\p"), ("b", JBool True), ("q k", JArray [JInt 1, JInt 2])])]))
        (parseToml "a = 1 # one\n\n[t]\ns = \"x\\ty\"\nlit = 'c:\\p'\nb = true\n\"q k\" = [1,\n  2, # two\n]\n")
  , test "dotted headers and keys" $
      assertEqual (Right (JObject [("keys", JObject [("normal", JObject [("g g", JString "x")])]), ("a", JObject [("b", JInt 3)])]))
        (parseToml "[keys.normal]\n\"g g\" = \"x\"\n[a]\nb = 3\n")
  , test "a # inside a string is not a comment" (assertEqual (Right (JObject [("k", JString "a # b")])) (parseToml "k = \"a # b\""))
  , test "inline tables, nested, over several lines, with a trailing comma" $
      assertEqual
        (Right (JObject [("a.b", JObject []), ("s", JObject [("fg", JString "red"), ("m", JArray [JString "bold", JString "dim"]), ("u", JObject [("c", JString "#fff")]), ("x", JObject [("y", JInt 1)])])]))
        (parseToml "\"a.b\" = {}\ns = { fg = \"red\", m = [\n  \"bold\",\n  \"dim\"], u = { c = '#fff' }, x.y = 1, }\n")
  , test "a # in a literal string does not end the line" $
      assertEqual (Right (JObject [("a", JObject [("fg", JString "#123456")]), ("b", JInt 2)])) (parseToml "a = { fg = '#123456' }\nb = 2")
  , test "errors name the line" $
      assertEqual [Left "line 2: k is set twice", Left "line 1: expected = after the key", Left "line 1: unterminated string"]
        [parseToml "k = 1\nk = 2", parseToml "k 1", parseToml "k = \"abc"]
  ]

ignoreTests :: [Test]
ignoreTests =
  [ test "a name matches at any depth" (assertEqual [True, True, False] (map (ign "*.o") ["a.o", "dir/b.o", "a.oo"]))
  , test "a leading slash anchors" (assertEqual [True, False] (map (ign "/build") ["build", "src/build"]))
  , test "a slash in the middle anchors" (assertEqual [True, False, False] (map (ign "doc/*.txt") ["doc/a.txt", "doc/sub/a.txt", "x/doc/a.txt"]))
  , test "a trailing slash matches directories only" $
      assertEqual (Just True, Nothing) (matchRules (parseIgnore "build/") "build" True, matchRules (parseIgnore "build/") "build" False)
  , test "**/ matches any directories" (assertEqual [True, True] (map (ign "**/foo") ["foo", "a/b/foo"]))
  , test "/**/ matches zero or more directories" (assertEqual [True, True, False] (map (ign "a/**/b") ["a/b", "a/x/y/b", "c/a/b"]))
  , test "/** matches everything inside" (assertEqual [True, True, False] (map (ign "abc/**") ["abc/x", "abc/x/y", "abc"]))
  , test "? and character classes" $
      assertEqual [True, False, True, False, True, False] (map (uncurry ign) [("a?c", "abc"), ("a?c", "a/c"), ("[a-c]x", "bx"), ("[a-c]x", "dx"), ("[!a]x", "bx"), ("[!a]x", "ax")])
  , test "comments, blank lines, escapes and trailing spaces" $
      assertEqual [Nothing, Just True, Just True] [matchRules (parseIgnore "# c\n\n") "c" False, matchRules (parseIgnore "\\#x") "#x" False, matchRules (parseIgnore "foo   ") "foo" False]
  , test "the last matching rule wins; ! re-includes" $
      assertEqual [True, False] (map (\f -> isIgnored [(Below "", parseIgnore "*.log\n!keep.log")] f False) ["a.log", "keep.log"])
  , test "a deeper file overrides" $
      let ig = [(Below "", parseIgnore "*.txt"), (Below "sub", parseIgnore "!keep.txt")]
       in assertEqual [False, True] (map (\f -> isIgnored ig f False) ["sub/keep.txt", "other/keep.txt"])
  , test "rules from an ancestor see the root's path in it" $
      assertEqual [True, False] (map (\f -> isIgnored [(Above "src", parseIgnore "/src/gen")] f True) ["gen", "other"])
  ]
  where
    ign pat path = isIgnored [(Below "", parseIgnore pat)] path False
