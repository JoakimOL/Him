module Main (main) where

import Control.Monad.Trans.State.Strict (execStateT)
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.ByteString.Builder (toLazyByteString)
import Data.ByteString.Lazy qualified as BL
import Data.Foldable (foldlM)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Him.Action
import Him.App (handleEvent)
import Him.Buffer qualified as B
import Data.Map.Strict qualified as Map
import Him.Config (Config (..))
import Him.Config.Default (allActions, configWith, defaultConfig)
import Him.Document
import Him.Edit
import Him.Editor
import Him.Event (Event (..))
import Him.Ex (parseExLine)
import Him.Search (Direction (..), Match (..), compileNeedle, findMatch)
import Him.Commands.Search (refreshSearchPreview)
import Him.History qualified as H
import Him.File (decodeChunks, decodeDocument, encodeDocument, loadDocument, loadDocumentChunked, saveDocument)
import Data.Text.Encoding qualified as TE
import System.Directory (getTemporaryDirectory, removeFile)
import Him.Key
import Him.Keymap
import Him.Mode (Mode (..))
import Him.Motion
import Him.Position (Pos (..))
import Him.Render.Diff (diffFrames)
import Him.Render.Frame (blankFrame, putCells, putText)
import Him.Selection
import Him.Terminal.Ansi (Color (..), Style (..), defaultStyle, packStyle, sgr, unpackStyle)
import Him.Terminal.Input (decodeKeys)
import Him.View (View (..), scrollToCursor)
import Him.TextWidth (charIndexAtCol, charWidth, displayCol, glyphs, isWide)
import Him.Render (render)
import Him.Render.Frame (Cell (..), Frame (..), ScrollInfo (..), continuation)
import Him.Render.Theme (defaultTheme)
import Data.Foldable (toList)
import Data.Sequence qualified as Seq
import Data.Text qualified as T
import Test.Harness

-- | Keys through a configuration with user bindings on top of the defaults.
rebindTests :: IO [Test]
rebindTests = do
  config <-
    either (fail . T.unpack) pure $
      configWith
        ( Map.fromList
            [ (Normal, [("C-d", "move_line_down 2"), ("j", "no_op"), ("space i", "insert_text \"// \""), ("Q", "ex q!"), ("g 3", "goto_line 3"), ("F", "search_text two")])
            , (Insert, [("C-a", "set_mode normal")])
            ]
        )
  let start t = newEditor (24, 80) (newDocument Nothing (buf t))
      typeKeys ks ed = foldlM (\e k -> execStateT (handleEvent config (EvKey k)) e) ed (fromMaybe (error ("bad keys: " <> show ks)) (parseKeys ks))
      headAfter t ks = rangeHead . primary . docSelection . edDoc <$> typeKeys ks (start t)
      textAfter t ks = B.toText . docBuffer . edDoc <$> typeKeys ks (start t)
  ctrlD <- headAfter "a\nb\nc\nd" "C-d"
  disabled <- headAfter "a\nb" "j"
  inherited <- headAfter "a\nb\nc\nd" "v C-d"
  inserted <- textAfter "x" "space i"
  quitByKey <- typeKeys "i y esc Q" (start "")
  gotoThree <- headAfter "a\nb\nc\nd" "g 3"
  gotoStill <- headAfter "a\nb\nc\nd" "g e g g"
  searchKey <- headAfter "one two" "F"
  insertExit <- typeKeys "i C-a" (start "")
  let badConfig =
        configWith
          ( Map.fromList
              [ (Normal, [("a", "fly"), ("b", "goto_line")])
              , (Insert, [("C-x", "set_mode command")])
              ]
          )
  pure
    [ test "a key bound to an action with an argument" (assertEqual (Pos 2 0) ctrlD)
    , test "no_op disables a default key" (assertEqual (Pos 0 0) disabled)
    , test "select mode inherits user normal bindings" (assertEqual (Pos 2 0) inherited)
    , test "insert_text with a quoted argument" (assertEqual "// x" inserted)
    , test "ex runs a : command" (assertEqual True (edQuit quitByKey))
    , test "a new chord next to default ones" (assertEqual (Pos 2 0) gotoThree)
    , test "default chords on the same prefix still work" (assertEqual (Pos 0 0) gotoStill)
    , test "search_text selects the match" (assertEqual (Pos 0 6) searchKey)
    , test "set_mode" (assertEqual Normal (edMode insertExit))
    , test "every bad binding is reported" $
        assertEqual
          ( Left
              ( T.intercalate
                  "\n"
                  [ "Normal mode, a: unknown action: fly"
                  , "Normal mode, b: goto_line: missing argument <line>"
                  , "Insert mode, C-x: set_mode: <mode> must be one of normal, insert, select, got command"
                  ]
              )
          )
          (() <$ badConfig)
    , test "the default keymaps cover every mode" (assertEqual [minBound .. maxBound] (Map.keys (cfgKeymaps (either (error . T.unpack) id defaultConfig))))
    ]

main :: IO ()
main = do
  integration <- integrationTests
  rebinding <- rebindTests
  loading <- loadingTests
  runTests
    [ group "Him.Key" keyTests
    , group "Him.Terminal.Input.decodeKeys" decodeTests
    , group "Him.Buffer" bufferTests
    , group "Him.Buffer (randomized against a list model)" ropeModelTests
    , group "Him.Search (randomized against a naive search)" searchTests
    , group "Him.Motion" motionTests
    , group "Him.Edit" editTests
    , group "Him.History" historyTests
    , group "Him.Keymap" keymapTests
    , group "Him.Action" actionTests
    , group "Him.File" fileTests
    , group "Him.Ex" exTests
    , group "Him.View" viewTests
    , group "Him.TextWidth" widthTests
    , group "Him.Render" renderTests
    , group "Him.Render.Diff" diffTests
    , group "keys through the default config" integration
    , group "rebinding keys to actions" rebinding
    , group "Him.File (from disk)" loading
    ]

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

buf :: Text -> B.Buffer
buf = B.fromText

bufferTests :: [Test]
bufferTests =
  [ test "empty has one line" (assertEqual 1 (B.lineCount B.empty))
  , test "fromText/toText round trip" (assertEqual "a\nb\n" (B.toText (buf "a\nb\n")))
  , test "insert in line" (assertEqual ("abXc", Pos 0 3) (ins (Pos 0 2) "X" "abc"))
  , test "insert newline" (assertEqual ("ab\nc", Pos 1 0) (ins (Pos 0 2) "\n" "abc"))
  , test "insert multi-line" (assertEqual ("aX\nYZ\nYb", Pos 2 1) (ins (Pos 0 1) "X\nYZ\nY" "ab"))
  , test "delete within line" (assertEqual "ac" (B.toText (B.deleteRange (Pos 0 1) (Pos 0 2) (buf "abc"))))
  , test "delete across lines" (assertEqual "ad" (B.toText (B.deleteRange (Pos 0 1) (Pos 1 1) (buf "ab\ncd"))))
  , test "textRange across lines" (assertEqual "b\nc" (B.textRange (Pos 0 1) (Pos 1 1) (buf "ab\ncd")))
  , test "nextPos crosses line end" (assertEqual (Pos 1 0) (B.nextPos (buf "a\nb") (Pos 0 1)))
  , test "nextPos stops at end" (assertEqual (Pos 1 1) (B.nextPos (buf "a\nb") (Pos 1 1)))
  , test "prevPos crosses line start" (assertEqual (Pos 0 1) (B.prevPos (buf "a\nb") (Pos 1 0)))
  , test "charAt line end is newline" (assertEqual (Just '\n') (B.charAt (Pos 0 1) (buf "a\nb")))
  , test "charAt end of file" (assertEqual Nothing (B.charAt (Pos 1 1) (buf "a\nb")))
  ]
  where
    ins :: Pos -> Text -> Text -> (Text, Pos)
    ins p t s = let (b', p') = B.insertText p t (buf s) in (B.toText b', p')

-- | A tiny deterministic pseudo-random generator (no QuickCheck: boot
-- libraries only).
randoms :: Int -> [Int]
randoms = drop 1 . iterate (\x -> (x * 6364136223846793005 + 1442695040888963407) `mod` (2 ^ (62 :: Int)))

ropeModelTests :: [Test]
ropeModelTests =
  [ test "fromRegions joins regions" (assertEqual ["a", "b", "c", "d"] (B.toLines (B.fromRegions [(False, "a\nb"), (False, "c\nd")])))
  , test "CR regions strip \\r" (assertEqual ["a", "b"] (B.toLines (B.fromRegions [(True, "a\r\nb\r")])))
  , test "2000 random edits match the model" (runModel 2000 1)
  , test "edits on a single line" (runModel 500 7)
  ]
  where
    start = B.fromRegions [(False, T.intercalate "\n" [T.pack ("line " <> show i) | i <- [n .. n + 49 :: Int]]) | n <- [0, 50 .. 450]]
    model0 = B.toLines start
    runModel steps seed = go steps (randoms seed) start model0
    go :: Int -> [Int] -> B.Buffer -> [Text] -> Either String ()
    go 0 _ b m = check b m
    go n (r1 : r2 : r3 : r4 : rs) b m = case check b m of
      Left e -> Left ("before step " <> show n <> ": " <> e)
      Right () ->
        let lineN = length m
            l1 = r1 `mod` lineN
            c1 = r2 `mod` (T.length (m !! l1) + 1)
            p1 = Pos l1 c1
         in if even r3
              then
                let t = ["x", "\n", "ab\ncd", "\n\n", "é"] !! (r4 `mod` 5)
                    (b', _) = B.insertText p1 t b
                 in go (n - 1) rs b' (modelInsert m p1 t)
              else
                let l2 = min (lineN - 1) (l1 + r4 `mod` 3)
                    c2 = (r4 `div` 3) `mod` (T.length (m !! l2) + 1)
                    p2 = max p1 (Pos l2 c2)
                 in go (n - 1) rs (B.deleteRange p1 p2 b) (modelDelete m p1 p2)
    go _ _ b m = check b m
    check b m
      | B.toLines b /= m = Left ("lines differ: " <> show (take 3 (B.toLines b)) <> " vs " <> show (take 3 m))
      | B.lineCount b /= length m = Left "lineCount differs"
      | [B.lineAt i b | i <- [0 .. length m - 1]] /= m = Left "lineAt differs"
      | B.linesDownFrom (length m - 1) b /= reverse m = Left "linesDownFrom differs"
      | otherwise = Right ()
    modelText = T.intercalate "\n"
    offsetOf m (Pos l c) = sum [T.length x + 1 | x <- take l m] + c
    modelInsert m p t = let whole = modelText m; (a, z) = T.splitAt (offsetOf m p) whole in T.splitOn "\n" (a <> t <> z)
    modelDelete m p q = let whole = modelText m in T.splitOn "\n" (T.take (offsetOf m p) whole <> T.drop (offsetOf m q) whole)

searchTests :: [Test]
searchTests =
  [ test "empty and multi-line patterns are rejected" (assertEqual (Nothing, Nothing) (compileNeedle "", compileNeedle "a\nb"))
  , test "smart case: lower-case pattern ignores case" (assertEqual (Just (Pos 0 4)) (matchStart <$> findIn "abc ABC" "abc" (Pos 0 0)))
  , test "smart case: upper-case pattern is exact" (assertEqual (Just (Pos 0 4)) (matchStart <$> findIn "abc ABC Abc" "ABC" (Pos 0 0)))
  , test "match end is inclusive" (assertEqual (Just (Pos 0 6)) (matchEnd <$> findIn "abc ABC" "abc" (Pos 0 0)))
  , test "wraps around" (assertEqual (Just (Match (Pos 0 0) (Pos 0 1) True)) (findIn "ab ab" "ab" (Pos 0 3)))
  , test "multi-byte columns" (assertEqual (Just (Pos 0 3)) (matchStart <$> findIn "漢字 x" "x" (Pos 0 0)))
  , test "1500 random searches match the naive search" (randomSearches 1500)
  ]
  where
    findIn t p pos = compileNeedle p >>= \n -> findMatch Forward n (buf t) pos
    alphabet = "aAbB é漢\t" :: String
    randomSearches :: Int -> Either String ()
    randomSearches count = go count (randoms 99)
      where
        go 0 _ = Right ()
        go k (r1 : r2 : r3 : r4 : r5 : r6 : rs) =
          let (b, rs') = randomBuffer r1 rs
              ls = B.toLines b
              needle = T.pack [alphabet !! (x `mod` length alphabet) | x <- take (1 + r2 `mod` 3) rs']
              l = r3 `mod` length ls
              pos = Pos l (r4 `mod` (T.length (ls !! l) + 1))
              dir = if even r5 then Forward else Backward
              expected = naive dir needle ls pos
              actual = (\n -> matchStart <$> findMatch dir n b pos) =<< compileNeedle needle
           in if expected == actual
                then go (k - 1) (drop 6 rs')
                else Left (show (dir, needle, pos, ls) <> ": expected " <> show expected <> ", got " <> show actual <> show r6)
        go _ _ = Right ()
    -- Several regions (blocks) plus a few edits, so searches cross block
    -- boundaries and edited single-line blocks.
    randomBuffer r rs =
      let lineOf xs = T.pack [alphabet !! (x `mod` length alphabet) | x <- xs]
          rows = [lineOf (take (x `mod` 7) (drop (i * 7) rs)) | (i, x) <- zip [0 .. 11] (drop 100 rs)]
          regions = [(False, T.intercalate "\n" chunk) | chunk <- chunksOf (1 + r `mod` 4) rows]
          b0 = B.fromRegions regions
          b1 = fst (B.insertText (Pos (r `mod` B.lineCount b0) 0) (lineOf (take 3 (drop 200 rs))) b0)
       in (b1, drop 300 rs)
    chunksOf n xs = case splitAt n xs of
      (a, []) -> [a]
      (a, z) -> a : chunksOf n z
    -- All match starts in document order, overlapping matches included.
    naive dir needle ls pos =
      let fold = not (T.any isUpperAscii needle)
          norm = if fold then T.map lowerAscii else id
          n = norm needle
          starts = [Pos li c | (li, line) <- zip [0 ..] ls, let ln = norm line, c <- [0 .. T.length ln - T.length n], n `T.isPrefixOf` T.drop c ln]
       in case dir of
            Forward -> case filter (> pos) starts of
              (p : _) -> Just p
              [] -> case starts of
                (p : _) -> Just p
                [] -> Nothing
            Backward -> case reverse (filter (< pos) starts) of
              (p : _) -> Just p
              [] -> case reverse starts of
                (p : _) -> Just p
                [] -> Nothing
    isUpperAscii c = c >= 'A' && c <= 'Z'
    lowerAscii c = if isUpperAscii c then toEnum (fromEnum c + 32) else c

-- | Apply a motion to a collapsed range at a position; returns (anchor, head).
runMotion :: Motion -> Text -> Pos -> (Pos, Pos)
runMotion m t p = let r = m (buf t) (point p) in (rangeAnchor r, rangeHead r)

motionTests :: [Test]
motionTests =
  [ test "h wraps to previous line end" (assertEqual (Pos 0 2, Pos 0 2) (runMotion charLeft "ab\ncd" (Pos 1 0)))
  , test "j keeps desired column" $
      let b = buf "abcd\na\nabcd"
          r1 = lineDown b (point (Pos 0 3))
          r2 = lineDown b r1
       in assertEqual (Pos 1 1, Pos 2 3) (rangeHead r1, rangeHead r2)
  , test "w selects word and trailing blanks" (assertEqual (Pos 0 0, Pos 0 5) (runMotion nextWordStart "hello world" (Pos 0 0)))
  , test "w from end of word selects next word" (assertEqual (Pos 0 6, Pos 0 10) (runMotion nextWordStart "hello world" (Pos 0 5)))
  , test "w skips to next line" (assertEqual (Pos 1 0, Pos 1 2) (runMotion nextWordStart "ab\ncde" (Pos 0 1)))
  , test "w treats punctuation as its own word" (assertEqual (Pos 0 0, Pos 0 2) (runMotion nextWordStart "foo.bar" (Pos 0 0)))
  , test "e selects to end of word" (assertEqual (Pos 0 0, Pos 0 4) (runMotion nextWordEnd "hello world" (Pos 0 0)))
  , test "e from end of word goes to next word end" (assertEqual (Pos 0 5, Pos 0 10) (runMotion nextWordEnd "hello world" (Pos 0 4)))
  , test "b selects back to word start" (assertEqual (Pos 0 8, Pos 0 6) (runMotion prevWordStart "hello world" (Pos 0 8)))
  , test "b from word start goes to previous word" (assertEqual (Pos 0 5, Pos 0 0) (runMotion prevWordStart "hello world" (Pos 0 6)))
  , test "b at file start stays" (assertEqual (Pos 0 0, Pos 0 0) (runMotion prevWordStart "hello" (Pos 0 0)))
  , test "w at end of file stays" (assertEqual (Pos 0 5, Pos 0 5) (runMotion nextWordStart "hello" (Pos 0 5)))
  , test "x selects the line with its newline" (assertEqual (Pos 1 0, Pos 1 2) (runMotion selectLine "ab\ncd\nef" (Pos 1 1)))
  , test "x twice extends a line" $
      let b = buf "ab\ncd\nef"
          r = selectLine b (selectLine b (point (Pos 0 1)))
       in assertEqual (Pos 0 0, Pos 1 2) (rangeAnchor r, rangeHead r)
  , test "extend keeps the anchor" $
      let r = applyMotion Extend charRight (buf "abc") (Range (Pos 0 0) (Pos 0 1) Nothing)
       in assertEqual (Pos 0 0, Pos 0 2) (rangeAnchor r, rangeHead r)
  ]

runEdit :: Edit -> Text -> Range -> (Text, Pos)
runEdit e t r = let (b', r') = e (buf t) r in (B.toText b', rangeHead r')

editTests :: [Test]
editTests =
  [ test "backspace joins lines" (assertEqual ("abcd", Pos 0 2) (runEdit deleteBackward "ab\ncd" (point (Pos 1 0))))
  , test "backspace at start does nothing" (assertEqual ("ab", Pos 0 0) (runEdit deleteBackward "ab" (point (Pos 0 0))))
  , test "newline keeps indentation" (assertEqual ("  ab\n  c", Pos 1 2) (runEdit insertNewline "  abc" (point (Pos 0 4))))
  , test "delete selection is inclusive" (assertEqual ("ad", Pos 0 1) (runEdit deleteSelection "abcd" (Range (Pos 0 1) (Pos 0 2) Nothing)))
  , test "delete a line with its newline" (assertEqual ("ab\nef", Pos 1 0) (runEdit deleteSelection "ab\ncd\nef" (Range (Pos 1 0) (Pos 1 2) Nothing)))
  , test "delete the last line removes it" (assertEqual ("ab", Pos 0 0) (runEdit deleteSelection "ab\ncd" (Range (Pos 1 0) (Pos 1 2) Nothing)))
  , test "open line below" (assertEqual ("  ab\n  \ncd", Pos 1 2) (runEdit openLineBelow "  ab\ncd" (point (Pos 0 1))))
  ]

historyTests :: [Test]
historyTests =
  [ test "nothing to undo" (assertEqual Nothing (fst <$> H.undo (snap "a") H.emptyHistory))
  , test "only the first state of a change is kept" $
      let h = H.commit (H.beginChange (snap "b") (H.beginChange (snap "a") H.emptyHistory))
       in assertEqual (Just (snap "a")) (fst <$> H.undo (snap "c") h)
  , test "undo then redo returns to the current state" $
      let h = H.commit (H.beginChange (snap "a") H.emptyHistory)
       in assertEqual (Just (snap "b")) (H.undo (snap "b") h >>= \(s', h') -> fst <$> H.redo s' h')
  , test "a new change clears redo" $
      let h1 = H.commit (H.beginChange (snap "a") H.emptyHistory)
          h2 = maybe H.emptyHistory snd (H.undo (snap "b") h1)
          h3 = H.commit (H.beginChange (snap "a") h2)
       in assertEqual Nothing (fst <$> H.redo (snap "x") h3)
  ]
  where
    snap t = H.Snapshot (buf t) (single (point (Pos 0 0)))

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

exTests :: [Test]
exTests =
  [ test "name and args" (assertEqual (Just ("w", ["file.txt"])) (parseExLine "w file.txt"))
  , test "blank line" (assertEqual Nothing (parseExLine "   "))
  ]

viewTests :: [Test]
viewTests =
  [ test "scrolls down with scrolloff" (assertEqual (View 3 0) (scrollToCursor (10, 80) 3 (9, 0) (View 0 0)))
  , test "scrolls up with scrolloff" (assertEqual (View 2 0) (scrollToCursor (10, 80) 3 (5, 0) (View 10 0)))
  , test "no scroll when visible" (assertEqual (View 0 0) (scrollToCursor (10, 80) 3 (4, 0) (View 0 0)))
  , test "scrolls right" (assertEqual (View 0 21) (scrollToCursor (10, 80) 3 (0, 100) (View 0 0)))
  ]

diffTests :: [Test]
diffTests =
  [ test "styles survive packing" $
      let samples = [defaultStyle, defaultStyle {styleFg = Rgb 1 2 3, styleBg = Indexed 240, styleBold = True, styleReverse = True}, defaultStyle {styleFg = Ansi 9, styleItalic = True, styleUnderline = True}]
       in assertEqual samples (map (unpackStyle . packStyle) samples)
  , test "300 random frame sequences replay exactly in a terminal model" (randomDiffs 300)
  , test "identical frames redraw no rows" (assertEqual False ("top" `isInfix` emit (Just f1) f1))
  , test "only the changed row is drawn" $
      let out = emit (Just f1) f2
       in assertEqual (True, False) ("\ESC[2;1H" `isInfix` out, "\ESC[1;1H" `isInfix` out)
  , test "no previous frame clears the screen" (assertEqual True ("\ESC[2J" `isInfix` emit Nothing f1))
  ]
  where
    f1 = putText 0 0 defaultStyle "top" (blankFrame 3 10)
    f2 = putText 1 0 defaultStyle "changed" f1
    emit p f = toLazyByteString (diffFrames p f)
    isInfix needle hay = BL.toStrict needle `BS.isInfixOf` BL.toStrict hay

widthTests :: [Test]
widthTests =
  [ test "ascii is narrow" (assertEqual 1 (charWidth 'a'))
  , test "CJK is wide" (assertEqual 2 (charWidth '漢'))
  , test "emoji is wide" (assertEqual 2 (charWidth '😀'))
  , test "control chars show as ^X" (assertEqual ("^A", 2) (glyphs '\SOH' 2, charWidth '\SOH'))
  , test "tab expands to the next stop" (assertEqual 4 (displayCol "\tx" 1))
  , test "tab after text" (assertEqual 4 (displayCol "ab\tx" 3))
  , test "wide chars shift columns" (assertEqual 4 (displayCol "漢字x" 2))
  , test "column inside a wide char maps to it" (assertEqual 1 (charIndexAtCol "漢字x" 3))
  , test "column past the end" (assertEqual 3 (charIndexAtCol "abc" 10))
  , test "j keeps the visual column across tabs" $
      let b = buf "\tabc\n    xyz"
          r = lineDown b (point (Pos 0 2))
       in assertEqual (Pos 1 5) (rangeHead r)
  ]

renderTests :: [Test]
renderTests =
  [ test "gutter shows line numbers" (assertEqual "  1 hello" (T.take 9 (rowText (frameOf "hello") 0)))
  , test "wide chars use a continuation cell" $
      assertEqual [Just '漢', Just continuation, Just 'x'] (map (cellAt (frameOf "漢x") 0) [4, 5, 6])
  , test "control chars are drawn as ^X" (assertEqual "^[x" (T.take 3 (T.drop 4 (rowText (frameOf "\ESCx") 0))))
  , test "rendering with the previous frame gives the same frame" $
      let base = start "hello\nworld\nthird line"
          eds = [base, base {edMode = Insert}, base {edDoc = (edDoc base) {docSelection = single (Range (Pos 0 1) (Pos 1 2) Nothing)}}, base]
          withPrev = go Nothing eds
          go _ [] = []
          go p (e : es) = let f = render defaultTheme p e in f : go (Just f) es
       in assertEqual (map (frameCells . render defaultTheme Nothing) eds) (map frameCells withPrev)
  , test "long file names are shortened" $
      let ed = (start "") {edSize = (5, 30), edDoc = (newDocument (Just (replicate 60 'p' <> "/name.txt")) B.empty) {docDirty = True}}
          status = rowText (render defaultTheme Nothing ed) 3
       in assertEqual (True, True) ("[+]" `T.isInfixOf` status, "name.txt" `T.isInfixOf` status)
  ]
  where
    start t = newEditor (5, 40) (newDocument Nothing (buf t))
    frameOf t = render defaultTheme Nothing (start t)
    rowText f r = T.pack [c | Cell c _ <- maybe [] toList (lookupRow f r)]
    lookupRow f r = case drop r (toList (frameCells f)) of
      (row : _) -> Just row
      [] -> Nothing
    cellAt f r c = case drop c (maybe [] toList (lookupRow f r)) of
      (Cell ch _ : _) -> Just ch
      [] -> Nothing

-- | Files larger than the read chunk, with multi-byte characters straddling
-- chunk boundaries, load exactly like an in-memory decode.
loadingTests :: IO [Test]
loadingTests = do
  dir <- getTemporaryDirectory
  let path = dir <> "/him-test-load.txt"
      line i = T.pack (show i) <> " æøå 漢字 😀 lorem ipsum\r\n"
      bytes = TE.encodeUtf8 (T.concat (map line [1 .. 60000 :: Int]) <> "tail")
      summary d = (B.toLines (docBuffer d), docLineEnding d, docTrailingNewline d)
      expected = summary (decodeDocument (Just path) bytes)
  BS.writeFile path bytes
  loaded <- loadDocument path
  -- Saving an unedited large file writes its pinned regions directly; an
  -- edit in the middle mixes both paths.
  savedBytes <- case loaded of
    Right d -> do
      let d' = d {docBuffer = fst (B.insertText (Pos 30000 2) "EDIT" (docBuffer d))}
      _ <- saveDocument (path <> ".out") d
      plainSave <- BS.readFile (path <> ".out")
      _ <- saveDocument (path <> ".out") d'
      editedSave <- BS.readFile (path <> ".out")
      removeFile (path <> ".out")
      pure (Just (plainSave, editedSave, encodeDocument d'))
    Left _ -> pure Nothing
  -- Invalid UTF-8 takes the lenient decoding fallback.
  let invalid = "ok\n\255\254 bad\r\nend" :: ByteString
  BS.writeFile path invalid
  loadedInvalid <- loadDocument path
  removeFile path
  -- What pipes get (no known size): chunked reads, with characters and
  -- lines straddling the 1 MB chunks.
  BS.writeFile path bytes
  loadedChunked <- loadDocumentChunked path
  removeFile path
  pure
    [ test "large file loads like an in-memory decode" (assertEqual (Right expected) (summary <$> loaded))
    , test "saving a loaded file writes the same bytes" (assertEqual (Just bytes) ((\(a, _, _) -> a) <$> savedBytes))
    , test "saving after an edit matches encodeDocument" (assertEqual True (maybe False (\(_, b, c) -> b == c) savedBytes))
    , test "invalid UTF-8 loads like a lenient decode" $
        assertEqual (Right (summary (decodeDocument (Just path) invalid))) (summary <$> loadedInvalid)
    , test "chunked reads (pipes) load the same" (assertEqual (Right expected) (summary <$> loadedChunked))
    ]

-- | Replays diff output on a minimal terminal model and checks that the
-- screen equals the new frame (characters and styles, cell by cell).
randomDiffs :: Int -> Either String ()
randomDiffs n = go n (randoms 5) Nothing emptyScreen
  where
    rows = 5
    cols = 12
    styles = [defaultStyle, defaultStyle {styleReverse = True}, defaultStyle {styleFg = Indexed 240}]
    chars = "ab  漢x" :: String
    emptyScreen = replicate rows (replicate cols (' ', sgrText defaultStyle))
    go 0 _ _ _ = Right ()
    go k rs prev screen =
      let (frame, rs') = randomFrame rs
          out = TE.decodeUtf8 (BL.toStrict (toLazyByteString (diffFrames prev frame)))
          screen' = replay (T.unpack out) screen
          expected = [[(c, sgrText (unpackStyle st)) | Cell c st <- toList row] | row <- toList (frameCells frame)]
       in if screen' == expected
            then go (k - 1) rs' (Just frame) screen'
            else Left ("mismatch at step " <> show (n - k) <> ": " <> show out)
    -- Random text, and a random view position for rows 0-3 (so the diff
    -- sometimes scrolls the terminal).
    randomFrame rs0 =
      let (f, rs1) = foldl addText (blankFrame rows cols, rs0) [0 .. 5 :: Int]
          (r, rs2) = case rs1 of
            (x : xs) -> (x, xs)
            [] -> (0, [])
       in ((sanitize f) {frameScroll = Just (ScrollInfo 0 4 (r `mod` 6))}, rs2)
      where
        addText (f, r1 : r2 : r3 : r4 : rest) _ =
          let t = T.pack [chars !! (x `mod` length chars) | x <- take (r3 `mod` 6) rest]
              st = packStyle (styles !! (r4 `mod` length styles))
              cells = concat [if isWide c then [Cell c st, Cell continuation st] else [Cell c st] | c <- T.unpack t]
           in (putCells (r1 `mod` rows) (r2 `mod` cols) cells f, drop 6 rest)
        addText acc _ = acc
    -- Frames from the renderer never contain half a wide character; make
    -- the random ones valid the same way.
    sanitize f = f {frameCells = fmap (Seq.fromList . fixRow . toList) (frameCells f)}
    fixRow (Cell a sa : Cell b sb : rest)
      | isWide a && b == continuation = Cell a sa : Cell b sb : fixRow rest
      | isWide a = Cell ' ' sa : fixRow (Cell b sb : rest)
      | a == continuation = Cell ' ' sa : fixRow (Cell b sb : rest)
    fixRow [Cell a sa] | isWide a || a == continuation = [Cell ' ' sa]
    fixRow (c : rest) = c : fixRow rest
    fixRow [] = []
    sgrText st = TE.decodeUtf8 (BL.toStrict (toLazyByteString (sgr st)))
    -- The terminal model: cursor, current SGR, scroll region, and a grid of
    -- (char, SGR).
    replay str scr = run str (0 :: Int, 0 :: Int) (sgrText defaultStyle) (0, rows - 1) scr
    run [] _ _ _ scr = scr
    run ('\ESC' : '[' : rest) cur cs region scr =
      let (params, rest1) = span (\c -> c >= '0' && c <= '?') rest
          (inter, rest2) = span (\c -> c >= ' ' && c <= '/') rest1
          blankRow = [(' ', cs) | _ <- [1 .. cols]]
          (rt, rb) = region
       in case rest2 of
            (final : rest3) -> case final of
              'H' -> let (r, c) = break (== ';') params in run rest3 (read r - 1, read (drop 1 c) - 1) cs region scr
              'm' -> run rest3 cur cs' region scr where cs' = T.pack ("\ESC[" <> params <> inter <> "m")
              'K' -> let (r, c) = cur in run rest3 cur cs region (setRow r [(c', (' ', cs)) | c' <- [c .. cols - 1]] scr)
              'J' -> run rest3 cur cs region [blankRow | _ <- [1 .. rows]]
              'r' -> case break (== ';') params of
                ("", _) -> run rest3 (0, 0) cs (0, rows - 1) scr
                (a, b) -> run rest3 (0, 0) cs (read a - 1, read (drop 1 b) - 1) scr
              'S' -> run rest3 cur cs region (scrollRows (read params) rt rb blankRow scr)
              'T' -> run rest3 cur cs region (scrollRows (negate (read params)) rt rb blankRow scr)
              _ -> run rest3 cur cs region scr
            [] -> scr
    run (ch : rest) (r, c) cs region scr
      | isWide ch = run rest (r, c + 2) cs region (setRow r [(c, (ch, cs)), (c + 1, (continuation, cs))] scr)
      | otherwise = run rest (r, c + 1) cs region (setRow r [(c, (ch, cs))] scr)
    -- Positive: contents move up.
    scrollRows d rt rb blankRow scr =
      [ if i < rt || i > rb
          then row
          else case i + d of
            j | j >= rt && j <= rb -> scr !! j
            _ -> blankRow
      | (i, row) <- zip [0 ..] scr
      ]
    setRow r updates scr =
      [ if i == r then [maybe old id (lookup j updates) | (j, old) <- zip [0 ..] row] else row
      | (i, row) <- zip [0 ..] scr
      ]

-- | Feed key sequences through 'handleEvent' with the real keymaps.
integrationTests :: IO [Test]
integrationTests = do
  config <- either (fail . show) pure defaultConfig
  let start t = newEditor (24, 80) (newDocument Nothing (buf t))
      -- Like the main loop: handle the key, then refresh the search preview.
      typeKeys ks ed = foldlM (\e k -> refreshSearchPreview <$> execStateT (handleEvent config (EvKey k)) e) ed (fromMaybe (error ("bad keys: " <> show ks)) (parseKeys ks))
      selectionAfter t ks = (\e -> let r = primary (docSelection (edDoc e)) in (rangeAnchor r, rangeHead r)) <$> typeKeys ks (start t)
      textAfter t ks = B.toText . docBuffer . edDoc <$> typeKeys ks (start t)
  typed <- textAfter "" "i h i space t h e r e esc"
  newline <- textAfter "ab" "a ret c esc"
  opened <- textAfter "one\ntwo" "o x esc"
  backspace <- textAfter "abc" "a a backspace backspace esc"
  quitDirty <- typeKeys ": q ret" =<< typeKeys "i x esc" (start "")
  quitClean <- typeKeys ": q ret" (start "")
  cmdEsc <- typeKeys ": w esc" (start "")
  wordDelete <- textAfter "hello world" "w d"
  lineDelete <- textAfter "one\ntwo\nthree" "j x d"
  lineTwice <- textAfter "one\ntwo\nthree" "x x d"
  change <- textAfter "foo bar" "e c b a z esc"
  gotoEnd <- textAfter "one\ntwo\nthree" "g e x d"
  gotoTop <- textAfter "one\ntwo" "j g g x d"
  selectExtend <- textAfter "abcdef" "v l l esc d"
  collapsed <- textAfter "hello world" "w ; d"
  undoInsert <- textAfter "" "i a b c esc u"
  redoInsert <- textAfter "" "i a b c esc u U"
  undoChange <- textAfter "foo bar" "e c x esc u"
  undoTwice <- textAfter "a\nb\nc" "x d x d u"
  undoClean <- typeKeys "i x esc u" (start "abc")
  nothingToUndo <- typeKeys "u" (start "abc")
  pasteLineBelow <- textAfter "one\ntwo\nthree" "x y j p"
  movedLine <- textAfter "a\nb\nc" "x d p"
  pasteAtEnd <- textAfter "a\nb" "x y g e p"
  pasteAbove <- textAfter "one\ntwo" "j x y g g P"
  pasteChars <- textAfter "hello world" "e y p"
  undoPaste <- textAfter "hello world" "e y p u"
  searched <- selectionAfter "one two\nthree two" "/ t w o ret"
  searchNext <- selectionAfter "one two\nthree two" "/ t w o ret n"
  searchWrap <- selectionAfter "one two\nthree two" "/ t w o ret n n"
  searchBack <- selectionAfter "one two\nthree two" "g e ? t w o ret"
  searchPrev <- selectionAfter "one two\nthree two" "/ t w o ret n N"
  previewed <- selectionAfter "one two\nthree two" "/ t h r"
  cancelled <- selectionAfter "one two\nthree two" "l / t h r esc"
  starSearch <- selectionAfter "one two\none" "e * n"
  notFound <- typeKeys "/ z z ret" (start "abc")
  deleteMatch <- textAfter "one two three" "/ t w o ret d"
  pendingG <- typeKeys "g" (start "abc")
  badChord <- typeKeys "g z" (start "abc")
  pure
    [ test "typing in insert mode" (assertEqual "hi there" typed)
    , test "append then newline" (assertEqual "a\ncb" newline)
    , test "open below" (assertEqual "one\nx\ntwo" opened)
    , test "backspace in insert mode" (assertEqual "bc" backspace)
    , test ":q refuses when dirty" (assertEqual (False, True) (edQuit quitDirty, isError (edStatus quitDirty)))
    , test ":q quits when clean" (assertEqual True (edQuit quitClean))
    , test "esc leaves the command line" (assertEqual (Normal, "") (edMode cmdEsc, edCmdLine cmdEsc))
    , test "w d deletes a word and its blanks" (assertEqual "world" wordDelete)
    , test "x d deletes a line" (assertEqual "one\nthree" lineDelete)
    , test "x x d deletes two lines" (assertEqual "three" lineTwice)
    , test "e c replaces a word" (assertEqual "baz bar" change)
    , test "g e goes to the last line" (assertEqual "one\ntwo" gotoEnd)
    , test "g g goes to the first line" (assertEqual "two" gotoTop)
    , test "select mode extends" (assertEqual "def" selectExtend)
    , test "; collapses the selection" (assertEqual "helloworld" collapsed)
    , test "u undoes a whole insert session" (assertEqual "" undoInsert)
    , test "U redoes it" (assertEqual "abc" redoInsert)
    , test "u undoes a change (c + typing) at once" (assertEqual "foo bar" undoChange)
    , test "u undoes one step at a time" (assertEqual "b\nc" undoTwice)
    , test "undo back to the saved text is not dirty" (assertEqual False (docDirty (edDoc undoClean)))
    , test "nothing to undo is reported" (assertEqual (Just (Status Info "nothing to undo")) (edStatus nothingToUndo))
    , test "p pastes a yanked line below" (assertEqual "one\ntwo\none\nthree" pasteLineBelow)
    , test "x d p moves a line down" (assertEqual "b\na\nc" movedLine)
    , test "p pastes a line below the last line" (assertEqual "a\nb\na" pasteAtEnd)
    , test "P pastes a line above" (assertEqual "two\none\ntwo" pasteAbove)
    , test "p pastes characters after the selection" (assertEqual "hellohello world" pasteChars)
    , test "u undoes a paste" (assertEqual "hello world" undoPaste)
    , test "/ selects the first match" (assertEqual (Pos 0 4, Pos 0 6) searched)
    , test "n selects the next match" (assertEqual (Pos 1 6, Pos 1 8) searchNext)
    , test "n wraps around" (assertEqual (Pos 0 4, Pos 0 6) searchWrap)
    , test "? searches backward" (assertEqual (Pos 0 4, Pos 0 6) searchBack)
    , test "N goes back" (assertEqual (Pos 0 4, Pos 0 6) searchPrev)
    , test "typing previews the match" (assertEqual (Pos 1 0, Pos 1 2) previewed)
    , test "esc restores the selection" (assertEqual (Pos 0 1, Pos 0 1) cancelled)
    , test "* then n searches for the selection" (assertEqual (Pos 1 0, Pos 1 2) starSearch)
    , test "a missing pattern is reported" (assertEqual (Just (Status Error "pattern not found: zz")) (edStatus notFound))
    , test "d deletes the match" (assertEqual "one  three" deleteMatch)
    , test "g waits for the next key" (assertEqual [plain (KChar 'g')] (edPending pendingG))
    , test "an unknown chord is dropped" (assertEqual ([], "abc") (edPending badChord, B.toText (docBuffer (edDoc badChord))))
    ]
  where
    isError = \case
      Just (Status Error _) -> True
      _ -> False
