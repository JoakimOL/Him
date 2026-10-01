module Main (main) where

import Control.Monad.Trans.State.Strict (execStateT)
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.ByteString.Builder (toLazyByteString)
import Data.ByteString.Lazy qualified as BL
import Data.Foldable (foldlM)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Him.App (handleEvent)
import Him.Buffer qualified as B
import Him.Config.Default (defaultConfig)
import Him.Document
import Him.Edit
import Him.Editor
import Him.Event (Event (..))
import Him.Ex (parseExLine)
import Him.File (decodeDocument, encodeDocument)
import Him.Key
import Him.Keymap
import Him.Mode (Mode (..))
import Him.Motion
import Him.Position (Pos (..))
import Him.Render.Diff (diffFrames)
import Him.Render.Frame (blankFrame, putText)
import Him.Selection
import Him.Terminal.Ansi (defaultStyle)
import Him.Terminal.Input (decodeKeys)
import Him.View (View (..), scrollToCursor)
import Test.Harness

main :: IO ()
main = do
  integration <- integrationTests
  runTests
    [ group "Him.Key" keyTests
    , group "Him.Terminal.Input.decodeKeys" decodeTests
    , group "Him.Buffer" bufferTests
    , group "Him.Motion" motionTests
    , group "Him.Edit" editTests
    , group "Him.Keymap" keymapTests
    , group "Him.File" fileTests
    , group "Him.Ex" exTests
    , group "Him.View" viewTests
    , group "Him.Render.Diff" diffTests
    , group "keys through the default config" integration
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
  , test "invalid key sequence is an error" (assertEqual (Left "invalid key sequence: nope") (() <$ fromBindings [("nope", "x")]))
  , test "default config is valid" (assertEqual (Right ()) (() <$ defaultConfig))
  ]
  where
    km = either (error . show) id (fromBindings [("a", "one"), ("g g", "top")])
    keys = fromMaybe [] . parseKeys

fileTests :: [Test]
fileTests =
  [ test "trailing newline is remembered" (assertEqual "a\nb\n" (roundTrip "a\nb\n"))
  , test "missing trailing newline is kept" (assertEqual "a\nb" (roundTrip "a\nb"))
  , test "CRLF is kept" (assertEqual "a\r\nb\r\n" (roundTrip "a\r\nb\r\n"))
  , test "CRLF lines are split" (assertEqual ["a", "b"] (B.toLines (docBuffer (decodeDocument Nothing "a\r\nb\r\n"))))
  , test "empty file stays empty" (assertEqual "" (roundTrip ""))
  ]
  where
    roundTrip :: ByteString -> ByteString
    roundTrip = encodeDocument . decodeDocument Nothing

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
  [ test "identical frames redraw no rows" (assertEqual False ("top" `isInfix` render (Just f1) f1))
  , test "only the changed row is drawn" $
      let out = render (Just f1) f2
       in assertEqual (True, False) ("\ESC[2;1H" `isInfix` out, "\ESC[1;1H" `isInfix` out)
  , test "no previous frame clears the screen" (assertEqual True ("\ESC[2J" `isInfix` render Nothing f1))
  ]
  where
    f1 = putText 0 0 defaultStyle "top" (blankFrame 3 10)
    f2 = putText 1 0 defaultStyle "changed" f1
    render p f = toLazyByteString (diffFrames p f)
    isInfix needle hay = BL.toStrict needle `BS.isInfixOf` BL.toStrict hay

-- | Feed key sequences through 'handleEvent' with the real keymaps.
integrationTests :: IO [Test]
integrationTests = do
  config <- either (fail . show) pure defaultConfig
  let start t = newEditor (24, 80) (newDocument Nothing (buf t))
      typeKeys ks ed = foldlM (\e k -> execStateT (handleEvent config (EvKey k)) e) ed (fromMaybe (error ("bad keys: " <> show ks)) (parseKeys ks))
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
    , test "g waits for the next key" (assertEqual [plain (KChar 'g')] (edPending pendingG))
    , test "an unknown chord is dropped" (assertEqual ([], "abc") (edPending badChord, B.toText (docBuffer (edDoc badChord))))
    ]
  where
    isError = \case
      Just (Status Error _) -> True
      _ -> False
