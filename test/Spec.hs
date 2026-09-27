module Main (main) where

import Data.ByteString (ByteString)
import Data.Text (Text)
import Him.Key
import Him.Terminal.Input (decodeKeys)
import Test.Harness

main :: IO ()
main =
  runTests
    [ group "Him.Key" keyTests
    , group "Him.Terminal.Input.decodeKeys" decodeTests
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
