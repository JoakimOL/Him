-- | Registers (@\"@ and its picks, @_@, @+@ / @*@ through a fake
-- clipboard), @:registers@ and @:clear-register@, through the default keys.
module Test.Register
  ( registerTests
  ) where

import Control.Monad.Trans.State.Strict (execStateT)
import Data.ByteString.Char8 qualified as BC
import Data.Foldable (foldlM)
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import Data.Map.Strict qualified as Map
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Him.Buffer qualified as B
import Him.Clipboard (ClipboardKind (..), ClipboardProvider (..), base64, joinValues)
import Him.Config (Config (..))
import Him.Config.Default (defaultConfig)
import Him.Document
import Him.Editor
import Him.Event (Event (..))
import Him.Key (parseKeys)
import Him.Mode (Mode (..))
import Him.Session (handleEvent)
import Test.Harness
import Test.Util

-- | A clipboard in memory: (clipboard, primary).
fakeClipboard :: IORef (Text, Text) -> ClipboardProvider
fakeClipboard ref =
  ClipboardProvider
    { cbName = "fake"
    , cbAvailable = pure True
    , cbGet = \k -> Right . pick k <$> readIORef ref
    , cbSet = \k t -> do
        (c, p) <- readIORef ref
        writeIORef ref (if k == PrimarySelection then (c, t) else (t, p))
        pure (Right ())
    }
  where
    pick k (c, p) = if k == PrimarySelection then p else c

registerTests :: IO [Test]
registerTests = do
  defaults <- either (fail . show) pure defaultConfig
  clip <- newIORef ("", "")
  let config = defaults {cfgClipboardProviders = [fakeClipboard clip]}
      start t = newEditor (24, 80) (newDocument Nothing (buf t))
      typeKeys ks ed = foldlM (\e k -> execStateT (handleEvent config (EvKey k)) e) ed (fromMaybe (error ("bad keys: " <> show ks)) (parseKeys ks))
      textOf = B.toText . docBuffer . edDoc
      reg c = Map.lookup c . edRegisters
  -- Named registers keep their own values.
  named <- typeKeys "x \" a y j x y" (start "one\ntwo\n")
  pastedNamed <- typeKeys "\" a p" named
  pastedDefault <- typeKeys "p" named
  -- The pick lasts one command.
  oneShot <- typeKeys "\" a y j y" (start "one\ntwo")
  -- _ discards: deleting into it leaves the default register alone.
  blackHole <- typeKeys "x y j x \" _ d" (start "one\ntwo\n")
  -- + goes to the clipboard and comes back from it.
  copied <- typeKeys "\" + y" (start "one")
  clipAfterYank <- fst <$> readIORef clip
  writeIORef clip ("pasted", "")
  pastedClip <- typeKeys "g l space p" (start "x")
  primary <- typeKeys "\" * y" (start "pri")
  primaryAfter <- snd <$> readIORef clip
  -- R replaces the selection with a register, space R with the clipboard.
  replaced <- typeKeys "x y j x R" (start "one\ntwo\n")
  replacedNamed <- typeKeys "x \" a y j x y j x \" a R" (start "one\ntwo\nthree\n")
  writeIORef clip ("CLIP", "")
  replacedClip <- typeKeys "v l space R" (start "abc")
  -- y in select mode yanks and goes back to normal mode.
  yankedSelect <- typeKeys "v l y" (start "abc")
  -- C-r in insert mode inserts a register.
  inserted <- typeKeys "\" a y i C-r a esc" (start "one")
  -- :registers lists them; the popup shows the clipboard's current text.
  yanked <- typeKeys "y \" + y" (start "a")
  writeIORef clip ("fresh", "")
  listed <- typeKeys ": r e g i s t e r s ret" yanked
  -- :clear-register forgets some, or all.
  both <- typeKeys "\" a y \" b y" (start "a")
  clearedA <- typeKeys ": c l e a r - r e g i s t e r space a ret" both
  clearedAll <- typeKeys ": c l e a r - r e g i s t e r ret" both
  -- The " prefix shows the registers.
  prompting <- typeKeys "y \"" (start "a")
  picked <- typeKeys "\" q" (start "a")
  pure
    [ test "\" a y yanks into a only, y into \"" (assertEqual (Just ["one\n"], Just ["two\n"]) (reg 'a' named, reg '"' named))
    , test "\" a p pastes register a" (assertEqual "one\ntwo\none\n" (textOf pastedNamed))
    , test "p still pastes the default register" (assertEqual "one\ntwo\ntwo\n" (textOf pastedDefault))
    , test "a picked register is for one command" (assertEqual (Just ["o"], Just ["t"]) (reg 'a' oneShot, reg '"' oneShot))
    , test "\" _ d deletes without touching a register" (assertEqual ("one\n", Just ["one\n"], Nothing) (textOf blackHole, reg '"' blackHole, reg '_' blackHole))
    , test "\" + y copies to the clipboard" (assertEqual ("o", Just ["o"]) (clipAfterYank, reg '+' copied))
    , test "space p pastes the clipboard" (assertEqual "xpasted" (textOf pastedClip))
    , test "\" * y copies to the primary selection" (assertEqual ("p", Nothing) (primaryAfter, reg '+' primary))
    , test "R replaces the selection with the register" (assertEqual "one\none\n" (textOf replaced))
    , test "\" a R replaces with register a" (assertEqual "one\ntwo\none\n" (textOf replacedNamed))
    , test "v … space R replaces the selection with the clipboard, back in normal mode" (assertEqual ("CLIPc", Normal) (textOf replacedClip, edMode replacedClip))
    , test "v l y yanks and returns to normal mode" (assertEqual (Just ["ab"], Normal) (reg '"' yankedSelect, edMode yankedSelect))
    , test "C-r a inserts register a" (assertEqual "oone" (textOf inserted))
    , test ":registers lists the registers, reading the clipboard again" (assertEqual (Just [("\"", "a"), ("+", "fresh")]) (infoRows <$> edPopup listed))
    , test ":clear-register a forgets a" (assertEqual [('b', ["a"])] (Map.toList (edRegisters clearedA)))
    , test ":clear-register forgets every register" (assertEqual Map.empty (edRegisters clearedAll))
    , test "\" lists the registers" (assertEqual (Just "registers") (infoTitle <$> edInfo prompting))
    , test "\" q picks q for the next command" (assertEqual (Just 'q') (edSelectedRegister picked))
    , test "several values join one per line" (assertEqual "a\nb\nc\n" (joinValues ["a", "b\n", "c\n"]))
    , test "base64 pads" (assertEqual (map BC.pack ["", "Zg==", "Zm8=", "Zm9v", "Zm9vYg=="]) (map (base64 . BC.pack) ["", "f", "fo", "foo", "foob"]))
    ]
