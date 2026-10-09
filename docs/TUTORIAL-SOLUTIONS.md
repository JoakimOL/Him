# Tutorial solutions: building the editor task by task

*Solutions to the **▶ Task**s in [TUTORIAL.md](TUTORIAL.md). Try each task yourself first.*

The tutorial's tasks are small pieces of a large editor. Here they are solved in order,
and each solution is wired into **one small editor that grows with every task**. After
Task 1 you have a program that prints the bytes you type. After Task 7 you have a modal
editor. It opens and saves files, moves by words, selects, inserts and deletes, and
draws tabs and wide characters correctly. It uses a colour theme and searches with a
live preview.

Every stage below compiles without warnings and passes its tests. The code was written
in exactly this order, and each stage was checked out and built on its own. Between
the tasks there is some glue the tutorial describes but doesn't set as a task: a main
loop, rendering, and the editor state. It is kept as small as possible and marked as
glue.

The mini editor is not him. It keeps the tutorial's names and shapes, so you can compare
it with the real modules. But it stays simple where him is fast: the buffer is a
`Seq Text`, every frame is a full redraw, and the search scans line by line. Part 7 of
the tutorial is about replacing exactly those parts.

| Stage | Task | The editor can now… | Tests |
|---|---|---|---:|
| 1 | Raw input loop | show the bytes each key sends | 0 |
| 2 | Decode keys | show key names, with `~` rows and resizing | 10 |
| 3 | `deleteRange` | display a file | 21 |
| 4 | Word motions | move with `hjkl w e b G`, showing the selection | 30 |
| 5 | Keymaps | edit: modes, chords, `x d c i a`, save and quit | 45 |
| 5b | Invocations | take bindings with arguments: `C-d` = `move_line_down 10` | 52 |
| 6 | Display columns | draw tabs, CJK and control characters correctly | 60 |
| 6b | Colours | use a Helix-style theme, in 24-bit or 256 colours | 68 |
| 7 | Search | search with `/ n N *`, live preview, smart case | 80 |

---

## Setup

Use Part 0's `package.yaml`, plus a test suite. The test suite uses the 50-line harness
from Part 0 (`test/Test/Harness.hs`). This is the final dependency list:

```yaml
library:
  source-dirs: src
  c-sources: [cbits/winsize.c]
  dependencies: [base, bytestring, containers, text, transformers, unix]

executables:
  him:
    main: Main.hs
    source-dirs: app
    ghc-options: [-threaded, -rtsopts]
    dependencies: [base, him, bytestring, text, transformers]

tests:
  spec:
    main: Spec.hs
    source-dirs: test
    dependencies: [base, him, containers, text, transformers]
```

`-Wunused-packages` is on, so at an early stage it will tell you which of these an
executable doesn't use yet. In Stage 1, for example, `Main` uses `unix` directly.

Run with `stack run -- somefile.txt` and test with `stack test`.

---

## Stage 1: Task 1, a raw byte loop

> Write a loop that reads bytes with `fdRead stdInput 64` and prints their values,
> quitting on `q`. Then make the loop throw an exception and check with `stty -a` that
> the terminal is still fine.

`withRawTerminal` comes straight from §1.1. It is the first module of the editor, and
it stays unchanged until the end. `emit` writes and flushes, so each line appears
immediately.

`src/Him/Terminal/Raw.hs`:

```haskell
-- | Raw mode, and always getting out of it again.
module Him.Terminal.Raw
  ( withRawTerminal
  , emit
  ) where

import Control.Exception (bracket)
import Data.ByteString (ByteString)
import Data.ByteString qualified as B
import System.IO (hFlush, stdout)
import System.Posix.IO (stdInput)
import System.Posix.Terminal

makeRaw :: TerminalAttributes -> TerminalAttributes
makeRaw attrs =
  foldl withoutMode attrs
    [ EnableEcho, ProcessInput, KeyboardInterrupts, ExtendedFunctions
    , StartStopOutput, MapCRtoLF, InterruptOnBreak, CheckParity
    , StripHighBit, ProcessOutput ]
    `withBits` 8 `withMinInput` 1 `withTime` 0

withRawTerminal :: IO a -> IO a
withRawTerminal action = bracket enter leave (const action)
  where
    enter = do
      original <- getTerminalAttributes stdInput
      setTerminalAttributes stdInput (makeRaw original) WhenFlushed
      emit "\ESC[?1049h\ESC[2J\ESC[H"
      pure original
    leave original = do
      emit "\ESC[0m\ESC[0 q\ESC[?25h\ESC[?1049l"
      setTerminalAttributes stdInput original WhenFlushed

emit :: ByteString -> IO ()
emit bytes = B.hPut stdout bytes >> hFlush stdout
```

The loop prints `\r\n`, because raw mode switched off output processing: a bare `\n`
would move down without returning to column 0. Pressing `x` throws:

`app/Main.hs`:

```haskell
module Main (main) where

import Data.ByteString qualified as B
import Data.ByteString.Char8 qualified as BC
import Him.Terminal.Raw (emit, withRawTerminal)
import System.Posix.IO (stdInput)
import System.Posix.IO.ByteString (fdRead)

main :: IO ()
main = withRawTerminal loop
  where
    loop = do
      bytes <- fdRead stdInput 64
      emit (BC.pack (show (B.unpack bytes)) <> "\r\n")
      if bytes == "x" then error "boom" else if bytes /= "q" then loop else pure ()
```

**What you see.** Up arrow gives `[27,91,65]`, `Ctrl-a` gives `[1]` and `æ` gives
`[195,166]`. After `x` the program dies with `him: boom`, and the error message is
printed with proper line breaks. That shows `bracket` restored cooked mode before the
exception reached the top. `stty -a` shows `icanon echo` again.

---

## Stage 2: Task 2, decoding keys

> Write `decodeKeys` for ASCII, Enter/Tab/Backspace, `Ctrl-letters`, the four arrow keys
> and a lone ESC.

First the key type from §1.4, with a printer for the status line (the parser comes in
Task 5, when keymaps need it):

`src/Him/Key.hs`:

```haskell
-- | Keys as the editor sees them, independent of terminal encodings.
module Him.Key
  ( Key (..)
  , KeyCode (..)
  , Modifier (..)
  , plain
  , ctrl
  , alt
  , showKey
  ) where

import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as T

data KeyCode
  = KChar Char | KEnter | KEsc | KBackspace | KTab
  | KUp | KDown | KLeft | KRight
  deriving stock (Eq, Ord, Show)

data Modifier = Ctrl | Alt | Shift
  deriving stock (Eq, Ord, Show)

data Key = Key {keyCode :: !KeyCode, keyMods :: !(Set Modifier)}
  deriving stock (Eq, Ord, Show)

plain :: KeyCode -> Key
plain code = Key code Set.empty

ctrl, alt :: Char -> Key
ctrl c = Key (KChar c) (Set.singleton Ctrl)
alt c = Key (KChar c) (Set.singleton Alt)

-- | The readable syntax: @a@, @C-s@, @A-x@, @ret@, @space@.
showKey :: Key -> Text
showKey (Key code mods) = foldMap prefix (Set.toList mods) <> name code
  where
    prefix = \case Ctrl -> "C-"; Alt -> "A-"; Shift -> "S-"
    name = \case
      KChar ' ' -> "space"
      KChar c -> T.singleton c
      KEnter -> "ret"
      KEsc -> "esc"
      KBackspace -> "backspace"
      KTab -> "tab"
      KUp -> "up"
      KDown -> "down"
      KLeft -> "left"
      KRight -> "right"
```

The decoder looks at one byte at a time. For each byte, `decodeOne` gives one of three
answers:
- the keys it starts, and the rest of the input;
- **no** keys, for a sequence it recognises as complete but doesn't know, such as
  `ESC[3~` (Delete) before Delete exists;
- `Nothing`, for a sequence that has only started. This is the case that leaves bytes
  over.

A few things go beyond the task, because the editor needs them right away:
- multi-byte UTF-8, so `æ` can be typed in insert mode;
- `ESC x` as `Alt-x`;
- skipping unknown CSI sequences whole, so pressing Delete doesn't type `[3~`.

`readKeys` is the 30 ms timeout from §1.4.

`src/Him/Terminal/Input.hs`:

```haskell
-- | Turning terminal bytes into keys.
module Him.Terminal.Input
  ( decodeKeys
  , readKeys
  ) where

import Control.Concurrent (threadWaitRead)
import Data.ByteString (ByteString)
import Data.ByteString qualified as B
import Data.Char (chr)
import Data.Maybe (isJust)
import Data.Text qualified as T
import Data.Text.Encoding (decodeUtf8')
import Data.Word (Word8)
import Him.Key
import System.Posix.IO (stdInput)
import System.Posix.IO.ByteString (fdRead)
import System.Timeout (timeout)

-- | Keys, and leftover bytes that start an incomplete sequence.
-- With final = True no more input is coming: a lone ESC is the Esc key.
decodeKeys :: Bool -> ByteString -> ([Key], ByteString)
decodeKeys final = go []
  where
    go acc bs = case B.uncons bs of
      Nothing -> (reverse acc, "")
      Just (b, rest) -> case decodeOne b rest of
        Just (keys, rest') -> go (reverse keys <> acc) rest'
        Nothing
          | final, b == 27 -> go (plain KEsc : acc) rest
          | final -> go (plain (KChar '\xFFFD') : acc) rest -- a cut-off character
          | otherwise -> (reverse acc, bs)

    -- The keys the first byte starts (none for a sequence we don't know),
    -- or Nothing when it starts a sequence that isn't complete yet.
    decodeOne :: Word8 -> ByteString -> Maybe ([Key], ByteString)
    decodeOne b rest
      | b == 27 = escape rest
      | b == 13 = one (plain KEnter) rest
      | b == 9 = one (plain KTab) rest
      | b == 127 || b == 8 = one (plain KBackspace) rest
      | b >= 1 && b <= 26 = one (ctrl (chr (fromIntegral b + 96))) rest
      | b < 128 = one (plain (KChar (chr (fromIntegral b)))) rest
      | otherwise = utf8 b rest

    one key rest = Just ([key], rest)

    escape rest = case B.uncons rest of
      Nothing -> Nothing
      Just (91, more) -> csi more -- ESC [
      Just (c, more) | c >= 32 && c < 127 -> one (alt (chr (fromIntegral c))) more
      Just _ -> one (plain KEsc) rest

    -- ESC [ parameters final: parameters are 0x30..0x3F, the final byte 0x40..0x7E.
    csi more = case B.uncons after of
      Nothing -> Nothing
      Just (end, rest')
        | B.null params, Just code <- lookup end arrows -> one (plain code) rest'
        | otherwise -> Just ([], rest') -- a key we don't know: skip it whole
      where
        (params, after) = B.span (\c -> c >= 0x30 && c <= 0x3F) more

    arrows = [(65, KUp), (66, KDown), (67, KRight), (68, KLeft)]

    -- A lead byte says how long the character is.
    utf8 b rest
      | B.length rest < n = Nothing
      | otherwise = case decodeUtf8' (B.cons b (B.take n rest)) of
          Right t | [c] <- T.unpack t -> one (plain (KChar c)) (B.drop n rest)
          _ -> one (plain (KChar '\xFFFD')) rest
      where
        n | b >= 0xF0 = 3
          | b >= 0xE0 = 2
          | otherwise = 1

-- | Block until there are keys. An incomplete sequence at the end waits
-- 30 ms for the rest before it is decoded as final (a lone Esc).
readKeys :: ByteString -> IO ([Key], ByteString)
readKeys pending = do
  ready <-
    if B.null pending
      then threadWaitRead stdInput >> pure True
      else isJust <$> timeout 30000 (threadWaitRead stdInput)
  bytes <- if ready then (pending <>) <$> fdRead stdInput 64 else pure pending
  case decodeKeys (not ready) bytes of
    ([], rest) -> readKeys rest
    result -> pure result
```

Two details matter:
- **Only incomplete input is left over.** `"\ESC"` is incomplete, so with
  `final = False` it comes back as leftover bytes. With `final = True` it is the Esc
  key. The same applies to the first byte of a cut-off UTF-8 character, which becomes
  U+FFFD only when no more input is coming.
- **`threadWaitRead` comes before `fdRead`.** `timeout` can interrupt the wait but not
  a blocking foreign read (§1.4).

The tests, using the harness from Part 0:

`test/Spec.hs`:

```haskell
module Main (main) where

import Him.Key
import Him.Terminal.Input (decodeKeys)
import Test.Harness

main :: IO ()
main = runTests [inputTests]

inputTests :: Test
inputTests =
  group "decodeKeys"
    [ test "ascii" $ assertEqual ([plain (KChar 'a'), plain (KChar 'Z')], "") (decodeKeys False "aZ")
    , test "enter, tab, backspace" $
        assertEqual ([plain KEnter, plain KTab, plain KBackspace], "") (decodeKeys False "\r\t\DEL")
    , test "ctrl" $ assertEqual ([ctrl 'a', ctrl 's'], "") (decodeKeys False "\SOH\DC3")
    , test "arrows" $
        assertEqual ([plain KUp, plain KDown, plain KRight, plain KLeft], "")
          (decodeKeys False "\ESC[A\ESC[B\ESC[C\ESC[D")
    , test "lone esc waits" $ assertEqual ([], "\ESC") (decodeKeys False "\ESC")
    , test "lone esc, final" $ assertEqual ([plain KEsc], "") (decodeKeys True "\ESC")
    , test "esc then a key is alt" $ assertEqual ([alt 'x'], "") (decodeKeys False "\ESCx")
    , test "unknown sequence is skipped" $
        assertEqual ([plain (KChar 'a')], "") (decodeKeys False "\ESC[3~a")
    , test "utf-8" $ assertEqual ([plain (KChar 'æ')], "") (decodeKeys False "\195\166")
    , test "half a character waits" $ assertEqual ([], "\195") (decodeKeys False "\195")
    ]
```

**Glue: the checkpoint.** The window size goes through the C shim from §1.3 (copy
`cbits/winsize.c` as it is). Here the size is asked for on every draw, which is simpler
than handling `SIGWINCH` and works as well while the screen is redrawn after every key:

`src/Him/Terminal/Size.hs`:

```haskell
-- | The terminal size, through the C shim in @cbits/winsize.c@.
module Him.Terminal.Size
  ( getWindowSize
  ) where

import Foreign.C.Types (CInt (..))
import Foreign.Marshal.Alloc (alloca)
import Foreign.Ptr (Ptr)
import Foreign.Storable (peek)

foreign import ccall unsafe "him_get_winsize"
  c_getWinsize :: Ptr CInt -> Ptr CInt -> IO CInt

-- | @(rows, cols)@, or 80×24 when stdout is not a terminal.
getWindowSize :: IO (Int, Int)
getWindowSize =
  alloca $ \rowsPtr -> alloca $ \colsPtr -> do
    rc <- c_getWinsize rowsPtr colsPtr
    if rc /= 0
      then pure (24, 80)
      else (,) <$> (fromIntegral <$> peek rowsPtr) <*> (fromIntegral <$> peek colsPtr)
```

`app/Main.hs`:

```haskell
module Main (main) where

import Control.Monad (unless)
import Data.ByteString (ByteString)
import Data.ByteString.Char8 qualified as BC
import Data.Text.Encoding (encodeUtf8)
import Him.Key
import Him.Terminal.Input (readKeys)
import Him.Terminal.Raw (emit, withRawTerminal)
import Him.Terminal.Size (getWindowSize)

main :: IO ()
main = withRawTerminal (loop "" "press keys, C-q quits")
  where
    loop pending status = do
      draw status
      (keys, rest) <- readKeys pending
      unless (ctrl 'q' `elem` keys) $
        loop rest (encodeUtf8 (foldMap ((<> " ") . showKey) keys))

-- | A `~` on every row and the status on the last one. The size is asked for
-- on every draw, so resizing just works on the next key.
draw :: ByteString -> IO ()
draw status = do
  (rows, cols) <- getWindowSize
  emit $
    "\ESC[H\ESC[2J"
      <> mconcat (replicate (rows - 1) "~\r\n")
      <> BC.take cols status
```

**Try it.** Press arrows, `Esc`, `Alt-x` and `æ`; the bottom row names them. `C-q`
quits.

---

## Stage 3: Task 3, `deleteRange`

> Implement `deleteRange` for `Seq Text`. Hint: keep the part of the first line before
> the start and the part of the last line after the end, merge them into one line, and
> replace the lines in between.

The whole buffer interface from §2.1. `deleteRange` is the hint, almost word for word:

```haskell
deleteRange p q b@(Buffer ls)
  | from >= to = b
  | otherwise = Buffer (Seq.take l1 ls <> Seq.singleton merged <> Seq.drop (l2 + 1) ls)
  where
    from@(Pos l1 c1) = clampPos b (min p q)
    to@(Pos l2 c2) = clampPos b (max p q)
    merged = T.take c1 (lineAt l1 b) <> T.drop c2 (lineAt l2 b)
```

It works the same when both ends are on one line (`l1 == l2`): the line is replaced by
itself without the middle. Deleting exactly a line end (`(0,3)` to `(1,0)`) joins two
lines.

Ordering the ends and clamping them makes the function total. Callers can pass the ends
in either order, or past the end of the text, and there is still only one place where
the real work happens.

`src/Him/Position.hs`:

```haskell
module Him.Position
  ( Pos (..)
  ) where

-- | A line and a character index in it. Ord is document order.
data Pos = Pos {posLine :: !Int, posCol :: !Int}
  deriving stock (Eq, Ord, Show)
```

`src/Him/Buffer.hs`:

```haskell
-- | The text: lines without their terminators, always at least one.
-- Callers only see these functions, never the 'Seq'.
module Him.Buffer
  ( Buffer
  , fromText
  , toText
  , lineCount
  , lineAt
  , lineLength
  , clampPos
  , insertText
  , deleteRange
  , textRange
  , nextPos
  , prevPos
  , charAt
  ) where

import Data.Foldable (toList)
import Data.Sequence (Seq)
import Data.Sequence qualified as Seq
import Data.Text (Text)
import Data.Text qualified as T
import Him.Position

newtype Buffer = Buffer (Seq Text)
  deriving stock (Eq, Show)

fromText :: Text -> Buffer
fromText = Buffer . Seq.fromList . T.splitOn "\n"

toText :: Buffer -> Text
toText (Buffer ls) = T.intercalate "\n" (toList ls)

lineCount :: Buffer -> Int
lineCount (Buffer ls) = Seq.length ls

lineAt :: Int -> Buffer -> Text
lineAt l (Buffer ls) = Seq.index ls (max 0 (min (Seq.length ls - 1) l))

lineLength :: Int -> Buffer -> Int
lineLength l = T.length . lineAt l

-- | The nearest position that exists. The line end (col == length) exists.
clampPos :: Buffer -> Pos -> Pos
clampPos b (Pos l c) = Pos l' (max 0 (min (lineLength l' b) c))
  where
    l' = max 0 (min (lineCount b - 1) l)

-- | Insert text (which may contain newlines); returns the position after it.
insertText :: Pos -> Text -> Buffer -> (Buffer, Pos)
insertText pos t b@(Buffer ls) =
  (Buffer (Seq.take l ls <> Seq.fromList newLines <> Seq.drop (l + 1) ls), Pos (l + breaks) endCol)
  where
    Pos l c = clampPos b pos
    (before, after) = T.splitAt c (lineAt l b)
    newLines = T.splitOn "\n" (before <> t <> after)
    breaks = T.count "\n" t
    endCol
      | breaks == 0 = c + T.length t
      | otherwise = T.length (T.takeWhileEnd (/= '\n') t)

-- | Delete the half-open range [from, to). Deleting a line end joins lines.
deleteRange :: Pos -> Pos -> Buffer -> Buffer
deleteRange p q b@(Buffer ls)
  | from >= to = b
  | otherwise = Buffer (Seq.take l1 ls <> Seq.singleton merged <> Seq.drop (l2 + 1) ls)
  where
    from@(Pos l1 c1) = clampPos b (min p q)
    to@(Pos l2 c2) = clampPos b (max p q)
    merged = T.take c1 (lineAt l1 b) <> T.drop c2 (lineAt l2 b)

-- | The text of [from, to), with "\n" between lines.
textRange :: Pos -> Pos -> Buffer -> Text
textRange p q b
  | l1 == l2 = T.take (c2 - c1) (T.drop c1 (lineAt l1 b))
  | otherwise =
      T.intercalate "\n" $
        [T.drop c1 (lineAt l1 b)] <> [lineAt l b | l <- [l1 + 1 .. l2 - 1]] <> [T.take c2 (lineAt l2 b)]
  where
    Pos l1 c1 = clampPos b (min p q)
    Pos l2 c2 = clampPos b (max p q)

-- | One character on, crossing line ends; stays put at the end of the text.
nextPos :: Buffer -> Pos -> Pos
nextPos b p = case clampPos b p of
  Pos l c
    | c < lineLength l b -> Pos l (c + 1)
    | l < lineCount b - 1 -> Pos (l + 1) 0
    | otherwise -> Pos l c

prevPos :: Buffer -> Pos -> Pos
prevPos b p = case clampPos b p of
  Pos l c
    | c > 0 -> Pos l (c - 1)
    | l > 0 -> Pos (l - 1) (lineLength (l - 1) b)
    | otherwise -> Pos l c

-- | The character at a position. A line end reads as '\n', except after
-- the last line, where there is nothing.
charAt :: Pos -> Buffer -> Maybe Char
charAt (Pos l c) b
  | l < 0 || l >= lineCount b || c < 0 = Nothing
  | c < T.length line = Just (T.index line c)
  | c == T.length line && l < lineCount b - 1 = Just '\n'
  | otherwise = Nothing
  where
    line = lineAt l b
```

`charAt` reads a line end as `'\n'`. The last line is the exception: the buffer stores
no newline after it, so there is nothing there. Task 4's word motions rely on this.

The tests cover each case in the hint, plus the edges: empty range, ends in reverse
order, deleting everything.

`test/Spec.hs`, changes:

```diff
@@
 module Main (main) where
 
+import Him.Buffer
 import Him.Key
+import Him.Position
 import Him.Terminal.Input (decodeKeys)
 import Test.Harness
 
 main :: IO ()
-main = runTests [inputTests]
+main = runTests [inputTests, bufferTests]
 
 inputTests :: Test
@@ inputTests =
     , test "half a character waits" $ assertEqual ([], "\195") (decodeKeys False "\195")
     ]
+
+bufferTests :: Test
+bufferTests =
+  group "buffer"
+    [ test "insert with newlines" $
+        assertEqual (fromText "abX\nYc", Pos 1 1) (insertText (Pos 0 2) "X\nY" (fromText "abc"))
+    , test "delete inside a line" $ assertEqual (fromText "ad") (deleteRange (Pos 0 1) (Pos 0 3) abcd)
+    , test "delete a line end joins" $
+        assertEqual (fromText "abcdef") (deleteRange (Pos 0 3) (Pos 1 0) (fromText "abc\ndef"))
+    , test "delete across lines" $
+        assertEqual (fromText "aXYZ\nghi") (deleteRange (Pos 0 1) (Pos 2 0) (fromText "abc\ndef\nXYZ\nghi"))
+    , test "delete everything" $ assertEqual (fromText "") (deleteRange (Pos 0 0) (Pos 1 3) (fromText "abc\ndef"))
+    , test "delete is order-insensitive" $ assertEqual (fromText "ad") (deleteRange (Pos 0 3) (Pos 0 1) abcd)
+    , test "empty range" $ assertEqual abcd (deleteRange (Pos 0 2) (Pos 0 2) abcd)
+    , test "text range" $ assertEqual "c\nde" (textRange (Pos 0 2) (Pos 1 2) (fromText "abc\ndef"))
+    , test "char at line end" $ assertEqual (Just '\n', Nothing) (charAt (Pos 0 3) b2, charAt (Pos 1 3) b2)
+    , test "nextPos crosses lines" $ assertEqual (Pos 1 0) (nextPos b2 (Pos 0 3))
+    , test "prevPos crosses lines" $ assertEqual (Pos 0 3) (prevPos b2 (Pos 1 0))
+    ]
+  where
+    abcd = fromText "abcd"
+    b2 = fromText "abc\ndef"
```

**Glue.** `Main` loads the file named on the command line and shows the first
screenful:

`app/Main.hs`:

```haskell
module Main (main) where

import Control.Monad (unless)
import Data.ByteString qualified as B
import Data.Text qualified as T
import Data.Text.Encoding (decodeUtf8Lenient, encodeUtf8)
import Him.Buffer
import Him.Key
import Him.Terminal.Input (readKeys)
import Him.Terminal.Raw (emit, withRawTerminal)
import Him.Terminal.Size (getWindowSize)
import System.Environment (getArgs)

main :: IO ()
main = do
  args <- getArgs
  buffer <- case args of
    [path] -> fromText . decodeUtf8Lenient <$> B.readFile path
    _ -> pure (fromText "")
  withRawTerminal (loop buffer "")
  where
    loop buffer pending = do
      draw buffer
      (keys, rest) <- readKeys pending
      unless (ctrl 'q' `elem` keys) $ loop buffer rest

-- | The first screenful of lines, `~` below the end of the text.
draw :: Buffer -> IO ()
draw buffer = do
  (rows, cols) <- getWindowSize
  let row r
        | r < lineCount buffer = encodeUtf8 (T.take cols (lineAt r buffer))
        | otherwise = "~"
  emit ("\ESC[H\ESC[2J" <> B.intercalate "\r\n" (map row [0 .. rows - 1]))
```

**Try it.** `stack run -- src/Him/Buffer.hs`. A file that ends with a newline shows one
empty line at the end. That's how the buffer models it, and Stage 5 handles it when
saving.

---

## Stage 4: Task 4, word motions

> Write `nextWordEnd` (`e`): from the boundary-stepped start, skip blanks and line ends,
> then extend over one class. Write `prevWordStart` (`b`) as the mirror image using
> `prevPos`.

First the range type from §2.2. The mini editor has one range rather than a
`NonEmpty` list, because multiple cursors are not part of the tasks.

`src/Him/Selection.hs`:

```haskell
module Him.Selection
  ( Range (..)
  , cursor
  , rangeStart
  , rangeEnd
  , rangeContains
  ) where

import Him.Position

-- | Covers every character from anchor to head, both included.
data Range = Range
  { rangeAnchor :: !Pos
  , rangeHead :: !Pos -- where the cursor is drawn
  , rangeWantCol :: !(Maybe Int) -- column to return to on j/k through short lines
  }
  deriving stock (Eq, Show)

-- | A plain cursor: selects the one character under it.
cursor :: Pos -> Range
cursor p = Range p p Nothing

rangeStart, rangeEnd :: Range -> Pos
rangeStart r = min (rangeAnchor r) (rangeHead r)
rangeEnd r = max (rangeAnchor r) (rangeHead r)

rangeContains :: Range -> Pos -> Bool
rangeContains r p = rangeStart r <= p && p <= rangeEnd r
```

Then the motions. `stepOffBoundary`, `extendWhile` and `skipWhile` are the three
combinators §2.3 names. Note that their definitions are not symmetrical:
- `extendWhile` returns the **last** position that matches, the inclusive end of a
  word;
- `skipWhile` returns the **first** position that doesn't match, where the next word
  starts.

With them, `e` is exactly what the task says:

```haskell
blanksThenWord step b r = Range start end Nothing
  where
    start = stepOffBoundary step b (rangeHead r)
    wordStart = skipWhile step (`elem` [Blank, LineEnd]) b start
    end = extendWhile step (== classAt b wordStart) b wordStart
```

`b` is the same function with `prevPos`. The mirror image doesn't need its own code:
`nextWordEnd = blanksThenWord nextPos` and `prevWordStart = blanksThenWord prevPos`.

Check the task's test by hand. Take `"hello world"` with the head at `(0,4)`, the `o`:
1. `o` and the blank after it have different classes, so the start steps off to
   `(0,5)`.
2. Skipping blanks reaches `(0,6)`.
3. Extending over word characters ends at `(0,10)`.

The result is anchor `(0,5)`, head `(0,10)`. The anchor sits on the blank, so the range
covers `" world"`, just like Helix.

`src/Him/Motion.hs`:

```haskell
-- | Motions: pure functions from a range to a range.
module Him.Motion
  ( Motion
  , charLeft
  , charRight
  , lineBy
  , nextWordStart
  , nextWordEnd
  , prevWordStart
  , gotoFileStart
  , gotoFileEnd
  ) where

import Data.Char (isAlphaNum, isSpace)
import Data.Maybe (fromMaybe)
import Him.Buffer
import Him.Position
import Him.Selection

type Motion = Buffer -> Range -> Range

headMotion :: (Buffer -> Pos -> Pos) -> Motion
headMotion f b r = cursor (f b (rangeHead r))

charLeft, charRight :: Motion
charLeft = headMotion prevPos
charRight = headMotion nextPos

-- | Up or down, back to the remembered column where the line is long enough.
lineBy :: Int -> Motion
lineBy delta b r = Range p p (Just want)
  where
    Pos l c = rangeHead r
    want = fromMaybe c (rangeWantCol r)
    l' = max 0 (min (lineCount b - 1) (l + delta))
    p = Pos l' (min want (lineLength l' b))

gotoFileStart, gotoFileEnd :: Motion
gotoFileStart _ _ = cursor (Pos 0 0)
gotoFileEnd b _ = cursor (Pos (lineCount b - 1) 0)

data CharClass = Blank | LineEnd | WordChar | Punct
  deriving stock (Eq)

classAt :: Buffer -> Pos -> CharClass
classAt b p = case charAt p b of
  Nothing -> LineEnd
  Just '\n' -> LineEnd
  Just c
    | isSpace c -> Blank
    | isAlphaNum c || c == '_' -> WordChar
    | otherwise -> Punct

-- | At the last character of a word, start from the next character instead.
-- Without this a second w would select the word it just selected.
stepOffBoundary :: (Buffer -> Pos -> Pos) -> Buffer -> Pos -> Pos
stepOffBoundary step b p
  | p' /= p && classAt b p /= classAt b p' = p'
  | otherwise = p
  where
    p' = step b p

-- | The last position, going this way, whose class satisfies the predicate.
extendWhile :: (Buffer -> Pos -> Pos) -> (CharClass -> Bool) -> Buffer -> Pos -> Pos
extendWhile step ok b = go
  where
    go p
      | p' /= p && ok (classAt b p') = go p'
      | otherwise = p
      where
        p' = step b p

-- | The first position, going this way, whose class does not satisfy it.
skipWhile :: (Buffer -> Pos -> Pos) -> (CharClass -> Bool) -> Buffer -> Pos -> Pos
skipWhile step ok b = go
  where
    go p
      | ok (classAt b p) && p' /= p = go p'
      | otherwise = p
      where
        p' = step b p

-- | w: the next word and the blanks after it.
nextWordStart :: Motion
nextWordStart b r = Range start end Nothing
  where
    start = skipWhile nextPos (== LineEnd) b (stepOffBoundary nextPos b (rangeHead r))
    cls = classAt b start
    wordEnd = extendWhile nextPos (== cls) b start
    end = extendWhile nextPos (== Blank) b wordEnd

-- | e: blanks first, then to the end of the word.
nextWordEnd :: Motion
nextWordEnd = blanksThenWord nextPos

-- | b: the mirror image of e.
prevWordStart :: Motion
prevWordStart = blanksThenWord prevPos

blanksThenWord :: (Buffer -> Pos -> Pos) -> Motion
blanksThenWord step b r = Range start end Nothing
  where
    start = stepOffBoundary step b (rangeHead r)
    wordStart = skipWhile step (`elem` [Blank, LineEnd]) b start
    end = extendWhile step (== classAt b wordStart) b wordStart
```

`lineBy` remembers a *character* column for now. Task 6 changes it to display columns.

`test/Spec.hs`, changes:

```diff
@@ module Main (main) where
 import Him.Buffer
 import Him.Key
+import Him.Motion
 import Him.Position
+import Him.Selection
 import Him.Terminal.Input (decodeKeys)
 import Test.Harness
 
 main :: IO ()
-main = runTests [inputTests, bufferTests]
+main = runTests [inputTests, bufferTests, motionTests]
 
 inputTests :: Test
@@ bufferTests =
     abcd = fromText "abcd"
     b2 = fromText "abc\ndef"
+
+motionTests :: Test
+motionTests =
+  group "motions"
+    [ test "w selects the word and the blanks after it" $
+        assertEqual (Range (Pos 0 0) (Pos 0 5) Nothing) (nextWordStart hw (cursor (Pos 0 0)))
+    , test "w again selects the next word" $
+        assertEqual (Range (Pos 0 6) (Pos 0 10) Nothing) (nextWordStart hw (cursor (Pos 0 5)))
+    , test "e from a word's end" $
+        assertEqual (Range (Pos 0 5) (Pos 0 10) Nothing) (nextWordEnd hw (cursor (Pos 0 4)))
+    , test "e inside a word" $
+        assertEqual (Range (Pos 0 1) (Pos 0 4) Nothing) (nextWordEnd hw (cursor (Pos 0 1)))
+    , test "e crosses lines" $
+        assertEqual (Range (Pos 0 3) (Pos 1 2) Nothing) (nextWordEnd (fromText "abc\ndef") (cursor (Pos 0 2)))
+    , test "b from a word's start" $
+        assertEqual (Range (Pos 0 5) (Pos 0 0) Nothing) (prevWordStart hw (cursor (Pos 0 6)))
+    , test "b inside a word" $
+        assertEqual (Range (Pos 0 8) (Pos 0 6) Nothing) (prevWordStart hw (cursor (Pos 0 8)))
+    , test "punctuation is its own word" $
+        assertEqual (Range (Pos 0 0) (Pos 0 2) Nothing) (nextWordStart (fromText "foo.bar") (cursor (Pos 0 0)))
+    , test "j keeps the column through a short line" $
+        assertEqual (Pos 2 3) (rangeHead (lineBy 1 b3 (lineBy 1 b3 (cursor (Pos 0 3)))))
+    ]
+  where
+    hw = fromText "hello world"
+    b3 = fromText "abcd\nx\nabcd"
```

**Glue: an editor that moves.** The editor state holds the buffer, the range and the
view:

`src/Him/Editor.hs`:

```haskell
-- | Everything the editor knows, and pure functions on it.
module Him.Editor
  ( Editor (..)
  , newEditor
  , motion
  , textRows
  , scrollToCursor
  ) where

import Him.Buffer
import Him.Motion
import Him.Position
import Him.Selection

data Editor = Editor
  { edBuffer :: !Buffer
  , edRange :: !Range
  , edTop :: !Int -- first line on screen
  , edSize :: !(Int, Int) -- terminal rows and columns
  , edQuit :: !Bool
  }

newEditor :: Buffer -> Editor
newEditor b = Editor b (cursor (Pos 0 0)) 0 (24, 80) False

motion :: Motion -> Editor -> Editor
motion m ed = ed {edRange = m (edBuffer ed) (edRange ed)}

-- | Rows for text: all but the status line.
textRows :: Editor -> Int
textRows = subtract 1 . fst . edSize

-- | Scroll just enough to show the cursor.
scrollToCursor :: Editor -> Editor
scrollToCursor ed = ed {edTop = top}
  where
    l = posLine (rangeHead (edRange ed))
    top = max (l - textRows ed + 1) (min l (edTop ed))
```

Rendering is a function from editor state to one `Builder`: the whole screen, written
with one `hPutBuilder`, as §1.2 recommends. Text is plain, the selection is reverse
video, and the status line shows the position. Tabs and control characters are drawn as
one blank for now. Printing them raw would break the layout, and doing better needs
Task 6.

`src/Him/Render.hs`:

```haskell
-- | Drawing the editor: one Builder for the whole screen.
module Him.Render
  ( render
  ) where

import Data.ByteString.Builder (Builder, charUtf8, intDec, stringUtf8)
import Data.Text qualified as T
import Him.Buffer
import Him.Editor
import Him.Position
import Him.Selection

moveCursor :: Int -> Int -> Builder -- 0-based in, 1-based on the wire
moveCursor row col = "\ESC[" <> intDec (row + 1) <> ";" <> intDec (col + 1) <> "H"

render :: Editor -> Builder
render ed =
  "\ESC[?25l" -- hide the cursor while drawing
    <> foldMap textRow [0 .. textRows ed - 1]
    <> moveCursor (textRows ed) 0 <> "\ESC[7m" <> stringUtf8 (T.unpack (T.take cols status)) <> "\ESC[K\ESC[0m"
    <> moveCursor (posLine hd - edTop ed) (min (cols - 1) (posCol hd))
    <> "\ESC[?25h"
  where
    b = edBuffer ed
    r = edRange ed
    hd = rangeHead r
    cols = snd (edSize ed)
    status = T.pack (show (posLine hd + 1) <> ":" <> show (posCol hd + 1))
    textRow row = moveCursor row 0 <> body <> "\ESC[0m\ESC[K"
      where
        l = edTop ed + row
        line = lineAt l b
        body
          | l >= lineCount b = "~"
          | otherwise = cells (zip [0 ..] (take cols (T.unpack line <> " ")))
        -- The extra " " is the line end, visible when it is selected.
        cells = go False
          where
            go _ [] = mempty
            go inSel ((c, ch) : rest) =
              switch <> charUtf8 (shown ch) <> go sel rest
              where
                sel = rangeContains r (Pos l c)
                switch
                  | sel == inSel = mempty
                  | sel = "\ESC[7m"
                  | otherwise = "\ESC[0m"
        -- Until display columns exist (task 6), a tab or control character
        -- would break the layout, so it is drawn as one blank.
        shown ch = if ch < ' ' || ch == '\DEL' then ' ' else ch
```

Keys are matched with a hard-wired `case`. That's exactly what Task 5 removes:

`app/Main.hs`:

```haskell
module Main (main) where

import Data.ByteString qualified as B
import Data.ByteString.Builder (hPutBuilder)
import Data.Text.Encoding (decodeUtf8Lenient)
import Him.Buffer
import Him.Editor
import Him.Key
import Him.Motion
import Him.Render (render)
import Him.Terminal.Input (readKeys)
import Him.Terminal.Raw (withRawTerminal)
import Him.Terminal.Size (getWindowSize)
import System.Environment (getArgs)
import System.IO (hFlush, stdout)

main :: IO ()
main = do
  args <- getArgs
  buffer <- case args of
    [path] -> fromText . decodeUtf8Lenient <$> B.readFile path
    _ -> pure (fromText "")
  withRawTerminal (loop (newEditor buffer) "")
  where
    loop ed0 pending = do
      size <- getWindowSize
      let ed = scrollToCursor ed0 {edSize = size}
      hPutBuilder stdout (render ed) >> hFlush stdout
      (keys, rest) <- readKeys pending
      let ed' = foldl (flip handleKey) ed keys
      if edQuit ed' then pure () else loop ed' rest

-- | Hard-wired for now; task 5 replaces this with keymaps.
handleKey :: Key -> Editor -> Editor
handleKey key ed = case key of
  Key (KChar c) mods | null mods, Just m <- lookup c motions -> motion m ed
  _ | key == ctrl 'q' -> ed {edQuit = True}
  _ -> ed
  where
    motions =
      [ ('h', charLeft), ('l', charRight), ('j', lineBy 1), ('k', lineBy (-1))
      , ('w', nextWordStart), ('e', nextWordEnd), ('b', prevWordStart)
      , ('G', gotoFileEnd) ]
```

**Try it.** Open a file and press `w w w`, `e`, `b`, `j`, `G`. Each `w` highlights the
next word with its trailing blanks.

---

## Stage 5: Task 5, keymaps

> Implement `fromBindings :: [(Text, Text)] -> Either Text Keymap`, where an `insert`
> creates `Prefix` nodes along the way. Then write the four `resolve` tests: single key,
> prefix, chord, unknown.

`fromBindings` needs to read key names, so `Him.Key` gets the inverse of `showKey`:

`src/Him/Key.hs`, changes:

```diff
@@ module Him.Key
   , alt
   , showKey
+  , parseKey
+  , parseKeys
   ) where
 
@@ alt c = Key (KChar c) (Set.singleton Alt)
 -- | The readable syntax: @a@, @C-s@, @A-x@, @ret@, @space@.
 showKey :: Key -> Text
-showKey (Key code mods) = foldMap prefix (Set.toList mods) <> name code
+showKey (Key code mods) = foldMap prefix (Set.toList mods) <> name
   where
     prefix = \case Ctrl -> "C-"; Alt -> "A-"; Shift -> "S-"
-    name = \case
-      KChar ' ' -> "space"
-      KChar c -> T.singleton c
-      KEnter -> "ret"
-      KEsc -> "esc"
-      KBackspace -> "backspace"
-      KTab -> "tab"
-      KUp -> "up"
-      KDown -> "down"
-      KLeft -> "left"
-      KRight -> "right"
+    name = case [n | (n, c) <- namedKeys, c == code] of
+      n : _ -> n
+      [] | KChar c <- code -> T.singleton c
+      [] -> "?"
+
+namedKeys :: [(Text, KeyCode)]
+namedKeys =
+  [ ("space", KChar ' '), ("ret", KEnter), ("esc", KEsc), ("backspace", KBackspace)
+  , ("tab", KTab), ("up", KUp), ("down", KDown), ("left", KLeft), ("right", KRight) ]
+
+-- | The inverse of 'showKey'.
+parseKey :: Text -> Maybe Key
+parseKey t = case T.splitOn "-" t of
+  [m, k] | not (T.null k), Just mods <- traverse modifier (T.unpack m) -> withMods mods <$> parseKey k
+  _ | Just code <- lookup t namedKeys -> Just (plain code)
+  _ | [c] <- T.unpack t -> Just (plain (KChar c))
+  _ -> Nothing
+  where
+    modifier = \case 'C' -> Just Ctrl; 'A' -> Just Alt; 'S' -> Just Shift; _ -> Nothing
+    withMods mods (Key code ms) = Key code (ms <> Set.fromList mods)
+
+-- | Space-separated keys: @"g g"@, @"space w"@.
+parseKeys :: Text -> Maybe [Key]
+parseKeys = traverse parseKey . T.words
```

The keymap is generic in its leaves (`Keymap a`) from the start. §3.6 makes it generic
later. Doing it now costs nothing, and Task 5b can use it without changing this module.

`insert` walks down the keys and creates a `Prefix` node wherever there isn't one yet.
There are two ways a binding can conflict:
- binding `g` as a command when `g g` exists;
- binding `g g` when `g` is a command.

Both are errors, because one of the two bindings could never be reached.

`src/Him/Keymap.hs`:

```haskell
-- | Keymaps: tries from key sequences to whatever a binding holds.
module Him.Keymap
  ( Keymap
  , Resolved (..)
  , emptyKeymap
  , fromBindings
  , unionKeymap
  , resolve
  ) where

import Control.Monad (foldM)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Text (Text)
import Him.Key

newtype Keymap a = Keymap (Map Key (Node a))
  deriving stock (Show, Functor, Foldable, Traversable)

data Node a = Leaf a | Prefix (Keymap a)
  deriving stock (Show, Functor, Foldable, Traversable)

data Resolved a = Found a | NeedMore | NoMatch
  deriving stock (Eq, Show)

emptyKeymap :: Keymap a
emptyKeymap = Keymap Map.empty

-- | Build a keymap from @("g g", x)@ pairs. A later pair for the same keys
-- wins; a key that is both a command and a prefix is an error.
fromBindings :: [(Text, a)] -> Either Text (Keymap a)
fromBindings = foldM add emptyKeymap
  where
    add km (keysText, v) = case parseKeys keysText of
      Just keys@(_ : _) -> insert keysText keys v km
      _ -> Left ("not a key sequence: " <> keysText)

insert :: Text -> [Key] -> a -> Keymap a -> Either Text (Keymap a)
insert keysText keys v (Keymap m) = case keys of
  [] -> Left ("empty key sequence: " <> keysText)
  [k] -> case Map.lookup k m of
    Just (Prefix _) -> conflict
    _ -> Right (Keymap (Map.insert k (Leaf v) m))
  k : ks -> case Map.lookup k m of
    Nothing -> under k ks emptyKeymap
    Just (Prefix sub) -> under k ks sub
    Just (Leaf _) -> conflict
  where
    conflict = Left ("both a command and a prefix: " <> keysText)
    under k ks sub = (\sub' -> Keymap (Map.insert k (Prefix sub') m)) <$> insert keysText ks v sub

-- | Left-biased; prefixes present in both are merged.
unionKeymap :: Keymap a -> Keymap a -> Keymap a
unionKeymap (Keymap a) (Keymap b) = Keymap (Map.unionWith merge a b)
  where
    merge (Prefix x) (Prefix y) = Prefix (unionKeymap x y)
    merge x _ = x

resolve :: Keymap a -> [Key] -> Resolved a
resolve _ [] = NeedMore
resolve (Keymap m) (k : ks) = case Map.lookup k m of
  Nothing -> NoMatch
  Just (Leaf a)
    | null ks -> Found a
    | otherwise -> NoMatch
  Just (Prefix sub)
    | null ks -> NeedMore
    | otherwise -> resolve sub ks
```

The tests are the four from the task, plus a conflict, plus the check from §3.2 that
the default keymaps are valid:

```haskell
    [ test "single key" $ assertEqual (Found "left") (resolve km [k "h"])
    , test "prefix" $ assertEqual NeedMore (resolve km [k "g"])
    , test "chord" $ assertEqual (Found "top") (resolve km [k "g", k "g"])
    , test "unknown" $ assertEqual NoMatch (resolve km [k "q"])
```

**Glue: the editor from Part 3.** With keymaps in place, the rest of §2.4, §2.5 and
Part 3 follows, all in small pieces.

The edits from §2.4. One of them is `deleteSelection`, which deletes the newline
*before* the range when `x d` is used on the last line:

`src/Him/Edit.hs`:

```haskell
-- | Edits: pure functions that change the buffer at a range.
module Him.Edit
  ( Edit
  , insertAtHead
  , deleteBackward
  , insertNewline
  , deleteSelection
  ) where

import Data.Text (Text)
import Data.Text qualified as T
import Him.Buffer
import Him.Position
import Him.Selection

type Edit = Buffer -> Range -> (Buffer, Range)

-- | In insert mode the head is a gap: text goes before the character there.
insertAtHead :: Text -> Edit
insertAtHead t b r = (b', cursor p)
  where
    (b', p) = insertText (rangeHead r) t b

deleteBackward :: Edit
deleteBackward b r = (deleteRange p (rangeHead r) b, cursor p)
  where
    p = prevPos b (rangeHead r)

-- | A new line with the current line's indentation.
insertNewline :: Edit
insertNewline b r = insertAtHead ("\n" <> indent) b r
  where
    indent = T.takeWhile (`elem` [' ', '\t']) (lineAt (posLine (rangeHead r)) b)

-- | Delete every character the range covers. The last line has no newline
-- after it, so when the range runs to the end of the text from a line start,
-- the newline before it goes too: x d on the last line leaves no empty line.
deleteSelection :: Edit
deleteSelection b r
  | to == lastPos && posCol from == 0 && posLine from > 0 =
      (deleteRange (prevPos b from) to b, cursor (Pos (posLine from - 1) 0))
  | otherwise = (deleteRange from to b, cursor from)
  where
    from = rangeStart r
    to = nextPos b (rangeEnd r)
    lastLine = lineCount b - 1
    lastPos = Pos lastLine (lineLength lastLine b)
```

`x` (`extendLine`), as §2.3 describes it:

`src/Him/Motion.hs`, changes:

```diff
@@ module Him.Motion
   , gotoFileStart
   , gotoFileEnd
+  , extendLine
   ) where
 
@@ gotoFileStart _ _ = cursor (Pos 0 0)
 gotoFileEnd b _ = cursor (Pos (lineCount b - 1) 0)
 
+-- | x: the whole lines the range touches, newline included; one more line
+-- when it already covers whole lines.
+extendLine :: Motion
+extendLine b r
+  | rangeStart r == rangeStart whole && rangeEnd r == rangeEnd whole && end < lineCount b - 1 =
+      lineRange start (end + 1)
+  | otherwise = whole
+  where
+    start = posLine (rangeStart r)
+    end = posLine (rangeEnd r)
+    whole = lineRange start end
+    lineRange from to = Range (Pos from 0) (Pos to (lineLength to b)) Nothing
+
 data CharClass = Blank | LineEnd | WordChar | Punct
   deriving stock (Eq)
```

Loading and saving keep track of the final newline, so a file is saved with exactly
the ending it had:

`src/Him/File.hs`:

```haskell
-- | Loading and saving, keeping a final newline if the file had one.
module Him.File
  ( loadFile
  , saveFile
  ) where

import Control.Exception (IOException, catch)
import Data.ByteString qualified as B
import Data.Text qualified as T
import Data.Text.Encoding (decodeUtf8Lenient, encodeUtf8)
import Him.Buffer

-- | The buffer, and whether the file ended with a newline. A file that
-- doesn't exist yet is empty.
loadFile :: FilePath -> IO (Buffer, Bool)
loadFile path = (decode <$> B.readFile path) `catch` \(_ :: IOException) -> pure (fromText "", True)
  where
    decode bytes = case T.unsnoc (decodeUtf8Lenient bytes) of
      Just (t, '\n') -> (fromText t, True)
      _ -> (fromText (decodeUtf8Lenient bytes), False)

saveFile :: FilePath -> Bool -> Buffer -> IO ()
saveFile path trailingNewline b =
  B.writeFile path (encodeUtf8 (toText b <> if trailingNewline then "\n" else ""))
```

The editor gets modes, pending keys and a file. Select mode is just "a motion keeps
the anchor":

`src/Him/Editor.hs`:

```haskell
-- | Everything the editor knows, and pure functions on it.
module Him.Editor
  ( Editor (..)
  , Mode (..)
  , newEditor
  , applyMotion
  , applyEdit
  , textRows
  , scrollToCursor
  ) where

import Data.Text (Text)
import Him.Buffer
import Him.Edit
import Him.Key
import Him.Motion
import Him.Position
import Him.Selection

data Mode = Normal | Insert | Select
  deriving stock (Eq, Ord, Show)

data Editor = Editor
  { edBuffer :: !Buffer
  , edRange :: !Range
  , edMode :: !Mode
  , edPending :: ![Key] -- e.g. the "g" of "g g"
  , edPath :: !(Maybe FilePath)
  , edTrailingNewline :: !Bool
  , edDirty :: !Bool
  , edStatus :: !(Maybe Text) -- a message, until the next key
  , edTop :: !Int -- first line on screen
  , edSize :: !(Int, Int) -- terminal rows and columns
  , edQuit :: !Bool
  }

newEditor :: Maybe FilePath -> (Buffer, Bool) -> Editor
newEditor path (b, trailing) =
  Editor b (cursor (Pos 0 0)) Normal [] path trailing False Nothing 0 (24, 80) False

-- | In select mode a motion keeps the anchor, so the selection grows.
applyMotion :: Motion -> Editor -> Editor
applyMotion m ed = ed {edRange = moved}
  where
    r = edRange ed
    r' = m (edBuffer ed) r
    moved
      | edMode ed == Select = r' {rangeAnchor = rangeAnchor r}
      | otherwise = r'

applyEdit :: Edit -> Editor -> Editor
applyEdit e ed = ed {edBuffer = b, edRange = r, edDirty = True}
  where
    (b, r) = e (edBuffer ed) (edRange ed)

-- | Rows for text: all but the status line.
textRows :: Editor -> Int
textRows = subtract 1 . fst . edSize

-- | Scroll just enough to show the cursor.
scrollToCursor :: Editor -> Editor
scrollToCursor ed = ed {edTop = top}
  where
    l = posLine (rangeHead (edRange ed))
    top = max (l - textRows ed + 1) (min l (edTop ed))
```

The commands, the bindings and `handleKey` from §3.2–3.4. Keys map to *names*. The
startup check rejects a keymap that names a command that doesn't exist. Then
`handleKey` looks the name up in the registry, and an unbound key falls back to
inserting itself in insert mode.

`src/Him/Commands.hs`:

```haskell
-- | Named commands, the default keymaps, and what a key press does.
module Him.Commands
  ( EditorM
  , Command (..)
  , Keymaps
  , registry
  , defaultKeymaps
  , handleKey
  ) where

import Control.Exception (IOException, try)
import Control.Monad (when)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.State.Strict (StateT, get, gets, modify)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as T
import Him.Buffer
import Him.Edit
import Him.Editor
import Him.File (saveFile)
import Him.Key
import Him.Keymap
import Him.Motion
import Him.Position
import Him.Selection

type EditorM = StateT Editor IO

data Command = Command
  { cmdName :: !Text -- snake_case: "move_next_word_start"
  , cmdDoc :: !Text
  , cmdRun :: EditorM ()
  }

motion :: Motion -> EditorM ()
motion m = modify (applyMotion m)

edit :: Edit -> EditorM ()
edit e = modify (applyEdit e)

setMode :: Mode -> EditorM ()
setMode m = modify $ \ed -> ed {edMode = m}

status :: Text -> EditorM ()
status msg = modify $ \ed -> ed {edStatus = Just msg}

-- | Put the cursor (the insert gap) somewhere and enter insert mode.
insertAt :: (Buffer -> Range -> Pos) -> EditorM ()
insertAt place = do
  modify $ \ed -> ed {edRange = cursor (place (edBuffer ed) (edRange ed))}
  setMode Insert

write :: EditorM ()
write = do
  ed <- get
  case edPath ed of
    Nothing -> status "no file name"
    Just path -> do
      result <- liftIO (try (saveFile path (edTrailingNewline ed) (edBuffer ed)))
      case result of
        Left (e :: IOException) -> status (T.pack (show e))
        Right () -> do
          modify $ \ed' -> ed' {edDirty = False}
          status ("wrote " <> T.pack path)

quit :: EditorM ()
quit = do
  dirty <- gets edDirty
  if dirty
    then status "unsaved changes (space w saves, space Q quits anyway)"
    else modify $ \ed -> ed {edQuit = True}

commands :: [Command]
commands =
  [ Command "move_char_left" "Move left" (motion charLeft)
  , Command "move_char_right" "Move right" (motion charRight)
  , Command "move_line_down" "Move down" (motion (lineBy 1))
  , Command "move_line_up" "Move up" (motion (lineBy (-1)))
  , Command "move_next_word_start" "Select to the start of the next word" (motion nextWordStart)
  , Command "move_next_word_end" "Select to the end of the next word" (motion nextWordEnd)
  , Command "move_prev_word_start" "Select to the start of the previous word" (motion prevWordStart)
  , Command "goto_file_start" "Go to the first line" (motion gotoFileStart)
  , Command "goto_file_end" "Go to the last line" (motion gotoFileEnd)
  , Command "extend_line" "Select whole lines" (motion extendLine)
  , Command "normal_mode" "Back to normal mode" (setMode Normal)
  , Command "select_mode" "Motions extend the selection" (setMode Select)
  , Command "insert_mode" "Insert before the selection" (insertAt (\_ r -> rangeStart r))
  , Command "append_mode" "Insert after the selection" (insertAt (\b r -> nextPos b (rangeEnd r)))
  , Command "delete_selection" "Delete the selection" (edit deleteSelection >> setMode Normal)
  , Command "change_selection" "Delete the selection, then insert" (edit deleteSelection >> setMode Insert)
  , Command "delete_char_backward" "Delete the character before the cursor" (edit deleteBackward)
  , Command "insert_newline" "Break the line, keeping the indentation" (edit insertNewline)
  , Command "write" "Save the file" write
  , Command "quit" "Quit, unless there are unsaved changes" quit
  , Command "quit_force" "Quit, discarding changes" (modify $ \ed -> ed {edQuit = True})
  ]

registry :: Map Text Command
registry = Map.fromList [(cmdName c, c) | c <- commands]

normalBindings, selectBindings, insertBindings :: [(Text, Text)]
normalBindings =
  [ ("h", "move_char_left"), ("l", "move_char_right"), ("j", "move_line_down"), ("k", "move_line_up")
  , ("left", "move_char_left"), ("right", "move_char_right"), ("down", "move_line_down"), ("up", "move_line_up")
  , ("w", "move_next_word_start"), ("e", "move_next_word_end"), ("b", "move_prev_word_start")
  , ("g g", "goto_file_start"), ("g e", "goto_file_end"), ("x", "extend_line")
  , ("v", "select_mode"), ("i", "insert_mode"), ("a", "append_mode")
  , ("d", "delete_selection"), ("c", "change_selection")
  , ("space w", "write"), ("C-s", "write"), ("space q", "quit"), ("space Q", "quit_force")
  ]
selectBindings = [("esc", "normal_mode"), ("v", "normal_mode")]
insertBindings =
  [ ("esc", "normal_mode"), ("backspace", "delete_char_backward"), ("ret", "insert_newline")
  , ("left", "move_char_left"), ("right", "move_char_right"), ("down", "move_line_down"), ("up", "move_line_up")
  ]

type Keymaps = Map Mode (Keymap Text)

-- | The keymaps, or what is wrong with them: a bad key, or a name that
-- isn't a command. Checked once, before the editor starts.
defaultKeymaps :: Either Text Keymaps
defaultKeymaps = do
  normal <- fromBindings normalBindings
  select <- fromBindings selectBindings
  insert <- fromBindings insertBindings
  let kms = Map.fromList [(Normal, normal), (Select, unionKeymap select normal), (Insert, insert)]
      unknown = [name | km <- Map.elems kms, name <- foldr (:) [] km, not (Map.member name registry)]
  if null unknown
    then Right kms
    else Left ("keymap refers to unknown commands: " <> T.intercalate ", " unknown)

handleKey :: Keymaps -> Key -> EditorM ()
handleKey kms key = do
  ed <- get
  modify $ \e -> e {edStatus = Nothing}
  let keys = edPending ed <> [key]
  case resolve (Map.findWithDefault emptyKeymap (edMode ed) kms) keys of
    NeedMore -> setPending keys
    Found name -> setPending [] >> mapM_ cmdRun (Map.lookup name registry)
    NoMatch -> do
      setPending []
      when (null (edPending ed)) (fallback (edMode ed) key)
  where
    setPending ks = modify $ \e -> e {edPending = ks}

-- | What an unbound key does: in insert mode a character inserts itself.
fallback :: Mode -> Key -> EditorM ()
fallback Insert (Key (KChar c) mods) | Set.null mods = edit (insertAtHead (T.singleton c))
fallback _ _ = pure ()
```

The status line shows the mode, the file, `[+]` when there are unsaved changes, pending
keys (`g`, `space`) and messages. The cursor is a bar in insert mode:

`src/Him/Render.hs`, changes:

```diff
@@ module Him.Render
 
 import Data.ByteString.Builder (Builder, charUtf8, intDec, stringUtf8)
+import Data.Maybe (fromMaybe)
 import Data.Text qualified as T
 import Him.Buffer
 import Him.Editor
+import Him.Key (showKey)
 import Him.Position
 import Him.Selection
@@ render ed =
     <> moveCursor (textRows ed) 0 <> "\ESC[7m" <> stringUtf8 (T.unpack (T.take cols status)) <> "\ESC[K\ESC[0m"
     <> moveCursor (posLine hd - edTop ed) (min (cols - 1) (posCol hd))
+    <> (if edMode ed == Insert then "\ESC[6 q" else "\ESC[2 q") -- bar or block
     <> "\ESC[?25h"
   where
@@ render ed =
     hd = rangeHead r
     cols = snd (edSize ed)
-    status = T.pack (show (posLine hd + 1) <> ":" <> show (posCol hd + 1))
+    status =
+      T.unwords $
+        [ case edMode ed of Normal -> " NOR"; Insert -> " INS"; Select -> " SEL"
+        , T.pack (fromMaybe "[scratch]" (edPath ed)) <> (if edDirty ed then " [+]" else "")
+        , T.pack (show (posLine hd + 1) <> ":" <> show (posCol hd + 1))
+        , T.unwords (map showKey (edPending ed))
+        , fromMaybe "" (edStatus ed)
+        ]
     textRow row = moveCursor row 0 <> body <> "\ESC[0m\ESC[K"
       where
```

`Main` now refuses to start with a broken keymap, and runs every key through
`handleKey`:

`app/Main.hs`:

```haskell
module Main (main) where

import Control.Monad.Trans.State.Strict (execStateT)
import Data.ByteString.Builder (hPutBuilder)
import Data.Text qualified as T
import Him.Buffer (fromText)
import Him.Commands
import Him.Editor
import Him.File (loadFile)
import Him.Render (render)
import Him.Terminal.Input (readKeys)
import Him.Terminal.Raw (withRawTerminal)
import Him.Terminal.Size (getWindowSize)
import System.Environment (getArgs)
import System.Exit (die)
import System.IO (hFlush, stdout)

main :: IO ()
main = do
  kms <- either (die . T.unpack) pure defaultKeymaps
  args <- getArgs
  ed <- case args of
    [path] -> newEditor (Just path) <$> loadFile path
    _ -> pure (newEditor Nothing (fromText "", True))
  withRawTerminal (loop kms ed "")
  where
    loop kms ed0 pending = do
      size <- getWindowSize
      let ed = scrollToCursor ed0 {edSize = size}
      hPutBuilder stdout (render ed) >> hFlush stdout
      (keys, rest) <- readKeys pending
      ed' <- execStateT (mapM_ (handleKey kms) keys) ed
      if edQuit ed' then pure () else loop kms ed' rest
```

Since `handleKey` is an ordinary function, the editing tests type key names, as in
§3.4. They need no terminal:

`test/Spec.hs`, changes:

```diff
@@
 module Main (main) where
 
+import Control.Monad.Trans.State.Strict (execStateT)
+import Data.Maybe (fromMaybe)
+import Data.Text (Text)
 import Him.Buffer
+import Him.Commands
+import Him.Editor
 import Him.Key
+import Him.Keymap
 import Him.Motion
 import Him.Position
@@ import Test.Harness
 
 main :: IO ()
-main = runTests [inputTests, bufferTests, motionTests]
+main = do
+  editing <- editingTests
+  runTests [inputTests, bufferTests, motionTests, keymapTests, editing]
 
 inputTests :: Test
@@ motionTests =
     hw = fromText "hello world"
     b3 = fromText "abcd\nx\nabcd"
+
+keymapTests :: Test
+keymapTests =
+  group "keymap"
+    [ test "single key" $ assertEqual (Found "left") (resolve km [k "h"])
+    , test "prefix" $ assertEqual NeedMore (resolve km [k "g"])
+    , test "chord" $ assertEqual (Found "top") (resolve km [k "g", k "g"])
+    , test "unknown" $ assertEqual NoMatch (resolve km [k "q"])
+    , test "unknown after a prefix" $ assertEqual NoMatch (resolve km [k "g", k "q"])
+    , test "a command can't also be a prefix" $
+        assertEqual (Left "both a command and a prefix: g g") (() <$ fromBindings [("g", "x"), ("g g", "y" :: Text)])
+    , test "the default keymaps are valid" $ assertEqual (Right ()) (() <$ defaultKeymaps)
+    ]
+  where
+    km = either (error . show) id (fromBindings [("h", "left"), ("g g", "top"), ("g e", "bottom" :: Text)])
+    k = fromMaybe (error "bad key") . parseKey
+
+-- | The text after typing keys at the start of a buffer.
+textAfter :: Text -> Text -> IO Text
+textAfter start keys = do
+  let kms = either (error . show) id defaultKeymaps
+      ks = fromMaybe (error "bad keys") (parseKeys keys)
+  ed <- execStateT (mapM_ (handleKey kms) ks) (newEditor Nothing (fromText start, False))
+  pure (toText (edBuffer ed))
+
+editingTests :: IO Test
+editingTests = do
+  typed <- textAfter "" "i h i space t h e r e esc"
+  deleted <- textAfter "hello world" "w d"
+  lines' <- textAfter "a\nb\nc" "x d"
+  lastLine <- textAfter "a\nb" "j x d"
+  changed <- textAfter "foo bar" "e c b a z esc"
+  chord <- textAfter "a\nb\nc" "g e g g x d"
+  indented <- textAfter "  foo" "g e x a ret b a r esc"
+  backspace <- textAfter "abc" "e a backspace backspace esc"
+  pure $
+    group "editing"
+      [ test "insert" $ assertEqual "hi there" typed
+      , test "w d" $ assertEqual "world" deleted
+      , test "x d" $ assertEqual "b\nc" lines'
+      , test "x d on the last line" $ assertEqual "a" lastLine
+      , test "c" $ assertEqual "baz bar" changed
+      , test "chords" $ assertEqual "b\nc" chord
+      , test "newline keeps the indentation" $ assertEqual "  foo\n  bar" indented
+      , test "backspace" $ assertEqual "a" backspace
+      ]
```

**Try it.** `x d` deletes a line. `w c` changes a word, and `Esc` ends the change. `v`
followed by motions extends the selection. `g g` and `g e` jump to the first and last
line. `space w` saves, `space q` quits, and `space Q` quits without saving.

---

## Stage 5b: Task 5b, invocations

> Write `parseInvocation :: Text -> Either Text Invocation` (words, plus double-quoted
> strings with `\"` and `\\`) and its inverse, and test that they round-trip.

The parser reads words, or quoted strings, separated by any amount of space. The printer
quotes a word only when it has to: if it is empty, or contains a blank, a quote or a
backslash. Every other word reads back as itself unquoted. That is why the round trip
holds.

`src/Him/Invocation.hs`:

```haskell
-- | An action and its arguments, as text: @move_line_down 20@,
-- @insert_text "// "@.
module Him.Invocation
  ( Invocation (..)
  , parseInvocation
  , renderInvocation
  ) where

import Data.Char (isSpace)
import Data.Text (Text)
import Data.Text qualified as T

data Invocation = Invocation {invName :: !Text, invArgs :: ![Text]}
  deriving stock (Eq, Show)

-- | Words, or double-quoted strings in which @\\"@ and @\\\\@ are escapes.
parseInvocation :: Text -> Either Text Invocation
parseInvocation input = do
  ws <- go (T.stripStart input)
  case ws of
    [] -> Left "empty binding"
    name : args -> Right (Invocation name args)
  where
    go t
      | T.null t = Right []
      | Just rest <- T.stripPrefix "\"" t = do
          (word, rest') <- quoted "" rest
          (word :) <$> go (T.stripStart rest')
      | otherwise =
          let (word, rest) = T.break isSpace t
           in (word :) <$> go (T.stripStart rest)
    quoted acc t = case T.uncons t of
      Nothing -> Left ("unterminated string in: " <> input)
      Just ('"', rest) -> Right (T.pack (reverse acc), rest)
      Just ('\\', rest) | Just (c, rest') <- T.uncons rest, c `elem` ['"', '\\'] -> quoted (c : acc) rest'
      Just (c, rest) -> quoted (c : acc) rest

-- | The inverse: a word that wouldn't read back as itself is quoted.
renderInvocation :: Invocation -> Text
renderInvocation (Invocation name args) = T.unwords (map word (name : args))
  where
    word w
      | T.null w || T.any (\c -> isSpace c || c == '"' || c == '\\') w =
          "\"" <> T.concatMap escape w <> "\""
      | otherwise = w
    escape c
      | c `elem` ['"', '\\'] = T.pack ['\\', c]
      | otherwise = T.singleton c
```

**Glue: actions (§3.6).** Bindings become invocations, like `move_line_down 10`, and they
are bound **once, at startup**. `bindAll` parses every pair, looks the action up and
checks its arguments. The keymap's leaves are then `EditorM ()` values ready to run, so
a key press does no lookup and no parsing.

Every bad binding is reported at once, with its mode and key, because errors are
collected with `partitionEithers` instead of stopping at the first. A count is an
optional first argument. The tutorial's `ArgSpec` generalises this. Three small
helpers, `plainAction`, `countAction` and `textAction`, cover what this editor needs.

`src/Him/Commands.hs`, changes:

```diff
@@
--- | Named commands, the default keymaps, and what a key press does.
+-- | Actions, the default keymaps, and what a key press does.
 module Him.Commands
   ( EditorM
-  , Command (..)
+  , Action (..)
   , Keymaps
   , registry
+  , bindAll
   , defaultKeymaps
   , handleKey
@@ module Him.Commands
 import Control.Exception (IOException, try)
 import Control.Monad (when)
+import Data.Either (partitionEithers)
 import Control.Monad.IO.Class (liftIO)
 import Control.Monad.Trans.State.Strict (StateT, get, gets, modify)
@@ import Data.Set qualified as Set
 import Data.Text (Text)
 import Data.Text qualified as T
+import Text.Read (readMaybe)
 import Him.Buffer
 import Him.Edit
 import Him.Editor
 import Him.File (saveFile)
+import Him.Invocation
 import Him.Key
 import Him.Keymap
@@ import Him.Selection
 type EditorM = StateT Editor IO
 
-data Command = Command
-  { cmdName :: !Text -- snake_case: "move_next_word_start"
-  , cmdDoc :: !Text
-  , cmdRun :: EditorM ()
+-- | A stable name, and how to turn text arguments into something to run.
+-- The arguments are checked once, when a key is bound, not on every press.
+data Action = Action
+  { actName :: !Text -- snake_case: "move_next_word_start"
+  , actDoc :: !Text
+  , actBind :: [Text] -> Either Text (EditorM ())
   }
 
+-- | No arguments.
+plainAction :: Text -> Text -> EditorM () -> Action
+plainAction name doc run = Action name doc $ \case
+  [] -> Right run
+  _ -> Left (name <> ": takes no arguments")
+
+-- | An optional count, 1 if left out: @move_line_down 20@.
+countAction :: Text -> Text -> (Int -> EditorM ()) -> Action
+countAction name doc run = Action name doc $ \case
+  [] -> Right (run 1)
+  [n] | Just count <- readMaybe (T.unpack n), count > 0 -> Right (run count)
+  [n] -> Left (name <> ": not a count: " <> n)
+  _ -> Left (name <> ": takes at most one argument <count>")
+
+-- | One text argument: @insert_text "// "@.
+textAction :: Text -> Text -> (Text -> EditorM ()) -> Action
+textAction name doc run = Action name doc $ \case
+  [t] -> Right (run t)
+  [] -> Left (name <> ": missing argument <text>")
+  _ -> Left (name <> ": takes one argument <text>")
+
 motion :: Motion -> EditorM ()
 motion m = modify (applyMotion m)
@@ quit = do
     else modify $ \ed -> ed {edQuit = True}
 
-commands :: [Command]
-commands =
-  [ Command "move_char_left" "Move left" (motion charLeft)
-  , Command "move_char_right" "Move right" (motion charRight)
-  , Command "move_line_down" "Move down" (motion (lineBy 1))
-  , Command "move_line_up" "Move up" (motion (lineBy (-1)))
-  , Command "move_next_word_start" "Select to the start of the next word" (motion nextWordStart)
-  , Command "move_next_word_end" "Select to the end of the next word" (motion nextWordEnd)
-  , Command "move_prev_word_start" "Select to the start of the previous word" (motion prevWordStart)
-  , Command "goto_file_start" "Go to the first line" (motion gotoFileStart)
-  , Command "goto_file_end" "Go to the last line" (motion gotoFileEnd)
-  , Command "extend_line" "Select whole lines" (motion extendLine)
-  , Command "normal_mode" "Back to normal mode" (setMode Normal)
-  , Command "select_mode" "Motions extend the selection" (setMode Select)
-  , Command "insert_mode" "Insert before the selection" (insertAt (\_ r -> rangeStart r))
-  , Command "append_mode" "Insert after the selection" (insertAt (\b r -> nextPos b (rangeEnd r)))
-  , Command "delete_selection" "Delete the selection" (edit deleteSelection >> setMode Normal)
-  , Command "change_selection" "Delete the selection, then insert" (edit deleteSelection >> setMode Insert)
-  , Command "delete_char_backward" "Delete the character before the cursor" (edit deleteBackward)
-  , Command "insert_newline" "Break the line, keeping the indentation" (edit insertNewline)
-  , Command "write" "Save the file" write
-  , Command "quit" "Quit, unless there are unsaved changes" quit
-  , Command "quit_force" "Quit, discarding changes" (modify $ \ed -> ed {edQuit = True})
+actions :: [Action]
+actions =
+  [ countAction "move_char_left" "Move left" (motion . repeated charLeft)
+  , countAction "move_char_right" "Move right" (motion . repeated charRight)
+  , countAction "move_line_down" "Move down" (motion . lineBy)
+  , countAction "move_line_up" "Move up" (motion . lineBy . negate)
+  , countAction "move_next_word_start" "Select to the start of the next word" (motion . repeated nextWordStart)
+  , countAction "move_next_word_end" "Select to the end of the next word" (motion . repeated nextWordEnd)
+  , countAction "move_prev_word_start" "Select to the start of the previous word" (motion . repeated prevWordStart)
+  , plainAction "goto_file_start" "Go to the first line" (motion gotoFileStart)
+  , plainAction "goto_file_end" "Go to the last line" (motion gotoFileEnd)
+  , plainAction "extend_line" "Select whole lines" (motion extendLine)
+  , plainAction "normal_mode" "Back to normal mode" (setMode Normal)
+  , plainAction "select_mode" "Motions extend the selection" (setMode Select)
+  , plainAction "insert_mode" "Insert before the selection" (insertAt (\_ r -> rangeStart r))
+  , plainAction "append_mode" "Insert after the selection" (insertAt (\b r -> nextPos b (rangeEnd r)))
+  , plainAction "delete_selection" "Delete the selection" (edit deleteSelection >> setMode Normal)
+  , plainAction "change_selection" "Delete the selection, then insert" (edit deleteSelection >> setMode Insert)
+  , plainAction "delete_char_backward" "Delete the character before the cursor" (edit deleteBackward)
+  , plainAction "insert_newline" "Break the line, keeping the indentation" (edit insertNewline)
+  , textAction "insert_text" "Insert text before the selection" $ \t -> do
+      modify $ \ed -> ed {edRange = cursor (rangeStart (edRange ed))}
+      edit (insertAtHead t)
+  , plainAction "write" "Save the file" write
+  , plainAction "quit" "Quit, unless there are unsaved changes" quit
+  , plainAction "quit_force" "Quit, discarding changes" (modify $ \ed -> ed {edQuit = True})
+  , plainAction "no_op" "Do nothing (switches a key off)" (pure ())
   ]
 
-registry :: Map Text Command
-registry = Map.fromList [(cmdName c, c) | c <- commands]
+-- | A motion done n times.
+repeated :: Motion -> Int -> Motion
+repeated m n b r = iterate (m b) r !! n
+
+registry :: Map Text Action
+registry = Map.fromList [(actName a, a) | a <- actions]
 
 normalBindings, selectBindings, insertBindings :: [(Text, Text)]
@@ normalBindings =
   , ("v", "select_mode"), ("i", "insert_mode"), ("a", "append_mode")
   , ("d", "delete_selection"), ("c", "change_selection")
+  , ("C-d", "move_line_down 10"), ("C-u", "move_line_up 10"), ("space c", "insert_text \"// \"")
   , ("space w", "write"), ("C-s", "write"), ("space q", "quit"), ("space Q", "quit_force")
   ]
@@ insertBindings =
   ]
 
-type Keymaps = Map Mode (Keymap Text)
+-- | Keymaps whose leaves are ready to run: no lookup or parsing on a key press.
+type Keymaps = Map Mode (Keymap (EditorM ()))
 
--- | The keymaps, or what is wrong with them: a bad key, or a name that
--- isn't a command. Checked once, before the editor starts.
+-- | Parse and bind every pair; report every bad one, not just the first.
+bindAll :: Text -> [(Text, Text)] -> Either [Text] [(Text, EditorM ())]
+bindAll modeName pairs = case partitionEithers (map bind pairs) of
+  ([], bound) -> Right bound
+  (errors, _) -> Left errors
+  where
+    bind (keys, binding) = either (\e -> Left (modeName <> " mode, " <> keys <> ": " <> e)) (Right . (keys,)) $ do
+      inv <- parseInvocation binding
+      action <- maybe (Left ("unknown action: " <> invName inv)) Right (Map.lookup (invName inv) registry)
+      actBind action (invArgs inv)
+
+-- | The keymaps, or everything that is wrong with them. Checked once,
+-- before the editor starts.
 defaultKeymaps :: Either Text Keymaps
 defaultKeymaps = do
-  normal <- fromBindings normalBindings
-  select <- fromBindings selectBindings
-  insert <- fromBindings insertBindings
-  let kms = Map.fromList [(Normal, normal), (Select, unionKeymap select normal), (Insert, insert)]
-      unknown = [name | km <- Map.elems kms, name <- foldr (:) [] km, not (Map.member name registry)]
-  if null unknown
-    then Right kms
-    else Left ("keymap refers to unknown commands: " <> T.intercalate ", " unknown)
+  normal <- keymap "Normal" normalBindings
+  select <- keymap "Select" selectBindings
+  insert <- keymap "Insert" insertBindings
+  pure (Map.fromList [(Normal, normal), (Select, unionKeymap select normal), (Insert, insert)])
+  where
+    keymap modeName pairs = either (Left . T.unlines) fromBindings (bindAll modeName pairs)
 
 handleKey :: Keymaps -> Key -> EditorM ()
@@ handleKey kms key = do
   case resolve (Map.findWithDefault emptyKeymap (edMode ed) kms) keys of
     NeedMore -> setPending keys
-    Found name -> setPending [] >> mapM_ cmdRun (Map.lookup name registry)
+    Found run -> setPending [] >> run
     NoMatch -> do
       setPending []
```

`test/Spec.hs`, changes:

```diff
@@ import Him.Buffer
 import Him.Commands
 import Him.Editor
+import Him.Invocation
 import Him.Key
 import Him.Keymap
@@ main :: IO ()
 main = do
   editing <- editingTests
-  runTests [inputTests, bufferTests, motionTests, keymapTests, editing]
+  runTests [inputTests, bufferTests, motionTests, keymapTests, invocationTests, editing]
 
 inputTests :: Test
@@ editingTests = do
   lastLine <- textAfter "a\nb" "j x d"
   changed <- textAfter "foo bar" "e c b a z esc"
+  counted <- textAfter "a\nb\nc\nd" "C-d x d"
+  comment <- textAfter "foo" "space c"
   chord <- textAfter "a\nb\nc" "g e g g x d"
   indented <- textAfter "  foo" "g e x a ret b a r esc"
@@ editingTests = do
       , test "x d on the last line" $ assertEqual "a" lastLine
       , test "c" $ assertEqual "baz bar" changed
+      , test "a binding with a count" $ assertEqual "a\nb\nc" counted
+      , test "a binding with text" $ assertEqual "// foo" comment
       , test "chords" $ assertEqual "b\nc" chord
       , test "newline keeps the indentation" $ assertEqual "  foo\n  bar" indented
       , test "backspace" $ assertEqual "a" backspace
       ]
+
+invocationTests :: Test
+invocationTests =
+  group "invocation"
+    [ test "words" $ assertEqual (Right (Invocation "move_line_down" ["20"])) (parseInvocation "move_line_down  20")
+    , test "quoted" $
+        assertEqual (Right (Invocation "insert_text" ["say \"hi\" \\o/"])) (parseInvocation "insert_text \"say \\\"hi\\\" \\\\o/\"")
+    , test "unterminated" $ assertEqual True (either (const True) (const False) (parseInvocation "insert_text \"oops"))
+    , test "round trip" $
+        mapM_ (\i -> assertEqual (Right i) (parseInvocation (renderInvocation i)))
+          [ Invocation "a" [], Invocation "a" ["", " ", "\"", "\\", "x y", "plain"], Invocation "insert_text" ["// "] ]
+    , test "every bad binding is reported" $
+        assertEqual (Left ["Normal mode, a: unknown action: fly", "Normal mode, b: move_line_down: not a count: x"])
+          (map fst <$> bindAll "Normal" [("a", "fly"), ("b", "move_line_down x"), ("c", "write")])
+    ]
```

**Try it.** `C-d` and `C-u` move 10 lines. `space c` inserts `// ` at the cursor. Put a
typo in a binding, such as `("C-d", "move_line_down x")`. The editor refuses to start
and says `Normal mode, C-d: move_line_down: not a count: x`.

---

## Stage 6: Task 6, display columns

> Write `displayCol :: Text -> Int -> Int` and its inverse
> `charIndexAtCol :: Text -> Int -> Int`.

Both functions use `layoutLine`, which gives every character its column and width.
`charWidth` takes the column too, because a tab's width depends on where it starts.
The other cases are as in §4.2: control characters take 2 cells (`^[`), wide characters
take 2, and the ASCII test comes first because it is by far the most common case.

`charIndexAtCol` finds the character whose cells *cover* the column. Column 3 of
`"漢字x"` is the second cell of `字`, so the answer is 1. Past the end of the line
both functions continue with one column per index. That way the line end, where the
cursor can be, has a column too.

`isWide` here is the linear scan of ranges that §7.3 found to be 19% of render time.
That's fine at this size; the profiler can find it later.

`src/Him/TextWidth.hs`:

```haskell
-- | Screen columns: a tab runs to the next multiple of 4, CJK and emoji take
-- two cells, a control character is shown as ^X in two cells.
module Him.TextWidth
  ( tabWidth
  , charWidth
  , layoutLine
  , displayCol
  , charIndexAtCol
  ) where

import Data.Char (isControl, ord)
import Data.Text (Text)
import Data.Text qualified as T

tabWidth :: Int
tabWidth = 4

-- | Cells a character takes at a display column.
charWidth :: Int -> Char -> Int
charWidth col c
  | c >= ' ' && c < '\DEL' = 1 -- the common case first
  | c == '\t' = tabWidth - col `mod` tabWidth
  | isControl c = 2
  | isWide c = 2
  | otherwise = 1

-- | East Asian wide and fullwidth characters, and emoji.
isWide :: Char -> Bool
isWide c = any (\(lo, hi) -> n >= lo && n <= hi) wide
  where
    n = ord c
    wide =
      [ (0x1100, 0x115F), (0x2E80, 0x303E), (0x3041, 0x33FF), (0x3400, 0x4DBF)
      , (0x4E00, 0x9FFF), (0xA000, 0xA4CF), (0xAC00, 0xD7A3), (0xF900, 0xFAFF)
      , (0xFE30, 0xFE4F), (0xFF00, 0xFF60), (0xFFE0, 0xFFE6), (0x1F300, 0x1F64F)
      , (0x1F900, 0x1F9FF), (0x20000, 0x3FFFD) ]

-- | (character index, display column, width, character) for each character.
layoutLine :: Text -> [(Int, Int, Int, Char)]
layoutLine = go 0 0 . T.unpack
  where
    go _ _ [] = []
    go i col (c : cs) = (i, col, w, c) : go (i + 1) (col + w) cs
      where
        w = charWidth col c

-- | The column where character i starts. Past the end, one column per index.
displayCol :: Text -> Int -> Int
displayCol line i = case drop i (layoutLine line) of
  (_, col, _, _) : _ -> col
  [] -> lineWidth + (i - T.length line)
  where
    lineWidth = sum [w | (_, _, w, _) <- layoutLine line]

-- | The character covering a column; past the end, the line end.
charIndexAtCol :: Text -> Int -> Int
charIndexAtCol line col = case [i | (i, start, w, _) <- layoutLine line, col < start + w] of
  i : _ -> i
  [] -> T.length line
```

**Glue.** Two callers change:
- `lineBy` remembers a *display* column, so `j` stays visually aligned across tabs.
- The renderer lays out each line with `layoutLine`:
  - a tab becomes spaces;
  - a control character is drawn as `^X`;
  - a wide character that doesn't fit at the screen edge is drawn as blanks, never as
    half a character;
  - the cursor goes to the display column.

`src/Him/Motion.hs`, changes:

```diff
@@ import Him.Buffer
 import Him.Position
 import Him.Selection
+import Him.TextWidth (charIndexAtCol, displayCol)
 
 type Motion = Buffer -> Range -> Range
@@ charRight = headMotion nextPos
 
 -- | Up or down, back to the remembered column where the line is long enough.
+-- The column is a display column, so j stays visually aligned across tabs.
 lineBy :: Int -> Motion
 lineBy delta b r = Range p p (Just want)
   where
     Pos l c = rangeHead r
-    want = fromMaybe c (rangeWantCol r)
+    want = fromMaybe (displayCol (lineAt l b) c) (rangeWantCol r)
     l' = max 0 (min (lineCount b - 1) (l + delta))
-    p = Pos l' (min want (lineLength l' b))
+    p = Pos l' (charIndexAtCol (lineAt l' b) want)
 
 gotoFileStart, gotoFileEnd :: Motion
```

`src/Him/Render.hs`, changes:

```diff
@@ module Him.Render
 
 import Data.ByteString.Builder (Builder, charUtf8, intDec, stringUtf8)
+import Data.Char (chr, isControl, ord)
 import Data.Maybe (fromMaybe)
 import Data.Text qualified as T
@@ import Him.Key (showKey)
 import Him.Position
 import Him.Selection
+import Him.TextWidth
 
 moveCursor :: Int -> Int -> Builder -- 0-based in, 1-based on the wire
@@ render ed =
     <> foldMap textRow [0 .. textRows ed - 1]
     <> moveCursor (textRows ed) 0 <> "\ESC[7m" <> stringUtf8 (T.unpack (T.take cols status)) <> "\ESC[K\ESC[0m"
-    <> moveCursor (posLine hd - edTop ed) (min (cols - 1) (posCol hd))
+    <> moveCursor (posLine hd - edTop ed) (min (cols - 1) (displayCol (lineAt (posLine hd) b) (posCol hd)))
     <> (if edMode ed == Insert then "\ESC[6 q" else "\ESC[2 q") -- bar or block
     <> "\ESC[?25h"
@@ render ed =
         body
           | l >= lineCount b = "~"
-          | otherwise = cells (zip [0 ..] (take cols (T.unpack line <> " ")))
-        -- The extra " " is the line end, visible when it is selected.
-        cells = go False
+          | otherwise = cells False (takeWhile onScreen (layoutLine line <> [lineEnd]))
+        -- The line end, visible when it is selected.
+        lineEnd = (T.length line, displayCol line (T.length line), 1, ' ')
+        onScreen (_, col, _, _) = col < cols
+        cells _ [] = mempty
+        cells inSel ((i, col, w, ch) : rest) = switch <> glyph <> cells sel rest
           where
-            go _ [] = mempty
-            go inSel ((c, ch) : rest) =
-              switch <> charUtf8 (shown ch) <> go sel rest
-              where
-                sel = rangeContains r (Pos l c)
-                switch
-                  | sel == inSel = mempty
-                  | sel = "\ESC[7m"
-                  | otherwise = "\ESC[0m"
-        -- Until display columns exist (task 6), a tab or control character
-        -- would break the layout, so it is drawn as one blank.
-        shown ch = if ch < ' ' || ch == '\DEL' then ' ' else ch
+            sel = rangeContains r (Pos l i)
+            switch
+              | sel == inSel = mempty
+              | sel = "\ESC[7m"
+              | otherwise = "\ESC[0m"
+            glyph
+              | col + w > cols = spaces (cols - col) -- never half a wide character
+              | ch == '\t' = spaces w
+              | ch == '\DEL' = "^?"
+              | isControl ch = charUtf8 '^' <> charUtf8 (chr (ord ch + 64))
+              | otherwise = charUtf8 ch
+        spaces n = stringUtf8 (replicate n ' ')
```

`test/Spec.hs`, changes:

```diff
@@ import Him.Position
 import Him.Selection
 import Him.Terminal.Input (decodeKeys)
+import Him.TextWidth
 import Test.Harness
 
@@ main :: IO ()
 main = do
   editing <- editingTests
-  runTests [inputTests, bufferTests, motionTests, keymapTests, invocationTests, editing]
+  runTests [inputTests, bufferTests, motionTests, keymapTests, invocationTests, widthTests, editing]
 
 inputTests :: Test
@@ invocationTests =
           (map fst <$> bindAll "Normal" [("a", "fly"), ("b", "move_line_down x"), ("c", "write")])
     ]
+
+widthTests :: Test
+widthTests =
+  group "display columns"
+    [ test "a tab runs to the next stop" $ assertEqual 4 (displayCol "\tx" 1)
+    , test "wide characters" $ assertEqual 4 (displayCol "漢字x" 2)
+    , test "a column inside a wide character" $ assertEqual 1 (charIndexAtCol "漢字x" 3)
+    , test "a column inside a tab" $ assertEqual 1 (charIndexAtCol "a\tb" 2)
+    , test "past the end" $ assertEqual (3, 5) (charIndexAtCol "abc" 9, displayCol "abc" 5)
+    , test "control characters take two cells" $ assertEqual 3 (displayCol "\ESCx" 2)
+    , test "inverse" $
+        mapM_ (\i -> assertEqual i (charIndexAtCol line (displayCol line i))) [0 .. 6]
+    , test "j through a tab keeps the screen column" $
+        assertEqual (Pos 1 1) (rangeHead (lineBy 1 (fromText "abcde\n\tx") (cursor (Pos 0 4))))
+    ]
+  where
+    line = "a\t漢\ESCbc"
```

The `inverse` test is the property worth keeping. For every character,
`charIndexAtCol line (displayCol line i) == i`, on a line that mixes all the cases.

**Try it.** Open a file with tabs and CJK text. Move with `j` and `k` through a line
that has a tab.

---

## Stage 6b: Task 6b, colours

> Write `parseColor :: Map Text Text -> Text -> Maybe Color`, handling palette names,
> `#rrggbb`, `#rgb` and the 16 named colours. Then map an RGB colour to the nearest
> xterm-256 colour.

`parseColor` checks the palette first, as Helix does, so a theme can redefine `red`. A
palette value is parsed with an empty palette, which means one entry can't refer to
another. So there are no loops to guard against. `#rgb` doubles each digit (`#f0a` is
`#ff00aa`).

`nearest256` follows the task. Each channel picks the nearest of the cube's six levels
`0, 95, 135, 175, 215, 255`. The grey ramp has 24 steps `8, 18, …, 238`, and the step
nearest the average is picked. Whichever of the two candidates is closer in RGB space
wins. Mid-grey `#808080` lands on the ramp (244), not on the cube's `#878787`.

`src/Him/Theme.hs`:

```haskell
-- | Colours written the way Helix themes write them, and the editor's theme.
module Him.Theme
  ( Color (..)
  , Style (..)
  , Theme (..)
  , parseColor
  , nearest256
  , builtinTheme
  , sgr
  ) where

import Data.ByteString.Builder (Builder, intDec)
import Data.Char (isHexDigit)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Text qualified as T
import Numeric (readHex)

data Color = Default | Ansi Int | Rgb Int Int Int
  deriving stock (Eq, Show)

data Style = Style {styleFg, styleBg :: !Color}
  deriving stock (Eq, Show)

data Theme = Theme {themeText, themeSelection, themeStatus :: !Style}
  deriving stock (Eq, Show)

-- | A palette name, @#rrggbb@, @#rgb@, or one of the 16 terminal colours.
parseColor :: Map Text Text -> Text -> Maybe Color
parseColor palette t
  | Just v <- Map.lookup t palette = parseColor Map.empty v -- a palette entry can't name another
  | Just hex <- T.stripPrefix "#" t, T.all isHexDigit hex = case T.unpack hex of
      [r1, r2, g1, g2, b1, b2] -> Just (Rgb (byte [r1, r2]) (byte [g1, g2]) (byte [b1, b2]))
      [r, g, b] -> Just (Rgb (byte [r, r]) (byte [g, g]) (byte [b, b]))
      _ -> Nothing
  | otherwise = lookup t named
  where
    byte digits = case readHex digits of
      [(n, "")] -> n
      _ -> 0
    named =
      [ ("default", Default), ("black", Ansi 0), ("red", Ansi 1), ("green", Ansi 2), ("yellow", Ansi 3)
      , ("blue", Ansi 4), ("magenta", Ansi 5), ("cyan", Ansi 6), ("light-gray", Ansi 7), ("gray", Ansi 8)
      , ("light-red", Ansi 9), ("light-green", Ansi 10), ("light-yellow", Ansi 11), ("light-blue", Ansi 12)
      , ("light-magenta", Ansi 13), ("light-cyan", Ansi 14), ("white", Ansi 15) ]

-- | The closest xterm-256 colour: the nearest level of the 6×6×6 cube in
-- each channel, or the nearest step of the grey ramp, whichever is closer.
nearest256 :: Int -> Int -> Int -> Int
nearest256 r g b
  | dist cubeRgb <= dist greyRgb = 16 + 36 * ri + 6 * gi + bi
  | otherwise = 232 + grey
  where
    levels = [0, 95, 135, 175, 215, 255]
    level v = snd (minimum [(abs (v - l), i) | (i, l) <- zip [0 ..] levels])
    (ri, gi, bi) = (level r, level g, level b)
    cubeRgb = (levels !! ri, levels !! gi, levels !! bi)
    grey = max 0 (min 23 (((r + g + b) `div` 3 - 8 + 5) `div` 10)) -- steps 8, 18, .., 238
    greyRgb = let v = 8 + 10 * grey in (v, v, v)
    dist (r', g', b') = sq (r - r') + sq (g - g') + sq (b - b')
    sq x = x * x

-- | The built-in theme, in Helix's terms: scopes, and a palette. Without
-- 24-bit colour in the terminal, every RGB colour is mapped to the palette.
builtinTheme :: Bool -> Theme
builtinTheme trueColor =
  Theme (style "ui.text") (style "ui.selection") (style "ui.statusline")
  where
    palette = Map.fromList [("fg", "#abb2bf"), ("bg-selection", "#3e4452"), ("bg-status", "#2c323c")]
    scopes :: [(Text, (Text, Text))]
    scopes =
      [ ("ui.text", ("fg", "default"))
      , ("ui.selection", ("default", "bg-selection"))
      , ("ui.statusline", ("fg", "bg-status")) ]
    style scope = case lookup scope scopes of
      Just (fg, bg) -> Style (color fg) (color bg)
      Nothing -> Style Default Default
    color = downsample . fromMaybe Default . parseColor palette
    downsample = \case
      Rgb r g b | not trueColor -> Ansi (nearest256 r g b)
      c -> c

-- | Reset, then set both colours.
sgr :: Style -> Builder
sgr (Style fg bg) = "\ESC[0;" <> color 38 39 fg <> ";" <> color 48 49 bg <> "m"
  where
    color :: Int -> Int -> Color -> Builder
    color set def = \case
      Default -> intDec def
      Ansi n -> intDec set <> ";5;" <> intDec n
      Rgb r g b -> intDec set <> ";2;" <> intDec r <> ";" <> intDec g <> ";" <> intDec b
```

**Glue.** The built-in theme is written in Helix's terms: scopes (`ui.selection`) and a
palette. That's a small step towards reading real theme files (§4.4). Without
`COLORTERM=truecolor` in the environment, `nearest256` maps every RGB colour to the 256
palette. The renderer takes a `Theme` and writes `sgr` styles instead of reverse video:

`src/Him/Render.hs`, changes:

```diff
@@ import Him.Position
 import Him.Selection
 import Him.TextWidth
+import Him.Theme
 
 moveCursor :: Int -> Int -> Builder -- 0-based in, 1-based on the wire
 moveCursor row col = "\ESC[" <> intDec (row + 1) <> ";" <> intDec (col + 1) <> "H"
 
-render :: Editor -> Builder
-render ed =
+render :: Theme -> Editor -> Builder
+render theme ed =
   "\ESC[?25l" -- hide the cursor while drawing
     <> foldMap textRow [0 .. textRows ed - 1]
-    <> moveCursor (textRows ed) 0 <> "\ESC[7m" <> stringUtf8 (T.unpack (T.take cols status)) <> "\ESC[K\ESC[0m"
+    <> moveCursor (textRows ed) 0 <> sgr (themeStatus theme) <> stringUtf8 (T.unpack (T.take cols status)) <> "\ESC[K\ESC[0m"
     <> moveCursor (posLine hd - edTop ed) (min (cols - 1) (displayCol (lineAt (posLine hd) b) (posCol hd)))
     <> (if edMode ed == Insert then "\ESC[6 q" else "\ESC[2 q") -- bar or block
@@ render ed =
         , fromMaybe "" (edStatus ed)
         ]
-    textRow row = moveCursor row 0 <> body <> "\ESC[0m\ESC[K"
+    textRow row = moveCursor row 0 <> sgr (themeText theme) <> body <> "\ESC[0m\ESC[K"
       where
         l = edTop ed + row
@@ render ed =
             switch
               | sel == inSel = mempty
-              | sel = "\ESC[7m"
-              | otherwise = "\ESC[0m"
+              | sel = sgr (themeSelection theme)
+              | otherwise = sgr (themeText theme)
             glyph
               | col + w > cols = spaces (cols - col) -- never half a wide character
```

`app/Main.hs`, changes:

```diff
@@ import Him.Terminal.Input (readKeys)
 import Him.Terminal.Raw (withRawTerminal)
 import Him.Terminal.Size (getWindowSize)
-import System.Environment (getArgs)
+import Him.Theme (builtinTheme)
+import System.Environment (getArgs, lookupEnv)
 import System.Exit (die)
 import System.IO (hFlush, stdout)
@@ main = do
     [path] -> newEditor (Just path) <$> loadFile path
     _ -> pure (newEditor Nothing (fromText "", True))
-  withRawTerminal (loop kms ed "")
+  colorterm <- lookupEnv "COLORTERM"
+  let theme = builtinTheme (colorterm `elem` [Just "truecolor", Just "24bit"])
+  withRawTerminal (loop theme kms ed "")
   where
-    loop kms ed0 pending = do
+    loop theme kms ed0 pending = do
       size <- getWindowSize
       let ed = scrollToCursor ed0 {edSize = size}
-      hPutBuilder stdout (render ed) >> hFlush stdout
+      hPutBuilder stdout (render theme ed) >> hFlush stdout
       (keys, rest) <- readKeys pending
       ed' <- execStateT (mapM_ (handleKey kms) keys) ed
-      if edQuit ed' then pure () else loop kms ed' rest
+      if edQuit ed' then pure () else loop theme kms ed' rest
```

`test/Spec.hs`, changes:

```diff
@@ import Him.Selection
 import Him.Terminal.Input (decodeKeys)
 import Him.TextWidth
+import Him.Theme
+import Data.Map.Strict qualified as Map
 import Test.Harness
 
@@ main :: IO ()
 main = do
   editing <- editingTests
-  runTests [inputTests, bufferTests, motionTests, keymapTests, invocationTests, widthTests, editing]
+  runTests [inputTests, bufferTests, motionTests, keymapTests, invocationTests, widthTests, colorTests, editing]
 
 inputTests :: Test
@@ widthTests =
   where
     line = "a\t漢\ESCbc"
+
+colorTests :: Test
+colorTests =
+  group "colours"
+    [ test "palette name" $ assertEqual (Just (Rgb 0x5c 0x63 0x70)) (parseColor palette "grey")
+    , test "#rrggbb" $ assertEqual (Just (Rgb 0x3e 0x44 0x52)) (parseColor palette "#3e4452")
+    , test "#rgb" $ assertEqual (Just (Rgb 0xff 0x00 0xaa)) (parseColor palette "#f0a")
+    , test "named" $ assertEqual (Just (Ansi 1), Just (Ansi 15)) (parseColor palette "red", parseColor palette "white")
+    , test "invalid" $ assertEqual [Nothing, Nothing, Nothing] (map (parseColor palette) ["#12", "#zzzzzz", "mauve"])
+    , test "cube" $ assertEqual [16, 196, 67, 231] [nearest256 0 0 0, nearest256 255 0 0, nearest256 95 135 175, nearest256 255 255 255]
+    , test "grey ramp" $ assertEqual [244, 232] [nearest256 128 128 128, nearest256 10 10 10]
+    , test "built-in theme without true colour" $
+        assertEqual (Style (Ansi 249) (Ansi 236)) (themeStatus (builtinTheme False))
+    ]
+  where
+    palette = Map.fromList [("grey", "#5c6370")]
```

**Try it.** Run it with and without `COLORTERM=truecolor`. The selection and status
line look nearly the same in both modes, and that is the point of the mapping.

---

## Stage 7: Task 7, search

> Write a naive reference search … Use it in a randomized test against `findMatch` for
> both directions.

The randomized test needs a `findMatch` to test, so here is one. It searches line by
line with `T.breakOn`, in the order the cursor would go:
1. the rest of the current line;
2. the other lines, wrapping around the end of the buffer;
3. the part of the current line before the cursor, last.

A match exactly at the cursor is found last, which is why `n` moves on.

Smart case folds with `T.map toLower`, one character at a time, and never with
`T.toLower`. `T.toLower` can change the length of the text (`İ` becomes two
characters), and then match columns would point at the wrong characters.

`src/Him/Search.hs`:

```haskell
-- | Finding text, smart case, wrapping around the end of the buffer.
module Him.Search
  ( Direction (..)
  , Needle
  , needleLength
  , compileNeedle
  , findMatch
  ) where

import Data.Char (isUpper, toLower)
import Data.Maybe (listToMaybe, mapMaybe)
import Data.Text (Text)
import Data.Text qualified as T
import Him.Buffer
import Him.Position

data Direction = Forward | Backward
  deriving stock (Eq, Show)

data Needle = Needle {needleText :: !Text, needleFold :: !Bool}

needleLength :: Needle -> Int
needleLength = T.length . needleText

-- | Smart case: without upper-case letters the search ignores case.
-- Folding goes character by character, so columns stay where they are.
compileNeedle :: Text -> Maybe Needle
compileNeedle t
  | T.null t || T.any (== '\n') t = Nothing
  | T.any isUpper t = Just (Needle t False)
  | otherwise = Just (Needle (T.map toLower t) True)

-- | The start of the nearest match strictly after (or before) a position,
-- wrapping around; a match at the position itself is found last.
findMatch :: Direction -> Needle -> Buffer -> Pos -> Maybe Pos
findMatch dir needle b (Pos l c) = listToMaybe (mapMaybe try order)
  where
    n = lineCount b
    others = case dir of
      Forward -> [l + 1 .. n - 1] <> [0 .. l - 1]
      Backward -> [l - 1, l - 2 .. 0] <> [n - 1, n - 2 .. l + 1]
    order = [(l, \col -> after col c)] <> [(o, const True) | o <- others] <> [(l, \col -> not (after col c))]
    after col ref = if dir == Forward then col > ref else col < ref
    try (line, ok) = Pos line <$> pick (filter ok (matchesIn (lineAt line b)))
    pick = if dir == Forward then listToMaybe else listToMaybe . reverse
    matchesIn line = go 0 (fold line)
      where
        go from hay = case T.breakOn (needleText needle) hay of
          (before, rest)
            | T.null rest -> []
            | otherwise -> let col = from + T.length before in col : go (col + 1) (T.drop 1 rest)
    fold = if needleFold needle then T.map toLower else id
```

The test has three parts:
- **The reference.** `naiveFind` lists every match by trying every column of every
  line. Then it picks the first match after the position, or wraps around.
- **The randomness.** The test generates random cases with splitmix64, which is a few
  lines of code: `random` is not a boot package. The alphabet `abAB \n` is small on
  purpose, so matches, overlaps and case differences come up often.
- **The cases.** The seed is fixed, so a failure is reproducible. It is reported with
  the buffer, needle, position and direction, ready to paste into a unit test.

`test/Spec.hs`, changes:

```diff
@@
 module Main (main) where
 
-import Control.Monad.Trans.State.Strict (execStateT)
-import Data.Maybe (fromMaybe)
+import Control.Monad.Trans.State.Strict (State, evalState, execStateT, state)
+import Data.Bits (shiftR, xor)
+import Data.Char (isUpper, toLower)
+import Data.List (find)
+import Data.Word (Word64)
+import Control.Applicative ((<|>))
+import Data.Maybe (fromMaybe, listToMaybe)
 import Data.Text (Text)
 import Him.Buffer
@@ import Him.Keymap
 import Him.Motion
 import Him.Position
+import Him.Search
 import Him.Selection
 import Him.Terminal.Input (decodeKeys)
@@ import Him.TextWidth
 import Him.Theme
 import Data.Map.Strict qualified as Map
+import Data.Text qualified as T
 import Test.Harness
 
@@ main :: IO ()
 main = do
   editing <- editingTests
-  runTests [inputTests, bufferTests, motionTests, keymapTests, invocationTests, widthTests, colorTests, editing]
+  runTests [inputTests, bufferTests, motionTests, keymapTests, invocationTests, widthTests, colorTests, searchTests, editing]
 
 inputTests :: Test
@@ editingTests = do
   counted <- textAfter "a\nb\nc\nd" "C-d x d"
   comment <- textAfter "foo" "space c"
+  searched <- textAfter "hello world" "/ w o r ret d"
+  wrapped <- textAfter "ab ab ab" "/ a b ret n n d"
+  backwards <- textAfter "ab ab ab" "/ a b ret n N d"
+  cancelled <- textAfter "hello" "/ l l esc d"
+  smartCase <- textAfter "Foo foo" "/ F o o ret d"
+  star <- textAfter "ab ab ab" "e * n d"
   chord <- textAfter "a\nb\nc" "g e g g x d"
   indented <- textAfter "  foo" "g e x a ret b a r esc"
@@ editingTests = do
       , test "a binding with a count" $ assertEqual "a\nb\nc" counted
       , test "a binding with text" $ assertEqual "// foo" comment
+      , test "search selects the match" $ assertEqual "hello ld" searched
+      , test "n wraps around" $ assertEqual " ab ab" wrapped
+      , test "N goes back" $ assertEqual "ab  ab" backwards
+      , test "esc goes back to where the search started" $ assertEqual "ello" cancelled
+      , test "an upper-case letter makes the case count" $ assertEqual " foo" smartCase
+      , test "* searches for the selection" $ assertEqual "ab  ab" star
       , test "chords" $ assertEqual "b\nc" chord
       , test "newline keeps the indentation" $ assertEqual "  foo\n  bar" indented
@@ colorTests =
   where
     palette = Map.fromList [("grey", "#5c6370")]
+
+-- | Every match, the slow and obvious way.
+naiveFind :: Direction -> Text -> Buffer -> Pos -> Maybe Pos
+naiveFind dir needle b p = case dir of
+  Forward -> find (> p) matches <|> listToMaybe matches
+  Backward -> find (< p) (reverse matches) <|> listToMaybe (reverse matches)
+  where
+    fold = if T.any isUpper needle then id else T.map toLower
+    matches =
+      [ Pos l c
+      | l <- [0 .. lineCount b - 1]
+      , let line = fold (lineAt l b)
+      , c <- [0 .. T.length line]
+      , fold needle `T.isPrefixOf` T.drop c line
+      ]
+
+-- | splitmix64: enough randomness for tests, without a package.
+type Gen = State Word64
+
+randomInt :: Int -> Gen Int -- 0 .. n-1
+randomInt n = state $ \s ->
+  let s' = s + 0x9e3779b97f4a7c15
+      z1 = (s' `xor` (s' `shiftR` 30)) * 0xbf58476d1ce4e5b9
+      z2 = (z1 `xor` (z1 `shiftR` 27)) * 0x94d049bb133111eb
+   in (fromIntegral ((z2 `xor` (z2 `shiftR` 31)) `mod` fromIntegral n), s')
+
+randomText :: String -> Int -> Gen Text
+randomText alphabet maxLen = do
+  len <- randomInt (maxLen + 1)
+  T.pack <$> mapM (const ((alphabet !!) <$> randomInt (length alphabet))) [1 .. len]
+
+-- | One random case: Nothing if fast and naive agree.
+searchCase :: Gen (Maybe String)
+searchCase = do
+  b <- fromText <$> randomText "abAB \n" 40
+  needle <- T.cons <$> ((("abAB" :: String) !!) <$> randomInt 4) <*> randomText "abAB" 2
+  l <- randomInt (lineCount b)
+  c <- randomInt (lineLength l b + 1)
+  dir <- (\i -> if i == 0 then Forward else Backward) <$> randomInt 2
+  let fast = compileNeedle needle >>= \n -> findMatch dir n b (Pos l c)
+      slow = naiveFind dir needle b (Pos l c)
+  pure $
+    if fast == slow
+      then Nothing
+      else Just (show (dir, needle, toText b, Pos l c) <> ": fast " <> show fast <> ", naive " <> show slow)
+
+searchTests :: Test
+searchTests =
+  group "search"
+    [ test "forward wraps" $ assertEqual (Just (Pos 0 0)) (go Forward "ab" "ab x" (Pos 0 2))
+    , test "backward wraps" $ assertEqual (Just (Pos 1 2)) (go Backward "ab" "x\nxxab" (Pos 0 0))
+    , test "the match at the cursor comes last" $ assertEqual (Just (Pos 0 0)) (go Forward "ab" "ab" (Pos 0 0))
+    , test "smart case" $ assertEqual (Just (Pos 0 4), Nothing) (go Forward "ab" "x   AB" (Pos 0 0), go Forward "Ab" "x   AB" (Pos 0 0))
+    , test "no match" $ assertEqual Nothing (go Forward "zz" "ab\ncd" (Pos 0 0))
+    , test "agrees with the naive search on 1500 random cases" $
+        case [failure | Just failure <- evalState (mapM (const searchCase) [1 .. 1500 :: Int]) 42] of
+          [] -> Right ()
+          failure : _ -> Left failure
+    ]
+  where
+    go dir needle text p = compileNeedle needle >>= \n -> findMatch dir n (fromText text) p
```

To check that the test can actually fail, break `findMatch`. For example, make it look
for the next match *after the end* of the previous one:

```haskell
go (col + T.length (needleText needle)) (T.drop (T.length (needleText needle)) rest)
```

The randomized test fails at once. The cause is an overlapping match that the broken
version skips:

```
FAIL search / agrees with the naive search on 1500 random cases
     (Backward,"aa","AbAaaBa B  B\nAAa\nB aaaBb\nbbbAbbAa",Pos {posLine = 2, posCol = 7}):
     fast Just (Pos {posLine = 2, posCol = 2}), naive Just (Pos {posLine = 2, posCol = 3})
```

None of the hand-written tests would have caught that.

**Glue: `/`, `n`, `N`, `*` (§5.3).** A fourth mode, `Prompt`, holds the typed pattern.
Its fallback appends characters to the pattern, the same mechanism that inserts text in
insert mode. Each change shows the first match from where the search started, and `Esc`
restores that selection.

This preview searches on every keystroke. That is the naive version §5.3 warns about,
and the next improvement would be the `edPreviewPending` flag described there.

`src/Him/Editor.hs`, changes:

```diff
@@ import Him.Position
 import Him.Selection
 
-data Mode = Normal | Insert | Select
+data Mode = Normal | Insert | Select | Prompt
   deriving stock (Eq, Ord, Show)
 
@@ data Editor = Editor
   , edDirty :: !Bool
   , edStatus :: !(Maybe Text) -- a message, until the next key
+  , edPrompt :: !Text -- what has been typed after /
+  , edSearchOrigin :: !Range -- the selection to go back to on Esc
+  , edLastSearch :: !(Maybe Text) -- for n and N
   , edTop :: !Int -- first line on screen
   , edSize :: !(Int, Int) -- terminal rows and columns
@@ data Editor = Editor
 newEditor :: Maybe FilePath -> (Buffer, Bool) -> Editor
 newEditor path (b, trailing) =
-  Editor b (cursor (Pos 0 0)) Normal [] path trailing False Nothing 0 (24, 80) False
+  Editor b (cursor (Pos 0 0)) Normal [] path trailing False Nothing "" (cursor (Pos 0 0)) Nothing 0 (24, 80) False
 
 -- | In select mode a motion keeps the anchor, so the selection grows.
```

`src/Him/Commands.hs`, changes:

```diff
@@ import Control.Monad.Trans.State.Strict (StateT, get, gets, modify)
 import Data.Map.Strict (Map)
 import Data.Map.Strict qualified as Map
+import Data.Maybe (fromMaybe)
 import Data.Set qualified as Set
 import Data.Text (Text)
@@ import Him.Keymap
 import Him.Motion
 import Him.Position
+import Him.Search
 import Him.Selection
 
@@ actions =
       modify $ \ed -> ed {edRange = cursor (rangeStart (edRange ed))}
       edit (insertAtHead t)
+  , plainAction "search" "Search forwards, showing the match while typing" startSearch
+  , plainAction "search_next" "Select the next match" (searchNext Forward)
+  , plainAction "search_prev" "Select the previous match" (searchNext Backward)
+  , plainAction "search_selection" "Search for the selected text" searchSelection
+  , plainAction "prompt_confirm" "Accept the search" confirmSearch
+  , plainAction "prompt_cancel" "Abandon the search, back to where it started" cancelSearch
+  , plainAction "prompt_backspace" "Delete the last character of the search" (editPrompt (T.dropEnd 1))
   , plainAction "write" "Save the file" write
   , plainAction "quit" "Quit, unless there are unsaved changes" quit
@@ actions =
   ]
 
+-- | Select the nearest match of a search, if there is one.
+searchFrom :: Direction -> Text -> Pos -> Editor -> Maybe Range
+searchFrom dir t from ed = do
+  needle <- compileNeedle t
+  start <- findMatch dir needle (edBuffer ed) from
+  pure (Range start (Pos (posLine start) (posCol start + needleLength needle - 1)) Nothing)
+
+startSearch :: EditorM ()
+startSearch = modify $ \ed -> ed {edMode = Prompt, edPrompt = "", edSearchOrigin = edRange ed}
+
+-- | Show the first match while the pattern is typed.
+previewSearch :: EditorM ()
+previewSearch = modify $ \ed ->
+  let origin = edSearchOrigin ed
+   in ed {edRange = fromMaybe origin (searchFrom Forward (edPrompt ed) (rangeHead origin) ed)}
+
+editPrompt :: (Text -> Text) -> EditorM ()
+editPrompt f = modify (\ed -> ed {edPrompt = f (edPrompt ed)}) >> previewSearch
+
+confirmSearch :: EditorM ()
+confirmSearch = do
+  ed <- get
+  modify $ \e -> e {edMode = Normal, edLastSearch = Just (edPrompt ed)}
+  when (edRange ed == edSearchOrigin ed && not (T.null (edPrompt ed))) $ status "no match"
+
+cancelSearch :: EditorM ()
+cancelSearch = modify $ \ed -> ed {edMode = Normal, edRange = edSearchOrigin ed}
+
+-- | n and N: from the end of the selection forwards, or its start backwards.
+searchNext :: Direction -> EditorM ()
+searchNext dir = do
+  ed <- get
+  let from = (if dir == Forward then rangeEnd else rangeStart) (edRange ed)
+  case edLastSearch ed of
+    Nothing -> status "no search yet"
+    Just t -> maybe (status "no match") (\r -> modify (\e -> e {edRange = r})) (searchFrom dir t from ed)
+
+-- | *: search for the selected text.
+searchSelection :: EditorM ()
+searchSelection = do
+  ed <- get
+  let b = edBuffer ed
+      r = edRange ed
+      selected = textRange (rangeStart r) (nextPos b (rangeEnd r)) b
+  modify $ \e -> e {edLastSearch = Just selected}
+  status ("search: " <> selected)
+
 -- | A motion done n times.
 repeated :: Motion -> Int -> Motion
@@ registry :: Map Text Action
 registry = Map.fromList [(actName a, a) | a <- actions]
 
-normalBindings, selectBindings, insertBindings :: [(Text, Text)]
+normalBindings, selectBindings, insertBindings, promptBindings :: [(Text, Text)]
 normalBindings =
   [ ("h", "move_char_left"), ("l", "move_char_right"), ("j", "move_line_down"), ("k", "move_line_up")
@@ normalBindings =
   , ("v", "select_mode"), ("i", "insert_mode"), ("a", "append_mode")
   , ("d", "delete_selection"), ("c", "change_selection")
+  , ("/", "search"), ("n", "search_next"), ("N", "search_prev"), ("*", "search_selection")
   , ("C-d", "move_line_down 10"), ("C-u", "move_line_up 10"), ("space c", "insert_text \"// \"")
   , ("space w", "write"), ("C-s", "write"), ("space q", "quit"), ("space Q", "quit_force")
@@ insertBindings =
 
 -- | Keymaps whose leaves are ready to run: no lookup or parsing on a key press.
+promptBindings = [("esc", "prompt_cancel"), ("ret", "prompt_confirm"), ("backspace", "prompt_backspace")]
+
 type Keymaps = Map Mode (Keymap (EditorM ()))
 
@@ defaultKeymaps = do
   select <- keymap "Select" selectBindings
   insert <- keymap "Insert" insertBindings
-  pure (Map.fromList [(Normal, normal), (Select, unionKeymap select normal), (Insert, insert)])
+  prompt <- keymap "Prompt" promptBindings
+  pure (Map.fromList [(Normal, normal), (Select, unionKeymap select normal), (Insert, insert), (Prompt, prompt)])
   where
     keymap modeName pairs = either (Left . T.unlines) fromBindings (bindAll modeName pairs)
@@ handleKey kms key = do
     setPending ks = modify $ \e -> e {edPending = ks}
 
--- | What an unbound key does: in insert mode a character inserts itself.
+-- | What an unbound key does: in insert mode a character inserts itself,
+-- in the prompt it is added to the search.
 fallback :: Mode -> Key -> EditorM ()
 fallback Insert (Key (KChar c) mods) | Set.null mods = edit (insertAtHead (T.singleton c))
+fallback Prompt (Key (KChar c) mods) | Set.null mods = editPrompt (`T.snoc` c)
 fallback _ _ = pure ()
```

`src/Him/Render.hs`, changes:

```diff
@@ render theme ed =
     <> foldMap textRow [0 .. textRows ed - 1]
     <> moveCursor (textRows ed) 0 <> sgr (themeStatus theme) <> stringUtf8 (T.unpack (T.take cols status)) <> "\ESC[K\ESC[0m"
-    <> moveCursor (posLine hd - edTop ed) (min (cols - 1) (displayCol (lineAt (posLine hd) b) (posCol hd)))
+    <> cursorAt
     <> (if edMode ed == Insert then "\ESC[6 q" else "\ESC[2 q") -- bar or block
     <> "\ESC[?25h"
@@ render theme ed =
     hd = rangeHead r
     cols = snd (edSize ed)
-    status =
-      T.unwords $
-        [ case edMode ed of Normal -> " NOR"; Insert -> " INS"; Select -> " SEL"
+    cursorAt
+      | edMode ed == Prompt = moveCursor (textRows ed) (min (cols - 1) (displayCol status (T.length status)))
+      | otherwise = moveCursor (posLine hd - edTop ed) (min (cols - 1) (displayCol (lineAt (posLine hd) b) (posCol hd)))
+    status
+      | edMode ed == Prompt = "/" <> edPrompt ed
+      | otherwise = T.unwords $
+        [ case edMode ed of Normal -> " NOR"; Insert -> " INS"; Select -> " SEL"; Prompt -> ""
         , T.pack (fromMaybe "[scratch]" (edPath ed)) <> (if edDirty ed then " [+]" else "")
         , T.pack (show (posLine hd + 1) <> ":" <> show (posCol hd + 1))
```

**Try it.** `/two` selects the next `two` while you type, and `Enter` keeps it. `n` and
`N` go to the next and previous match, wrapping around. `e *` searches for the word
under the cursor. `/Two` only matches with a capital T.

---

## Where this leaves you

About 1,300 lines of Haskell and the small C shim: a modal editor whose behaviour is almost
entirely pure functions, with 80 tests that need no terminal. Comparing it with him
shows what the rest of the tutorial adds:

| Here | In him | Tutorial |
|---|---|---|
| one `Range` | a `NonEmpty` selection, edits applied bottom-up | §2.2, §5.4 |
| no undo | snapshots of the persistent buffer | §5.1 |
| `Seq Text` | a rope of multi-line blocks | §7.4 |
| full redraw every key | frame diff, row reuse, terminal scrolling | §4.1, §7.6 |
| one render per key | one render per batch of input | §7.1 |
| `T.breakOn` per line | one C scan per block | §7.5 |
| linear `isWide` | an early exit plus an `IntMap` | §7.3 |

Because the core is pure and tested, each of these can replace its counterpart here
without touching the rest. Part 7 of the tutorial does that in him.
