# Building a modal text editor in Haskell

*A step-by-step guide to **him**: a Helix-style terminal editor built from GHC's boot
libraries alone. It ends with a deep dive into how it was made fast and small.*

---

## What we are building

him is a modal editor for the terminal, in the spirit of Vim and Helix. By the end of
this guide it can:

- open, edit, and save files (UTF-8, LF or CRLF, with or without a final newline)
- move and select Helix-style: you **select first, then act** (`w` then `d`)
- insert, change, delete, yank and paste, undo and redo
- run `:w`, `:q`, `:q!`, `:wq`
- search with `/`, `?`, `n`, `N`, `*`, including a live preview while you type
- start faster and use less memory than Vim and Helix on a 14 MB file, and search it
  faster than both

It is about 3,600 lines of Haskell plus two small C files. It uses no packages from
Hackage, only the libraries that ship with GHC (`base`, `unix`, `text`, `bytestring`,
`containers`, `transformers`, `stm`, `directory`).

The guide follows the order the editor was actually built in. Each part ends with
something you can run. Along the way there are tasks, marked **▶ Task**: small pieces to
write yourself before you look at how him does it. The full source is in this
repository, so you can always compare.

| Part | Builds | Key idea |
|---|---|---|
| 1 | Raw terminal, escape codes, key decoding | The terminal is a byte stream in both directions |
| 2 | Buffer, positions, selections, motions, edits | A pure core you can test without a terminal |
| 3 | Commands, keymaps, modes, the main loop | Everything a key does is a *named command* |
| 4 | Frames, components, diffing | Render to a pure grid, then send only the difference |
| 5 | Undo, registers, search | Small modules on top of the core |
| 6 | A benchmark harness | Drive real editors in a pseudo-terminal |
| 7 | Optimizations | Measure, find the real cost, fix it, measure again |

---

## Part 0: Project setup

We use Stack with an LTS snapshot. The snapshot pins the compiler, so everyone gets the
same GHC (here 9.10.3, which haskell-language-server supports).

```sh
stack new him && cd him
```

Trim `package.yaml` down to boot libraries only, and turn on the warnings that catch
mistakes early:

```yaml
language: GHC2021
default-extensions: [OverloadedStrings, LambdaCase, DerivingStrategies]

ghc-options:
- -Wall
- -Wcompat
- -Wincomplete-uni-patterns
- -Wmissing-export-lists
- -Wunused-packages     # every dependency must actually be used

library:
  source-dirs: src
  c-sources: [cbits/winsize.c]   # added in part 1
  dependencies: [base, bytestring, containers, text, transformers, unix]

executables:
  him:
    main: Main.hs
    source-dirs: app
    ghc-options: [-threaded, -rtsopts]
    dependencies: [base, him]
```

Some developer-experience files are worth adding on day one:
- **`hie.yaml`:** an explicit Stack cradle for HLS.
- **`fourmolu.yaml`** and **`.hlint.yaml`:** formatting and lint settings.
- **`Makefile`:** short `build` / `run` / `test` / `watch` targets.
- **`docs/PLAN.md`:** a living design document. It lists the assumptions, indexes the
  architecture decisions ("ADRs", one file each in `docs/adr/`), and has a milestone checklist with a "where to pick up"
  section.

A test framework would be the one Hackage dependency we'd want, so we write a 50-line
harness instead (`test/Test/Harness.hs`): `test`, `group`, `assertEqual`, `runTests`.
It is all we need.

---

## Part 1: Talking to the terminal

A terminal is two byte streams. We write bytes (text, plus escape sequences that move the
cursor and set colours) and read bytes (typed characters, plus escape sequences for
special keys). Everything else is built on that.

### 1.1 Raw mode

Normally the terminal driver edits a line for you (echo, backspace, Ctrl-C), and the
program only sees input after Enter. An editor needs every key immediately, so we turn
all of that off: *raw mode*. The `unix` package exposes the termios flags:

```haskell
makeRaw :: TerminalAttributes -> TerminalAttributes
makeRaw attrs =
  foldl withoutMode attrs
    [ EnableEcho, ProcessInput, KeyboardInterrupts, ExtendedFunctions
    , StartStopOutput, MapCRtoLF, InterruptOnBreak, CheckParity
    , StripHighBit, ProcessOutput ]
    `withBits` 8 `withMinInput` 1 `withTime` 0
```

Leaving the terminal in raw mode after a crash leaves the user with a broken shell, so
restoring it must be guaranteed. `bracket` does that:

```haskell
withRawTerminal :: IO a -> IO a
withRawTerminal action = bracket enter leave (const action)
  where
    enter = do
      original <- getTerminalAttributes stdInput
      setTerminalAttributes stdInput (makeRaw original) WhenFlushed
      emit "\ESC[?1049h\ESC[2J\ESC[H"      -- alternate screen, clear, home
      pure original
    leave original = do
      emit "\ESC[0m\ESC[0 q\ESC[?25h\ESC[?1049l"
      setTerminalAttributes stdInput original WhenFlushed
```

The alternate screen (`?1049h`) is why Vim's screen disappears when you quit, and your
shell history comes back.

> **Pitfall ([ADR input-from-fd](adr/input-from-fd.md)).** Never call `hSetBuffering stdin` or `hSetEcho` while in raw mode.
> GHC then saves the termios state itself and restores *that* state at exit. That state
> is already raw, so it undoes your restore. Read from the file descriptor directly
> instead (`fdRead stdInput`). We found this by comparing `stty -g` before and after a
> run.

**▶ Task 1.** Write a loop that reads bytes with `fdRead stdInput 64` and prints their
values (`\r\n` at line ends, since output processing is off), quitting on `q`. Press the
arrow keys, `Ctrl-a` and `æ`. You'll see `[27,91,65]`, `[1]`, `[195,166]`. Then make the
loop throw an exception and check with `stty -a` that the terminal is still fine.

**Ctrl-Z.** Raw mode switches off the terminal's signal keys, so Ctrl-Z arrives as an
ordinary key. To suspend the editor, give the terminal back exactly as you found it,
then stop yourself with `raiseSignal sigTSTP`. The call returns when the shell sends
`SIGCONT` (`fg`). At that point, set raw mode again and redraw everything: the screen
belongs to whatever ran meanwhile ([ADR suspend](adr/suspend.md)).

### 1.2 Escape sequences as pure builders

All output goes through pure `ByteString.Builder` values (`Him.Terminal.Ansi`), so it
is testable and composes cheaply:

```haskell
csi :: Builder
csi = "\ESC["

moveCursor :: Int -> Int -> Builder        -- 0-based in, 1-based on the wire
moveCursor row col = csi <> intDec (row + 1) <> ";" <> intDec (col + 1) <> "H"

data Style = Style
  { styleFg, styleBg :: !Color
  , styleBold, styleItalic, styleUnderline, styleReverse :: !Bool }

sgr :: Style -> Builder   -- "Select Graphic Rendition": reset, then the whole style
```

The whole frame is written with **one** `hPutBuilder` and a flush. Many small writes would
let the terminal show half-drawn screens (flicker).

### 1.3 The window size, through a C shim

The `unix` package doesn't expose `ioctl(TIOCGWINSZ)`, so we add six lines of C
(`cbits/winsize.c`) and call them through the FFI:

```c
int him_get_winsize(int *rows, int *cols) {
    struct winsize ws;
    if (ioctl(STDOUT_FILENO, TIOCGWINSZ, &ws) == -1 || ws.ws_col == 0) return -1;
    *rows = ws.ws_row; *cols = ws.ws_col; return 0;
}
```

```haskell
foreign import ccall unsafe "him_get_winsize"
  c_getWinsize :: Ptr CInt -> Ptr CInt -> IO CInt
```

Resizes arrive as `SIGWINCH`. `installHandler windowChange` turns them into an event.

### 1.4 Decoding keys

Keys are values, independent of how a terminal encodes them:

```haskell
data KeyCode = KChar Char | KEnter | KEsc | KBackspace | KTab
             | KUp | KDown | KLeft | KRight | KHome | KEnd
             | KPageUp | KPageDown | KInsert | KDelete | KF Int
data Modifier = Ctrl | Alt | Shift
data Key = Key { keyCode :: !KeyCode, keyMods :: !(Set Modifier) }
```

There is a matching parser and printer for a readable syntax (`"C-s"`, `"A-x"`, `"g g"`,
`"ret"`), which keymaps use later. As in Helix, a shifted letter is just the upper-case
character; `Shift` only appears on keys like `S-tab`.

Decoding is a **pure** function, which makes it easy to test:

```haskell
-- | Keys, and leftover bytes that start an incomplete sequence.
-- With final = True no more input is coming: a lone ESC is the Esc key.
decodeKeys :: Bool -> ByteString -> ([Key], ByteString)
```

It handles:
- control bytes (`1..26` are `Ctrl-a..z`, `0x7f` is backspace)
- CSI sequences `ESC [ params final` (arrows; `1;5C` is `Ctrl-right`; `3~` is delete)
- SS3 sequences `ESC O A`
- `ESC x` as `Alt-x`
- multi-byte UTF-8

**ESC is ambiguous.** It is both the Esc key and the start of every sequence. The input
thread solves this with a timeout: if bytes end with an incomplete sequence, it waits
30 ms for more, then decodes with `final = True`.

```haskell
loop pending = do
  ready <- if B.null pending
             then threadWaitRead stdInput >> pure True
             else isJust <$> timeout 30000 (threadWaitRead stdInput)
  ...
```

`threadWaitRead` before `fdRead` matters: a blocking foreign read can't be interrupted
by `timeout`, but waiting in the IO manager can.

**▶ Task 2.** Write `decodeKeys` for ASCII, Enter/Tab/Backspace, `Ctrl-letters`, the four
arrow keys and a lone ESC. Test it with the harness:
`decodeKeys False "\ESC[A" == ([plain KUp], "")` and
`decodeKeys False "\ESC" == ([], "\ESC")`. him's version and its 17 tests are in
`Him.Terminal.Input` and `test/Spec.hs`.

**Checkpoint.** A loop that draws `~` on every row and shows the name of the last decoded
key at the bottom works, including resize (the raw mode, output and input milestones in `docs/PLAN.md`).

---

## Part 2: The pure core

From here to the end of Part 3, almost nothing touches IO. That's what makes the
editor easy to change: every behaviour is a pure function you can call in a test.

### 2.1 The buffer interface

The buffer is text as lines without their terminators, and always at least one line.
Callers only use an interface:

```haskell
lineCount   :: Buffer -> Int
lineAt      :: Int -> Buffer -> Text
insertText  :: Pos -> Text -> Buffer -> (Buffer, Pos)   -- position after the text
deleteRange :: Pos -> Pos -> Buffer -> Buffer           -- half-open
textRange   :: Pos -> Pos -> Buffer -> Text
nextPos, prevPos :: Buffer -> Pos -> Pos                -- cross line ends
charAt      :: Pos -> Buffer -> Maybe Char              -- line end reads as '\n'
```

The first implementation is just `newtype Buffer = Buffer (Seq Text)`. `Data.Sequence`
gives O(log n) indexing and splitting. Inserting text containing newlines is a nice
exercise in edge cases:

```haskell
insertText pos t b@(Buffer ls) =
  (Buffer (Seq.take l ls <> Seq.fromList newLines <> Seq.drop (l + 1) ls), Pos (l + breaks) endCol)
  where
    Pos l c = clampPos b pos
    (before, after) = T.splitAt c (lineAt l b)
    newLines = T.splitOn "\n" (before <> t <> after)
    breaks = T.count "\n" t
    endCol | breaks == 0 = c + T.length t
           | otherwise   = T.length (T.takeWhileEnd (/= '\n') t)
```

Part 7 replaces the representation with a rope, and no caller notices. That was the
point of hiding it.

**▶ Task 3.** Implement `deleteRange` for `Seq Text`. Hint: keep the part of the first
line before the start and the part of the last line after the end, merge them into one
line, and replace the lines in between.

### 2.2 Helix selections

Positions are `Pos { posLine, posCol }`, where the column is a character index. Ord is
document order. `posCol == lineLength` addresses the line end, i.e. the newline.

The central idea of Helix ([ADR selection-first-editing](adr/selection-first-editing.md), [ADR selection-model](adr/selection-model.md)): **every cursor is a selection**. A `Range`
has an anchor and a head, and covers every character between them *inclusive*. A "plain
cursor" selects the one character under it.

```haskell
data Range = Range
  { rangeAnchor  :: !Pos
  , rangeHead    :: !Pos          -- where the cursor is drawn
  , rangeWantCol :: !(Maybe Int)  -- column to return to on j/k through short lines
  }
data Selection = Selection { selRanges :: !(NonEmpty Range), selPrimary :: !Int }
```

Only single ranges are created so far, but because the type is `NonEmpty`, motions and
rendering already handle many ranges. Multiple cursors will be an addition, not a
rewrite.

In **insert mode** the head is read as a *gap*: text goes before the character at the
head. The same data serves both modes.

### 2.3 Motions are pure functions

```haskell
type Motion = Buffer -> Range -> Range

data Movement = Move | Extend          -- Extend keeps the anchor (select mode)

applyMotion :: Movement -> Motion -> Buffer -> Range -> Range
applyMotion Move   m b r = m b r
applyMotion Extend m b r = (m b r) { rangeAnchor = rangeAnchor r }
```

Simple motions only move the head: `charLeft = headMotion prevPos`. Vertical motion
remembers its column:

```haskell
lineBy delta b r = Range p p (Just want)
  where
    Pos l c = rangeHead r
    want = fromMaybe (displayCol (lineAt l b) c) (rangeWantCol r)
    l' = max 0 (min (lineCount b - 1) (l + delta))
    p  = Pos l' (charIndexAtCol (lineAt l' b) want)
```

(It remembers the *display* column, so `j` stays visually aligned across tabs; Part 4
explains display columns.)

Word motions are where Helix differs from Vim: `w` *selects* the next word and the blanks
after it. Characters are classified (`Blank | LineEnd | WordChar | Punct`), and three
small combinators do the walking:

```haskell
stepOffBoundary :: (Buffer -> Pos -> Pos) -> Buffer -> Pos -> Pos  -- start from the next char at a word's end
extendWhile     :: (Buffer -> Pos -> Pos) -> (CharClass -> Bool) -> Buffer -> Pos -> Pos
skipWhile       :: (Buffer -> Pos -> Pos) -> (CharClass -> Bool) -> Buffer -> Pos -> Pos

nextWordStart b r = Range start end Nothing
  where
    start   = skipWhile nextPos (== LineEnd) b (stepOffBoundary nextPos b (rangeHead r))
    cls     = classAt b start
    wordEnd = extendWhile nextPos (== cls) b start
    end     = extendWhile nextPos (== Blank) b wordEnd
```

`stepOffBoundary` is what makes repeated `w` select successive words. Without it, the
second `w` would stay on the word it just selected.

**▶ Task 4.** Write `nextWordEnd` (`e`): from the boundary-stepped start, skip blanks and
line ends, then extend over one class. Test: in `"hello world"` from `(0,4)` the result
is anchor `(0,5)`, head `(0,10)`. Write `prevWordStart` (`b`) as the mirror image using
`prevPos`.

`x` selects the whole line including its newline, and extends by one line if the range
already covers whole lines. That's one small function with two cases.

### 2.4 Edits are pure too

```haskell
type Edit = Buffer -> Range -> (Buffer, Range)

insertAtHead :: Text -> Edit
deleteBackward, deleteForward, insertNewline, openLineBelow :: Edit
deleteSelection :: Edit
```

`insertNewline` copies the current line's indentation; that's auto-indent in one line.
`deleteSelection` has the one interesting edge case. The last line has no newline after
it in the buffer, so `x d` on it would leave an empty line behind. When the range runs
to the end of the file and starts at a line start, the newline *before* it is deleted
too.

Every edge case gets a test. When the buffer later becomes a rope (Part 7), a "model"
test joins them: thousands of random inserts and deletes, checked against a plain list
of lines after every step. A test like that is what lets you rewrite a data structure
without fear.

### 2.5 Documents and files

A `Document` is a buffer plus everything that belongs to it:

```haskell
data Document = Document
  { docBuffer :: !Buffer, docSelection :: !Selection, docPath :: !(Maybe FilePath)
  , docDirty :: !Bool, docLineEnding :: !LineEnding, docTrailingNewline :: !Bool
  , docHistory :: !History, docSavedBuffer :: !Buffer }
```

Loading decodes UTF-8 leniently (invalid bytes become U+FFFD), detects CRLF from the
first line, and remembers whether the file ended with a newline, so saving writes back
exactly what was there. `decodeDocument`/`encodeDocument` are pure and round-trip tested.

---

## Part 3: Commands, keymaps, and the main loop

### 3.1 Editor state

```haskell
data Editor = Editor
  { edDoc :: !Document, edMode :: !Mode, edView :: !View, edSize :: !(Int, Int)
  , edStatus :: !(Maybe Status), edPending :: ![Key]   -- e.g. the "g" of "g g"
  , edCmdLine :: !Text, edPrompt :: !PromptKind, edRegisters :: !(Map Char Text)
  , edQuit :: !Bool, ... }

data Mode = Normal | Insert | Select | CmdLine
```

### 3.2 Everything is a named command

This is the main extension point ([ADR command-registry](adr/command-registry.md)):

```haskell
type EditorM = StateT Editor IO

data Command = Command
  { cmdName :: !Text          -- snake_case: "move_next_word_start"
  , cmdDoc  :: !Text
  , cmdRun  :: EditorM () }

type Registry = Map Text Command
```

Commands are thin wrappers around the pure core:

```haskell
motion :: Motion -> EditorM ()
motion m = do
  mode <- gets edMode
  let movement = if mode == Select then Extend else Move
  modifyDoc $ \d -> d { docSelection = mapRanges (applyMotion movement m (docBuffer d)) (docSelection d) }

commands =
  [ Command "move_next_word_start" "Select to the start of the next word" (motion nextWordStart)
  , Command "delete_selection" "Delete the selection (and yank it)" (yank >> edit deleteSelection >> setMode Normal)
  , ... ]
```

Keys are bound to *names*, never to functions. That has three effects:
- bindings can later come from a config file
- the startup check can report "keymap refers to unknown commands: …"
- a test can assert that the default config is valid

### 3.3 Keymaps are tries

Multi-key chords (`g g`, `space f`) need a tree, not a map:

```haskell
newtype Keymap = Keymap (Map Key Node)
data Node = Leaf Text | Prefix Keymap
data Resolved = Found Text | NeedMore | NoMatch

resolve :: Keymap -> [Key] -> Resolved
resolve _ [] = NeedMore
resolve (Keymap m) (k : ks) = case Map.lookup k m of
  Nothing -> NoMatch
  Just (Leaf name)  | null ks   -> Found name
                    | otherwise -> NoMatch
  Just (Prefix sub) | null ks   -> NeedMore
                    | otherwise -> resolve sub ks
```

Bindings are written as plain pairs, with a left-biased `unionKeymap` that merges
prefixes. Select mode is "normal mode plus two overrides":

```haskell
normalBindings = [("h", "move_char_left"), ("w", "move_next_word_start"), ("g g", "goto_file_start"), ...]
selectBindings = [("esc", "normal_mode"), ("v", "normal_mode")]
keymaps = Map.fromList [(Normal, normal), (Select, unionKeymap select normal), ...]
```

**▶ Task 5.** Implement `fromBindings :: [(Text, Text)] -> Either Text Keymap`, where an
`insert` creates `Prefix` nodes along the way. Then write the four `resolve` tests:
single key, prefix, chord, unknown.

### 3.4 Handling a key

```haskell
handleEvent :: Config -> Event -> EditorM ()
handleEvent config (EvKey key) = do
  ed <- get
  let keys = edPending ed <> [key]
  case resolve (keymapFor (edMode ed)) keys of
    NeedMore   -> setPending keys                       -- show "g" in the status line
    Found name -> setPending [] >> runCommand name
    NoMatch    -> do
      setPending []
      when (null (edPending ed)) $ sequence_ (cfgFallback config (edMode ed) key)
  commitOutsideInsert                                   -- undo grouping, see 5.1
```

The *fallback* is how typing works: in insert mode an unbound printable key inserts
itself, and in command-line mode it is appended to the prompt.

Because `handleEvent` is an ordinary function, the integration tests feed key strings
through the real keymap without a terminal:

```haskell
typed <- textAfter "" "i h i space t h e r e esc"     -- "hi there"
moved <- textAfter "a\nb\nc" "x d p"                   -- "b\na\nc"
```

The IO shell around it (`Him.App`) is small. It reads events from a channel, handles
them, renders, and writes the output. Part 7 changes how many events it handles per
render.

### 3.5 `:` commands

Ex commands get their own tiny table, because they take arguments:

```haskell
data ExCommand = ExCommand { exNames :: [Text], exDoc :: Text, exRun :: [Text] -> EditorM () }

exCommands =
  [ ExCommand ["write", "w"] "Write the file" (\args -> () <$ write args)
  , ExCommand ["quit", "q"]  "Quit (refuses with unsaved changes)" $ \_ -> do
      dirty <- docDirty <$> getDoc
      if dirty then failWith "unsaved changes (use :q! to discard them, or :wq to save)" else quit
  , ... ]
```

### 3.6 From commands to actions ([ADR actions](adr/actions.md))

Names alone cannot say "move down **5** lines" or "insert `// `". Commands therefore
became *actions*. An action is a stable name plus typed parameters, and a binding is
text that a config file could contain:

```haskell
normalBindings = [("j", "move_line_down"), ("C-d", "move_line_down 20"), ("space c", "insert_text \"// \"")]
```

Parameters are described by a tiny applicative. The same value lists the parameters
(for help) and converts the text arguments:

```haskell
data ArgSpec a = ArgSpec { specParams :: [Param], specParse :: [Text] -> Either Text (a, [Text]) }

int :: Text -> ArgSpec Int                       -- one positional argument
optional :: Text -> a -> ArgSpec a -> ArgSpec a  -- may be left out

action "move_line_down" GMovement "Move down" (optional "1" 1 (int "count")) (motion . lineBy)
action "goto_line" GMovement "Go to a line" (int "line") (motion . gotoLine)
```

The important design choice is *when* the text is checked. `buildConfig` binds every
pair once, at startup: it parses the invocation, looks up the action, and converts the
arguments. The result is a `Keymap Bound`, a trie whose leaves already hold the
`EditorM ()` to run. A key press does no lookup and no parsing, and a typo in a binding
is reported before the editor starts. The report names every bad binding:

```
Normal mode, a: unknown action: fly
Normal mode, b: goto_line: missing argument <line>
```

To make that work, `Keymap` became generic (`Keymap a`, a `Functor`): `Keymap Text`
while reading, `Keymap Bound` while running. User bindings go on top of the defaults
with `overrideBindings`, and `no_op` switches a key off.

Counts fall out of this design. `5 j` stores the digits in `edCount`, and when the
binding has no arguments and the action's first parameter is `int "count"`, the count is
passed in as that argument. The decision is made once, in `bindInvocation`, which stores
a `boundCounted :: Maybe (Int -> EditorM ())` next to `boundRun`.

That design paid off later: the config file ([ADR toml-config](adr/toml-config.md)) is little more than a TOML reader
that produces the same `Bindings` pairs, plus `him --dump-default-config`, which prints
the defaults through the same tables.

**▶ Task 5b.** Write `parseInvocation :: Text -> Either Text Invocation` (words, plus
double-quoted strings with `\"` and `\\`) and its inverse, and test that they
round-trip.

**Checkpoint.** You can open a file, move with `hjkl`/`w b e`, select with `x`/`v`, edit
in insert mode, and `:wq` (the milestones from loading a buffer to Helix's selection actions).

---

## Part 4: Rendering

### 4.1 Frames and components

Rendering is a pure function from editor state to a grid of styled cells:

```haskell
data Cell = Cell { cellChar :: !Char, cellStyle :: !Style }
data Frame = Frame
  { frameRows, frameCols :: !Int
  , frameCells :: !(Seq (Seq Cell))
  , frameCursor :: !(Maybe (Int, Int)), frameCursorShape :: !CursorShape, ... }
```

The screen is split into rectangles by a `layout`:
- the gutter and text area on top
- then the status line
- then the command line

Each region is drawn by a *component*, and adding a UI element means adding a component:

```haskell
type Component = Theme -> Editor -> Rect -> Frame -> Frame

render theme prev ed =
    drawCommandLine theme ed cmdR
  . drawStatusLine  theme ed statusR
  . drawTextArea    theme prev ed textR
  . drawGutter      theme ed gutterR
  $ blankFrame rows cols
```

Then `diffFrames :: Maybe Frame -> Frame -> Builder` turns "old screen, new screen" into
output. The first version was simple: redraw every row that differs. Part 7 makes it
much smarter.

### 4.2 Display columns

A character index is not a screen column:
- a tab runs to the next multiple of 4
- CJK characters and emoji take 2 cells
- a control character (a stray `ESC` or `\r` in a file) would corrupt the screen if
  printed raw, so it is shown as `^[` and takes 2 cells

`Him.TextWidth` maps characters to cells:

```haskell
layoutLine :: Text -> [(Int, Int, Int, Char)]   -- (charIndex, displayCol, width, char)
charWidth c
  | c >= ' ' && c < '\DEL' = 1                   -- fast path, see Part 7
  | isControl c = 2
  | isWide c = 2
  | otherwise = 1
```

A wide character takes its cell plus a *continuation* cell (`'\0'`). The diff skips
continuation cells, because the terminal fills them by itself. A wide character cut off
by the screen edge is drawn as a blank, so the terminal never draws half of one.

**▶ Task 6.** Write `displayCol :: Text -> Int -> Int` and its inverse
`charIndexAtCol :: Text -> Int -> Int`. Check that `displayCol "\tx" 1 == 4`,
`displayCol "漢字x" 2 == 4`, and `charIndexAtCol "漢字x" 3 == 1`.

### 4.3 The view

`scrollToCursor` keeps the cursor on screen with a 3-line margin (scrolloff). It is pure
and has four tests. The gutter width follows the line count, so `layout` depends on the
editor.

### 4.4 Themes: borrow a format, get 219 themes ([ADR helix-themes](adr/helix-themes.md))

Highlighting (5.9) produces *scopes* with Helix's names (`keyword.control.import`). So
reading Helix's theme files, rather than inventing a format, makes every Helix theme
work in him:

```toml
inherits = "onedark"
"comment" = { fg = "gray", modifiers = ["italic"] }
"ui.selection" = { bg = "#3e4452" }
"diagnostic.error" = { underline = { color = "red", style = "curl" } }
[palette]
gray = "#5c6370"
```

The work splits three ways:
- **`Him.Theme` (pure):**
  - turns the file into a map from scope to `Style`: palette names, hex colours,
    modifiers, underlines;
  - merges a child theme over its parent;
  - holds the built-in theme, written in the same format.
- **`Him.Theme.Load` (IO):** finds the files and follows `inherits`.
- **`Him.Render.Theme.fromScopes`:** looks up the UI styles once (`ui.statusline.insert`,
  `ui.menu.selected`, …), falling back by prefix as Helix does. Drawing then never
  searches the map for them.

Two ideas carry most of it:
- **Layering instead of replacing.** `patchStyle under over` takes `over`'s colours
  where it has them and the modifiers of both. A cell's style is built up in layers:
  text, then syntax, then the diagnostic underline, then the selection, then the
  cursor. A selection that only sets a background keeps the syntax colour.
- **Let the terminal paint the background.** A theme's `ui.background` is not written
  into every cell. It becomes the terminal's default background through OSC 11:
  `\ESC]11;rgb:28/2c/34\ESC\\`. Blank cells, cleared lines and rows scrolled in by
  the terminal all show it for free, so none of the diff's shortcuts (7.6) change.

**▶ Task 6b.** Write `parseColor :: Map Text Text -> Text -> Maybe Color`, handling
palette names, `#rrggbb`, `#rgb` and the 16 named colours. Then map an RGB colour to
the nearest xterm-256 colour: try the 6×6×6 cube level of each channel, and the grey
ramp, and keep the closer one.

---

## Part 5: Undo, registers, and search

### 5.1 Undo with snapshots ([ADR snapshot-undo](adr/snapshot-undo.md))

Because the buffer is a persistent data structure, a snapshot of `(Buffer, Selection)`
shares almost all of its memory with the current state. So undo can simply keep
snapshots:

```haskell
data History = History { histUndo, histRedo :: ![Snapshot], histPending :: !(Maybe Snapshot) }

beginChange :: Snapshot -> History -> History    -- called by every edit; keeps the FIRST state only
commit      :: History -> History                -- pending becomes an undo step
```

The trick is *when* to commit. The main loop commits after every key, **unless the
editor is in insert mode**. So `c foo esc` (delete, type, leave) is one undo step,
exactly like Helix. Undoing back to the saved text clears the `[+]` marker, because the
document remembers `docSavedBuffer`.

### 5.2 Registers and linewise paste ([ADR registers-in-editor](adr/registers-in-editor.md))

`y`, `d` and `c` store the selection in register `"`. Text ending in a newline (what
`x` selects) pastes as whole lines: `p` puts it below the current line, `P` above. One
subtle bug turned up here. The last line has no newline in the buffer, so yanking it
with `x` gave charwise text. `selectionText` now adds the implicit newline in that case.

### 5.3 Search

Search reuses the command line. A `PromptKind` says what Enter will do, and remembers the
selection the search started from, so `Esc` can restore it:

```haskell
data PromptKind = ExPrompt | SearchPrompt !Direction !Selection
```

The matching is pure (`Him.Search`):

```haskell
compileNeedle :: Text -> Maybe Needle     -- smart case: no upper-case letters => case-insensitive
findMatch :: Direction -> Needle -> Buffer -> Pos -> Maybe Match   -- wraps around
```

The **incremental preview** shows the first match while you type. A naive version
searches on every keystroke, so pasting a 15-character pattern would search 15 times.
Instead, typing only sets `edPreviewPending = True`, and the main loop computes the
preview once, just before it renders:

```haskell
go prev ed0 = do
  let ed    = ensureCursorVisible (refreshSearchPreview ed0)
      frame = render defaultTheme prev ed
  ...
```

**▶ Task 7.** Write a naive reference search: for every line and every column, check
`needle `T.isPrefixOf` T.drop c line` (with case folding when the needle has no
upper-case letters). Use it in a randomized test against `findMatch` for both
directions. That's how him's fast search is tested: 1,500 random cases on buffers built
from several blocks plus edits.

---

### 5.4 Many cursors ([ADR bottom-up-multi-range-edits](adr/bottom-up-multi-range-edits.md))

Every motion already worked on all ranges, so multiple selections only needed edits to
do the same. The usual approach maps every position through every change. Instead, him
applies the edit to the *last* range first and walks upwards. An edit only touches text
near its own range, so the ranges already done lie entirely after the change. If each
finished result is stored as "this many lines from the end, this many characters from
the end of its line", no edit further up can disturb it:

```haskell
applyEdits :: (Int -> Edit) -> Buffer -> Selection -> (Buffer, Selection)
-- for each range, last first:
--   (b', r') = f i b r
--   remember (lineCount b' - 1 - line, lineLength line b' - col) for both ends of r'
-- finally turn the distances back into positions in the final buffer
```

All the single-range edits (`insertAtHead`, `deleteSelection`, `pasteAfter`, …) work
with many cursors unchanged. A randomized test compares inserts and backspaces at
random cursors with the same operations on a plain string.

On top of that, the Helix selection tools are small pure functions. `s` (select the
matches inside the selection) reuses the search's block scanner and its incremental
preview. `C` copies each range onto the next line where it fits, and `A-s` splits ranges
into lines.

### 5.5 Buffers, menus, and pickers ([ADR buffer-zipper](adr/buffer-zipper.md), [ADR menus-as-data](adr/menus-as-data.md))

**Buffers without touching `edDoc`.** Dozens of functions read `edDoc`. Instead of
replacing it with a list and an index, the other documents sit on either side of it, as
a zipper:

```haskell
data Editor = Editor { edDoc :: Document, edBefore :: [Buffered], edAfter :: [Buffered], ... }
data Buffered = Buffered { bufDoc :: Document, bufView :: View }
```

Switching buffers moves documents between the lists. Everything that edits "the
document" keeps working unchanged.

**Menus as derived data.** Helix shows which keys can follow `g` or `space`. The
keymap trie already has that answer, so the info box is a function of the state:

```haskell
refreshInfo :: Config -> Editor -> Editor      -- runs after every key
keyInfo config ed = do
  sub <- lookupPrefix keymap (edPending ed)     -- the trie below "g"
  pure (InfoBox "goto" [(showKey k, docOf b) | (k, b) <- children sub] BottomRight)
```

The `:` menu works the same way, from the ex-command table. Since the box is
recomputed rather than updated, there is no "forgot to close the popup" bug.

**A trap from the row cache.** Rows are copied from the previous frame when their
`RowKey` is unchanged (7.6). A popup draws over those rows, so after it closes, the
cached rows would bring it back. The fix is one line: a popup deletes the keys of the
rows it covers. The test for it renders with a box, then without, and compares the
result with a fresh render. With the line removed, the test fails.

**Pickers** are a mode (`Picking`) with their own keymap, so the arrow keys, `ret` and
`esc` are ordinary bindings. Typed characters reach the query through the fallback.
The fuzzy score is the number of characters skipped between the first and last match,
and every start position is tried, so `ab` matches `src/ab.hs` before `src/a/long/b.hs`.

### 5.6 Ignore files and a directory viewer ([ADR gitignore-matcher](adr/gitignore-matcher.md), [ADR directory-documents](adr/directory-documents.md))

**Gitignore without a regex engine.** A pattern compiles to a few tokens, and a
backtracking matcher walks the path:

```haskell
data Tok = Lit Char | One | Star | AnyAll | Dirs | Class Bool [(Char, Char)]
-- "*.o"    -> [Dirs, Star, Lit '.', Lit 'o']   (no slash: match at any depth)
-- "/build" -> [Lit 'b', ...]                   (anchored)
glob (Star : ts) s = any (glob ts) [drop n s | n <- [0 .. length (takeWhile (/= '/') s)]]
glob (Dirs : ts) s = glob ts s || or [glob ts rest | ('/' : rest) <- tails s]
```

Each rule set remembers the directory of its file. The walk adds a directory's rules
before listing it, and simply never enters an ignored directory. That one choice gives
git's rule that nothing inside an ignored directory can be re-included.

**A directory is just a document.** Emacs's dired shows a directory as a buffer of
text, and that is the trick here too. A listing is a read-only `Document` whose lines are
its entries, so `j`, `5 k`, `/name`, `g g` and the buffer commands work with no new code.
What it adds is a keymap *layer*: in normal mode on a listing, `keymapMode` returns
`Directory`, a binding set that inherits normal mode exactly as select mode does. It
adds `ret` (open), `-` (up) and `g r` (refresh). The only other changes are guards that
refuse edits and insert mode in a read-only document.

File operations reuse the command line. `r` opens a prompt whose kind,
`FilePrompt (RenameEntry dir old)`, says what `ret` will do, and the line starts out
holding the old name. `d` collects the entries under *every* selection, so the
multiple-selection tools from 5.4 double as dired's marks. One detail is worth a test of
its own: deleting a symlink to a directory must unlink it, not delete what it points to.

### 5.7 Effects, background jobs, and a picker that never blocks ([ADR effects-and-runtime](adr/effects-and-runtime.md), [ADR streaming-file-picker](adr/streaming-file-picker.md))

Everything so far ran on the main thread, and that was fine, since a key costs well
under a millisecond. A file picker over 200,000 files is different: the walk takes
hundreds of milliseconds and ranking takes tens. Two ideas keep the editor responsive.

**Effects as data.** An action that wants work done asks for it:

```haskell
simple "file_picker" GBuffers "Open a file from the working directory" $ do
  gen <- freshGeneration
  open (newPicker "files" []) {pkGeneration = gen, pkLoading = True}
  request (StartJob (ScanFiles gen "."))
```

The main loop owns the runtime. It starts the job on a thread, and the job posts its
results on the same channel as the keys (`EvJob (FilesFound gen batch)`). Results
are handled by `handleEvent` like a key press, so there are no locks around the
editor state. A tagged generation lets the picker ignore results meant for one that
was closed.

**Measure, then choose.** The first profile showed three separate costs. Each got
the cheapest fix that worked:
- *rejecting* a non-match: a precomputed lower-case key and an in-order check,
  65 → 12 ms;
- *sorting* every match: buckets keyed by rank, keeping the best 1000;
- *walking*: parallel workers, plus types from `readdir` instead of a `stat` per file,
  732 → about 370 ms.

What remained was about 75 ms when every item matches. So instead of more micro-tuning,
large pickers rank in a background job and keep showing their last results until the
new ones arrive. Typing never waits.

### 5.8 Git signs and staging lines ([ADR git](adr/git.md))

Git is two diffs away. The index and HEAD versions of a file come from
`git show :path` and `git show HEAD:path`, run in a background job. Two diffs follow:
- index → buffer, the *unstaged* changes;
- HEAD → index, the *staged* ones, whose lines are moved onto the buffer through the
  first diff.

The diff is ours (Myers, after trimming the common prefix and suffix), so the signs
follow every keystroke, not only saves. A randomized test checks every diff against a
textbook longest-common-subsequence: it must be correct *and* minimal.

Staging, unstaging and resetting look like three features, but they are one function:

```haskell
-- old with the selected changes towards new applied
applySelected :: [Text] -> [Text] -> [Hunk] -> (Int -> Bool) -> [Text]

stage   = applySelected index buffer (diffLines index buffer) selected
unstage = applySelected head index  (diffLines head index)   (not . selected')
reset   = applySelected index buffer (diffLines index buffer) (not . selected)
```

Reverting the selected changes is the same as applying the unselected ones. The new
index version goes in with `git hash-object -w --stdin` and `git update-index
--cacheinfo`. No patches are built, so partial hunks cannot produce an invalid patch.

### 5.9 Highlighting: one interface, tree-sitter behind it ([ADR syntax-providers](adr/syntax-providers.md), [ADR tree-sitter](adr/tree-sitter.md), [ADR regex-engine](adr/regex-engine.md))

The requirement was "the code shouldn't care whether it is tree-sitter or TextMate".
In Haskell that is a record of functions:

```haskell
data SyntaxProvider = SyntaxProvider { spName :: Text, spStart :: Language -> IO (Maybe SyntaxSession) }
data SyntaxSession  = SyntaxSession  { ssUpdate :: Int -> Buffer -> [TextChange] -> IO ()
                                     , ssHighlight :: Int -> Int -> IO (IntMap [LineSpan])
                                     , ssClose :: IO () }
```

The editor asks for spans of the lines around the view, in a background job, and draws
them. The tests plug in a provider that only knows the word `let`, and everything
works: jobs, versions, rendering, the theme. Only then does tree-sitter come in.

**A war story.** The first tree-sitter build crashed the test suite with "corrupted size
vs. prev_size", but only for Haskell, only for longer files, and only after the
highlight query had been compiled. The search narrowed the cause in steps:
1. A plain C program with our runtime and Helix's `haskell.so` crashed the same way,
   so it was not the Haskell bindings.
2. The grammar built from source at `-O0` worked, but at `-O3` it crashed.
3. AddressSanitizer pointed at `array_push` in the grammar's scanner.

The grammar's old `array.h` reallocates through an `(Array *)` cast, and strict
aliasing lets the compiler keep the stale pointer. The lesson: native code from
elsewhere is part of your program's memory safety. him now builds its own grammars
(`him --build-grammars`, with `-fno-strict-aliasing`) instead of trusting prebuilt
ones.

### 5.10 A language-server client without blocking ([ADR lsp-client](adr/lsp-client.md))

LSP is JSON-RPC over a pipe. The design question is where the waiting happens, and the
answer is: never on the main thread. Three pieces cooperate:
- **The runtime** owns the server processes. A reader thread turns the byte stream into
  messages with a pure framer (`feedFramer`, tested at every split point of a stream),
  and a writer thread drains a queue. Requests from the server that need no decision,
  such as "send me your configuration", are answered right there.
- **The editor** builds messages as plain JSON values and sends them as an effect
  (`LspSend`). It records what each request was for: `IntMap Pending`, keyed by id.
- **Replies** arrive as events. `answered` is a pure function from `Pending` and the
  result to an editor change: a hover popup, a jump, a picker, a completion menu.

Positions are where clients often go wrong. LSP counts UTF-16 code units by default;
him counts characters. The client offers UTF-8, which clangd and rust-analyzer accept,
and converts columns per line (`toLspColumn` / `fromLspColumn`). The conversions are
tested with an emoji, which is 1 character, 2 UTF-16 units and 4 UTF-8 bytes.

The tests drive a real clangd: open a C file with a type error, wait for the
diagnostic, hover, jump to a definition, fix the error and watch the diagnostic
disappear, then complete a word.

**Sending less.** The first version sent the whole file after every burst of typing.
Incremental sync needs to know *what* changed. Rather than threading a change log
through every edit function (and through undo, which swaps whole snapshots), the
client compares the text it last sent with the text now (`Buffer.changeBetween`).
That sounds expensive but is not. The rope shares unchanged blocks between versions,
so a memory comparison skips them; only the edited block is compared line by line,
and the remaining lines character by character. A one-character edit in 196,000 lines
costs under a millisecond. A randomized test checks that applying the computed change
to the old text gives the new one.

**Where edits come back.** Rename, formatting and code actions all return edits. One
function applies them, from the last position to the first so that earlier positions
stay valid. One trap is worth knowing: clangd's "extract variable" is a *command*.
The client runs it, and clangd answers by *asking the client* to apply an edit
(`workspace/applyEdit`). A client that answers every server request automatically, as
the first version did, silently drops that edit.

**Seeing before choosing.** A list of references is only useful if you can see them.
The picker's preview is a pure function of the editor state (`previewFor`). An item
that is a place resolves to an open buffer's text, or to a file read in the background
and cached while the picker is open. The renderer draws what that function returns:
the text, "loading…", or why the file is not shown. Because the preview is derived
data, moving the selection needs no bookkeeping beyond asking for a file that has not
been read yet.

**Queries the server answers.** Workspace symbols cannot be filtered locally; there
are too many. So a picker can have a *source*. A `ServerQuery` picker sends each
change of its query to the server, keeps showing the last answer, and drops answers to
older queries, the same staleness rule as everywhere else.

### 5.11 Splits and a REPL beside the code ([ADR window-splits](adr/window-splits.md), [ADR repl](adr/repl.md))

**Splits** keep the old state for the focused window: `edDoc` and `edView` are still
"what you are editing". The other windows are only `Window { winDoc, winView,
winSelection }`, and the screen is a tree:

```haskell
data Layout = Leaf Int | Split Axis [Layout]   -- Axis: Beside (:vsplit) | Stacked (:hsplit)
```

- **Focusing** a window swaps it with the focused one: the focused window is put
  away as a `Window`, and the other one's document becomes current, with its view and
  selection. No action had to change.
- **Drawing** an unfocused window uses the same components: `windowEditor` builds the
  editor *as that window shows it*, and the gutter, text area and status line draw it
  as usual.

**The REPL plugin** turns a buffer into a terminal-like transcript:
- The document remembers where the input starts.
- Output from the process is inserted *before* that point, so it never interrupts
  what you type. `ret` sends what follows it.
- Code sent from a file (`space e`) is echoed into the transcript, because a REPL
  reading a pipe does not echo.

**Using it for testing while developing** (him itself is the example):

```toml
# ~/.config/him/config.toml
[repl.haskell]
args = ["ghci", "him:lib", "him:test:him-test"]   # the library and the tests
```

1. Open `src/Him/Window.hs`, then `:repl`. `stack ghci` starts in the project root
   and loads everything.
2. Write a function, select a call to it (`x`, or `v` and a motion), and press
   `space e`. The result appears in the REPL window. Several lines are wrapped in
   `:{ … :}` for you.
3. Save (`:w`). The REPL runs `:reload` by itself, so the next `space e` uses the new
   code.
4. `:repl-send main` runs the whole test suite. To run one group, select an
   expression such as `runTests [group "w" windowTestsPure]` and press `space e`.
5. `C-c` (in the REPL buffer) or `:repl-interrupt` stops a runaway evaluation.

**▶ Task 7b.** Write `insertOutput` for a transcript. Given the input start `p` and
some output text, insert the text at `p`. Then move every cursor at or after `p`, so
that one at the end of the typed input stays at the end. Test it with output that has
no newline, and with output that has two.

### 5.12 Text objects and an AI chat ([ADR match-mode](adr/match-mode.md), [ADR ai-chat](adr/ai-chat.md))

**Match mode** (`m`) is small once selections are pure:
- `m i w` and `m a (` ask `Him.TextObject.textObject` for a range around the cursor:
  the run of word characters, the innermost pair around it, the paragraph.
- Brackets are found by walking the text lazily backwards and forwards, counting
  nesting.
- `m s (` surrounds every selection through the same `applyEdits` that typing uses.

**The chat** reuses three earlier pieces:
- the transcript from the REPL (5.11), now `Him.Transcript`;
- splits, for the window beside the code;
- the provider idea from highlighting (5.9). A `ChatProvider` takes a request and
  streams events back, so the tests drive the whole flow with a scripted provider.

The interesting part is the model's edits. They are *proposed*, all in one turn, and
reviewed like staged hunks ([ADR change-review](adr/change-review.md)):
- A document under review keeps its text from before the chat's first change (the
  base). The proposed changes are simply the diff from the base to the buffer.
- Approving one applies it to the base, the way `git add -p` stages a hunk, and writes
  the base to the file. Denying applies the reverse to the buffer.
- The text area draws extra rows for each change: a header, and the removed lines.
  These rows are not in the buffer.

The chat buffer is laid out like an editor's chat panel ([ADR chat-panel](adr/chat-panel.md), `Him.Chat.Transcript`).
Output goes *above* the prompt, so the input box stays at the bottom. The transcript
only ever grows at its last line, so a line's number never changes: what each line is
(your message, a code block, a tool line) is kept in a map by line number, and the
renderer styles lines from it. The model's prose is wrapped as it streams by
re-wrapping just the last line with each new chunk.

The history sent to the model is append-only. The assistant's messages go back exactly
as they came, thinking blocks included, which the API requires.

With **Claude Code** as the provider ([ADR claude-code-provider](adr/claude-code-provider.md)), the model runs its own loop, so him's
tools reach it over MCP. `him --mcp-bridge` is a tiny MCP server that Claude Code
starts; it forwards each tool call through a named pipe to the running editor, and
waits. The editor answers an edit only after you decide, so the model's turn simply
pauses until then.

**▶ Task 7c.** Write `textObject True 'w'`: given a line and a column, return the run
of characters of the same kind (word, punctuation, blank) around it. Then make `m a w`
include the blanks after the word, or before it at the end of a line.

### 5.13 A plugin API, and writing a plugin ([ADR picker-actions](adr/picker-actions.md), [ADR plugin-building-blocks](adr/plugin-building-blocks.md), [ADR plugin-api](adr/plugin-api.md))

Everything so far was compiled in, and so are plugins: like xmonad, him has no plugin
loader. Instead, `Him.Plugin` is one module that a plugin imports, and the **contrib
collection** (`src/Him/Contrib/`) is compiled into every release, off until you switch
a plugin on:

```toml
[plugins]
recent-files = true          # space o: the files opened lately

[plugins.wordcount]          # a table instead, for settings
enabled = true
max-lines = 20000
```

`:plugins` lists every plugin in a picker; `ret` switches the chosen ones on or off.

A plugin is a `PluginSpec s`, where `s` is its own state:

```haskell
wordCount :: PluginSpec (IntMap Int)
wordCount =
  (pluginSpec "wordcount" "The number of words in the buffer" IntMap.empty)
    { psDefaultOn = False
    , psOnEvent = \case
        BufferChanged i _ -> recount i
        _ -> pure ()
    }

recount :: BufferId -> PluginM (IntMap Int) ()
recount i = do
  n <- maybe 0 (length . T.words) <$> bufferText i
  modifyState (IntMap.insert i n)
  counts <- getState
  setSegments [(segment (T.pack (show c) <> " words")) {segDoc = Just b} | (b, c) <- IntMap.toList counts]
```

The design keeps the editor's state pure:
- **What a plugin shows is data.** Status line segments, gutter signs and end-of-line
  annotations live in the `Editor`, and the renderer draws them. A plugin can't draw
  into the frame, so plugins can't break each other's layout.
- **Events are found, not raised.** After every key or job result, housekeeping
  compares the documents' versions and saves, the mode and the focused buffer with what
  it saw last time. No code path that edits or saves has to remember to tell the
  plugins.
- **Actions are named.** A picker names its primary (`ret`) and secondary (`del`)
  actions, and `tab` marks items for both to act on. A plugin's picker names its own
  actions, which read `chosenItems`.
- **State lives in the editor.** It is a `Dynamic` per plugin, so two editors (or two
  tests) never share it.

**▶ Task 7d.** Write a plugin that shows, as an annotation on the cursor's line, how
many times the word under the cursor occurs in the buffer. Which event do you need,
and what happens to your annotation when the cursor moves to another buffer?

---

## Part 6: Benchmarking against Vim and Helix

You can't optimize what you don't measure, and you can't compare editors with
`hyperfine`: they are interactive. `bench/bench.py` (Python standard library only; a dev
tool, not part of the editor) runs each editor the way a person would. It has four
parts.

1. **A pseudo-terminal** (`pty.fork`) of 120×40, with `TERM=xterm-256color`.
2. **Answers to terminal queries.** Vim and Helix ask the terminal about itself at
   startup: device attributes, cursor position, colours, DECRQM, XTGETTCAP. A real
   terminal answers. One wrong answer made Vim wait 100 ms when exiting. Vim's
   ambiguous-width check prints `▽` and asks for the cursor position, expecting
   column 2. The harness now tracks where the editor last moved the cursor and answers
   like a terminal would.
3. **A careful definition of "ready".** Raw mode is not enough, because editors still
   exchange queries with the terminal, and Helix drops keys typed meanwhile. The harness
   waits until output has been quiet for 100 ms.
4. **Measurements.** Wall time with pauses subtracted (after `Esc`, so `Esc :` isn't
   read as `Alt-:`), per-key latency to the last byte of the redraw, and peak RSS and
   CPU from `wait4()`. Results are *verified*: the edit scenario checks the saved file
   byte for byte, and the search scenario types `HIT` at the match and saves.

Scenarios: startup, opening a 14 MB file, 2000 × `j` sent at once, jumps, typing and
saving, per-key latency, far/no-match search, and `n` through many matches.

---

## Part 7: Making it fast and small

The first benchmark was humbling:

| | him | vim | helix |
|---|---:|---:|---:|
| 2000 × `j` (ms) | 2809 | 61 | 666 |
| per-key latency (ms) | 4.6 | 0.3 | 1.4 |
| peak memory after editing (MB) | 88 | 37 | 67 |

Each optimization below follows the same loop: **measure, find the real cost, fix
that, measure again.** Some guesses turned out to be wrong, and those are worth
reading too.

### 7.1 Render once per batch of input ([ADR render-per-batch](adr/render-per-batch.md))

**Finding.** 2000 queued `j` presses produced 2000 full renders. Vim skips redraws while
typeahead is pending.

**Fix.** The input thread puts all keys decoded from one read on an STM `TChan`. The main
loop handles everything that's already queued (up to 512 events) and then renders once:

```haskell
batch n ed ev = do
  ed' <- step ed ev
  if edQuit ed' || n <= 1 then pure ed' else
    atomically (tryReadTChan events) >>= \case
      Nothing  -> pure ed'
      Just ev' -> batch (n - 1) (ensureCursorVisible ed') ev'
```

`Chan` from `base` can't be checked without blocking, hence `stm`. **Result:** 2000 × `j`
went from 2809 to 26 ms. That's 100× faster, and already faster than Vim.

### 7.2 Don't build frames cell by cell

**Finding.** Rendering wrote every cell with `Seq.adjust (Seq.update col cell) row`:
4,800 small tree updates per frame.

**Fix.** Write whole runs with one splice per row:

```haskell
putCells row col cells f = f { frameCells = Seq.adjust' splice row (frameCells f) }
  where
    start = max 0 col
    visible = take (frameCols f - start) (drop (start - col) cells)
    splice r = Seq.take start r <> Seq.fromList visible <> Seq.drop (start + length visible) r
```

The text area builds each visible line as one list of cells. **Result:** render went
from 1.43 to 0.75 ms.

### 7.3 Let the profiler choose

GHC ships profiling libraries, so `stack build --profile --work-dir .stack-prof` plus a
small driver that calls `render` in a loop gives a cost-centre profile without any
downloads. The top two entries were surprises:

- **`isWide` (19%).** It was a linear scan over about 40 Unicode ranges for every
  character. Fix: nothing below U+1100 is wide, so return immediately; the rest is an
  `IntMap.lookupLE`.
- **Selection checks (15%).** `contains`/`min`/`max` ran per character per range. Fix:
  compute each line's selected column intervals once per row.

**Result:** render went from 0.75 to 0.54 ms. A guess would not have found `isWide`.

### 7.4 Memory: a rope of blocks

**Measure first.** `him +RTS -s` reports *live data* (what is actually reachable) and
*memory in use* (what the GC holds) separately. With the 14 MB file:

| | live | in use |
|---|---:|---:|
| open | 17.5 MB | 34 MB |
| edit and save | 30.7 MB | 83 MB |

Saving was the first culprit. `encodeDocument` built the whole file as one `Text` and
then one `ByteString`, which is 28 MB of temporary copies. **Fix:** stream the lines
straight to the file through a `Builder`.

**Fix: read in chunks into one reused buffer.** Each chunk's complete UTF-8 prefix is
decoded, and an incomplete character at the end is carried to the next read:

```haskell
loop buf pending acc = do
  n <- hGetBuf h (buf `plusPtr` pending) (chunkSize - pending)
  bytes <- unsafePackCStringLen (castPtr buf, pending + n)
  let cut = completePrefix bytes
  !text <- evaluate (decodeUtf8Lenient (B.take cut bytes))   -- the only copy
  ...
  copyBytes buf (buf `plusPtr` cut) (pending + n - cut)       -- keep the partial character
```

**The surprise.** Peak memory got *worse*. A heap profile by closure type
(`+RTS -hT`, no profiling build needed) showed 28 MB of byte arrays for 14 MB of text.
The culprit was `T.breakOnEnd`, used to find a chunk's last newline. In `text` it's
implemented by *reversing the whole input*, so every chunk was copied twice, and the
lines ended up as slices of the copies. `T.dropWhileEnd` and `T.takeWhileEnd` scan from
the end and return slices.

**The real fix ([ADR rope-buffer](adr/rope-buffer.md)).** Even with all of that fixed, live data was about 24 MB: 14 MB
of text plus about 10 MB of *line objects*. Every line in `Seq Text` costs a `Text`
constructor and finger-tree nodes, about 50 bytes, 200,000 times over. And the copying
GC has to copy all of them, which doubles the space. So the buffer became a **rope of
multi-line blocks**:

```haskell
-- Lines first .. first+count-1 of one Text; line k spans [start k, start (k+1) - 1).
data Block = Block
  { blkText :: !Text, blkStarts :: !Offsets   -- Word32 line starts, filled by C (memchr)
  , blkFirst :: !Int, blkCount :: !Int, blkCR :: !Bool }

-- A weight-balanced tree (Adams; Hirai & Yamamoto's (3,2) parameters), caching line counts.
data Rope = Tip | Bin !Int !Int !Rope !Block !Rope
```

The structure works like this:
- **Per line:** a line now costs 4 bytes of offset. A loaded file is about 30 big blocks,
  one per read chunk.
- **Lookup:** `ropeLineAt` descends the tree by cached line counts in O(log blocks), then
  slices the block's text, so nothing is copied.
- **Edits:** `ropeSplitAt` splits a block by adjusting `blkFirst`/`blkCount`, in O(1),
  sharing the offsets array. The changed lines become a new small block.
- **Interface:** `Him.Buffer` exposed the same functions as before, so nothing else in
  the editor changed. The rewrite came with a randomized model test (2,000 random edits
  compared with a list of lines after every step), and the existing tests passed
  unchanged.

Finally, the **non-moving GC** (`-with-rtsopts=-xn`) needs no room for a copy of the old
generation. It was measured against `-c`, `-F1.2` and `-A1m`; those either saved less or
cost latency.

| Peak RSS | before | after | vim |
|---|---:|---:|---:|
| open 14 MB file | 38.6 MB | **24.1 MB** | 37.2 MB |
| edit and save | 88.5 MB | **35.4 MB** | 37.1 MB |

### 7.5 Search: scan blocks, anchor on the rarest byte ([ADR c-byte-loops](adr/c-byte-loops.md), [ADR literal-search](adr/literal-search.md))

Searching per line would mean 200,000 calls. With the rope, a whole block, thousands of
lines joined by their original newlines, is one contiguous byte range. A needle never
contains a newline, so a match can't cross lines, and one C call scans the entire block.
The byte offset of a hit maps back to a line by binary search over the block's line
starts.

The C functions take the `Text`'s internal array directly. The calls are `unsafe`, so
the GC can't move the array during the call, and `UnliftedFFITypes` lets us pass a
`ByteArray#`:

```haskell
foreign import ccall unsafe "him_find_forward"
  c_findForward :: ByteArray# -> CSize -> CSize -> ByteArray# -> CSize -> CSize -> CInt -> CPtrdiff
```

- **Exact search** is glibc's `memmem` (two-way, vectorised), which is hard to beat.
- **Case-insensitive search** (smart case) took three tries:
  1. `memchr` on the needle's *first* byte, then verify. That is fast for `zzz` and slow
     for `0199999 lorem`, because `0` is on every line of the test file.
  2. Case-folding **Boyer–Moore–Horspool**. Measured: barely better on the bad case and
     **4× slower** on the good one (5.7 → 23.6 ms), so it was dropped.
  3. **Anchor on the rarest byte** (the idea behind ripgrep's prefilter). Count byte
     frequencies in a 4 KB sample of the block, pick the needle byte that is rarest *in
     this text*, and scan for it (both cases) 16 bytes at a time with SSE2:

     ```c
     __m128i x = _mm_loadu_si128((const __m128i *)p);
     int m = _mm_movemask_epi8(_mm_or_si128(_mm_cmpeq_epi8(x, va), _mm_cmpeq_epi8(x, vb)));
     if (m) return p + __builtin_ctz(m);
     ```

     Then verify each candidate window. The sample makes this adapt to whatever file is
     open.

On the 14 MB file in a tight loop, a match on the last line takes 1.4 ms, and "no match"
(two full scans, because of the wrap-around) takes 0.6 ms.

**A lesson about measuring.** Inside the editor the same search took about 6 ms. The
code was the same, so the cause was the CPU: the `powersave` governor parks idle cores
at 800 MHz, and a search that arrives after a pause starts on a slow core. All editors
pay this, but it explains why tight-loop numbers and real latency differ.

### 7.6 Rendering: write only what changed ([ADR row-reuse-and-scrolling](adr/row-reuse-and-scrolling.md))

Comparing bytes per `n` (a jump of 1,000 lines) was revealing: Vim wrote 748 bytes,
Helix 1,867, and him **5,681**. Neighbouring matches look nearly identical on screen,
and Vim and Helix only send the cells that changed. Four fixes followed:

1. **Cell-level diff.** In a changed row, emit only the runs of changed cells. Runs
   closer than 6 cells are merged, because a cursor move costs about as much. Trailing
   blanks are cleared with one `EL` (`ESC[K`) instead of being written as spaces.
2. **Row reuse.** Even a key that changes nothing cost about 2.7 ms, because the whole
   frame was rebuilt. Now each text-area row records a `RowKey`: line number, text,
   selected spans, cursors, horizontal scroll, and width. If the previous frame drew the
   same key, the row is copied with one `Seq` slice:

   ```haskell
   | Just p <- prevFrame
   , Map.lookup (prevRowOf screenRow, rectCol rect) (frameRowKeys p) == Just key
   = remember (copyCells (prevRowOf screenRow) screenRow (rectCol rect) (rectWidth rect) p f)
   ```

   `prevRowOf` looks the row up *by line* (`screenRow + top - prevTop`), so rows survive
   scrolling. (The key is a row *and* a column since splits put windows side by side,
   [ADR window-splits](adr/window-splits.md).)
3. **Terminal scrolling.** When the view moves by less than a screen, the diff sets a
   scroll region over the text area and scrolls it (`ESC[1;38r`, `ESC[1S`). It then
   compares the new frame with the *shifted* old one, so a `j` that scrolls writes one
   new line instead of 38.
4. **ASCII fast path.** Lines of printable ASCII skip the general layout: one cell per
   character, no tab or width logic. A full redraw now renders in 0.24 ms instead of
   0.62 ms.

**How do you trust a clever diff?** With a model. The tests contain a 60-line terminal
emulator. It understands cursor moves, SGR, `EL`, clear screen, scroll regions, scrolling,
and wide characters. 300 random frame sequences are diffed, replayed on the model, and
compared cell by cell and style by style with the new frame. On its first run it failed,
and the bug was in the test: random overlapping writes produced half wide characters,
which the real renderer never does. The generator now produces only valid frames.

### 7.7 The second pass: catching Helix everywhere (strategies 14–20)

After 7.6, him still lost to Helix when opening a large file, on edit-and-save, and
tied on `n`. Every fix below was measured in isolation first. The full log is in
`docs/BENCHMARK.md`.

**Opening (51 → 15–30 ms first paint; see `docs/BENCHMARK.md`).**
1. **Count lines with the scan you already do.** `T.count "\n"` is a general substring
   search. Counting newlines inside the C scan that finds line starts made
   `loadDocument` 25 → 5.4 ms.
2. **SIMD counting and lazy offsets.** The newline count compares 16 bytes at a time
   and sums the hits with `_mm_sad_epu8`. A block's line-start array is a lazy field, so
   it is only built when the block is shown or searched. The first paint only needs the
   count.
3. **Zero copy ([ADR lazy-file-loading](adr/lazy-file-loading.md)).** A regular file is read into one pinned array of exactly its
   size. If the bytes are valid UTF-8, that array *is* the `Text`. There is no decode
   step and no copy. Invalid files and pipes still take the lenient and chunked paths,
   and tests force both.

**Saving (14.7 → 3.9 ms).**
4. **Write regions, not lines.** The rope already stores many lines contiguously. A
   region whose line endings match the file is written in one piece, instead of 200,000
   `encodeUtf8Builder` calls.
5. **`hPutBuf` from pinned arrays.** A large region from a loaded file goes to the
   handle directly, without the builder's copy. That saved little, because the rest is
   the kernel's `write`. It is still logged, because a small effect is a result too.

**Rendering (`n` 2.5 → about 1.4–2.0 ms, measured on a busy machine).** Measured inside the editor on a slow-clocked core,
the cell diff cost more than rendering itself.
6. **One pass over the cells.** `drawChanges` walks the old and new row together and
   builds merged runs as it goes. Before, it used `zip3`, `drop`/`take` per run, and two
   `reverse`s.
7. **Unboxed cells.** A `Cell` was a boxed `Char` plus a pointer to a six-field `Style`.
   Now the style is packed into one `Word64` (two 26-bit colours plus four flags), and
   `Cell` unpacks to `Char#` and `Word64#`. Comparing two cells is two machine
   compares.

The lesson of this pass: **look at the end-to-end number, but find the cause with a
micro-benchmark.** Each micro result (diff 0.23 → 0.18 ms) looks too small to matter.
Inside the editor, on a CPU that clocks down between keys, the same change took a
millisecond off every `n`.

### 7.8 Where it ended up

Median of five runs, 200,000-line (14 MB) file, same machine. "Now" is 2026-10-02,
with themes, plugins, splits and all (every plugin on; `docs/BENCHMARK.md` has the
plugins-off and IDE-style runs):

| Scenario | first version | now | vim | helix |
|---|---:|---:|---:|---:|
| startup (ms) | 6.3 | **~12** | 24.0 | 30.2 |
| open 14 MB: first paint (ms) | — | 30.2 | 34.8 | **22.8** |
| open 14 MB: peak RSS (MB) | 38.6 | **30.2** | 37.2 | 46.8 |
| 2000 × `j` at once (ms) | 2809 | **≈20** | 62.2 | 628 |
| 100 × jump to end and back (ms) | 505 | **7.0** | 34.3 | 114 |
| edit and save (ms) | — | **13.0** | 22.7 | 18.2 |
| per-key latency `j` (ms) | 4.6 | 1.3 | **0.5** | 2.0 |
| per-key latency typing (ms) | 4.7 | 1.1 | **0.3** | 1.9 |
| search, match at the end (ms) | — | **11.2** | 30.3 | 25.7 |
| search, no match (ms) | — | **5.1** | 24.9 | 47.6 |
| `n` (ms) | — | 2.8 | **1.6** | 2.8 |

The remaining gaps are documented in `docs/BENCHMARK.md`:
- **Opening large plain files** is slower than Helix (30 vs 23 ms).
- **Per-key latency** is about 2–3× Vim's.

Each has a concrete next step.

### Lessons

1. **Design for replacement.** The buffer was swapped for a rope and nothing else
   changed, because callers never saw `Seq`.
2. **Keep the core pure.** Every optimization was checked against tests that call
   pure functions: motions, edits, decoding, search, diff replay.
3. **Measure the thing, not your guess.** The profiler found `isWide`. A heap profile
   found `T.breakOnEnd`. A byte count found the diff problem. Horspool looked better on
   paper and measured worse.
4. **Check that benchmarks measure what you think.** An early render benchmark drew
   identical frames and so never measured a redraw. Helix "failed" a check because it
   only redraws changed cells. Vim was slow at exit because the fake terminal answered
   wrong.
5. **Model-based tests make clever code safe.** The random-edit model for the rope, the
   naive search for the C scanner, and the terminal emulator for the diff each turned a
   risky optimization into a routine change.

---

## Where to go next

The editor is deliberately unfinished. Good next exercises, in increasing difficulty:

- **Tree-sitter text objects:** `m i f` (inside a function) like Helix, from the
  syntax tree the highlighter already has.
- **Highlight all matches:** a render pass over the visible rows. Remember to add the
  highlight to `RowKey`.
- **Regex search:** write a small backtracking or Thompson-NFA engine. `Him.Search` only
  needs a block-level matcher, so the rest stays as it is.

`docs/PLAN.md` records every decision made so far and where to pick up.
