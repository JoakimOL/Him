# him — project plan & status

This is the living design document for **him**. It is updated at the end of every milestone.
**New here? Read [Where to pick up](#8-where-to-pick-up) first.**

---

## 1. Overview & goals

him is a modal text editor for the terminal, inspired by Helix and Vim.

Goals:
- **Terminal only.** It draws with ANSI escape sequences in raw mode. There is no GUI.
- **Selection-first editing (Helix-style).** You select, then act (`w` then `d`, not `dw`).
- **Few dependencies.** Only GHC boot libraries; nothing from Hackage.
- **Modular and easy to extend.** Adding a command, a keybinding, or a UI component
  should mean adding code in one obvious place, without changing the core loop.
- **Testable.** The editing logic is pure, and the IO layer is thin.

Non-goals (for now): GUI, plugin runtime, LSP, Windows support.

## 2. Assumptions

- Linux or another POSIX system, with a terminal that understands xterm/ANSI escape sequences
  (alternate screen, SGR colours, cursor-shape `DECSCUSR`).
- The terminal and the files are UTF-8. Invalid bytes are decoded leniently (replacement char).
- Files are small enough to hold in memory as a sequence of lines.
- There is one user, one process, and no concurrent editing of the same file.
- Toolchain: **GHC 9.10.3** via Stack snapshot **LTS 24.60**, and
  **haskell-language-server 2.14** (it ships a 9.10.3 binary). These must be bumped together.
- Stack 2.15.7 prints "not tested with GHC 9.10" warnings. They are harmless;
  `stack upgrade` removes them.

## 3. Architecture decisions

Each entry lists the decision, why it was made, and the alternatives considered.

**ADR-1: Helix-style selection-first editing.**
Every motion produces a selection, and actions operate on selections. This makes the result
of a command visible before it is applied, and multiple cursors fall out naturally.
*Alternative:* Vim's verb→object grammar. It is simpler at first, but retrofitting
selections onto it later is harder.

**ADR-2: GHC boot libraries only.**
Allowed: `base`, `unix`, `bytestring`, `text`, `containers`, `transformers`, `directory`,
`filepath`, `stm`, `array`. (`filepath` is used since milestone 19.) Each one is added to `package.yaml` only when a module first uses
it (`-Wunused-packages` enforces this).
*Alternatives:* `vty`/`brick` (large and opinionated), `text-rope` (see ADR-3).

**ADR-3 (superseded by ADR-12): `Seq Text` line buffer behind an abstract interface.**
`Him.Buffer` exposes operations (`lineCount`, `getLine`, `insertAt`, `deleteRange`, …) and
hides its representation. `Data.Sequence` gives O(log n) splitting and indexing by line,
which is plenty for normal files.
*Alternative:* a rope. It can replace the internals later without changing any callers.

**ADR-4: Pure core, thin IO shell.**
Buffers, motions, edits, selections, keymap resolution, and rendering to a `Frame` are all
pure. Only `Him.Terminal.*`, `Him.File`, and `Him.App` perform IO.

**ADR-5: Commands are named values in a registry. Keymaps are tries of command names.**
*(Commands became actions with arguments; see ADR-17.)*
`Command { cmdName, cmdDoc, cmdRun :: EditorM () }` with `EditorM = StateT Editor IO`.
Keys are bound to command *names*, so bindings can later be loaded from a config file.
The trie supports multi-key chords (`g g`, `space f`). Resolving a key sequence gives
`Found | NeedMore | NoMatch`.
*Alternative:* pattern-matching on keys in one big `case`. It is fast to write but does not
extend or rebind.

**ADR-6: Render to a pure `Frame`, then diff.**
Components (`TextArea`, `Gutter`, `StatusLine`, `CommandLine`) each draw into a `Rect`.
The new frame is compared row by row with the previous one, and only changed rows are
written, in one `Builder` per frame. This avoids flicker and keeps redraws cheap.

**ADR-7: Terminal size through a C shim.**
The `unix` package does not expose `ioctl(TIOCGWINSZ)`. `cbits/winsize.c` wraps it in a
single function, and `SIGWINCH` triggers a resize event.
*Alternative:* the cursor-position-report trick (`ESC[999C ESC[6n`). It is slower and racy.

**ADR-7b: Read input from the file descriptor, never through the `stdin` Handle.**
If `hSetBuffering stdin` or `hSetEcho` is called on a tty, GHC saves the termios state
itself and restores *that* state when the program exits. When those calls happen after
we enter raw mode, the saved state is already raw, so it undoes our restore. We found this
in milestone 2. Input is read with `System.Posix.IO.ByteString.fdRead` on `stdInput`
(after `threadWaitRead`, which keeps it interruptible for timeouts).

**ADR-5b: Details of the selection model.**
Ranges are *inclusive* (a cursor covers the character under it), and `col == lineLength`
addresses the line end (the newline). In insert mode the head is read as a gap: text is
inserted before the character at the head. Edits apply to every range (ADR-18).

**ADR-6b: Components are `Theme -> Editor -> Rect -> Frame -> Frame`.**
This replaces the `[DrawOp]` lists in the original plan: composing frame transformers is
simpler and just as modular. The mode `Command` was renamed `CmdLine` because it clashed
with the `Command` type.

**ADR-9: Undo with snapshots, committed outside insert mode.**
`edit` records the state before the first edit of a change. After every key, the main loop
commits it if the editor is not in insert mode. So `c foo esc` undoes in one step, as in
Helix. Snapshots are cheap because `Seq` shares structure. `docSavedBuffer` lets undo
back to the saved text clear the `[+]` marker.
*Alternative:* inverse change sets, which are smaller and needed for an undo tree or
collaboration. They can come later behind the same `Him.History` interface.

**ADR-10: Registers live in the `Editor`, and pasting is linewise when the text ends
with a newline.**
This is the Helix/Vim convention. `selectionText` adds the implicit newline when `x`
selects the last line, so yank/delete/paste of lines behaves the same everywhere.

**ADR-11: Render once per batch of input.**
The input thread queues all keys decoded from one read on a `TChan` (from `stm`). The
main loop handles every queued event (up to 512) before rendering once. That's what
makes typeahead (pastes, key repeat) cheap. Components write whole rows
(`putCells`) instead of single cells.

**ADR-12: The buffer is a rope of multi-line blocks.**
A `Block` is one UTF-8 `Text` holding many lines, plus an array of `Word32` line starts.
Blocks live in a weight-balanced tree that caches line counts (`Him.Buffer.Rope`).
- **Loading:** a file loads as a few large blocks, one per 1 MB read chunk.
- **Edits:** splitting a block is O(1) slicing; changed lines become a small new block.
- **Why:** with `Seq Text`, each of the 200,000 lines of the test file cost about
  50 bytes of heap objects. The copying GC also had to copy all of them.
- **Search:** blocks make search fast, because a whole block is scanned with one C call.
- **Interface:** `Him.Buffer`'s interface did not change. A randomized test compares
  thousands of edits against a list model.

**ADR-13: Hot byte loops in C, called with `unsafe` FFI on the `Text`'s array.**
`cbits/text.c` scans newlines (`memchr`) and searches (`memmem`, SSE2 two-byte scan).
`unsafe` calls cannot be interrupted by the GC, so the unpinned arrays can be passed
directly (`UnliftedFFITypes`). This keeps the editor to boot libraries plus two small C
files.

**ADR-14: Search is literal, smart case, and anchored on the rarest byte.**
- **Matching:** there is no regex engine, since none ships with GHC. A pattern
  without upper-case letters matches ASCII letters case-insensitively.
- **Exact matches:** these use glibc `memmem`.
- **Case-insensitive matches:** these scan for the needle byte that is rarest in a
  4 KB sample of each block, in both cases, 16 bytes at a time, and verify each
  candidate.
- **Why not Boyer–Moore–Horspool:** it was tried. It was 4× slower when the first
  byte was rare, and only slightly faster when it was common.
- **Incremental preview:** this runs once per input batch (`edPreviewPending`), not
  once per key.

**ADR-15: The renderer reuses rows and lets the terminal scroll.**
`render` takes the previous frame. Text-area rows are keyed (`RowKey`: line, text,
selection spans, cursors, scroll, width) and copied when unchanged, looked up by line so
scrolling keeps them. When the view moved less than a screen, `diffFrames` scrolls the
terminal's region (`DECSTBM` + `SU`/`SD`) and diffs against the shifted old frame. It
writes only changed cell runs and clears trailing blanks with `EL`. Tests replay the
output on a small terminal model with scroll regions and compare cell by cell.

**ADR-16: Load files without copying, and index blocks lazily.**
A regular file is read into one pinned array of its size. If it is valid UTF-8, that
array becomes the buffer's `Text` directly, so nothing is decoded or copied.
- **Fallbacks:** invalid bytes get a lenient decode. Files of unknown size (pipes
  report 0) are read in chunks.
- **Lazy offsets:** a block's line starts are built on first use. Only the line count,
  an SSE2 newline count, is needed up front.
- **Testing:** the chunked path is checked through `loadDocumentChunked`.

**ADR-17: Keys bind to actions: named, grouped, with typed arguments (supersedes the
`Command` registry of ADR-5).**
An `Action` (`Him.Action`) has a stable snake_case name, a group, a doc string, and typed
positional parameters. A binding is text: the action's name plus arguments, such as
`move_line_down 5`, `goto_line 12`, `insert_text "// "` or `ex "w"`.
- **Names are the stable interface.** Bindings, and later config files, refer to the flat
  name. Helix uses the same flat names, so they stay familiar. The group (movement,
  selection, modes, editing, clipboard, history, search, prompt, misc) is metadata for
  help and docs only, so moving an action between groups breaks nothing. Renaming an
  action is a breaking change: keep the old name as a second action if it ever happens.
- **Arguments are typed, and are checked when the keymap is built.** Parameters are
  described with a small applicative (`int`, `text`, `choice`, `optional`). The same
  value lists the parameters (`actParams`, for help or a config UI) and converts the text
  arguments. Binding produces a `Bound` (the invocation plus the `EditorM ()` to run). So
  a key press neither looks anything up nor parses anything, and a bad binding is an
  error at startup that names the mode, the keys and the problem. Every error is
  reported, not only the first.
- **Keymaps are generic.** `Keymap a` is a trie of any binding type: `Keymap Text` while
  parsing and `Keymap Bound` at run time.
- **Prepared for a config file.** `Bindings = Map Mode [(keys, invocation)]`.
  `overrideBindings user defaults` puts user bindings on top, and `no_op` disables a key.
  `buildConfig actions bindings fallback` validates and builds everything. Select mode
  inherits normal mode's bindings, the user's included (`inheritsFrom`). A config parser
  only has to produce `Bindings` and call `Him.Config.Default.configWith`.
- **Invocation syntax.** Words are separated by spaces. A double-quoted argument may
  contain spaces and the escapes `\"`, `\\`, `\n` and `\t`. `renderInvocation` is the
  inverse.
*Alternatives:* separate names per argument value (`move_line_down_5`), which does not
scale. A `Value` sum type checked inside each action at run time, which reports errors
only when the key is pressed. Arguments stored per key in the keymap and passed on each
press, which is the same thing with an extra lookup.
**Counts** (`5 j`, `1 2 j`, `2 w`): in normal and select mode, digits typed before a key
sequence build `edCount`, which the status line shows. A binding without arguments
whose action's first parameter is `int "count"` runs with the count (`boundCounted`).
Other bindings ignore it, as does a binding that already gives arguments
(`move_line_down 20`). `0` only continues a count, and a digit that the keymap binds
keeps its binding. The count is capped at 1,000,000.
*Later:* `:` could gain a command that runs any action by its invocation text.

**ADR-18: Multi-range edits are applied from the bottom up, and positions are kept
relative to the end.**
`Him.Edit.applyEdits` runs an ordinary single-range `Edit` on each range, from the last
range to the first. An edit only changes text around its own range, which lies before
every range already edited. So each result is kept as (lines from the last line,
characters from the end of its line). Those two numbers are unaffected by any change
earlier in the buffer, and they turn back into positions at the end. That avoids
change sets and position mapping, and every existing `Edit` works with many cursors
unchanged. A single range takes a direct path, so typing costs what it did before.
- **Normalizing:** `Selection.fromRanges` sorts the ranges and merges overlapping ones.
  It runs before edits and after motions, so the bottom-up order is well defined.
- **Registers:** a register holds one value per range. Pasting with as many values
  as ranges gives each range its own value; otherwise every range gets all of them,
  joined.
- **Adjacent ranges:** an edit may reach just outside its own range. A backspace
  deletes the character before the cursor, and deleting the last lines takes the line
  break before them. Either can touch the text of the range before it, but only after
  that range's start, so that range's positions stay valid when its turn comes. The
  stored results of later ranges lie after the change, so they are not affected either.
  Randomized model tests cover inserts, backspaces, forward deletes, and range deletes
  with ranges right next to each other. Cursors that end up on the same position are
  merged. (An earlier version of this ADR listed adjacent cursors as a known limit.
  That was wrong: the tests show the same results as the string model.)
*Alternative:* change sets with position mapping (as in Helix). They are more general,
and needed for an undo tree or collaboration, but they are much more code.

**ADR-19: Buffers are a zipper around the current document.**
The `Editor` keeps the current document in `edDoc` (with `edView`), as before, plus
`edBefore` (nearest first) and `edAfter`: the other buffers, each with its own view.
Code that works on the current document did not change. Switching moves documents
between the lists (`switchBuffer`, `gotoBuffer`), `:open` inserts after the current
buffer, and closing the only buffer leaves a scratch buffer. Each document keeps its
own selection and undo history. `:open` compares canonical paths, so a file that is
already open is switched to rather than loaded twice. `:q` refuses while any buffer is
modified.
*Alternative:* a `Seq Document` plus an index. That would change every `edDoc` access.

**ADR-20: Menus are data computed after every key; popups invalidate the rows they
cover.**
- **Info box:** after each event, `Him.Info.refreshInfo` sets `edInfo :: Maybe InfoBox`
  from the editor and the config. After a prefix (`g`, `space`), the box lists the keys
  below it in the keymap trie and each action's doc. Prefix titles come from
  `cfgPrefixNames`. On the `:` line it lists the matching ex commands (`cfgExCommands`).
  The box is derived from state, never edited, so it cannot go stale. Rendering
  (`Him.Render.Info`) only draws it.
- **Completion:** `tab` on the `:` line completes the command name, or a path for
  commands whose `exArgs` is `PathArgs`. With several candidates it extends the line to
  their common prefix and lists them (`edCompletions`, cleared when the line changes).
- **Pickers:** a `Picking` mode with its own keymap, and a fallback that types into the
  query. `Him.Picker` is pure: items carry a `PickTarget` (a file or a buffer index)
  rather than an action, so the editor state stays plain data. The fuzzy score counts
  the characters skipped between the first and last match, from the best start; ties
  go to the shorter label.
- **Row cache:** a popup draws over text-area rows, and the next frame could copy those
  rows (popup included) from the cache (ADR-15). So every popup deletes the row keys of
  the rows it covers. A test renders a frame with a box, then one without, and
  compares it with a fresh render.

**ADR-21: Our own gitignore matcher, applied while walking.**
`Him.Ignore` parses `.gitignore` / `.ignore` files with git's syntax:
- comments, `!` to re-include, and a trailing `/` for directories only;
- a `/` at the start or in the middle anchors the pattern to the file's directory;
- globs `*`, `?`, `[a-z]` / `[!a-z]`, and `**` (`**/x`, `x/**`, `a/**/b`).

Rule sets are scoped to the directory of their file, and the last match of the most
specific set wins. `.ignore` is read after `.gitignore` in the same directory, so it wins
there. `Him.FileTree.listFiles` reads each directory's files as it descends, and never
enters an ignored directory, so (as in git) nothing inside one can be re-included. It
also applies the ignore files of the walk root's ancestors, up to the enclosing git
repository, and `.git/info/exclude`.
- **Differences from ripgrep/Helix:** `.gitignore` is honoured outside git
  repositories too, and the global gitignore (`core.excludesFile`) is not read.
- **Hidden entries** are still skipped, as Helix does by default.

*Alternative:* translate patterns to a regex engine. There is none in the boot
libraries, and a direct backtracking matcher over path strings is about 40 lines.

**ADR-22: A directory is a read-only document, plus a keymap layer.**
`:open dir` (or `him dir`, `space d`, `space D`) loads a listing, as in Emacs's dired.
- **The document:** a `Document` whose `docKind` is `DirectoryDoc entries`. Line 0 is the
  path, line 1 is `../`, then subdirectories (ending in `/`) and files, sorted. The
  entries are kept in the kind, so a line maps to its entry even when a name contains
  odd characters. Since it is an ordinary buffer, motions, counts, search and the
  buffer commands work unchanged.
- **The keys:** in normal mode on a listing, `keymapMode` selects the `Directory` layer,
  which inherits normal mode like select mode does. It adds `ret` (enter a directory in
  the same buffer, or open a file as a new buffer), `-` / `backspace` (the parent, with
  the cursor on the directory just left) and `g r` (list again). The status line shows
  `DIR`.
- **Read-only:** `edit` / `editEach` and `setMode Insert` refuse in a read-only document,
  and `:w` refuses to write a listing.
- **Colours:** the header and directories have their own styles. The row key gained
  the line's class (`rkClass`), so a cached row is never reused with the wrong colour.

- **File operations** (milestone 20): `a` (new file; a trailing `/` makes a directory,
  and missing parents are created), `+` (new directory), `r` (rename or move; the
  prompt starts with the current name) and `d` (delete). `d` deletes every entry the
  selections cover, so `x x d` or `% s` work, and asks for `y` first. Directories are
  deleted recursively, but a symlink is only unlinked, never followed (tested). The
  names are typed on the command line through a `FilePrompt` with a `FileAction`, so
  editing the name uses the ordinary command-line keys. After a rename, buffers showing
  the old path (or something inside a renamed directory) take the new path.
- **Dotfiles** are hidden by default, like the file picker. The header counts them,
  and `g .` (`edShowHidden`, shared by all listings) shows them.

*Alternatives:* a separate directory UI component, which would have to reimplement
movement and search. Editing the listing text and applying the difference (as Emacs's
wdired and oil.nvim do) would allow batch renames, but it needs a careful diff and
confirmation; prompts were simpler and safer first. Or a real editor mode, which every `setMode Normal` would have to
know about.

**ADR-8: No test framework.**
`test/Test/Harness.hs` is about 50 lines and does `test`, `group`, `assertEqual`, and
`runTests`, which keeps us within the boot libraries. hspec/tasty can be adopted later
if needed.

## 4. Module map

Legend: ✅ exists, ⏳ planned.

| Module | Status | Responsibility |
|---|---|---|
| `Him.App` | ✅ | Main loop: event → keymap → command → render. `handleEvent` is exported so tests can drive it. |
| `Him.Log` | ✅ | `logMsg`, which appends to the file named by `$HIM_LOG`. It is a no-op when unset. |
| `Him.Terminal.Size` + `cbits/winsize.c` | ✅ | `getWindowSize :: IO (Maybe (Int, Int))`, returning (rows, cols); `onResize` installs the SIGWINCH handler. |
| `Him.Terminal.Raw` | ✅ | Raw mode + alternate screen; `withRawTerminal` always restores the terminal. |
| `Him.Terminal.Ansi` | ✅ | Pure `Builder`s for escape codes (cursor, clear, SGR, cursor shape). |
| `Him.Terminal.Output` | ✅ | Writes one builder per frame and flushes. |
| `Him.Terminal.Input` | ✅ | Reader thread → `TChan Event` (from `stm`, so the main loop can drain queued events before rendering); pure `decodeKeys final bytes`; a 30 ms lone-ESC timeout. |
| `Him.Key`, `Him.Event` | ✅ | Key/modifier types and a `"C-s"`-style key parser; the `Event` sum type. |
| `Him.Buffer.Rope` | ✅ | Blocks (a `Text` plus line starts) in a weight-balanced tree with line counts: split, append, line lookup, block iteration. |
| `Him.Native` + `cbits/text.c` | ✅ | `unsafe` FFI on `Text` arrays: line starts, newline count, forward and backward search. |
| `Him.Search` | ✅ | `compileNeedle` (smart case) and `findMatch` (direction, wrap-around), on top of `Him.Buffer.findForwardFrom` / `findBackwardBefore`. |
| `Him.Commands.Search` | ✅ | `/ ? n N *`, the search prompt, and `refreshSearchPreview`. |
| `Him.Buffer` | ✅ | Abstract text storage (`Seq Text`), path, dirty flag. |
| `Him.Position`, `Him.Selection` | ✅ | `Pos`, `Range {anchor, head}`, `Selection` (sorted, merged NonEmpty ranges + primary; `fromRanges`, `normalize`, primary operations). |
| `Him.Motion` | ✅ | Pure motions: char, line (desired column), word, line/file start/end. |
| `Him.Edit` | ✅ | Pure single-range edits, and `applyEdits`, which applies one to every range (ADR-18). |
| `Him.Editor`, `Him.Mode`, `Him.View` | ✅ | Editor state (the buffer zipper, ADR-19; `InfoBox`; the open picker), modes (`Normal`, `Insert`, `Select`, `CmdLine`, `Picking`), viewport + scrolloff. |
| `Him.Info` | ✅ | `refreshInfo`: the info box after a prefix key or on the `:` line (ADR-20). |
| `Him.Ignore`, `Him.FileTree` | ✅ | The gitignore matcher (pure), and the ignore-aware breadth-first file walk for the picker (ADR-21). |
| `Him.Directory`, `Him.Commands.Directory` | ✅ | Directory listings as read-only documents (`loadPath`, `loadDirectory`, `entryAt`, `selectEntry`), and their actions (ADR-22). |
| `Him.Picker`, `Him.Commands.Picker` | ✅ | Pure picker (fuzzy matching, selection); `space f` / `space b` and the picker keys; `listFiles`. |
| `Him.Action` | ✅ | Actions (name, group, doc, typed parameters), the registry, invocation parsing (`name arg "quoted arg"`), and binding to a runnable `Bound` (ADR-17). |
| `Him.Command`, `Him.Keymap` | ✅ | `EditorM` and helpers for writing actions; per-mode keymap tries, generic in what they bind (`Keymap a`). |
| `Him.Ex` | ✅ | `:`-command parser. |
| `Him.Render`, `Him.Render.*` | ✅ | Frame, layout, components, diffing. Components: `Gutter` (line numbers), `TextArea`, `StatusLine`, `CommandLine`, `Info` and `Picker` (popups over the text area). `layout` depends on the editor, because the gutter width follows the line count. |
| `Him.File` | ✅ | Load/save (UTF-8, line endings, trailing newline). |
| `Him.History` | ✅ | Undo/redo snapshots: `beginChange` (called by `edit`), `commit` (called by the main loop outside insert mode), `undo`, `redo`. |
| `Him.Document` | ✅ | Buffer + selection + path + dirty flag + line ending/trailing newline. (Split out of `Buffer` so the buffer stays pure text.) |
| `Him.Config` | ✅ | `Config { cfgActions, cfgKeymaps, cfgFallback }`, held by the main loop rather than the `Editor`, which avoids a module cycle. `Bindings`, `overrideBindings` and `buildConfig`, which validates every binding. |
| `Him.Commands.*` | ✅ | Action lists: `Motion`, `Edit` (modes and text), `Search`, `CommandLine`; `File` holds the ex commands. |
| `Him.TextWidth` | ✅ | Tab expansion (width 4), `charWidth` (a compact East-Asian-wide/emoji table; control chars are 2 wide and shown as `^X`), char↔display-column mapping. |
| `Him.Config.Default` | ✅ | `allActions`, `defaultBindings`, `defaultConfig`, and `configWith` (the defaults with user bindings on top). **This is where bindings are added.** |

## 5. Development goals / milestones

Each milestone ends with something runnable, and with this file updated.

- [x] **1. Project setup & DX.** Stack/LTS 24.60 pinned; `hie.yaml`, `fourmolu.yaml`,
  `.hlint.yaml`, `.editorconfig`, `Makefile`; FFI shim compiles; test harness in place;
  `Him.Log`. *Done when:* `make build` and `make test` pass, and HLS loads all components.
  **Note:** build and test pass, but HLS does not load yet. See the known issue in §8.
- [x] **2. Raw mode.** `Terminal.Raw` + alternate screen. A temporary loop echoes byte
  values and `q` quits. *Done when:* the terminal is restored after a normal quit and after
  an exception.
- [x] **3. Output & drawing.** `Terminal.Ansi/Output`; draw `~` rows and a welcome message;
  redraw on SIGWINCH. *Done when:* resizing redraws correctly.
- [x] **4. Input decoding.** `Key`, `Event`, `Terminal.Input`. *Done when:* arrows, Ctrl-,
  Alt-, and a lone Esc are distinguished and shown on screen; `decodeKeys` is unit tested.
- [x] **5. Buffer, file loading, rendering.** `Buffer`, `File`, `Editor`, `View`, `Render`
  (TextArea + StatusLine), `Render.Diff`. *Done when:* `him file` shows the file and it
  scrolls.
- [x] **6. Selections & motions.** `h j k l`, clamping, desired column, viewport follows the
  cursor, selection highlighted. *Done when:* motions are unit tested.
- [x] **7. Commands, keymap, modes.** Registry, trie, Normal/Insert, `i a o`, typing,
  Backspace, Enter, cursor shape, dirty flag.
- [x] **8. Command mode.** `:w`, `:q` (refuses when dirty), `:q!`, `:wq`; status messages.
- [x] **9. Helix selection actions.** `w b e x v ; d c`, plus `g g` / `g e`, with the
  pending keys shown in the status line.
- [x] **10. Polish.** Line-number gutter, tab expansion, wide-character width, horizontal
  scrolling.

- [x] **11. Undo/redo.** `Him.History` holds snapshots. An insert session (including `c`
  and `o`) is one undo step, and the dirty flag is recomputed after undo/redo.
- [x] **12. Yank/paste.** `y`, `p`, `P` with a default register; `d` and `c` also yank.
  Text ending in a newline (from `x`) pastes as whole lines.

- [x] **13. Memory.** A rope of blocks, streaming load/save, and the non-moving GC.
  Peak memory with the 14 MB file: 40 → 24 MB open, 87 → 35 MB after editing and saving.
- [x] **14. Search.** `/`, `?`, `n`, `N`, `*`; smart case; incremental preview;
  rare-byte SIMD scanning; benchmark scenarios with result checks.
- [x] **15. Rendering pass.** Row reuse, terminal scroll regions, cell-level diff, and an
  ASCII fast path.
- [x] **16. Action layer.** Keys bind to actions with typed arguments, validated at
  startup; user bindings override the defaults (ADR-17). Count prefixes fill an
  action's `count` parameter.
- [x] **17. Multiple selections.** `C`, `s` (with preview), `%`, `,`, `A-,`, `(`, `)`,
  `A-s`. Edits, yank and paste work on every range (ADR-18).
- [x] **18. Buffers and menus.** `:open`/`:e` (with `tab` path completion), `:new`, `:bc`,
  `:bn`/`:bp`, `:wa`, `:wqa`, `g n`/`g p`, and `him FILE...` (ADR-19). An info box shows
  the keys after `g`/`space` and the matching `:` commands. `space f` (file picker) and
  `space b` (buffer picker) (ADR-20).
- [x] **19. Ignore files and a directory viewer.** The file picker honours `.gitignore`
  and `.ignore` (ADR-21). Directories open as dired-style listings: `ret`, `-`, `g r`,
  `space d` / `space D`, `:cd`, `:pwd` (ADR-22).
- [x] **20. File operations in listings.** `a`, `+`, `r`, `d` (with confirmation),
  and `g .` for dotfiles (ADR-22).

Later (the architecture already has room for these):
- [ ] Regex search (a small engine of our own, since there is none in the boot libraries)
- [ ] Highlight all matches
- [ ] Undo tree / change sets instead of snapshots
- [ ] Named registers and the system clipboard
- [ ] User config file for keymaps (only the file parser is left: it produces `Bindings`)
- [ ] Syntax highlighting (a styling pass at render time)
- [ ] More pickers (global search, symbols) and a scrollable `:help` listing of actions

## 6. Keybindings

Implemented (defined in `Him.Config.Default`):

| Mode | Keys |
|---|---|
| Normal | counts (`5 j`, `3 w`, `2 x`) on `h j k l`, arrows, `w b e`, `x`; `h j k l`, arrows, `home`/`end`; `w b e` (select words), `x` (select line, repeat to extend), `;` (collapse), `v` (select mode), `d` (delete), `c` (change); `y` (yank), `p` / `P` (paste after / before); `u` / `U` (undo / redo); `g g` / `g e` (first / last line), `g h` / `g l` (line start / end); `i a o`; `:` |
| Select | same as normal, but motions extend; `v` / `esc` → normal |
| Insert | printable chars, `ret` (keeps indent), `tab`, `backspace`, `del`, arrows, `esc` |
| Normal (selections) | `%` (select all), `s` (select matches in the selection, with preview), `C` (copy the selection onto the next line), `,` (keep the primary), `A-,` (remove the primary), `(` / `)` (rotate the primary), `A-s` (split into lines). The status line shows `i/n sels`. |
| Normal (search) | `/` / `?` (search forward / backward, with preview), `n` / `N` (next / previous match), `*` (selection becomes the pattern) |
| Command line | printable chars, `backspace` (leaves when empty), `ret`, `esc` (a search restores the selection) |
| `:` commands | `:w [path]`, `:q` / `:qa` (refuse when any buffer is modified), `:q!` / `:qa!`, `:wq` / `:x`, `:wa`, `:wqa` / `:xa`, `:open` / `:o` / `:e path...`, `:new` / `:n`, `:buffer-close` / `:bc` (`!` discards), `:buffer-next` / `:bn`, `:buffer-previous` / `:bp`. `tab` completes names and paths. |
| Directory listings | `:o dir`, `him dir`, `space d` (the current file's directory, cursor on the file), `space D` (the working directory). In a listing: normal motions and search, `ret` (enter a directory / open a file), `-` or `backspace` (parent), `g r` (refresh), `a` (new file, or directory with a trailing `/`), `+` (new directory), `r` (rename/move), `d` (delete the selected entries, asks `y`), `g .` (show/hide dotfiles). `:cd [dir]` (default: the listed directory), `:pwd`. |
| Buffers and pickers | `g n` / `g p` (next / previous buffer), `space f` (file picker), `space b` (buffer picker). In a picker: type to filter, `up`/`down`/`C-p`/`C-n`/`tab`/`S-tab` move, `ret` opens, `esc` closes. |

Actions that take arguments, and have no default key yet: `move_char_left/right`,
`move_line_up/down [count]`, `goto_line <line>`, `insert_text <text>`,
`set_mode normal|insert|select`, `search_text <pattern>`, `ex <command>`, and `no_op`
(it disables a key).

## 7. How to extend

- **Add an action:** write the `EditorM ()` code (keep the logic pure in `Him.Motion` or
  `Him.Edit` where you can). Wrap it with `simple name group doc run`, or with
  `action name group doc spec run` when it takes arguments. The spec is built from
  `int`, `text`, `choice` and `optional`, e.g. `optional "1" 1 (int "count")`. Add it to
  an action list in `Him.Commands.*` (each list is part of `allActions`). The name is
  public, because bindings and config files use it, so choose it carefully.
- **Add a keybinding:** add `("g h", "goto_line_start")` or `("C-d", "move_line_down 20")`
  entries to that mode's list in `Him.Config.Default`. Chords are parsed by `Him.Key`. At
  startup, `buildConfig` rejects unknown actions and bad arguments, and a test checks the
  defaults.
- **Add a `:` command:** add an `ExCommand` (names, doc, `[Text] -> EditorM ()`) to
  `Him.Commands.File` or a new list, and include it in `exCommands` in `Him.Config.Default`.
- **Add a `:` command:** give it `PathArgs` if its arguments are paths, so `tab`
  completes them. It shows up in the `:` menu automatically.
- **Name a key prefix:** add it to `prefixNames` in `Him.Config.Default`, which gives the
  info box a title such as "goto".
- **Add a picker:** build `PickerItem`s with a `PickTarget` (add a constructor for a
  new kind of target), open them with `newPicker`, and handle the target in
  `picker_accept` (`Him.Commands.Picker`).
- **Add a render component:** write `Theme -> Editor -> Rect -> Frame -> Frame` in
  `Him.Render.<Name>`, give it a `Rect` in `layout`, and compose it in `render`
  (`Him.Render`).
- **Debugging:** run `HIM_LOG=/tmp/him.log make run ARGS=file` and `tail -f /tmp/him.log` in
  another terminal. Never print to stdout while the terminal is in raw mode.

## 8. Where to pick up

*Last updated 2026-10-02. The action layer (ADR-17, milestone 16) is done. Benchmarking
is **on hold**: the user was using the machine during the runs, so this session's
numbers are provisional.*

- **Benchmarking on hold. Resume here when the machine is idle:**
  1. Run the full suite again: `python3 bench/bench.py --runs 5`. Then add a dated results
     section to `docs/BENCHMARK.md` (a draft table is below) and refresh the "final"
     column of the table in `docs/TUTORIAL.md` §7.8.
  2. **open_large is unresolved.** Log entry 16 recorded a first paint of 15 ms, but
     every run this session measured 25–30 ms (Helix 22). A build of `b93060f`, the commit
     before the action layer, also measured 25.7 and 26.8 ms in the same session as
     27.9 ms for `9016654`, so the action layer did not cause it. Re-measure on an idle
     machine. If it is still about 25 ms, profile the path from loading to the first paint.
  3. **Provisional numbers (machine in use).** The focused run for strategies 19/20
     (`search_next,latency`): `n` 1.4 ms (Helix 1.9), `j` 0.9 (Helix 1.5), typing 0.8
     (Helix 1.3). These are recorded in log entries 19/20. A full run at `9016654`
     (him / vim / helix):
     - startup: 5.7 / 32.5 / 27.8 ms
     - open_large first paint: 30.5 / 34.7 / 21.9 ms
     - scroll: 18.4 / 72.2 / 649 ms
     - jump: 4.2 / 33.8 / 115 ms
     - edit_save: 10.7 / 29.9 / 18.8 ms (RSS 25.1 / 37.2 / 66.6 MB)
     - `j` latency: 0.7 / 0.4 / 1.6 ms; typing 0.7 / 0.3 / 1.4 ms
     - search_far: 9.7 / 28.7 / 23.0 ms; search_none: 4.3 / 23.4 / 45.4 ms
     - `n`: 2.0 / 1.3 / 2.1 ms (p95 2.5 / 1.4 / 2.3); 200 × `n`: 13.9 / 58.1 / 93.9 ms
  4. If `n` still ties Helix on an idle machine, the remaining ideas are: skip the diff
     for rows whose `RowKey` matches the old row at the same screen position, or use a
     cheaper row representation than `Seq Cell`.
- **The action layer is done (ADR-17).** The config-file parser is still to do. It only
  has to produce `Bindings` (`Map Mode [(keys, invocation)]`) and call
  `Him.Config.Default.configWith`, which reports every bad binding.
- **Done this session:** count prefixes (`5 j`), multiple selections (milestone 17,
  ADR-18), buffers, info menus and pickers (milestone 18, ADR-19/20), ignore files and the
  directory viewer (milestone 19, ADR-21/22), file operations in listings (milestone 20).
- **Next suggestions:**
  1. **Regex search.** It plugs into `Him.Search`, which only needs a block-level
     matcher. `s` would get regexes for free.
  2. `S` (split the selection on a pattern) and `A-;` (flip the selections).
  3. A config file for keymaps (the parser only). The prefix titles (`cfgPrefixNames`)
     could be configurable too.
  4. Pickers: global search (`space /`, which can reuse `listFiles` and the block
     search), and a `:help` picker of all actions (`registryActions` already lists them
     by group).
  5. Directory listings: copy (`c`/`p`?), batch rename by editing the listing (wdired /
     oil.nvim style), and marking entries.
- **Known issues:**
  - Zero-width combining characters are treated as width 1.
  - Case-insensitive search folds ASCII letters only.
  - `s` searches from each range's start, and a range without a match can scan on to
    the next match beyond it. With many ranges and few matches, that is slow.
  - Search (`/`, `n`) moves only the primary range.
  - The file picker skips hidden entries and lists at most 50,000 files. It does not
    read the global gitignore (ADR-21).
  - A directory listing does not refresh by itself; `g r` lists it again.
  - Deleting a file leaves an open buffer for it (saving it recreates the file).
  - The info box and picker measure text by characters, so wide characters in file
    names can misalign the right border.
- **Working rule:** revert temporary instrumentation by editing it out (or with
  `git checkout` on a clean tree only). A `git checkout` once threw away uncommitted
  work (the search preview hook).
- **Benchmark:** `bench/bench.py` uses the Python standard library only (it is a dev
  tool; the editor itself stays Haskell). Record new results in `docs/BENCHMARK.md` with
  the date and commit.
- **How to verify:** `make test` (308 tests: pure modules, plus key sequences through the
  real keymap). For a manual check, `tmux new-session -d -s t -x 60 -y 10 "<him binary> file"`
  plus `tmux send-keys` / `tmux capture-pane -p`. The binary path is
  `$(stack path --local-install-root)/bin/him`.
- **Known issue (the user will handle it): HLS rejects Stack's GHC ("GHC ABIs don't match").** The installed HLS
  (AUR `haskell-language-server-static`, the upstream `linux-unknown` release) was built
  against the *rocky8* GHC 9.10.3 bindist. Stack's default `tinfo6` 9.10.3 is the
  *fedora33* bindist, which has different ABI hashes. We verified that the rocky8 bindist's
  `containers` and `ghc` hashes match what HLS expects. The fixes below all download or
  build something, so the user needs to choose one:
  1. Pin the rocky8 bindist in `stack.yaml` with a custom variant (verified to work; about
     3 GB installed):
     ```yaml
     ghc-variant: hls
     setup-info:
       ghc:
         linux64-custom-hls-tinfo6:
           9.10.3:
             url: "https://downloads.haskell.org/ghc/9.10.3/ghc-9.10.3-x86_64-rocky8-linux.tar.xz"
             sha256: d0504f094b827e64653f8e8d7b4b3029bf7ff026f5091c0b22366dcd1b032f2d
     ```
  2. Use ghcup to manage GHC and HLS as a matched pair, with Stack using that GHC
     (`system-ghc: true`).
  3. Build HLS from source against this snapshot.
