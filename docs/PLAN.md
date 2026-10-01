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
`filepath`, `stm`, `array`. Each one is added to `package.yaml` only when a module first uses
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
inserted before the character at the head. Edits apply to the primary range only. Motions
and rendering already handle every range. Multi-range edits need position mapping and
will come with multiple selections.

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
| `Him.Position`, `Him.Selection` | ✅ | `Pos`, `Range {anchor, head}`, `Selection` (NonEmpty ranges + primary). |
| `Him.Motion` | ✅ | Pure motions: char, line (desired column), word, line/file start/end. |
| `Him.Edit` | ✅ | Pure edits over all selections. |
| `Him.Editor`, `Him.Mode`, `Him.View` | ✅ | Editor state, modes, viewport + scrolloff. |
| `Him.Command`, `Him.Keymap` | ✅ | Command registry and per-mode keymap tries. |
| `Him.Ex` | ✅ | `:`-command parser. |
| `Him.Render`, `Him.Render.*` | ✅ | Frame, layout, components, diffing. Components: `Gutter` (line numbers), `TextArea`, `StatusLine`, `CommandLine`. `layout` depends on the editor, because the gutter width follows the line count. |
| `Him.File` | ✅ | Load/save (UTF-8, line endings, trailing newline). |
| `Him.History` | ✅ | Undo/redo snapshots: `beginChange` (called by `edit`), `commit` (called by the main loop outside insert mode), `undo`, `redo`. |
| `Him.Document` | ✅ | Buffer + selection + path + dirty flag + line ending/trailing newline. (Split out of `Buffer` so the buffer stays pure text.) |
| `Him.Config` | ✅ | `Config { cfgRegistry, cfgKeymaps, cfgFallback }`, held by the main loop rather than the `Editor`, which avoids a module cycle. |
| `Him.Commands.*` | ✅ | Command lists: `Motion`, `Edit` (modes and text), `CommandLine`, `File` (ex commands). |
| `Him.TextWidth` | ✅ | Tab expansion (width 4), `charWidth` (a compact East-Asian-wide/emoji table; control chars are 2 wide and shown as `^X`), char↔display-column mapping. |
| `Him.Config.Default` | ✅ | The default keymaps and registry. **This is where bindings are added.** |

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

Later (the architecture already has room for these):
- [ ] Regex search (a small engine of our own, since there is none in the boot libraries)
- [ ] Highlight all matches
- [ ] Undo tree / change sets instead of snapshots
- [ ] Named registers and the system clipboard
- [ ] Multiple selections (`C`, `s` split)
- [ ] Multiple buffers, `:e`
- [ ] User config file for keymaps
- [ ] Syntax highlighting (a styling pass at render time)
- [ ] Popups and pickers (render components)

## 6. Keybindings

Implemented (defined in `Him.Config.Default`):

| Mode | Keys |
|---|---|
| Normal | `h j k l`, arrows, `home`/`end`; `w b e` (select words), `x` (select line, repeat to extend), `;` (collapse), `v` (select mode), `d` (delete), `c` (change); `y` (yank), `p` / `P` (paste after / before); `u` / `U` (undo / redo); `g g` / `g e` (first / last line), `g h` / `g l` (line start / end); `i a o`; `:` |
| Select | same as normal, but motions extend; `v` / `esc` → normal |
| Insert | printable chars, `ret` (keeps indent), `tab`, `backspace`, `del`, arrows, `esc` |
| Normal (search) | `/` / `?` (search forward / backward, with preview), `n` / `N` (next / previous match), `*` (selection becomes the pattern) |
| Command line | printable chars, `backspace` (leaves when empty), `ret`, `esc` (a search restores the selection) |
| `:` commands | `:w [path]`, `:q` (refuses when dirty), `:q!`, `:wq` / `:x` |

## 7. How to extend

- **Add a command:** write an `EditorM ()` action (keep the logic pure in `Him.Motion` or
  `Him.Edit` where you can), wrap it in a `Command` with a snake_case name and a doc string,
  and add it to the registry in `Him.Config.Default`.
- **Add a keybinding:** add `("g h", "goto_line_start")`-style entries to that mode's list
  in `Him.Config.Default`. Chords are parsed by `Him.Key`. At startup, `defaultConfig`
  rejects bindings to unknown commands, and a test checks it.
- **Add a `:` command:** add an `ExCommand` (names, doc, `[Text] -> EditorM ()`) to
  `Him.Commands.File` or a new list, and include it in `exCommands` in `Him.Config.Default`.
- **Add a render component:** write `Theme -> Editor -> Rect -> Frame -> Frame` in
  `Him.Render.<Name>`, give it a `Rect` in `layout`, and compose it in `render`
  (`Him.Render`).
- **Debugging:** run `HIM_LOG=/tmp/him.log make run ARGS=file` and `tail -f /tmp/him.log` in
  another terminal. Never print to stdout while the terminal is in raw mode.

## 8. Where to pick up

*Last session ended on 2026-10-01, after milestones 13–15 (memory, search, rendering).
`docs/TUTORIAL.md` walks through how the whole editor is built.*

- **Performance status** (see `docs/BENCHMARK.md`):
  - **Ahead:** him leads in startup, memory, batched input and search throughput.
  - **Close:** per-key latency is close to Helix and about 2× Vim's.
  - **Behind:** opening a 14 MB file (about 50 ms vs. 22–35 ms).
- **Next suggestions:**
  1. **Multiple selections** (`C`, `s`, `,`). `edit` must map positions across ranges
     (ADR-5b).
  2. **Regex search.** It plugs into `Him.Search`, which only needs a block-level
     matcher.
  3. **Faster large-file open.** Build line indexes lazily per block, and avoid the
     decode copy for valid UTF-8.
  4. Multiple buffers / `:e`, and a config file for keymaps.
- **Known issues:**
  - Zero-width combining characters are treated as width 1.
  - Case-insensitive search folds ASCII letters only.
  - `edit` changes only the primary range.
- **Working rule:** revert temporary instrumentation by editing it out (or with
  `git checkout` on a clean tree only). A `git checkout` once threw away uncommitted
  work (the search preview hook).
- **Benchmark:** `bench/bench.py` uses the Python standard library only (it is a dev
  tool; the editor itself stays Haskell). Record new results in `docs/BENCHMARK.md` with
  the date and commit.
- **How to verify:** `make test` (165 tests: pure modules, plus key sequences through the
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
