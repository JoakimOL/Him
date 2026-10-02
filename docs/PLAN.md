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

Non-goals (for now): a GUI (the core is frontend-free, ADR-36, so one could be added),
plugins loaded at run time (plugins are compiled in and switched on or off, ADR-35),
Windows support.

## 2. Assumptions

- Linux or another POSIX system, with a terminal that understands xterm/ANSI escape sequences
  (alternate screen, SGR colours, cursor-shape `DECSCUSR`).
- The terminal and the files are UTF-8. Invalid bytes are decoded leniently (replacement char).
- Files fit in memory (a rope of blocks; a 14 MB file costs about 15 MB, ADR-16).
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
`filepath`, `stm`, `array`, `process`. (`process` since milestone 21, for git and
language servers; `filepath` since milestone 19.) **Amended by ADR-27:** the
tree-sitter C runtime is vendored in `cbits/tree-sitter`; it is the one C dependency
beyond the small shims. (`filepath` is used since milestone 19.) Each one is added to `package.yaml` only when a module first uses
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

**ADR-23: Effects as data, and a runtime for background jobs.**
Actions are state changes (`EditorM = StateT Editor IO`). The `Editor` is plain data
and holds no handles, and actions cannot see the config.
- **Requests:** to ask for more, an action queues an `Effect` (`Him.Effect`) with
  `request`.
- **Immediate effects:** `handleEvent` carries out `RunAction` (run another action by
  its `Invocation`; used by the palette and `:action`) and `OpenPalette` right after the
  key, with the config at hand. A chain of them is capped at 8 rounds.
- **Background effects:** `StartJob` / `CancelJob` are left for the main loop, which
  owns the `Runtime` (`Him.Runtime`). The runtime runs each `Job` on its own thread, at
  most one per `JobKey` (starting one cancels the old one with `killThread`), and posts
  each `JobResult` as an `EvJob` event on the same channel as the keys. So results are
  handled by `handleEvent` like any input, batched with it (ADR-11), and drawn once.
- **Stale results:** documents have a `docId` (assigned when opened) and a `docVersion`
  (bumped by edits, undo/redo and `replaceText`). Pickers have a generation. Every
  result names what it was computed for, and a stale one is dropped.
- **Testing:** tests can assert the effects an action requested. The `settle` helper in
  `test/Spec.hs` runs jobs on a real runtime, as the main loop does.
- **`Him.Process`** runs external programs (stdin in; stdout and stderr read
  concurrently), and `Him.Json` is a small JSON library. Both are for the git and LSP
  phases.

*Alternative:* `ReaderT Env (StateT Editor IO)`, which would give actions the handles
directly. Every action would change, and tests would need a full runtime.

**ADR-24: The file picker streams, and large pickers filter in the background.**
- **Walk:** `Him.FileTree.walkFiles` reads directories with a pool of up to 8 workers
  over an STM queue. It takes entry types from `readdir` (`unix`'s
  `readDirStreamWith`), so only links and unknown types are `stat`ed. Directory links
  are followed (as in Helix), each target once, and never a link that contains itself.
  On 200k files the walk went from 732 to about 370 ms (log 22).
- **Streaming:** `space f` opens the picker at once, and a `ScanFiles` job sends files
  in batches (every 5000 files or 100 ms). The count shows `…` while loading.
- **Ranking:** each item has a precomputed lower-case key, file name and length. An
  in-order character check rejects non-matches before scoring. Matches are bucketed by
  rank, and only the best 1000 are kept, plus a total count (log 21). It still costs
  about 75 ms when 200k items all match. So a picker of more than 20k items, with a
  non-empty query, ranks in a `FilterPicker` job, and keeps showing its last matches
  until the answer arrives. An answer for an older query is dropped.
- **Ties:** among equal fuzzy scores, an exact first word or file name wins
  (`goto_line` before `goto_line_end`), then the shorter label.

**ADR-25: Git through the `git` program; the diff in-process.**
- **Base texts:** a document's git state (`docGit`, `Him.GitState`) holds its file's
  index and HEAD versions. They are loaded by a `GitLoad` job running `git rev-parse`,
  `ls-files --stage` and `show :path` / `show HEAD:path` (`Him.Git` through
  `Him.Process`). This happens when the document becomes current, after a save, and
  after staging. An untracked file has an empty index version, so every line shows as
  added. Outside a repository there is no state.
- **Diffs:** `Him.Diff` is Myers' algorithm after trimming the common prefix and suffix.
  A middle that needs more than 1000 edits becomes one hunk. Unstaged hunks are
  index → buffer. Staged hunks are HEAD → index, with their new side moved onto buffer
  lines by `mapLine` through the unstaged hunks. One `GitDiff` job runs per document at
  a time; when it answers for an older version, the next event asks again. On 200k
  lines a diff costs 5–53 ms, on a background thread (`bench/DiffBench.hs`).
- **Gutter:** a one-column sign lane in front of the line numbers. Added and changed
  lines get `▎`, a removal gets `▁` on the line above; staged signs are dimmer, and
  unstaged ones win.
- **Staging is one pure function.** `applySelected old new hunks selected` applies the
  selected changes. In a changed hunk, old and new lines are paired by position, so a
  single line can be taken. The extra new lines count as additions, and the extra old
  lines as a removal attached to the hunk's last line. With it:
  - staging is applying index → buffer;
  - unstaging is applying the *unselected* HEAD → index changes, which reverts the
    selected ones;
  - resetting is the same on the buffer, as one undoable change.
  The new index version is written with `git hash-object -w --stdin --path=` and
  `git update-index --cacheinfo`. Staging writes what the buffer shows, even unsaved,
  as `git add -p` does with the work tree.

*Alternatives:* parsing `git diff` output. That only sees saved files, and a patch for
partial hunks is fragile; computing the diff ourselves sees every keystroke. Linking
libgit2 is not a boot library.

**ADR-26: One syntax-highlighting interface, with injected providers.**
- **The interface:** the editor knows only `Him.Syntax`. A `SyntaxProvider` has
  `spStart :: Language -> IO (Maybe SyntaxSession)`, and a session has `ssUpdate`
  (version, buffer, edits if known), `ssHighlight` (spans for a range of lines) and
  `ssClose`.
- **Spans:** spans are per line, carrying dotted scope names
  (`keyword.control.import`), which both tree-sitter captures and TextMate scopes use.
  The theme resolves a scope by its longest known prefix (`scopeStyle`).
- **Configuration:** providers are listed in `cfgSyntaxProviders` and tried in order.
  `Him.Language` detects the language (file name, extension, shebang) and maps it to
  each provider's grammar name.
- **Jobs:** sessions live in the runtime. A `SyntaxStart` job picks the provider; then
  `Highlight` jobs, at most one per document in flight, return spans for the view plus
  100 lines either side. The document keeps the last spans (`docSyntax`), so typing
  shows at most one burst of staleness. The row key includes the spans.
- **Adding a provider** (e.g. TextMate) means one module that builds the record, plus
  one entry in `syntaxProviders` in `Him.Config.Default`. Nothing else changes. Tests
  inject a fake provider, which proves the editor works against the interface alone.

**ADR-27: Tree-sitter: vendored runtime, grammars built for him.**
- **Runtime:** the C runtime (v0.26.9, MIT) is in `cbits/tree-sitter`, copied unchanged
  from the local cargo registry (no download). It is compiled through
  `cbits/ts_runtime.c`, which sets the feature macros for that unit only.
  `cbits/ts_shim.c` runs one query over a byte range and returns every capture in one
  array.
- **Provider:** `Him.Syntax.TreeSitter` dlopens `grammars/NAME.so` and reads
  `queries/LANG/highlights.scm`. It resolves `; inherits:` in place, and evaluates
  `#eq?`, `#match?`, `#any-of?` (and negations, plus `#lua-match?`) in Haskell.
  Unknown predicates drop the match; directives and `#is?`/`#is-not?` are ignored.
- **Precedence:** the innermost node wins; on the same node the *last* pattern wins.
  That is Helix's documented rule, which its queries are written for.
- **Grammars are built by him, not borrowed.** Many grammar repositories ship an old
  `tree_sitter/array.h`. Its `array_push` reallocates through an `(Array *)` cast and
  then writes through the typed pointer. Under strict aliasing at `-O3`, the compiler
  may keep the old pointer. Helix's `haskell.so` (GCC 16) corrupted the heap on ordinary
  files. This was reproduced in plain C and located with AddressSanitizer
  (`scanner.c:651`, `advance`). 168 of 198 grammar sources here use that `array.h`.
  So only grammars from `$HIM_RUNTIME` or `~/.config/him/runtime` are loaded.
  `him --build-grammars [SOURCES] [NAMES]` compiles grammar sources (by default Helix's
  `runtime/grammars/sources`) with `-O2 -fno-strict-aliasing` into
  `~/.config/him/runtime/grammars`; 299 of 301 built here. Queries still come from
  Helix's runtime.
- **Cost** (`bench/HighlightBench.hs`, log 24): a 7,241-line Rust file parses in 22 ms,
  and a 260-line window highlights in 3 ms. Loading a grammar and its query takes
  30–160 ms, once per document. All of it runs in jobs. Every edit re-parses fully for
  now; incremental parsing (roadmap 3b) would use the edits.

*Alternative:* the Hackage `tree-sitter` package. It is not a boot library, was last
tested with GHC 9.2, bundles an old runtime and its own grammars, and targets AST
extraction, not highlight queries.

**ADR-28: A small regex engine of our own.** `Him.Regex` is a backtracking matcher:
literals and escapes, `.`, classes with `\d \w \s`, anchors and `\b`, groups
(including `(?:…)`), alternation, and greedy or lazy `* + ? {m,n}`. Lua patterns are
translated (`compileLua`). There are no backreferences or lookaround. It is used for
query predicates; all 205 `#match?` patterns in Helix's queries compile. It is the
starting point for regex search.

**ADR-29: The LSP client: processes in the runtime, protocol as pure data.**
- **Pure protocol:** `Him.Lsp.Protocol` covers Content-Length framing (tested at every
  split point), file URIs, columns in UTF-8/16/32, building and classifying JSON-RPC
  messages, and reading diagnostics, locations, hover contents and completion items
  (snippets reduced to plain text).
- **Processes:** `Him.Lsp.Server` runs one server process with:
  - a reader thread that splits its output into messages;
  - a writer thread with a queue, so the main loop never blocks on a busy server;
  - a drain for stderr;
  - automatic answers to the server's own requests (configuration, progress,
    registration);
  - the initialize handshake, which advertises UTF-8 positions.

  The runtime starts one server per (command, project root) on demand (`LspEnsure`).
  Roots come from markers in `Him.Lsp.Config`. The runtime forwards `LspSend` effects
  and stops the servers on quit.
- **Editor side:** the state is pure (`Him.Lsp.State`):
  - documents attach to a server (`docLsp`);
  - requests are remembered as `Pending` values by id, so a reply is applied by a pure
    function;
  - diagnostics are kept by absolute path, and converted to character columns against
    the current text when drawn.
- **Sync** (milestone 25, `Him.Lsp.Sync`): it runs once per input batch, just before
  drawing (`lspFlush`), and before every request.
  - **Incremental changes:** `didChange` sends one range edit, computed by
    `Buffer.changeBetween` from the text last sent. Shared storage blocks are skipped
    with a memory comparison, then lines, then characters. A one-character edit in a
    196,000-line buffer is found in under 1 ms, where the whole text was 14 MB per
    batch before. This covers any kind of change, undo included. Servers that ask for
    full sync get the whole text; an undo back to the sent text sends nothing.
  - **The server's copy** is the buffer plus the file's final line break, which edits
    never touch, so buffer positions are valid in it.
  - **Saves and closes:** `didSave` follows a save (`docSaves`), and `didClose` follows
    `:bc`.
- **Server commands:** `:lsp-start`, `:lsp-stop`, `:lsp-restart` and `:lsp-info`.
  `LspStop` removes the server from the runtime before stopping it, and an exiting
  server is only reported if it is still the one registered under its key, so a
  restart cannot be undone by the old server's exit. Stopping drops the server's
  attachments, diagnostics and pending requests; restarting re-attaches its documents.
- **Edits** (`Him.Lsp.Edit`): text edits apply from the last to the first, so
  positions stay valid, and insertions at the same place keep the server's order.
  Workspace edits change open buffers in place; files that are not open are opened and
  left modified, and the current buffer stays current.
- **Features:**
  - diagnostics: a gutter sign over git signs, an underline in the severity's colour,
    the cursor line's message in the bottom row, `] d` / `[ d`, and `space x`;
  - `space k` hover, in a popup at the cursor;
  - `g d` definition and `g r` references (one location jumps, several open a picker);
  - completion in insert mode, automatic or on `C-x`, in a `Completing` keymap layer
    over insert mode.
  - since milestone 25:
    - `space r` rename (a prompt starting with the word);
    - `:format`;
    - `space a` code actions. The request sends the selected lines' diagnostics back
      raw. Picking one applies its edit, runs its command, or resolves it first.
      Servers that apply edits through `workspace/applyEdit` (clangd's tweaks) are
      handled.
    - signature help after the server's trigger characters (above the cursor, until
      `)`);
    - `space s` document symbols, `g y` type definition, `g i` implementation;
    - jumps convert the server's columns exactly.
  - since milestone 26:
    - `space S` workspace symbols. The picker's source is `ServerQuery`, so each
      change of the query asks the server again; stale answers are dropped.
    - completion imports. Items' `additionalTextEdits` are applied with the insertion,
      moving the cursor down with lines added above it. Items without them are
      resolved (`completionItem/resolve`, advertised through `resolveSupport`), and the
      edits are applied if the text is unchanged.
    - references are on `g r`.
- **Tests:** against clangd when it is installed (attach, diagnostics, hover, `g d`,
  fixing an error, completion), plus the pure protocol tests.

*Alternatives:* `ReaderT` handles in actions (rejected in ADR-23), or blocking request
calls from actions, which would freeze the editor while a server thinks.

**ADR-30: Pickers preview where an item points.**
- **What has a preview:** an item that is a place (a file, a buffer, a position from a
  language server). It shows beside the list when the box is at least 60 columns wide:
  the file around the item's line, with that line highlighted and line numbers.
- **Where the text comes from** (`previewFor` in `Him.Editor`, pure):
  - an open buffer gives its own text, unsaved changes included;
  - any other file is read by a `LoadPreview` job (one per path) and cached in
    `edPreviews` while the picker is open; the cache is dropped when the picker closes.
- **What is not shown:** binary files (a NUL in the first 8 KB) and files over 20 MB
  show a note instead.
- **Drawing:** the box keeps one border, with a divider between the list and the
  preview; the preview's rows are dropped from the row cache, like every popup.

*Alternative:* opening the file in a hidden buffer. That loads more than needed and
mixes previews into the buffer list.

**ADR-31: Ctrl-Z suspends the editor.** Raw mode turns off the terminal's own signal
keys (`ISIG`), so Ctrl-Z arrives as a key. It is bound to `suspend`, which queues a
`Suspend` effect. The main loop then:
1. calls the suspend action that `withRawTerminal` hands it: leave the alternate
   screen, restore the original terminal attributes, and stop the process with
   `SIGTSTP` (the default action stops every thread);
2. when the shell continues the process (`fg`), sets raw mode again and re-enters the
   alternate screen;
3. marks the editor `edRepaint`, so the next frame is drawn without diffing against the
   old one, and reads the window size again in case it changed meanwhile.

**ADR-32: The config file is TOML, layered on the defaults.**
`~/.config/him/config.toml` (or `$XDG_CONFIG_HOME/him/config.toml`, or `$HIM_CONFIG`)
has three sections:
- `[editor]` and its sub-tables: every setting (ADR-34) and `theme` (ADR-33).
- `[keys.<mode>]`: `"keys" = "action invocation"`. The modes are normal, select,
  insert, command, picker, directory and completion.
- `[language-server.<language>]`: `command`, `args`, `roots`, `language-id` and
  `enabled`.

How it is read:
- **Format:** TOML, because Helix users know it. `Him.Toml` reads the subset a config
  needs into the JSON value type, and its errors name the line.
- **Keys:** the action layer (ADR-17) did most of the work. Bindings are the same
  `Bindings` text as the defaults, put on top of them with `overrideBindings` and
  validated with them, so a bad key or action is reported with mode and keys.
  `no_op` unbinds a key.
- **Strict checking:** unknown sections, modes and settings are errors, because a
  silently ignored typo is worse. Every error is collected.
- **Failure:** a broken file starts the defaults, and the status row names the first
  problem, so the user can fix it in him (`:config-open`).
- **Defaults to start from:** `him --dump-default-config` prints every default as a
  config file: the bindings per mode, commented with what they do; the servers; and a
  list of all actions with their parameters. Read back, it equals the defaults
  (tested).
- **Changing it while running:** `:config-open` opens the file, pre-filled with the
  defaults if missing; saving creates the directory. `:config-reload` replaces the
  loop's config, the runtime's server table and the editor settings.

*Alternative:* a custom `keys = action` line format. It is simpler to parse, but it
leaves no room to grow and is unfamiliar.

**ADR-33: Themes are Helix theme files.**
`[editor] theme = "onedark"` in the config, or `:theme <name>` while running (`tab`
completes the name; `:theme` alone names the current one). Why Helix's format:
- **The scope names were already Helix's** (ADR-26), so its themes colour him's
  highlighting as they are. All 219 installed themes load without a warning (checked
  with a throwaway script).
- **Users can switch editors** without redoing their colours, and write their own the
  same way.

How it works:
- **Lookup:** a theme is `<name>.toml` in `themes/` next to the config file, then in each
  runtime directory's `themes/` (`Him.Paths.themeDirs`), and the first match wins.
  `default` is built in (`Him.Theme.defaultThemeText`, written in the same format), but
  a file of that name replaces it. A theme that inherits its own name (a user's tweak
  of a Helix theme) gets the next file of that name.
- **Format (`Him.Theme`, pure):**
  - `inherits`: the child's entries replace the parent's whole, palettes merge by name,
    and the merged palette colours both, as Helix does.
  - Colours: palette names (they may chain), `#rrggbb`, `#rgb`, `"110"` (a palette
    index), the 16 terminal colour names, `default`.
  - Modifiers: bold, dim, italic, underlined, reversed, crossed_out.
  - Underlines: `{ color, style = line|curl|double_line|dotted|dashed }`.
  - Anything not understood is skipped and logged, not fatal (rainbow brackets, blink).
- **Reader:** `Him.Toml` gained inline tables (`{ fg = "red" }`, also spanning lines, as
  some themes write them). It now skips `#` inside literal strings when joining lines.
- **Styles are layered (`patchStyle`, Helix's patch):** text, then syntax, then a
  diagnostic underline, then the selection, then a cursor. A selection that only sets
  a background keeps the text's colour. The primary selection uses
  `ui.selection.primary`.
- **What `Style` holds:** dim, strikethrough, an underline kind and an underline colour
  (SGR `4:3` and `58;2;…`). `PackedStyle` is now a `Word64` plus a `Word32` (the
  underline colour), still unpacked into each cell.
- **UI from scopes (`Him.Render.Theme.fromScopes`):** the render components keep
  their record fields, filled once per theme from Helix's UI scopes:
  - `ui.text`, `ui.selection(.primary)`, `ui.cursor`, `ui.linenr(.selected)`,
    `ui.gutter`, `ui.virtual`;
  - `ui.statusline` and `ui.statusline.normal|insert|select`;
  - `ui.popup`, `ui.menu.selected`, `ui.text.inactive|focus|directory`;
  - `error|warning|info|hint`, `diagnostic.*`, `diff.plus|minus|delta`.
  him's own scopes fall back to those: `ui.statusline.command` and `.picker` (to
  `.normal`), `ui.popup.key`, and `diff.*.staged` (to the same colour, dimmed). Lookups
  fall back by prefix, as Helix's do.
- **Background:** `ui.background` (with `ui.text`'s foreground) becomes the
  terminal's *default* colours through OSC 11/10, carried in `frameColors` and sent
  when they change. Cleared areas, scrolled-in rows and blank cells then show the theme
  without the diff ever writing a background. Leaving or suspending resets them (OSC
  111/110). The alternative, painting every cell, would defeat the blank-tail and
  scroll-region optimizations of ADR-15.
- **Older terminals:** without `COLORTERM=truecolor|24bit`, 24-bit colours are mapped
  to the nearest of the 256-colour palette (the cube or the grey ramp) when the theme
  is loaded.
- **Switching:** the theme lives in the main loop beside the config (an `IORef`).
  `ChangeTheme` is an effect the loop carries out, like `ReloadConfig`. The loop's
  effects now run in order with one `foldM`. A switch repaints everything, because
  cached rows hold the old colours. `:config-reload` loads the theme again too.

**ADR-34: Settings are one table.**
Every setting is an `OptionSpec` in `Him.Options.optionSpecs`, holding:
- its key (`tab-width`, or `search.smart-case` for `[editor.search]`);
- its doc;
- a setter that checks the value's type and range;
- a printer for the default.

Three things use that one table: checking the config file (`setOption`; an unknown key
lists the known keys of its table), applying it (`userOptions`), and the
`--dump-default-config` section. They cannot drift apart. The values live in
`edOptions :: Options` on the editor (which replaced `edScrolloff` and `edShowHidden`),
so actions and render components read them like any state. The settings are:
- `[editor]`: `scrolloff`, `show-hidden-files`, `tab-width`, `expand-tab`,
  `line-number` (absolute / relative / off), `escape-timeout` (read at startup only);
- `[editor.cursor-shape]`: normal, insert, select, command;
- `[editor.lsp]`: `auto-completion`, `completion-trigger-len`, `auto-signature-help`,
  `hover-lines`;
- `[editor.search]`: `smart-case`, `wrap-around`;
- `[editor.file-picker]`: `hidden`, `git-ignore`, `ignore`, `follow-symlinks`,
  `max-files`;
- `[editor.picker]`: `preview`, `preview-min-width`, `preview-max-size`.

Names follow Helix where it has the same setting. Pure code that needed a value now
takes it as a parameter: `layoutLine`/`displayCol`/`charIndexAtCol` (tab width),
`lineBy` (tab width), `compileNeedle` (smart case), `findMatch` (wrap), and
`walkFiles` (a `WalkOptions` carried by the `ScanFiles` job).

Still constants, on purpose: undo levels, gutter glyphs, the language table, the LSP
start timeout, and the internal tuning values (`maxBatch`, `chunkSize`, `mergeGap`,
`syncLimit`, `matchLimit`, `maxEdits`, the scan batching, `highlightMargin`).

**ADR-35: Git and the LSP client are plugins.**
A `Plugin` (in `Him.Config`) names everything a feature adds:
- its actions, default bindings, `:` commands and key-prefix titles;
- whether it draws in the gutter's sign lane;
- hooks: housekeeping after every event, `plBeforeRender` once per input batch, and
  `plJobResult` for every job result;
- `plEnable` / `plDisable` for switching it while running.

The core folds over `cfgPlugins` (the enabled ones) where it used to call git and LSP
code by name: `App.housekeeping`, the job-result dispatch, and the per-batch flush.
`Config.Default.configWith enabled userBindings` builds the config:
- the core's actions and bindings, plus those of the enabled plugins;
- the user's bindings on top, minus those that name a switched-off plugin's actions
  (they come back with the plugin, rather than failing the config).

How plugins are switched:
- **At startup:** `[plugins] git = false` (all are on by default).
- **While running:** `:plugin-enable` / `:plugin-disable <name>` (`tab` completes the
  names; `ExArgs` gained `NameArgs`), and `:plugins` lists them. The loop keeps the
  `UserConfig`, makes the config again with the new set, and runs the hooks of the
  plugins that came and went (`switchPlugins`; `:config-reload` uses it too).
  - Switching git off resets every document's git state, so no signs are left.
    Switching it on looks every document up again.
  - Switching the LSP off sends `LspStopAll`, which the runtime turns into stopping
    every server. It also clears `edLsp`, the attachments, the completion menu and
    the popups.
- **Gutter:** the sign lane exists only while a sign-drawing plugin is on
  (`edSignLane`).

Coupling that remains, on purpose:
- Plugin state still lives in `Document` (`docGit`, `docLsp`) and `Editor` (`edLsp`).
- `Effect`, `Job` and `JobResult` keep their plugin constructors.
- Rendering reads that state directly. With the plugin off, the state is empty, so
  nothing is drawn.
- A few core actions call LSP functions: the rename prompt, code-action and
  workspace-symbol pickers. They are only reachable through LSP actions.

Making these generic (state as `Dynamic`, plugin-provided gutter lanes) would cost
type safety for no user-visible gain yet. Syntax highlighting could become a plugin the
same way.

**ADR-36: Module names say what modules hold (2026-10-02).**
Since ADR-17, the modules under `Him.Commands.*` hold *actions*, and `Him.Command`
holds the `EditorM` monad and its helpers. They are now `Him.Actions.*` and
`Him.EditorM`. The same pass made three more cuts:
- `Him.App` became the terminal frontend, and `Him.Session` the frontend-free event
  handling.
- `Him.Actions.Lsp` was split by feature.
- One `changeDocument` / `replaceBuffer` helper replaced four hand-built undoable
  replacements.

The old names stay in the older ADRs and log entries, which describe the code as it
was then.

**ADR-37: Splits are a tree of windows; the focused one is the editor's state.**
Helix's model, without Vim's tabs (the buffer list stays as it was):
- **Layout (`Him.Window`, pure):** `Layout` is a tree, `Leaf windowId | Split Axis
  [Layout]`, where the axis is `Beside` (`:vsplit`) or `Stacked` (`:hsplit`).
  - Splitting inside a split along the same axis adds a sibling; otherwise the window
    is split in two.
  - Closing collapses a split that is left with one child.
  - `boxes` shares a split's space evenly, with a one-column border between windows
    side by side.
  - `neighbour` finds the nearest window on a side that overlaps across, preferring
    the one most in line.
- **State:** the *focused* window is still `edDoc` + `edView`, so no action changed.
  The other windows are `Window { winDoc, winView, winSelection }` in `edWindows`.
  - Focusing a window stashes the focused one as a `Window`, then makes the other's
    document current (the buffer zipper's `gotoBuffer`) with its view and selection,
    clamped in case the text changed meanwhile.
  - Each window therefore keeps its own cursor and scroll position, even on the same
    document. Edits are not mapped through an unfocused window's selection; clamping
    keeps it valid.
- **Rendering:** each window gets its own gutter, text area and status line. An
  unfocused window is drawn by the same components through `windowEditor`, the editor
  as that window shows it (normal mode, no popups).
  - Unfocused windows draw their primary cursor as a cell, use
    `ui.statusline.inactive`, and show no mode.
  - Borders use `ui.window`.
  - Row-cache keys are now keyed by `(row, column)`, since windows side by side share
    rows.
  - The terminal-scroll optimization (ADR-15) applies only when the focused window
    spans the full width, because scrolling moves whole rows.
- **Highlighting** asks for the lines visible in every window on the current document
  when they are within 2000 lines of each other; otherwise only for the focused one.
- **Keys:** Helix's, after `C-w` or `space w`:
  - `v` / `s` split side by side / stacked;
  - `w` next window;
  - `h j k l` (also with `C-` and arrows) focus a neighbour;
  - `H J K L` swap;
  - `q` close, `o` only;
  - `n v` / `n s` split with a new scratch buffer.
- **Commands:** `:vsplit` / `:vs [files]`, `:hsplit` / `:hs [files]`, `:vnew`,
  `:hnew`.
- **Quitting:** `:q` / `:q!` / `:wq` close the focused window, and quit with the last
  one, as in Helix. `:qa` quits everything.
- **Paging** (`C-f`, `C-d`, …) moves by the focused window's height.

**ADR-38: A REPL is a buffer with a process behind it (the `repl` plugin).**
`:repl` opens the REPL of the file's language in a window beside it.
- **Typing:** in the REPL buffer you type as anywhere, and `ret` in insert mode sends
  the line. This is the `Repl` keymap layer, `[keys.repl]`, which inherits insert
  mode; `C-c` interrupts.
- **From a file:** `space e` sends the selection, or the line when only one character
  is selected. Code of several lines is wrapped in the REPL's markers (ghci's
  `:{ … :}`; a blank line for Python). The focus stays in the file.
- **Reloading:** `space E` reloads the project (`:reload`). It also happens by itself
  after a file of the language is saved, when `reload-on-save` is set (the ghci
  default).
- **Commands:** `:repl-send <text>`, `:repl-reload`, `:repl-interrupt`, `:repl-stop`,
  `:repl-restart`.

How it is built:
- **The transcript (`Him.Repl.Transcript`, pure).** A REPL buffer is a document of
  kind `ReplDoc ReplState`, whose `rsInput` is where the next input starts.
  - Output is inserted just before the input, and cursors at or after that point move
    with it. Output arriving while you type never splits your line.
  - Output is not an edit: no undo step, never dirty.
  - `ret` takes the text after `rsInput` and closes it with a line break.
  - REPLs reading a pipe do not echo, so the editor shows sent code itself, as if
    typed.
  - Escape sequences and carriage returns are removed from output.
- **The process (`Him.Repl.Process`).** stdout and stderr share one pipe. The process
  runs with `TERM=dumb` and in its own process group, so `C-c` interrupts the REPL,
  not the editor. A streaming UTF-8 decoder handles characters split across reads.
  - The pipe's ends are close-on-exec. A language server started at the same time
    inherited the write end, so a REPL's exit was never seen. A test caught this:
    with pylsp starting for `t.py`.
- **Starting.** The runtime starts the REPL as soon as it performs the effect, not as
  a job, so text sent straight after reaches it. It runs in the project root, found
  from `roots` markers (like language servers), so `stack ghci` loads the project.
  The runtime holds the REPL table (`setReplTable` on `:config-reload`) and the
  processes by buffer id.
- **Windows.** If the REPL buffer is not shown, a split opens beside the current
  window. Unfocused windows on a REPL buffer follow its end as output arrives.
- **Highlighting.** The transcript is highlighted as its language.
- **Config:** `[repl.<language>]` sets `command`, `args`, `roots`,
  `multiline = [start, end]` (or `[]`), `reload`, `reload-on-save` and `enabled`.
  Built in:
  - haskell: `stack ghci`, `:{ :}`, `:reload` on save;
  - python: `python3 -i -q -u`;
  - javascript: `node -i`.

*Testing while developing:* point the Haskell REPL at the library and the test suite
(`args = ["ghci", "him:lib", "him:test:him-test"]`). Then `:repl-send main` runs the
suite. Selecting an expression (a test, or a call into the module being written) and
pressing `space e` evaluates it. Saving reloads. See the tutorial, §5.11.

**ADR-39: Per-key work follows what is visible or settled (2026-10-02).**
These are the low-hanging fixes from the 2026-10-02 benchmark:
- **Diagnostics are converted per frame for the drawn lines only**
  (`shownDiagnosticsIn`). A server can publish thousands of them.
- **The git diff is debounced:** the job waits 50 ms, and a newer version's job
  replaces it. Typing in a large tracked file therefore diffs once per pause instead
  of once per key, and the gutter catches up 50 ms after typing stops.
- **The built-in theme is parsed once,** and the TOML reader skips the character walks
  on lines that cannot need them.

Not done, on purpose:
- Moving diagnostic parsing off the main loop. It is rare (one publish per server
  pass), and doing it would need a second representation of the state.
- Writing the default theme as Haskell values, which would give two sources for one
  theme.

**ADR-40: Match mode, as in Helix.**
- `m m` jumps to the matching bracket.
- `m s c` / `m r c d` / `m d c` add, replace and delete the pair around each selection.
- `m i x` / `m a x` select inside / around a text object.

The pure part is `Him.TextObject`:
- **Words:** `w` is a run of word characters (or of punctuation, or of blanks); `W` is
  a run of non-blanks. Around takes the blanks after it, or the blanks before it at the
  end of a line.
- **Paragraphs:** `p` is the run of non-blank (or blank) lines; around adds the blank
  lines after it.
- **Pairs:** brackets nest, found by scanning lazily backwards and forwards through the
  lines. Quotes pair up within a line, in order.
- **`m`:** the innermost pair of any kind.

The commands that need a character wait for the next key the way `f` does: `Await`
gained constructors, and `Him.Actions.Match.awaitedMatchKey` handles them. Surround
edits go through `applyEdits` (each range's pair is next to it). Tree-sitter objects
(`f`, `t`, `a` in Helix) are not done.

`I` and `A` arrived with it: insert after the line's indentation, and at its end.

**ADR-41: An AI chat, with edits approved in the editor.**
- **A provider interface** (`Him.Chat.ChatProvider`), like the syntax one.
  - `cpSend config request emit` starts a request and returns a cancel action.
  - Events come back through `emit`: text as it streams, then `ChatFinished` (stop
    reason, the assistant message exactly as returned, the tool calls) or
    `ChatFailed`.
  - The runtime keeps one request per chat buffer and posts the events as
    `ChatReply` jobs.
  - Tests use a scripted provider. Nothing in the test suite talks to a model (the
    user tests live models).
- **The Claude API provider** (`Him.Chat.Anthropic`).
  - There is no Haskell SDK and only boot libraries are allowed, so it uses raw HTTP
    through `curl`. The whole request (the key header and the body) goes to curl's
    stdin as a config file: no key in the process list, no temporary file.
  - The request: `claude-opus-5-5`, adaptive thinking at `[chat] effort` (default
    `high`), streaming, `fallbacks: "default"` (beta `server-side-fallback-2026-07-01`),
    `max_tokens` 64000, and `eager_input_streaming` on every tool. Tool inputs
    are therefore parsed and checked here: an invalid one is answered with an
    `INVALID_JSON` error result, and nothing runs.
  - Credentials: `$ANTHROPIC_API_KEY`, `$ANTHROPIC_AUTH_TOKEN`, or
    `ant auth print-credentials --access-token` (Bearer plus the OAuth beta header).
  - The event stream is parsed purely (`streamStep` / `streamEnd`). Each block is
    rebuilt from its start and deltas, thinking signatures included, so the assistant
    message goes back into the history unchanged.
- **The history is append-only:** user messages, assistant messages as returned, tool
  results. Earlier turns are never edited, as preserved thinking requires.
  - A refused or cut-off turn gets error results for its tool calls, and they never
    run.
- **Tools** (`Him.Chat.Tools`):
  - `read_file` (the buffer's text if the file is open) and `list_files` run at once.
  - `edit_file` (one exact occurrence of `old_text`) and `write_file` become **pending
    edits**. Each is applied to the file's buffer in the editor window as an undoable
    change of whole lines, highlighted with `ui.highlight`, and summarized as a diff in
    the chat.
  - Paths must stay inside the project.
- **Deciding edits:** `space c a` / `space c d` approve or deny the next edit, and
  `A` / `D` do all of them.
  - Approving keeps the edit and saves the file.
  - Denying puts the old lines back.
  - When every edit of a turn is decided, the tool results go back in the order of the
    calls, and the model continues.
- **The chat buffer** is a transcript like the REPL's (`Him.Transcript`, shared since
  then): `ret` sends, `A-ret` makes a line break, and `C-c` stops the answer. Each
  message is prefixed with the file and line the user is looking at.
- **Transcripts never count as unsaved** (`Document.unsaved`), so `:q` does not refuse
  over a REPL or chat buffer.
- **`Runtime` now takes the `Config`** (`newRuntime config post`, `reconfigure`), instead
  of one argument and one setter per table.

**ADR-42: Claude Code as a chat provider, with him's tools over MCP.**
`[chat] provider = "claude-code"` is the default. It uses the `claude` program you are
logged in to, so no API key is needed, and it keeps the approve-before-writing design
of ADR-41.

- **Sessions.** Providers now start a session per chat buffer (`cpStart` →
  `ChatSession { sessSend, sessAnswer, sessClose }`), like syntax providers. The API
  provider's session is stateless. Claude Code's keeps one `claude -p` process with
  streaming JSON in and out.
  - A cancelled turn ends the process, and the next turn uses `--resume <session id>`.
  - A conversation that starts over (`:chat-new`) starts a new session.
- **Tools.** Claude Code's own editing and command tools are off: `--tools Grep,Glob`
  (read-only search) and `--permission-mode dontAsk` (anything not allowed is denied,
  never asked). Its file tools are him's, served over MCP:
  `--mcp-config` names `him --mcp-bridge DIR`, and `--strict-mcp-config` ignores the
  user's other MCP servers.
- **The bridge.** Claude Code starts MCP servers itself, over stdio, so the server can't
  be the running editor. `Him.Mcp` is a small bridge that speaks MCP (newline-delimited
  JSON-RPC: `initialize`, `ping`, `tools/list`, `tools/call`) and forwards each tool
  call to the editor. `mcpStep` is the pure part.
- **Bridge ↔ editor.** Two named pipes in a temporary directory: `calls` and `answers`,
  one JSON object per line. The boot libraries have no sockets, and `unix` has named
  pipes.
  - GHC opens files non-blocking. On a named pipe, that makes a write-open fail with
    no reader, and a read-open see end-of-file at once.
  - So the editor opens both pipes **read-write** (Linux allows it; it never blocks),
    before Claude Code starts. The bridge's opens always find this end, and a bridge
    restart is invisible to the editor.
  - Call ids carry the bridge's process id, so an answer left behind by a bridge that
    died is ignored by the next.
- **Live tool calls.** A call arrives during the turn as `ChatToolCall`.
  - Reads are answered at once.
  - An edit becomes a pending edit (ADR-41) and is answered when you approve or deny
    it. Claude Code waits meanwhile and goes on by itself afterwards, in the same turn.
  - The batch path of the API provider (tool calls with the finished reply, results
    with the next request) is unchanged. Both share `runCall`.
- **Output.** Text streams from `stream_event` lines (`--include-partial-messages`). The
  use of its own tools shows as `[Grep]`, and the `result` line ends the turn (or
  fails it).
- **Not tried against the live service** (the user tests live models). The tests cover:
  - Claude Code's output shapes;
  - the MCP messages;
  - a real round trip, client → bridge → named pipes → editor → answer;
  - the live approve flow with a fake session that waits for answers like Claude Code.

  `him --mcp-bridge` was also checked by hand, with a scripted MCP handshake.

**ADR-8: No test framework.**
The tests live in `test/Test/<Area>.hs` (Text, Formats, Config, Git, Lsp, Syntax,
Render, Integration, with helpers in `Test.Util`), and `test/Spec.hs` runs them.
`test/Test/Harness.hs` is about 50 lines and does `test`, `group`, `assertEqual`, and
`runTests`, which keeps us within the boot libraries. hspec/tasty can be adopted later
if needed.

## 4. Module map

Every module starts with a header comment saying what it is for; this is the overview.
Pure modules are marked *(pure)*.

**Core text and editing**

| Module | Responsibility |
|---|---|
| `Him.Buffer`, `Him.Buffer.Rope` | Text storage: a weight-balanced tree of blocks with lazy line starts (ADR-12…16); insert, delete, ranges, search hooks *(pure)*. |
| `Him.Native` + `cbits/text.c` | `unsafe` FFI on text arrays: newline scans (SSE2), forward/backward search. |
| `Him.Position`, `Him.Selection` | Positions; Helix selections: ranges with anchor and head, a primary *(pure)*. |
| `Him.Motion`, `Him.Edit` | Motions and edits applied to every range (ADR-18) *(pure)*. |
| `Him.TextObject` | Match mode's objects (`m i w`, `m a (`), the pair around a position, the matching bracket (ADR-40) *(pure)*. |
| `Him.Search`, `Him.Regex` | Literal search with smart case and wrap-around; the regex subset used by highlight queries (ADR-28) *(pure)*. |
| `Him.History` | Undo/redo snapshots *(pure)*. |
| `Him.TextWidth`, `Him.View` | Display columns (tabs, wide and control characters); scrolling with scrolloff *(pure)*. |
| `Him.Document` | A buffer with its selection, path, history, kind (text, directory listing, REPL, chat), git/LSP/syntax state; `changeDocument`, `replaceBuffer`, `unsaved` *(pure)*. |
| `Him.Transcript` | REPL and chat buffers: output before the input, `ret` takes the input (ADR-38, ADR-41) *(pure)*. |
| `Him.File`, `Him.Directory`, `Him.FileTree`, `Him.Ignore` | Loading and saving files (zero-copy for valid UTF-8); directory listings; the ignore-aware parallel file walk; gitignore rules. |

**Editor state, actions and keys**

| Module | Responsibility |
|---|---|
| `Him.Editor` | The whole editor state: the focused document and view, the buffer zipper, windows (ADR-37), popups, plugin state; `windowEditor`, `pendingEditLines` *(pure)*. |
| `Him.Window` | The layout tree of windows, boxes, neighbours (ADR-37) *(pure)*. |
| `Him.Mode`, `Him.Key`, `Him.Keymap` | Modes and keymap layers (directory, completion, REPL, chat); keys; keymap tries. |
| `Him.EditorM` | The monad actions run in and its helpers (`edit`, `motion`, `request`, `info`). |
| `Him.Action`, `Him.Invocation` | Named actions with typed parameters; invocations as text (ADR-17). |
| `Him.Actions.*` | The actions: `Motion`, `Edit`, `Search`, `Match`, `File` (`:` commands for files, buffers, quitting), `CommandLine`, `Picker`, `Directory`, `Window`, `Syntax`; the plugins `Git`, `Lsp` (+ `Lsp.Core`, `.Navigation`, `.Edits`, `.Completion`), `Repl`, `Chat`. |
| `Him.Ex`, `Him.Info`, `Him.Palette`, `Him.Picker` | `:` commands; the info box after a prefix; the command palette; pickers and fuzzy ranking. |
| `Him.Config`, `Him.Config.Default` | `Config` and the `Plugin` record (ADR-35); the default bindings, actions and plugins; `configWith`. |
| `Him.Options`, `Him.UserConfig`, `Him.Toml`, `Him.Paths` | Settings (ADR-34); the config file (ADR-32); the TOML reader; where files live. |

**Running it**

| Module | Responsibility |
|---|---|
| `Him.App` | The terminal frontend: raw mode, input, the loop (batching, rendering, loop-only effects), themes. |
| `Him.Session` | Events to state changes without a frontend: keys, job results, effects, housekeeping, plugins (ADR-36). |
| `Him.Effect`, `Him.Event`, `Him.Runtime` | Effects and jobs as data; events; the runtime that runs jobs, language servers, REPLs and chat requests (ADR-23). |
| `Him.Process`, `Him.Json`, `Him.Log` | Running programs; JSON; debug logging. |
| `Him.Terminal.*` | Raw mode, input decoding, output, size, escape sequences (incl. OSC 10/11 colours). |
| `Him.Render`, `Him.Render.*` | Layout per window and the components: `Gutter`, `TextArea`, `StatusLine`, `CommandLine`, `Info`, `Picker`, `Completion`; `Frame` and `Diff` (ADR-15). |
| `Him.Theme`, `Him.Theme.Load`, `Him.Render.Theme` | Helix theme files *(pure)*; finding and loading them; the render-side theme (ADR-33). |

**Subsystems**

| Module | Responsibility |
|---|---|
| `Him.Syntax`, `Him.Syntax.Span`, `Him.Syntax.TreeSitter`, `Him.Language`, `Him.GrammarBuild` + `cbits/ts_*.c`, `cbits/tree-sitter` | Highlighting: the provider interface (ADR-26), tree-sitter (ADR-27), languages, `him --build-grammars`. |
| `Him.Diff`, `Him.GitState`, `Him.Git` | Line diffs, a document's git state and signs *(pure)*; running git (ADR-25). |
| `Him.Lsp.*` | The LSP client: protocol, state, sync, edits *(pure)*, server processes, the server table (ADR-29). |
| `Him.Repl`, `Him.Repl.Process` | REPL config and state *(pure)*; the process (ADR-38). |
| `Him.Chat`, `Him.Chat.Tools`, `Him.Chat.Anthropic`, `Him.Chat.ClaudeCode`, `Him.Mcp` | The chat provider interface (sessions) and state, the model's tools and pending edits *(pure)*; the Claude API provider over curl (ADR-41); the Claude Code provider and the MCP bridge (`him --mcp-bridge`) that serves him's tools to it (ADR-42). |

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
- [x] **21. Foundation, palette, async picker** (roadmap phases 0–1). Effects and
  background jobs (ADR-23), `:action`, `Him.Json`, `Him.Process`; the command palette
  `space ?`; the streaming, parallel file picker with background filtering (ADR-24).
- [x] **22. Git** (roadmap phase 2). Gutter signs for added/changed/removed lines,
  staged ones dimmer; `] g` / `[ g`; stage, unstage or reset the selected lines or the
  file from the editor (ADR-25).
- [x] **23. Syntax highlighting** (roadmap phase 3). One provider interface (ADR-26);
  the tree-sitter provider with a vendored runtime and grammars built by
  `him --build-grammars` (ADR-27); `Him.Regex` (ADR-28).
- [x] **24. LSP client** (roadmap phase 4). Diagnostics, hover, definition, references
  and completion, with servers run by the runtime (ADR-29).
- [x] **25. More LSP.** Incremental sync (`Buffer.changeBetween`), `didSave` and
  `didClose`; rename, format, code actions, signature help, document symbols, type
  definition and implementation; `:lsp-start`, `:lsp-stop`, `:lsp-restart` and
  `:lsp-info` (ADR-29).
- [x] **26. Previews, workspace symbols, imports.** Picker previews (ADR-30), `space S`,
  completion imports, references on `g r` (ADR-29).
- [x] **27. Movement.** Page and half-page motions, `f t F T` with `A-.`, `<count> g g`,
  and Ctrl-Z to suspend (ADR-31).
- [x] **28. Reload.** `:reload`, `:reload!`, `:reload-all`: the file is read again as one
  undoable change, keeping the cursor; modified buffers are refused unless forced.
- [x] **29. Config file.** `config.toml` with keys, editor settings and language servers;
  `him --dump-default-config`, `:config-open`, `:config-reload` (ADR-32).
- [x] **30. Themes.** Helix theme files (`[editor] theme`, `:theme`), styles layered
  as in Helix, underline kinds and colours, the theme's background through OSC 11, and
  a 256-colour fallback (ADR-33).
- [x] **31. Settings.** Tab width, expand-tab, relative line numbers, cursor shapes,
  completion, search, file-picker and preview settings, and the escape timeout, all
  from one table (ADR-34).
- [x] **32. Plugins.** Git and LSP as plugins: `[plugins]`, `:plugins`,
  `:plugin-enable`, `:plugin-disable` (ADR-35).
- [x] **33. Modules.** `Him.Session`, `Him.Actions.*`, `Him.EditorM`, the LSP split,
  and the test modules (ADR-36).
- [x] **34. Splits.** Windows side by side and stacked, Helix's `C-w` / `space w`
  keys, `:vsplit`, `:hsplit` (ADR-37).
- [x] **35. REPL plugin.** `:repl`, `space e` (send the selection), `space E`
  (reload), reload on save, typing in the REPL buffer (ADR-38).
- [x] **36. Match mode.** `m m`, `m s`, `m r`, `m d`, `m i`, `m a`; `I` and `A` (ADR-40).
- [x] **37. AI chat plugin.** A chat beside the code; the model's edits wait for
  approval in the editor (ADR-41).
- [x] **38. Claude Code provider.** The chat through `claude`, with him's tools served
  over MCP by `him --mcp-bridge` (ADR-42).

Later (not started; the architecture has room for them):
- [ ] Highlight all matches of a search; regex search on `Him.Regex`; `S` (split the
  selection on a pattern).
- [ ] Undo tree / change sets instead of snapshots; named registers and the system
  clipboard.
- [ ] Incremental parsing and injections for tree-sitter (ADR-27); highlighting in the
  picker preview.
- [ ] Global search picker (`space /`).
- [ ] Detecting files changed on disk.

## 6. Keybindings

Defined in `Him.Config.Default` (core) and in each plugin (`plBindings`).
`him --dump-default-config` prints all of them with what they do, and `space ?` lists
them in the editor.

| Mode | Keys |
|---|---|
| Normal (moving) | counts (`5 j`); `h j k l`, arrows, `home`/`end`; `w b e` (words); `g g` / `g e` (first / last line), `<count> g g` (that line), `g h` / `g l` (line start / end); `C-f` / `C-b` (also `pagedown` / `pageup`), `C-d` / `C-u` (half pages); `f t F T` + a character, `A-.` repeats; `m m` (the matching bracket); `C-z` suspends. |
| Normal (selecting) | `x` (line, repeat to extend), `;` (collapse), `v` (select mode), `%` (all), `s` (matches inside the selection), `C` (copy the selection to the next line), `,` / `A-,` (keep / remove the primary), `(` / `)` (rotate), `A-s` (split into lines); `m i` / `m a` + `w W p m ( [ { < " ' \`` (inside / around a text object). |
| Normal (editing) | `i a I A o` (insert before / after / at the line's start / end / on a new line), `d` (delete), `c` (change), `y` (yank), `p` / `P` (paste after / before), `u` / `U` (undo / redo); `m s` + a character (surround), `m r` + two (replace the pair), `m d` + one (delete the pair; `m` the closest). |
| Normal (search) | `/` / `?` (with preview), `n` / `N`, `*` (selection as the pattern). |
| Select | Normal mode where motions extend; `v` / `esc` back. |
| Insert | typing, `ret` (keeps indentation), `tab` (spaces with `expand-tab`), `backspace`, `del`, arrows, `esc`. |
| Command line | typing, `tab` (complete names, paths, themes, plugins), `backspace`, `ret`, `esc`. |
| Buffers, pickers | `g n` / `g p` (next / previous buffer), `space f` (files), `space b` (buffers), `space ?` (every action); in a picker: type to filter, `up`/`down`/`C-n`/`C-p`/`tab`/`S-tab`, `ret`, `esc`. |
| Directory listings | `space d` (the file's directory), `space D` (the working directory), `:o dir`; in a listing: `ret`, `-` / `^` / `backspace` (parent), `g r` (refresh), `a` (new file or `dir/`), `+` (new directory), `r` (rename), `d` (delete, asks), `g .` (dotfiles). |
| Windows | `C-w` or `space w`, then `v` / `s` (split side by side / stacked), `w` (next), `h j k l` (focus), `H J K L` (swap), `q` (close), `o` (only), `n v` / `n s` (split with a scratch buffer). |
| git plugin | `] g` / `[ g` (next / previous change); `space g s` / `u` (stage / unstage the selected lines), `S` / `U` (the file), `r` (reset the lines). |
| lsp plugin | `space k` (hover), `g d` / `g y` / `g i` / `g r` (definition, type definition, implementation, references), `space s` / `space S` (symbols / in the project), `space r` (rename), `space a` (code actions), `space x` / `] d` / `[ d` (diagnostics); insert mode: completion (`C-x`, `tab` / `C-n` / `C-p`, `ret`), signature help. |
| repl plugin | `space e` (send the selection or line), `space E` (reload); in the REPL buffer (insert): `ret` sends, `C-c` interrupts. |
| chat plugin | `space c c` (open the chat), `space c s` (put the selection into the message), `space c a` / `space c d` (approve / deny the next pending edit), `space c A` / `space c D` (all of them); in the chat (insert): `ret` sends, `A-ret` a line break, `C-c` stops the answer. |

`:` commands (`tab` completes, and the `:` menu lists them as you type):
- **files and buffers:** `:w [path]`, `:wa`, `:wq` / `:x`, `:wqa`, `:q` (closes the window; quits with the last), `:q!`, `:qa`, `:qa!`, `:o` / `:e path…`, `:reload` (`!`), `:reload-all`, `:new`, `:bc` (`!`), `:cd`, `:pwd`;
- **windows:** `:vsplit` / `:vs [files]`, `:hsplit` / `:hs [files]`, `:vnew`, `:hnew`;
- **config:** `:theme [name]`, `:config-open`, `:config-reload`, `:plugins`, `:plugin-enable` / `:plugin-disable <name>`, `:action <invocation>`;
- **plugins:** `:format`, `:lsp-info`, `:lsp-start`, `:lsp-stop`, `:lsp-restart`; `:repl [language]`, `:repl-send <text>`, `:repl-reload`, `:repl-interrupt`, `:repl-stop`, `:repl-restart`; `:chat`, `:chat-new`, `:chat-approve [all]`, `:chat-deny [all]`.

## 7. How to extend

- **An action:** write the `EditorM ()` code (keep the logic pure where you can, as in
  `Him.Motion`, `Him.Edit`, `Him.TextObject`). Wrap it with `simple name group doc run`, or
  `action name group doc spec run` when it takes arguments (`int`, `text`, `choice`,
  `optional`). Add it to an action list in `Him.Actions.*`. The name is public, because
  bindings and config files use it.
- **A key:** add `("g h", "goto_line_start")` to the mode's list in `Him.Config.Default`
  (or a plugin's `plBindings`). `buildConfig` rejects unknown actions and bad arguments,
  and a test checks the defaults. Prefix titles (`"match"`, `"window"`) go in
  `prefixNames` or `plPrefixNames`.
- **A `:` command:** an `ExCommand` (names, doc, argument kind for completion, run) in a
  module's `exCommands`, listed in `Him.Config.Default` (or `plExCommands`).
- **A setting:** one `OptionSpec` in `Him.Options.optionSpecs`; checking, applying and
  the dumped default follow (ADR-34).
- **A plugin:** a `Plugin` record (`Him.Config`): actions, bindings, commands, hooks
  (housekeeping, per batch, job results, enable/disable); add it to `plugins` in
  `Him.Config.Default` (ADR-35). It can be switched off like the others.
- **A provider:** a highlighter is a `SyntaxProvider` (ADR-26), a chat backend a
  `ChatProvider` (ADR-41); register it in `Him.Config.Default`.
- **A render component:** `Theme -> Editor -> Rect -> Frame -> Frame` in
  `Him.Render.<Name>`, composed in `Him.Render` (per window or over everything).
- **Debugging:** `HIM_LOG=/tmp/him.log make run ARGS=file`, `tail -f /tmp/him.log` in
  another terminal. Never print to stdout while the terminal is in raw mode.

## 8. Where to pick up

*Last updated 2026-10-02.* Everything the user asked for so far is done; the latest
work is match mode and `I` / `A` (ADR-40), the AI chat plugin (ADR-41) with Claude
Code as its default provider over MCP (ADR-42), and a sweep of the repository and the
documents.

- **State:** milestones 1–38 (§5) and ADR-1…42 (§3). `make test` runs 552 tests (pure
  modules, key sequences through the real keymap, git in a temporary repository,
  clangd when installed, tree-sitter when grammars are built, REPLs with `cat`, the
  chat with a scripted provider).
- **Benchmarks:** the reference is `docs/BENCHMARK.md`, 2026-10-02 (idle machine,
  `him` and `him-lite`, plain text and an IDE-like setup). Open items there: the first
  paint of a large plain file (Helix 23 vs him 30 ms), per-key latency against Vim
  (0.5 vs about 1.1 ms), and parsing large diagnostic lists on the main loop.
- **The chat plugin has not been tried against a live model** (the user tests live
  models themselves). The default provider, `claude-code`, needs the `claude` program
  and a login; `provider = "anthropic"` needs `ANTHROPIC_API_KEY` (or
  `ANTHROPIC_AUTH_TOKEN`, or `ant auth login`). `[chat]` sets the model and effort.
  Things to watch on the first live run: Claude Code's stream-json input format for
  user messages, and that `--tools Grep,Glob` plus `--allowedTools mcp__him__*` gives
  it exactly him's tools (`Him.Chat.ClaudeCode.claudeArgs`).
- **Ideas, roughly by value:**
  1. Regex search and `S` (split on a pattern), on `Him.Regex`.
  2. Incremental tree-sitter parsing (the buffer's `changeBetween` is ready) and
     injections.
  3. Detecting files changed on disk.
  4. Chat: show the model's reasoning summaries (`display: "summarized"`), a picker of
     pending edits, more tools (search the project).
  5. Moving the buffer zipper out of `Editor`, and per-subsystem job runners (ADR-36
     left them for later).
- **Known issues:**
  - Zero-width combining characters are treated as width 1; case-insensitive search
    folds ASCII letters only.
  - `s` scans from each range's start; with many ranges and few matches it is slow.
    `/` and `n` move only the primary range.
  - Files changed on disk are not noticed (`:reload` / `:rla`), and saving does not warn
    about them. Git signs follow saves, staging and switching buffers only.
  - LSP, deferred by choice: inlay hints, semantic tokens, snippets (inserted as plain
    text), and file operations in code actions.
  - Highlighting needs `him --build-grammars` once (ADR-27); without grammars, files
    are plain and only `$HIM_LOG` says why. Syntax sessions are not closed with their
    buffer. The preview is not highlighted.
  - A directory listing does not refresh by itself (`g r`). Deleting a file leaves its
    buffer open.
  - The info box and picker measure text by characters, so wide characters can
    misalign their right border.
  - An unfocused window's selection is not moved by edits made in another window on
    the same document; it is clamped (ADR-37).
  - A chat edit's lines are tracked by line number; editing above a pending edit by
    hand before deciding it moves it (ADR-41).
  - The MCP bridge's pipes are opened read-write by the editor, which Linux allows but
    POSIX leaves undefined (ADR-42).
- **Working rules:**
  - Revert temporary instrumentation by editing it out (or `git checkout` on a clean
    tree only).
  - Manual tests in tmux run from a scratch directory, never the repository: two stray
    files (`src/Him/Coq!`, `NEXT_SESSION_PROMPT.md`) were once saved into it that
    way and committed.
  - No automated tests against a live model; the user does those.
- **How to verify:** `make test`. For a manual check:
  `tmux new-session -d -s t -x 100 -y 30 "$(stack path --local-install-root)/bin/him file"`
  from a scratch directory, then `tmux send-keys` / `tmux capture-pane -p`. Benchmarks need
  the optimized build (`stack build`, not `--fast`).
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
