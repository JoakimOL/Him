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

**ADR-8: No test framework.**
The tests live in `test/Test/<Area>.hs` (Text, Formats, Config, Git, Lsp, Syntax,
Render, Integration, with helpers in `Test.Util`), and `test/Spec.hs` runs them.
`test/Test/Harness.hs` is about 50 lines and does `test`, `group`, `assertEqual`, and
`runTests`, which keeps us within the boot libraries. hspec/tasty can be adopted later
if needed.

## 4. Module map

Legend: ✅ exists, ⏳ planned.

| Module | Status | Responsibility |
|---|---|---|
| `Him.App` | ✅ | The terminal frontend: raw mode, input, the loop (batching, rendering, loop-only effects such as suspend, reload, theme, plugins), theme loading. |
| `Him.Session` | ✅ | The session without a frontend: `handleEvent` (keys through the keymap, job results), effects that need the config, housekeeping, plugin switching, config loading. Tests drive it directly. |
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
| `Him.Actions.Search` | ✅ | `/ ? n N *`, the search prompt, and `refreshSearchPreview`. |
| `Him.Buffer` | ✅ | Abstract text storage (`Seq Text`), path, dirty flag. |
| `Him.Position`, `Him.Selection` | ✅ | `Pos`, `Range {anchor, head}`, `Selection` (sorted, merged NonEmpty ranges + primary; `fromRanges`, `normalize`, primary operations). |
| `Him.Motion` | ✅ | Pure motions: char, line (desired column), word, line/file start/end. |
| `Him.Edit` | ✅ | Pure single-range edits, and `applyEdits`, which applies one to every range (ADR-18). |
| `Him.Editor`, `Him.Mode`, `Him.View` | ✅ | Editor state (the buffer zipper, ADR-19; `InfoBox`; the open picker), modes (`Normal`, `Insert`, `Select`, `CmdLine`, `Picking`), viewport + scrolloff. |
| `Him.Info` | ✅ | `refreshInfo`: the info box after a prefix key or on the `:` line (ADR-20). |
| `Him.Ignore`, `Him.FileTree` | ✅ | The gitignore matcher (pure), and the ignore-aware breadth-first file walk for the picker (ADR-21). |
| `Him.Directory`, `Him.Actions.Directory` | ✅ | Directory listings as read-only documents (`loadPath`, `loadDirectory`, `entryAt`, `selectEntry`), and their actions (ADR-22). |
| `Him.Diff` | ✅ | Myers line diff with trimming; `applyHunks`, `mapLine` (ADR-25). |
| `Him.GitState`, `Him.Git`, `Him.Actions.Git` | ✅ | A document's git state, gutter signs and `applySelected` (pure); the `git` commands; housekeeping, change navigation, stage/unstage/reset actions (ADR-25). |
| `Him.Syntax`, `Him.Syntax.Span`, `Him.Language`, `Him.Actions.Syntax` | ✅ | The provider interface, spans, language detection, and highlighting housekeeping (ADR-26). |
| `Him.Syntax.TreeSitter`, `Him.GrammarBuild` + `cbits/tree-sitter`, `cbits/ts_shim.c` | ✅ | The tree-sitter provider and the grammar builder (`him --build-grammars`) (ADR-27). |
| `Him.Regex` | ✅ | Backtracking regex subset and Lua patterns (ADR-28). |
| `Him.Lsp.Protocol`, `Him.Lsp.State`, `Him.Lsp.Config`, `Him.Lsp.Server`, `Him.Actions.Lsp` | ✅ | The LSP client: pure protocol and editor state, server table, server processes, and the editor-side actions and housekeeping (ADR-29). |
| `Him.Actions.Lsp.{Core,Navigation,Edits,Completion}` | ✅ | `Actions.Lsp` split by feature: requests, attaching and syncing; definitions, references, symbols, diagnostics; edits, code actions, format, rename; completion and signature help. `Actions.Lsp` keeps the plugin, the actions and the result dispatch. |
| `Him.Lsp.Sync`, `Him.Lsp.Edit` | ✅ | Sync messages (incremental `didChange`, `didSave`, `didClose`); parsing and applying text and workspace edits. |
| `Him.Render.Completion` | ✅ | The completion menu. |
| `Him.Effect`, `Him.Runtime` | ✅ | Effects as data (`RunAction`, `OpenPalette`, `StartJob`, `CancelJob`) and the background-job runtime (ADR-23). |
| `Him.Invocation` | ✅ | Pure invocation parsing/rendering (re-exported by `Him.Action`). |
| `Him.Process`, `Him.Json` | ✅ | External programs with stdin/stdout/stderr; a JSON value type, parser and encoder. |
| `Him.Palette` | ✅ | The command palette's rows: every action with its parameters, keys (from the config) and doc. |
| `Him.Picker`, `Him.Actions.Picker` | ✅ | Pure picker (fuzzy matching, selection); `space f` / `space b` and the picker keys; `listFiles`. |
| `Him.Action` | ✅ | Actions (name, group, doc, typed parameters), the registry, invocation parsing (`name arg "quoted arg"`), and binding to a runnable `Bound` (ADR-17). |
| `Him.EditorM`, `Him.Keymap` | ✅ | `EditorM` and helpers for writing actions; per-mode keymap tries, generic in what they bind (`Keymap a`). |
| `Him.Ex` | ✅ | `:`-command parser. |
| `Him.Render`, `Him.Render.*` | ✅ | Frame, layout, components, diffing. Components: `Gutter` (line numbers), `TextArea`, `StatusLine`, `CommandLine`, `Info` and `Picker` (popups over the text area). `layout` depends on the editor, because the gutter width follows the line count. |
| `Him.File` | ✅ | Load/save (UTF-8, line endings, trailing newline). |
| `Him.History` | ✅ | Undo/redo snapshots: `beginChange` (called by `edit`), `commit` (called by the main loop outside insert mode), `undo`, `redo`. |
| `Him.Document` | ✅ | Buffer + selection + path + dirty flag + line ending/trailing newline. (Split out of `Buffer` so the buffer stays pure text.) |
| `Him.Config` | ✅ | `Config { cfgActions, cfgKeymaps, cfgFallback }`, held by the main loop rather than the `Editor`, which avoids a module cycle. `Bindings`, `overrideBindings` and `buildConfig`, which validates every binding. |
| `Him.Actions.*` | ✅ | Action lists: `Motion`, `Edit` (modes and text), `Search`, `CommandLine`; `File` holds the ex commands. |
| `Him.TextWidth` | ✅ | Tab expansion (width 4), `charWidth` (a compact East-Asian-wide/emoji table; control chars are 2 wide and shown as `^X`), char↔display-column mapping. |
| `Him.Toml`, `Him.UserConfig` | ✅ | The TOML subset reader; the user's config file: checking, applying, the dumped defaults (ADR-32). |
| `Him.Repl`, `Him.Repl.Transcript`, `Him.Repl.Process`, `Him.Actions.Repl` | ✅ | The REPL plugin: config and state (pure), the transcript (pure), the process, the plugin's actions and commands (ADR-38). |
| `Him.Window`, `Him.Actions.Window` | ✅ | Splits: the layout tree, boxes and neighbours (pure); window actions, keys and `:vsplit`/`:hsplit` (ADR-37). The editor's window operations (`focusWindow`, `splitWindow`, `closeWindow`, `windowEditor`, …) are in `Him.Editor`. |
| `Him.Options` | ✅ | The settings (`Options`, `edOptions`) and the table that checks, applies and dumps them (ADR-34). |
| `Him.Paths` | ✅ | Where things are: the config file, the runtime directories, the theme directories. |
| `Him.Theme`, `Him.Theme.Load`, `Him.Render.Theme` | ✅ | Helix theme files: parsing, colours, `inherits`, the built-in theme, 256-colour fallback (pure); finding and loading them; the render-side `Theme` built from scopes (ADR-33). |
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

Later (the architecture already has room for these):
- [ ] Regex search (a small engine of our own, since there is none in the boot libraries)
- [ ] Highlight all matches
- [ ] Undo tree / change sets instead of snapshots
- [ ] Named registers and the system clipboard
- [ ] Syntax highlighting (a styling pass at render time)
- [ ] More pickers (global search, symbols) and a scrollable `:help` listing of actions

## 6. Keybindings

Implemented (defined in `Him.Config.Default`):

| Mode | Keys |
|---|---|
| Normal | counts (`5 j`, `3 w`, `2 x`) on `h j k l`, arrows, `w b e`, `x`; `h j k l`, arrows, `home`/`end`; `w b e` (select words), `x` (select line, repeat to extend), `;` (collapse), `v` (select mode), `d` (delete), `c` (change); `y` (yank), `p` / `P` (paste after / before); `u` / `U` (undo / redo); `g g` / `g e` (first / last line), `g h` / `g l` (line start / end); `i a o`; `:` |
| Normal (movement) | `C-f` / `C-b` (also `pagedown` / `pageup`) page down / up, `C-d` / `C-u` half page; `f` / `t` + a character: select to / up to its next occurrence, `F` / `T` backwards (counts work, `ret` finds a line break); `A-.` repeats the last one; `<count> g g` goes to that line; `C-z` suspends (`fg` in the shell returns). |
| Select | same as normal, but motions extend; `v` / `esc` → normal |
| Insert | printable chars, `ret` (keeps indent), `tab`, `backspace`, `del`, arrows, `esc` |
| Normal (selections) | `%` (select all), `s` (select matches in the selection, with preview), `C` (copy the selection onto the next line), `,` (keep the primary), `A-,` (remove the primary), `(` / `)` (rotate the primary), `A-s` (split into lines). The status line shows `i/n sels`. |
| Normal (search) | `/` / `?` (search forward / backward, with preview), `n` / `N` (next / previous match), `*` (selection becomes the pattern) |
| Command line | printable chars, `backspace` (leaves when empty), `ret`, `esc` (a search restores the selection) |
| `:` commands | `:w [path]`, `:q` / `:qa` (refuse when any buffer is modified), `:q!` / `:qa!`, `:wq` / `:x`, `:wa`, `:wqa` / `:xa`, `:open` / `:o` / `:e path...`, `:reload` / `:rl` (refuses unsaved changes; `:reload!` discards them; undoable), `:reload-all` / `:rla` (skips modified buffers), `:new` / `:n`, `:buffer-close` / `:bc` (`!` discards), `:buffer-next` / `:bn`, `:buffer-previous` / `:bp`, `:theme [name]`, `:config-open`, `:config-reload`. `tab` completes names, paths and theme names. |
| Directory listings | `:o dir`, `him dir`, `space d` (the current file's directory, cursor on the file), `space D` (the working directory). In a listing: normal motions and search, `ret` (enter a directory / open a file), `-` or `backspace` (parent), `g r` (refresh), `a` (new file, or directory with a trailing `/`), `+` (new directory), `r` (rename/move), `d` (delete the selected entries, asks `y`), `g .` (show/hide dotfiles). `:cd [dir]` (default: the listed directory), `:pwd`. |
| Language server | Diagnostics in the gutter, underlined, and the cursor line's message at the bottom; `] d` / `[ d` next/previous, `space x` list. `space k` hover, `g d` definition, `g y` type definition, `g i` implementation, `g r` references, `space s` symbols, `space S` workspace symbols, `space r` rename, `space a` code actions, `:format`. Insert mode: completion opens by itself (or `C-x`); `tab`/`C-n`/`down` and `S-tab`/`C-p`/`up` select, `ret` accepts, `esc` closes; signature help appears after `(` and `,`. `:lsp-start`, `:lsp-stop`, `:lsp-restart`, `:lsp-info`. Servers: hls, rust-analyzer, clangd, typescript-language-server, pylsp, gopls (`Him.Lsp.Config`). |
| Git | Gutter signs (green added, yellow changed, red removed; dimmer when staged). `] g` / `[ g` next/previous change; `space g s` / `space g u` stage/unstage the selected lines, `space g S` / `space g U` the whole file, `space g r` reset the selected lines to the index. |
| Picker preview | Items that are places (files, buffers, symbols, references, diagnostics) show the file around their line beside the list. |
| Windows | `C-w` or `space w`, then: `v` / `s` split side by side / stacked, `w` next, `h j k l` focus, `H J K L` swap, `q` close, `o` only, `n v` / `n s` split with a scratch buffer. `:vsplit` / `:vs [files]`, `:hsplit` / `:hs [files]`, `:vnew`, `:hnew`; `:q` closes the window (quits with the last). |
| REPL | `:repl [language]` opens it beside the file; in it, type and `ret` (insert mode) sends, `C-c` interrupts. From a file: `space e` sends the selection (or the line), `space E` reloads (also after saving, for ghci). `:repl-send <text>`, `:repl-reload`, `:repl-interrupt`, `:repl-stop`, `:repl-restart`. |
| Buffers and pickers | `g n` / `g p` (next / previous buffer), `space f` (file picker), `space b` (buffer picker), `space ?` (command palette: every action, its keys and doc; one with arguments opens `:action <name> `). `:action <invocation>` runs any action. In a picker: type to filter, `up`/`down`/`C-p`/`C-n`/`tab`/`S-tab` move, `ret` opens, `esc` closes. |

Actions that take arguments, and have no default key yet: `move_char_left/right`,
`move_line_up/down [count]`, `goto_line <line>`, `insert_text <text>`,
`set_mode normal|insert|select`, `search_text <pattern>`, `ex <command>`, and `no_op`
(it disables a key).

## 7. How to extend

- **Add an action:** write the `EditorM ()` code (keep the logic pure in `Him.Motion` or
  `Him.Edit` where you can). Wrap it with `simple name group doc run`, or with
  `action name group doc spec run` when it takes arguments. The spec is built from
  `int`, `text`, `choice` and `optional`, e.g. `optional "1" 1 (int "count")`. Add it to
  an action list in `Him.Actions.*` (each list is part of `allActions`). The name is
  public, because bindings and config files use it, so choose it carefully.
- **Users rebind keys** in their config file (ADR-32); `him --dump-default-config` shows
  everything. New editor settings go in `Him.UserConfig` (parse, apply, dump).
- **Add a keybinding:** add `("g h", "goto_line_start")` or `("C-d", "move_line_down 20")`
  entries to that mode's list in `Him.Config.Default`. Chords are parsed by `Him.Key`. At
  startup, `buildConfig` rejects unknown actions and bad arguments, and a test checks the
  defaults.
- **Add a `:` command:** add an `ExCommand` (names, doc, `[Text] -> EditorM ()`) to
  `Him.Actions.File` or a new list, and include it in `exCommands` in `Him.Config.Default`.
- **Add a `:` command:** give it `PathArgs` if its arguments are paths, so `tab`
  completes them. It shows up in the `:` menu automatically.
- **Name a key prefix:** add it to `prefixNames` in `Him.Config.Default`, which gives the
  info box a title such as "goto".
- **Add a picker:** build `PickerItem`s with a `PickTarget` (add a constructor for a
  new kind of target), open them with `newPicker`, and handle the target in
  `picker_accept` (`Him.Actions.Picker`).
- **Add a render component:** write `Theme -> Editor -> Rect -> Frame -> Frame` in
  `Him.Render.<Name>`, give it a `Rect` in `layout`, and compose it in `render`
  (`Him.Render`).
- **Debugging:** run `HIM_LOG=/tmp/him.log make run ARGS=file` and `tail -f /tmp/him.log` in
  another terminal. Never print to stdout while the terminal is in raw mode.

## 8. Where to pick up

*Last updated 2026-10-02. The queue agreed with the user is done: themes, settings,
plugins, modules, splits, the REPL plugin (milestones 30–35, ADR-33–38) and a full
benchmark run on an idle machine with low-hanging fixes (ADR-39, `docs/BENCHMARK.md`
2026-10-02, log 25–27).*

- **Queue agreed with the user (2026-10-02), in this order:**
  1. [x] Theming (ADR-33).
  2. [x] **Expose user-facing settings** (ADR-34; what stays a constant is listed
     there). The original list: in `[editor]` (from the survey of hard-coded
     values): tab width and spaces-vs-tabs (`Him.TextWidth.tabWidth`, `tab` in insert
     mode, the `tabSize` sent with `:format`); line numbers absolute/relative/off
     (`Render.Gutter`); cursor shape per mode (`Render.render`); auto-completion
     on/off and minimum word length (`Commands.Lsp`, now `>= 2`); signature help
     on/off; hover lines (30); file picker hidden files, symlinks, ignore files and file
     limit (`FileTree`, `Runtime.maxFiles`); preview on/off, minimum width (60) and size
     limit (20 MB); smart case and wrap-around for search; the escape timeout (30 ms);
     undo levels (1000); gutter glyphs; a `[language.<name>]` table (extensions,
     comment token, grammar); the LSP start timeout (60 s). Internal tuning constants
     (`maxBatch`, `chunkSize`, `mergeGap`, `syncLimit`, `matchLimit`, `maxEdits`,
     scan batching, `highlightMargin`) stay constants, possibly gathered in one module
     with their reasons.
  3. [x] **Plugins** (ADR-35, milestone 32). The request (2026-10-02): git and LSP (maybe syntax)
     become plugins that can be switched off in the config (`[plugins]`) and at run
     time (`:plugin-enable`, `:plugin-disable`, `:plugins`). A disabled plugin has no
     actions, keys, `:` commands, gutter lane, housekeeping or state. This is the core
     of the modularization below, so it comes first:
  4. [x] **Modularize for readability** (ADR-36). Done: the `replaceBuffer` helper,
     the `Actions.Lsp` split, `Him.Session` / `Him.App`, the renames, and the test
     split (`test/Test/*.hs`; `Spec.hs` only runs the groups). Left for later:
     - moving the buffer zipper out of `Editor`; splits reshape the editor state
       anyway, so it is done with them;
     - per-subsystem job runners; best done when jobs become plugin-generic.
     The original list:
     - split `Commands.Lsp` (796 lines) into attach/sync, navigation, edits,
       completion and `:lsp-*` commands;
     - split `App` into a frontend-free session (`handleEvent`, effects, housekeeping,
       counts) and the terminal loop (also the first step towards a GUI);
     - move the buffer zipper out of `Editor`, and `previewFor` next to the picker;
     - add one `replaceBuffer` helper for the four hand-made undoable replacements
       (`Command.editAll`, Git `replaceLines`, Lsp `applyToDocument`, File `reloaded`);
     - give each subsystem its own job runners and result handlers instead of one big
       `Runtime.runJob`;
     - rename `Him.EditorM` → `Him.EditorM` and `Him.Actions.*` → `Him.Actions.*`;
     - split `test/Spec.hs` into `test/Test/*`.
  6. [x] **A REPL plugin** (ADR-38, milestone 35). The request (2026-10-02): it opens a split with a
     REPL (a process: `ghci`, `python3`, … per language, configurable). You can type
     into it by hand, and a key sends the selection from the file to it. Set it up so
     it is useful for testing while developing, e.g. `stack ghci` in a Haskell project
     loads the project, `:reload` after saving, and the selection runs as an
     expression. It needs splits (5) and a buffer that holds a process's output and
     takes input.
  7. [x] **Benchmark again** (ADR-39, `docs/BENCHMARK.md` 2026-10-02). The request (2026-10-02) against Helix and Vim,
     also with plugins on and off, and apply low-hanging optimizations that keep the
     code readable. See "Benchmarking on hold" below for where it stopped.
  5. [x] **Splits** (ADR-37, milestone 34). The plan was: with Helix's keys: `C-w` / `space w` then `v`/`s` (split
     vertically/horizontally), `h j k l` / `C-h …` (focus), `w` (next), `q` (close), `o`
     (only), `H J K L` (swap); `:vsplit`/`:hsplit` (`:vs`, `:hs`) with an optional file.
     No Vim tabs: the buffer list stays as it is. Splits will need a view per window
     (`edView` becomes per window, sharing documents), the layout to become a tree of
     rects, and rendering per window (gutter, text area, a status line each).

- **Benchmarks (2026-10-02, idle machine; `docs/BENCHMARK.md`):**
  - `bench.py` has `him` (every plugin) and `him-lite` (none), plus `him-nogit` and
    `him-nolsp` on request. Each has its own config file. `--ext rs --git` measures
    an IDE-like setup.
  - him leads Vim and Helix on startup, scrolling, jumps, saving and search. Vim keeps
    the best per-key latency (0.5 vs about 1.1 ms). Helix keeps the fastest first
    paint of a large plain file (23 vs 30 ms).
  - Still open:
    - `open_large` first paint: the first render and line indexing; Helix 23 vs 30
      ms.
    - Per-key latency vs Vim, mostly the thread handoff and the diff.
    - Handling a server's large `publishDiagnostics` on the main loop.
  - Two old provisional numbers did not reproduce (`n` 1.4 ms, `open_large` 15 ms).
    The 2026-10-02 section is the reference.
- **The action layer is done (ADR-17).** The config-file parser is still to do. It only
  has to produce `Bindings` (`Map Mode [(keys, invocation)]`) and call
  `Him.Config.Default.configWith`, which reports every bad binding.
- **Done this session:** count prefixes (`5 j`), multiple selections (milestone 17,
  ADR-18), buffers, info menus and pickers (milestone 18, ADR-19/20), ignore files and the
  directory viewer (milestone 19, ADR-21/22), file operations in listings (milestone 20).
- **Roadmap (approved 2026-10-02): `docs/ROADMAP.md`.** It lists the phases in order:
  0 foundation (effects as data, a job runtime, document ids/versions, `Him.Process`,
  `Him.Json`), 1 the command palette (`space ?`) and an async file picker, 2 git signs
  and staging, 3 syntax highlighting (one common provider API; tree-sitter first,
  TextMate later behind the same interface), 4 the LSP client. Work through it in that
  order and tick phases off here.
  - [x] Phase 0, foundation (milestone 21, ADR-23).
  - [x] Phase 1, command palette and async picker (milestone 21, ADR-24).
  - [x] Phase 2, git signs and staging (milestone 22, ADR-25).
  - [x] Phase 3, syntax highlighting (milestone 23, ADR-26/27/28). Still open in it:
    3b incremental parsing (a change log in the buffer, `ts_tree_edit`), and 3c
    injections (code blocks in Markdown, `<script>` in HTML).
  - [x] Phase 4, LSP client (milestone 24, ADR-29). Diagnostics, hover, definition,
    references, completion.
  - **Roadmap complete.** Follow-ups, roughly by value: incremental parsing (3b),
    which can now reuse `Buffer.changeBetween`; highlighting in the preview; a theme
    section in the config file,
    languages and servers; syntax injections; regex search on `Him.Regex`; and a full
    benchmark run on an idle machine (see above).
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
  - Git: the signs refresh when a buffer becomes current, on save and after staging, but
    not when the files change outside the editor while it is shown.
  - Files changed on disk are not detected automatically; `:reload` / `:rla` read them
    again. Saving does not warn when the file changed on disk meanwhile. A diff of a huge file
    with an edit in the middle takes about 50 ms (in the background).
  - LSP, deferred by choice: inlay hints, semantic tokens, snippets (inserted as plain
    text), and code actions that create, rename or delete files (only their text edits
    are applied).
  - The preview is plain text, without highlighting.
  - Highlighting needs `him --build-grammars` once (see ADR-27). Without built grammars,
    files are shown plain, and nothing says why except `$HIM_LOG`.
  - Syntax sessions are not closed when their buffer is closed.
  - The file picker skips hidden entries and lists at most 500,000 files. It does not
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
- **How to verify:** `make test` (518 tests: pure modules, plus key sequences through the
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
