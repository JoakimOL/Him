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

Non-goals (for now): a GUI (the core is frontend-free, [ADR module-names](adr/module-names.md), so one could be added),
plugins loaded at run time (plugins are compiled in and switched on or off, [ADR git-and-lsp-as-plugins](adr/git-and-lsp-as-plugins.md)),
Windows support.

## 2. Assumptions

- Linux or another POSIX system, with a terminal that understands xterm/ANSI escape sequences
  (alternate screen, SGR colours, cursor-shape `DECSCUSR`).
- The terminal and the files are UTF-8. Invalid bytes are decoded leniently (replacement char).
- Files fit in memory (a rope of blocks; a 14 MB file costs about 15 MB, [ADR lazy-file-loading](adr/lazy-file-loading.md)).
- There is one user, one process, and no concurrent editing of the same file.
- Toolchain: **GHC 9.10.3** via Stack snapshot **LTS 24.60**, and
  **haskell-language-server 2.14** (it ships a 9.10.3 binary). These must be bumped together.
- Stack 2.15.7 prints "not tested with GHC 9.10" warnings. They are harmless;
  `stack upgrade` removes them.

## 3. Architecture decisions

Each decision is a file in `docs/adr/`, named by a short slug: the decision, why it
was made, and the alternatives considered. Code and docs cite one as `ADR <slug>`
(in Markdown, a link to the file). A new decision gets a new file and a line here;
there are no numbers, so branches that add decisions do not conflict.

- [selection-first-editing](adr/selection-first-editing.md): Helix-style selection-first editing
- [boot-libraries-only](adr/boot-libraries-only.md): GHC boot libraries only
- [seq-text-buffer](adr/seq-text-buffer.md): `Seq Text` line buffer behind an abstract interface *(superseded)*
- [pure-core](adr/pure-core.md): Pure core, thin IO shell
- [command-registry](adr/command-registry.md): Commands are named values in a registry. Keymaps are tries of command names
- [frame-diff-rendering](adr/frame-diff-rendering.md): Render to a pure `Frame`, then diff
- [terminal-size-shim](adr/terminal-size-shim.md): Terminal size through a C shim
- [input-from-fd](adr/input-from-fd.md): Read input from the file descriptor, never through the `stdin` Handle
- [selection-model](adr/selection-model.md): Details of the selection model
- [render-components](adr/render-components.md): Components are `Theme -> Editor -> Rect -> Frame -> Frame`
- [snapshot-undo](adr/snapshot-undo.md): Undo with snapshots, committed outside insert mode
- [registers-in-editor](adr/registers-in-editor.md): Registers live in the `Editor`, and pasting is linewise when the text ends with a newline
- [render-per-batch](adr/render-per-batch.md): Render once per batch of input
- [rope-buffer](adr/rope-buffer.md): The buffer is a rope of multi-line blocks
- [c-byte-loops](adr/c-byte-loops.md): Hot byte loops in C, called with `unsafe` FFI on the `Text`'s array
- [literal-search](adr/literal-search.md): Search is literal, smart case, and anchored on the rarest byte
- [row-reuse-and-scrolling](adr/row-reuse-and-scrolling.md): The renderer reuses rows and lets the terminal scroll
- [lazy-file-loading](adr/lazy-file-loading.md): Load files without copying, and index blocks lazily
- [actions](adr/actions.md): Keys bind to actions: named, grouped, with typed arguments (supersedes the `Command` registry of [ADR command-registry](adr/command-registry.md))
- [bottom-up-multi-range-edits](adr/bottom-up-multi-range-edits.md): Multi-range edits are applied from the bottom up, and positions are kept relative to the end
- [buffer-zipper](adr/buffer-zipper.md): Buffers are a zipper around the current document
- [menus-as-data](adr/menus-as-data.md): Menus are data computed after every key; popups invalidate the rows they cover
- [gitignore-matcher](adr/gitignore-matcher.md): Our own gitignore matcher, applied while walking
- [directory-documents](adr/directory-documents.md): A directory is a read-only document, plus a keymap layer
- [effects-and-runtime](adr/effects-and-runtime.md): Effects as data, and a runtime for background jobs
- [streaming-file-picker](adr/streaming-file-picker.md): The file picker streams, and large pickers filter in the background
- [git](adr/git.md): Git through the `git` program; the diff in-process
- [syntax-providers](adr/syntax-providers.md): One syntax-highlighting interface, with injected providers
- [tree-sitter](adr/tree-sitter.md): Tree-sitter: vendored runtime, grammars built for him
- [grammar-setup](adr/grammar-setup.md): Grammars set up by him itself: a pinned list, fetched and built, queries built in
- [regex-engine](adr/regex-engine.md): A small regex engine of our own
- [lsp-client](adr/lsp-client.md): The LSP client: processes in the runtime, protocol as pure data
- [picker-preview](adr/picker-preview.md): Pickers preview where an item points
- [suspend](adr/suspend.md): Ctrl-Z suspends the editor
- [toml-config](adr/toml-config.md): The config file is TOML, layered on the defaults
- [helix-themes](adr/helix-themes.md): Themes are Helix theme files
- [settings-table](adr/settings-table.md): Settings are one table
- [git-and-lsp-as-plugins](adr/git-and-lsp-as-plugins.md): Git and the LSP client are plugins
- [module-names](adr/module-names.md): Module names say what modules hold (2026-10-02)
- [window-splits](adr/window-splits.md): Splits are a tree of windows; the focused one is the editor's state
- [repl](adr/repl.md): A REPL is a buffer with a process behind it (the `repl` plugin)
- [per-key-work](adr/per-key-work.md): Per-key work follows what is visible or settled (2026-10-02)
- [match-mode](adr/match-mode.md): Match mode, as in Helix
- [ai-chat](adr/ai-chat.md): An AI chat, with edits approved in the editor
- [claude-code-provider](adr/claude-code-provider.md): Claude Code as a chat provider, with him's tools over MCP
- [change-review](adr/change-review.md): Proposed changes are reviewed like staged hunks, in any order
- [transcripts](adr/transcripts.md): REPL and chat buffers are transcripts, not text to edit
- [chat-panel](adr/chat-panel.md): The chat looks and works like an editor's chat panel (VS Code's)
- [global-search](adr/global-search.md): A global search picker (`space /`) that searches as you type
- [jumplist](adr/jumplist.md): A jumplist per window, as in Helix, with entries you add and remove
- [picker-actions](adr/picker-actions.md): Every picker has a primary and a secondary action, and items can be marked
- [registers-and-clipboard](adr/registers-and-clipboard.md): Registers as in Vim and Helix, and the clipboard behind one provider API
- [plugin-building-blocks](adr/plugin-building-blocks.md): What plugins build on: events, processes, segments, signs, annotations
- [plugin-api](adr/plugin-api.md): `Him.Plugin`, the public plugin API, and the contrib collection
- [personal-builds](adr/personal-builds.md): Personal builds, as in xmonad: `himMain`, `him --rebuild`, and a template repository
- [no-test-framework](adr/no-test-framework.md): No test framework

## 4. Module map

Every module starts with a header comment saying what it is for; this is the overview.
Pure modules are marked *(pure)*.

**Core text and editing**

| Module | Responsibility |
|---|---|
| `Him.Buffer`, `Him.Buffer.Rope` | Text storage: a weight-balanced tree of blocks with lazy line starts ([ADR rope-buffer](adr/rope-buffer.md), [ADR c-byte-loops](adr/c-byte-loops.md), [ADR literal-search](adr/literal-search.md), [ADR row-reuse-and-scrolling](adr/row-reuse-and-scrolling.md), [ADR lazy-file-loading](adr/lazy-file-loading.md)); insert, delete, ranges, search hooks *(pure)*. |
| `Him.Clipboard` | The system clipboard behind `+` / `*`: one `ClipboardProvider` API, backends for wl-clipboard, xclip, xsel, pbcopy, tmux, OSC 52 ([ADR registers-and-clipboard](adr/registers-and-clipboard.md)). |
| `Him.Native` + `cbits/text.c` | `unsafe` FFI on text arrays: newline scans (SSE2), forward/backward search. |
| `Him.Position`, `Him.Selection` | Positions; Helix selections: ranges with anchor and head, a primary *(pure)*. |
| `Him.Motion`, `Him.Edit` | Motions and edits applied to every range ([ADR bottom-up-multi-range-edits](adr/bottom-up-multi-range-edits.md)) *(pure)*. |
| `Him.TextObject` | Match mode's objects (`m i w`, `m a (`), the pair around a position, the matching bracket ([ADR match-mode](adr/match-mode.md)) *(pure)*. |
| `Him.Search`, `Him.Regex` | Literal search with smart case and wrap-around; the regex subset used by highlight queries ([ADR regex-engine](adr/regex-engine.md)) *(pure)*. |
| `Him.Grep` | The global search: the lines of a text (or a file) that contain a needle ([ADR global-search](adr/global-search.md)). |
| `Him.Jumplist` | Helix's jumplist: push, back, forward, remove, and moving positions through a change ([ADR jumplist](adr/jumplist.md)) *(pure)*. |
| `Him.History` | Undo/redo snapshots *(pure)*. |
| `Him.TextWidth`, `Him.View` | Display columns (tabs, wide and control characters); scrolling with scrolloff *(pure)*. |
| `Him.Document` | A buffer with its selection, path, history, kind (text, directory listing, REPL, chat), git/LSP/syntax state; `changeDocument`, `replaceBuffer`, `unsaved` *(pure)*. |
| `Him.Transcript` | REPL and chat buffers: output before the input, `ret` takes the input ([ADR repl](adr/repl.md), [ADR ai-chat](adr/ai-chat.md)) *(pure)*. |
| `Him.File`, `Him.Directory`, `Him.FileTree`, `Him.Ignore` | Loading and saving files (zero-copy for valid UTF-8); directory listings; the ignore-aware parallel file walk; gitignore rules. |

**Editor state, actions and keys**

| Module | Responsibility |
|---|---|
| `Him.Editor` | The whole editor state: the focused document and view, the buffer zipper, windows ([ADR window-splits](adr/window-splits.md)), popups, plugin state; `windowEditor`, `pendingEditLines` *(pure)*. |
| `Him.Window` | The layout tree of windows, boxes, neighbours ([ADR window-splits](adr/window-splits.md)) *(pure)*. |
| `Him.Chat.Transcript` | The chat buffer's layout: blocks above the prompt, marks per line, wrapping, code blocks, the input ([ADR chat-panel](adr/chat-panel.md)) *(pure)*. |
| `Him.Mode`, `Him.Key`, `Him.Keymap` | Modes and keymap layers (directory, completion, REPL, chat); keys; keymap tries. |
| `Him.EditorM` | The monad actions run in and its helpers (`edit`, `motion`, `request`, `info`). |
| `Him.Action`, `Him.Invocation` | Named actions with typed parameters; invocations as text ([ADR actions](adr/actions.md)). |
| `Him.Actions.*` | The actions: `Motion`, `Edit`, `Search`, `Match`, `File` (`:` commands for files, buffers, quitting), `CommandLine`, `Picker`, `Directory`, `Window`, `Syntax`, `Jump` (the jumplist and `jumping`); the plugins `Git`, `Lsp` (+ `Lsp.Core`, `.Navigation`, `.Edits`, `.Completion`), `Repl`, `Chat`. |
| `Him.Plugin` (+ `.Types`, `.Host`, `.Internal`), `Him.PluginState`, `Him.Contrib` (+ `.WordCount`, `.RecentFiles`) | The public plugin API, plugins' state, the contrib collection ([ADR plugin-api](adr/plugin-api.md)). |
| `Him.PluginUI`, `Him.PluginEvent`, `Him.Spawn` | What plugins show (segments, signs, annotations); events found by comparing with what was seen; plugin processes ([ADR plugin-building-blocks](adr/plugin-building-blocks.md)) *(the first two pure)*. |
| `Him.Main`, `Him.Rebuild` | The program as `himMain [Plugin]`; `him --rebuild` and starting a personal build ([ADR personal-builds](adr/personal-builds.md)). |
| `Him.Ex`, `Him.Info`, `Him.Palette`, `Him.Picker` | `:` commands; the info box after a prefix; the command palette; pickers and fuzzy ranking. |
| `Him.Config`, `Him.Config.Default` | `Config` and the `Plugin` record ([ADR git-and-lsp-as-plugins](adr/git-and-lsp-as-plugins.md)); the default bindings, actions and plugins; `configWith`. |
| `Him.Options`, `Him.UserConfig`, `Him.Toml`, `Him.Paths` | Settings ([ADR settings-table](adr/settings-table.md)); the config file ([ADR toml-config](adr/toml-config.md)); the TOML reader; where files live. |

**Running it**

| Module | Responsibility |
|---|---|
| `Him.App` | The terminal frontend: raw mode, input, the loop (batching, rendering, loop-only effects), themes. |
| `Him.Session` | Events to state changes without a frontend: keys, job results, effects, housekeeping, plugins ([ADR module-names](adr/module-names.md)). |
| `Him.Effect`, `Him.Event`, `Him.Runtime` | Effects and jobs as data; events; the runtime that runs jobs, language servers, REPLs and chat requests ([ADR effects-and-runtime](adr/effects-and-runtime.md)). |
| `Him.Process`, `Him.Json`, `Him.Log` | Running programs; JSON; debug logging. |
| `Him.Terminal.*` | Raw mode, input decoding, output, size, escape sequences (incl. OSC 10/11 colours). |
| `Him.Render`, `Him.Render.*` | Layout per window and the components: `Gutter`, `TextArea`, `StatusLine`, `CommandLine`, `Info`, `Picker`, `Completion`; `Frame` and `Diff` ([ADR row-reuse-and-scrolling](adr/row-reuse-and-scrolling.md)). |
| `Him.Theme`, `Him.Theme.Load`, `Him.Render.Theme` | Helix theme files *(pure)*; finding and loading them; the render-side theme ([ADR helix-themes](adr/helix-themes.md)). |

**Subsystems**

| Module | Responsibility |
|---|---|
| `Him.Syntax`, `Him.Syntax.Span`, `Him.Syntax.TreeSitter`, `Him.Language`, `Him.GrammarList`, `Him.GrammarBuild`, `Him.Embedded` (+ `.TH`) + `cbits/ts_*.c`, `cbits/tree-sitter`, `runtime/` | Highlighting: the provider interface ([ADR syntax-providers](adr/syntax-providers.md)), tree-sitter ([ADR tree-sitter](adr/tree-sitter.md)), languages; the grammar list, `him --grammar` (fetch, build) and the built-in queries ([ADR grammar-setup](adr/grammar-setup.md)). |
| `Him.Diff`, `Him.GitState`, `Him.Git` | Line diffs, a document's git state and signs *(pure)*; running git ([ADR git](adr/git.md)). |
| `Him.Lsp.*` | The LSP client: protocol, state, sync, edits *(pure)*, server processes, the server table ([ADR lsp-client](adr/lsp-client.md)). |
| `Him.Repl`, `Him.Repl.Process` | REPL config and state *(pure)*; the process ([ADR repl](adr/repl.md)). |
| `Him.Review` | Reviewing proposed changes: approve / deny one change, the change at a line, the rows a text area shows (header, removed lines) ([ADR change-review](adr/change-review.md)) *(pure)*. |
| `Him.Chat`, `Him.Chat.Tools`, `Him.Chat.Anthropic`, `Him.Chat.ClaudeCode`, `Him.Mcp` | The chat provider interface (sessions) and state, the model's tools and pending edits *(pure)*; the Claude API provider over curl ([ADR ai-chat](adr/ai-chat.md)); the Claude Code provider and the MCP bridge (`him --mcp-bridge`) that serves him's tools to it ([ADR claude-code-provider](adr/claude-code-provider.md)). |

## 5. Development goals / milestones

Each milestone ends with something runnable, and with this file updated.

- [x] **Project setup & DX.** Stack/LTS 24.60 pinned; `hie.yaml`, `fourmolu.yaml`,
  `.hlint.yaml`, `.editorconfig`, `Makefile`; FFI shim compiles; test harness in place;
  `Him.Log`. *Done when:* `make build` and `make test` pass, and HLS loads all components.
  **Note:** build and test pass, but HLS does not load yet. See the known issue in §8.
- [x] **Raw mode.** `Terminal.Raw` + alternate screen. A temporary loop echoes byte
  values and `q` quits. *Done when:* the terminal is restored after a normal quit and after
  an exception.
- [x] **Output & drawing.** `Terminal.Ansi/Output`; draw `~` rows and a welcome message;
  redraw on SIGWINCH. *Done when:* resizing redraws correctly.
- [x] **Input decoding.** `Key`, `Event`, `Terminal.Input`. *Done when:* arrows, Ctrl-,
  Alt-, and a lone Esc are distinguished and shown on screen; `decodeKeys` is unit tested.
- [x] **Buffer, file loading, rendering.** `Buffer`, `File`, `Editor`, `View`, `Render`
  (TextArea + StatusLine), `Render.Diff`. *Done when:* `him file` shows the file and it
  scrolls.
- [x] **Selections & motions.** `h j k l`, clamping, desired column, viewport follows the
  cursor, selection highlighted. *Done when:* motions are unit tested.
- [x] **Commands, keymap, modes.** Registry, trie, Normal/Insert, `i a o`, typing,
  Backspace, Enter, cursor shape, dirty flag.
- [x] **Command mode.** `:w`, `:q` (refuses when dirty), `:q!`, `:wq`; status messages.
- [x] **Helix selection actions.** `w b e x v ; d c`, plus `g g` / `g e`, with the
  pending keys shown in the status line.
- [x] **Polish.** Line-number gutter, tab expansion, wide-character width, horizontal
  scrolling.

- [x] **Undo/redo.** `Him.History` holds snapshots. An insert session (including `c`
  and `o`) is one undo step, and the dirty flag is recomputed after undo/redo.
- [x] **Yank/paste.** `y`, `p`, `P` with a default register; `d` and `c` also yank.
  Text ending in a newline (from `x`) pastes as whole lines.

- [x] **Memory.** A rope of blocks, streaming load/save, and the non-moving GC.
  Peak memory with the 14 MB file: 40 → 24 MB open, 87 → 35 MB after editing and saving.
- [x] **Search.** `/`, `?`, `n`, `N`, `*`; smart case; incremental preview;
  rare-byte SIMD scanning; benchmark scenarios with result checks.
- [x] **Rendering pass.** Row reuse, terminal scroll regions, cell-level diff, and an
  ASCII fast path.
- [x] **Action layer.** Keys bind to actions with typed arguments, validated at
  startup; user bindings override the defaults ([ADR actions](adr/actions.md)). Count prefixes fill an
  action's `count` parameter.
- [x] **Multiple selections.** `C`, `s` (with preview), `%`, `,`, `A-,`, `(`, `)`,
  `A-s`. Edits, yank and paste work on every range ([ADR bottom-up-multi-range-edits](adr/bottom-up-multi-range-edits.md)).
- [x] **Buffers and menus.** `:open`/`:e` (with `tab` path completion), `:new`, `:bc`,
  `:bn`/`:bp`, `:wa`, `:wqa`, `g n`/`g p`, and `him FILE...` ([ADR buffer-zipper](adr/buffer-zipper.md)). An info box shows
  the keys after `g`/`space` and the matching `:` commands. `space f` (file picker) and
  `space b` (buffer picker) ([ADR menus-as-data](adr/menus-as-data.md)).
- [x] **Ignore files and a directory viewer.** The file picker honours `.gitignore`
  and `.ignore` ([ADR gitignore-matcher](adr/gitignore-matcher.md)). Directories open as dired-style listings: `ret`, `-`, `g r`,
  `space d` / `space D`, `:cd`, `:pwd` ([ADR directory-documents](adr/directory-documents.md)).
- [x] **File operations in listings.** `a`, `+`, `r`, `d` (with confirmation),
  and `g .` for dotfiles ([ADR directory-documents](adr/directory-documents.md)).
- [x] **Foundation, palette, async picker** (roadmap phases 0–1). Effects and
  background jobs ([ADR effects-and-runtime](adr/effects-and-runtime.md)), `:action`, `Him.Json`, `Him.Process`; the command palette
  `space ?`; the streaming, parallel file picker with background filtering ([ADR streaming-file-picker](adr/streaming-file-picker.md)).
- [x] **Git** (roadmap phase 2). Gutter signs for added/changed/removed lines,
  staged ones dimmer; `] g` / `[ g`; stage, unstage or reset the selected lines or the
  file from the editor ([ADR git](adr/git.md)).
- [x] **Syntax highlighting** (roadmap phase 3). One provider interface ([ADR syntax-providers](adr/syntax-providers.md));
  the tree-sitter provider with a vendored runtime and grammars built by
  `him --build-grammars` ([ADR tree-sitter](adr/tree-sitter.md)); `Him.Regex` ([ADR regex-engine](adr/regex-engine.md)).
- [x] **LSP client** (roadmap phase 4). Diagnostics, hover, definition, references
  and completion, with servers run by the runtime ([ADR lsp-client](adr/lsp-client.md)).
- [x] **More LSP.** Incremental sync (`Buffer.changeBetween`), `didSave` and
  `didClose`; rename, format, code actions, signature help, document symbols, type
  definition and implementation; `:lsp-start`, `:lsp-stop`, `:lsp-restart` and
  `:lsp-info` ([ADR lsp-client](adr/lsp-client.md)).
- [x] **Previews, workspace symbols, imports.** Picker previews ([ADR picker-preview](adr/picker-preview.md)), `space S`,
  completion imports, references on `g r` ([ADR lsp-client](adr/lsp-client.md)).
- [x] **Movement.** Page and half-page motions, `f t F T` with `A-.`, `<count> g g`,
  and Ctrl-Z to suspend ([ADR suspend](adr/suspend.md)).
- [x] **Reload.** `:reload`, `:reload!`, `:reload-all`: the file is read again as one
  undoable change, keeping the cursor; modified buffers are refused unless forced.
- [x] **Config file.** `config.toml` with keys, editor settings and language servers;
  `him --dump-default-config`, `:config-open`, `:config-reload` ([ADR toml-config](adr/toml-config.md)).
- [x] **Themes.** Helix theme files (`[editor] theme`, `:theme`), styles layered
  as in Helix, underline kinds and colours, the theme's background through OSC 11, and
  a 256-colour fallback ([ADR helix-themes](adr/helix-themes.md)).
- [x] **Settings.** Tab width, expand-tab, relative line numbers, cursor shapes,
  completion, search, file-picker and preview settings, and the escape timeout, all
  from one table ([ADR settings-table](adr/settings-table.md)).
- [x] **Plugins.** Git and LSP as plugins: `[plugins]`, `:plugins`,
  `:plugin-enable`, `:plugin-disable` ([ADR git-and-lsp-as-plugins](adr/git-and-lsp-as-plugins.md)).
- [x] **Modules.** `Him.Session`, `Him.Actions.*`, `Him.EditorM`, the LSP split,
  and the test modules ([ADR module-names](adr/module-names.md)).
- [x] **Splits.** Windows side by side and stacked, Helix's `C-w` / `space w`
  keys, `:vsplit`, `:hsplit` ([ADR window-splits](adr/window-splits.md)).
- [x] **REPL plugin.** `:repl`, `space e` (send the selection), `space E`
  (reload), reload on save, typing in the REPL buffer ([ADR repl](adr/repl.md)).
- [x] **Match mode.** `m m`, `m s`, `m r`, `m d`, `m i`, `m a`; `I` and `A` ([ADR match-mode](adr/match-mode.md)).
- [x] **AI chat plugin.** A chat beside the code; the model's edits wait for
  approval in the editor ([ADR ai-chat](adr/ai-chat.md)).
- [x] **Claude Code provider.** The chat through `claude`, with him's tools served
  over MCP by `him --mcp-bridge` ([ADR claude-code-provider](adr/claude-code-provider.md)).
- [x] **A chat panel like VS Code's.** Blocks for messages and answers, an input
  box with a placeholder, wrapping, tool and change lines, a review summary, input
  history, copying code blocks ([ADR chat-panel](adr/chat-panel.md)).
- [x] **Transcripts.** REPL and chat buffers: only the input after the prompt can
  change; select and yank anywhere; insert mode goes to the input ([ADR transcripts](adr/transcripts.md)).
- [x] **Reviewing proposed changes.** All of a turn's changes at once, decided in
  any order with the cursor on one, shown inline with their removed lines ([ADR change-review](adr/change-review.md)).
- [x] **Global search.** `space /` searches the project's files as you type,
  streaming hits into a picker with a preview ([ADR global-search](adr/global-search.md)).
- [x] **Jumplist.** `C-o` / `C-i` / `tab`, `C-s`, `space j` with `del` to remove an
  entry; jumps follow edits ([ADR jumplist](adr/jumplist.md)).
- [x] **Picker actions.** Every picker has a primary (`ret`) and a secondary
  (`del`) action, and `tab` marks items for both to act on ([ADR picker-actions](adr/picker-actions.md)).
- [x] **Registers and the clipboard.** `"` + a register for the next command,
  `+` / `*` as the system clipboard / primary selection (`space y`, `space p`), `R` /
  `space R` (replace with a register / the clipboard), `_`,
  `C-r` in insert mode, `:registers`, `:clear-register` ([ADR registers-and-clipboard](adr/registers-and-clipboard.md)).
- [x] **Plugin building blocks.** Events, plugin processes, status line segments,
  gutter signs and annotations; git's signs and a branch segment on them ([ADR plugin-building-blocks](adr/plugin-building-blocks.md)).
- [x] **`Him.Plugin` and contrib.** The public plugin API, settings under
  `[plugins.<name>]`, the `:plugins` picker; contrib `wordcount` and `recent-files`
  ([ADR plugin-api](adr/plugin-api.md)).
- [x] **Personal builds.** `himMain`, `him --rebuild` from `plugins.toml`, the
  released him starting a personal build, the template repository ([ADR personal-builds](adr/personal-builds.md)).
- [x] **Highlighting without Helix.** `him --grammar` fetches the grammars at pinned
  revisions and builds them; the highlight queries are built into him ([ADR grammar-setup](adr/grammar-setup.md)).

Later (not started; the architecture has room for them):
- [ ] Highlight all matches of a search; regex search on `Him.Regex`; `S` (split the
  selection on a pattern).
- [ ] Undo tree / change sets instead of snapshots.
- [ ] Incremental parsing and injections for tree-sitter ([ADR tree-sitter](adr/tree-sitter.md)); highlighting in the
  picker preview.
- [ ] Detecting files changed on disk.

## 6. Keybindings

Defined in `Him.Config.Default` (core) and in each plugin (`plBindings`).
`him --dump-default-config` prints all of them with what they do, and `space ?` lists
them in the editor.

| Mode | Keys |
|---|---|
| Normal (moving) | counts (`5 j`); `h j k l`, arrows, `home`/`end`; `w b e` (words); `g g` / `g e` (first / last line), `<count> g g` (that line), `g h` / `g l` (line start / end); `C-f` / `C-b` (also `pagedown` / `pageup`), `C-d` / `C-u` (half pages); `f t F T` + a character, `A-.` repeats; `m m` (the matching bracket); `C-z` suspends. |
| Normal (selecting) | `x` (the selection's whole lines; repeat to extend), `X` (the whole lines, never adding one), `;` (collapse), `v` (select mode), `%` (all), `s` (matches inside the selection), `C` (copy the selection to the next line), `,` / `A-,` (keep / remove the primary), `(` / `)` (rotate), `A-s` (split into lines); `m i` / `m a` + `w W p m ( [ { < " ' \`` (inside / around a text object). |
| Normal (editing) | `i a I A` (insert before / after / at the line's start / end), `o` / `O` (on a new line below / above), `d` (delete), `c` (change), `y` (yank), `p` / `P` (paste after / before), `"` + a register before any of them (`+` clipboard, `*` primary, `_` discard), `r` + a character (replace each selected character), `R` (replace with the register), `space y` / `space p` / `space P` / `space R` (the clipboard), `u` / `U` (undo / redo); `m s` + a character (surround), `m r` + two (replace the pair), `m d` + one (delete the pair; `m` the closest). |
| Normal (search) | `/` / `?` (with preview), `n` / `N`, `*` (selection as the pattern). |
| Select | Normal mode where motions extend; `v` / `esc` back; `y` yanks and goes back. |
| Insert | typing, `ret` (keeps indentation), `tab` (spaces with `expand-tab`), `backspace`, `del`, arrows, `C-r` + a register (insert it), `esc`. |
| Command line | typing, `tab` (complete names, paths, themes, plugins; again to cycle the candidates), `S-tab` (cycle back), `backspace`, `ret`, `esc`. |
| Buffers, pickers | `g n` / `g p` (next / previous buffer), `space f` (files), `space b` (buffers), `space /` (search the files), `space j` (the jumplist), `space ?` (every action); in a picker: type to filter, `up`/`down`/`C-n`/`C-p`/`S-tab`, `tab` (mark, for `ret`/`del` to act on all marked), `ret`, `del` (the picker's second action: the jumplist removes the entry), `esc`. |
| Jumplist | `C-o` (back), `C-i` / `tab` (forward), both with a count; `C-s` (save the selection). |
| Directory listings | `space d` (the file's directory), `space D` (the working directory), `:o dir`; in a listing: `ret`, `-` / `^` / `backspace` (parent), `g r` (refresh), `a` (new file or `dir/`), `+` (new directory), `r` (rename), `d` (delete, asks), `g .` (dotfiles). |
| Windows | `C-w` or `space w`, then `v` / `s` (split side by side / stacked), `w` (next), `h j k l` (focus), `H J K L` (swap), `q` (close), `o` (only), `n v` / `n s` (split with a scratch buffer). |
| git plugin | `] g` / `[ g` (next / previous change); `space g s` / `u` (stage / unstage the selected lines), `S` / `U` (the file), `r` (reset the lines). |
| lsp plugin | `space k` (hover), `g d` / `g y` / `g i` / `g r` (definition, type definition, implementation, references), `space s` / `space S` (symbols / in the project), `space r` (rename), `space a` (code actions), `space x` / `] d` / `[ d` (diagnostics); insert mode: completion (`C-x`, `tab` / `C-n` / `C-p`, `ret`), signature help. |
| repl plugin | `space e` (send the selection or line), `space E` (reload); in the REPL buffer (insert): `ret` sends, `C-c` interrupts. |
| chat plugin | `space c c` (open the chat), `space c s` (put the selection into the message), `space c y` (copy a code block), `space c n` (new chat); proposed changes: `space c a` / `space c d` (keep / discard the one under the cursor), `space c A` / `space c D` (all), `] c` / `[ c` (next / previous), `space c l` (list); in the chat (insert): `ret` sends, `A-ret` a line break, `up` / `down` earlier messages, `C-c` stops the answer, `C-l` a new chat. |

`:` commands (`tab` completes, and the `:` menu lists them as you type):
- **files and buffers:** `:w [path]`, `:wa`, `:wq` / `:x`, `:wqa`, `:q` (closes the window; quits with the last), `:q!`, `:qa`, `:qa!`, `:o` / `:e path…`, `:reload` (`!`), `:reload-all`, `:new`, `:bc` (`!`), `:cd`, `:pwd`;
- **windows:** `:vsplit` / `:vs [files]`, `:hsplit` / `:hs [files]`, `:vnew`, `:hnew`;
- **config:** `:theme [name]`, `:config-open`, `:config-reload`, `:plugins`, `:plugin-enable` / `:plugin-disable <name>`, `:action <invocation>`;
- **plugins:** `:format`, `:lsp-info`, `:lsp-start`, `:lsp-stop`, `:lsp-restart`; `:repl [language]`, `:repl-send <text>`, `:repl-reload`, `:repl-interrupt`, `:repl-stop`, `:repl-restart`; `:chat`, `:chat-new`, `:chat-keep [all]` (`:chat-approve`), `:chat-discard [all]` (`:chat-deny`).

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
  the dumped default follow ([ADR settings-table](adr/settings-table.md)).
- **A plugin:** a `Plugin` record (`Him.Config`): actions, bindings, commands, hooks
  (housekeeping, per batch, job results, enable/disable); add it to `plugins` in
  `Him.Config.Default` ([ADR git-and-lsp-as-plugins](adr/git-and-lsp-as-plugins.md)). It can be switched off like the others.
- **A provider:** a highlighter is a `SyntaxProvider` ([ADR syntax-providers](adr/syntax-providers.md)), a chat backend a
  `ChatProvider` ([ADR ai-chat](adr/ai-chat.md)); register it in `Him.Config.Default`. A clipboard backend is
  a `ClipboardProvider` in `Him.Clipboard.systemProviders` ([ADR registers-and-clipboard](adr/registers-and-clipboard.md)).
- **A render component:** `Theme -> Editor -> Rect -> Frame -> Frame` in
  `Him.Render.<Name>`, composed in `Him.Render` (per window or over everything).
- **Debugging:** `HIM_LOG=/tmp/him.log make run ARGS=file`, `tail -f /tmp/him.log` in
  another terminal. Never print to stdout while the terminal is in raw mode.

## 8. Where to pick up

*Last updated 2026-10-06.* Everything the user asked for so far is done; the latest
work is setting up highlighting without Helix (`him --grammar`, [ADR grammar-setup](adr/grammar-setup.md)),
`r` and `O`, and before that, on the `plugin-api` branch, the plugin API: building blocks ([ADR plugin-building-blocks](adr/plugin-building-blocks.md)),
`Him.Plugin` and contrib ([ADR plugin-api](adr/plugin-api.md)), personal builds ([ADR personal-builds](adr/personal-builds.md)). Before that came
registers and the system clipboard ([ADR registers-and-clipboard](adr/registers-and-clipboard.md)), cycling the `:` line's completions with
`tab` / `S-tab`, previewing themes as `:theme <name>` is typed, picker actions and
marks ([ADR picker-actions](adr/picker-actions.md)) and the jumplist ([ADR jumplist](adr/jumplist.md)).

- **State:** every milestone in §5 is done; the decisions are in §3 (`docs/adr/`).
  `make test` covers pure
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
  The first live run worked as far as Claude Code is concerned (it used only him's
  tools); the faults it found were him's and are fixed ([ADR claude-code-provider](adr/claude-code-provider.md)). `dev/fake-claude`
  checks the flow without a model.
- **Ideas, roughly by value:**
  0. **A public plugin API** (in progress on `plugin-api`): the design and phases are
     in `docs/PLUGIN-API.md`. Plugins are compiled in, as in xmonad.
     Development focuses on releases that include a contrib collection, off by
     default. A template repository with CI builds and `him --rebuild` come later.
     Plugin processes and `.so` loading are ruled out. Phase 0, picker actions,
     is [ADR picker-actions](adr/picker-actions.md); the open questions at the end of that file are for the user.
     Earlier note: every picker gets a
     primary and a secondary action on two keys (the user suggested `ret` and
     `tab`). For example, the file picker's `tab` marks several files and `ret`
     opens them; the jumplist's `ret` jumps and its secondary deletes. Today
     `picker_secondary` (on `del`) is that hook, with only the jumplist using it,
     and `tab` moves the selection. Moving `tab` would need another key for
     "next" (`down` / `C-n` stay). The longer aim: make the picker a component of a
     public plugin API, so a user's plugin can open its own picker with its own
     actions. That means `PickTarget` (a closed sum read in `picker_accept`) has to
     give way to items whose actions come from the picker, e.g. named actions
     ([ADR actions](adr/actions.md) invocations) that receive the chosen items.
  1. Regex search and `S` (split on a pattern), on `Him.Regex`.
  2. Incremental tree-sitter parsing (the buffer's `changeBetween` is ready) and
     injections.
  3. Detecting files changed on disk.
  4. Chat: show the model's reasoning summaries (`display: "summarized"`), a picker of
     pending edits, more tools (search the project).
  5. Moving the buffer zipper out of `Editor`, and per-subsystem job runners ([ADR module-names](adr/module-names.md)
     left them for later).
  6. Global search ([ADR global-search](adr/global-search.md)): regex patterns (with item 1), searching open buffers'
     unsaved text, highlighting the match in the list and preview, and speed (scan
     the bytes before decoding them; more capabilities).
- **Known issues:**
  - Zero-width combining characters are treated as width 1; case-insensitive search
    folds ASCII letters only.
  - `s` scans from each range's start; with many ranges and few matches it is slow.
    `/` and `n` move only the primary range.
  - Files changed on disk are not noticed (`:reload` / `:rla`), and saving does not warn
    about them. Git signs follow saves, staging and switching buffers only.
  - LSP, deferred by choice: inlay hints, semantic tokens, snippets (inserted as plain
    text), and file operations in code actions.
  - Highlighting needs `him --grammar` once ([ADR grammar-setup](adr/grammar-setup.md)); without grammars, files
    are plain and only `$HIM_LOG` says why. Syntax sessions are not closed with their
    buffer. The preview highlights a file's first 20000 lines (a search hit further down
    shows plain).
  - A directory listing does not refresh by itself (`g r`). Deleting a file leaves its
    buffer open.
  - The info box and picker measure text by characters, so wide characters can
    misalign their right border.
  - An unfocused window's selection is not moved by edits made in another window on
    the same document; it is clamped ([ADR window-splits](adr/window-splits.md)).
  - A proposed change that overlaps an unsaved edit of your own (made before the
    chat's first change to that file) cannot be approved until you save or undo
    your edit ([ADR change-review](adr/change-review.md)). `:w` on a buffer under review writes what it shows,
    proposals included.
  - The clipboard providers were tried with xclip only (reading); wl-clipboard, xsel,
    pbcopy, tmux and OSC 52 are untested on a real system. Counts don't repeat a paste.
  - The MCP bridge's pipes are opened read-write by the editor, which Linux allows but
    POSIX leaves undefined ([ADR claude-code-provider](adr/claude-code-provider.md)).
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
