# him roadmap: language awareness, git, and the remaining gaps

This is the approved plan (2026-10-02) for the next phases. Progress is tracked in
`docs/PLAN.md` §8; each phase ends with its ADRs and a §8 update.

## Context

him is now a usable modal editor: rope buffer, multiple selections, buffers, pickers,
info menus, directory listings, and a validated action layer. Three things still stop it
from being a daily driver: no language awareness (highlighting, LSP) and no git
integration. Two smaller gaps: no list of all commands (Helix's `space ?`), and the file
picker blocks and gets slow on big projects. This plan adds all five, in this order:
minor features, then git, then syntax, then LSP. They share a small foundation that is
built first.

Decisions taken with the user:
- **Tree-sitter:** the runtime is vendored from the local copy at
  `~/.cargo/registry/src/index.crates.io-1949cf8c6b5b557f/tree-house-bindings-0.3.2/vendor`
  (MIT, ABI 13–15, ~466 KB of `.c`). Grammars are Helix's compiled ones, loaded with
  dlopen from `~/.config/helix/runtime/grammars/*.so` (301 of them), with
  `queries/<lang>/highlights.scm`. This amends ADR-2. Nothing is downloaded.
  - **The Hackage `tree-sitter` package was considered and rejected.** It is version
    0.9.0.3, from the haskell-tree-sitter project, with packages like
    `tree-sitter-rust`.
    - It is not a boot library, so it would be a download, and probably needs a
      snapshot override.
    - It is tested only up to GHC 9.2 (we use 9.10.3).
    - It bundles an older runtime, which may not load Helix's grammars.
    - Its language packages bundle their own grammars (about 14 languages, against
      Helix's 301 with queries).
    - It is aimed at producing ASTs for `semantic`, not at highlight queries.
    - Our own bindings cover the dozen calls we need (parser, tree, query cursor), plus
      one C shim.
- **TextMate:** the provider API is designed so that a TextMate provider can plug in.
  Only the API is built; there is no TextMate implementation in this plan.
- **Order:** foundation, minor features, git, syntax, LSP.

Ground rules that still hold:
- Nothing is downloaded.
- Only boot libraries are added: `process` (git, LSP), and `unix` dlopen, which is
  already a dependency.
- Each strategy and decision is documented: an ADR in `docs/PLAN.md`, an entry in the
  `docs/BENCHMARK.md` log for each performance strategy, and §8 refreshed when a phase
  stops.
- Every verified step is committed with the trailer.
- Tests are written with the existing harness (`test/Spec.hs`, `Test.Harness`).

What's already on the machine:
- git 2.55.
- Language servers: `haskell-language-server-wrapper`, `clangd`, `rust-analyzer`,
  `typescript-language-server`.
- `~/Documents/helix` (69k files; 2.3k without `target/` and grammar sources), a good
  benchmark for the picker.

---

## Phase 0: Foundation (shared by everything below)

1. **Effects as data.** Actions stay pure state changes.
   - `Him.Editor` gets `edEffects :: [Effect]`. Actions call `request :: Effect ->
     EditorM ()` (new, in `Him.EditorM`).
   - `Effect` (new module `Him.Effect`) is plain data:
     - `RunAction Invocation`: run synchronously in `handleEvent`, which has the config
       (`bindText (cfgActions config)` from `Him.Action`);
     - `StartJob Job` and `CancelJob JobKey`: run by the runtime;
     - later, LSP sends.
   - **Why:** actions cannot reach the config or IO handles today (EditorM =
     `StateT Editor IO`, and `Editor` derives Eq/Show). Effects as data keep that, and
     let tests assert which effects an action requested.
2. **Runtime and job events.**
   - `Him.Runtime` is owned by `Him.App.eventLoop`. It holds the event `TChan`, job
     threads, process handles, and later the tree-sitter sessions and LSP clients.
   - `Him.Event` gets `EvJob JobResult`. `JobResult` is pure data, so `Event` keeps
     Eq/Show.
   - The loop drains effects after each `handleEvent` (`App.hs` `step`/`batch`), and
     job results come back through the same channel. The existing batching (ADR-11)
     already renders once per burst.
3. **Document identity and versions.**
   - `docId :: Int` is assigned when a document is opened (`edNextId` in `Editor`).
   - `docVersion :: Int` is bumped in `editAll` (`Him.EditorM`) and in undo/redo
     (`history` in `Him.Actions.Edit`).
   - Every job result carries `(docId, version)`; a stale result is dropped. This is
     needed because buffers switch (the zipper, ADR-19) while jobs run.
4. **`Him.Process`.** Run a command with stdin text and capture stdout, stderr and the
   exit code (`process`, `bytestring`). Used by git; LSP spawns its own pipes.
5. **`Him.Json`.** A value type, a strict parser (`ByteString`, `\u` escapes,
   surrogates), a `Builder` encoder, and small accessors (`key`, `asInt`, …). Tests:
   round trips, escapes, nesting, malformed input.
6. **ADR-23** (effects/runtime/jobs) and **ADR-2 amended** (the `process` boot library).

## Phase 1: Minor features

### 1a. Command palette (`space ?`)
- `Him.Picker`:
  - `PickerItem` gains `piDetail :: Text`, matched after the label and drawn dimmed in
    `Him.Render.Picker`.
  - `PickTarget` gains `PickAction Text` (an invocation).
- A new `command_palette` action in `Him.Actions.Picker`:
  - **Items:** `registryActions` (`Him.Action`, already sorted by group).
  - **Keys:** for each action, the keys bound to it per mode, from `keymapBindings`
    (`Him.Keymap`) over `cfgKeymaps`. They are rendered with `showKeys`, e.g.
    `goto_file_start  g g  Go to the first line`.
  - The config is needed to build these, so the action requests `RunAction`-style
    handling. Simplest: `handleEvent` fills the palette items when it sees an
    `OpenPalette` effect.
- **Accept:** `RunAction` with the invocation. For an action with required parameters
  (`actParams` with no default), it opens the `:` line pre-filled with
  `action <name> `. A new ex command `action <invocation>` runs any action by its text,
  as ADR-17 noted for later.
- Bind `space ?`. The info box after `space` shows it automatically.

### 1b. Async, faster file picker
- **Measure first** and log it in `docs/BENCHMARK.md` as a new strategy row:
  - `listFiles` (`Him.FileTree`) time on `~/Documents/helix` and on a synthetic
    200k-file tree;
  - `matches` (`Him.Picker`) time per keystroke at 10k, 100k and 200k items.
- **Streaming scan:**
  - `file_picker` opens the picker at once and requests `StartJob (ScanFiles gen
    root)`.
  - The walk posts `EvJob (FilesFound gen batch)` every ~2k files or 50 ms, and
    `ScanDone` at the end. Items are appended; the count shows `scanning… N`.
  - Closing the picker cancels the scan; the generation drops late batches.
- **Faster walk:** a pool of N directory workers (`getNumCapabilities`) over an STM
  queue, keeping the ignore semantics (`Him.Ignore`; each directory's `Ignorer` is
  carried with it). Measure whether `unix` dirent types can replace the
  `doesDirectoryExist` stat per entry.
- **Faster matching:**
  - precompute a lowercased label;
  - reject with a `Text` subsequence check before scoring;
  - replace the full `sortOn` with a bounded top-K (e.g. 1000) best list;
  - score only new items when a batch arrives.
  - Only if per-keystroke matching at 200k still exceeds ~10 ms: move filtering into a
    job keyed by `(gen, query)`, keeping the last results until it answers.
- Raise `maxFiles` (`Him.Actions.Picker`) to e.g. 500k, since streaming makes a limit
  mostly a memory guard.
- **ADR-24** (async picker), plus benchmark log rows with before/after numbers.

## Phase 2: Git (gutter signs + staging)

1. **`Him.Diff`** (pure).
   - A Myers O(ND) line diff, after trimming the common prefix and suffix (typical
     edits stay cheap on big files).
   - Output is hunks: `Hunk {oldStart, oldCount, newStart, newCount}`, each classified
     as added, removed or changed.
   - It also builds a line map (old line → new line), used to place staged hunks.
   - Randomized test against a naive LCS diff: applying the hunks to the old lines
     must give the new ones (same style as `ropeModelTests`).
2. **`Him.Git`** (IO, through `Him.Process`, run as jobs).
   - repository root;
   - index blob `git show :<path>`;
   - HEAD blob `git show HEAD:<path>`;
   - mode `git ls-files -s`.
   - An untracked file shows all lines as added; outside a repository there are no
     signs.
3. **State.** `docGit :: Maybe GitState` holds the index lines, the HEAD lines, the
   unstaged hunks (index → buffer) and the staged hunks (HEAD → index, mapped onto
   buffer lines through the unstaged line map).
   - The base texts are loaded on open, save, staging, and switching to a buffer.
   - The unstaged diff is recomputed by a job after edits (by `docVersion`, debounced
     to once per input batch).
4. **Gutter.** `Him.Render.Gutter` gets a 1-column sign lane before the numbers
   (`gutterWidth` +1, in `Him.Render`).
   - Added: `▎` green. Changed: `▎` yellow. Removed: `▁` red, at the boundary.
   - Staged: the same glyphs in dim colours. Unstaged wins on lines that have both.
   - New `Theme` fields. Diagnostics will share this lane later, with priority.
5. **Navigation:** `] g` / `[ g` (next/previous change). The new `]` / `[` prefixes get
   titles in `prefixNames` (`Him.Config.Default`).
6. **Staging (`space g` prefix "git").**
   - `git_stage_selection` (`space g s`): build the new index text by applying, to the
     index lines, the unstaged hunks (line by line) that intersect the selected buffer
     lines. Write it with `git hash-object -w --stdin --path=<p>`, then
     `git update-index --cacheinfo <mode>,<sha>,<p>` (`--add` for an untracked file).
     The blob is encoded with the document's line ending and trailing newline (reuse
     the encoding in `Him.File`).
   - `git_unstage_selection` (`space g u`): the same, on HEAD → index.
   - `git_stage_file` (`space g S`).
   - `git_reset_selection` (`space g r`): an ordinary undoable edit back to the index
     text.
   - Staging what the buffer shows works even when it is unsaved, the same way
     `git add -p` stages worktree hunks.
   - Afterwards, refresh the base texts and diffs.
   - Line-granular staging: a changed hunk's removed/added lines are paired by position;
     the leftovers are pure adds or removes.
7. **Tests:** the diff model; staging pure functions (index text + hunks + selection →
   new index text); an integration test in a temp repo (`git init`, commit, edit,
   stage the selection, check `git diff --cached`). **ADR-25.**

## Phase 3: Syntax highlighting (provider API + tree-sitter)

1. **One common highlighting API (`Him.Syntax`); providers are interchangeable.**
   - The rest of the editor (the runtime's jobs, the document state, rendering, the
     theme, the config) knows only `Him.Syntax`. It never knows whether tree-sitter,
     TextMate or something else does the work.
   - Provider modules (`Him.Syntax.TreeSitter`, later `Him.Syntax.TextMate`) export
     exactly one value, a `SyntaxProvider`. No provider type appears anywhere else.
   - Swapping or adding a provider is a one-line change to the provider list in
     `Him.Config.Default`, a dependency injected as a record of functions.

   ```haskell
   -- A provider: something that can highlight some languages.
   data SyntaxProvider = SyntaxProvider
     { spName  :: Text                                   -- "tree-sitter", "textmate"
     , spStart :: Language -> IO (Maybe SyntaxSession) }  -- Nothing: no definition here

   -- A running highlighter for one document. Its state stays inside (a tree-sitter
   -- tree, or TextMate's per-line rule stacks).
   data SyntaxSession = SyntaxSession
     { ssUpdate    :: Int -> Buffer -> [TextChange] -> IO ()  -- version, text, edits
     , ssHighlight :: Int -> Int -> IO (IntMap [LineSpan])    -- spans for lines a..b
     , ssClose     :: IO () }

   data LineSpan = LineSpan { lsStart, lsEnd :: !Int, lsScope :: !ScopeId }
   ```

   - **Why the shape fits both kinds of provider:**
     - tree-sitter reparses (incrementally, from the edits) and queries a byte range;
     - TextMate re-tokenizes from the first changed line, using its cached line-end
       states, and returns the spans of the requested lines.
     - Either may ignore the edits and redo everything (`[TextChange]` may be empty,
       e.g. after an undo).
   - **The shared currency is scopes:** dotted names such as `keyword.control.import`.
     tree-sitter capture names and TextMate scope names use the same form, so one
     theme (longest-prefix lookup) colours both.
   - **Selection is configuration.** `Config` gets `cfgSyntaxProviders ::
     [SyntaxProvider]`, tried in order; the first `Just` wins. A language lists its
     definitions per provider (`langGrammars :: [(Text, Text)]`, e.g.
     `[("tree-sitter","rust"), ("textmate","source.rust")]`), so the same language can
     be served by whichever provider has a definition.
   - **TextMate:** per the earlier choice, no TextMate provider is implemented now. It
     will be a new module implementing this record, plus one entry in the provider
     list, and no other code changes. The ADR documents this as the contract.
   - **Tests inject a fake provider** (one that highlights a keyword list). That
     proves the editor works against the interface alone: jobs, versions, rendering
     and theme, with no tree-sitter.
2. **Scopes and themes.**
   - Scopes are dotted names (`keyword.control.import`); tree-sitter captures and
     TextMate scopes use the same form.
   - `ScopeId` is an interned index.
   - `Theme` gets `themeScopes :: Map Text Style`, resolved by the longest prefix
     (`keyword.control.import` → `keyword.control` → `keyword`), with defaults for
     Helix's scope names.
3. **Languages (`Him.Language`).** A built-in table of about 25 languages:
   - name and grammar name (matching the Helix `.so` names);
   - file extensions, file names, shebangs;
   - comment token;
   - LSP command and root markers (for phase 4).
   Detection happens on open (`Him.Actions.File.openFile`, `App.run`). A config file
   can extend the table later.
4. **Tree-sitter provider (`Him.Syntax.TreeSitter`).**
   - **Runtime:** copy the vendored runtime into `cbits/tree-sitter/` (`src/`,
     `include/`, `LICENSE`). Compile `src/lib.c` (amalgamation) through `c-sources` and
     `include-dirs` in `package.yaml`.
   - **Shim:** a small `cbits/ts_shim.c` that runs one query over a byte range and
     writes captures into an array, so there is one FFI call per highlight request.
   - **Grammars:**
     - Located through `$HIM_RUNTIME`, then `~/.config/him/runtime`, then
       `~/.config/helix/runtime`, then `/usr/lib/helix/runtime`.
     - Loaded with `System.Posix.DynamicLinker` (`dlopen`, then `dlsym
       "tree_sitter_<name>"`) and called through a `FunPtr` import.
     - The ABI version is checked (13–15).
   - **Queries:** `highlights.scm`, with `; inherits: a,b` resolved recursively.
     Predicates are evaluated in Haskell: `#eq?`, `#not-eq?`, `#any-of?`, `#match?`,
     `#not-match?`, and `#lua-match?` mapped onto the regex subset. Any other
     predicate counts as a non-match, logged once.
   - **Precedence:** Helix's convention, where the earliest pattern wins.
   - **Regex:** `#match?` needs a small backtracking regex engine (`Him.Regex`):
     literals, `.`, classes (`\d\w\s`, `[^…]`), anchors, groups, alternation, and
     `* + ? {m,n}` (greedy and lazy). It also enables regex search later.
   - **Parsing:** the text is copied into one pinned buffer per parse, in a job. The
     viewport plus a margin (±200 lines) is highlighted. When the view scrolls outside
     that window, new highlights are requested; the tree stays in the runtime.
   - **Incremental parsing (3b):** until this step, `ssUpdate` always gets `[]` (full
     reparse). 3b fills in the edits: a `Buffer` change log with byte offsets (`Him.Buffer`
     / `Him.Buffer.Rope` gain cached byte lengths), `ts_tree_edit`, and reparse with
     the old tree. Undo falls back to a full parse.
   - Injections (Markdown code blocks, HTML `<script>`) come later (3c).
5. **Rendering.** `docHighlights :: IntMap [LineSpan]` (with its version) on the
   document.
   - `TextArea.styleAt` uses the scope style as the base under the selection and the
     cursor.
   - `RowKey` (`Him.Render.Frame`) gains the line's spans, so cached rows stay correct
     (same idea as `rkClass`).
   - The ASCII fast path keeps working with per-span styles.
6. **Performance:** measure parse and highlight times on a large Rust/Haskell file from
   `~/Documents/helix` and log them. Budget: no input waits on parsing, because it all
   runs in jobs.
7. **Tests:**
   - `Him.Regex` against a naive reference on random small patterns;
   - query parsing and inheritance;
   - predicates;
   - the fake-provider integration;
   - a tree-sitter smoke test on a Rust snippet. If the runtime directory is missing,
     the test reports "skipped" and passes.
8. **ADR-26** (syntax API and providers), **ADR-27** (vendored tree-sitter, amending
   ADR-2), **ADR-28** (regex subset).

## Phase 4: LSP client

1. **Transport (`Him.Lsp.Transport`).**
   - Spawn the server with `process` pipes.
   - A reader thread does Content-Length framing → `Him.Json` → `EvLsp serverId value`
     events.
   - Writes are serialized through a `TChan`; stderr goes to `$HIM_LOG`.
   - The framing parser is pure and tested with arbitrary chunk splits.
2. **Client (`Him.Lsp.Client`, state in the runtime and in `edLsp`).**
   - One server per (language, root). Roots are found from the markers in
     `Him.Language`.
   - Lifecycle: `initialize` (advertising `general.positionEncodings` utf-8, then
     utf-16), `initialized`, `didOpen`/`didChange` (full sync first, incremental later
     from the phase 3b change log)/`didSave`/`didClose`, and `shutdown`/`exit` on quit.
   - Pending requests are stored as pure data (`PendingHover docId pos`, …), so the
     replies are applied by pure functions.
3. **`Him.Lsp.Position`:** convert between `Pos` (character index) and utf-8/16/32
   offsets per line. Tested with emoji and CJK.
4. **Features, in order:**
   1. **Diagnostics:** signs in the gutter lane (by severity, ahead of git), underline
      style in the text, the message under the cursor in the status line, `] d` / `[ d`,
      and a diagnostics picker on `space x` (Helix uses `space d`, which is taken by
      the directory viewer).
   2. **Hover:** `space k`, in an `InfoBox` anchored at the cursor (`InfoPlace` gains
      `AtCursor`). Markdown is shown as plain text.
   3. **Navigation:** `g d` (definition), `g y` (type definition), `g i`
      (implementation), `g R` (references; a picker of `file:line  text`). This avoids
      the directory layer's `g r`. Opening uses `openFile` plus moving the cursor.
   4. **Completion:**
      - an insert-mode popup near the cursor;
      - it opens on trigger characters or after typing identifier characters
        (debounced per batch), or with `C-x`;
      - `C-n`/`C-p`/`tab` select and `ret` accepts;
      - `textEdit` is applied, with snippets reduced to their plain text.
   5. **Editing:** `space r` (rename; a `WorkspaceEdit` applied to buffers, opening the
      ones that are needed), `:format`, and `space a` (code actions that are edits).
5. **Tests:**
   - framing;
   - JSON-RPC classification;
   - position conversion;
   - applying `TextEdit`s (pure);
   - a fake in-process server over pipes that answers `initialize`/`hover`/
     `definition`.
   - Manual: clangd on a small C file, and hls on him itself.
6. **ADR-29** (LSP architecture).

---

## Critical files

- **New:** `Him.Effect`, `Him.Runtime`, `Him.Process`, `Him.Json`, `Him.Diff`, `Him.Git`,
  `Him.Language`, `Him.Syntax`, `Him.Syntax.TreeSitter`, `Him.Regex`,
  `Him.Lsp.{Transport,Client,Position}`, and `Him.Actions.{Git,Lsp}`.
- **New C:** `cbits/tree-sitter/` (vendored) and `cbits/ts_shim.c`.
- **Changed:**
  - `src/Him/App.hs` (effects, runtime, `EvJob`/`EvLsp`);
  - `Him.Editor` (`edEffects`, `edNextId`, `edLsp`);
  - `Him.Document` (`docId`, `docVersion`, `docGit`, `docHighlights`, `docLanguage`);
  - `Him.EditorM` (`request`, version bump);
  - `Him.Event`;
  - `Him.Picker`, `Him.Actions.Picker`, `Him.FileTree`;
  - `Him.Render.{Gutter,TextArea,Frame,Picker,Info}`, `Him.Render.Theme`;
  - `Him.Config` / `Him.Config.Default` (providers, bindings: `space ?`, `space g …`,
    `] g`, `space k`, `g d`, …);
  - `package.yaml` (`process`, the tree-sitter C sources).
- **Reused:** `Him.Action` (`registryActions`, `bindText`, `actParams`), `Him.Keymap`
  (`keymapBindings`, `children`), `Him.Ignore`/`Him.FileTree`, `Him.Actions.File.openFile`,
  `Him.Edit.applyEdits` (applying LSP edits), `InfoBox`/`drawInfo`, and `Picker`/`drawPicker`.

## Verification

- **Each step:** `stack test` passes (308 tests today, plus each step's new tests), with
  no warnings, and is committed.
- **Manual checks in tmux** (`tmux send-keys` / `capture-pane`, as before):
  - `space ?` lists every action with its keys, and running one works;
  - the file picker on `~/Documents/helix` opens at once and fills in while typing;
  - in a temp git repo, edit, see the signs, `space g s` on some lines, and check
    `git diff --cached` plus the dim signs;
  - open a `.rs`/`.hs` file and see colours;
  - with clangd on a C file: diagnostics, `space k`, `g d`, completion.
- **Performance:** `bench/bench.py` latency scenarios must not regress, measured when
  the machine is idle (the benchmark pause in §8 still applies). New picker and
  highlighting numbers go into the `docs/BENCHMARK.md` log.
- **Docs:** ADRs 23–29, the module map, keybindings §6, milestones, the tutorial parts,
  and §8 updated at the end of each phase.
