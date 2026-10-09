# him

*This is just ai slop, but i kinda like the product*

## What is it?
A modal, selection-first (Helix-style) text editor for the terminal, written in Haskell
using only GHC boot libraries.

The design, the decisions behind it and the status are in
**[docs/PLAN.md](docs/PLAN.md)**. **[docs/TUTORIAL.md](docs/TUTORIAL.md)** explains how it
is built, step by step, and **[docs/BENCHMARK.md](docs/BENCHMARK.md)** compares it with
Vim and Helix.

## Requirements

- [Stack](https://haskellstack.org). The snapshot pins GHC 9.10.3, and Stack installs it
  if needed.
- Optional: haskell-language-server 2.14+ (it has a GHC 9.10.3 binary). `hie.yaml` is
  included.
- Optional: `fourmolu` and `hlint` on your PATH, for `make fmt` and `make lint`.

## Usage

```sh
stack build                  # to build
stack run -- <files>         # run the editor (each file opens as a buffer)
stack test                   # run the test suite
stack install                # build and install the built binary
```

## What it does

- **Editing, Helix-style.** Select, then act. Multiple selections; `f t`, counts,
  pages. Match mode: `m i w`, `m a (`, `m s"`, `m r ( [`, `m d (`, `m m`.
- **Registers and the clipboard,** as in Vim/Helix: `" a y` yanks into `a` without
  touching the others, `" a p` pastes it; `+` is the system clipboard and `*` the
  primary selection (`space y` / `space p` for short), `R` replaces the selection
  with a register (`space R`: with the clipboard), `_` discards, `C-r a` inserts in
  insert mode. `:registers` lists them, `:clear-register [a]` forgets them. The
  clipboard tool is found by itself (wl-clipboard, xclip, xsel, pbcopy, tmux, or the
  terminal's OSC 52); `[editor] clipboard-provider` picks one.
- **Files, buffers, windows.**
  - `space f` / `space b` pickers with a highlighted preview.
  - `space /` searches the project's files as you type.
  - A jumplist, as in Helix: `C-o` / `tab`, `C-s` to save a place, `space j` to
    list (and prune) it.
  - `space d` lists a directory (create, rename, delete).
  - `space ?` lists every command with its keys.
  - Splits: `C-w v` / `C-w s` (or `space w`), `:vsplit`, `:hsplit`.
- **The `:` line** shows the commands as you type; `tab` completes, and `tab` / `S-tab`
  again cycle through the candidates. `:theme <name>` previews the theme as you type
  or cycle; `esc` goes back.
- **Highlighting** with tree-sitter grammars that him fetches and compiles itself:
  run `him --grammar` once (it needs `git` and a C compiler). The highlight queries
  are built into him.
- **Themes:** any Helix theme (`:theme onedark`), or your own.
- **Plugins**, each of which can be switched off (`[plugins]` in the config,
  `:plugin-disable`):
  - **git:** signs for changed lines; `space g s` stages the selected lines, `] g`
    jumps.
  - **lsp:** diagnostics, `space k` hover, `g d`, `g r`, `space r` rename,
    `space a` code actions, completion, `:format`. Servers: clangd, rust-analyzer,
    haskell-language-server, typescript-language-server, pylsp, gopls.
  - **repl:** `:repl` opens one beside the file (`stack ghci` in a Haskell project),
    `space e` sends the selection, and saving reloads. Like the chat, it is a
    transcript: only the input after the prompt can change, but you can select and
    yank anywhere.
  - **chat:** `space c c` opens an AI chat beside the code. By default it runs
    through Claude Code (`claude`, with your login), which gets him's file tools over
    MCP. `provider = "anthropic"` in `[chat]` uses the API with `ANTHROPIC_API_KEY`
    instead. Like VS Code's chat: your messages and the answers as blocks, an input
    box (`ret` sends, `up` recalls), what the model read and changed, `space c y`
    copies a code block. The model proposes all its changes in one go; they show up
    in the editor with the lines they remove, and nothing is written until you keep
    them. With the cursor on a change, `space c a` / `space c d` keep or discard it
    (in any order); `] c` / `[ c` move between them, `A` / `D` do all.
  - **contrib**, off until switched on: `wordcount`, `recent-files` (`space o`),
    `magit` (`space g g`: a magit-like status buffer; `s` / `u` stage and
    unstage files, hunks or the selected lines, `tab` shows hunks, `c` commits) and `tetris` (`:tetris`).
- `C-z` suspends the editor (`fg` brings it back).

## Configuration

`~/.config/him/config.toml` holds keys, settings, the theme, language servers, REPLs,
the chat and plugins. `him --dump-default-config` prints every default, with what each
key does. `:config-open` edits the file and `:config-reload` applies it.

Debug logging: set the `HIM_LOG` env var.

## Layout

```
app/Main.hs         himMain (Him.Main: --grammar, --dump-default-config, …), then Him.App.run
src/Him/…           the library (module map: docs/PLAN.md §4)
cbits/              C used through FFI: text scans, terminal size, tree-sitter
test/               the test suite (test/Test/*.hs) with a minimal built-in harness
bench/              bench.py (vim/helix comparison, Python stdlib only), micro-benchmarks
dev/                fake-claude: a stand-in for `claude` to check the chat without a model;
                    sync-helix-runtime.py: refresh runtime/ from a Helix checkout
runtime/            the grammar list and highlight queries built into him (from Helix, MPL-2.0)
docs/               PLAN.md (decisions, status), TUTORIAL.md, BENCHMARK.md, ROADMAP.md
```

## Credits

- **[Helix](https://helix-editor.com)** ([github.com/helix-editor/helix](https://github.com/helix-editor/helix)):
  him's editing model follows Helix's, and him reads Helix themes. The highlight
  queries in `runtime/queries/` and the grammar list in `runtime/grammars.toml` are
  the Helix project's work, copied from its repository under the Mozilla Public
  License 2.0 (`runtime/queries/LICENSE`, `runtime/README.md`). Thank you to everyone
  who wrote and maintains them.
- **[tree-sitter](https://tree-sitter.github.io)**: the parsing runtime in
  `cbits/tree-sitter` (MIT), and the authors of each grammar, which `him --grammar`
  fetches from their own repositories under their own licences.
