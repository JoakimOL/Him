# him

*This is just ai slop, but i kinda like the product*

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
make build                   # stack build
make run ARGS="a.txt b.txt"  # run the editor (each file opens as a buffer)
make test                    # run the test suite
make bench                   # compare performance with vim and helix
make watch                   # rebuild on save
make ghci                    # REPL
make fmt / make lint         # format / lint
```

## What it does

- **Editing, Helix-style.** Select, then act. Multiple selections; `f t`, counts,
  pages. Match mode: `m i w`, `m a (`, `m s"`, `m r ( [`, `m d (`, `m m`.
- **Files, buffers, windows.**
  - `space f` / `space b` pickers with a preview.
  - `space /` searches the project's files as you type.
  - A jumplist, as in Helix: `C-o` / `tab`, `C-s` to save a place, `space j` to
    list (and prune) it.
  - `space d` lists a directory (create, rename, delete).
  - `space ?` lists every command with its keys.
  - Splits: `C-w v` / `C-w s` (or `space w`), `:vsplit`, `:hsplit`.
- **The `:` line** shows the commands as you type; `tab` completes.
- **Highlighting** with tree-sitter grammars that him compiles itself. Fetch grammar
  sources once with Helix (`hx --grammar fetch`), then run `him --build-grammars`.
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
- `C-z` suspends the editor (`fg` brings it back).

## Configuration

`~/.config/him/config.toml` holds keys, settings, the theme, language servers, REPLs,
the chat and plugins. `him --dump-default-config` prints every default, with what each
key does. `:config-open` edits the file and `:config-reload` applies it.

Debug logging: `HIM_LOG=/tmp/him.log make run ARGS=file.txt`.

## Layout

```
app/Main.hs         arguments (--dump-default-config, --build-grammars), then Him.App.run
src/Him/…           the library (module map: docs/PLAN.md §4)
cbits/              C used through FFI: text scans, terminal size, tree-sitter
test/               the test suite (test/Test/*.hs) with a minimal built-in harness
bench/              bench.py (vim/helix comparison, Python stdlib only), micro-benchmarks
dev/                fake-claude: a stand-in for `claude` to check the chat without a model
docs/               PLAN.md (decisions, status), TUTORIAL.md, BENCHMARK.md, ROADMAP.md
```
