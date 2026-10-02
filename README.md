# him

A modal, selection-first (Helix-style) text editor for the terminal, written in Haskell
using only GHC boot libraries.

The design, the decisions behind it, and the milestone status are in
**[docs/PLAN.md](docs/PLAN.md)**.

## Requirements

- [Stack](https://haskellstack.org). The snapshot pins GHC 9.10.3, and Stack installs it
  if needed.
- Optional: haskell-language-server 2.14+ (it has a GHC 9.10.3 binary). `hie.yaml` is
  included.
- Optional: `fourmolu` and `hlint` on your PATH, for `make fmt` and `make lint`
  (`stack install fourmolu hlint`). HLS runs both inline anyway.

## Usage

```sh
make build              # stack build
make run ARGS="a.txt b.txt"  # run the editor (each file opens as a buffer)
make test               # run the test suite
make bench              # compare performance with vim and helix (docs/BENCHMARK.md)
make watch              # rebuild on save
make ghci               # REPL
make fmt / make lint    # format / lint
```

In the editor, `space ?` lists every command with its keys, `space f` opens a file picker
(it streams in the background and honours `.gitignore` and `.ignore`),
`space b` a buffer picker, `space d` a directory listing (`ret` opens, `-` goes up, `a` /
`+` / `r` / `d` create, rename and delete, `g .` shows dotfiles; also `:o dir` or
`him dir`), and `:` shows
the commands as you type (`tab` completes). In a git repository the gutter shows changed
lines; `space g s` stages the selected lines (`space g u` unstages, `] g` jumps). After a prefix key such as `g` or `space`, a
menu shows what can follow. The full key list is in docs/PLAN.md §6.

With a language server installed (clangd, rust-analyzer, haskell-language-server,
typescript-language-server, …), diagnostics show in the gutter. `space k` shows
documentation, `g d` goes to a definition, `g R` lists references, `space r` renames,
`space a` shows code actions, `:format` formats, and completion and signature help show
while you type. `:lsp-restart` restarts the server.

Syntax highlighting uses tree-sitter grammars that him compiles itself. Fetch grammar
sources once with Helix (`hx --grammar fetch`), then run `him --build-grammars`. That
builds them into `~/.config/him/runtime/grammars`; the highlight queries are read from
Helix's runtime.

Debug logging: `HIM_LOG=/tmp/him.log make run ARGS=file.txt`.

## Layout

```
app/Main.hs         argument parsing, then Him.App.run
src/Him/…           the library (see the module map in docs/PLAN.md)
cbits/              tiny C shims used through FFI (terminal size)
test/               test suite with a minimal built-in harness
bench/bench.py      benchmark against vim and helix (Python stdlib only)
docs/PLAN.md        living plan: assumptions, ADRs, milestones, where to pick up
```
