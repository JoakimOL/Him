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
make run ARGS=file.txt  # run the editor
make test               # run the test suite
make bench              # compare performance with vim and helix (docs/BENCHMARK.md)
make watch              # rebuild on save
make ghci               # REPL
make fmt / make lint    # format / lint
```

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
