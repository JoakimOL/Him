# Tree-sitter: vendored runtime, grammars built for him

- **Runtime:** the C runtime (v0.26.9, MIT) is in `cbits/tree-sitter`, copied unchanged
  from the local cargo registry (no download). It is compiled through
  `cbits/ts_runtime.c`, which sets the feature macros for that unit only.
  `cbits/ts_shim.c` runs one query over a byte range and returns every capture in one
  array.
- **Provider:** `Him.Syntax.TreeSitter` dlopens `grammars/NAME.so` and reads the
  language's `highlights.scm` (built into him, see [ADR grammar-setup](grammar-setup.md)). It resolves `; inherits:` in place, and evaluates
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
  So only grammars from `$HIM_RUNTIME` or `~/.config/him/runtime` are loaded, and
  him compiles them itself with `-O2 -fno-strict-aliasing`; 299 of 301 built here.
  Where the sources come from, and the queries, is [ADR grammar-setup](grammar-setup.md)
  (`him --grammar`).
- **Cost** (`bench/HighlightBench.hs`, log 24): a 7,241-line Rust file parses in 22 ms,
  and a 260-line window highlights in 3 ms. Loading a grammar and its query takes
  30–160 ms, once per document. All of it runs in jobs. Every edit re-parses fully for
  now; incremental parsing (roadmap 3b) would use the edits.

*Alternative:* the Hackage `tree-sitter` package. It is not a boot library, was last
tested with GHC 9.2, bundles an old runtime and its own grammars, and targets AST
extraction, not highlight queries.
