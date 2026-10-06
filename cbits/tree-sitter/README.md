# Vendored tree-sitter runtime

The C runtime of [tree-sitter](https://github.com/tree-sitter/tree-sitter)
**v0.26.9** (`lib/src` and `lib/include`), MIT licence (`LICENSE`).

Copied unchanged from the local cargo registry
(`tree-house-bindings-0.3.2/vendor`, which vendors that tag), so no download was
needed. It is compiled into him through `src/lib.c` (an amalgamation); see
`docs/adr/tree-sitter.md`. Grammars are not vendored: `him --grammar` fetches and
builds them, and him loads them at run time (`docs/adr/grammar-setup.md`).

To update: replace `src/`, `include/` and `LICENSE` with those of a newer tag,
and check `ts_language_abi_version` compatibility with the grammars in use.
