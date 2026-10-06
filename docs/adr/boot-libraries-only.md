# GHC boot libraries only

Allowed: `base`, `unix`, `bytestring`, `text`, `containers`, `transformers`, `directory`,
`filepath`, `stm`, `array`, `process`. (`process` came with git and
language servers, `filepath` with the directory viewer.) **Amended by [ADR tree-sitter](tree-sitter.md):** the
tree-sitter C runtime is vendored in `cbits/tree-sitter`; it is the one C dependency
beyond the small shims. Each one is added to `package.yaml` only when a module first uses
it (`-Wunused-packages` enforces this).
*Alternatives:* `vty`/`brick` (large and opinionated), `text-rope` (see [ADR seq-text-buffer](seq-text-buffer.md)).
