# him roadmap: language awareness, git, and the remaining gaps

> **Done.** All five phases of this plan (approved 2026-10-02) are built. What was built,
> and how it differs from the plan, is in the ADRs in `docs/adr/`. The current status and
> what comes next are in `docs/PLAN.md` §8. Three items from phase 3 are still open; they
> are listed there too.

## Phase 0: Foundation

Done. Effects as data, the runtime and jobs, document ids and versions, `Him.Process`
and `Him.Json`. [ADR effects-and-runtime](adr/effects-and-runtime.md); the `process` boot
library amends [ADR boot-libraries-only](adr/boot-libraries-only.md).

## Phase 1: Minor features

Done. The command palette on `space ?` and `:action <invocation>`
([ADR actions](adr/actions.md)), and the streaming file picker
([ADR streaming-file-picker](adr/streaming-file-picker.md)).

## Phase 2: Git

Done. `Him.Diff`, `Him.Git`, gutter signs, and staging, unstaging and resetting lines
on `space g`. [ADR git](adr/git.md).

## Phase 3: Syntax highlighting

Done: one provider API with a tree-sitter provider, scopes in the theme, the language
table, and a regex subset for query predicates. [ADR syntax-providers](adr/syntax-providers.md),
[ADR tree-sitter](adr/tree-sitter.md), [ADR regex-engine](adr/regex-engine.md),
[ADR grammar-setup](adr/grammar-setup.md). Grammars are not Helix's `.so` files: him
builds them itself into its runtime directory (`$HIM_RUNTIME` or
`~/.config/him/runtime`) with `him --grammar`.

Still open:
- **Incremental parsing (3b).** Every edit re-parses the whole file. The provider API
  already passes the edits; the tree-sitter provider ignores them.
- **Injections (3c):** Markdown code blocks, HTML `<script>`.
- **A TextMate provider.** Only the API allows for it; nothing is built.

## Phase 4: LSP client

Done. Transport, client, position encodings, diagnostics, hover, go to, completion,
rename, format and code actions. [ADR lsp-client](adr/lsp-client.md). The keys differ
from the plan: diagnostics are under `space d`, references on `g r`.

---

## Ground rules that still hold

- Nothing is downloaded.
- Only boot libraries are added.
- Each strategy and decision is documented: an ADR in `docs/adr/` (with the numbers
  for a performance strategy), and §8 refreshed when a phase stops.
  `docs/BENCHMARK.md` only holds the latest benchmark run.
- Every verified step is committed.
- Tests are written with the existing harness (`test/Spec.hs`, `Test.Harness`).
- **Performance:** `bench/bench.py` latency scenarios must not regress, measured when
  the machine is idle. New numbers go into their ADRs.
