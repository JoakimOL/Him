# Module names say what modules hold (2026-10-02)

Since [ADR actions](actions.md), the modules under `Him.Commands.*` hold *actions*, and `Him.Command`
holds the `EditorM` monad and its helpers. They are now `Him.Actions.*` and
`Him.EditorM`. The same pass made three more cuts:
- `Him.App` became the terminal frontend, and `Him.Session` the frontend-free event
  handling.
- `Him.Actions.Lsp` was split by feature.
- One `changeDocument` / `replaceBuffer` helper replaced four hand-built undoable
  replacements.

The old names stay in the older ADRs and log entries, which describe the code as it
was then.
