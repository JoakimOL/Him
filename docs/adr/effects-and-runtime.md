# Effects as data, and a runtime for background jobs

Actions are state changes (`EditorM = StateT Editor IO`). The `Editor` is plain data
and holds no handles, and actions cannot see the config.
- **Requests:** to ask for more, an action queues an `Effect` (`Him.Effect`) with
  `request`.
- **Immediate effects:** `handleEvent` carries out `RunAction` (run another action by
  its `Invocation`; used by the palette and `:action`) and `OpenPalette` right after the
  key, with the config at hand. A chain of them is capped at 8 rounds.
- **Background effects:** `StartJob` / `CancelJob` are left for the main loop, which
  owns the `Runtime` (`Him.Runtime`). The runtime runs each `Job` on its own thread, at
  most one per `JobKey` (starting one cancels the old one with `killThread`), and posts
  each `JobResult` as an `EvJob` event on the same channel as the keys. So results are
  handled by `handleEvent` like any input, batched with it ([ADR render-per-batch](render-per-batch.md)), and drawn once.
- **Stale results:** documents have a `docId` (assigned when opened) and a `docVersion`
  (bumped by edits, undo/redo and `replaceText`). Pickers have a generation. Every
  result names what it was computed for, and a stale one is dropped.
- **Testing:** tests can assert the effects an action requested. The `settle` helper in
  `test/Spec.hs` runs jobs on a real runtime, as the main loop does.
- **`Him.Process`** runs external programs (stdin in; stdout and stderr read
  concurrently), and `Him.Json` is a small JSON library. Both are for the git and LSP
  phases.

*Alternative:* `ReaderT Env (StateT Editor IO)`, which would give actions the handles
directly. Every action would change, and tests would need a full runtime.
