# Pure core, thin IO shell

Buffers, motions, edits, selections, keymap resolution, and rendering to a `Frame` are all
pure. Only `Him.Terminal.*`, `Him.File`, and `Him.App` perform IO.

Later: actions run in `StateT Editor IO` and ask for IO as effects that `Him.Session`
and the runtime (`Him.Runtime`) carry out ([ADR effects-and-runtime](effects-and-runtime.md)).
