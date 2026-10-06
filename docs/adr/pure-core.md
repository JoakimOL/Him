# Pure core, thin IO shell

Buffers, motions, edits, selections, keymap resolution, and rendering to a `Frame` are all
pure. Only `Him.Terminal.*`, `Him.File`, and `Him.App` perform IO.
