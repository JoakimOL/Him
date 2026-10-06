# Buffers are a zipper around the current document

The `Editor` keeps the current document in `edDoc` (with `edView`), as before, plus
`edBefore` (nearest first) and `edAfter`: the other buffers, each with its own view.
Code that works on the current document did not change. Switching moves documents
between the lists (`switchBuffer`, `gotoBuffer`), `:open` inserts after the current
buffer, and closing the only buffer leaves a scratch buffer. Each document keeps its
own selection and undo history. `:open` compares canonical paths, so a file that is
already open is switched to rather than loaded twice. `:q` refuses while any buffer is
modified.
*Alternative:* a `Seq Document` plus an index. That would change every `edDoc` access.
