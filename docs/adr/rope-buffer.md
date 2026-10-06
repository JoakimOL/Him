# The buffer is a rope of multi-line blocks

A `Block` is one UTF-8 `Text` holding many lines, plus an array of `Word32` line starts.
Blocks live in a weight-balanced tree that caches line counts (`Him.Buffer.Rope`).
- **Loading:** a file loads as a few large blocks, one per 1 MB read chunk.
- **Edits:** splitting a block is O(1) slicing; changed lines become a small new block.
- **Why:** with `Seq Text`, each of the 200,000 lines of the test file cost about
  50 bytes of heap objects. The copying GC also had to copy all of them.
- **Search:** blocks make search fast, because a whole block is scanned with one C call.
- **Interface:** `Him.Buffer`'s interface did not change. A randomized test compares
  thousands of edits against a list model.
