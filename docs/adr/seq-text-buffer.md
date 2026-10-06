# `Seq Text` line buffer behind an abstract interface

*Superseded by [ADR rope-buffer](rope-buffer.md).*

`Him.Buffer` exposes operations (`lineCount`, `getLine`, `insertAt`, `deleteRange`, …) and
hides its representation. `Data.Sequence` gives O(log n) splitting and indexing by line,
which is plenty for normal files.
*Alternative:* a rope. It can replace the internals later without changing any callers.
