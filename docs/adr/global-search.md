# A global search picker (`space /`) that searches as you type

Helix's `space /` and VS Code's search panel: lines of the project's files that contain
the query, in a picker with a preview.
- **Pattern:** literal text with smart case, the same needle as `/` (`Him.Search`), not
  a regex. Regex search is a separate item (§8), and `Him.Regex` is a backtracking
  engine on strings, too slow to run over a project.
- **Search** (`Him.Grep.grepFiles`): each file is read whole (a file descriptor, no
  `Handle`) into a `malloc`ed buffer reused for a directory's files, and its bytes are
  scanned in C (`Him.Native.grepLinesAt`, a chunk of hit lines per call, so the anchor
  byte is chosen once, not once per hit). Only the hit lines are decoded: one hit per
  line, with the line number, the column of the first match, and the line (cut to 300
  characters). A file without hits leaves nothing for the garbage collector, which
  matters with the non-moving collector (`-xn`), where allocating every file kept
  more capabilities from helping. Files over 20 MB and binary files (a NUL in the first
  8 KB, as for the preview) are skipped.
- **Job:** the picker's source is `GrepQuery`. Every change of the query starts a
  `GrepFiles` job (one `GrepJob` at a time, so the last one cancels the one before).
  - The job waits 80 ms first, so typing a word searches once.
  - It walks the files as `space f` does (`walkFiles`, the `[editor.file-picker]`
    options), searching each directory's files in the walk's worker threads.
  - Each call of the walk's `emit` packs its files' hits into a `HitChunk`
    (`Him.Grep`): file index, line, column and text end per hit in unboxed arrays, and
    the lines in one text. Chunks go out in batches (`GrepFound`, every 50 ms or 20000
    hits). `GrepFinished` ends it.
  - Results name their generation and query; others are dropped.
- **Picker:** keeps every hit, as Helix does (no `matchLimit`): its matches are
  `SearchHits`, the chunks by their first index, and an item (`path:line` with the line
  as the detail, and a `PickPosition` target, so the preview and `ret` go to the match)
  is made only for the rows shown, the selection and marked hits. A `PickerItem` per
  hit (five texts) would cost hundreds of bytes each; a hit costs its line plus 16. While a new query runs, the last
  hits stay, marked stale, until its first batch replaces them. The count is the
  number of matching lines found (not `matches/items`). A label too long for its
  column is cut so the detail does not cover it (search hits from the left, keeping
  the file name and line).
- **Speed:** on `/usr/include` (50k files, 620 MB, warm cache), `the` (1.08M hits)
  takes about 1.1 s of CPU in the editor and peaks at 200 MB (with no hits, 0.4 s and
  34 MB; Helix 25.07 peaked at 1.1 GB on the same search). The editor runs
  on one capability (no `-N`), so the walk's workers overlap I/O but do not search in
  parallel.
- **Filtering by path:** `pattern %p words` searches only files whose path matches
  the words ([ADR picker-query](picker-query.md)).
- **The selection:** `space '` (and `space *`, next to `*`) opens the picker with the
  primary selection as the query, and makes it the search pattern, so `n` / `N` find
  it in the file a hit opens. A selection spanning lines is refused, as for `*`.
- **Not done:** open buffers' unsaved text is not searched (the files on disk are);
  matches are not highlighted in the list or the preview; files are searched in walk
  order, not sorted.

*Alternatives:* running `rg` / `grep` (fast, but an outside program the editor would
depend on, and its ignore rules would differ from the file picker's); searching only
after `ret` (Helix's old behaviour), which loses the "search as you type" the user
asked for.
