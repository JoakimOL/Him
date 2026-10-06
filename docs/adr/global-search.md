# A global search picker (`space /`) that searches as you type

Helix's `space /` and VS Code's search panel: lines of the project's files that contain
the query, in a picker with a preview.
- **Pattern:** literal text with smart case, the same needle as `/` (`Him.Search`), not
  a regex. Regex search is a separate item (§8), and `Him.Regex` is a backtracking
  engine on strings, too slow to run over a project.
- **Search** (`Him.Grep`, pure apart from reading a file): each file is read whole and
  scanned in C (`Him.Native.findForward`), one hit per line: the line number, the
  column of the first match, and the line (cut to 300 characters and copied, so a hit
  does not keep its file alive). Files over 20 MB and binary files (a NUL in the first
  8 KB, as for the preview) are skipped.
- **Job:** the picker's source is `GrepQuery`. Every change of the query starts a
  `GrepFiles` job (one `GrepJob` at a time, so the last one cancels the one before).
  - The job waits 80 ms first, so typing a word searches once.
  - It walks the files as `space f` does (`walkFiles`, the `[editor.file-picker]`
    options), searching each directory's files in the walk's worker threads.
  - Hits go out in batches (`GrepFound`, every 50 ms or 500 hits), only until the
    picker holds `matchLimit` (1000) of them; after that only the count grows.
    `GrepFinished` ends it.
  - Results name their generation and query; others are dropped.
- **Picker:** items are `path:line` with the line as the detail, and `PickPosition`
  targets, so the preview and `ret` go to the match. While a new query runs, the last
  hits stay, marked stale, until its first batch replaces them. The count is the
  number of matching lines found (not `matches/items`). A label too long for its
  column is cut so the detail does not cover it (search hits from the left, keeping
  the file name and line).
- **Speed:** on `/usr/include` (49k files, 600 MB, warm cache) the search takes about
  0.6 s and the first hits show within about 0.2 s. Reading the files is most of it.
  The editor runs on one capability (no `-N`), so the walk's workers overlap
  I/O but do not search in parallel; `-N4` changed little in a benchmark.
- **Not done:** open buffers' unsaved text is not searched (the files on disk are);
  matches are not highlighted in the list or the preview; files are searched in walk
  order, not sorted.

*Alternatives:* running `rg` / `grep` (fast, but an outside program the editor would
depend on, and its ignore rules would differ from the file picker's); searching only
after `ret` (Helix's old behaviour), which loses the "search as you type" the user
asked for.
