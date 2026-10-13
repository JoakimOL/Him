# Picker queries are words, as in Helix: fzf atoms and `%column`

The user asked to type several words they know are in a path (`src him magit .hs`)
and to filter by column with `%p`, as in Helix.
- **Words:** spaces separate words, and every word must match, in any order. An
  item's score is the sum of its words' scores, then the usual tie-breaks (an exact
  first word, the shorter label).
- **Word starts:** a fuzzy word's score is its cheapest match. Every character skipped
  between the first and last matched one costs 1, and every run of matched characters
  that starts inside a word costs 8 (`runPenalty`). A word starts at the first character,
  after `/ - _ .` or a space, at an upper-case letter after a lower-case one, and at a digit
  after a non-digit. So `lsp` puts `Lsp.hs` and `lsp-client.md` before `last-picker.md`,
  and `lp` puts `last-picker` before `help`. It works like fzf's and Helix's bonuses,
  but as a cost. A small dynamic-programming pass (one row per query character)
  finds the cheapest match, so it can choose word starts over first occurrences. An item's
  word starts are worked out once, lazily, as bits (`piStarts`). LSP completion
  (`fuzzyScore`) ranks the same way, which also favours camelCase starts.
- **Atoms** (fzf's, which Helix uses through nucleo): `word` fuzzy, `'word` a
  substring, `^word` a prefix, `word$` a suffix, `^word$` the whole text. `!` in front
  of any of them excludes the items that match. A word that is only `!` or `'` filters
  nothing, so typing it doesn't empty the list. `\` makes the next character plain
  (`\ ` for a space inside a word, `\%`, `\!`, …). Case is ignored, as before.
- **Columns:** every picker names its label and detail columns (`pkColumns`):
  files and buffers `path`, the global search `path` / `line`, the command list
  `name` / `description`, others `name` / `detail`. `%column` (any prefix: `%p`, `%d`)
  sends the words after it to that column until the next one. Words before it match
  the label, or else the detail, with label matches ranked first (as before). A `%word`
  that names no column is a plain word.
- **Global search:** its query is not fuzzy. The text before the first `%path` (any
  prefix, at the start or after a space) is the pattern, spaces and all. The words
  after it must match a file's path for its hits to be listed (`grepQuery`,
  `pathMatches`). The filter applies in the job, before a file is read, so it also
  makes a search faster. `printf("%p")` is still a pattern because the `%` follows a
  `(`, and `\%p` makes it plain. The pattern comes first: `%p src foo` searches
  nothing, since the pattern is empty.
- **Cost:** on 50k files (`/usr/include`, `bench/PickerBench.hs`), queries take 21 to
  25 ms, against 13 to 16 ms with the plain skip count. Lists that large filter in a
  background job (ADR streaming-file-picker), and a project of a few thousand files
  takes a millisecond or two. Lazy, cached `piStarts`, and a one-pass bit scan instead of
  lists, brought the pass down from 46 to 56 ms.
- **`'` first:** on an empty query, `'` still brings back the last picker ([ADR last-picker](last-picker.md)).
  To start with a substring word, type another word first (the order of words
  doesn't matter).

*Alternatives:*
- One fuzzy string with spaces as characters (the old way). Words had to appear in
  the order typed.
- Columns as more fields on items. Label and detail are the two columns every
  picker already draws, and naming them per picker was enough.
- Smart case, as in Helix. Keys are kept lower case for speed (ADR
  streaming-file-picker), and nobody has asked for it.
