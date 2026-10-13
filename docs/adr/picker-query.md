# Picker queries are words, as in Helix: fzf atoms and `%column`

The user asked to type several words they know are in a path (`src him magit .hs`)
and to filter by column with `%p`, as in Helix.
- **Words:** spaces separate words, and every word must match, in any order. An
  item's score is the sum of its words' scores (fewest characters skipped), then the
  usual tie-breaks (an exact first word, the shorter label).
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
- **Cost:** a one-word query does the same work as before. On 50k files (`/usr/include`)
  queries took 9 to 15 ms, against 9 to 14 ms before.
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
