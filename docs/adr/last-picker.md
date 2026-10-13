# `'` in a picker brings back the last picker of its kind

The user asked for Helix's `space '` (the last picker), but per kind: a file search can
stay in progress while `space b` switches buffers in between.
- **Kept per title.** `edLastPickers` maps a picker's title (`files`, `search`,
  `buffers`, …) to the last one of that kind. A picker is kept when it closes or another
  replaces it (`EditorM.openPicker` / `closePicker`, which plugins use too), unless it
  was left untouched: no query and nothing marked. Glancing at a picker, or opening one
  only to press `'`, does not lose the one before.
- **`'` (`picker_last`)** works on an empty query only. With a query typed, or nothing
  kept, it types `'`, so `foldl'` or `don't` still go into a search. A query that starts
  with `'` needs something typed before it.
- **Whole or afresh.** Files and global search (`pkKeepItems`) come back whole: items,
  query, selection and marks, with no new scan or search. Their items cost a walk of
  the project to find, and keeping the selection is the point when going through hits.
  If the kept one was still scanning or searching when it closed, its query goes on the
  new picker instead, which searches again. Every other picker lists what the editor
  holds now (buffer indices, the jumplist, diagnostics can all have changed). It gets
  the kept query, and its selection when the items are given at once.
- **Stale hits.** A kept file list misses files created since, and a kept search's lines
  and positions may have moved. Opening the picker without `'` gives fresh ones.

*Alternatives:*
- Helix's single `space '`. It can't keep a file search going while switching
  buffers, which was the reason for the request.
- A key that is never typed (`C-r`). `'` was the user's choice, and it matches `'`
  for searching the selection.
- Always re-running the kept query. That is simpler, but it loses the selection in a
  long list of hits.
