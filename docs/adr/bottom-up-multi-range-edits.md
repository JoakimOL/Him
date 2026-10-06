# Multi-range edits are applied from the bottom up, and positions are kept relative to the end

`Him.Edit.applyEdits` runs an ordinary single-range `Edit` on each range, from the last
range to the first. An edit only changes text around its own range, which lies before
every range already edited. So each result is kept as (lines from the last line,
characters from the end of its line). Those two numbers are unaffected by any change
earlier in the buffer, and they turn back into positions at the end. That avoids
change sets and position mapping, and every existing `Edit` works with many cursors
unchanged. A single range takes a direct path, so typing costs what it did before.
- **Normalizing:** `Selection.fromRanges` sorts the ranges and merges overlapping ones.
  It runs before edits and after motions, so the bottom-up order is well defined.
- **Registers:** a register holds one value per range. Pasting with as many values
  as ranges gives each range its own value; otherwise every range gets all of them,
  joined. Named registers, `_`, and the clipboard as `+` / `*` are in [ADR registers-and-clipboard](registers-and-clipboard.md).
- **Adjacent ranges:** an edit may reach just outside its own range. A backspace
  deletes the character before the cursor, and deleting the last lines takes the line
  break before them. Either can touch the text of the range before it, but only after
  that range's start, so that range's positions stay valid when its turn comes. The
  stored results of later ranges lie after the change, so they are not affected either.
  Randomized model tests cover inserts, backspaces, forward deletes, and range deletes
  with ranges right next to each other. Cursors that end up on the same position are
  merged. (An earlier version of this ADR listed adjacent cursors as a known limit.
  That was wrong: the tests show the same results as the string model.)
*Alternative:* change sets with position mapping (as in Helix). They are more general,
and needed for an undo tree or collaboration, but they are much more code.
