# Per-key work follows what is visible or settled (2026-10-02)

These are the low-hanging fixes from the 2026-10-02 benchmark:
- **Diagnostics are converted per frame for the drawn lines only**
  (`shownDiagnosticsIn`). A server can publish thousands of them.
- **The git diff is debounced:** the job waits 50 ms, and a newer version's job
  replaces it. Typing in a large tracked file therefore diffs once per pause instead
  of once per key, and the gutter catches up 50 ms after typing stops.
- **The built-in theme is parsed once,** and the TOML reader skips the character walks
  on lines that cannot need them.

Not done, on purpose:
- Moving diagnostic parsing off the main loop. It is rare (one publish per server
  pass), and doing it would need a second representation of the state.
- Writing the default theme as Haskell values, which would give two sources for one
  theme.
