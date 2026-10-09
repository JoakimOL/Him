# What plugins build on: events, processes, segments, signs, annotations

This is phase 1 of the plugin API (`docs/PLUGIN-API.md`). The pieces are all core and
pure where they can be, and `Him.Plugin` exposes them ([ADR plugin-api](plugin-api.md)).
- **UI as data** (`Him.PluginUI`): each plugin has a `PluginUI` in `edPluginUI`
  (by plugin name) holding status line `Segment`s, gutter `SignSpan`s and end-of-line
  `Annotation`s by document. The renderer draws them, so a plugin never draws into the
  frame.
  - **Faces:** a `Face` names a theme scope, with an optional dimmed fallback to its
    parent (git's staged signs use `diff.plus.staged`). `faceStyle` turns a face into
    a style.
  - **Segments** come left after the file name or right before the position. They are
    shown best priority first while they fit, and the file name keeps up to 16 cells.
  - **Signs:** where signs overlap, the higher priority wins, and diagnostics win over
    all of them.
  - **Annotations** are part of the row cache key.
  - **Clean-up:** a closed document's entries are dropped, and a plugin that is
    switched off loses its `PluginUI`.
- **Events** (`Him.PluginEvent`) cover a buffer being opened, closed, entered,
  changed or saved, a mode change, and the cursor moving (`CursorMoved`, added with
  [ADR plugin-api](plugin-api.md)). Housekeeping compares the documents (version,
  saves), the mode and the focused document with `edSeen`, instead of each code path
  raising them. They reach `plEvent` of every enabled plugin, then the effects those
  handlers ask for run. At startup, every document is "opened".
- **Processes** (`Him.Spawn`): `ProcessStart key cmd args dir`, `ProcessSend`,
  `ProcessStop`, `ProcessStopAll owner`.
  - The key is `plugin:name`. Output and errors arrive line by line as `ProcessLine`
    and are routed to the owner's `plEvent` as `ProcessOutput name line`, then
    `ProcessExited name code`.
  - A process that is replaced or stopped says nothing more, guarded by a `Unique`.
  - Switching the plugin off stops its processes.
- **Git on top of it:** the git plugin sets its signs from the hunks (`gitSignSpans`;
  `GitState.gitSigns` is gone) and a branch segment for each document in a repository.
  `loadBase` reads the branch in the same `rev-parse` call (`gbBranch`). The branch is
  updated whenever the base reloads (save, staging).

*Alternatives:* raising events at each place a buffer opens, changes or saves (many
paths, easy to miss one); a plugin drawing into the frame itself (impure, and plugins
could overwrite each other); signs per line instead of spans (a long untracked file
would mean a map entry per line on every diff).
