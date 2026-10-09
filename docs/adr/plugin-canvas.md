# Plugins colour buffers, give them keys, draw canvases and keep time

More building blocks for `Him.Plugin`, after [ADR plugin-building-blocks](plugin-building-blocks.md) and [ADR plugin-api](plugin-api.md).
Two contrib plugins came with them and use nothing else: `magit` (a magit-like
status buffer) and `tetris`. As before, a plugin sets data and the core draws it.

- **Highlights:** `setHighlights buffer [Highlight line from to face]` colours parts of
  lines (character columns). They are stored in the plugin's `PluginUI` by document,
  then by line (`puHighlights`), and are drawn over the syntax highlighting. They are
  part of the row cache key. They do not move when the text changes, so the plugin
  sets them again. A closed document's highlights are dropped, like its signs.
- **Keymaps of a plugin's own:** `psKeymaps` names keymaps (keys to action
  invocations). They are checked against the actions when the config is built and
  are stored as `cfgKeymapLayers` under `plugin:name`.
  - `setBufferKeymap buffer (Just name)` puts one over normal and select mode's keys
    in that buffer (`puKeymaps`). `magit` binds `s`, `u`, `tab`, `ret`, `c`,
    `g r` and `q` this way.
  - A canvas names one in `canvasKeymap`.
- **Canvas:** `showCanvas name (Canvas title width height rows keymap)` shows a box in
  the middle of the screen (`edCanvas`, one at a time; a new one replaces it).
  - The rows are runs of text in faces, one cell per character. The box shrinks to
    fit the screen, and it is drawn last, over pickers and popups
    (`Him.Render.Canvas`). The terminal cursor is hidden while it shows.
  - While it is open it has every key. Keys its keymap binds run their actions;
    other keys reach the plugin as `CanvasKey name key`, with the key written as in
    bindings (`"left"`, `"C-x"`). An unbound `esc` closes it and sends
    `CanvasClosed name`, so a plugin that handles no keys cannot trap the user.
  - `closeCanvas` closes it from code. Switching the plugin off closes it too.
- **Timers:** `startTimer name ms` and `stopTimer name` are effects (`TimerStart`,
  `TimerStop`). The runtime keeps a thread per timer that posts `TimerTick`, which
  reaches the owner as `TimerFired name`, like a process's output. Switching the
  plugin off stops its timers (`TimerStopAll`), and quitting stops them all.
- **Smaller additions:**
  - `setScratchText` replaces a scratch buffer's text and keeps its cursor;
    `openScratch` still moves the cursor to the top and shows the buffer.
  - `closeBuffer` closes a buffer.
  - The core action `command_mode_with <text>` opens the `:` line with the text
    typed (`magit`'s `c` opens `:magit-commit `).
  - `apiVersion` is 2.

*Alternatives:*
- **Raw colours in canvases** (RGB per cell). Faces follow the theme, and tetris looks
  right in every theme, so faces only for now. A colour constructor on `Face` would
  make `faceScope` partial.
- **Plugin-defined modes** in `Him.Mode`: `Mode` is a closed enum that the config
  file's `[keys.<mode>]` tables name. Layers keyed by text keep plugin names out of
  it. The user cannot rebind a plugin's layers yet.
- **A canvas as a window in the layout tree:** that is a larger change (focus,
  splitting, closing). A floating box over everything is what a game or a dashboard
  needs.
- **Delivering every canvas key as an event, with no keymap:** this is simpler, but
  the keys could not be listed or rebound, so both are offered.
