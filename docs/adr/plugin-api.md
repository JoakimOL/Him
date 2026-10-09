# `Him.Plugin`, the public plugin API, and the contrib collection

These are phases 2 and 3 of `docs/PLUGIN-API.md`. Plugins are compiled in, as in
xmonad. Releases include a contrib collection that is off until switched on.
- **`PluginSpec s`** describes a plugin. It has:
  - its name, doc and `psInitial` state;
  - `psDefaultOn`;
  - actions (`action`, `actionWith` with the `ArgSpec` combinators);
  - `:` commands (`command`);
  - default keys and prefix names;
  - `psOptions` (its settings);
  - `psSigns`;
  - `psOnEvent`, `psStart` and `psStop`.

  `Him.Plugin.Host.hostPlugin` turns it into the `Plugin` record. Switching a plugin
  off also drops its state.
- **`PluginM s`** is `ReaderT (Ctx s) EditorM`, with `MonadIO`. Its operations:
  - **Queries:** `BufferInfo`/`WindowInfo` views, `bufferText`, `bufferLine`, `cursor`,
    `selections`, `mode`, `options`, `diagnostics`.
  - **Changes:** `openFile` (a jump), `focusBuffer`, `setCursor`, `replaceRange` (one
    undoable change; the selections move through it with `mapThroughChange`),
    `runAction`, `notify`/`warn`.
  - **Processes** ([ADR plugin-building-blocks](plugin-building-blocks.md)): `spawn`, `sendInput`, `stopProcess`.
  - **UI:** `setSegments`, `setSigns`, `setAnnotations`, `showPopup`, `openScratch`
    (a new read-only `ScratchDoc`, made once and then replaced), `openPicker` with
    `Item`s whose `Target` is a value, a file or a position (files and positions get a
    preview and work with `picker_open`), `chosenItems`, `closePicker`.
  - **Settings:** `option`, `optionText`/`Int`/`Bool`, read from `[plugins.<name>]`.
  - **Files kept between runs:** `stateFile`, under `Him.Paths.stateDir`
    (`$HIM_STATE`, `$XDG_STATE_HOME/him`, `~/.local/state/him`).
  - **Escape hatch:** `Him.Plugin.Internal.liftEditor`, for built-in code only.
- **State** of any type lives in the editor as a `Dynamic` per plugin
  (`Him.PluginState`). Its `Eq`/`Show` instances look only at which plugins have
  state, which keeps `Editor`'s instances. Keeping state in the editor rather than in
  a global `IORef` means each editor, and each test, has its own.
- **Config:**
  - A plugin is on by default if its `plDefaultOn` says so. `[plugins]` takes
    `name = true|false`, or a `[plugins.name]` table with `enabled` and the plugin's
    settings (TOML has no room for both). Unknown settings are errors.
  - `defaultConfig` has the built-in plugins on.
  - The dumped config lists every plugin and the settings it takes.
- **Switching on while running:** a plugin switched on while the editor runs gets
  `BufferOpened` for every open buffer, then `BufferEntered` for the focused one, as
  it would have at startup. Without this, `wordcount` ignored buffers that were
  already open.
- **`:plugins`** opens a picker of every plugin, on or off, with its doc. `ret`
  (`plugin_toggle`) switches the chosen ones; `tab` marks several.
- **Contrib** (`Him.Contrib`): plugins import only `Him.Plugin`. Two came with
  it (`magit` and `tetris` came with [ADR plugin-canvas](plugin-canvas.md)).
  - **`wordcount`:** a segment per buffer, recounted on change, with `max-lines`
    (default 10000) because counting follows every change.
  - **`recent-files`:** `space o` / `:recent` opens a picker of the files entered
    lately, kept in its state file, with `max` (default 100). `ret` opens the chosen
    files and `del` forgets them.
- **Not done:** no built-in plugin moved onto the API. git, LSP, REPL and chat depend
  on their own jobs and document fields, so a port would mostly be `liftEditor`. The
  contrib plugins exercised the API instead, and the missing `CursorMoved` event came
  out of writing the tutorial's task.

*Alternatives:* a global `IORef` per plugin (state shared between editors and tests);
`.so` loading or plugin processes (ruled out in `docs/PLUGIN-API.md`); a typed options
record per plugin (more machinery than reading `Value`s with defaults).
