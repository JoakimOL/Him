# Plugin API: design and plan

*2026-10-05, branch `plugin-api`. Phases 0–5 are done ([ADR picker-actions](adr/picker-actions.md), [ADR plugin-building-blocks](adr/plugin-building-blocks.md), [ADR plugin-api](adr/plugin-api.md), [ADR personal-builds](adr/personal-builds.md), [ADR plugin-canvas](adr/plugin-canvas.md)). Not done
yet: `him --update`, and a real run of a personal build, which needs the network.* The goal is for users to write their own
plugins **in Haskell** and switch them on and off with the `:plugin` commands. A plugin
builds its UI from building blocks (pickers, status line segments, …) and can see the
editor's state and loaded buffers, much as Vim plugins can.

## Decisions so far

- **Plugins are compiled into `him`, as in xmonad.** There are no plugin processes and
  no run-time loading of `.so` files (see "Ruled out").
- **Releases include the plugins (option 1, the focus of development).** A release is
  built with the built-in plugins plus a curated contrib collection. The contrib
  plugins are compiled in and off by default. A user without a Haskell toolchain gets
  a plugin by switching it on (`:plugin-enable name`, or `name = true` under
  `[plugins]`). A new plugin reaches these users with the next release.
- **Later, for plugins outside the collection:**
  - option 2: a template repository whose CI builds a personal `him`, so no local
    toolchain is needed;
  - option 3: `him --rebuild` for users with GHC and stack.

  Both read the same plugin list, and both exist now (phase 4).

## Where we started

| What plugins need | What exists | Gap |
|---|---|---|
| Registering actions, keys and `:` commands | The `Plugin` record (`Him.Config`): `plActions`, `plBindings`, `plExCommands`, `plPrefixNames` | None. |
| Switching on and off | `:plugins`, `:plugin-enable`, `:plugin-disable`, `[plugins]` in the TOML. `App.setPlugin` rebuilds the `Config` and runs `plEnable`/`plDisable`. | Every plugin is on unless the TOML says otherwise. Contrib plugins need to be off by default. |
| Pickers | [ADR picker-actions](adr/picker-actions.md): items plus named primary/secondary actions, marks, `PickValue` payloads, `openPicker` | None, apart from the final API shape. |
| Status line | Hard-coded segments (`Render.StatusLine`) | No plugin segments. |
| Gutter signs, virtual text | `plSigns` only reserves the lane; git and diagnostics drawing is hard-coded | No generic signs or annotations. |
| Messages, popups | `info`/`failWith`, `edPopup` (`InfoBox`) | Timed notifications (later). |
| Events | `plHousekeeping`, `plBeforeRender`, `plJobResult`. Plugins poll for saves (`docSaves`) and opens. | No on-open, on-save, on-change or on-mode events. |
| Processes | LSP, REPL and chat each have their own constructors in the closed `Effect`/`Job`/`JobResult` sums | No generic "run a process and stream its output to plugin X". |
| Plugin state | Hard-coded fields (`edLsp`, `docGit`, `docLsp`, `DocKind`) | A new plugin can't add fields. |
| Reading state | The whole `Editor`/`Document` records, through `gets` in `EditorM` | Too much: the records change with almost every ADR, so they can't be a stable API. |

## The public API

The API is in-process and synchronous: a plugin's handlers run in the editor monad. The
model is **events in, commands out, queries for state, and UI described as data that
the core draws**. Plugins can't draw into the frame themselves, which keeps rendering
pure and keeps one plugin from breaking another's layout.

### Shape

```haskell
-- Him.Plugin: the stable surface. Everything else in Him.* is internal.
data PluginSpec s = PluginSpec
  { psName, psDoc     :: Text
  , psInitial         :: s                     -- the plugin's own state
  , psDefaultOn       :: Bool                  -- False for contrib
  , psActions         :: [PluginAction s]      -- name, doc, params, handler
  , psCommands        :: [PluginCommand s]     -- : commands
  , psBindings        :: [(Mode, Text, Text)]  -- default keys (the user's win)
  -- … psPrefixNames, psKeymaps, psOptions, psSigns
  , psOnEvent         :: Event -> PluginM s () -- see Events
  , psStart, psStop   :: PluginM s ()          -- plEnable / plDisable
  }

-- PluginM s: the editor monad, plus getState/putState for the plugin's own state s.
-- `hostPlugin :: PluginSpec s -> Plugin` (in Him.Plugin.Host) turns a spec into
-- the editor's Plugin record.
```

- **Queries** return stable *view* types, not `Editor`/`Document`:
  - the view types: `BufferInfo` (id, path, name, kind, language, dirty, line count,
    version), `WindowInfo` (id, buffer, focused), `Mode`, `Options` (read-only),
    `Diagnostic`;
  - text reads: `bufferText`, `bufferLine`;
  - `currentBuffer`, `buffers`, `windows`, `cursor`, `selections`.

  The internal records can then keep changing.
- **Commands:**
  - Buffers: `openFile`, `focusBuffer`, `setCursor`, `replaceRange` (through
    `changeDocument`, so undo, the jumplist and LSP sync keep working), `runAction`
    (any action, as an `Invocation`), `notify` / `warn`, `closeBuffer`.
  - Processes: `spawn` (a process whose output arrives as events), `sendInput`,
    `stopProcess`. Timers: `startTimer`, `stopTimer`.
- **Events:** `BufferOpened`, `BufferClosed`, `BufferEntered`, `BufferChanged id
  version`, `BufferSaved`, `ModeChanged`, `CursorMoved`, `ProcessOutput key line`,
  `ProcessExited`, `TimerFired`, `CanvasKey`, `CanvasClosed`.
  - `Session.housekeeping` finds them by comparing the editor with what it saw last
    time (`Him.PluginEvent.detectEvents`), and delivers them. Only Session sees
    `cfgPlugins`.
  - A picker's choice is not an event: `ret` / `del` run the plugin's named action,
    which reads `chosenItems`.
- **UI building blocks (declarative, owned by the core):**
  - **Picker:** `openPicker PickerSpec { pickerTitle, pickerItems :: [Item],
    pickerPrimary, pickerSecondary }`. Here primary/secondary name the plugin's
    actions ([ADR picker-actions](adr/picker-actions.md)), an `Item` carries a
    `Target`, and the action reads the choice with `chosenItems`.
  - **Status line segments:** `setSegments [Segment { segSide, segText, segFace,
    segPriority, segDoc }]` per plugin (or per buffer). The core lays them out with the built-in segments. Git's
    branch and LSP status are the first users.
  - **Signs and annotations:** `setSigns buffer [SignSpan]` and `setAnnotations
    buffer [Annotation]` (a line, virtual text, a face). The gutter and text area draw them in place
    of the hard-coded git and diagnostics paths.
  - **Popup:** `showPopup title rows`, an `InfoBox`.
  - **Scratch buffers:** `openScratch name text` (a read-only `DocKind`) for output
    such as logs or results; `setScratchText` replaces the text.
  - **Highlights, buffer keymaps, canvases** (phase 5): `setHighlights`,
    `setBufferKeymap`, `showCanvas`.
  - Later: an input prompt (`ask "Name: "` → an event), timed notifications.

### State without losing `Eq`/`Show` on `Editor`

- **UI state is plain data in the `Editor`:** `edPluginUI :: Map Text PluginUI` holds
  segments, signs and annotations. So `Editor` keeps `Eq`/`Show`, and the renderer
  stays pure.
- **A plugin's own state `s` is any Haskell type.**
  - The editor keeps it as a `Dynamic` under the plugin's name (`edPluginStates`,
    `Him.PluginState`), so each editor and each test has its own.
  - Switching a plugin off resets its state to `psInitial`.
  - Tests can read the state through the spec.
- **Escape hatch:** `liftEditor :: EditorM a -> PluginM s a`, from `Him.Plugin.Internal`.
  - It is for the built-in plugins while they move over.
  - Contrib plugins shouldn't need it, and review keeps them off it.

### Module layout

- `Him.Plugin`: the only module plugins import. It holds the operations and
  re-exports the types.
- `Him.Plugin.Types`: `PluginSpec`, `PluginM`, the view types, `PickerSpec`, `Item`,
  `apiVersion`. `Event` is in `Him.PluginEvent`, `Segment` and the other UI data in
  `Him.PluginUI`.
- `Him.Plugin.Host`: `hostPlugin :: PluginSpec s -> Plugin`.
- `Him.Plugin.Internal`: the escape hatch.
- `Him.Contrib.<Name>`: one module (or directory) per contrib plugin, importing only
  `Him.Plugin`.

## The contrib collection (option 1)

- **Where:** `src/Him/Contrib/` in this repository, listed in `Him.Contrib.contribPlugins`.
  `Him.Config.Default` adds them to `plugins`, so `[plugins]`, `:plugins` and
  `:plugin-enable` know them.
- **Off by default:** `Plugin` gets `plDefaultOn :: Bool`. It is true for git, LSP,
  REPL and chat, and false for contrib. `enabledPlugins` (UserConfig) uses it instead of
  "always on".
- **`:plugins` lists every plugin:** on or off, built-in or contrib, with its `plDoc`.
  A picker over them ([ADR picker-actions](adr/picker-actions.md)) can toggle one: `ret` switches it on or off, `tab` marks
  several.
- **Rules for a contrib plugin:**
  - it imports `Him.Plugin` only;
  - it has a README section (what it does, its keys, its options);
  - it has tests through the fake editor (`Test.Util`), and no network in tests;
  - it uses boot libraries only, which is the repository's rule anyway;
  - it does nothing until switched on: no cost at startup or per key when off (checked
    by a test).
- **How a plugin gets in:** a pull request that adds the module, its tests and its line
  in `Him.Contrib.contribPlugins`. The API is versioned (`apiVersion`) so contrib plugins and
  the core change together in one place.
- **Options:** a plugin's settings live under `[plugins.<name>]` in the TOML.
  `psOptions` names them (others are refused), and `optionText`, `optionInt`,
  `optionBool` read them.

## Later: options 2 and 3

- **One plugin list** for both, e.g. `~/.config/him/plugins.toml`:
  `plugins = ["github:someone/him-harpoon@1.2"]`. Each entry is a Haskell package that
  depends on `him` and states the `him` versions it builds against.
- **Option 2:** a `him-config` template repository. Its GitHub Actions workflow
  generates a `Main.hs` that adds the listed plugins to `plugins`, builds `him` for the
  user's platform, and publishes the binary as a release. `him --update` would download
  from the user's fork.
- **Option 3:** `him --rebuild` builds the same generated project locally with stack,
  and the running `him` execs the new binary.
- Both need `him` to work as a library with a small `main` (`Him.App.runWith
  extraPlugins`). Phase 2 should keep that possible.

## Ruled out

- **Plugins as separate programs** (Neovim remote plugins or LSP style): the user
  prefers compiled-in Haskell.
- **Loading `.so` files at run time:**
  - a `.so` only loads into the exact build of `him` it was compiled against, so every
    change to `him` breaks every plugin;
  - Haskell code can't really be unloaded;
  - a dynamically linked `him` starts slower.
- **An embedded interpreter** (the GHC API, `hint`, Lua): it needs a toolchain at run
  time or isn't Haskell.

## Phases

0. **Done:** picker actions and marks ([ADR picker-actions](adr/picker-actions.md)).
1. **Core building blocks**, with no public API yet, each with tests:
   - the event list and its delivery
   - plugin status segments
   - generic signs and annotations
   - a generic `spawn`, with output going to the plugin that owns it
   - `edPluginUI`

   As the proof: move git's signs onto the generic signs, and add a git branch segment.
2. **`Him.Plugin` and the host.**
   - `PluginSpec s`, `PluginM s`, the view types, `apiVersion`.
   - Port the REPL or git plugin onto it, to find what is missing.
   - Write the ADR.
3. **The contrib collection.**
   - `plDefaultOn`, `Him.Contrib`, `[plugins.<name>]` options, and the `:plugins`
     picker.
   - Two small first plugins that use the building blocks. Suggestions:
     - `wordcount`: a status segment;
     - `recent-files`: a picker with a "forget" secondary.
   - A tutorial section on writing one (since dropped: the tutorial covers only the
     core editor; the contrib plugins are the examples).
4. **Later:** the plugin list format, `runWith extraPlugins`, the template repository
   with CI (option 2), and `him --rebuild` (option 3).
5. **Done:** buffers and boxes of a plugin's own ([ADR plugin-canvas](adr/plugin-canvas.md)): highlights,
   keymaps for a plugin's buffers, a canvas with its own keys, timers. `magit`
   (magit-like) and `tetris` are the contrib plugins that prove them.

## Open questions for the user

- Should the built-in status segments (mode, file, position, the chat's) become
  ordinary segments that users can reorder in the TOML, as in Helix's
  `[editor.statusline]`?
- Should contrib live in the same package as the core (the simplest choice, and the
  current suggestion) or in a second stack package in this repository?
- Which first contrib plugins would you like to have?
