# Plugin API: design and plan

*Draft, 2026-10-05, branch `plugin-api`.* The goal is for users to write their own
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

  Both read the same plugin list. They are written down here but not developed yet.

## Where we are

| What plugins need | What exists | Gap |
|---|---|---|
| Registering actions, keys and `:` commands | The `Plugin` record (`Him.Config`): `plActions`, `plBindings`, `plExCommands`, `plPrefixNames` | None. |
| Switching on and off | `:plugins`, `:plugin-enable`, `:plugin-disable`, `[plugins]` in the TOML. `App.setPlugin` rebuilds the `Config` and runs `plEnable`/`plDisable`. | Every plugin is on unless the TOML says otherwise. Contrib plugins need to be off by default. |
| Pickers | ADR-48: items plus named primary/secondary actions, marks, `PickValue` payloads, `openPicker` | None, apart from the final API shape. |
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
  { name, doc   :: Text
  , initial     :: s                        -- the plugin's own state
  , actions     :: [PluginAction s]         -- name, doc, params, handler
  , commands    :: [PluginCommand s]        -- : commands
  , bindings    :: [(Mode, Text, Text)]     -- default keys (the user's win)
  , onEvent     :: Event -> PluginM s ()    -- see Events
  , start, stop :: PluginM s ()             -- plEnable / plDisable
  }

-- PluginM s: the editor monad, plus get/put for the plugin's own state s.
-- `plugin :: PluginSpec s -> Plugin` (in Him.Plugin.Host) turns a spec into
-- today's Plugin record.
```

- **Queries** return stable *view* types, not `Editor`/`Document`:
  - the view types: `BufferInfo` (id, path, language, dirty, line count, version),
    `WindowInfo` (id, buffer, selection, size), `Mode`, `Options` (read-only),
    `Diagnostic`;
  - text reads: `lineAt`, `textRange`, `bufferText`;
  - `currentBuffer`, `buffers`, `windows`, `selection`.

  The internal records can then keep changing.
- **Commands:**
  - Buffers: `openFile`, `setSelection`, `applyEdits` (through `changeDocument`, so
    undo, the jumplist and LSP sync keep working), `runAction` (any action, as an
    `Invocation`), `notify` (`info`/`failWith`).
  - Processes: `spawn` (a process whose output arrives as events) and `startJob`.
- **Events:** `BufferOpened`, `BufferSaved`, `BufferChanged id version`,
  `BufferClosed`, `ModeChanged`, `FocusChanged`, `PickerChose action items`,
  `ProcessOutput key line`, `ProcessExited`, `Tick` (housekeeping).
  - Core code raises them into an event list, `edEvents`.
  - `Session.housekeeping` delivers them. Only Session sees `cfgPlugins`, and this is
    where git and the REPL already poll.
- **UI building blocks (declarative, owned by the core):**
  - **Picker:** `openPicker PickerSpec { title, items :: [Item], primary, secondary }`.
    Here `primary`/`secondary` name the plugin's actions (ADR-48), `Item` carries a
    `PickValue` payload, and the choice arrives as `PickerChose`.
  - **Status line segments:** `setStatus [Segment { side, priority, text, style }]` per
    plugin (or per buffer). The core lays them out with the built-in segments. Git's
    branch and LSP status are the first users.
  - **Signs and annotations:** `setSigns buffer [(line, Sign)]` and `setAnnotations
    buffer [(range, Style, virtual text)]`. The gutter and text area draw them in place
    of the hard-coded git and diagnostics paths.
  - **Popup:** `showPopup InfoBox`, which exists already.
  - **Scratch buffers:** `openScratch name text` (a read-only `DocKind`) for output
    such as logs or results.
  - Later: an input prompt (`ask "Name: "` → an event), timed notifications.

### State without losing `Eq`/`Show` on `Editor`

- **UI state is plain data in the `Editor`:** `edPluginUI :: Map Text PluginUI` holds
  segments, signs and annotations. So `Editor` keeps `Eq`/`Show`, and the renderer
  stays pure.
- **A plugin's own state `s` is any Haskell type.**
  - The host keeps it in an `IORef` created when the `Config` is built. Functions and
    `IORef`s live in `Config`, as they do now.
  - Switching a plugin off resets its state to `initial`.
  - Tests can read the state through the spec.
- **Escape hatch:** `liftEditor :: EditorM a -> PluginM s a`, from `Him.Plugin.Internal`.
  - It is for the built-in plugins while they move over.
  - Contrib plugins shouldn't need it, and review keeps them off it.

### Module layout

- `Him.Plugin`: the only module plugins import (it re-exports the next three).
- `Him.Plugin.Types`: the view types, `Event`, `Segment`, `Sign`, `PickerSpec`, `Item`,
  `apiVersion`.
- `Him.Plugin.Query`, `Him.Plugin.Command`: the operations.
- `Him.Plugin.Host`: `plugin :: PluginSpec s -> Plugin`, state cells, event delivery.
- `Him.Plugin.Internal`: the escape hatch.
- `Him.Contrib.<Name>`: one module (or directory) per contrib plugin, importing only
  `Him.Plugin`.

## The contrib collection (option 1)

- **Where:** `src/Him/Contrib/` in this repository, listed in `Him.Contrib.plugins`.
  `Him.Config.Default` adds them to `allPlugins`, so `[plugins]`, `:plugins` and
  `:plugin-enable` know them.
- **Off by default:** `Plugin` gets `plDefaultOn :: Bool`. It is true for git, LSP,
  REPL and chat, and false for contrib. `enabledPlugins` (UserConfig) uses it instead of
  "always on".
- **`:plugins` lists every plugin:** on or off, built-in or contrib, with its `plDoc`.
  A picker over them (ADR-48) can toggle one: `ret` switches it on or off, `tab` marks
  several.
- **Rules for a contrib plugin:**
  - it imports `Him.Plugin` only;
  - it has a README section (what it does, its keys, its options);
  - it has tests through the fake editor (`Test.Util`), and no network in tests;
  - it uses boot libraries only, which is the repository's rule anyway;
  - it does nothing until switched on: no cost at startup or per key when off (checked
    by a test).
- **How a plugin gets in:** a pull request that adds the module, its tests and its line
  in `Him.Contrib.plugins`. The API is versioned (`apiVersion`) so contrib plugins and
  the core change together in one place.
- **Options:** a plugin's settings live under `[plugins.<name>]` in the TOML. Add a
  small typed reader to `PluginSpec`, e.g. `options :: OptionSpec o`.

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

0. **Done:** picker actions and marks (ADR-48).
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
   - A tutorial section on writing one.
4. **Later:** the plugin list format, `runWith extraPlugins`, the template repository
   with CI (option 2), and `him --rebuild` (option 3).

## Open questions for the user

- Should the built-in status segments (mode, file, position, the chat's) become
  ordinary segments that users can reorder in the TOML, as in Helix's
  `[editor.statusline]`?
- Should contrib live in the same package as the core (the simplest choice, and the
  current suggestion) or in a second stack package in this repository?
- Which first contrib plugins would you like to have?
