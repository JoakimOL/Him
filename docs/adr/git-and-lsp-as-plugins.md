# Git and the LSP client are plugins

A `Plugin` (in `Him.Config`) names everything a feature adds:
- its actions, default bindings, `:` commands and key-prefix titles;
- whether it draws in the gutter's sign lane;
- hooks: housekeeping after every event, `plBeforeRender` once per input batch, and
  `plJobResult` for every job result;
- `plEnable` / `plDisable` for switching it while running.

The core folds over `cfgPlugins` (the enabled ones) where it used to call git and LSP
code by name: `Session.housekeeping`, the job-result dispatch, and the per-batch flush.
`Config.Default.configWith enabled userBindings` builds the config:
- the core's actions and bindings, plus those of the enabled plugins;
- the user's bindings on top, minus those that name a switched-off plugin's actions
  (they come back with the plugin, rather than failing the config).

How plugins are switched:
- **At startup:** `[plugins] git = false` (all are on by default; later, contrib plugins are off by
  default, [ADR plugin-api](plugin-api.md)).
- **While running:** `:plugin-enable` / `:plugin-disable <name>` (`tab` completes the
  names; `ExArgs` gained `NameArgs`), and `:plugins` lists them. The loop keeps the
  `UserConfig`, makes the config again with the new set, and runs the hooks of the
  plugins that came and went (`switchPlugins`; `:config-reload` uses it too).
  - Switching git off resets every document's git state, so no signs are left.
    Switching it on looks every document up again.
  - Switching the LSP off sends `LspStopAll`, which the runtime turns into stopping
    every server. It also clears `edLsp`, the attachments, the completion menu and
    the popups.
- **Gutter:** the sign lane exists only while a sign-drawing plugin is on
  (`edSignLane`).

Coupling that remains, on purpose:
- Plugin state still lives in `Document` (`docGit`, `docLsp`) and `Editor` (`edLsp`).
- `Effect`, `Job` and `JobResult` keep their plugin constructors.
- Rendering reads that state directly. With the plugin off, the state is empty, so
  nothing is drawn.
- A few core actions call LSP functions: the rename prompt, code-action and
  workspace-symbol pickers. They are only reachable through LSP actions.

Making these generic (state as `Dynamic`, plugin-provided gutter lanes) would cost
type safety for no user-visible gain yet. Syntax highlighting could become a plugin the
same way.
