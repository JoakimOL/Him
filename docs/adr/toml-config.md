# The config file is TOML, layered on the defaults

`~/.config/him/config.toml` (or `$XDG_CONFIG_HOME/him/config.toml`, or `$HIM_CONFIG`)
has three sections:
- `[editor]` and its sub-tables: every setting ([ADR settings-table](settings-table.md)) and `theme` ([ADR helix-themes](helix-themes.md)).
- `[keys.<mode>]`: `"keys" = "action invocation"`. The modes are normal, select,
  insert, command, picker, directory and completion.
- `[language-server.<language>]`: `command`, `args`, `roots`, `language-id` and
  `enabled`.

How it is read:
- **Format:** TOML, because Helix users know it. `Him.Toml` reads the subset a config
  needs into the JSON value type, and its errors name the line.
- **Keys:** the action layer ([ADR actions](actions.md)) did most of the work. Bindings are the same
  `Bindings` text as the defaults, put on top of them with `overrideBindings` and
  validated with them, so a bad key or action is reported with mode and keys.
  `no_op` unbinds a key.
- **Strict checking:** unknown sections, modes and settings are errors, because a
  silently ignored typo is worse. Every error is collected.
- **Failure:** a broken file starts the defaults, and the status row names the first
  problem, so the user can fix it in him (`:config-open`).
- **Defaults to start from:** `him --dump-default-config` prints every default as a
  config file: the bindings per mode, commented with what they do; the servers; and a
  list of all actions with their parameters. Read back, it equals the defaults
  (tested).
- **Changing it while running:** `:config-open` opens the file, pre-filled with the
  defaults if missing; saving creates the directory. `:config-reload` replaces the
  loop's config, the runtime's server table and the editor settings.

*Alternative:* a custom `keys = action` line format. It is simpler to parse, but it
leaves no room to grow and is unfamiliar.
