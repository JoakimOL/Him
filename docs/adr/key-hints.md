# Texts that name keys look them up in the running config

Hints such as the review header ("space c a keep · space c d discard"), the chat's
placeholder, messages ("space c l lists them"), picker titles, the listing header,
magit's help line and the tetris panel used to name the default keys. They went
wrong as soon as a key was rebound, and some went wrong when the defaults moved
([ADR layout-friendly-keys](layout-friendly-keys.md)).

- **`Him.KeyHints`** (pure): a table from (scope, invocation) to the key sequences
  that run it. A scope is a mode (its keymap with inherited keys) or a plugin's
  keymap by its full name. The best keys come first: the fewest presses, then the
  shortest to read.
- **Where the table lives:** `cfgKeyHints` is built from the keymaps when a config is
  built (`withKeyHints`). It is lazy, so it costs nothing until a hint is shown.
  `Session.handleEvent` copies it into `edKeyHints` before every event, so reloads,
  switched plugins and a test's own config are always current, and render code
  can read it from the `Editor`.
- **Lookups:**
  - `keyHint mode action` and `keyHints mode [(action, what)]` in `EditorM`;
  - `keyFor` / `hintLine` in render code;
  - `keyFor mode action` and `keyInKeymap keymap action` in `Him.Plugin`.

  A single key falls back to `:action name` when nothing is bound. A hint line
  leaves unbound actions out.
- **Header in the buffer:** a listing's header line holds only its path and hidden
  count. The keys are drawn after it as virtual text, because the listing is
  loaded in IO, far from the config.
- **What stays as written:** the descriptions of actions and plugins, option docs,
  the comments in the dumped config, and `him --help`. These describe the
  defaults, and the command palette shows the live keys beside each action.

*Alternative:* passing the `Config` to the render code and the actions. That is a
much wider change, and the editor already carries similar derived state
(`edSignLane`).
