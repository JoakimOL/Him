# Keys bind to actions: named, grouped, with typed arguments (supersedes the `Command` registry of [ADR command-registry](command-registry.md))

An `Action` (`Him.Action`) has a stable snake_case name, a group, a doc string, and typed
positional parameters. A binding is text: the action's name plus arguments, such as
`move_line_down 5`, `goto_line 12`, `insert_text "// "` or `ex "w"`.
- **Names are the stable interface.** Bindings, and later config files, refer to the flat
  name. Helix uses the same flat names, so they stay familiar. The group (movement,
  selection, modes, editing, clipboard, history, search, prompt, misc) is metadata for
  help and docs only, so moving an action between groups breaks nothing. Renaming an
  action is a breaking change: keep the old name as a second action if it ever happens.
- **Arguments are typed, and are checked when the keymap is built.** Parameters are
  described with a small applicative (`int`, `text`, `choice`, `optional`). The same
  value lists the parameters (`actParams`, for help or a config UI) and converts the text
  arguments. Binding produces a `Bound` (the invocation plus the `EditorM ()` to run). So
  a key press neither looks anything up nor parses anything, and a bad binding is an
  error at startup that names the mode, the keys and the problem. Every error is
  reported, not only the first.
- **Keymaps are generic.** `Keymap a` is a trie of any binding type: `Keymap Text` while
  parsing and `Keymap Bound` at run time.
- **Prepared for a config file.** `Bindings = Map Mode [(keys, invocation)]`.
  `overrideBindings user defaults` puts user bindings on top, and `no_op` disables a key.
  `buildConfig actions bindings fallback` validates and builds everything. Select mode
  inherits normal mode's bindings, the user's included (`inheritsFrom`). A config parser
  only has to produce `Bindings` and call `Him.Config.Default.configWith`.
- **Invocation syntax.** Words are separated by spaces. A double-quoted argument may
  contain spaces and the escapes `\"`, `\\`, `\n` and `\t`. `renderInvocation` is the
  inverse.
*Alternatives:* separate names per argument value (`move_line_down_5`), which does not
scale. A `Value` sum type checked inside each action at run time, which reports errors
only when the key is pressed. Arguments stored per key in the keymap and passed on each
press, which is the same thing with an extra lookup.
**Counts** (`5 j`, `1 2 j`, `2 w`): in normal and select mode, digits typed before a key
sequence build `edCount`, which the status line shows. A binding without arguments
whose action's first parameter is `int "count"` runs with the count (`boundCounted`).
Other bindings ignore it, as does a binding that already gives arguments
(`move_line_down 20`). `0` only continues a count, and a digit that the keymap binds
keeps its binding. The count is capped at 1,000,000.
*Later:* `:action` runs any action by its invocation text (`:action goto_line 12`), and the
config file's `[keys.*]` tables produce the `Bindings` ([ADR toml-config](toml-config.md)).
