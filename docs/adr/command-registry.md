# Commands are named values in a registry. Keymaps are tries of command names

*(Commands became actions with arguments; see [ADR actions](actions.md).)*
`Command { cmdName, cmdDoc, cmdRun :: EditorM () }` with `EditorM = StateT Editor IO`.
Keys are bound to command *names*, so bindings can later be loaded from a config file.
The trie supports multi-key chords (`g g`, `space f`). Resolving a key sequence gives
`Found | NeedMore | NoMatch`.
*Alternative:* pattern-matching on keys in one big `case`. It is fast to write but does not
extend or rebind.
