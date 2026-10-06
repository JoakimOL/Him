# Menus are data computed after every key; popups invalidate the rows they cover

- **Info box:** after each event, `Him.Info.refreshInfo` sets `edInfo :: Maybe InfoBox`
  from the editor and the config. After a prefix (`g`, `space`), the box lists the keys
  below it in the keymap trie and each action's doc. Prefix titles come from
  `cfgPrefixNames`. On the `:` line it lists the matching ex commands (`cfgExCommands`).
  The box is derived from state, never edited, so it cannot go stale. Rendering
  (`Him.Render.Info`) only draws it.
- **Completion:** `tab` on the `:` line completes the command name, or a path for
  commands whose `exArgs` is `PathArgs`. With several candidates it extends the line to
  their common prefix and lists them (`edCompletions`, cleared when the line is
  edited). Another `tab` (or `S-tab`) cycles through the candidates as in Helix,
  highlighting the one on the line; when the common prefix adds nothing, the first
  `tab` already puts the first candidate on the line. While the line reads
  `:theme <name>`, the screen is drawn in that theme if it loads (`previewedTheme`,
  checked by the main loop before each frame); `esc` goes back, `ret` keeps it.
- **Pickers:** a `Picking` mode with its own keymap, and a fallback that types into the
  query. `Him.Picker` is pure: items carry a `PickTarget` (a file or a buffer index)
  rather than an action, so the editor state stays plain data. The fuzzy score counts
  the characters skipped between the first and last match, from the best start; ties
  go to the shorter label.
- **Row cache:** a popup draws over text-area rows, and the next frame could copy those
  rows (popup included) from the cache ([ADR row-reuse-and-scrolling](row-reuse-and-scrolling.md)). So every popup deletes the row keys of
  the rows it covers. A test renders a frame with a box, then one without, and
  compares it with a fresh render.
