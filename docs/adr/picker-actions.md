# Every picker has a primary and a secondary action, and items can be marked

The user asked for a common picker API: two actions on two keys, so the file picker can
open several files and the jumplist can jump or remove. The longer aim is for the picker
to be a component of a public plugin API (it is: [ADR plugin-api](plugin-api.md)).
- **Named actions.** A picker names its actions: `pkPrimary` (default `picker_open`)
  and `pkSecondary` (`Maybe`). `ret` (`picker_accept`) and `del` (`picker_secondary`)
  run them as `RunAction` invocations. The named action reads the chosen items with
  `chosenItems` and closes the picker itself. A plugin gets picker actions by adding
  ordinary actions to `plActions`, nothing more. Pickers can't hold functions because
  `Editor` derives `Eq`/`Show` and `EditorM` can't see the `Config`.
- **Marks are generic.** `tab` (`picker_mark`) marks or unmarks the selected item and
  selects the next. The actions act on the marked items in list order, or on the
  selected one if none are marked. A mark is kept by the item's `piId`, its position in
  `pkItems`, so marks survive a new query. A picker that replaces its items (a search's
  new query, workspace symbols) clears them. The row shows `●` and the count adds
  "k marked".
- **`picker_open`** goes to every chosen file, buffer or position inside one
  `jumping` and ends on the last. Commands, code actions and jumplist entries use the
  first chosen item only. `jumplist_remove` removes every chosen entry, the last first.
- **`PickValue Text`** is a payload for a plugin's own actions; `picker_open` ignores
  it. `openPicker` (in `Him.EditorM`) is the one way to show a picker.

*Alternatives:* a registry of handler functions in `Config`, keyed by a picker kind
(more machinery for what named actions already give); marking as a per-picker secondary
(the jumplist couldn't remove several entries); `tab` as the secondary, as first
suggested (moving to the next item would lose `tab`).
