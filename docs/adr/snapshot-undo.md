# Undo with snapshots, committed outside insert mode

`edit` records the state before the first edit of a change. After every key, the main loop
commits it if the editor is not in insert mode. So `c foo esc` undoes in one step, as in
Helix. Snapshots are cheap because `Seq` shares structure. `docSavedBuffer` lets undo
back to the saved text clear the `[+]` marker.
*Alternative:* inverse change sets, which are smaller and needed for an undo tree or
collaboration. They can come later behind the same `Him.History` interface.
