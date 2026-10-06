# A directory is a read-only document, plus a keymap layer

`:open dir` (or `him dir`, `space d`, `space D`) loads a listing, as in Emacs's dired.
- **The document:** a `Document` whose `docKind` is `DirectoryDoc entries`. Line 0 is the
  path, line 1 is `../`, then subdirectories (ending in `/`) and files, sorted. The
  entries are kept in the kind, so a line maps to its entry even when a name contains
  odd characters. Since it is an ordinary buffer, motions, counts, search and the
  buffer commands work unchanged.
- **The keys:** in normal mode on a listing, `keymapMode` selects the `Directory` layer,
  which inherits normal mode like select mode does. It adds `ret` (enter a directory in
  the same buffer, or open a file as a new buffer), `-` / `backspace` (the parent, with
  the cursor on the directory just left) and `g r` (list again). The status line shows
  `DIR`.
- **Read-only:** `edit` / `editEach` and `setMode Insert` refuse in a read-only document,
  and `:w` refuses to write a listing.
- **Colours:** the header and directories have their own styles. The row key gained
  the line's class (`rkClass`), so a cached row is never reused with the wrong colour.

- **File operations:** `a` (new file; a trailing `/` makes a directory,
  and missing parents are created), `+` (new directory), `r` (rename or move; the
  prompt starts with the current name) and `d` (delete). `d` deletes every entry the
  selections cover, so `x x d` or `% s` work, and asks for `y` first. Directories are
  deleted recursively, but a symlink is only unlinked, never followed (tested). The
  names are typed on the command line through a `FilePrompt` with a `FileAction`, so
  editing the name uses the ordinary command-line keys. After a rename, buffers showing
  the old path (or something inside a renamed directory) take the new path.
- **Dotfiles** are hidden by default, like the file picker. The header counts them,
  and `g .` (`edShowHidden`, shared by all listings) shows them.

*Alternatives:* a separate directory UI component, which would have to reimplement
movement and search. Editing the listing text and applying the difference (as Emacs's
wdired and oil.nvim do) would allow batch renames, but it needs a careful diff and
confirmation; prompts were simpler and safer first. Or a real editor mode, which every `setMode Normal` would have to
know about.
