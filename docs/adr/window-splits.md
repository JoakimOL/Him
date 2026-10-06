# Splits are a tree of windows; the focused one is the editor's state

Helix's model, without Vim's tabs (the buffer list stays as it was):
- **Layout (`Him.Window`, pure):** `Layout` is a tree, `Leaf windowId | Split Axis
  [Layout]`, where the axis is `Beside` (`:vsplit`) or `Stacked` (`:hsplit`).
  - Splitting inside a split along the same axis adds a sibling; otherwise the window
    is split in two.
  - Closing collapses a split that is left with one child.
  - `boxes` shares a split's space evenly, with a one-column border between windows
    side by side.
  - `neighbour` finds the nearest window on a side that overlaps across, preferring
    the one most in line.
- **State:** the *focused* window is still `edDoc` + `edView`, so no action changed.
  The other windows are `Window { winDoc, winView, winSelection }` in `edWindows`.
  - Focusing a window stashes the focused one as a `Window`, then makes the other's
    document current (the buffer zipper's `gotoBuffer`) with its view and selection,
    clamped in case the text changed meanwhile.
  - Each window therefore keeps its own cursor and scroll position, even on the same
    document. Edits are not mapped through an unfocused window's selection; clamping
    keeps it valid.
- **Rendering:** each window gets its own gutter, text area and status line. An
  unfocused window is drawn by the same components through `windowEditor`, the editor
  as that window shows it (normal mode, no popups).
  - Unfocused windows draw their primary cursor as a cell, use
    `ui.statusline.inactive`, and show no mode.
  - Borders use `ui.window`.
  - Row-cache keys are now keyed by `(row, column)`, since windows side by side share
    rows.
  - The terminal-scroll optimization ([ADR row-reuse-and-scrolling](row-reuse-and-scrolling.md)) applies only when the focused window
    spans the full width, because scrolling moves whole rows.
- **Highlighting** asks for the lines visible in every window on the current document
  when they are within 2000 lines of each other; otherwise only for the focused one.
- **Keys:** Helix's, after `C-w` or `space w`:
  - `v` / `s` split side by side / stacked;
  - `w` next window;
  - `h j k l` (also with `C-` and arrows) focus a neighbour;
  - `H J K L` swap;
  - `q` close, `o` only;
  - `n v` / `n s` split with a new scratch buffer.
- **Commands:** `:vsplit` / `:vs [files]`, `:hsplit` / `:hs [files]`, `:vnew`,
  `:hnew`.
- **Quitting:** `:q` / `:q!` / `:wq` close the focused window, and quit with the last
  one, as in Helix. `:qa` quits everything.
- **Paging** (`C-f`, `C-d`, …) moves by the focused window's height.
