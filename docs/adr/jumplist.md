# A jumplist per window, as in Helix, with entries you add and remove

The user asked for Helix's jumplist as the baseline, plus deleting entries from its
picker and adding entries by hand.
- **The list** (`Him.Jumplist`, pure) is Helix's: at most 30 jumps, oldest first,
  and a current index that equals the length when you are not walking the list.
  - `push` drops the jumps after the current one, does not repeat the last jump,
    and drops the oldest when the list is full.
  - `backward` (`C-o`) first pushes where the cursor is when you are not walking the
    list yet, so `forward` (`C-i`, `tab`) can come back to it. A jump to where the
    cursor already is gets skipped. Both take a count.
- **Per window:** `edJumps` maps window ids to lists. A new split starts with an empty
  list, and a closed window's list is dropped.
- **What jumps:** an action wraps its work in `jumping`, which pushes the place
  before if the cursor moved or the document changed. Helix's set: `g g` (and
  `<count> g g`, `goto_line`), `g e`, `%`, `g d` / `g y` / `g i` / `g r` (via `openAt`),
  `space d n` / `space d p`, `space g n` / `space g p`, and switching documents (`g n` / `g p`, `:o`,
  accepting any picker that goes to a place). Searches (`/`, `?`, `n`, `N`) are
  jumps too, as in Vim.
- **By hand:** `C-s` (`save_selection`) pushes the selection. `space j` lists the
  jumps, newest first, with a preview. `ret` goes to an entry as a jump of its own,
  so picking never cuts the list short, and `del` removes an entry
  (`picker_secondary`, see below).
- **Following edits:** a jump stores a document id and a selection. `edJumpTexts`
  keeps, for each document with jumps, the version and text the positions refer
  to. After every event (`syncJumps`, in housekeeping) and before the list is used,
  a document whose version changed has its jumps moved through
  `Buffer.changeBetween` (one span; positions inside it go to its start) and clamped.
  So edits from anywhere (typing, undo, a language server, the chat) are followed
  without each edit path knowing about jumps. Jumps into closed documents are
  dropped.
- **`picker_secondary`** (`del` in pickers) is a picker's second action on the
  selected item. For the jumplist it removes the entry. [ADR picker-actions](picker-actions.md) makes it part of every
  picker.

*Alternatives:* keeping the positions in each document and mapping them inside every
edit path (more exact for several edits in one event, but every path would have to
take part); clamping only, as unfocused windows' selections do ([ADR window-splits](window-splits.md)), which sends
`C-o` to the wrong line after edits above it.
