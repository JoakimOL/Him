# Proposed changes are reviewed like staged hunks, in any order

This replaces the approve-before-continuing flow of [ADR ai-chat](ai-chat.md)/42. The user asked for all
proposed edits at once, decided in any order with the cursor on one, and for edits
that are easier to understand.

- **Proposals do not block.** Every tool call is answered at once. An edit is answered
  "proposed: the user reviews after your turn; go on as if applied". So the model
  makes all its changes in one turn, with Claude Code (MCP, [ADR claude-code-provider](claude-code-provider.md)) and the API alike.
  The `ChatDeciding` state and the per-call bookkeeping are gone.
- **A review per document** (`Him.Chat.Review`):
  - **The base** is the document's text before the chat's first change.
  - **The proposed changes** are the diff from the base to the buffer (`Him.Diff`),
    recomputed in the plugin's housekeeping when the buffer's version changes. This
    is the git-signs idea ([ADR git](git.md)), so changes that touch or follow each other, and
    edits by hand in between, need no line bookkeeping.
  - **Approving** one change applies it to the base (`applyHunks`, as staging does)
    and writes the base to the file.
  - **Denying** applies the reverse to the buffer.
  - A review with no changes left is done.
- **Deciding:** `space c a` / `space c d` act on the change under the cursor
  (`hunkAtLine`: its new lines, or for a removal the line after it), and `space c A`
  / `D` act on all of them. `space c n` / `space c p` move between changes, and `space c l` lists
  them in a picker with a preview. When a turn ends with proposals, the editor window
  takes the focus, in normal mode, with the cursor on the first change.
- **Your own unsaved edits are kept out of approvals** (`approveOnto`). Approving
  writes the file *as it is on disk* with the one change applied. If you had unsaved
  edits before the chat's first change, the file differs from the base: the change is
  moved onto the file's text past your edits (`mapLine` over the base-to-file diff),
  and they stay unsaved in the buffer. A change that overlaps one of them, or meets
  it at an insertion, is refused ("save it (:w) or undo it first"), because which
  lines are whose is not clear. Approving all goes change by change, from the last,
  with the same check.
- **Telling the model:** decisions are collected and put before the user's next
  message, with what still waits ("[The user reviewed your proposed changes: approved
  a.txt:2 (-1 +2) …] [Still waiting for review: …]").
- **Showing a change** (`Him.Review.displayRows`): the text area and the gutter draw
  rows, not just buffer lines.
  - Above each change's new lines there is a header row ("change 1/2 (-1 +2) space
    c a/d: approve/deny space c n: next") and its removed lines (red, from the base, with
    `-` in the gutter).
  - The new lines are highlighted with `+` in the gutter.
  - These extra rows are never in the buffer. The cursor, `cursorPosition` and the
    scrolling (`ensureCursorVisible` scrolls further while extra rows push the cursor
    below the margin) count them.
  - Row caching is unaffected (a cached row is copied only when its key, its
    content, matches). The terminal-scroll shortcut stays correct, because the diff
    compares cells.
  - The status line shows "N to review".
- **The chat transcript is easier to read:** `you>` and `claude>` lines are styled, and
  notes in brackets are dimmed.
- **Checked with the real binary** and `dev/fake-claude` (which now proposes two
  changes in one turn): approving the second change wrote only it, denying the first
  restored it, and the next message carried the review note.
