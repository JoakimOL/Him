# The chat looks and works like an editor's chat panel (VS Code's)

The transcript of `you>` / `claude>` lines and `[…]` notes was hard to read in a narrow
split. Its long lines ran off the edge, and the review hints were scattered. The chat
is now laid out as blocks, with an input box at the bottom (`Him.Chat.Transcript`,
pure).
- **Layout.** The buffer is the transcript, then the prompt `› ` and the message being
  typed.
  - Output goes on the transcript's last line, *above* the prompt, so the box stays
    at the bottom while the answer streams, and what you type moves down with it.
  - The transcript only grows at its last line, so lines keep their numbers. What
    each line is (`ChatMark`: You, your text, the context, Claude, a tool line, a
    proposed change, the review summary, a note, an error, a code fence or code) is
    kept in `csMarks` by line number.
  - The model's prose has no mark. The text area styles inline `code`, `**bold**`
    and headings in it (`inlineSpans`).
- **Wrapping.** Prose is wrapped at spaces to the chat window's width as it arrives:
  the last line is re-wrapped with each chunk, and list items wrap under their text.
  Code blocks are not wrapped (copying code must give the code).
  - This is hard wrapping: a window resized later keeps the old width. Soft wrapping
    would mean wrapping in the renderer, cursor movement and the review rows, which
    is too big for this.
- **Drawing.**
  - No line numbers: the gutter is one column, with a bar beside your messages and
    the code, review and input boxes.
  - Code blocks, the review summary and the input are drawn across the whole width
    (`ui.cursorline.primary` or `ui.popup` backgrounds).
  - An empty input shows a placeholder: the keys, or "Claude is working… (C-c stops
    it)" while the model answers. The status line says `working…` too.
- **What the model did** is shown as it happens:
  - `◦ Read a.txt`, `◦ Listed the project's files`;
  - Claude Code's own tools, through a new `ChatActivity` event: `◦ Searched the
    code` (Grep), `◦ Looked for files` (Glob);
  - `✎ a.txt +2 −1` for each proposed change.
  - At the end of the turn, a summary of the changes per file, with the keys.
- **Words.** Proposed changes are *kept* or *discarded*, as in VS Code (its "Undo"
  would be confused with `u`). The keys and the action names stay
  (`chat_approve` / `chat_deny`), and `:chat-keep` / `:chat-discard` are new aliases.
  Messages to the model still say "approved" and "rejected".
- **Input.** `up` / `down` recall the messages sent (on the input's first / last
  line; elsewhere they move a line), and `C-l` (or `space c n`) starts a new
  conversation. `space c y` copies the code block under the cursor (or the last one)
  into the register. A new chat shows a short welcome with the keys.
- **Smaller fixes found on the way:**
  - The view scrolls back to column 0 when the cursor returns to a column that fits
    (`scrollToCursor`). Before, it stopped with the cursor at the left edge.
  - The focused chat window follows the end of the transcript while its cursor is
    in the input.
