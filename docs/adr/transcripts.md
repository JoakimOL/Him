# REPL and chat buffers are transcripts, not text to edit

They should feel like a REPL. Everything before the input is read-only, and so is the
prompt; you can still move, select and yank anywhere.

- **One guard, where all editing passes.** `EditorM.editAll` is used by every typing,
  delete, change, paste and surround action. It applies the edit, asks
  `Buffer.changeBetween` for the first position that differs (cheap: shared blocks are
  skipped), and refuses the edit if that position is before the input: "only the
  input after the prompt can be changed (select and y copy from anywhere)". Edits
  that change nothing before the input go through as usual (with undo).
  - Undo is checked the same way. It may take back typing in the input, but not
    output that arrived since, because that would rewrite the transcript.
  - Output from the REPL or the model doesn't pass the guard: it is inserted by
    `Him.Transcript`, not by an edit.
- **Typing goes to the input.** Entering insert mode with the cursor up in the
  transcript (`i`, `a`, `o`, …) moves it to the end of the input, as typing in a
  terminal does (`EditorM.setMode`).
- **What is the transcript** is the document's input position (`inputPos`): REPL and
  chat buffers have one, other documents don't, so nothing else changes for them.
