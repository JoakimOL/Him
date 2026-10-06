# Registers live in the `Editor`, and pasting is linewise when the text ends with a newline

This is the Helix/Vim convention. `selectionText` adds the implicit newline when `x`
selects the last line, so yank/delete/paste of lines behaves the same everywhere.
