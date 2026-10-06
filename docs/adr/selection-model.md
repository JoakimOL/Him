# Details of the selection model

Ranges are *inclusive* (a cursor covers the character under it), and `col == lineLength`
addresses the line end (the newline). In insert mode the head is read as a gap: text is
inserted before the character at the head. Edits apply to every range ([ADR bottom-up-multi-range-edits](bottom-up-multi-range-edits.md)).
