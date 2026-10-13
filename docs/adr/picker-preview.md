# Pickers preview where an item points

- **What has a preview:** an item that is a place (a file, a buffer, a position from a
  language server). It shows beside the list when the box is at least 60 columns wide:
  the file around the item's line, with that line highlighted and line numbers.
- **Where the text comes from** (`previewFor` in `Him.Editor`, pure):
  - an open buffer gives its own text, unsaved changes included;
  - any other file is read by a `LoadPreview` job (one per path) and cached in
    `edPreviews` while the picker is open; the cache is dropped when the picker closes.
- **Highlighting:** an open buffer's preview uses the spans it has. A file read for the
  preview is highlighted by the same providers in the `LoadPreview` job, after its text
  is posted (`PreviewHighlighted`), with a session of its own that is closed after,
  for its first 20000 lines.
- **What is not shown:** binary files (a NUL in the first 8 KB) and files over 20 MB
  show a note instead.
- **Scrolling:** `C-d` / `C-u` move the preview by half its height. It starts a third
  of the way above the item's line and stops once the file's end is in view
  (`previewTop` in `Him.Picker`, shared with the renderer; the box's size follows from
  the terminal's alone, so the keys know the page). The scroll belongs to one item
  (`pkPreviewScroll` holds its `piId`), so selecting another item shows that one from its
  start, and a new query starts over too, since ids can then name other items. The
  keys mean half a page in normal mode as well, and letters cannot be used because
  they go into the query.
- **Drawing:** the box keeps one border, with a divider between the list and the
  preview; the preview's rows are dropped from the row cache, like every popup.

*Alternative:* opening the file in a hidden buffer. That loads more than needed and
mixes previews into the buffer list.
