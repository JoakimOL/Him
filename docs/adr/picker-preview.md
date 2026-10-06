# Pickers preview where an item points

- **What has a preview:** an item that is a place (a file, a buffer, a position from a
  language server). It shows beside the list when the box is at least 60 columns wide:
  the file around the item's line, with that line highlighted and line numbers.
- **Where the text comes from** (`previewFor` in `Him.Editor`, pure):
  - an open buffer gives its own text, unsaved changes included;
  - any other file is read by a `LoadPreview` job (one per path) and cached in
    `edPreviews` while the picker is open; the cache is dropped when the picker closes.
- **What is not shown:** binary files (a NUL in the first 8 KB) and files over 20 MB
  show a note instead.
- **Drawing:** the box keeps one border, with a divider between the list and the
  preview; the preview's rows are dropped from the row cache, like every popup.

*Alternative:* opening the file in a hidden buffer. That loads more than needed and
mixes previews into the buffer list.
