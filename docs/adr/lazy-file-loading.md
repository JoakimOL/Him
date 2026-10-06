# Load files without copying, and index blocks lazily

A regular file is read into one pinned array of its size. If it is valid UTF-8, that
array becomes the buffer's `Text` directly, so nothing is decoded or copied.
- **Fallbacks:** invalid bytes get a lenient decode. Files of unknown size (pipes
  report 0) are read in chunks.
- **Lazy offsets:** a block's line starts are built on first use. Only the line count,
  an SSE2 newline count, is needed up front.
- **Testing:** the chunked path is checked through `loadDocumentChunked`.
