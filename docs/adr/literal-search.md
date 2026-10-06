# Search is literal, smart case, and anchored on the rarest byte

- **Matching:** there is no regex engine, since none ships with GHC. A pattern
  without upper-case letters matches ASCII letters case-insensitively.
- **Exact matches:** these use glibc `memmem`.
- **Case-insensitive matches:** these scan for the needle byte that is rarest in a
  4 KB sample of each block, in both cases, 16 bytes at a time, and verify each
  candidate.
- **Why not Boyer–Moore–Horspool:** it was tried. It was 4× slower when the first
  byte was rare, and only slightly faster when it was common.
- **Incremental preview:** this runs once per input batch (`edPreviewPending`), not
  once per key.
