# The file picker streams, and large pickers filter in the background

- **Walk:** `Him.FileTree.walkFiles` reads directories with a pool of up to 8 workers
  over an STM queue. It takes entry types from `readdir` (`unix`'s
  `readDirStreamWith`), so only links and unknown types are `stat`ed. Directory links
  are followed (as in Helix), each target once, and never a link that contains itself.
  On 200k files the walk went from 732 to about 370 ms.
- **Streaming:** `space f` opens the picker at once, and a `ScanFiles` job sends files
  in batches (every 5000 files or 100 ms). The count shows `…` while loading.
- **Ranking:** each item has a precomputed lower-case key, file name and length. An
  in-order character check rejects non-matches before scoring. Matches are bucketed by
  rank, and only the best 1000 are kept, plus a total count. It still costs
  about 75 ms when 200k items all match. So a picker of more than 20k items, with a
  non-empty query, ranks in a `FilterPicker` job, and keeps showing its last matches
  until the answer arrives. An answer for an older query is dropped.
- **Ties:** among equal fuzzy scores, an exact first word or file name wins
  (`goto_line` before `goto_line_end`), then the shorter label.
