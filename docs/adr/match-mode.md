# Match mode, as in Helix

- `m m` jumps to the matching bracket.
- `m s c` / `m r c d` / `m d c` add, replace and delete the pair around each selection.
- `m i x` / `m a x` select inside / around a text object.

The pure part is `Him.TextObject`:
- **Words:** `w` is a run of word characters (or of punctuation, or of blanks); `W` is
  a run of non-blanks. Around takes the blanks after it, or the blanks before it at the
  end of a line.
- **Paragraphs:** `p` is the run of non-blank (or blank) lines; around adds the blank
  lines after it.
- **Pairs:** brackets nest, found by scanning lazily backwards and forwards through the
  lines. Quotes pair up within a line, in order.
- **`m`:** the innermost pair of any kind.

The commands that need a character wait for the next key the way `f` does: `Await`
gained constructors, and `Him.Actions.Match.awaitedMatchKey` handles them. Surround
edits go through `applyEdits` (each range's pair is next to it). Tree-sitter objects
(`f`, `t`, `a` in Helix) are not done.

`I` and `A` arrived with it: insert after the line's indentation, and at its end.
