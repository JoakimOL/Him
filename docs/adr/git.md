# Git through the `git` program; the diff in-process

- **Base texts:** a document's git state (`docGit`, `Him.GitState`) holds its file's
  index and HEAD versions. They are loaded by a `GitLoad` job running `git rev-parse`,
  `ls-files --stage` and `show :path` / `show HEAD:path` (`Him.Git` through
  `Him.Process`). This happens when the document becomes current, after a save, and
  after staging. An untracked file has an empty index version, so every line shows as
  added. Outside a repository there is no state.
- **Diffs:** `Him.Diff` is Myers' algorithm after trimming the common prefix and suffix.
  A middle that needs more than 1000 edits becomes one hunk. Unstaged hunks are
  index → buffer. Staged hunks are HEAD → index, with their new side moved onto buffer
  lines by `mapLine` through the unstaged hunks. One `GitDiff` job runs per document at
  a time; when it answers for an older version, the next event asks again. On 200k
  lines a diff costs 5–53 ms, on a background thread (`bench/DiffBench.hs`).
- **Gutter:** a one-column sign lane in front of the line numbers. Added and changed
  lines get `▎`, a removal gets `▁` on the line above; staged signs are dimmer, and
  unstaged ones win.
- **Staging is one pure function.** `applySelected old new hunks selected` applies the
  selected changes. In a changed hunk, old and new lines are paired by position, so a
  single line can be taken. The extra new lines count as additions, and the extra old
  lines as a removal attached to the hunk's last line. With it:
  - staging is applying index → buffer;
  - unstaging is applying the *unselected* HEAD → index changes, which reverts the
    selected ones;
  - resetting is the same on the buffer, as one undoable change.
  The new index version is written with `git hash-object -w --stdin --path=` and
  `git update-index --cacheinfo`. Staging writes what the buffer shows, even unsaved,
  as `git add -p` does with the work tree.

*Alternatives:* parsing `git diff` output. That only sees saved files, and a patch for
partial hunks is fragile; computing the diff ourselves sees every keystroke. Linking
libgit2 is not a boot library.
