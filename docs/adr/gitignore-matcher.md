# Our own gitignore matcher, applied while walking

`Him.Ignore` parses `.gitignore` / `.ignore` files with git's syntax:
- comments, `!` to re-include, and a trailing `/` for directories only;
- a `/` at the start or in the middle anchors the pattern to the file's directory;
- globs `*`, `?`, `[a-z]` / `[!a-z]`, and `**` (`**/x`, `x/**`, `a/**/b`).

Rule sets are scoped to the directory of their file, and the last match of the most
specific set wins. `.ignore` is read after `.gitignore` in the same directory, so it wins
there. `Him.FileTree.listFiles` reads each directory's files as it descends, and never
enters an ignored directory, so (as in git) nothing inside one can be re-included. It
also applies the ignore files of the walk root's ancestors, up to the enclosing git
repository, and `.git/info/exclude`.
- **Differences from ripgrep/Helix:** `.gitignore` is honoured outside git
  repositories too, and the global gitignore (`core.excludesFile`) is not read.
- **Hidden entries** are still skipped, as Helix does by default.

*Alternative:* translate patterns to a regex engine. There is none in the boot
libraries, and a direct backtracking matcher over path strings is about 40 lines.
