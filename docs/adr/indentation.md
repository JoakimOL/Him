# One level of indentation per document: project, text, language, settings

`<` and `>` (and `tab` in insert mode) indent by one level, and a level differs between
projects and languages. `[editor] tab-width` / `expand-tab` alone would indent a
Makefile with spaces and a Go file with four spaces, or force the user to keep
changing them.

- **Where a level comes from**, the first that says wins, part by part (style, then
  width; `Him.Indent.IndentHint`):
  1. the project's `.editorconfig` files (`indent_style`, `indent_size`, `tab_width`;
     nearer files and later sections win; `root = true` stops the search upward;
     `Him.EditorConfig`);
  2. the file's own text (`detectIndent`): tabs if more lines start with a tab than
     with spaces, else the most common step between consecutive lines' indentation;
  3. the language's custom (`langIndent` in `Him.Language`: tabs for Go and Make, 4
     spaces for Python and Rust, 2 for Haskell, …);
  4. the settings `tab-width` and `expand-tab`.
- **When:** sources 1 to 3 are read when a file is loaded (`Him.File`) and kept on
  the document (`docIndent`); the settings are applied when a key needs a level, so
  `:config-reload` takes effect at once. `:reload` keeps the document's hint.
- **What `<` / `>` do:** whole lines the selection touches, a count for several
  levels. `>` leaves blank lines alone (no trailing whitespace); `<` takes off a tab,
  or up to one level of spaces, per level.
- **`.editorconfig` globs:** `*`, `**`, `?`, `[...]`, `[!...]` and `{a,b}`; a glob
  without `/` matches a file name in any directory. Numeric ranges (`{1..3}`) are not
  supported, and properties other than the three above are not read.

*Alternatives:* only detecting from the text (Helix's approach) misses new and
short files, which is where a project's `.editorconfig` matters most. A setting per
language in the config file is still possible on top of `langIndent`.
