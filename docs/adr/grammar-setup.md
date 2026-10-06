# Grammars set up by him itself: a pinned list, fetched and built, queries built in

A new user should get highlighting without installing another editor. Before this,
grammar sources came from Helix (`hx --grammar fetch`) and the queries from Helix's
runtime directory. Now him needs only `git` and a C compiler:

```
him --grammar                      # fetch and build the grammars of the built-in languages
him --grammar fetch [NAME...]      # only clone the sources
him --grammar build [--force] [NAME...]
```

- **The grammar list** (`runtime/grammars.toml`, built into him by `Him.Embedded`,
  read by `Him.GrammarList`): for each grammar its git repository, a pinned revision,
  and the subdirectory for repositories with several grammars. It is Helix's
  `[[grammar]]` list (303 grammars), so the revisions are the ones its queries are
  written for. One table per grammar, because `Him.Toml` has no arrays of tables.
- **Fetching** (`Him.GrammarBuild.fetchGrammars`) does what Helix does: `git init`,
  then one `git fetch --depth 1 origin REV` and a checkout, into
  `RUNTIME/grammars/sources/NAME`. A source already at its revision is skipped. Git
  runs with `GIT_TERMINAL_PROMPT=0`, so a repository that moved fails instead of
  asking for a password (`Him.Process.runProcessEnv`).
- **Building** is the build of ADR [tree-sitter](tree-sitter.md) (`-O2
  -fno-strict-aliasing`), now from the list's subpath instead of guessing it. Each
  `NAME.so` gets a `NAME.rev` with the revision it was built from, so running
  `him --grammar` again only builds what changed (`--force` builds anyway).
- **RUNTIME** is `$HIM_RUNTIME` or `~/.config/him/runtime`. Grammars are only loaded
  from there (`Him.Paths.ownRuntimeDirs`).
- **Queries are built into him.** Every `highlights.scm` from Helix (341 languages,
  about 0.7 MB) is in `runtime/queries/`, embedded at compile time with Template
  Haskell (`Him.Embedded.TH`; `template-haskell` is a boot library). A
  `queries/LANG/highlights.scm` in a runtime directory replaces the built-in one, and
  `; inherits:` is resolved the same way for each inherited language. Helix's runtime
  directories are no longer searched for queries; they still are for themes.
- **Updating** is a maintainer's job: `dev/sync-helix-runtime.py HELIX_CHECKOUT`
  regenerates the list and the queries from a Helix commit (recorded in
  `runtime/HELIX-VERSION`). Users never need Helix.
- **Licences.** The list and the queries are Helix's, MPL-2.0: file-level copyleft,
  so they can live in this BSD-3-Clause project as long as they keep that licence
  (`runtime/queries/LICENSE`) and changes to them stay MPL-2.0. The grammars are not
  redistributed at all: each user's him fetches them from their own repositories,
  under their own licences (mostly MIT; a few are GPL or have no licence, which only
  rules out bundling them).

*Alternatives:* reading Helix's `languages.toml` and runtime at run time (keeps the
dependency); installing the queries as Cabal `data-files` (the path is baked into the
binary, so a binary built in CI and copied elsewhere loses them); prebuilt grammar
bundles (no compiler needed, but every grammar's licence must allow redistribution,
and someone has to build for each platform). That last one can come later, on top of
the same list.
