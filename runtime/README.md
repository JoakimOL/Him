# runtime/

What him builds into its binary for syntax highlighting (`docs/adr/grammar-setup.md`).

- `grammars.toml`: the tree-sitter grammars `him --grammar` can fetch and build, each
  with its repository and the pinned revision.
- `queries/*/highlights.scm`: the highlight query of each language.

Both come from the [Helix](https://github.com/helix-editor/helix) editor: the
`[[grammar]]` entries of its `languages.toml` and its `runtime/queries`, at the commit
in `HELIX-VERSION`. They are the Helix project's work, under the Mozilla Public
License 2.0 (`queries/LICENSE`), and stay under it here: changes to these files are
MPL-2.0 as well. The rest of him is BSD-3-Clause.

The grammars themselves are not in this repository: each one is fetched from its own
repository, under its own licence.

Refresh with `dev/sync-helix-runtime.py HELIX_CHECKOUT`; do not edit the files by hand.
