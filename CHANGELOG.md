# Changelog for `him`

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/),
and this project adheres to the
[Haskell Package Versioning Policy](https://pvp.haskell.org/).

## Unreleased

- Case: `g u` lower case, `g U` upper case, `g s` swaps (`to_lower`, `to_upper`, `swap_case`), on every selection.
- The picker's preview is highlighted.
- Plugin API (version 2): highlights over a buffer's text, keymaps of a plugin's own for its buffers, a canvas in the middle of the screen with its own keys, and timers ([ADR plugin-canvas](docs/adr/plugin-canvas.md)).
- Contrib plugins `magit` (`space g g`: a magit-like status buffer to stage, unstage and commit files, hunks and selected lines) and `tetris` (`:tetris`).
- `X` selects the whole lines a selection touches. `x` in select mode does too, instead of keeping the old anchor.
- `y` in select mode goes back to normal mode.
- `o` in select mode puts the cursor at the other end of the selection, to extend it from there (`flip_selections`).
- `command_mode_with <text>` opens the `:` line with text typed.
- Keys that need AltGr or dead keys on European layouts moved: next / previous are in their menus (`space g n` / `p` for git changes, `space c n` / `p` for proposed changes, `space c N` a new chat), and `] g`, `[ g`, `] d`, `[ d`, `] c`, `[ c` are gone. Directory views moved to `space -` (the file's) and `space .` (the working directory).
- A `space d` diagnostics menu: `d` this file's, `D` the workspace's (new), `n` / `p` next / previous, `f` / `l` first / last.
- Letters for pairs in match mode: `b` (), `B` {}, `r` [], `c` <>, `q` backticks (`m i B`, `m s r`, …).
- `u` goes up in a directory listing.
- Texts that name keys (the review header, the chat's hints, messages, picker titles, the listing header, magit's help line, tetris's panel) show the keys as bound, so they follow your config. Plugins get `keyFor` / `keyInKeymap`.
- `him --grammar` sets up highlighting with no other editor installed: it fetches the tree-sitter grammars (pinned revisions) with `git` and builds them; the highlight queries are built into him. `him --build-grammars` and the Helix runtime for queries are gone.
- Commit messages are highlighted (the language asked for a grammar named `git-commit`; it is `gitcommit`).
- `r` + a character replaces every selected character with it (line breaks stay; `r ret` splits), as in Helix.
- `O` opens a line above, with the line's indentation (as `o` does below).
- The info box lists the keys `m i`, `m a`, `m s`, `m d` and `m r` wait for (text objects and pairs).
- Personal builds: `him --rebuild` builds a him with the plugins in `~/.config/him/plugins.toml`, and the released him starts it. `templates/him-config` builds one in GitHub Actions, with no toolchain needed.
- Plugin API (`Him.Plugin`): plugins see buffers and events, run programs, and show status line segments, gutter signs, annotations, pickers and scratch buffers. Settings go under `[plugins.<name>]`.
- Contrib plugins, off until switched on: `wordcount` (words in the status line) and `recent-files` (`space o`, files opened lately).
- `:plugins` is a picker: `ret` switches the chosen plugins on or off.
- The git branch shows in the status line.
- Registers and the system clipboard, as in Vim/Helix: `" a y` / `" a p`, `+` (clipboard) and `*` (primary selection), `space y` / `space p` / `space P` / `space R`, `R` replaces the selection with a register, `_` discards, `C-r` inserts one in insert mode; `:registers`, `:clear-register`; `[editor] clipboard-provider`.
- The `:` line: `tab` / `S-tab` cycle the completions; `:theme <name>` previews the theme as you type (`esc` goes back).
- Pickers: `tab` marks items, `ret` acts on all marked (the file picker opens them all), `del` is the picker's second action (the jumplist removes all marked).
- Jumplist, as in Helix: `C-o` / `C-i` (`tab`) go back and forward, `C-s` saves the selection, `space j` lists the jumps (`del` removes one); jumps follow edits.
- Global search (`space /`): search the project's files as you type, with hits streaming into a picker with a preview.
- The chat looks and works like VS Code's: message blocks, an input box with history, wrapped answers, code blocks (`space c y` copies one), tool and change lines, a review summary; changes are kept or discarded.
- Insert-mode chords such as `"j j" = "normal_mode"`: a first key the chord doesn't continue from is typed as usual.
- REPL and chat buffers are transcripts: only the input can change.
- Claude Code chat provider (the default): him's tools served over MCP (`him --mcp-bridge`), no API key needed.
- AI chat plugin: a chat beside the code (Claude API over curl); the model's edits are applied as pending edits and approved or denied in the editor.
- Match mode (`m m`, `m s`, `m r`, `m d`, `m i`, `m a`); `I` and `A`.
- REPL plugin: `:repl`, send the selection (`space e`), reload on save.
- Splits (`C-w` / `space w`, `:vsplit`, `:hsplit`).
- Plugins (git, lsp, repl, chat) that can be switched off in the config or at run time.
- Settings in `[editor]` (tab width, expand-tab, relative numbers, cursor shapes, search, pickers, completion).
- Themes: Helix theme files, `:theme`, 256-colour fallback.
- Config file (`config.toml`, `--dump-default-config`, `:config-open`, `:config-reload`).
- LSP client: diagnostics, hover, go to, references, completion with imports, signature help, rename, format, code actions, symbols.
- Tree-sitter highlighting (`--build-grammars`); git signs and line staging.
- Pickers with previews, command palette, directory listings, info menus, buffers, multiple selections, `f t`, pages, counts, suspend, reload.

## 0.1.0.0 - unreleased

- Rope buffer, streaming load/save, non-moving GC; search (/ ? n N *) with rare-byte SIMD scanning; row reuse and terminal scrolling in the renderer.
- Undo/redo, yank/paste, line-number gutter, wide-character and control-character display.
- Core editor: file loading/saving, rendering with frame diffing, Helix-style selections and motions, command registry + keymap tries, insert mode, `:` commands.

- Project scaffold: tooling, FFI terminal-size shim, test harness, docs/PLAN.md.
