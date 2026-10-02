# Changelog for `him`

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/),
and this project adheres to the
[Haskell Package Versioning Policy](https://pvp.haskell.org/).

## Unreleased

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
