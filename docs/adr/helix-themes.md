# Themes are Helix theme files

`[editor] theme = "onedark"` in the config, or `:theme <name>` while running (`tab`
completes the name; `:theme` alone names the current one). Why Helix's format:
- **The scope names were already Helix's** ([ADR syntax-providers](syntax-providers.md)), so its themes colour him's
  highlighting as they are. All 219 installed themes load without a warning (checked
  with a throwaway script).
- **Users can switch editors** without redoing their colours, and write their own the
  same way.

How it works:
- **Lookup:** a theme is `<name>.toml` in `themes/` next to the config file, then in each
  runtime directory's `themes/` (`Him.Paths.themeDirs`), and the first match wins.
  `default` is built in (`Him.Theme.defaultThemeText`, written in the same format), but
  a file of that name replaces it. A theme that inherits its own name (a user's tweak
  of a Helix theme) gets the next file of that name.
- **Format (`Him.Theme`, pure):**
  - `inherits`: the child's entries replace the parent's whole, palettes merge by name,
    and the merged palette colours both, as Helix does.
  - Colours: palette names (they may chain), `#rrggbb`, `#rgb`, `"110"` (a palette
    index), the 16 terminal colour names, `default`.
  - Modifiers: bold, dim, italic, underlined, reversed, crossed_out.
  - Underlines: `{ color, style = line|curl|double_line|dotted|dashed }`.
  - Anything not understood is skipped and logged, not fatal (rainbow brackets, blink).
- **Reader:** `Him.Toml` gained inline tables (`{ fg = "red" }`, also spanning lines, as
  some themes write them). It now skips `#` inside literal strings when joining lines.
- **Styles are layered (`patchStyle`, Helix's patch):** text, then syntax, then a
  diagnostic underline, then the selection, then a cursor. A selection that only sets
  a background keeps the text's colour. The primary selection uses
  `ui.selection.primary`.
- **What `Style` holds:** dim, strikethrough, an underline kind and an underline colour
  (SGR `4:3` and `58;2;…`). `PackedStyle` is now a `Word64` plus a `Word32` (the
  underline colour), still unpacked into each cell.
- **UI from scopes (`Him.Render.Theme.fromScopes`):** the render components keep
  their record fields, filled once per theme from Helix's UI scopes:
  - `ui.text`, `ui.selection(.primary)`, `ui.cursor`, `ui.linenr(.selected)`,
    `ui.gutter`, `ui.virtual`;
  - `ui.statusline` and `ui.statusline.normal|insert|select`;
  - `ui.popup`, `ui.menu.selected`, `ui.text.inactive|focus|directory`;
  - `error|warning|info|hint`, `diagnostic.*`, `diff.plus|minus|delta`.
  him's own scopes fall back to those: `ui.statusline.command` and `.picker` (to
  `.normal`), `ui.popup.key`, and `diff.*.staged` (to the same colour, dimmed). Lookups
  fall back by prefix, as Helix's do.
- **Background:** `ui.background` (with `ui.text`'s foreground) becomes the
  terminal's *default* colours through OSC 11/10, carried in `frameColors` and sent
  when they change. Cleared areas, scrolled-in rows and blank cells then show the theme
  without the diff ever writing a background. Leaving or suspending resets them (OSC
  111/110). The alternative, painting every cell, would defeat the blank-tail and
  scroll-region optimizations of [ADR row-reuse-and-scrolling](row-reuse-and-scrolling.md).
- **Older terminals:** without `COLORTERM=truecolor|24bit`, 24-bit colours are mapped
  to the nearest of the 256-colour palette (the cube or the grey ramp) when the theme
  is loaded.
- **Switching:** the theme lives in the main loop beside the config (an `IORef`).
  `ChangeTheme` is an effect the loop carries out, like `ReloadConfig`. The loop's
  effects now run in order with one `foldM`. A switch repaints everything, because
  cached rows hold the old colours. `:config-reload` loads the theme again too.
