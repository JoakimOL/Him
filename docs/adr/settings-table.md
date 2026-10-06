# Settings are one table

Every setting is an `OptionSpec` in `Him.Options.optionSpecs`, holding:
- its key (`tab-width`, or `search.smart-case` for `[editor.search]`);
- its doc;
- a setter that checks the value's type and range;
- a printer for the default.

Three things use that one table: checking the config file (`setOption`; an unknown key
lists the known keys of its table), applying it (`userOptions`), and the
`--dump-default-config` section. They cannot drift apart. The values live in
`edOptions :: Options` on the editor (which replaced `edScrolloff` and `edShowHidden`),
so actions and render components read them like any state. The settings are:
- `[editor]`: `scrolloff`, `show-hidden-files`, `tab-width`, `expand-tab`,
  `line-number` (absolute / relative / off), `escape-timeout` (read at startup only);
- `[editor.cursor-shape]`: normal, insert, select, command;
- `[editor.lsp]`: `auto-completion`, `completion-trigger-len`, `auto-signature-help`,
  `hover-lines`;
- `[editor.search]`: `smart-case`, `wrap-around`;
- `[editor.file-picker]`: `hidden`, `git-ignore`, `ignore`, `follow-symlinks`,
  `max-files`;
- `[editor.picker]`: `preview`, `preview-min-width`, `preview-max-size`.

Names follow Helix where it has the same setting. Pure code that needed a value now
takes it as a parameter: `layoutLine`/`displayCol`/`charIndexAtCol` (tab width),
`lineBy` (tab width), `compileNeedle` (smart case), `findMatch` (wrap), and
`walkFiles` (a `WalkOptions` carried by the `ScanFiles` job).

Still constants, on purpose: undo levels, gutter glyphs, the language table, the LSP
start timeout, and the internal tuning values (`maxBatch`, `chunkSize`, `mergeGap`,
`syncLimit`, `matchLimit`, `maxEdits`, the scan batching, `highlightMargin`).
