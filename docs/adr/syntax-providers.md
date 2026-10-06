# One syntax-highlighting interface, with injected providers

- **The interface:** the editor knows only `Him.Syntax`. A `SyntaxProvider` has
  `spStart :: Language -> IO (Maybe SyntaxSession)`, and a session has `ssUpdate`
  (version, buffer, edits if known), `ssHighlight` (spans for a range of lines) and
  `ssClose`.
- **Spans:** spans are per line, carrying dotted scope names
  (`keyword.control.import`), which both tree-sitter captures and TextMate scopes use.
  The theme resolves a scope by its longest known prefix (`scopeStyle`).
- **Configuration:** providers are listed in `cfgSyntaxProviders` and tried in order.
  `Him.Language` detects the language (file name, extension, shebang) and maps it to
  each provider's grammar name.
- **Jobs:** sessions live in the runtime. A `SyntaxStart` job picks the provider; then
  `Highlight` jobs, at most one per document in flight, return spans for the view plus
  100 lines either side. The document keeps the last spans (`docSyntax`), so typing
  shows at most one burst of staleness. The row key includes the spans.
- **Adding a provider** (e.g. TextMate) means one module that builds the record, plus
  one entry in `syntaxProviders` in `Him.Config.Default`. Nothing else changes. Tests
  inject a fake provider, which proves the editor works against the interface alone.
