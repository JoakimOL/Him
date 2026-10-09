# No test framework

The tests live in `test/Test/<Area>.hs` (Text, Formats, Config, Git, Lsp, Syntax,
Render, Integration, Repl, PluginApi, …, with helpers in `Test.Util`), and
`test/Spec.hs` runs them.
`test/Test/Harness.hs` is about 50 lines and does `test`, `group`, `assertEqual`, and
`runTests`, which keeps us within the boot libraries. hspec/tasty can be adopted later
if needed.
