# Your own him

A him with the plugins you choose, built by GitHub Actions: no Haskell
toolchain needed on your machine.

1. Use this repository as a template (or fork it).
2. List your plugins in `plugins.toml` and push. The workflow builds him with
   them and publishes the binaries as a release of your repository.
3. Download the binary for your system to `~/.local/state/him/bin/him`
   (`$XDG_STATE_HOME/him/bin/him`) and make it executable. The released `him`
   starts it from then on.
4. After updating him, push again (or run the workflow by hand): a personal
   build older than the installed him is not started, and him says so.

With GHC and stack installed, `him --rebuild` does the same on your machine,
reading `~/.config/him/plugins.toml`.

A plugin is a Haskell package that depends on `him` and exports a
`PluginSpec` (see `Him.Plugin`, and `src/Him/Contrib/` in him for examples).
A plugin of your own can live in this repository: put it in
`plugins/my-plugin/` and list it with `path = "plugins/my-plugin"`.
