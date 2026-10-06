# Personal builds, as in xmonad: `himMain`, `him --rebuild`, and a template repository

This is phase 4 of `docs/PLUGIN-API.md`, for plugins outside the contrib collection.
- **The program is a library function.** `Him.Main.himMain :: [Plugin] -> IO ()` is
  the whole command line, and `app/Main.hs` is `himMain []`. The list of every plugin
  is threaded through:
  - `cfgAllPlugins`;
  - `configWithPlugins`, `parseUserConfigWith`, `applyUserConfigIn` and
    `defaultConfigTextFor`;
  - `Session.loadConfigWith` and `App.runWith`.

  So `[plugins]`, `:plugins`, `:plugin-enable` and the dumped config know a personal
  build's own plugins. Two plugins with the same name stop `himMain`.
- **`him --rebuild`** (`Him.Rebuild`) reads `~/.config/him/plugins.toml`:
  - `[him]` says where him's source is: git and ref, or a path. The default is
    `github.com/JoakimOL/Him` at `v<version>`.
  - Each `[plugins.<name>]` gives git and ref (or a path), `package`, `module` and
    `spec`.

  It writes a stack project in `<state dir>/build` on him's own snapshot (`himSnapshot`;
  a test keeps it equal to `stack.yaml`). The project is a `.cabal` file and a
  `Main.hs` of `himMain [hostPlugin M.spec, …]`. It runs `stack build`, which copies
  the result to `<state dir>/bin/him`. An empty list builds nothing.
- **The released him starts a personal build:** when `<state dir>/bin/him` exists, is
  not itself, and is not older, it `exec`s it. An older one (built before an update)
  is not started, and the editor says so at startup.
- **Template repository** (`templates/him-config/`): `plugins.toml`, a README, and a
  GitHub Actions workflow. The workflow builds him at the repository variable
  `HIM_REF` (or the latest tag), runs `him --rebuild` in CI and publishes the binary
  as a release. The user downloads it to `<state dir>/bin/him`. There is no
  `him --update` yet.
- **Not tried:** the build itself. Running it fetches him and plugins from the
  network, which needs the user's go-ahead. The tests cover the list, the generated
  files and the snapshot.
- **Needs from the user:** release tags `v<version>`; none exist yet, and the default
  `ref` and the workflow assume them. The repository must also be reachable (public)
  for CI and for git-based personal builds.

*Alternatives:* loading `.so` files or running plugins as separate programs (ruled
out); a global plugin registry set at startup (hidden state that tests could see
half-set); the release including every plugin there is (that is contrib, and it needs
review).
