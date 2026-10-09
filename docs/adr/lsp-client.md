# The LSP client: processes in the runtime, protocol as pure data

- **Pure protocol:** `Him.Lsp.Protocol` covers Content-Length framing (tested at every
  split point), file URIs, columns in UTF-8/16/32, building and classifying JSON-RPC
  messages, and reading diagnostics, locations, hover contents and completion items
  (snippets reduced to plain text).
- **Processes:** `Him.Lsp.Server` runs one server process with:
  - a reader thread that splits its output into messages;
  - a writer thread with a queue, so the main loop never blocks on a busy server;
  - a drain for stderr;
  - automatic answers to the server's own requests (configuration, progress,
    registration);
  - the initialize handshake, which advertises UTF-8 positions.

  The runtime starts one server per (command, project root) on demand (`LspEnsure`).
  Roots come from markers in `Him.Lsp.Config`. The runtime forwards `LspSend` effects
  and stops the servers on quit.
- **Editor side:** the state is pure (`Him.Lsp.State`):
  - documents attach to a server (`docLsp`);
  - requests are remembered as `Pending` values by id, so a reply is applied by a pure
    function;
  - diagnostics are kept by absolute path, and converted to character columns against
    the current text when drawn.
- **Sync** (`Him.Lsp.Sync`): it runs once per input batch, just before
  drawing (`lspFlush`), and before every request.
  - **Incremental changes:** `didChange` sends one range edit, computed by
    `Buffer.changeBetween` from the text last sent. Shared storage blocks are skipped
    with a memory comparison, then lines, then characters. A one-character edit in a
    196,000-line buffer is found in under 1 ms, where the whole text was 14 MB per
    batch before. This covers any kind of change, undo included. Servers that ask for
    full sync get the whole text; an undo back to the sent text sends nothing.
  - **The server's copy** is the buffer plus the file's final line break, which edits
    never touch, so buffer positions are valid in it.
  - **Saves and closes:** `didSave` follows a save (`docSaves`), and `didClose` follows
    `:bc`.
- **Server commands:** `:lsp-start`, `:lsp-stop`, `:lsp-restart` and `:lsp-info`.
  `LspStop` removes the server from the runtime before stopping it, and an exiting
  server is only reported if it is still the one registered under its key, so a
  restart cannot be undone by the old server's exit. Stopping drops the server's
  attachments, diagnostics and pending requests; restarting re-attaches its documents.
- **Edits** (`Him.Lsp.Edit`): text edits apply from the last to the first, so
  positions stay valid, and insertions at the same place keep the server's order.
  Workspace edits change open buffers in place; files that are not open are opened and
  left modified, and the current buffer stays current.
- **Features:**
  - diagnostics: a gutter sign over git signs, an underline in the severity's colour,
    the cursor line's message in the bottom row, `space d n` / `space d p`, and `space d d` (now under the `space d` menu, [ADR layout-friendly-keys](layout-friendly-keys.md));
  - `space k` hover, in a popup at the cursor;
  - `g d` definition and `g r` references (one location jumps, several open a picker);
  - completion in insert mode, automatic or on `C-x`, in a `Completing` keymap layer
    over insert mode.
  - since "More LSP" (§5 of `docs/PLAN.md`):
    - `space r` rename (a prompt starting with the word);
    - `:format`;
    - `space a` code actions. The request sends the selected lines' diagnostics back
      raw. Picking one applies its edit, runs its command, or resolves it first.
      Servers that apply edits through `workspace/applyEdit` (clangd's tweaks) are
      handled.
    - signature help after the server's trigger characters (above the cursor, until
      `)`);
    - `space s` document symbols, `g y` type definition, `g i` implementation;
    - jumps convert the server's columns exactly.
  - since "Previews, workspace symbols, imports":
    - `space S` workspace symbols. The picker's source is `ServerQuery`, so each
      change of the query asks the server again; stale answers are dropped.
    - completion imports. Items' `additionalTextEdits` are applied with the insertion,
      moving the cursor down with lines added above it. Items without them are
      resolved (`completionItem/resolve`, advertised through `resolveSupport`), and the
      edits are applied if the text is unchanged.
    - references are on `g r`.
- **Tests:** against clangd when it is installed (attach, diagnostics, hover, `g d`,
  fixing an error, completion), plus the pure protocol tests.

*Alternatives:* `ReaderT` handles in actions (rejected in [ADR effects-and-runtime](effects-and-runtime.md)), or blocking request
calls from actions, which would freeze the editor while a server thinks.
