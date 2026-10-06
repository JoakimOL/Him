# An AI chat, with edits approved in the editor

- **A provider interface** (`Him.Chat.ChatProvider`), like the syntax one.
  - `cpSend config request emit` starts a request and returns a cancel action.
  - Events come back through `emit`: text as it streams, then `ChatFinished` (stop
    reason, the assistant message exactly as returned, the tool calls) or
    `ChatFailed`.
  - The runtime keeps one request per chat buffer and posts the events as
    `ChatReply` jobs.
  - Tests use a scripted provider. Nothing in the test suite talks to a model (the
    user tests live models).
- **The Claude API provider** (`Him.Chat.Anthropic`).
  - There is no Haskell SDK and only boot libraries are allowed, so it uses raw HTTP
    through `curl`. The whole request (the key header and the body) goes to curl's
    stdin as a config file: no key in the process list, no temporary file.
  - The request: `claude-opus-5-5`, adaptive thinking at `[chat] effort` (default
    `high`), streaming, `fallbacks: "default"` (beta `server-side-fallback-2026-07-01`),
    `max_tokens` 64000, and `eager_input_streaming` on every tool. Tool inputs
    are therefore parsed and checked here: an invalid one is answered with an
    `INVALID_JSON` error result, and nothing runs.
  - Credentials: `$ANTHROPIC_API_KEY`, `$ANTHROPIC_AUTH_TOKEN`, or
    `ant auth print-credentials --access-token` (Bearer plus the OAuth beta header).
  - The event stream is parsed purely (`streamStep` / `streamEnd`). Each block is
    rebuilt from its start and deltas, thinking signatures included, so the assistant
    message goes back into the history unchanged.
- **The history is append-only:** user messages, assistant messages as returned, tool
  results. Earlier turns are never edited, as preserved thinking requires.
  - A refused or cut-off turn gets error results for its tool calls, and they never
    run.
- **Tools** (`Him.Chat.Tools`):
  - `read_file` (the buffer's text if the file is open) and `list_files` run at once.
  - `edit_file` (one exact occurrence of `old_text`) and `write_file` become **pending
    edits**. Each is applied to the file's buffer in the editor window as an undoable
    change of whole lines, highlighted with `ui.highlight`, and summarized as a diff in
    the chat.
  - Paths must stay inside the project.
- **Deciding edits:** `space c a` / `space c d` approve or deny the next edit, and
  `A` / `D` do all of them.
  - Approving keeps the edit and saves the file.
  - Denying puts the old lines back.
  - When every edit of a turn is decided, the tool results go back in the order of the
    calls, and the model continues.
- **The chat buffer** is a transcript like the REPL's (`Him.Transcript`, shared since
  then): `ret` sends, `A-ret` makes a line break, and `C-c` stops the answer. Each
  message is prefixed with the file and line the user is looking at.
- **Transcripts never count as unsaved** (`Document.unsaved`), so `:q` does not refuse
  over a REPL or chat buffer.
- **`Runtime` now takes the `Config`** (`newRuntime config post`, `reconfigure`), instead
  of one argument and one setter per table.
