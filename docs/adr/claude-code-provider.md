# Claude Code as a chat provider, with him's tools over MCP

`[chat] provider = "claude-code"` is the default. It uses the `claude` program you are
logged in to, so no API key is needed, and it keeps the approve-before-writing design
of [ADR ai-chat](ai-chat.md).

- **Sessions.** Providers now start a session per chat buffer (`cpStart` →
  `ChatSession { sessSend, sessAnswer, sessClose }`), like syntax providers. The API
  provider's session is stateless. Claude Code's keeps one `claude -p` process with
  streaming JSON in and out.
  - A cancelled turn ends the process, and the next turn uses `--resume <session id>`.
  - A conversation that starts over (`:chat-new`) starts a new session.
- **Tools.** Claude Code's own editing and command tools are off: `--tools Grep,Glob`
  (read-only search) and `--permission-mode dontAsk` (anything not allowed is denied,
  never asked). Its file tools are him's, served over MCP:
  `--mcp-config` names `him --mcp-bridge DIR`, and `--strict-mcp-config` ignores the
  user's other MCP servers.
- **The bridge.** Claude Code starts MCP servers itself, over stdio, so the server can't
  be the running editor. `Him.Mcp` is a small bridge that speaks MCP (newline-delimited
  JSON-RPC: `initialize`, `ping`, `tools/list`, `tools/call`) and forwards each tool
  call to the editor. `mcpStep` is the pure part.
- **Bridge ↔ editor.** Two named pipes in a temporary directory: `calls` and `answers`,
  one JSON object per line. The boot libraries have no sockets, and `unix` has named
  pipes.
  - GHC opens files non-blocking. On a named pipe, that makes a write-open fail with
    no reader, and a read-open see end-of-file at once.
  - So the editor opens both pipes **read-write** (Linux allows it; it never blocks),
    before Claude Code starts. The bridge's opens always find this end, and a bridge
    restart is invisible to the editor.
  - Call ids carry the bridge's process id, so an answer left behind by a bridge that
    died is ignored by the next.
- **Live tool calls.** A call arrives during the turn as `ChatToolCall`.
  - Reads are answered at once.
  - An edit becomes a pending edit ([ADR ai-chat](ai-chat.md)) and is answered when you approve or deny
    it. Claude Code waits meanwhile and goes on by itself afterwards, in the same turn.
  - The batch path of the API provider (tool calls with the finished reply, results
    with the next request) is unchanged. Both share `runCall`.
- **Output.** Text streams from `stream_event` lines (`--include-partial-messages`). The
  use of its own tools shows as `[Grep]`, and the `result` line ends the turn (or
  fails it).
- **Not tried against the live service** (the user tests live models). The tests cover:
  - Claude Code's output shapes;
  - the MCP messages;
  - a real round trip, client → bridge → named pipes → editor → answer;
  - the live approve flow with a fake session that waits for answers like Claude Code.

  `him --mcp-bridge` was also checked by hand, with a scripted MCP handshake.
- **The first live run (by the user) found three faults.** Claude Code's session
  transcripts (`~/.claude/projects/…/*.jsonl`) showed that it did call
  `mcp__him__edit_file`; the faults were on him's side:
  1. **Every message ended the `claude` process.** The runtime cancelled the previous
     turn before each send, and for this provider cancelling means ending the process.
     Each message was then a new process resuming the session, which looks "one-shot".
     Only `ChatCancel` cancels now.
  2. **Approving did not work while typing.** After `ret` the chat is in insert mode,
     where `space c a` types text. When edits arrive, the chat now leaves insert mode.
  3. **An unanswered edit stayed in the buffer** once its turn died, and `read_file`
     (which reads buffers) showed it to the next turn as if it had been made. A turn
     that fails or is cancelled now undoes its undecided edits.
- **Checking it without a model.** `dev/fake-claude` stands in for `claude` with no
  model behind it. It starts the bridge from `--mcp-config`, calls `edit_file`, waits
  for the answer, and reports. With it, the real binary was driven in tmux through
  the whole flow: the edit is shown, approved straight after sending, saved only then,
  and a second message goes to the same process.

*Later:* edits no longer wait for approval; they are reviewed after the turn ([ADR change-review](change-review.md)). Its own tools show as `◦ Searched the code` and the like ([ADR chat-panel](chat-panel.md)).
