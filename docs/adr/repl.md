# A REPL is a buffer with a process behind it (the `repl` plugin)

`:repl` opens the REPL of the file's language in a window beside it.
- **Typing:** in the REPL buffer you type as anywhere, and `ret` in insert mode sends
  the line. This is the `Repl` keymap layer, `[keys.repl]`, which inherits insert
  mode; `C-c` interrupts.
- **From a file:** `space e` sends the selection, or the line when only one character
  is selected. Code of several lines is wrapped in the REPL's markers (ghci's
  `:{ … :}`; a blank line for Python). The focus stays in the file.
- **Reloading:** `space E` reloads the project (`:reload`). It also happens by itself
  after a file of the language is saved, when `reload-on-save` is set (the ghci
  default).
- **Commands:** `:repl-send <text>`, `:repl-reload`, `:repl-interrupt`, `:repl-stop`,
  `:repl-restart`.

How it is built:
- **The transcript (`Him.Repl.Transcript`, pure).** A REPL buffer is a document of
  kind `ReplDoc ReplState`, whose `rsInput` is where the next input starts.
  - Output is inserted just before the input, and cursors at or after that point move
    with it. Output arriving while you type never splits your line.
  - Output is not an edit: no undo step, never dirty.
  - `ret` takes the text after `rsInput` and closes it with a line break.
  - REPLs reading a pipe do not echo, so the editor shows sent code itself, as if
    typed.
  - Escape sequences and carriage returns are removed from output.
- **The process (`Him.Repl.Process`).** stdout and stderr share one pipe. The process
  runs with `TERM=dumb` and in its own process group, so `C-c` interrupts the REPL,
  not the editor. A streaming UTF-8 decoder handles characters split across reads.
  - The pipe's ends are close-on-exec. A language server started at the same time
    inherited the write end, so a REPL's exit was never seen. A test caught this:
    with pylsp starting for `t.py`.
- **Starting.** The runtime starts the REPL as soon as it performs the effect, not as
  a job, so text sent straight after reaches it. It runs in the project root, found
  from `roots` markers (like language servers), so `stack ghci` loads the project.
  The runtime holds the REPL table (`setReplTable` on `:config-reload`) and the
  processes by buffer id.
- **Windows.** If the REPL buffer is not shown, a split opens beside the current
  window. Unfocused windows on a REPL buffer follow its end as output arrives.
- **Highlighting.** The transcript is highlighted as its language.
- **Config:** `[repl.<language>]` sets `command`, `args`, `roots`,
  `multiline = [start, end]` (or `[]`), `reload`, `reload-on-save` and `enabled`.
  Built in:
  - haskell: `stack ghci`, `:{ :}`, `:reload` on save;
  - python: `python3 -i -q -u`;
  - javascript: `node -i`.

*Testing while developing:* point the Haskell REPL at the library and the test suite
(`args = ["ghci", "him:lib", "him:test:him-test"]`). Then `:repl-send main` runs the
suite. Selecting an expression (a test, or a call into the module being written) and
pressing `space e` evaluates it. Saving reloads. See the tutorial, §5.11.
