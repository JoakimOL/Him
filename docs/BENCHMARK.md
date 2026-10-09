# Benchmarks: him vs. Vim vs. Helix

`bench/bench.py` (Python 3, standard library only) runs each editor in a
pseudo-terminal the way a user would. It waits until the editor has started, types a
key script, and measures time, per-key latency, memory, and CPU.

```sh
make bench                                  # builds him, then runs all scenarios
python3 bench/bench.py --runs 3 --lines 50000
python3 bench/bench.py --editors him,vim --scenarios scroll,latency --json out.json
python3 bench/bench.py --help
```

Editors that are not installed are skipped. The script only uses binaries that are
already there and never downloads anything.

## How it works

- **Terminal:** a 120×40 pty with `TERM=xterm-256color`. Vim and Helix query the
  terminal at startup (device attributes, cursor position, colours, DECRQM,
  XTGETTCAP). The harness answers like a real terminal would. Without the answers,
  Vim, for example, waits about 100 ms at exit.
- **Ready:** first the editor has to put the tty into raw mode. Then its output has to
  stay quiet for 100 ms, because editors are still talking to the terminal at that
  point and Helix drops keys typed during that exchange. Only then does the script
  start typing.
- **Pauses:** after `Esc` the script pauses for 150 ms, so `Esc :` isn't read as
  `Alt-:`. Pauses are subtracted from the timings.
- **Memory and CPU:** from `wait4()` (`ru_maxrss`, user + system time) of the editor
  process.
- **Large file:** 200,000 generated lines (14 MB). A `.txt` file, so neither Vim nor
  Helix highlights syntax.
- **Editor configuration:**
  - Vim: `-u NONE -i NONE -N --noplugin -n` (no vimrc, plugins, viminfo or swap file)
    and `ttimeoutlen=10`.
  - Helix: an empty config, except that auto-completion and auto-pairs are off, so the
    typed text reaches the file verbatim.
  - him: as built by `stack build` (`-O1`).
- **Repetitions:** each scenario runs `--runs` times (default 5), and the median is
  reported.

Configurations: `--editors` takes `him`, `him-lite`, `vim` and `helix` (the
default). `him-lite` has every plugin off. `him-nogit` and `him-nolsp` switch off one
plugin each. The him variants run with their own config files, never the user's.
`--ext rs` gives the large file a code extension, so highlighting and language servers
start. `--git` commits it in a git repository, so git signs have work to do.

## Scenarios

| Scenario | What it does | Main metrics |
|---|---|---|
| `startup_empty` | Starts with no file. | `settled_ms` (last output before the screen goes quiet), `max_rss_mb` |
| `open_large` | Opens the large file. | `first_paint_ms` (line 1 appears in the output), `settled_ms`, `max_rss_mb` |
| `scroll` | Sends `j` × 2000 all at once, then `:q!`. | `work_ms` (from the first key to exit), `cpu_ms` |
| `jump` | Goes to the last line and back (`ge`/`gg`; Vim `G`/`gg`) × 100, then `:q!`. | `work_ms`, `cpu_ms` |
| `edit_save` | Types 880 characters at the top in insert mode, then `Esc` and `:wq`. The saved file is checked byte for byte. | `work_ms`, `max_rss_mb`, `correct` |
| `latency` | Presses `j` 100 times, then types `x` 100 times in insert mode, one key at a time. | Median and p95 of `frame_done` (from writing the key to the last byte of the redraw). `first_byte` is in the JSON. |
| `search_far` | From the top, `/0199999 lorem` + Enter, 10 times. The only match is on the last line. The run ends by searching once more and typing `HIT` before the match, then the saved file is checked. | `search_far_ms` (from writing the keys until drawing is done), `found` |
| `search_none` | From the top, `/zzznotfound` + Enter, 10 times, so the whole file is scanned and the search wraps. | `search_none_ms` |
| `search_next` | `/000 lorem` (a match every 1000 lines), then `n` 100 times one at a time, then `n` × 200 at once. | `next_ms` (median, p95), `next_x200_burst_ms` |

## Caveats

- **Typeahead:** Vim and him handle all queued keys before drawing (Helix partly), so
  `scroll` and `jump` reward that batching.
- **Latency limits:** `frame_done` is measured to the last byte written, with a 20 ms
  quiet threshold. It does not include the terminal emulator's own drawing.
- **Fairness:**
  - Helix has more features loaded (tree-sitter runtime, language config). Vim runs
    without any configuration.
  - him has far fewer features than either, so faster startup is expected, not an
    achievement.
- **CPU frequency:** this machine runs the `powersave` governor, and idle cores sit at
  800 MHz. A key that arrives after a pause starts on a slow core. As a result, a 1.4 ms
  search measured in a tight loop takes about 6 ms inside the editor. That affects every
  editor equally, but it means absolute latencies are higher than on a `performance`
  governor.
- **Search semantics:** all three editors search for literal text here. Helix and him
  use smart case (a lower-case pattern is case-insensitive). Vim runs with
  `ignorecase` off, so it is case-sensitive, which is slightly less work. Helix also
  searches incrementally on every keystroke. him also searches incrementally, but only
  once per batch of keys.
- **Vim's ruler:** Vim is started with `ruler`. It no longer serves the `found` check,
  which now edits and saves the file, but it is kept so all three show a position.
- **Variance:** results vary between machines and runs. Compare runs made on the same
  machine.

## Results

Commit `4e7df7b`, 2026-10-09. Machine: 16 cores, Linux 6.6, idle (load below 1).
Vim 9.2, Helix 25.07.1, him built with `stack build`. 5 runs, median. `him` has every
plugin on (git, LSP, REPL), `him-lite` every plugin off. Best per row in bold.

**Plain text** (200,000 lines, 14 MB, not in a git repository):

| Scenario / metric | him | him-lite | vim | helix |
|---|---:|---:|---:|---:|
| startup_empty: settled ms | 14.9 | **13.1** | 29.1 | 33.3 |
| startup_empty: max RSS MB | 19.1 | **17.1** | 21.8 | 24.4 |
| open_large: first paint ms | 35.5 | 31.6 | 39.7 | **23.8** |
| open_large: max RSS MB | **31.9** | 33.2 | 37.4 | 47.2 |
| scroll (2000 × `j`): work ms | **20.9** | 24.0 | 62.3 | 634 |
| jump (100 × `ge gg`): work ms | 12.6 | **7.7** | 35.6 | 114 |
| edit_save: work ms | 16.2 | **11.8** | 23.4 | 20.1 |
| edit_save: max RSS MB | **32.9** | 34.3 | 37.1 | 67.3 |
| latency `j`: frame done ms (p95) | 1.2 (1.9) | 1.3 (1.8) | **0.5 (0.7)** | 2.0 (2.6) |
| latency typing: frame done ms (p95) | 1.2 (1.5) | 1.2 (1.6) | **0.5 (0.7)** | 1.7 (2.1) |
| search_far: ms | 11.5 | **10.6** | 29.6 | 24.6 |
| search_none: ms | 5.3 | **5.2** | 24.3 | 45.5 |
| search_next `n`: ms (p95) | 2.9 (3.7) | 2.9 (3.9) | **1.6 (2.1)** | 2.5 (3.2) |
| search_next 200 × `n`: ms | 17.1 | **9.7** | 48.5 | 83.9 |

**Code, IDE-style** (`--ext rs --git --lines 20000`: 1.4 MB of Rust-looking text,
committed in a git repository; tree-sitter highlighting in him and Helix, git signs in
him, rust-analyzer in him with plugins and in Helix):

| Scenario / metric | him | him-lite | vim | helix |
|---|---:|---:|---:|---:|
| startup_empty: settled ms | **13.0** | 13.2 | 30.2 | 32.7 |
| open_large: first paint ms | 10.6 | **6.7** | 31.7 | 592 |
| open_large: settled ms | 82.2 | 41.0 | **32.0** | 736 |
| open_large: max RSS MB | 37.3 | 29.0 | **23.6** | 45.7 |
| scroll: work ms | 13.8 | **12.4** | 69.2 | 624 |
| jump: work ms | **3.3** | 3.5 | 44.0 | 115 |
| edit_save: work ms | 5.1 | **3.8** | 12.4 | 494 |
| latency `j`: ms (p95) | 1.0 (1.2) | **0.5 (0.7)** | **0.5 (0.7)** | 2.0 (2.5) |
| latency typing: ms (p95) | 0.8 (1.0) | **0.5 (0.6)** | **0.5 (0.6)** | 10.8 (12.3) |
| search_far / search_none: ms | 1.6 / 1.2 | **1.1 / 0.6** | 9.3 / 5.0 | 17.7 / 21.1 |
| search_next `n`: ms | 1.4 | **0.9** | 1.7 | 2.7 |

Notes:
- **Garbage collection in short scenarios.** `jump` and the 200 × `n` burst are a few
  milliseconds of work, so one young-generation collection more or less inside the
  window shows up as several ms. With plugins on, him allocates a little more per key
  and gets that collection; with `+RTS -A64m` him and him-lite measure the same.
- **First paint of a large plain file:** Helix is faster (24 vs 32 ms).
- **Per-key latency:** Vim is about twice as fast (0.5 vs 1.2 ms).
- **Helix** highlights the whole file before its first paint (592 ms here); him
  highlights the view in a job.

## Profiling him

GHC ships profiling libraries, so this needs no downloads. Use a separate work
directory, so normal builds are untouched:

```sh
stack build --profile --work-dir .stack-prof
# Then compile a small driver against the profiled library, e.g. one that calls
# Him.Render.render in a loop, and run it with +RTS -p:
stack --work-dir .stack-prof exec -- ghc -O1 -prof -fprof-auto-top \
  -package-db .stack-prof/install/x86_64-linux-tinfo6/*/9.10.3/pkgdb \
  -package him Driver.hs -o driver && ./driver +RTS -p
```

For memory, no profiling build is needed: `him +RTS -s -RTS` prints residency and GC
totals, and `+RTS -hT -i0.002` writes a heap profile by closure type (`him.hp`).

Lessons from this project's profiling:
- **Measure live data and peak memory separately.** `max_mem_in_use` was twice the live
  data because of GC headroom and garbage chunks, not because of the data itself.
- **`T.breakOnEnd` copies.** In `text` it reverses the input. It doubled peak memory
  while loading until it was replaced with `dropWhileEnd`/`takeWhileEnd`, which return
  slices.
- **Micro-benchmarks can lie.** An early render benchmark drew identical frames and
  never measured a real redraw. Check that the benchmark exercises the path you think it
  does.
