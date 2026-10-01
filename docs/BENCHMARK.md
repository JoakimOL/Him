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

## Scenarios

| Scenario | What it does | Main metrics |
|---|---|---|
| `startup_empty` | Starts with no file. | `settled_ms` (last output before the screen goes quiet), `max_rss_mb` |
| `open_large` | Opens the large file. | `first_paint_ms` (line 1 appears in the output), `settled_ms`, `max_rss_mb` |
| `scroll` | Sends `j` × 2000 all at once, then `:q!`. | `work_ms` (from the first key to exit), `cpu_ms` |
| `jump` | Goes to the last line and back (`ge`/`gg`; Vim `G`/`gg`) × 100, then `:q!`. | `work_ms`, `cpu_ms` |
| `edit_save` | Types 880 characters at the top in insert mode, then `Esc` and `:wq`. The saved file is checked byte for byte. | `work_ms`, `max_rss_mb`, `correct` |
| `latency` | Presses `j` 100 times, then types `x` 100 times in insert mode, one key at a time. | Median and p95 of `frame_done` (from writing the key to the last byte of the redraw). `first_byte` is in the JSON. |

## Caveats

- **Typeahead:** Vim (and partly Helix) skips redraws while more keys are queued, so
  `scroll` and `jump` reward that batching. him currently renders after every key.
- **Latency limits:** `frame_done` is measured to the last byte written, with a 20 ms
  quiet threshold. It does not include the terminal emulator's own drawing.
- **Fairness:**
  - Helix has more features loaded (tree-sitter runtime, language config). Vim runs
    without any configuration.
  - him has far fewer features than either, so faster startup is expected, not an
    achievement.
- **Variance:** results vary between machines and runs. Compare runs made on the same
  machine.

## Results

Newest first. Machine: 16 cores, Linux 6.6, Vim 9.2, Helix 25.07.1. 200,000 lines,
5 runs, median. Vim/Helix numbers vary by a few ms between runs, so compare within
one run.

### 2026-10-01: after the first performance pass

Changes since the baseline:
1. **Batching:** queued events are handled before rendering once (`TChan`, up to 512
   per frame).
2. **Row writes:** frame rows are written with one splice instead of cell by cell.
3. **Text area:** each visible line is built as one cell list.
4. **Selection:** the selected columns are computed per line, not per character.
5. **Character widths:** `isWide` has an ASCII fast path and an `IntMap` lookup.

`render` alone (a micro-benchmark, 40×120): 1.43 → 0.54 ms per frame. `diff`: 0.32 →
0.13 ms.

| Scenario / metric | him | vim | helix |
|---|---:|---:|---:|
| startup_empty: settled ms | **6.1** | 24.0 | 21.2 |
| startup_empty: max RSS MB | **14.3** | 21.8 | 24.3 |
| open_large: first paint ms | 50.2 | 24.3 | **21.9** |
| open_large: max RSS MB | 40.4 | **37.1** | 45.0 |
| scroll (2000 × `j`): work ms | **19.5** | 69.1 | 673 |
| scroll: CPU ms | **68.1** | 92.8 | 1213 |
| jump (100 × `ge gg`): work ms | **6.7** | 39.0 | 124 |
| edit_save (880 chars): work ms | 49.5 | 33.4 | **18.2** |
| edit_save: max RSS MB | 86.6 | **37.0** | 66.8 |
| latency `j`: frame done ms (p95) | 1.9 (3.0) | **0.3 (0.4)** | 1.3 (1.8) |
| latency typing: frame done ms (p95) | 1.6 (2.4) | **0.3 (0.4)** | 1.2 (1.5) |

What's left, by impact:

- **Opening large files** (50 ms vs. about 22 ms). The cost is decoding 14 MB,
  `T.splitOn`, and building a `Seq` of 200,000 `Text`s. Options: split on the
  `ByteString` and decode per line lazily, or move to a rope (ADR-3).
- **Memory in `edit_save`** (87 MB vs. 37 MB). This is mostly GC overhead on top of
  about 30 MB of live lines. The copying collector needs about twice the live data;
  try the compacting/non-moving GC, or a more compact line representation.
  (`-A16m`/`-A64m` were tried: latency is about the same, memory is worse.)
- **Per-key latency** (about 1.9 ms vs. Vim's 0.3 ms). `render` (0.54 ms) is still
  dominated by `layoutLine` allocation and building `Cell`s for every row on every frame.
  Next steps: cache rendered rows of unchanged lines, or render only rows whose line
  or selection changed. Write cost (about 0.5–1 ms, including the pty) could drop by
  emitting shorter SGR sequences.
- **Typing throughput** (`edit_save`): each typed key still runs `edit` plus the undo
  bookkeeping, but rendering is batched now. Profile before changing anything.

### 2026-10-01: baseline (`db992ae`)

| Scenario / metric | him | vim | helix |
|---|---:|---:|---:|
| startup_empty: settled ms | **6.3** | 23.0 | 20.8 |
| startup_empty: max RSS MB | **14.4** | 21.5 | 22.8 |
| open_large: first paint ms | 42.0 | 40.3 | **22.7** |
| open_large: max RSS MB | 38.6 | **37.1** | 44.8 |
| scroll (2000 × `j`): work ms | 2809 | **60.7** | 666 |
| scroll: CPU ms | 2850 | **85.4** | 1185 |
| jump (100 × `ge gg`): work ms | 505 | **41.2** | 114 |
| edit_save (880 chars): work ms | 495 | 24.4 | **19.0** |
| edit_save: max RSS MB | 88.5 | **37.1** | 66.8 |
| latency `j`: frame done ms (p95) | 4.6 (6.5) | **0.3 (0.5)** | 1.4 (2.3) |
| latency typing: frame done ms (p95) | 4.7 (6.1) | **0.3 (0.4)** | 1.2 (1.9) |

At the baseline, him rendered after every key, and frames were built one cell at a time
with `Seq.update`.

## Profiling him

GHC ships profiling libraries, so this needs no downloads. Use a separate work
directory, so normal builds are untouched:

```sh
stack build --profile --work-dir .stack-prof
# Then compile a small driver against the profiled library, e.g. one that calls
# Him.Render.render in a loop, and run it with +RTS -p.
```
