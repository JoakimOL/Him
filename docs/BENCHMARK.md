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

### 2026-10-01

Machine: 16 cores, Linux 6.6. Versions: him at `db992ae`, Vim 9.2, Helix 25.07.1.
200,000 lines, 5 runs, median.

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

What this says about him:

- **Startup and loading:** these are already competitive. Startup is fastest, and
  opening 14 MB is on par with Vim.
- **Per-key cost:** this is the bottleneck, at about 1.4 ms of CPU per key in batch
  and about 4.6 ms latency to a finished frame. All three CPU-heavy scenarios
  (`scroll`, `jump`, `edit_save`) are dominated by it. Likely causes, worth profiling
  first:
  1. A full frame is built after every key, and every cell is written with
     `Seq.adjust`/`Seq.update`. That is 4,800 small updates per frame, and more for
     selections and tabs.
  2. Nothing is coalesced. All queued keys could be processed before rendering once
     (drain the `Chan`, then render), which is what Vim does.
  3. The diff compares whole `Seq Cell` rows. Rows could be built as lists or `Text`
     with style runs, and then compared.
- **Memory in `edit_save`:** 88 MB. Undo snapshots and `docSavedBuffer` share
  structure, but every insert creates new `Text` for the edited line plus `Seq` spine
  nodes. Worth checking with `+RTS -s` once the rendering cost is fixed.
