#!/usr/bin/env python3
"""Benchmark him against vim and helix.

Each editor runs in a pseudo-terminal, like a real terminal session. The
harness waits until the editor has put the terminal into raw mode, types a key
script, and measures:

  * wall-clock time (pauses the script inserts, e.g. after Esc, are subtracted)
  * per-key latency: time from writing a key until the first output byte, and
    until the editor has finished drawing (the last byte before output goes quiet)
  * peak memory (max RSS) and CPU time of the editor process, from wait4()

Only the Python standard library is used. Usage:

    python3 bench/bench.py                       # all editors, defaults
    python3 bench/bench.py --runs 3 --lines 50000
    python3 bench/bench.py --editors him,vim --json results.json

See docs/BENCHMARK.md for what each scenario measures and the caveats.
"""

from __future__ import annotations

import argparse
import fcntl
import json
import os
import pty
import re
import select
import shutil
import signal
import statistics
import struct
import subprocess
import sys
import tempfile
import termios
import time
from dataclasses import dataclass, field
from pathlib import Path

ROWS, COLS = 40, 120
ESC_PAUSE = 0.15  # seconds to wait after Esc, so it isn't read as Alt+<next key>
SETTLE = 0.1  # output must be quiet this long before the editor counts as started
RUN_TIMEOUT = 120.0
MARKER = "BENCHFIRSTLINE"
TYPED = "the quick brown fox jumps over the lazy dog " * 20  # 880 characters

CSI_RE = re.compile(rb"\x1b(\[[0-9;?<=>!]*[ -/]*[@-~]|\][^\x07\x1b]*(\x07|\x1b\\)|[()][0-9A-Za-z]|[=>78DEHMc])")

CUP_RE = re.compile(rb"\x1b\[(\d+);(\d+)H")
DCS_RE = re.compile(rb"\x1bP.*?\x1b\\", re.S)


def cursor_report(output: bytearray, at: int) -> bytes:
    """Answer a cursor position request like a real terminal: the position of
    the last absolute cursor move before the request, plus the characters
    printed since. Vim relies on this for its ambiguous-width and DCS checks
    (with a wrong answer it waits ~100 ms when exiting)."""
    moves = list(CUP_RE.finditer(output, 0, at))
    if not moves:
        return b"\x1b[1;1R"
    row, col = int(moves[-1].group(1)), int(moves[-1].group(2))
    printed = CSI_RE.sub(b"", DCS_RE.sub(b"", bytes(output[moves[-1].end() : at])))
    width = len(printed.decode("utf-8", "replace").replace("\r", "").replace("\n", ""))
    return b"\x1b[%d;%dR" % (row, col + width)


# Terminal queries editors send at startup, and what a terminal would answer.
# Without answers, some editors wait for a timeout before starting or exiting.
# Each reply gets the match, the whole output so far, and the match's offset.
QUERIES = [
    (re.compile(rb"\x1b\[0?c"), lambda m, out, at: b"\x1b[?62;22c"),  # primary device attributes
    (re.compile(rb"\x1b\[>0?c"), lambda m, out, at: b"\x1b[>1;95;0c"),  # secondary device attributes
    (re.compile(rb"\x1b\[6n"), lambda m, out, at: cursor_report(out, at)),
    (re.compile(rb"\x1b\[\?(\d+)\$p"), lambda m, out, at: b"\x1b[?" + m.group(1) + b";0$y"),  # DECRQM: unknown
    (re.compile(rb"\x1b\](1[01]);\?(\x07|\x1b\\)"),
     lambda m, out, at: b"\x1b]" + m.group(1) + b";rgb:0000/0000/0000\x1b\\"),  # colour queries
    (re.compile(rb"\x1bP\+q[0-9a-fA-F;]*\x1b\\"), lambda m, out, at: b"\x1bP0+r\x1b\\"),  # XTGETTCAP: unknown
]


class Pause:
    """A pause inside a key script; its duration is not counted."""

    def __init__(self, seconds: float):
        self.seconds = seconds


@dataclass
class Editor:
    name: str
    cmd: list[str]
    goto_end: bytes
    goto_top: bytes
    env: dict[str, str] = field(default_factory=dict)

    def quit(self) -> list:
        return [b":q!\r"]

    def write_quit(self) -> list:
        return [b":wq\r"]


class Session:
    """One editor process attached to a pseudo-terminal."""

    def __init__(self, argv: list[str], env: dict[str, str]):
        full_env = {**os.environ, "TERM": "xterm-256color", **env}
        full_env.pop("COLORTERM", None)
        pid, fd = pty.fork()
        if pid == 0:  # child
            try:
                fcntl.ioctl(0, termios.TIOCSWINSZ, struct.pack("HHHH", ROWS, COLS, 0, 0))
                os.execvpe(argv[0], argv, full_env)
            finally:
                os._exit(127)
        self.pid, self.fd = pid, fd
        self.start = time.perf_counter()
        self.output = bytearray()
        self.scanned = 0
        self.eof = False
        self.last_data = self.start
        self.rusage = None
        self.exit_status = None
        os.set_blocking(fd, False)

    # -- output -----------------------------------------------------------

    def pump(self, timeout: float) -> int:
        """Read whatever output is available (waiting up to `timeout`)."""
        if self.eof:
            time.sleep(timeout)
            return 0
        ready, _, _ = select.select([self.fd], [], [], timeout)
        if not ready:
            return 0
        try:
            data = os.read(self.fd, 1 << 16)
        except BlockingIOError:
            return 0
        except OSError:  # EIO once the child has exited
            data = b""
        if not data:
            self.eof = True
            return 0
        self.last_data = time.perf_counter()
        self.output += data
        self._answer_queries()
        return len(data)

    def _answer_queries(self) -> None:
        # Collect replies first: writing can pump (and append) more output.
        window_start = max(0, self.scanned - 64)
        replies = [
            reply(m, self.output, m.start())
            for pattern, reply in QUERIES
            for m in pattern.finditer(self.output, window_start)
            if m.end() > self.scanned
        ]
        self.scanned = len(self.output)
        for r in replies:
            self.write(r)

    def text(self, limit: int = 4 << 20) -> str:
        """Output so far with escape sequences removed."""
        return CSI_RE.sub(b"", bytes(self.output[:limit])).decode("utf-8", "replace")

    # -- input ------------------------------------------------------------

    def write(self, data: bytes) -> None:
        view = memoryview(data)
        while view:
            try:
                n = os.write(self.fd, view)
                view = view[n:]
            except BlockingIOError:
                self.pump(0.001)  # input buffer full: let the editor catch up

    def send_script(self, script: list) -> float:
        """Type a script of bytes and Pauses. Returns the total pause time."""
        paused = 0.0
        for item in script:
            if isinstance(item, Pause):
                end = time.perf_counter() + item.seconds
                while time.perf_counter() < end:
                    self.pump(0.002)
                paused += item.seconds
            else:
                for i in range(0, len(item), 256):
                    self.write(item[i : i + 256])
                    self.pump(0)
        return paused

    # -- waiting ----------------------------------------------------------

    def in_raw_mode(self) -> bool:
        try:
            lflag = termios.tcgetattr(self.fd)[3]
        except termios.error:
            return False
        return not (lflag & termios.ICANON) and not (lflag & termios.ECHO)

    def wait_until(self, predicate, timeout: float, what: str) -> float:
        deadline = time.perf_counter() + timeout
        while not predicate():
            if time.perf_counter() > deadline:
                raise TimeoutError(f"timed out waiting for {what}")
            if self.poll_exit():
                raise RuntimeError(f"editor exited while waiting for {what}")
            self.pump(0.001)
        return time.perf_counter()

    def wait_ready(self, timeout: float = 30.0) -> float:
        return self.wait_until(self.in_raw_mode, timeout, "raw mode")

    def wait_for_text(self, needle: str, timeout: float = 60.0) -> float:
        checked = [0]

        def found() -> bool:
            # Re-strip escapes only when new output arrived.
            if len(self.output) == checked[0]:
                return False
            checked[0] = len(self.output)
            return needle in self.text()

        return self.wait_until(found, timeout, f"text {needle!r}")

    def wait_quiet(self, quiet: float = 0.05, timeout: float = 10.0) -> None:
        deadline = time.perf_counter() + timeout
        while time.perf_counter() - self.last_data < quiet and time.perf_counter() < deadline:
            self.pump(0.002)

    def poll_exit(self) -> bool:
        if self.exit_status is not None:
            return True
        pid, status, rusage = os.wait4(self.pid, os.WNOHANG)
        if pid == 0:
            return False
        self.exit_status, self.rusage = status, rusage
        self.end = time.perf_counter()
        return True

    def wait_exit(self, timeout: float = RUN_TIMEOUT) -> float:
        deadline = time.perf_counter() + timeout
        while not self.poll_exit():
            if time.perf_counter() > deadline:
                self.kill()
                raise TimeoutError("editor did not exit")
            self.pump(0.002)
        os.close(self.fd)
        return self.end

    def kill(self) -> None:
        try:
            os.kill(self.pid, signal.SIGKILL)
            os.wait4(self.pid, 0)
        except (ProcessLookupError, ChildProcessError):
            pass
        try:
            os.close(self.fd)
        except OSError:
            pass

    @property
    def max_rss_mb(self) -> float:
        return self.rusage.ru_maxrss / 1024  # ru_maxrss is in KiB on Linux

    @property
    def cpu_s(self) -> float:
        return self.rusage.ru_utime + self.rusage.ru_stime


# ---------------------------------------------------------------------------
# Scenarios. Each returns a dict of metric name -> value for one run.


def scripted(editor: Editor, file: Path | None, script: list, wait_paint: bool) -> dict:
    s = Session(editor.cmd + ([str(file)] if file else []), editor.env)
    try:
        ready = s.wait_ready()
        result = {"ready_ms": (ready - s.start) * 1000}
        if wait_paint:
            result["first_paint_ms"] = (s.wait_for_text(MARKER) - s.start) * 1000
        # Raw mode is not "ready": editors still exchange queries with the
        # terminal (helix drops keys typed meanwhile). Wait for the screen to
        # settle; the last output before the quiet period is the startup time.
        s.wait_quiet(SETTLE)
        result["settled_ms"] = (s.last_data - s.start) * 1000
        t0 = time.perf_counter()
        paused = s.send_script(script)
        end = s.wait_exit()
    except BaseException:
        s.kill()
        raise
    result["work_ms"] = (end - t0 - paused) * 1000
    result["max_rss_mb"] = s.max_rss_mb
    result["cpu_ms"] = s.cpu_s * 1000
    return result


def scenario_startup_empty(editor: Editor, ctx) -> dict:
    return scripted(editor, None, editor.quit(), wait_paint=False)


def scenario_open_large(editor: Editor, ctx) -> dict:
    return scripted(editor, ctx.large_file, editor.quit(), wait_paint=True)


def scenario_scroll(editor: Editor, ctx) -> dict:
    return scripted(editor, ctx.large_file, [b"j" * ctx.moves] + editor.quit(), wait_paint=True)


def scenario_jump(editor: Editor, ctx) -> dict:
    jumps = (editor.goto_end + editor.goto_top) * ctx.jumps
    return scripted(editor, ctx.large_file, [jumps] + editor.quit(), wait_paint=True)


def scenario_edit_save(editor: Editor, ctx) -> dict:
    target = ctx.workdir / f"edit-{editor.name}{ctx.large_file.suffix}"
    shutil.copyfile(ctx.large_file, target)
    script = [b"i", TYPED.encode(), Pause(ESC_PAUSE), b"\x1b", Pause(ESC_PAUSE)] + editor.write_quit()
    result = scripted(editor, target, script, wait_paint=True)
    with open(target, encoding="utf-8") as f:
        first = f.readline()
    result["correct"] = first.startswith(TYPED + MARKER)
    if target.stat().st_size != ctx.large_file.stat().st_size + len(TYPED.encode()):
        result["correct"] = False
    target.unlink()
    return result


def scenario_latency(editor: Editor, ctx) -> dict:
    """Per-key latency for normal-mode movement and insert-mode typing."""
    s = Session(editor.cmd + [str(ctx.large_file)], editor.env)
    try:
        s.wait_ready()
        s.wait_for_text(MARKER)
        s.wait_quiet(SETTLE * 2)

        def measure(key: bytes) -> tuple[float, float]:
            t = time.perf_counter()
            s.write(key)
            s.wait_until(lambda: s.last_data > t, 2.0, "a response to a key")
            first = s.last_data - t
            s.wait_quiet(0.02)
            return first, s.last_data - t

        move = [measure(b"j") for _ in range(ctx.samples)]
        s.write(b"i")
        s.wait_quiet(0.1)
        typing = [measure(b"x") for _ in range(ctx.samples)]
        s.send_script([Pause(ESC_PAUSE), b"\x1b", Pause(ESC_PAUSE)] + editor.quit())
        s.wait_exit()
    except BaseException:
        s.kill()
        raise
    ms = lambda xs: [x * 1000 for x in xs]
    return {
        "move_first_byte_ms": statistics.median(ms(f for f, _ in move)),
        "move_frame_done_ms": statistics.median(ms(d for _, d in move)),
        "move_frame_done_p95_ms": percentile(ms(d for _, d in move), 95),
        "type_first_byte_ms": statistics.median(ms(f for f, _ in typing)),
        "type_frame_done_ms": statistics.median(ms(d for _, d in typing)),
        "type_frame_done_p95_ms": percentile(ms(d for _, d in typing), 95),
        "max_rss_mb": s.max_rss_mb,
    }


def timed(s: Session, keys: bytes, quiet: float = 0.03) -> tuple[float, bytes]:
    """Write keys; return the time until the editor finished drawing (the
    last output before `quiet` seconds of silence) and the output produced."""
    mark = len(s.output)
    t = time.perf_counter()
    s.write(keys)
    s.wait_until(lambda: s.last_data > t, 30.0, "a response")
    s.wait_quiet(quiet)
    return s.last_data - t, bytes(s.output[mark:])


def search_session(editor: Editor, ctx, body, verify_target: str | None = None) -> dict:
    """Run `body` on a copy of the large file. With `verify_target`, finish
    by searching for it once more, typing HIT before the match and saving:
    the edited file shows whether the editor really found the right place."""
    target_file = ctx.workdir / f"search-{editor.name}{ctx.large_file.suffix}"
    shutil.copyfile(ctx.large_file, target_file)
    s = Session(editor.cmd + [str(target_file)], editor.env)
    try:
        s.wait_ready()
        s.wait_for_text(MARKER)
        s.wait_quiet(SETTLE * 2)
        result = body(s)
        if verify_target:
            timed(s, editor.goto_top)
            timed(s, b"/" + verify_target.encode() + b"\r")
            s.send_script([b"iHIT", Pause(ESC_PAUSE), b"\x1b", Pause(ESC_PAUSE)] + editor.write_quit())
        else:
            s.send_script([Pause(ESC_PAUSE), b"\x1b", Pause(ESC_PAUSE)] + editor.quit())
        s.wait_exit()
    except BaseException:
        s.kill()
        raise
    if verify_target:
        with open(target_file, encoding="utf-8") as f:
            last = f.read().splitlines()[-1]
        result["found"] = last.startswith("HIT" + verify_target)
    target_file.unlink()
    result["max_rss_mb"] = s.max_rss_mb
    return result


def scenario_search_far(editor: Editor, ctx) -> dict:
    """Search from the top for a pattern that only occurs on the last line."""
    target = f"{ctx.lines - 1:07d} lorem"

    def body(s: Session) -> dict:
        times = []
        for _ in range(ctx.search_samples):
            timed(s, editor.goto_top)
            times.append(timed(s, b"/" + target.encode() + b"\r")[0] * 1000)
        return {"search_far_ms": statistics.median(times)}

    return search_session(editor, ctx, body, verify_target=target)


def scenario_search_none(editor: Editor, ctx) -> dict:
    """Search for a pattern that does not occur (full scan plus wrap)."""

    def body(s: Session) -> dict:
        times = []
        for _ in range(ctx.search_samples):
            timed(s, editor.goto_top)
            took, _ = timed(s, b"/zzznotfound\r")
            times.append(took * 1000)
        return {"search_none_ms": statistics.median(times)}

    return search_session(editor, ctx, body)


def scenario_search_next(editor: Editor, ctx) -> dict:
    """`n` through a pattern that occurs every 1000 lines."""

    def body(s: Session) -> dict:
        timed(s, b"/000 lorem\r")
        steps = [timed(s, b"n")[0] * 1000 for _ in range(ctx.samples)]
        burst, _ = timed(s, b"n" * 200, quiet=0.05)
        return {
            "next_ms": statistics.median(steps),
            "next_p95_ms": percentile(steps, 95),
            "next_x200_burst_ms": burst * 1000,
        }

    return search_session(editor, ctx, body)


SCENARIOS = {
    "startup_empty": (scenario_startup_empty, "start with no file (time until the screen settles)"),
    "open_large": (scenario_open_large, "open the large file (first paint of line 1; screen settled)"),
    "scroll": (scenario_scroll, "press j MOVES times (sent at once), then :q!"),
    "jump": (scenario_jump, "go to the last line and back JUMPS times, then :q!"),
    "edit_save": (scenario_edit_save, "insert text at the top, Esc, :wq (output is verified)"),
    "latency": (scenario_latency, "per-key latency of j and of typing in insert mode"),
    "search_far": (scenario_search_far, "/pattern from the top; the only match is on the last line"),
    "search_none": (scenario_search_none, "/pattern that does not occur (whole file scanned)"),
    "search_next": (scenario_search_next, "n through matches every 1000 lines (one by one; 200 at once)"),
}

# Metrics to show in the summary table, per scenario.
SHOWN = {
    "startup_empty": ["settled_ms", "max_rss_mb"],
    "open_large": ["first_paint_ms", "settled_ms", "max_rss_mb"],
    "scroll": ["work_ms", "cpu_ms"],
    "jump": ["work_ms", "cpu_ms"],
    "edit_save": ["work_ms", "max_rss_mb", "correct"],
    "latency": ["move_frame_done_ms", "move_frame_done_p95_ms", "type_frame_done_ms", "type_frame_done_p95_ms"],
    "search_far": ["search_far_ms", "found"],
    "search_none": ["search_none_ms"],
    "search_next": ["next_ms", "next_p95_ms", "next_x200_burst_ms"],
}


def percentile(values, pct: float) -> float:
    xs = sorted(values)
    if not xs:
        return float("nan")
    k = (len(xs) - 1) * pct / 100
    lo, hi = int(k), min(int(k) + 1, len(xs) - 1)
    return xs[lo] + (xs[hi] - xs[lo]) * (k - lo)


# ---------------------------------------------------------------------------
# Setup and reporting.


@dataclass
class Context:
    workdir: Path
    large_file: Path
    moves: int
    jumps: int
    samples: int
    search_samples: int
    lines: int


def make_large_file(path: Path, lines: int) -> None:
    with open(path, "w", encoding="utf-8") as f:
        f.write(f"{MARKER} first line of the benchmark file\n")
        for i in range(1, lines):
            f.write(f"{i:07d} lorem ipsum dolor sit amet, consectetur adipiscing elit {i * 7919 % 100003}\n")


def find_him() -> str | None:
    root = Path(__file__).resolve().parent.parent
    try:
        out = subprocess.run(
            ["stack", "path", "--local-install-root"], cwd=root, capture_output=True, text=True, check=True
        ).stdout.strip()
    except (OSError, subprocess.CalledProcessError):
        return None
    exe = Path(out) / "bin" / "him"
    return str(exe) if exe.exists() else None


def editors(workdir: Path, him: str | None) -> dict[str, Editor]:
    helix_config = workdir / "helix.toml"
    # No auto-completion popup or auto-pairs while the script types, so the
    # typed text ends up in the file verbatim.
    helix_config.write_text("[editor]\nauto-completion = false\nauto-pairs = false\n")
    found = {}
    if him:
        # Own config files, so the user's config does not change the numbers:
        # every plugin (the default), and none (a "lightweight" him).
        full = workdir / "him-full.toml"
        full.write_text("")
        lite = workdir / "him-lite.toml"
        lite.write_text("[plugins]\ngit = false\nlsp = false\nrepl = false\n")
        found["him"] = Editor("him", [him], goto_end=b"ge", goto_top=b"gg", env={"HIM_CONFIG": str(full)})
        found["him-lite"] = Editor("him-lite", [him], goto_end=b"ge", goto_top=b"gg", env={"HIM_CONFIG": str(lite)})
        # One plugin off at a time, to see what each costs (not in the default list).
        for plugin in ("git", "lsp"):
            cfg = workdir / f"him-no{plugin}.toml"
            cfg.write_text(f"[plugins]\n{plugin} = false\n")
            found[f"him-no{plugin}"] = Editor(f"him-no{plugin}", [him], goto_end=b"ge", goto_top=b"gg", env={"HIM_CONFIG": str(cfg)})
    if shutil.which("vim"):
        # No vimrc, plugins, swap or viminfo; short key-code timeout.
        found["vim"] = Editor(
            "vim",
            ["vim", "-u", "NONE", "-i", "NONE", "-N", "--noplugin", "-n",
             "--cmd", "set ttimeout ttimeoutlen=10 ruler"],
            goto_end=b"G",
            goto_top=b"gg",
        )
    if shutil.which("hx"):
        found["helix"] = Editor("helix", ["hx", "-c", str(helix_config)], goto_end=b"ge", goto_top=b"gg")
    return found


def editor_version(editor: Editor) -> str:
    if editor.name.startswith("him"):
        plugins = {"him-lite": "plugins off", "him-nogit": "git off", "him-nolsp": "lsp off"}.get(editor.name, "all plugins")
        return f"him (this repository, {plugins}, {editor.cmd[0]})"
    cmd = {"vim": ["vim", "--version"], "helix": ["hx", "--version"]}[editor.name]
    try:
        return subprocess.run(cmd, capture_output=True, text=True, timeout=5).stdout.splitlines()[0]
    except (OSError, subprocess.SubprocessError, IndexError):
        return "?"


def fmt(value) -> str:
    if isinstance(value, bool):
        return "ok" if value else "FAIL"
    if isinstance(value, str):
        return value
    if value != value:  # NaN
        return "-"
    return f"{value:.1f}" if value < 100 else f"{value:.0f}"


def summarize(runs: list[dict]) -> dict:
    """Median of every numeric metric; booleans must hold in every run."""
    out = {}
    for key in runs[0]:
        values = [r[key] for r in runs if key in r]
        if isinstance(values[0], bool):
            out[key] = all(values)
        else:
            out[key] = statistics.median(values)
    return out


def print_table(results: dict, names: list[str]) -> None:
    width = max(len(n) for n in names) + 2
    for scenario, metrics in SHOWN.items():
        if not any(scenario in results[n] for n in names):
            continue
        print(f"\n{scenario}: {SCENARIOS[scenario][1]}")
        print(f"  {'metric':<26}" + "".join(f"{n:>{max(width, 10)}}" for n in names))
        for metric in metrics:
            row = f"  {metric:<26}"
            for n in names:
                r = results[n].get(scenario)
                if r is None:
                    cell = "-"
                elif "error" in r:
                    cell = "error"
                else:
                    cell = fmt(r.get(metric, float("nan")))
                row += f"{cell:>{max(width, 10)}}"
            print(row)


def main() -> int:
    ap = argparse.ArgumentParser(description=__doc__, formatter_class=argparse.RawDescriptionHelpFormatter)
    ap.add_argument("--editors", default="him,him-lite,vim,helix", help="comma-separated (default: him,him-lite,vim,helix)")
    ap.add_argument("--scenarios", default=",".join(SCENARIOS), help="comma-separated scenario names")
    ap.add_argument("--runs", type=int, default=5, help="runs per scenario; the median is reported")
    ap.add_argument("--lines", type=int, default=200_000, help="lines in the large file")
    ap.add_argument("--moves", type=int, default=2000, help="j presses in the scroll scenario")
    ap.add_argument("--jumps", type=int, default=100, help="end/top round trips in the jump scenario")
    ap.add_argument("--samples", type=int, default=100, help="keys measured per latency run")
    ap.add_argument("--search-samples", type=int, default=10, help="searches measured per search run")
    ap.add_argument("--him", help="path to the him binary (default: from `stack path`)")
    ap.add_argument("--json", help="also write all raw results to this file")
    ap.add_argument("--ext", default="txt", help="extension of the large file (e.g. rs: highlighting and language servers start)")
    ap.add_argument("--git", action="store_true", help="make the work directory a git repository with the file committed (git signs have work to do)")
    args = ap.parse_args()

    workdir = Path(tempfile.mkdtemp(prefix="him-bench-"))
    try:
        available = editors(workdir, args.him or find_him())
        names = [n for n in args.editors.split(",") if n]
        missing = [n for n in names if n not in available]
        if missing:
            print(f"not found, skipping: {', '.join(missing)}", file=sys.stderr)
        names = [n for n in names if n in available]
        if not names:
            print("no editors to benchmark", file=sys.stderr)
            return 1

        large = workdir / f"large.{args.ext}"
        make_large_file(large, args.lines)
        if args.git:
            for cmd in (["git", "init", "-q"], ["git", "add", large.name], ["git", "-c", "user.name=b", "-c", "user.email=b@b", "commit", "-qm", "bench"]):
                subprocess.run(cmd, cwd=workdir, check=True)
        ctx = Context(workdir, large, args.moves, args.jumps, args.samples, args.search_samples, args.lines)
        size_mb = large.stat().st_size / 1e6
        print(f"terminal {COLS}x{ROWS}, large file: {args.lines} lines ({size_mb:.1f} MB), "
              f"{args.runs} runs per scenario (median reported)")
        for n in names:
            print(f"  {n}: {editor_version(available[n])}")

        results: dict[str, dict] = {n: {} for n in names}
        raw: dict[str, dict] = {n: {} for n in names}
        for scenario in [s for s in args.scenarios.split(",") if s]:
            fn, _ = SCENARIOS[scenario]
            for n in names:
                runs = []
                print(f"  running {scenario:<14} {n:<6}", end="", flush=True, file=sys.stderr)
                try:
                    for _ in range(args.runs):
                        runs.append(fn(available[n], ctx))
                        print(".", end="", flush=True, file=sys.stderr)
                    results[n][scenario] = summarize(runs)
                except (TimeoutError, RuntimeError, OSError) as e:
                    results[n][scenario] = {"error": str(e)}
                    print(f" error: {e}", end="", file=sys.stderr)
                raw[n][scenario] = runs
                print(file=sys.stderr)

        print_table(results, names)
        if args.json:
            Path(args.json).write_text(json.dumps({"summary": results, "runs": raw}, indent=2))
            print(f"\nraw results written to {args.json}")
    finally:
        shutil.rmtree(workdir, ignore_errors=True)
    return 0


if __name__ == "__main__":
    sys.exit(main())
