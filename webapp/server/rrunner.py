"""
rrunner.py
----------
Locate Rscript and drive engine/run_pipeline.R, turning its stdout line
protocol (PROGRESS / LOG / RESULT / ERROR) into a stream of event dicts.

The subprocess is read in a background thread and lines are pushed onto an
asyncio.Queue, so this works regardless of the event-loop type uvicorn picks
on Windows (avoids asyncio subprocess-pipe pitfalls).

The child never outlives its run: it is terminated (with its own children -
Rscript.exe starts Rterm.exe) when the client disconnects, when the user
cancels, or when the stream is closed for any other reason.
"""

from __future__ import annotations

import asyncio
import glob
import json
import os
import re
import subprocess
import threading
import time
from typing import AsyncIterator, Awaitable, Callable


APP_ROOT = os.path.dirname(os.path.dirname(os.path.dirname(os.path.abspath(__file__))))
SETTINGS_PATH = os.path.join(APP_ROOT, "config", "settings.json")


def load_settings() -> dict:
    try:
        with open(SETTINGS_PATH, encoding="utf-8") as fh:
            return json.load(fh)
    except (OSError, json.JSONDecodeError):
        return {}


def save_settings(data: dict) -> None:
    os.makedirs(os.path.dirname(SETTINGS_PATH), exist_ok=True)
    with open(SETTINGS_PATH, "w", encoding="utf-8") as fh:
        json.dump(data, fh, indent=2)


def validate_rscript(path: str) -> tuple[bool, str]:
    """Run the candidate Rscript and report its version (or why it failed)."""
    if not path or not os.path.exists(path):
        return False, "That file does not exist."
    if not os.path.basename(path).lower().startswith("rscript"):
        return False, "Please choose Rscript.exe (inside R's bin folder)."
    try:
        out = run_captured([path, "--vanilla", "-e",
                            'cat(paste(R.version$major, R.version$minor, sep="."))'],
                           cwd=None, timeout=90)
    except Exception as e:  # noqa: BLE001
        return False, f"Could not run it: {e}"
    ver = (out.stdout or "").strip().splitlines()[-1:] or [""]
    ver = ver[0].strip()
    if out.returncode != 0 or not ver:
        return False, (out.stderr or "Rscript did not report a version.").strip()[:300]
    if not ver.startswith("4.5"):
        return True, (f"Found R {ver}. The bundled packages were built for R 4.5.x — "
                      "the analysis may fail. Installing R 4.5.x is recommended.")
    return True, f"Found R {ver}."


def find_rscript() -> str | None:
    # 1) saved app setting (set on the R Setup screen)
    saved = load_settings().get("rscript_path")
    if saved and os.path.exists(saved):
        return saved
    # 2) explicit override
    env = os.environ.get("RSCRIPT_PATH")
    if env and os.path.exists(env):
        return env
    # 3) standard install locations (machine-wide and per-user), newest first,
    #    preferring R 4.5.x (the version the bundled packages were built for)
    hits = _installed_rscripts()
    pref = [h for h in hits if _r_version(h)[:2] == (4, 5)]
    if pref:
        return pref[0]
    if hits:
        return hits[0]
    # 4) on PATH
    from shutil import which
    return which("Rscript")


def _r_install_patterns() -> list[str]:
    """Where R's Windows installer puts Rscript.exe: Program Files for an
    admin install, %LOCALAPPDATA%\\Programs for a per-user one."""
    pf = os.environ.get("ProgramFiles") or r"C:\Program Files"
    pf86 = os.environ.get("ProgramFiles(x86)") or r"C:\Program Files (x86)"
    local = os.environ.get("LOCALAPPDATA") or os.path.join(os.path.expanduser("~"), "AppData", "Local")
    roots = [os.path.join(pf, "R"), os.path.join(local, "Programs", "R"), os.path.join(pf86, "R")]
    pats = []
    for root in roots:
        pats.append(os.path.join(root, "R-*", "bin", "x64", "Rscript.exe"))
        pats.append(os.path.join(root, "R-*", "bin", "Rscript.exe"))
    pats.append(os.path.join(pf, "Microsoft", "R Open", "R-*", "bin", "Rscript.exe"))
    return pats


def _r_version(path: str) -> tuple[int, int, int]:
    """(major, minor, patch) from an 'R-4.5.10' folder name; (0, 0, 0) if none.
    A string sort would rank R-4.5.2 above R-4.5.10."""
    m = re.search(r"R-(\d+)\.(\d+)(?:\.(\d+))?", path)
    if not m:
        return (0, 0, 0)
    return (int(m.group(1)), int(m.group(2)), int(m.group(3) or 0))


def _installed_rscripts() -> list[str]:
    """Every Rscript.exe in the standard locations, newest version first; for
    one version the x64 binary comes before the bin\\ launcher."""
    seen, hits = set(), []
    for p in _r_install_patterns():
        for h in glob.glob(p):
            k = os.path.normcase(os.path.normpath(h))
            if k not in seen:
                seen.add(k)
                hits.append(h)
    # stable sort: pattern order (x64 first) breaks ties within a version
    hits.sort(key=_r_version, reverse=True)
    return hits


def discover_r_installs() -> list[str]:
    """All Rscript.exe candidates found on this machine (for the Setup screen)."""
    hits = _installed_rscripts()
    from shutil import which
    w = which("Rscript")
    if w:
        hits.append(w)
    seen, out = set(), []
    for h in hits:
        k = os.path.normcase(os.path.normpath(h))
        if k not in seen:
            seen.add(k)
            out.append(h)
    return out


# lines R prints after an error that are not part of its message
_R_TRAILER = re.compile(r"^\s*(Execution halted|Warning messages?:|In addition:|Calls:|"
                        r"There (were|was) \d+ warnings?|Backtrace:)")
_MAX_FOLDED = 12


def _classify(line: str) -> dict | None:
    line = line.rstrip("\r\n")
    if not line:
        return None
    if line.startswith("PROGRESS "):
        rest = line[len("PROGRESS "):].strip()
        parts = rest.split(" ", 1)
        try:
            frac = float(parts[0])
        except (ValueError, IndexError):
            frac = None
        msg = parts[1] if len(parts) > 1 else ""
        return {"type": "progress", "fraction": frac, "message": msg}
    if line.startswith("LOG "):
        return {"type": "log", "message": line[len("LOG "):]}
    if line.startswith("ERROR "):
        return {"type": "error", "message": line[len("ERROR "):]}
    if line.startswith("RESULT "):
        payload = line[len("RESULT "):]
        try:
            data = json.loads(payload)
        except json.JSONDecodeError:
            data = {"raw": payload}
        return {"type": "result", "data": data}
    # Non-protocol noise (e.g. "Warning messages:") -> surface quietly as debug
    return {"type": "debug", "message": line}


def kill_tree(proc: subprocess.Popen) -> None:
    """Terminate a child and everything it started."""
    if proc.poll() is not None:
        return
    try:
        if os.name == "nt":
            subprocess.run(["taskkill", "/T", "/F", "/PID", str(proc.pid)],
                           capture_output=True, timeout=30)
        if proc.poll() is None:
            proc.kill()
    except Exception:  # noqa: BLE001
        try:
            proc.kill()
        except Exception:  # noqa: BLE001
            pass
    try:
        proc.wait(timeout=10)
    except Exception:  # noqa: BLE001
        pass


# R children started by run_captured() that have not ended yet: stopped by
# kill_active() when the app shuts down
_ACTIVE: set = set()
_ACTIVE_LOCK = threading.Lock()


def run_captured(args: list, cwd: str | None, timeout: float) -> subprocess.CompletedProcess:
    """Run an R child to the end and return its output, read as UTF-8 (R
    writes UTF-8; the Windows code page would garble or refuse a Hebrew group
    name). On timeout the whole process tree is killed - Rscript.exe starts
    Rterm.exe, which holds the output pipe, so killing Rscript alone would
    leave communicate() waiting until R finished - and TimeoutExpired is
    raised. The child never outlives this call."""
    proc = subprocess.Popen(args, cwd=cwd, stdin=subprocess.DEVNULL,
                            stdout=subprocess.PIPE, stderr=subprocess.PIPE,
                            text=True, encoding="utf-8", errors="replace")
    with _ACTIVE_LOCK:
        _ACTIVE.add(proc)
    try:
        try:
            out, err = proc.communicate(timeout=timeout)
        except subprocess.TimeoutExpired:
            kill_tree(proc)
            try:
                proc.communicate(timeout=10)
            except Exception:  # noqa: BLE001
                pass
            raise
        return subprocess.CompletedProcess(args, proc.returncode, out, err)
    finally:
        if proc.poll() is None:
            kill_tree(proc)
        with _ACTIVE_LOCK:
            _ACTIVE.discard(proc)


def kill_active() -> None:
    """Stop every R child of run_captured() still running (app shutdown)."""
    with _ACTIVE_LOCK:
        procs = list(_ACTIVE)
    for p in procs:
        kill_tree(p)


def kill_tree_background(proc: subprocess.Popen) -> threading.Thread | None:
    """kill_tree() in a worker thread. taskkill and the wait for the child can
    take seconds; on the event loop they would stall every other request
    (counts, health, cancel) meanwhile."""
    if proc.poll() is not None:
        return None
    t = threading.Thread(target=kill_tree, args=(proc,), daemon=True, name="se-kill-tree")
    t.start()
    return t


class RunHandle:
    """One pipeline run: its child process and whether the user cancelled it."""

    def __init__(self) -> None:
        self.proc: subprocess.Popen | None = None
        self.cancelled = False
        self.started = time.time()
        self.finished: float | None = None     # when the stream ended
        self.killer: threading.Thread | None = None   # a kill still in progress

    def alive(self) -> bool:
        return self.proc is not None and self.proc.poll() is None

    def active(self, grace: float = 30.0) -> bool:
        """True while this run still owns the app (blocks a second run) -
        including while its child is still being killed."""
        if self.killer is not None and self.killer.is_alive():
            return True
        if self.finished is not None:
            return False
        if self.proc is None:
            # the stream has not started the child yet; give up after `grace`
            return time.time() - self.started < grace
        return True

    def terminate(self) -> bool:
        p = self.proc
        if p is None or p.poll() is not None:
            return False
        kill_tree(p)
        return True

    def terminate_nowait(self) -> bool:
        """terminate() without waiting: for the asyncio side (the stream)."""
        p = self.proc
        if p is None or p.poll() is not None:
            return False
        self.killer = kill_tree_background(p)
        return True


async def stream_pipeline(rscript: str, wrapper: str, config_path: str,
                          app_root: str, handle: RunHandle | None = None,
                          is_disconnected: Callable[[], Awaitable[bool]] | None = None,
                          poll: float = 1.0) -> AsyncIterator[dict]:
    """Yield event dicts as the R pipeline runs. Ends with a 'result' or
    'error' event, then a 'done' event with the process return code.

    `is_disconnected` is polled while R is quiet; when the client has gone the
    child is terminated and the stream ends. Leaving the generator early for
    any other reason terminates the child too."""
    queue: asyncio.Queue = asyncio.Queue()
    loop = asyncio.get_running_loop()
    sentinel = object()
    handle = handle or RunHandle()

    try:
        proc = subprocess.Popen(
            [rscript, "--vanilla", wrapper, config_path],
            cwd=app_root,
            stdin=subprocess.DEVNULL,
            stdout=subprocess.PIPE,
            stderr=subprocess.STDOUT,
            text=True,
            encoding="utf-8",
            errors="replace",
            bufsize=1,
        )
    except Exception as e:  # noqa: BLE001
        yield {"type": "error", "message": f"Could not start R: {e}"}
        yield {"type": "done", "returncode": 1}
        return
    handle.proc = proc
    if handle.cancelled:          # cancelled before the child existed
        handle.terminate_nowait()

    def push(ev):
        loop.call_soon_threadsafe(queue.put_nowait, ev)

    def worker():
        # An R condition message can span several lines; only the first
        # carries the ERROR prefix. Continuation lines are folded into that
        # error event (the cause is usually on them) until the next protocol
        # line or R's own trailer.
        pending = None
        n_folded = 0
        try:
            for line in iter(proc.stdout.readline, ""):
                ev = _classify(line)
                if ev is None:
                    continue
                if pending is not None:
                    if (ev["type"] == "debug" and n_folded < _MAX_FOLDED
                            and not _R_TRAILER.match(ev["message"])):
                        pending["message"] += " | " + ev["message"].strip()
                        n_folded += 1
                        continue
                    push(pending)
                    pending = None
                if ev["type"] == "error":
                    pending, n_folded = ev, 0
                    continue
                push(ev)
        except Exception:  # noqa: BLE001  (pipe closed by a kill)
            pass
        if pending is not None:
            try:
                push(pending)
            except RuntimeError:
                pass
        try:
            proc.stdout.close()
        except Exception:  # noqa: BLE001
            pass
        rc = proc.wait()
        try:
            loop.call_soon_threadsafe(queue.put_nowait, (sentinel, rc))
        except RuntimeError:      # event loop already closed (server shutdown)
            pass

    threading.Thread(target=worker, daemon=True).start()

    try:
        while True:
            try:
                item = await asyncio.wait_for(queue.get(), timeout=poll)
            except asyncio.TimeoutError:
                if is_disconnected is not None and await is_disconnected():
                    handle.terminate_nowait()
                    return
                continue
            if isinstance(item, tuple) and item and item[0] is sentinel:
                yield {"type": "done", "returncode": item[1]}
                return
            yield item
    finally:
        # client gone, stream closed or cancelled: never leave R running (the
        # kill runs in a thread: an await here could itself be cancelled)
        if proc.poll() is None and not (handle.killer is not None and handle.killer.is_alive()):
            handle.terminate_nowait()
