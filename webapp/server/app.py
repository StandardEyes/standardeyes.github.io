"""
app.py -- FastAPI backend for the Refractive-Analysis web GUI.

Responsibilities (GUI plumbing only; NO statistics live here):
  * pick / load an Excel file from local disk (native dialog, no upload)
  * serve column names, a preview, and per-column distinct values
  * live filter/group row counts via the pandas mirror (preview.py)
  * run the unchanged R engine and stream progress (SSE)
  * read back produced workbooks for in-app results preview
  * save / load named config presets
"""

from __future__ import annotations

import atexit
import glob
import json
import os
import re
import shutil
import sys
import threading
import time
import unicodedata
from datetime import datetime
from typing import Any
from urllib.parse import quote, urlparse

# Make sibling modules importable no matter how uvicorn is launched.
sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))

import pandas as pd
from fastapi import Body, FastAPI, HTTPException, Query, Request
from fastapi.responses import FileResponse, JSONResponse, StreamingResponse
from fastapi.staticfiles import StaticFiles
from pydantic import BaseModel

import config_schema
import preview
import rrunner

# ---------------------------------------------------------------- paths
SERVER_DIR = os.path.dirname(os.path.abspath(__file__))
WEBAPP_DIR = os.path.dirname(SERVER_DIR)
APP_ROOT = os.path.dirname(WEBAPP_DIR)                # the app folder
STATIC_DIR = os.path.join(WEBAPP_DIR, "static")
PRESETS_DIR = os.path.join(WEBAPP_DIR, "presets")
RUNS_DIR = os.path.join(WEBAPP_DIR, ".runs")
WRAPPER = os.path.join(APP_ROOT, "engine", "run_pipeline.R")
os.makedirs(PRESETS_DIR, exist_ok=True)
os.makedirs(RUNS_DIR, exist_ok=True)

KEEP_CONFIGS = 20      # newest run configs kept in .runs (each embeds a data path)

# Several StandardEyes windows may share this .runs folder (launch.py picks a
# free port, so a second copy runs beside the first). Every window names its
# temporary folders after its process id and holds .runs/owner_<pid>.lock open
# for as long as it runs; housekeeping removes only this window's own folders
# and those whose owner is gone, never another live window's results.
PID = os.getpid()
MATCHPREV_DIR = os.path.join(RUNS_DIR, f"matchprev_p{PID}")
_OWNED = re.compile(r"^(?:preview|matchprev)_p(\d+)(?:_|$)")


def _lock_path(pid: int) -> str:
    return os.path.join(RUNS_DIR, f"owner_{pid}.lock")


def _pid_running(pid: int) -> bool:
    """A process with this id exists (it may be an unrelated one: the lock
    file decides whether it is a StandardEyes window)."""
    if os.name != "nt":
        try:
            os.kill(pid, 0)
        except ProcessLookupError:
            return False
        except OSError:
            return True
        return True
    import ctypes  # noqa: PLC0415
    k32 = ctypes.WinDLL("kernel32", use_last_error=True)
    h = k32.OpenProcess(0x1000, False, pid)      # PROCESS_QUERY_LIMITED_INFORMATION
    if not h:
        return ctypes.get_last_error() == 5      # access denied: it exists
    try:
        code = ctypes.c_ulong()
        if not k32.GetExitCodeProcess(h, ctypes.byref(code)):
            return True
        return code.value == 259                 # STILL_ACTIVE
    finally:
        k32.CloseHandle(h)


def _owner_alive(pid: int) -> bool:
    """The StandardEyes window with this process id is still running: its
    process exists AND it still holds its lock file open (Windows refuses to
    delete a file another process has open; a closed or killed window has
    released it). A stale lock file is removed on the way."""
    if pid == PID:
        return True
    running = _pid_running(pid)
    lock = _lock_path(pid)
    if os.name != "nt":
        return running and os.path.exists(lock)
    try:
        os.remove(lock)
        return False
    except FileNotFoundError:
        return False
    except OSError:
        return running


def _owner_of(name: str) -> int | None:
    """Process id in a temporary folder's name; None for a name without one
    (written by a release before per-window names - stale by now)."""
    m = _OWNED.match(name)
    return int(m.group(1)) if m else None


def _is_preview_config(path: str) -> bool:
    """A config in the .runs root that belongs to a preview-only run (releases
    before configs moved into the staging): it names the data file and
    points at a preview_* folder."""
    try:
        with open(path, encoding="utf-8") as fh:
            out = str((json.load(fh) or {}).get("output_dir") or "")
    except (OSError, ValueError, AttributeError):
        return False
    return (os.path.basename(os.path.normpath(out)).startswith("preview_")
            and os.path.normcase(os.path.realpath(os.path.dirname(os.path.normpath(out))))
            == os.path.normcase(os.path.realpath(RUNS_DIR)))


def _housekeep_runs(keep_dir: str | None = None) -> None:
    """Remove preview-only stagings (they hold patient-level workbooks and
    CSVs inside the app folder) of THIS window except `keep_dir`, every
    staging and match-preview folder whose window is gone, stale lock files,
    preview configs of earlier releases, and all but the newest KEEP_CONFIGS
    run configs. This window's match-preview folder is left alone (a match
    preview may be running beside a new run)."""
    keep = os.path.realpath(keep_dir) if keep_dir else None
    for d in glob.glob(os.path.join(RUNS_DIR, "preview_*")) + \
            glob.glob(os.path.join(RUNS_DIR, "matchprev*")):
        if not os.path.isdir(d) or os.path.realpath(d) == keep:
            continue
        name = os.path.basename(d)
        pid = _owner_of(name)
        if pid == PID:
            if name.startswith("preview_"):
                shutil.rmtree(d, ignore_errors=True)
        elif pid is None or not _owner_alive(pid):
            shutil.rmtree(d, ignore_errors=True)
    for f in glob.glob(os.path.join(RUNS_DIR, "owner_*.lock")):
        m = re.match(r"^owner_(\d+)\.lock$", os.path.basename(f))
        if m:
            _owner_alive(int(m.group(1)))        # removes the file when stale
    cfgs = []
    for f in glob.glob(os.path.join(RUNS_DIR, "config_*.json")):
        if _is_preview_config(f):
            try:
                os.remove(f)
            except OSError:
                pass
        else:
            cfgs.append(f)
    cfgs.sort(key=lambda p: os.path.getmtime(p) if os.path.exists(p) else 0, reverse=True)
    for f in cfgs[KEEP_CONFIGS:]:
        try:
            os.remove(f)
        except OSError:
            pass


_housekeep_runs()
try:
    # held open (never written) until this process ends: tells other windows
    # that this one is alive
    _OWNER_FH = open(_lock_path(PID), "w", encoding="utf-8")   # noqa: SIM115
except OSError:
    _OWNER_FH = None

# ---------------------------------------------------------------- state
# Single-user local tool: one loaded dataset in memory is enough.
STATE: dict[str, Any] = {"df": None, "path": None, "sheet": None, "columns": [], "result": None,
                         "run": None}
_RUN_LOCK = threading.Lock()


@atexit.register
def shutdown_cleanup() -> None:
    """A preview-only staging holds patient-level files and is useless once
    the app is closed: remove this window's stagings and match-preview files
    (a hard kill is covered at the next start of any window). A run still in
    progress is stopped first, so R does not keep writing. Safe to call more
    than once (the console-close handler in launch.py, then atexit)."""
    global _OWNER_FH
    h = STATE.get("run")
    if h is not None:
        try:
            h.terminate()
        except Exception:  # noqa: BLE001
            pass
    try:
        rrunner.kill_active()          # a match preview still running
    except Exception:  # noqa: BLE001
        pass
    d = STATE.get("staging_dir")
    if d:
        shutil.rmtree(d, ignore_errors=True)
    for d in glob.glob(os.path.join(RUNS_DIR, f"preview_p{PID}_*")):
        shutil.rmtree(d, ignore_errors=True)
    shutil.rmtree(MATCHPREV_DIR, ignore_errors=True)
    if _OWNER_FH is not None:
        try:
            _OWNER_FH.close()
            os.remove(_lock_path(PID))
        except OSError:
            pass
        _OWNER_FH = None

# The ONE version source (decision D25): the engine reads these two lines from
# this file (R/export.R standardeyes_version) for every workbook's Manifest,
# also in a run from the command line; the release tools read them too. Keep
# each a plain string assignment on its own line.
APP_VERSION = "2026-10-04-se.10"  # build id; bump together with static/app.js UI_VERSION
PUBLIC_VERSION = "1.1.0"          # the release number the website and citation use

app = FastAPI(title="StandardEyes")

LOCAL_HOSTS = ("127.0.0.1", "localhost")


def _host_name(netloc: str) -> str:
    """'localhost:8760' -> 'localhost' (lower-case; IPv6 brackets kept)."""
    netloc = (netloc or "").strip().lower()
    if netloc.startswith("["):
        return netloc.split("]", 1)[0] + "]"
    return netloc.rsplit(":", 1)[0] if ":" in netloc else netloc


class LocalOriginOnly:
    """Accept a request only when it is addressed to this machine by name
    (Host 127.0.0.1 / localhost) and, when the browser says where it comes
    from (Origin, else Referer), only from this app's own page.

    The Host check stops DNS rebinding: a page whose host name was re-pointed
    at 127.0.0.1 still sends its own name. The Origin check stops other web
    pages from firing requests with side effects (file dialogs, writes) even
    though they cannot read the answer. Pure ASGI, so streamed runs and
    client-disconnect detection are not wrapped."""

    def __init__(self, app_) -> None:
        self.app = app_

    async def __call__(self, scope, receive, send):
        if scope.get("type") not in ("http", "websocket"):
            return await self.app(scope, receive, send)
        headers = {k.decode("latin-1").lower(): v.decode("latin-1")
                   for k, v in scope.get("headers") or []}
        host = headers.get("host", "")
        why = None
        if _host_name(host) not in LOCAL_HOSTS:
            why = "Host"
        else:
            src = headers.get("origin")
            if src is None and scope.get("method", "GET") not in ("GET", "HEAD"):
                src = headers.get("referer")
            if src is not None:
                u = urlparse(src.strip())
                # same origin = same scheme, host name AND port as this server
                if u.scheme != "http" or u.netloc.lower() != host.strip().lower():
                    why = "Origin"
        if why is None:
            return await self.app(scope, receive, send)
        if scope["type"] != "http":
            return await send({"type": "websocket.close", "code": 1008})
        resp = JSONResponse({"detail": f"Forbidden: StandardEyes only answers its own page "
                                       f"on this computer ({why} not allowed)."}, status_code=403)
        await resp(scope, receive, send)


app.add_middleware(LocalOriginOnly)


# ---------------------------------------------------------------- helpers
def _table(df: pd.DataFrame, max_rows: int | None = None) -> dict:
    """Serialize a DataFrame as {columns, rows} (rows = list of lists) so that
    duplicate/blank headers in result sheets don't collide."""
    d = df if max_rows is None else df.head(max_rows)
    d = d.copy()
    for c in d.columns:
        if pd.api.types.is_datetime64_any_dtype(d[c]):
            d[c] = d[c].astype(str)
    d = d.astype(object).where(pd.notna(d), None)
    return {"columns": [str(c) for c in d.columns], "rows": d.values.tolist()}


LEGACY_XLS_MSG = ("This is a legacy Excel 97-2003 workbook (.xls), which StandardEyes cannot "
                  "read. Open it in Excel, choose File > Save As > Excel Workbook (*.xlsx), "
                  "then load the .xlsx file.")
ENCRYPTED_XLSX_MSG = ("This workbook is password-protected (encrypted), or it is an Excel 97-2003 "
                      "workbook renamed to .xlsx; StandardEyes can read neither. Open it in Excel, "
                      "remove any password (File > Info > Protect Workbook), save it as Excel "
                      "Workbook (*.xlsx), then load it again.")
RENAMED_XLSX_MSG = ("This file is an Excel workbook (.xlsx) whose name ends in .xls. The analysis "
                    "reads the format from the name and would fail: rename it to .xlsx, then "
                    "load it again.")


def _unreadable_workbook(path: str) -> str | None:
    """Why a workbook cannot be opened by openpyxl - or by readxl in the run,
    which picks the format from the extension - judged by its first bytes
    (an .xlsx / .xlsm is a zip file; an OLE2 container is either an Excel
    97-2003 .xls or an encrypted .xlsx). None = looks readable."""
    try:
        with open(path, "rb") as fh:
            head = fh.read(8)
    except OSError:
        return None
    if head == b"\xd0\xcf\x11\xe0\xa1\xb1\x1a\xe1":
        return ENCRYPTED_XLSX_MSG if path.lower().endswith((".xlsx", ".xlsm")) else LEGACY_XLS_MSG
    if path.lower().endswith(".xls"):
        return RENAMED_XLSX_MSG if head.startswith(b"PK\x03\x04") else LEGACY_XLS_MSG
    return None


def _require_df() -> pd.DataFrame:
    if STATE["df"] is None:
        raise HTTPException(400, "No data file loaded yet.")
    return STATE["df"]


def _safe_output_path(file: str) -> str:
    """Only allow reading/serving files under a known output directory."""
    result = STATE.get("result") or {}
    out_dir = result.get("output_dir")
    real = os.path.realpath(file)
    if out_dir:
        # component-wise containment: a bare startswith() would also accept
        # sibling folders like "C:\output-other" for out_dir "C:\output"
        base = os.path.realpath(out_dir)
        try:
            inside = os.path.commonpath([real, base]) == base
        except ValueError:          # different drives
            inside = False
        if inside:
            return real
    # also allow files explicitly produced this run
    allowed = set(result.get("files", [])) | set(result.get("matched_files", []))
    if real in {os.path.realpath(f) for f in allowed}:
        return real
    raise HTTPException(403, "File is outside the analysis output folder.")


# ---------------------------------------------------------------- models
class LoadReq(BaseModel):
    path: str
    sheet: Any | None = None


class CountsReq(BaseModel):
    cfg: dict


class RunReq(BaseModel):
    cfg: dict
    preview_only: bool = False


class ExportReq(BaseModel):
    files: list[str]
    dest: str
    include_sidecars: bool = False   # love plots + filtered CSV from the run


class ExportFilteredReq(BaseModel):
    cfg: dict
    dest: str | None = None


class PresetReq(BaseModel):
    name: str
    cfg: dict
    # None (default) = overwrite and report it; False = refuse (409) when the
    # preset already exists
    overwrite: bool | None = None


# ---------------------------------------------------------------- routes: file
@app.post("/api/pick-file")
async def pick_file():
    """Open a native file dialog ON THE SERVER (local machine) and return the
    chosen path -- avoids uploading a multi-GB xlsx through the browser."""
    def _dialog() -> str:
        try:
            import tkinter as tk
            from tkinter import filedialog
            root = tk.Tk()
            root.withdraw()
            root.attributes("-topmost", True)
            path = filedialog.askopenfilename(
                title="Select data file",
                # legacy .xls (BIFF) is not readable by openpyxl: not offered
                filetypes=[("Excel workbooks", "*.xlsx *.xlsm"), ("All files", "*.*")],
            )
            root.destroy()
            return path or ""
        except Exception as e:  # noqa: BLE001
            return f"__ERROR__{e}"

    import asyncio
    path = await asyncio.to_thread(_dialog)
    if path.startswith("__ERROR__"):
        raise HTTPException(500, f"File dialog unavailable: {path[9:]}. Type the path instead.")
    return {"path": path}


@app.post("/api/pick-folder")
async def pick_folder():
    """Native folder dialog on this machine (for the output folder)."""
    def _dialog() -> str:
        try:
            import tkinter as tk
            from tkinter import filedialog
            root = tk.Tk(); root.withdraw(); root.attributes("-topmost", True)
            path = filedialog.askdirectory(title="Choose output folder")
            root.destroy()
            return path or ""
        except Exception as e:  # noqa: BLE001
            return f"__ERROR__{e}"
    import asyncio
    path = await asyncio.to_thread(_dialog)
    if path.startswith("__ERROR__"):
        raise HTTPException(500, f"Folder dialog unavailable: {path[9:]}. Type the path instead.")
    return {"path": path}


@app.post("/api/load")
def load(req: LoadReq):
    path = req.path.strip().strip('"')
    if not path or not os.path.isfile(path):
        raise HTTPException(400, f"File not found: {path}")
    why = _unreadable_workbook(path)
    if why:
        raise HTTPException(400, why)
    sheet_fallback = None
    try:
        with pd.ExcelFile(path, engine="openpyxl") as xls:
            sheets = xls.sheet_names
            sheet = req.sheet if req.sheet not in (None, "") else sheets[0]
            # allow 1-based index like the R side
            if isinstance(sheet, int) and not isinstance(sheet, bool):
                sheet = sheets[sheet - 1] if 1 <= sheet <= len(sheets) else sheets[0]
            elif str(sheet) not in sheets:
                # a sheet name remembered from another workbook: open the first
                # sheet instead of refusing the file, and say so
                sheet_fallback = str(sheet)
                sheet = sheets[0]
            # every cell as stored: pandas' own typing would turn text cells
            # like '007' / '1.50' into numbers and 'NA' / 'None' into missing
            raw = xls.parse(sheet_name=sheet, dtype=object, keep_default_na=False)
    except Exception as e:  # noqa: BLE001
        raise HTTPException(400, f"Could not read Excel: {e}")
    # R reads the sheet with readxl (column types from the cells, trim_ws =
    # TRUE): type it the same way ONCE here, so every preview count, pick-list
    # and filter sees the values the engine sees
    guess_max = int(config_schema.DEFAULTS["guess_max"])
    df = preview.readxl_types(raw, guess_max)

    # guess_max is pinned to this value for every run of this frame (see run())
    STATE.update({"df": df, "path": path, "sheet": sheet, "columns": [str(c) for c in df.columns],
                  "guess_max": guess_max})
    return {
        "path": path,
        "sheets": sheets,
        "sheet": sheet,
        "columns": STATE["columns"],
        "n_rows": int(len(df)),
        "n_cols": int(df.shape[1]),
        "preview": _table(df, max_rows=50),
        # blank / repeated headers shown under generated names ('Unnamed: 3',
        # 'Cyl.1'); the R engine uses the same names
        "renamed_headers": preview.renamed_headers(list(df.columns)),
        # headers that differ only by case / punctuation ('Pre UCVA', 'Pre-UCVA'):
        # each works when picked here, but a preset name matching neither
        # exactly cannot be resolved - rename one in the file
        "similar_headers": preview.similar_headers(list(df.columns)),
        # the requested sheet was not in this workbook (None = it was)
        "sheet_fallback": sheet_fallback,
    }


def _sorted_frame(df: pd.DataFrame, sort: str, dir: str) -> pd.DataFrame:
    """Numeric-aware full-frame sort (NAs last); returns df unchanged if no sort."""
    if not sort or sort not in df.columns:
        return df
    num = preview.as_numeric_safe(df[sort])
    n_vals = int(df[sort].notna().sum())
    if n_vals > 0 and int(num.notna().sum()) >= 0.5 * n_vals:
        key = num
    else:
        key = df[sort].astype("object").astype("string").str.lower()
    idx = key.sort_values(ascending=(dir != "desc"), na_position="last").index
    return df.loc[idx]


@app.get("/api/preview")
def preview_rows(sort: str = "", dir: str = "asc", max_rows: int = 50):
    """First rows of the loaded data, optionally sorted by a column over the
    FULL dataset (numeric-aware; NAs last)."""
    df = _require_df()
    return _table(_sorted_frame(df, sort, dir), max_rows=max_rows)


class FilteredPreviewReq(BaseModel):
    cfg: dict
    sort: str = ""
    dir: str = "asc"
    max_rows: int = 50


@app.post("/api/preview-filtered")
def preview_filtered(req: FilteredPreviewReq):
    """First rows of the data AFTER mutations/exclude/filters/subsets — the
    live 'what will actually be analyzed' table."""
    df = _require_df()
    try:
        work = preview.filtered_frame(df, req.cfg or {})
    except Exception as e:  # noqa: BLE001
        raise HTTPException(400, f"Filtering failed: {e}")
    out = _table(_sorted_frame(work, req.sort, req.dir), max_rows=req.max_rows)
    out["total"] = int(len(work))
    resolver = preview.build_resolver(list(df.columns))
    out["subset_col_missing"] = preview.subset_column_missing(req.cfg or {}, resolver)
    return out


@app.get("/api/distinct")
def distinct(col: str = Query(...), limit: int = 1000, q: str = ""):
    """Distinct values of a column (sorted, at most `limit`; `truncated` says
    there are more). `q` keeps only values containing it (case-insensitive),
    so a value beyond the cap can still be found by searching."""
    df = _require_df()
    resolver = preview.build_resolver(list(df.columns))
    actual = preview.resolve(col, resolver)
    if actual is None or actual not in df.columns:
        return {"values": [], "truncated": False, "total": 0, "resolved": None}
    limit = max(1, min(int(limit), 20000))
    # R-style strings (as.character), so a picked '1' is what the engine compares
    s = preview.as_str(df[actual])
    vals = [v for v in s.dropna().unique().tolist() if v not in ("", " ")]
    needle = (q or "").strip().lower()
    if needle:
        vals = [v for v in vals if needle in v.lower()]
    if preview._is_r_numeric(df[actual]):
        def _num_key(v: str):
            try:
                return (float(v), v)
            except ValueError:
                return (float("inf"), v)
        vals.sort(key=_num_key)
    else:
        vals.sort()
    return {"values": vals[:limit], "truncated": len(vals) > limit, "total": len(vals),
            "resolved": str(actual)}


class HistReq(BaseModel):
    col: str
    cfg: dict | None = None
    bins: int = 20
    group: str | None = None          # restrict the green series to one group
    stage: str = "filtered"           # "filtered" (live) | "matched" (last run)
    matched_file: str | None = None   # which matched-only workbook (if several)
    pairing: str | None = None        # stage=matched: only this pairing's rows


# columns of Matched_Data that belong to the pairing, not to the eye
_PAIRING_COLS = {"Pairing", "distance", "weights", "subclass", ".treat",
                 "PropensityScore", "MatchWeight"}


def _matched_df(matched_file: str | None = None) -> pd.DataFrame:
    """Matched cohort from the last run's Matched-Only workbook(s), cached.
    Exact MatchIt output — includes GroupLabel / weights / Pairing columns."""
    r = STATE.get("result") or {}
    files = r.get("matched_files") or []
    if not files:
        raise HTTPException(400, "No matched output yet — run an analysis with matching enabled first.")
    if matched_file:
        real = {os.path.realpath(f): f for f in files}.get(os.path.realpath(matched_file))
        if real is None:
            raise HTTPException(403, "Not a matched file from the last run.")
        files = [real]
    key = "|".join(files)
    cache = STATE.setdefault("matched_cache", {})
    if key not in cache:
        frames = []
        for f in files:
            try:
                frames.append(pd.read_excel(f, sheet_name="Matched_Data", engine="openpyxl"))
            except Exception:  # noqa: BLE001
                pass
        if not frames:
            raise HTTPException(400, "Matched output could not be read (file moved or deleted?). Re-run the analysis.")
        cache[key] = pd.concat(frames, ignore_index=True)
    return cache[key]


@app.post("/api/histogram")
def histogram(req: HistReq):
    """Distribution of one column for the live analysis-zone panel.

    stage=filtered: light series = rows passing filters/subsets (live pandas
    mirror); stage=matched: light series = the last run's matched cohort.
    With `group`, the green series is that group's rows within the stage cohort.
    """
    bins = max(5, min(req.bins, 60))

    if req.stage == "matched":
        mdf = _matched_df(req.matched_file)
        pairings = (sorted(mdf["Pairing"].astype(str).unique().tolist())
                    if "Pairing" in mdf.columns else [])
        n_stacked = int(len(mdf))
        if req.pairing and "Pairing" in mdf.columns:
            mdf = mdf[mdf["Pairing"].astype(str) == str(req.pairing)]
        # Matched_Data stacks the pairings: a reference eye matched in two
        # pairings is in it twice. Each eye is counted once (as the matched
        # Manifest counts distinct eyes): rows that differ only in the
        # per-pairing columns MatchIt adds are one eye.
        eye_cols = [c for c in mdf.columns if str(c) not in _PAIRING_COLS]
        if eye_cols:
            mdf = mdf.drop_duplicates(subset=eye_cols)
        resolver = preview.build_resolver(list(mdf.columns))
        actual = preview.resolve(req.col, resolver)
        if actual is None or actual not in mdf.columns:
            # Matched_Data names the scale of a VA column in its header
            # (<header>_logMAR / <header>_decimal), and a refraction column
            # transposed to minus cylinder <header>_minus_cyl, as the filtered
            # CSV does
            for sfx in ("_logMAR", "_decimal", "_minus_cyl"):
                actual = preview.resolve(str(req.col) + sfx, resolver)
                if actual is not None and actual in mdf.columns:
                    break
        if actual is None or actual not in mdf.columns:
            raise HTTPException(400, f"Column not present in the matched output: {req.col}")
        kept_series = None
        if req.group and "GroupLabel" in mdf.columns:
            sub = mdf[mdf["GroupLabel"].astype(str).str.strip() == str(req.group)]
            kept_series = sub[actual]
        try:
            out = preview.histogram(mdf[actual], kept_series, bins=bins)
        except Exception as e:  # noqa: BLE001
            raise HTTPException(400, f"Histogram failed: {e}")
        out["pairings"] = pairings
        out["pairing"] = req.pairing or None
        out["distinct_eyes"] = True            # n counts each eye once
        out["n_distinct_eyes"] = int(len(mdf))
        out["n_rows_stacked"] = n_stacked      # Matched_Data rows (all pairings)
        return out

    # ---- stage: filtered (live) ----
    df = _require_df()
    resolver = preview.build_resolver(list(df.columns))
    actual = preview.resolve(req.col, resolver)
    if actual is None or actual not in df.columns:
        raise HTTPException(400, f"Column not found: {req.col}")

    # value mutations (minus-cyl, VA notation / LogMAR) apply to the base series
    # too, so both bars are on the same scale — they never change row counts
    base = df
    cohort = df
    if req.cfg:
        try:
            base = preview.apply_va_columns(preview.apply_mutations(df, req.cfg, resolver),
                                            req.cfg, resolver)
        except Exception:
            base = df
        try:
            cohort = preview.filtered_frame(df, req.cfg)
        except Exception:
            cohort = base

    try:
        if req.group and req.cfg:
            gcol = preview.resolve(req.cfg.get("group_source_var"), resolver)
            vals = (req.cfg.get("groups") or {}).get(req.group) or []
            if gcol is None or gcol not in cohort.columns:
                raise HTTPException(400, "Group source column not found.")
            sub = cohort[preview.group_mask(cohort[gcol], vals).values]
            # light = filtered cohort, green = the chosen group within it
            return preview.histogram(cohort[actual], sub[actual], bins=bins)
        # default: light = full dataset (mutated values), green = filtered rows
        kept_series = cohort[actual] if req.cfg is not None else None
        return preview.histogram(base[actual], kept_series, bins=bins)
    except HTTPException:
        raise
    except Exception as e:  # noqa: BLE001
        raise HTTPException(400, f"Histogram failed: {e}")


def _reference_group(cfg: dict, groups: dict) -> str:
    """The reference group, which must be one of the groups that have values:
    a named group without values would be silently replaced in R."""
    ref = cfg.get("reference_group")
    if ref in (None, ""):
        return next(iter(groups))
    if ref not in groups:
        raise HTTPException(400, f"Reference group '{ref}' has no source values - add values "
                                 "to it or pick another reference group.")
    return ref


@app.post("/api/suggest-covariates")
def suggest_covariates(req: CountsReq):
    """Rank baseline variables by imbalance between groups (max pairwise
    standardized mean difference vs the reference) on the current cohort.
    Suggested = SMD >= 0.1, plus age & sex always."""
    import numpy as np
    df = _require_df()
    cfg = req.cfg or {}
    resolver = preview.build_resolver(list(df.columns))
    groups = {k: v for k, v in (cfg.get("groups") or {}).items() if v}
    gcol = preview.resolve(cfg.get("group_source_var"), resolver)
    if gcol is None or len(groups) < 2:
        raise HTTPException(400, "Define the group source column and at least 2 groups first.")
    ref = _reference_group(cfg, groups)
    work = preview.filtered_frame(df, cfg)
    masks = {g: preview.group_mask(work[gcol], v).values for g, v in groups.items()}

    base = cfg.get("baseline") or {}
    cand = []
    for k in ("continuous", "ordinal", "dichotomous", "categorical"):
        cand += list(base.get(k) or [])
    seen, out = set(), []

    def smd_numeric(a, b):
        a = a[np.isfinite(a)]; b = b[np.isfinite(b)]
        if len(a) < 3 or len(b) < 3: return None
        sp = np.sqrt((np.var(a, ddof=1) + np.var(b, ddof=1)) / 2)
        return abs(np.mean(a) - np.mean(b)) / sp if sp > 0 else None

    import numpy as np  # noqa: F401 (used above via closure)
    for raw in cand:
        if raw in seen: continue
        seen.add(raw)
        col = preview.resolve(raw, resolver)
        if col is None or col not in work.columns: continue
        v = work[col]
        num = preview.as_numeric_safe(v)
        numeric = num.notna().sum() >= 0.5 * max(1, v.notna().sum())
        best = 0.0
        for g in groups:
            if g == ref: continue
            if numeric:
                s = smd_numeric(num.values[masks[ref]], num.values[masks[g]])
                if s is not None: best = max(best, s)
            else:
                sa = preview.as_str(v)
                for lev in sa.dropna().unique()[:12]:
                    pa = float((sa[masks[ref]] == lev).mean())
                    pb = float((sa[masks[g]] == lev).mean())
                    denom = np.sqrt((pa * (1 - pa) + pb * (1 - pb)) / 2)
                    if denom > 0: best = max(best, abs(pa - pb) / denom)
        always = raw in (cfg.get("age_var"), cfg.get("sex_var"))
        out.append({"var": raw, "smd": round(float(best), 3),
                    "suggested": bool(best >= 0.1 or always)})
    out.sort(key=lambda x: -x["smd"])
    return {"reference": ref, "covariates": out}


# ---------------------------------------------------------------- routes: counts
@app.post("/api/preview-counts")
def preview_counts(req: CountsReq):
    df = _require_df()
    try:
        return preview.compute_counts(df, req.cfg)
    except Exception as e:  # noqa: BLE001
        raise HTTPException(400, f"Preview failed: {e}")


class MatchPreviewReq(BaseModel):
    cfg: dict


def _text_columns(cols: list, frame: pd.DataFrame | None = None) -> set:
    """Which of `cols` (loaded-frame column names) are text in R. The loaded
    frame is typed like readxl (/api/load), so its text columns are exactly
    R's character columns as read; pass `frame` (the whole sheet after the
    value mutations) for the types the run has when it matches."""
    df = _require_df() if frame is None else frame
    return preview.r_text_columns(df, [c for c in cols if c in df.columns])


@app.post("/api/match-preview")
def match_preview(req: MatchPreviewReq):
    """Live matched-count preview: real MatchIt on the current cohort (slim
    CSVs: GroupLabel + PSM covariates only), plus per-covariate missingness so
    complete-case losses are explained."""
    df = _require_df()
    cfg = req.cfg or {}
    if not cfg.get("do_matching"):
        raise HTTPException(400, "Matching is disabled.")
    why = config_schema.psm_method_problem(cfg) or \
        config_schema.psm_distance_problem({**config_schema.DEFAULTS, **cfg})
    if why:
        raise HTTPException(400, why)
    if config_schema.psm_method(cfg) != "nearest":
        return {"unsupported": cfg.get("psm_method"),
                "message": "Live preview supports the 'nearest' method only (others are too slow to preview)."}
    psm_errs = [e for e in config_schema.validate_raw(cfg) if not e.startswith("Filter on")]
    if psm_errs:
        raise HTTPException(400, " ".join(psm_errs))
    # the same coercion the run gets (empty caliper = no caliper, ...)
    psm = config_schema.normalize_psm({**config_schema.DEFAULTS, **cfg})
    resolver = preview.build_resolver(list(df.columns))
    if preview.subset_column_missing(cfg, resolver):
        raise HTTPException(400, f"Subset column '{cfg.get('subset_col_raw')}' not found in this file. "
                                 "Re-map it or clear the subset column.")
    covs = [c for c in (preview.resolve(x, resolver) for x in (cfg.get("psm_covariates_raw") or [])) if c is not None]
    if not covs:
        raise HTTPException(400, "Select matching covariates first.")
    gcol = preview.resolve(cfg.get("group_source_var"), resolver)
    groups = {k: v for k, v in (cfg.get("groups") or {}).items() if v}
    if gcol is None or len(groups) < 2:
        raise HTTPException(400, "Define the group source column and at least 2 groups.")
    reference = _reference_group(cfg, groups)

    work = preview.filtered_frame(df, cfg)
    # assign group labels
    label = pd.Series(pd.NA, index=work.index, dtype="object")
    for gname, vals in groups.items():
        label[preview.group_mask(work[gcol], vals).values] = gname
    work = work[label.notna().values].copy()
    work["__grp"] = label.dropna().values

    # runs (same semantics as pipeline build_runs)
    subset_col = preview.resolve(cfg.get("subset_col_raw"), resolver)
    subsets = cfg.get("subsets") or {}
    mode = preview.subset_mode(cfg)       # as normalize_cfg gives it to the run
    runs: list[tuple[str, pd.DataFrame]] = []
    if subset_col is not None and subset_col in work.columns and subsets:
        if mode in ("pooled", "both"):
            runs.append(("Pooled", work))          # filtered_frame already union-restricted
        if mode in ("separate", "both"):
            for sname, svals in subsets.items():
                runs.append((sname, work[preview.match_values(work[subset_col], svals).values]))
    else:
        runs.append(("All", work))

    pid_col = preview.resolve(cfg.get("patient_id_raw"), resolver)
    # covariates readxl reads as text are judged on the whole sheet column,
    # typed as the run types it before matching: after the plus-to-minus
    # cylinder standardisation (its sphere / cylinder / axis columns become
    # numbers, as standardize_minus_cyl() makes them in R); VA columns are
    # numbers by now too (filtered_frame -> apply_va_columns, as
    # set_va_analysis_columns does before the run matches), never text
    va_cols = set(preview.va_columns(cfg, resolver)[0])
    text_covs = _text_columns([c for c in covs if c not in va_cols],
                              preview.apply_mutations(df, cfg, resolver))
    # treated groups in the configured group order (the run's order), not
    # in the order the rows happen to list them
    treated_order = list(groups)
    run_refs: dict[str, str] = {}                   # run label -> its reference group
    stamp = datetime.now().strftime("%H%M%S%f")
    prev_dir = MATCHPREV_DIR             # this window's own (see _housekeep_runs)
    os.makedirs(prev_dir, exist_ok=True)
    # one CSV per PAIRING (reference + one treated group), because the run
    # decides per pairing how a text covariate enters the model
    r_runs, missing_info, run_frames = [], {}, {}
    pair_keys: dict[str, tuple[str, str]] = {}      # R run label -> (run label, treated)
    local_pairings: dict[tuple[str, str], dict] = {}
    id_tokens: dict[str, str] = {}
    id_back: dict[str, str] = {}
    for label_r, frame in runs:
        run_frames[label_r] = frame
        if pid_col and pid_col in frame.columns:
            # blank ids: one patient per eye, named like the pipeline does
            pid = preview.as_str(frame[pid_col]).str.strip()
            miss = pid.isna() | (pid == "")
            rown = pd.Series([f"(no ID) row {int(i) + 1}" for i in frame.index], index=frame.index)
            pid = pid.where(~miss, rown)
        else:
            pid = None
        # the run's reference, as add_group_label (R/pipeline.R) picks it: the
        # configured one when it has rows in this run, else the first defined
        # group that has rows
        present = [g for g in groups if (frame["__grp"] == g).any()]
        run_ref = reference if reference in present or not present else present[0]
        run_refs[label_r] = run_ref
        for tg in [g for g in groups if g != run_ref and g in present]:
            pm = frame["__grp"].isin([run_ref, tg]).values
            sub = frame[pm]
            enc, dropped, cat = preview.psm_covariates(sub, covs, text_covs)
            if enc.shape[1] == 0:
                n_t = int((sub["__grp"] == tg).sum())
                local_pairings[(label_r, tg)] = {
                    "treated": tg, "reference": run_ref,
                    "treated_total": n_t, "treated_cc": 0,
                    "ref_total": int(len(sub)) - n_t, "ref_cc": 0,
                    "matched_treated": None, "controls_used": None,
                    "error": "No usable matching covariate (text columns with many distinct values "
                             "are excluded: " + ", ".join(map(str, dropped)) + ")."}
                continue
            slim = enc.copy()
            # match_preview.R takes ONE reference name for every pairing: a run
            # with another reference labels it with that name (it has no rows
            # of its own here), and the answer is relabelled below
            slim.insert(0, "GroupLabel", sub["__grp"].replace({run_ref: reference}).values
                        if run_ref != reference else sub["__grp"].values)
            # RowID = 0-based position in the RUN frame (maps R's rows back)
            slim.insert(0, "RowID", [int(i) for i in pm.nonzero()[0]])
            if pid is not None:
                # opaque tokens: read.csv would turn '007' into 7
                toks = []
                for p_ in pid[pm].tolist():
                    t_ = id_tokens.get(p_)
                    if t_ is None:
                        t_ = id_tokens[p_] = f"id{len(id_tokens)}"
                        id_back[t_] = p_
                    toks.append(t_)
                slim.insert(0, "PatientID", toks)
            key = f"p{len(pair_keys)}"
            pair_keys[key] = (label_r, tg)
            csv_path = os.path.join(prev_dir, f"mp_{stamp}_{key}.csv")
            # 'NA' (not an empty field) so read.csv sees missing text as missing
            slim.to_csv(csv_path, index=False, encoding="utf-8-sig", na_rep="NA")
            r_runs.append({"label": key, "csv": csv_path, "text_covs": cat,
                           "excluded_covs": dropped})
        # per-covariate missingness by group (explains complete-case loss)
        mi = {}
        for gname in groups:
            gmask = (frame["__grp"] == gname).values
            per_cov = {c: int(preview._is_missing(frame.loc[gmask, c]).sum()) for c in covs}
            mi[gname] = {"n": int(gmask.sum()),
                         "missing": {k: v for k, v in per_cov.items() if v > 0}}
        missing_info[label_r] = mi

    rscript = rrunner.find_rscript()
    if not rscript:
        raise HTTPException(500, "Rscript not found.")
    D = config_schema.DEFAULTS
    pcfg = {"reference_group": reference,
            "psm": {"method": config_schema.psm_method(cfg),
                    "distance": config_schema.psm_distance({**D, **cfg}),
                    "ratio": psm["psm_ratio"], "replace": bool(cfg.get("psm_replace", D["psm_replace"])),
                    "caliper": psm["psm_caliper"],
                    "std_caliper": bool(cfg.get("psm_std_caliper", D["psm_std_caliper"])),
                    "m_order": cfg.get("psm_m_order", D["psm_m_order"]),
                    "seed": psm["psm_seed"]},
            "runs": r_runs}
    pcfg_path = os.path.join(prev_dir, f"mp_{stamp}.json")
    with open(pcfg_path, "w", encoding="utf-8") as fh:
        json.dump(pcfg, fh)

    r_result = {"runs": []}
    if r_runs:
        import subprocess
        wrapper = os.path.join(APP_ROOT, "engine", "match_preview.R")
        try:
            # read as UTF-8 (a Hebrew group name), and on timeout the whole
            # Rscript -> Rterm tree is killed, so the 504 comes on time
            proc = rrunner.run_captured([rscript, "--vanilla", wrapper, pcfg_path],
                                        cwd=APP_ROOT, timeout=300)
        except subprocess.TimeoutExpired:
            raise HTTPException(504, "Matching preview timed out (5 min).")
        finally:
            for f in [pcfg_path] + [r["csv"] for r in r_runs]:
                try: os.remove(f)
                except OSError: pass
        r_result = None
        for line in (proc.stdout or "").splitlines():
            if line.startswith("RESULT "):
                r_result = json.loads(line[len("RESULT "):])
            elif line.startswith("ERROR "):
                raise HTTPException(400, "Preview failed: " + line[6:])
        if r_result is None:
            raise HTTPException(400, "Preview produced no result. "
                                + ((proc.stdout or "")[-300:] or (proc.stderr or "")[-300:]))
    else:
        try: os.remove(pcfg_path)
        except OSError: pass

    # one R "run" per pairing: regroup the pairings under their run labels,
    # in the run order and treated-group order of the frames
    by_pair: dict[tuple[str, str], dict] = dict(local_pairings)
    for rr in r_result.get("runs") or []:
        key = pair_keys.get(rr.get("label"))
        if key is None:
            continue
        for p in rr.get("pairings") or []:
            if unicodedata.normalize("NFC", str(p.get("treated"))) == \
                    unicodedata.normalize("NFC", str(key[1])):
                p["treated"] = key[1]           # the spelling the interface sent
                for k in ("unmatched_treated_ids", "unmatched_control_ids"):
                    if isinstance(p.get(k), list):
                        p[k] = [id_back.get(str(t), str(t)) for t in p[k]]
                p["reference"] = run_refs.get(key[0], reference)
                by_pair[key] = p
    out_runs = []
    for label_r, frame in runs:
        pairings = [by_pair[(label_r, tg)] for tg in treated_order if (label_r, tg) in by_pair]
        run_ref = run_refs.get(label_r, reference)
        entry = {"label": label_r, "pairings": pairings, "reference": run_ref}
        if run_ref != reference:
            entry["note"] = (f"The reference group '{reference}' has no rows in run '{label_r}' - "
                             f"'{run_ref}' is the reference there, as in the run.")
        out_runs.append(entry)
    result = {"runs": out_runs, "missing_info": missing_info}

    # live matched-data preview: map R's matched RowIDs back onto the run frames
    matched_preview = {}
    for run in result.get("runs", []):
        frame = run_frames.get(run.get("label"))
        if frame is None:
            continue
        parts = []
        for p in run.get("pairings", []):
            rows = p.pop("matched_rows", None) or []
            ws = p.pop("matched_w", None) or []
            pss = p.pop("matched_ps", None) or []
            if not rows:
                continue
            sel = frame.iloc[rows].copy()
            n = len(sel)
            sel.insert(0, "PropensityScore", (list(pss) + [None] * n)[:n])
            sel.insert(0, "MatchWeight", (list(ws) + [None] * n)[:n])
            sel.insert(0, "GroupLabel", frame["__grp"].iloc[rows].values)
            sel.insert(0, "Pairing", f"{p.get('treated')}_vs_{p.get('reference')}")
            parts.append(sel)
        if parts:
            allm = pd.concat(parts).drop(columns=["__grp"], errors="ignore")
            tbl = _table(allm, max_rows=50)
            tbl["shown"] = min(50, len(allm))
            tbl["total"] = int(len(allm))
            matched_preview[run["label"]] = tbl
    result["matched_preview"] = matched_preview
    return result


# ---------------------------------------------------------------- routes: run
def _check_output_dir(path: str) -> str:
    """The output folder must exist (or be creatable) and be writable, else
    R 'saves' workbooks that never reach the disk."""
    if not path:
        raise HTTPException(400, "No output folder given.")
    if not os.path.isabs(path):
        path = os.path.join(APP_ROOT, path)    # R resolves it from the app folder
    path = os.path.normpath(path)
    try:
        os.makedirs(path, exist_ok=True)
        probe = os.path.join(path, f".se_write_test_{os.getpid()}_{time.time_ns()}")
        with open(probe, "w", encoding="utf-8") as fh:
            fh.write("ok")
        os.remove(probe)
    except OSError as e:
        reason = e.strerror or str(e)
        raise HTTPException(400, f"Output folder is not writable: {path} ({reason}). "
                                 "Choose another folder in Output settings.")
    return path


def _verify_result(data: dict) -> list[str]:
    """Drop listed outputs that do not exist on disk; return the missing ones."""
    missing: list[str] = []
    for k in ("files", "matched_files"):
        keep = []
        for f in data.get(k) or []:
            (keep if isinstance(f, str) and os.path.isfile(f) else missing).append(f)
        data[k] = keep
    data["graph_dirs"] = [d for d in (data.get("graph_dirs") or []) if isinstance(d, str) and os.path.isdir(d)]
    return missing


_STAMP_RE = re.compile(r"_(\d{8}_\d{6})\.xlsx$", re.IGNORECASE)


def _run_stamps(data: dict) -> set[str]:
    """The time stamps in the names of the run's workbooks (every file of one
    run carries the same stamp: workbooks, love plots, filtered CSV)."""
    out = set()
    for f in list(data.get("files") or []) + list(data.get("matched_files") or []):
        m = _STAMP_RE.search(os.path.basename(str(f)))
        if m:
            out.add(m.group(1))
    return out


_LOVEPLOT_EXTS = (".png", ".tiff", ".jpeg", ".pdf", ".eps", "_legend.txt")


def _run_sidecar_names(data: dict, folder: str) -> tuple[list[str], list[str]]:
    """(love plots, CSVs) in `folder` that the run of `data` wrote:
    LovePlot_<prefix>_..._<stamp>.png, Filtered_Dataset_<stamp>.csv and the
    astigmatism tables' copy <prefix>_<run stem>_Astigmatism_<stamp>.csv
    (decision D8) with this run's stamp (and output prefix, when known) -
    never the files of an earlier run into the same folder."""
    stamps = _run_stamps(data)
    prefix = str(data.get("output_prefix") or "")
    # the workbooks' names without the stamp: '<prefix>_<run stem>'
    heads = []
    for f in data.get("files") or []:
        m = _STAMP_RE.search(os.path.basename(str(f)))
        if m:
            heads.append(os.path.basename(str(f))[:m.start()])
    try:
        names = sorted(os.listdir(folder))
    except OSError:
        return [], []
    loves, csvs = [], []
    for fn in names:
        if not os.path.isfile(os.path.join(folder, fn)):
            continue
        low = fn.lower()
        # the love plot in every chosen figure format, and its legend file
        # (journal figure style): LovePlot_..._<stamp>.<ext> / _<stamp>_legend.txt
        if fn.startswith("LovePlot_") and any(
                low.endswith(f"_{s}{e}") for s in stamps for e in _LOVEPLOT_EXTS) and (
                not prefix or fn.startswith(f"LovePlot_{prefix}_")
                or any(fn.startswith(f"LovePlot_{h}_") for h in heads)):
            loves.append(fn)
        elif any(fn == f"Filtered_Dataset_{s}.csv" for s in stamps) or any(
                fn == f"{h}_Astigmatism_{s}.csv" for h in heads for s in stamps):
            csvs.append(fn)
    return loves, csvs


def _run_loveplots(data: dict) -> list[str]:
    """Paths of the love plots the run of `data` wrote (next to its workbooks)."""
    out: list[str] = []
    dirs = []
    for f in list(data.get("files") or []) + list(data.get("matched_files") or []):
        d = os.path.dirname(str(f))
        if d and d not in dirs:
            dirs.append(d)
    for d in dirs:
        out += [os.path.join(d, fn) for fn in _run_sidecar_names(data, d)[0]
                if not fn.lower().endswith("_legend.txt")]       # the figures, not their legends
    return out


def _analysed_rows(df: pd.DataFrame, cfg: dict, subsets: bool = True) -> pd.DataFrame:
    """The rows left after exclusions, filters and the subset union, with
    the acuity as recorded (the rows the engine assigns to groups).
    subsets=False: before the subset restriction - the rows on which R
    refuses a value that puts a row into two groups (pipeline.R step D)."""
    try:
        return preview.filtered_frame(df, cfg, va=False, subsets=subsets)
    except Exception:  # noqa: BLE001  (the mirror must never block a valid run)
        return df


def _grouped_rows(frame: pd.DataFrame, cfg: dict) -> pd.DataFrame:
    """The rows of `frame` that belong to a group (the others are dropped by
    add_group_label before anything is analysed)."""
    try:
        gcol = preview.resolve(cfg.get("group_source_var"), preview.build_resolver(list(frame.columns)))
        groups = {k: v for k, v in (cfg.get("groups") or {}).items() if v}
        if gcol is None or gcol not in frame.columns or not groups:
            return frame
        keep = pd.Series(False, index=frame.index)
        for vals in groups.values():
            keep = keep | preview.group_mask(frame[gcol], vals)
        return frame[keep.values]
    except Exception:  # noqa: BLE001
        return frame


def _run_plan_check(df: pd.DataFrame, cfg: dict) -> tuple[list[str], list[str]]:
    """(errors, notes) from the rows each planned run will hold: a run needs
    two groups with rows, so a configuration where NO run has two is refused;
    runs that will be skipped, or that lack the reference group, are named."""
    try:
        plan = preview.run_group_counts(df, cfg)
    except Exception:  # noqa: BLE001  (the mirror must never block a valid run)
        return [], []
    if not plan:
        return [], []
    groups = [g for g, v in (cfg.get("groups") or {}).items() if v]
    ref = cfg.get("reference_group") or (groups[0] if groups else "")

    def present(counts: dict) -> list[str]:
        return [g for g in groups if counts.get(g, 0) > 0]

    def describe(counts: dict) -> str:
        return ", ".join(f"{g} {counts.get(g, 0)}" for g in groups)

    ok = [(lab, c) for lab, c in plan if len(present(c)) >= 2]
    if not ok:
        return [("No analysis can be made: after exclusions, filters and subsets no run has two "
                 "groups with rows (" + "; ".join(f"{lab}: {describe(c)}" for lab, c in plan)
                 + "). Check the group values and filters.")], []
    notes = []
    for lab, c in plan:
        if len(present(c)) < 2:
            notes.append(f"Run '{lab}' will be skipped: fewer than two groups have rows "
                         f"({describe(c)}).")
        elif ref and c.get(ref, 0) == 0:
            notes.append(f"Run '{lab}': the reference group '{ref}' has no rows, so another "
                         "group serves as reference in this run (see the run log).")
    return [], notes


@app.post("/api/run")
def run(req: RunReq, request: Request):
    if STATE["path"] is None:
        raise HTTPException(400, "Load a data file first.")
    errs = config_schema.validate_raw(req.cfg)
    cfg = config_schema.normalize_cfg(req.cfg, STATE["path"], STATE.get("sheet"))
    notes: list[str] = []
    # the loaded frame was typed with STATE['guess_max']: R must type the
    # sheet the same way, or a column the preview reads as text is numeric
    # in the run (its text cells silently missing)
    gm = STATE.get("guess_max") or config_schema.DEFAULTS["guess_max"]
    try:
        asked = int(float(cfg.get("guess_max")))
    except (TypeError, ValueError):
        asked = gm
    if asked != gm:
        notes.append(f"The settings asked for guess_max {cfg.get('guess_max')}; the file is read "
                     f"with guess_max {gm}, the value the app typed the loaded columns with, so "
                     "the run sees the same column types as every preview.")
    cfg["guess_max"] = gm
    # one message per bad filter: the raw check already named it more precisely
    flagged = {e.split("': ", 1)[0] for e in errs if e.startswith("Filter on '")}
    columns = list(STATE["df"].columns) if STATE["df"] is not None else None
    analysed = _analysed_rows(STATE["df"], cfg) if STATE["df"] is not None else None
    # group values are judged on every row left after exclusions and filters,
    # BEFORE the subset restriction, as R refuses them (pipeline.R step D)
    all_rows = analysed
    if STATE["df"] is not None and cfg.get("subset_col_raw") and cfg.get("subsets"):
        all_rows = _analysed_rows(STATE["df"], cfg, subsets=False)
    errs += [e for e in config_schema.validate_cfg(cfg, columns, all_rows)
             if not (e.startswith("Filter on '") and e.split("': ", 1)[0] in flagged)]
    if STATE["df"] is not None:
        resolver = preview.build_resolver(columns)
        if preview.subset_column_missing(cfg, resolver):
            errs.append(f"Subset column '{cfg.get('subset_col_raw')}' not found in this file. "
                        "Re-map it or clear the subset column to analyse all rows.")
        grouped = _grouped_rows(analysed, cfg)
        if not errs:
            # R stops on a VA notation the values contradict, judged on the
            # rows that remain after exclusions, filters and grouping (the
            # rows it analyses): say so before it starts
            errs += preview.va_format_problems(grouped, cfg)
        notes += preview.va_format_notes(grouped, cfg)
        if not errs:
            errs, plan_notes = _run_plan_check(STATE["df"], cfg)
            notes += plan_notes
        notes += config_schema.column_problems(cfg, columns)[1]
        notes += config_schema.run_notes(cfg, columns)
    if errs:
        raise HTTPException(400, " ".join(errs))
    if not req.preview_only:
        cfg["output_dir"] = _check_output_dir(cfg.get("output_dir"))

    rscript = rrunner.find_rscript()
    if not rscript:
        raise HTTPException(500, "Rscript not found. Install R or set RSCRIPT_PATH.")

    # one run at a time: a second R process would write into the same folder
    # and race the first for the results
    with _RUN_LOCK:
        cur = STATE.get("run")
        if cur is not None and cur.active():
            raise HTTPException(409, "An analysis is already running. Wait for it to finish or cancel it.")
        handle = rrunner.RunHandle()
        STATE["run"] = handle

    try:
        stamp = datetime.now().strftime("%Y%m%d_%H%M%S_%f")

        # a failed run must never leave the PREVIOUS run's results answerable
        STATE["result"] = None
        STATE["matched_cache"] = {}
        STATE["preview_only"] = req.preview_only
        staging = None
        if req.preview_only:
            # keep results out of the user's output folder: run into a staging dir
            staging = os.path.join(RUNS_DIR, f"preview_p{PID}_{stamp}")
            os.makedirs(staging, exist_ok=True)
            STATE["staging_dir"] = staging
            cfg["output_dir"] = staging
        else:
            STATE["staging_dir"] = None
        cfg["app_version"] = APP_VERSION          # build id, recorded in the workbook Manifest
        cfg["public_version"] = PUBLIC_VERSION    # release number, recorded next to it
        # a preview run's config (it names the data file) lives and dies with
        # its staging folder; saved runs keep theirs in .runs (newest 20)
        cfg_path = os.path.join(staging or RUNS_DIR, f"config_{stamp}.json")
        with open(cfg_path, "w", encoding="utf-8") as fh:
            json.dump(cfg, fh, indent=2)
        _housekeep_runs(keep_dir=staging)
    except Exception:
        handle.finished = time.time()
        raise

    async def event_stream():
        try:
            yield _sse({"type": "log", "message": f"Using Rscript: {rscript}"})
            for n in notes:
                yield _sse({"type": "log", "message": "Note: " + n})
            async for ev in rrunner.stream_pipeline(rscript, WRAPPER, cfg_path, APP_ROOT,
                                                    handle=handle,
                                                    is_disconnected=request.is_disconnected):
                if ev["type"] == "result":
                    data = ev["data"] or {}
                    # guard against scalar-vs-array JSON from the R side
                    for k in ("files", "matched_files", "graph_dirs"):
                        v = data.get(k)
                        if isinstance(v, str):
                            data[k] = [v] if v else []
                        elif v is None:
                            data[k] = []
                    ev["data"] = data
                    missing = _verify_result(data)
                    # the love plots of THIS run (R does not list them): named
                    # LovePlot_<prefix>_..._<stamp>.png next to its workbooks
                    data["output_prefix"] = cfg.get("output_prefix")
                    data["loveplots"] = _run_loveplots(data)
                    STATE["result"] = data
                    STATE["matched_cache"] = {}
                    yield _sse(ev)
                    if missing:
                        yield _sse({"type": "error", "message":
                                    "The run reported output that was not written to disk: "
                                    + "; ".join(missing)
                                    + ". Check that the output folder is writable and the files are not open in Excel."})
                    elif not data["files"]:
                        yield _sse({"type": "error", "message": "The run finished without writing any workbook."})
                    continue
                if ev["type"] == "done" and handle.cancelled:
                    STATE["result"] = None
                    yield _sse({"type": "error", "message": "Analysis cancelled. Nothing from this run is shown."})
                yield _sse(ev)
        finally:
            handle.finished = time.time()

    return StreamingResponse(
        event_stream(),
        media_type="text/event-stream",
        headers={"Cache-Control": "no-cache", "X-Accel-Buffering": "no", "Connection": "keep-alive"},
    )


@app.post("/api/cancel")
def cancel():
    """Stop the running analysis (the R process and its children)."""
    h = STATE.get("run")
    if h is None or not h.active():
        return {"ok": True, "running": False, "terminated": False}
    h.cancelled = True
    terminated = h.terminate()
    return {"ok": True, "running": True, "terminated": terminated}


def _sse(obj: dict) -> str:
    return f"data: {json.dumps(obj)}\n\n"


# ---------------------------------------------------------------- routes: results
@app.post("/api/export")
def export_files(req: ExportReq):
    """Copy chosen result workbooks (from a preview-only run) to a destination
    folder, optionally with sidecar files (love plots, filtered CSV)."""
    r = STATE.get("result") or {}
    allowed = {os.path.realpath(f): f
               for f in list(r.get("files", [])) + list(r.get("matched_files", []))}
    dest = os.path.expandvars(os.path.expanduser((req.dest or "").strip().strip('"')))
    if not dest:
        raise HTTPException(400, "No destination folder given.")
    try:
        os.makedirs(dest, exist_ok=True)
    except OSError as e:
        raise HTTPException(400, f"Cannot create the destination folder {dest} "
                                 f"({e.strerror or e}). Choose another folder.")

    def _why(e: OSError) -> str:
        # Windows reports a workbook open in Excel as a sharing violation
        if isinstance(e, PermissionError) or getattr(e, "winerror", None) in (32, 33):
            return "open in another program (Excel?) or not writable - close it and retry"
        return e.strerror or str(e)

    def _same(a: str, b: str) -> bool:
        return os.path.normcase(os.path.realpath(a)) == os.path.normcase(os.path.realpath(b))

    saved: list[str] = []
    failed: list[str] = []
    src_dirs = set()
    for f in req.files:
        real = os.path.realpath(f)
        if real not in allowed:
            raise HTTPException(403, f"Not a file from the last run: {os.path.basename(f)}")
        if not os.path.exists(real):
            raise HTTPException(404, f"File no longer exists: {os.path.basename(f)}")
        target = os.path.join(dest, os.path.basename(real))
        src_dirs.add(os.path.dirname(real))
        if _same(target, real):            # already in the destination folder
            saved.append(target)
            continue
        try:
            shutil.copy2(real, target)
        except OSError as e:
            failed.append(f"{os.path.basename(real)} ({_why(e)})")
            continue
        saved.append(target)
    if req.include_sidecars:
        # only this run's sidecars: its graph folders, its love plots and its
        # filtered CSV - never those an earlier run left in the same folder
        run_graphs = {os.path.normcase(os.path.realpath(g)) for g in (r.get("graph_dirs") or [])}
        for sd in src_dirs:
            if _same(sd, dest):
                continue
            try:
                names = os.listdir(sd)
            except OSError:
                continue
            loves, csvs = _run_sidecar_names(r, sd)
            files_wanted = set(loves) | set(csvs)
            for fn in names:
                fp = os.path.join(sd, fn)
                try:
                    if os.path.isdir(fp) and fn.startswith("Graphs_") and \
                            os.path.normcase(os.path.realpath(fp)) in run_graphs:
                        shutil.copytree(fp, os.path.join(dest, fn), dirs_exist_ok=True)
                        saved.append(os.path.join(dest, fn))
                    elif fn in files_wanted:
                        shutil.copy2(fp, os.path.join(dest, fn))
                        saved.append(os.path.join(dest, fn))
                except shutil.Error:             # copytree: some files not copied
                    failed.append(f"{fn} (partly copied - a file is open or not writable)")
                except OSError as e:
                    failed.append(f"{fn} ({_why(e)})")
    if failed and not saved:
        raise HTTPException(409, "Could not save: " + "; ".join(failed))
    return {"saved": saved, "failed": failed, "dest": dest}


@app.post("/api/export-filtered")
def export_filtered(req: ExportFilteredReq):
    """Write the live-filtered dataset (standardize -> exclude -> filters ->
    subsets) as CSV, independent of running the analysis."""
    df = _require_df()
    try:
        work = preview.filtered_frame(df, req.cfg or {})
    except Exception as e:  # noqa: BLE001
        raise HTTPException(400, f"Filtering failed: {e}")
    dest = (req.dest or "").strip() or (req.cfg or {}).get("output_dir") or \
        os.path.join(os.path.expanduser("~"), "StandardEyes_Output")
    dest = os.path.expandvars(os.path.expanduser(str(dest).strip().strip('"')))
    stamp = datetime.now().strftime("%Y%m%d_%H%M%S")
    path = os.path.join(dest, f"Filtered_Dataset_live_{stamp}.csv")
    try:
        os.makedirs(dest, exist_ok=True)
        # utf-8-sig so Excel opens non-ASCII content correctly
        work.to_csv(path, index=False, encoding="utf-8-sig")
    except OSError as e:
        raise HTTPException(400, f"Could not write {path} ({e.strerror or e}). "
                                 "Choose a folder you can write to.")
    return {"path": path, "rows": int(len(work)), "cols": int(work.shape[1])}


@app.get("/api/results")
def results():
    r = STATE.get("result")
    if not r:
        return {"files": [], "matched_files": [], "output_dir": None, "preview_only": False}
    out = {"output_dir": r.get("output_dir"), "files": [], "matched_files": [],
           "graph_dirs": [], "preview_only": bool(STATE.get("preview_only")),
           # the balance (love) plots this run wrote, whatever their names
           "loveplots": [{"path": p, "name": os.path.basename(p)}
                         for p in _run_loveplots(r) if os.path.isfile(p)]}
    for gd in r.get("graph_dirs", []):
        try:
            # figure files only: the _preview folder holds low-res copies, and
            # the <figure>_legend.txt files of the journal style are not figures
            n_files = (len([f for f in os.listdir(gd) if os.path.isfile(os.path.join(gd, f))
                            and not f.lower().endswith("_legend.txt")])
                       if os.path.isdir(gd) else 0)
        except OSError:
            n_files = 0
        out["graph_dirs"].append({"path": gd, "name": os.path.basename(gd), "n_files": n_files})
    for key in ("files", "matched_files"):
        for f in r.get(key, []):
            sheets = _sheet_names(f)
            out[key].append({"path": f, "name": os.path.basename(f), "sheets": sheets})
    return out


@app.get("/api/results/sheet")
def results_sheet(file: str = Query(...), sheet: str = Query(...), max_rows: int = 500):
    real = _safe_output_path(file)
    max_rows = max(1, min(int(max_rows), 5000))
    try:
        # parse only the rows shown: a 20,000-row Matched_Data sheet otherwise
        # blocks the worker for seconds on every click
        df = pd.read_excel(real, sheet_name=sheet, engine="openpyxl", nrows=max_rows)
    except Exception as e:  # noqa: BLE001
        raise HTTPException(400, f"Could not read sheet: {e}")
    return _table(df, max_rows=max_rows)


def _sheet_names(path: str) -> list[str]:
    """Sheet names without loading every cell of the workbook."""
    try:
        import openpyxl  # noqa: PLC0415
        wb = openpyxl.load_workbook(path, read_only=True)
        try:
            return list(wb.sheetnames)
        finally:
            wb.close()
    except Exception:  # noqa: BLE001
        return []


@app.get("/api/download")
def download(file: str = Query(...)):
    real = _safe_output_path(file)
    if not os.path.exists(real):
        raise HTTPException(404, "File not found.")
    return FileResponse(real, filename=os.path.basename(real))


def _graph_dirs() -> list[str]:
    r = STATE.get("result") or {}
    return [d for d in (r.get("graph_dirs") or []) if isinstance(d, str) and os.path.isdir(d)]


@app.get("/api/figures")
def figures():
    """Low-resolution PNG copies of the last run's composite figures
    (<Graphs dir>/_preview/*.png, written by graphs.R whatever the formats)."""
    dirs = []
    for gd in _graph_dirs():
        pdir = os.path.join(gd, "_preview")
        # no preview copies (an older engine): the PNG / JPEG figures themselves
        src, exts = (pdir, (".png",)) if os.path.isdir(pdir) else (gd, (".png", ".jpg", ".jpeg"))
        previews = []
        try:
            names = sorted(os.listdir(src))
        except OSError:
            names = []
        for fn in names:
            fp = os.path.join(src, fn)
            if fn.lower().endswith(exts) and os.path.isfile(fp):
                previews.append({"name": os.path.splitext(fn)[0],
                                 "url": "/api/figure?path=" + quote(fp, safe="")})
        dirs.append({"name": os.path.basename(gd), "path": gd, "previews": previews})
    return {"dirs": dirs}


@app.get("/api/figure")
def figure(path: str = Query(...)):
    """One figure file; it must lie inside a graph folder of the last run."""
    real = os.path.realpath(path)
    for gd in _graph_dirs():
        base = os.path.realpath(gd)
        try:
            inside = os.path.commonpath([real, base]) == base
        except ValueError:          # different drives
            inside = False
        if inside and real != base:
            if not os.path.isfile(real):
                raise HTTPException(404, "Figure not found.")
            media = {".png": "image/png", ".jpg": "image/jpeg", ".jpeg": "image/jpeg",
                     ".tif": "image/tiff", ".tiff": "image/tiff", ".pdf": "application/pdf"}
            return FileResponse(real, media_type=media.get(os.path.splitext(real)[1].lower()),
                                headers={"Cache-Control": "no-cache"})
    raise HTTPException(403, "Not a figure from the last run.")


# ---------------------------------------------------------------- routes: presets
@app.get("/api/presets")
def list_presets():
    names = []
    for fn in sorted(os.listdir(PRESETS_DIR)):
        if fn.lower().endswith(".json"):
            names.append(os.path.splitext(fn)[0])
    return {"presets": names}


@app.get("/api/presets/{name}")
def get_preset(name: str):
    safe = _safe_preset_name(name)
    fp = os.path.join(PRESETS_DIR, safe + ".json")
    if not safe or not os.path.exists(fp):
        raise HTTPException(404, "Preset not found.")
    with open(fp, encoding="utf-8") as fh:
        return {"name": safe, "cfg": json.load(fh)}


@app.post("/api/presets")
def save_preset(req: PresetReq):
    name = _safe_preset_name(req.name)
    if not name:
        raise HTTPException(400, "Invalid preset name.")
    # Windows file names ignore case: 'Study' and 'study' are one preset, and
    # the file keeps the spelling it was created with - answer with that name
    try:
        on_disk = [os.path.splitext(fn)[0] for fn in os.listdir(PRESETS_DIR)
                   if fn.lower() == (name + ".json").lower()]
    except OSError:
        on_disk = []
    if on_disk:
        name = on_disk[0]
    fp = os.path.join(PRESETS_DIR, name + ".json")
    existed = os.path.exists(fp)
    if existed and req.overwrite is False:
        raise HTTPException(409, f"A preset named '{name}' already exists.")
    try:
        with open(fp, "w", encoding="utf-8") as fh:
            json.dump(req.cfg, fh, indent=2)
    except OSError as e:
        raise HTTPException(400, f"Could not save the preset ({e.strerror or e}). The StandardEyes "
                                 "folder must be writable - move it to e.g. Documents.")
    return {"name": name, "ok": True, "overwritten": existed,
            "renamed": name != unicodedata.normalize("NFC", (req.name or "").strip())}


@app.delete("/api/presets/{name}")
def delete_preset(name: str):
    safe = _safe_preset_name(name)
    fp = os.path.join(PRESETS_DIR, safe + ".json")
    if safe and os.path.exists(fp):
        os.remove(fp)
    return {"ok": True}


_WIN_RESERVED = {"con", "prn", "aux", "nul", *(f"com{i}" for i in range(1, 10)),
                 *(f"lpt{i}" for i in range(1, 10))}


def _safe_preset_name(name: str) -> str:
    """A preset name as a file name: keeps letters of any script (Hebrew,
    accents, ...), drops only what Windows forbids."""
    s = unicodedata.normalize("NFC", (name or "").strip())
    s = re.sub(r'[<>:"/\\|?*\x00-\x1f]+', "_", s)
    s = re.sub(r"[. ]+$", "", s[:120].strip()).strip()
    # 'CON', 'nul.backup', ...: Windows reserves the device name with any extension
    if s.split(".")[0].strip().lower() in _WIN_RESERVED:
        s = "_" + s
    return s


@app.get("/api/health")
def health():
    h = STATE.get("run")
    return {"ok": True, "version": APP_VERSION, "public_version": PUBLIC_VERSION,
            "rscript": rrunner.find_rscript(), "loaded": STATE["path"],
            "running": bool(h is not None and h.active())}


class VaDetectReq(BaseModel):
    columns: list[str] = []
    # optional: also report how each column reads in this notation
    va_format: str | None = None


@app.post("/api/va-detect")
def va_detect(req: VaDetectReq):
    """Guess the notation (decimal / logmar / snellen / etdrs) each column is
    recorded in; null when the values do not tell or the column is unknown.
    With `va_format`, `parse` also gives, per column, how many non-blank values
    cannot be read in that notation (the run log reports the same counts)."""
    df = _require_df()
    resolver = preview.build_resolver(list(df.columns))
    fmt = (req.va_format or "").strip().lower() or None
    if fmt is not None and fmt not in preview.VA_FORMATS:
        raise HTTPException(400, f"Unknown VA notation '{req.va_format}'.")
    guess: dict[str, Any] = {}
    parse: dict[str, Any] = {}
    warnings: list[str] = []
    for c in req.columns or []:
        actual = preview.resolve(c, resolver)
        if actual is None or actual not in df.columns:
            guess[c] = None
            continue
        guess[c] = preview.detect_va_format(df[actual])
        if fmt is not None:
            p = preview.parse_va(df[actual], fmt)
            parse[c] = {k: p[k] for k in ("n_nonblank", "n_lost", "examples",
                                         "n_offchart", "n_out_of_range")}
            # ETDRS declared, decimal recorded: the run stops on it (R4-7)
            warn = preview.etdrs_decimal_problem(df[actual], str(actual)) \
                if fmt == "etdrs" else None
            parse[c]["warning"] = warn
            if warn:
                warnings.append(warn)
    out: dict[str, Any] = {"guess": guess}
    if fmt is not None:
        out["parse"] = parse
        out["warnings"] = warnings
    return out


# ---------------------------------------------------------------- routes: R setup
@app.get("/api/r-setup")
def r_setup():
    """Current R status + every R install found on this machine."""
    cur = rrunner.find_rscript()
    info = ""
    if cur:
        _, info = rrunner.validate_rscript(cur)
    return {"rscript": cur, "info": info,
            "saved": rrunner.load_settings().get("rscript_path"),
            "candidates": rrunner.discover_r_installs(),
            "bundled_lib": os.path.isdir(os.path.join(APP_ROOT, "runtime", "Rlib"))}


@app.post("/api/r-setup")
def set_r_setup(body: dict = Body(...)):
    """Validate a user-chosen Rscript.exe and remember it."""
    path = (body or {}).get("path", "").strip().strip('"')
    ok, msg = rrunner.validate_rscript(path)
    if not ok:
        return {"ok": False, "message": msg}
    s = rrunner.load_settings()
    s["rscript_path"] = path
    try:
        rrunner.save_settings(s)
    except OSError as e:
        return {"ok": False, "message": f"{msg} It could not be remembered: the app folder is "
                                        f"not writable ({e.strerror or e}). Move the StandardEyes "
                                        "folder somewhere you can write to (e.g. Documents)."}
    return {"ok": True, "message": msg, "rscript": path}


@app.post("/api/pick-rscript")
def pick_rscript():
    """Native file picker for Rscript.exe (runs on the user's own machine)."""
    try:
        import tkinter as tk  # noqa: PLC0415
        from tkinter import filedialog  # noqa: PLC0415
        root = tk.Tk(); root.withdraw(); root.attributes("-topmost", True)
        p = filedialog.askopenfilename(
            title="Select Rscript.exe (in R's bin folder)",
            filetypes=[("Rscript", "Rscript.exe"), ("All files", "*.*")],
            initialdir=r"C:\Program Files\R")
        root.destroy()
        return {"path": p or ""}
    except Exception as e:  # noqa: BLE001  (tkinter is pruned in some bundles)
        return {"path": "", "error": str(e)}


# ---------------------------------------------------------------- static (last)
class NoCacheStaticFiles(StaticFiles):
    """Serve the SPA with revalidation so UI updates show up on plain reload
    (localhost: ETag round-trip is free; stale-CSS bugs are not)."""

    def file_response(self, *args, **kwargs):
        resp = super().file_response(*args, **kwargs)
        resp.headers["Cache-Control"] = "no-cache"
        return resp


app.mount("/", NoCacheStaticFiles(directory=STATIC_DIR, html=True), name="static")
