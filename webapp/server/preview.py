"""
preview.py
----------
Fast, in-memory pandas mirror of the R data-preparation logic, used ONLY to
give the GUI live feedback (row counts as filters are built, per-group / per-
subset n). It is NOT used for the reported statistics -- those always come from
the unchanged R engine via engine/run_pipeline.R.

The functions here reproduce the semantics of R/helpers.R:
  - resolve_raw_to_clean() -> column-name resolution (Resolver)
  - as_numeric_safe()
  - standardize_minus_cyl()
  - apply_filter_rules()   (NA rows are KEPT, exactly like the R version)
  - match_values()         (group definitions, subsets, in / notin)
  - parse_va()             (visual-acuity notation -> canonical decimal)
  - the exclude step and group-label assignment
Order matches R/pipeline.R: standardize -> exclude -> filters -> subset ->
VA analysis columns (LogMAR / notation). Exclusions and filters therefore see
the visual acuity AS RECORDED in the file.
"""

from __future__ import annotations

import math
import re
from datetime import date, datetime, time
from typing import Any

import numpy as np
import pandas as pd

# Values that the R side treats as missing when coercing to numeric.
_NA_TOKENS = {"", " ", "NA", "#VALUE!", "#DIV/0!", "nan", "NaN", "None"}

NUMERIC_OPS = {
    "==", "!=", "<", "<=", ">", ">=",
    "between", "between_any", "abs_ge", "abs_le", "abs_between",
}
TEXT_OPS = {"in", "notin", "contains", "not_contains"}
NULLARY_OPS = {"isna", "notna"}


# ---------------------------------------------------------------- name cleaning
def clean_key(name: str) -> str:
    """Case/punctuation-insensitive lookup key of one header (the fuzzy tier
    of the resolver; cf. .name_key() in R/helpers.R).

    It is only a fallback: both sides see the SAME header names (pandas'
    'Age', 'Age.1', 'Unnamed: 5' - R applies pandas_like_names() to the sheet),
    the exact name always wins, and a key shared by two headers resolves to
    nothing. As make.names(): names that would start with a digit get R's
    'X' prefix, reserved words ('NA', 'if', ...) a trailing '.'.
    """
    s = str(name).strip()
    s = re.sub(r"[^A-Za-z0-9]+", "_", s)
    s = re.sub(r"^_|_$", "", s)
    if s == "" or s[0].isdigit():
        s = "X" + s
    if s in _R_RESERVED:
        s += "."
    return s.lower()


def _has_stem(name: str) -> bool:
    """The header has at least one ASCII letter or digit (else R's lookup key
    is empty and the name can only be matched exactly)."""
    return re.search(r"[A-Za-z0-9]", str(name)) is not None


def similar_headers(columns: list) -> list[list[str]]:
    """Groups of different headers that differ only by case or punctuation
    ('Pre UCVA' / 'Pre-UCVA'). Each works when picked from the list, but a
    preset or typed name that matches neither exactly cannot be resolved."""
    groups: dict[str, list[str]] = {}
    for c in columns:
        s = str(c)
        if _has_stem(s):
            g = groups.setdefault(clean_key(s), [])
            if s not in g:
                g.append(s)
    return [g for g in groups.values() if len(g) > 1]


def _r_trim(s: str) -> str:
    """R's trimws(): only space, tab, CR and LF are trimmed."""
    return s.strip(" \t\r\n")


# words make.names() suffixes with "." (?Reserved)
_R_RESERVED = {"if", "else", "repeat", "while", "function", "for", "in", "next", "break",
               "TRUE", "FALSE", "NULL", "Inf", "NaN", "NA", "NA_integer_", "NA_real_",
               "NA_character_", "NA_complex_"}


class Resolver:
    """Raw label -> actual column, with the three tiers of .resolve_one() in
    R/helpers.R, so a label gets the same column (or none) in the GUI and in
    the engine:
      1. the exact header;
      2. the label without its surrounding spaces, again as an exact header;
      3. the case/punctuation-insensitive key, only when ONE header has it.
    Two headers sharing the key = ambiguous = None, so a near-duplicate
    header can never be picked silently (a cleaned name such as 'Pre_UCVA'
    goes through the key like any other label)."""

    def __init__(self, columns: list) -> None:
        cols = list(columns)
        self.exact: dict[str, Any] = {}
        self.by_key: dict[str, list] = {}
        for c in cols:
            s = str(c)
            self.exact.setdefault(s, c)
            if _has_stem(s):          # no ASCII letter/digit: no key, as in R
                self.by_key.setdefault(clean_key(s), []).append(c)

    def lookup(self, label: Any) -> tuple[Any, list]:
        """(column or None, the headers an ambiguous label matches)."""
        if label is None:
            return None, []
        s = str(label)
        if _r_trim(s) == "":
            return None, []
        if s in self.exact:
            return self.exact[s], []
        if _r_trim(s) in self.exact:
            return self.exact[_r_trim(s)], []
        if not _has_stem(s):
            return None, []
        hits = self.by_key.get(clean_key(s), [])
        if len(hits) == 1:
            return hits[0], []
        return None, list(hits)

    def candidates(self, label: Any) -> list:
        """Headers a label that does not resolve could have meant: more than
        one = ambiguous (differ only by case, punctuation or spacing)."""
        return self.lookup(label)[1]

    def resolve(self, label: Any) -> Any:
        return self.lookup(label)[0]


def build_resolver(columns: list) -> Resolver:
    return Resolver(columns)


def resolve(label: Any, resolver: Resolver) -> Any:
    if resolver is None:
        return None
    return resolver.resolve(label)


def unresolved_reason(label: Any, resolver: Resolver | None) -> str:
    """Why a label did not resolve: 'column not found', or the headers it
    cannot choose between."""
    cands = resolver.candidates(label) if isinstance(resolver, Resolver) else []
    if len(cands) > 1:
        return "column name is ambiguous - matches " + ", ".join(f"'{c}'" for c in cands)
    return "column not found"


def renamed_headers(columns: list) -> list[str]:
    """Headers pandas had to invent: blank ('Unnamed: 3') or repeated
    ('Cyl.1' next to 'Cyl'). R repairs the sheet's names the same way
    (pandas_like_names), so they work, but the user should know the names
    shown are not the ones typed in the file."""
    names = [str(c) for c in columns]
    present = set(names)
    out = []
    for n in names:
        if n.startswith("Unnamed: "):
            out.append(n)
            continue
        m = re.match(r"^(.*)\.(\d+)$", n)
        if m and m.group(1) in present:
            out.append(n)
    return out


# ------------------------------------------------------------------- coercion
def as_numeric_safe(series: pd.Series) -> pd.Series:
    s = series.astype("object").astype("string")
    s = s.where(~s.isin(_NA_TOKENS), other=pd.NA)
    # float64 (NaN, not pd.NA) so downstream numpy ops never hit ambiguous NA
    return pd.to_numeric(s, errors="coerce").astype("float64")


def _is_missing(series: pd.Series) -> pd.Series:
    """Mirror R is.na() on a column as read (blank cells -> NA)."""
    s = series
    isna = s.isna()
    # readxl leaves genuine blanks as NA; treat empty strings as missing too.
    as_str = s.astype("object").astype("string")
    return isna | as_str.isin({"", " "})


def _r_double_str(f: float) -> Any:
    """A double as R's as.character() prints it: at most 15 significant
    digits, trailing zeros dropped, and scientific notation whenever it is
    narrower than fixed notation (formatReal, scipen = 0): 1e+05, 1e-04,
    0.001, 123456, 0.666666666666667."""
    if math.isnan(f):
        return pd.NA
    if math.isinf(f):
        return "Inf" if f > 0 else "-Inf"
    if f == 0:
        return "0"
    mant, ex = f"{abs(f):.14e}".split("e")
    digits = mant.replace(".", "").rstrip("0") or "0"
    nsig, kp = len(digits), int(ex)
    neg = 1 if f < 0 else 0
    rgt = max(0, nsig - kp - 1)                     # digits right of the point
    w_fixed = neg + (kp + 1 if kp >= 0 else 1) + (rgt + 1 if rgt else 0)
    w_sci = neg + (nsig + 1 if nsig > 1 else 1) + (4 if abs(kp) < 100 else 5)
    if w_fixed <= w_sci:
        s = f"{abs(f):.{rgt}f}"
    else:
        s = digits[0] + ("." + digits[1:] if nsig > 1 else "") \
            + f"e{'+' if kp >= 0 else '-'}{abs(kp):02d}"
    return ("-" if neg else "") + s


def _r_str(x: Any) -> Any:
    """One cell as R's as.character() prints it (pd.NA for missing).

    readxl reads a numeric cell as double, and as.character(1) is "1", so an
    integral float must not print as "1.0" (pandas does, whenever the column
    holds one blank cell)."""
    if x is None or x is pd.NA or x is pd.NaT:
        return pd.NA
    if isinstance(x, (bool, np.bool_)):
        return "TRUE" if x else "FALSE"
    if isinstance(x, (int, np.integer)):
        return str(int(x))
    if isinstance(x, (float, np.floating)):
        return _r_double_str(float(x))
    if isinstance(x, (datetime, pd.Timestamp)):
        if (x.hour, x.minute, x.second) == (0, 0, 0):
            return x.strftime("%Y-%m-%d")
        return x.strftime("%Y-%m-%d %H:%M:%S")
    if isinstance(x, date):
        return x.strftime("%Y-%m-%d")
    return str(x).strip()


def as_str(series: pd.Series) -> pd.Series:
    """Mirror R as.character() on the loaded frame, trimmed: integral floats
    print without '.0'. Used everywhere value strings are built or compared
    (/api/distinct, group masks, in / notin, subsets, histogram labels).
    A numeric column is double in R (readxl), so whole numbers print as
    doubles too: 100000 is '1e+05' there."""
    if _is_r_numeric(series):
        return series.map(lambda v: _r_double_str(float(v)) if not pd.isna(v) else pd.NA) \
            .astype("string")
    return series.map(_r_str).astype("string")


def _is_r_numeric(series: pd.Series) -> bool:
    return pd.api.types.is_numeric_dtype(series) and not pd.api.types.is_bool_dtype(series)


def _flat_values(values: Any) -> list:
    if values is None:
        return []
    if isinstance(values, (list, tuple, set)):
        out: list = []
        for v in values:
            out.extend(_flat_values(v))
        return out
    return [values]


def _is_r_logical(series: pd.Series) -> bool:
    """readxl reads the column as logical: bool dtype, or bools with blanks."""
    if pd.api.types.is_bool_dtype(series):
        return True
    if not pd.api.types.is_object_dtype(series):
        return False
    present = [x for x in series.tolist() if not _is_blank_cell(x)]
    return bool(present) and all(isinstance(x, (bool, np.bool_)) for x in present)


def _is_blank_cell(x: Any) -> bool:
    return x is None or x is pd.NA or x is pd.NaT or (isinstance(x, float) and math.isnan(x))


def _as_r_number(s: str) -> float:
    """R's as.numeric() on one trimmed string (NaN where R gives NA)."""
    if "_" in s:                      # float() accepts '1_0'; R does not
        return float("nan")
    try:
        return float(s)
    except ValueError:
        return float("nan")


def match_values(series: pd.Series, values: Any) -> pd.Series:
    """Boolean mask of rows whose value is one of `values` (missing -> False).

    Mirrors match_values() in R/helpers.R: a numeric column is compared
    numerically, so '1', '1.0' and '1e+00' all select the code 1; any
    non-numeric value is compared as text. A logical column accepts
    TRUE / true / 1 / 1.0 (and FALSE / 0); a date column matches
    'YYYY-MM-DD' (the date part) or 'YYYY-MM-DD HH:MM:SS'. A text column is
    compared on the trimmed text, so '007' never turns into '7'."""
    vals = [s for s in (_r_str(v) for v in _flat_values(values)) if s is not pd.NA]
    vals = [str(s).strip() for s in vals]
    if _is_r_logical(series):
        nums = [_as_r_number(v) for v in vals]
        want = set()
        if any(v.upper() == "TRUE" or n == 1 for v, n in zip(vals, nums)):
            want.add(True)
        if any(v.upper() == "FALSE" or n == 0 for v, n in zip(vals, nums)):
            want.add(False)
        hit = series.map(lambda x: (not _is_blank_cell(x)) and bool(x) in want)
        return hit.astype(bool)
    if pd.api.types.is_datetime64_any_dtype(series):
        if not vals:
            return pd.Series(False, index=series.index)
        want = set(vals)
        day = series.dt.strftime("%Y-%m-%d")
        full = series.dt.strftime("%Y-%m-%d %H:%M:%S")
        return (day.isin(want) | full.isin(want)).where(series.notna(), False).astype(bool)
    if _is_r_numeric(series):
        num = pd.to_numeric(series, errors="coerce").astype("float64")
        nv = pd.to_numeric(pd.Series(vals, dtype="object"), errors="coerce")
        hit = num.isin(set(nv.dropna().astype("float64").tolist()))
        text_vals = {v for v, n in zip(vals, nv.tolist()) if pd.isna(n)}
        if text_vals:
            hit = hit | as_str(series).isin(text_vals).fillna(False).astype(bool)
        return hit.fillna(False).astype(bool)
    return as_str(series).isin(set(vals)).fillna(False).astype(bool)


# ------------------------------------------------------------- readxl typing
_BLANK, _LOGICAL, _DATE, _NUMERIC, _TEXT = range(5)   # readxl's type order
_EXCEL_EPOCH = datetime(1899, 12, 30)


def _cell_kind(x: Any) -> int:
    if x is None or x is pd.NA or x is pd.NaT:
        return _BLANK
    if isinstance(x, str):
        return _TEXT if x.strip() != "" else _BLANK
    if isinstance(x, (bool, np.bool_)):
        return _LOGICAL
    if isinstance(x, (datetime, date, time, np.datetime64)):
        return _DATE
    if isinstance(x, (int, float, np.integer, np.floating)):
        return _BLANK if isinstance(x, (float, np.floating)) and math.isnan(x) else _NUMERIC
    return _TEXT


def _as_datetime(x: Any) -> Any:
    """A date / time cell as readxl's POSIXct (a time-only cell sits on 1899-12-31)."""
    if isinstance(x, time):
        return pd.Timestamp(datetime.combine(date(1899, 12, 31), x))
    if isinstance(x, (datetime, date, np.datetime64)):
        return pd.Timestamp(x)
    return pd.NaT


def _excel_serial(x: Any) -> float:
    """A date / time cell as the Excel serial number readxl reports in a text column."""
    if isinstance(x, time):
        return (x.hour * 3600 + x.minute * 60 + x.second + x.microsecond / 1e6) / 86400
    ts = pd.Timestamp(x)
    return (ts - pd.Timestamp(_EXCEL_EPOCH)) / pd.Timedelta(days=1)


def _readxl_text(x: Any) -> Any:
    """One cell of a TEXT column as readxl gives it: text trimmed (blank ->
    missing), TRUE / FALSE, numbers with 16 significant digits ('%.16g', so
    1000000 stays '1000000'), dates as their Excel serial number."""
    k = _cell_kind(x)
    if k == _BLANK:
        return np.nan
    if k == _TEXT:
        return str(x).strip()
    if k == _LOGICAL:
        return "TRUE" if x else "FALSE"
    if k == _DATE:
        return "%.16g" % _excel_serial(x)
    return "%.16g" % float(x)


def _readxl_number(x: Any) -> float:
    """One cell of a NUMERIC column: TRUE / FALSE -> 1 / 0; a date-formatted
    cell is missing (read_input_sheet sets it to NA and logs it); text past the
    guess window is read as a number when it is one, else missing."""
    k = _cell_kind(x)
    if k == _NUMERIC:
        return float(x)
    if k == _LOGICAL:
        return 1.0 if x else 0.0
    if k == _TEXT:
        try:
            return float(str(x).strip())
        except ValueError:
            return np.nan
    return np.nan


def readxl_types(raw: pd.DataFrame, guess_max: int) -> pd.DataFrame:
    """Type every column of a sheet the way readxl::read_excel() does.

    `raw` must be read with dtype=object and keep_default_na=False, so each
    value is the cell as stored (pandas' default parse turns text cells such
    as '007' or '1.50' into numbers and 'NA' / 'None' into missing values;
    readxl does neither). The column type is the highest cell type in the
    first `guess_max` rows (blank < logical < date < numeric < text):
      text    -> character; numbers, TRUE / FALSE and dates written as text;
      numeric -> double; TRUE / FALSE -> 1 / 0; date cells -> missing;
      date    -> date-time;
      logical -> TRUE / FALSE.
    Text is trimmed and blank text is missing (trim_ws = TRUE). Excel error
    cells reach this function as missing values."""
    out = {}
    for j, c in enumerate(raw.columns):
        vals = raw.iloc[:, j].tolist()
        kind = max((_cell_kind(v) for v in vals[:guess_max]), default=_BLANK)
        if kind == _TEXT:
            s = pd.Series([_readxl_text(v) for v in vals], index=raw.index, dtype="object")
        elif kind == _NUMERIC:
            s = pd.to_numeric(pd.Series([_readxl_number(v) for v in vals], index=raw.index,
                                        dtype="float64"))
            if s.notna().all() and len(s) and (s % 1 == 0).all() and s.abs().max() < 2 ** 53:
                s = s.astype("int64")
        elif kind == _DATE:
            s = pd.to_datetime(pd.Series([_as_datetime(v) if _cell_kind(v) == _DATE else pd.NaT
                                          for v in vals], index=raw.index, dtype="object"))
        elif kind == _LOGICAL:
            s = pd.Series([bool(v) if _cell_kind(v) == _LOGICAL else np.nan for v in vals],
                          index=raw.index, dtype="object")
            if s.notna().all():
                s = s.astype(bool)
        else:
            s = pd.Series(np.nan, index=raw.index, dtype="float64")
        out[j] = s
    df = pd.DataFrame(out, index=raw.index)
    df.columns = raw.columns
    return df


# --------------------------------------------------------------- value parsing
def parse_filter_value(op: str, value: Any) -> Any:
    """Turn a UI value (string or list) into the typed value R expects.

    Mirrors the R pipeline's cfg parsing.
    """
    op = str(op).lower()
    if op in NULLARY_OPS:
        return None
    if (op == "between_any" and isinstance(value, (list, tuple)) and value
            and all(isinstance(v, (list, tuple)) for v in value)):
        # headless configs only: a list of [low, high] pairs
        return [parse_filter_value("between", list(rng)) for rng in value]
    if value is None:
        parts = []                    # no value given (never the text 'None')
    elif isinstance(value, (list, tuple)):
        parts = [str(v).strip() for v in value if str(v).strip() != ""]
    else:
        parts = [p.strip() for p in str(value).split(",") if p.strip() != ""]
    if op in TEXT_OPS:
        return parts
    # numeric
    nums: list[float] = []
    for p in parts:
        try:
            nums.append(float(p))
        except ValueError:
            nums.append(float("nan"))
    return nums


# ---------------------------------------------------------------- refraction
def _wrap_axis_0_180(series: pd.Series) -> pd.Series:
    """Axes in the clinical 1-180 range, as wrap_axis_0_180() in R/helpers.R:
    0 and 180 are the same meridian and 180 is how it is recorded."""
    ax = as_numeric_safe(series).mod(180)
    return ax.mask(ax == 0, 180.0).astype("float64")


# ------------------------------------------------------------- visual acuity
VA_FORMATS = ("decimal", "logmar", "snellen", "etdrs")
VA_ROLES = ("pre_ucva", "pre_bcva", "post_ucva", "post_bcva")

_SNELLEN_RE = re.compile(
    r"^\s*(\d+(?:[.,]\d+)?)\s*/\s*(\d+(?:[.,]\d+)?)\s*(?:([+-])\s*(\d+))?\s*$")
# off-chart notations; they are counted and set to missing, never given an
# acuity. Same pattern as parse_va() in R/helpers.R.
_OFFCHART_RE = re.compile(
    r"^(n\s*l\s*p|n\s*p\s*l|no\s*(l\s*p|p\s*l)|c\s*f|f\s*c|h\s*m|l\s*p|p\s*l)(?![a-z])"
    r"|^(no\s+)?(light\s+perception|perception\s+of\s+light)"
    r"|^(counting|count)\s+fingers?|^fingers?\s+count"
    r"|^hand\s+(motion|movement)s?", re.IGNORECASE)
# text R's parse_va() treats as a blank cell
_VA_BLANK = {"NA", "N/A", "#N/A"}
_COMMA_DECIMAL_RE = re.compile(r"^[-+]?[0-9]+,[0-9]+$")


def _va_text(raw: str) -> str:
    """Normalise VA text like R: Unicode minus, collapsed whitespace."""
    return re.sub(r"\s+", " ", raw.replace("−", "-")).strip()


def _va_number(x: Any) -> float:
    """A numeric cell, or numeric text with '.' or ',' as decimal mark."""
    if isinstance(x, (bool, np.bool_)):
        return float("nan")
    if isinstance(x, (int, float, np.integer, np.floating)):
        return float(x)
    s = _va_text(str(x))
    if "_" in s:                      # float() accepts '1_0'; R does not
        return float("nan")
    if _COMMA_DECIMAL_RE.match(s):
        s = s.replace(",", ".")
    try:
        v = float(s)
    except ValueError:
        return float("nan")
    # R's as.numeric() reads neither 'inf' nor 'nan' text as a finite number
    return v if math.isfinite(v) else float("nan")


def parse_va(series: pd.Series, fmt: str = "decimal") -> dict:
    """Canonical DECIMAL acuity for one column recorded in `fmt`.

    Mirrors parse_va() in R/helpers.R. Returns
    {decimal: float Series (NaN where unparseable), n_nonblank, n_lost,
     examples: up to 3 unparseable raw values, n_offchart, n_out_of_range
     (lost cells that were numbers outside the notation's range)}."""
    fmt = fmt if fmt in VA_FORMATS else "decimal"
    dec = np.full(len(series), np.nan)
    n_nonblank = n_lost = n_off = n_oor = 0
    examples: list[str] = []
    for i, x in enumerate(series.tolist()):
        if x is None or x is pd.NA or x is pd.NaT or (isinstance(x, float) and math.isnan(x)):
            continue
        raw = str(_r_str(x)) if not isinstance(x, str) else x.strip()
        if raw == "" or raw.upper() in _VA_BLANK:
            continue
        n_nonblank += 1
        text = _va_text(raw)
        if _OFFCHART_RE.match(text):
            n_off += 1
            continue
        val = float("nan")
        oor = False
        if fmt == "snellen":
            m = _SNELLEN_RE.match(text)
            if m:
                a = float(m.group(1).replace(",", "."))
                b = float(m.group(2).replace(",", "."))
                if a > 0 and b > 0:
                    lm = math.log10(b / a)
                    if m.group(3) and m.group(4):
                        k = int(m.group(4))
                        lm += 0.02 * k if m.group(3) == "-" else -0.02 * k
                    val = 10 ** (-lm)
                else:
                    oor = True
        else:
            v = _va_number(x)
            if math.isfinite(v):
                if fmt == "decimal" and v > 0:
                    val = v
                elif fmt == "logmar" and -0.5 <= v <= 3:
                    val = 10 ** (-v)
                elif fmt == "etdrs" and 0 <= v <= 100:
                    val = 10 ** (-(1.7 - 0.02 * v))
                else:
                    oor = True
        if math.isfinite(val):
            dec[i] = val
        else:
            n_lost += 1
            n_oor += int(oor)
            if raw not in examples and len(examples) < 3:
                examples.append(raw)
    return {"decimal": pd.Series(dec, index=series.index, dtype="float64"),
            "n_nonblank": n_nonblank, "n_lost": n_lost,
            "examples": examples, "n_offchart": n_off, "n_out_of_range": n_oor}


def va_format_problems(df: pd.DataFrame, cfg: dict) -> list[str]:
    """Columns whose values contradict the declared notation - the R engine
    stops on these (pipeline.R step A2a), so the run is refused up front:
    more than half of the non-blank cells unreadable, or numbers outside the
    notation's range (decimal <= 0, logMAR outside -0.5..3), or ETDRS letters
    declared for values that look like decimal acuity (etdrs_decimal_problem)."""
    fmt = cfg.get("va_format") or "decimal"
    if fmt not in VA_FORMATS:
        return []
    resolver = build_resolver(list(df.columns))
    cols, _ = va_columns(cfg, resolver)
    out = []
    for c in cols:
        if c not in df.columns:
            continue
        p = parse_va(df[c], fmt)
        if p["n_nonblank"] == 0:
            continue
        eg = ", ".join(f"'{e}'" for e in p["examples"])
        eg = f" (e.g. {eg})" if eg else ""
        if p["n_lost"] > 0.5 * p["n_nonblank"]:
            out.append(f"Visual-acuity column '{c}': {p['n_lost']} of {p['n_nonblank']} values "
                       f"cannot be read as {fmt}{eg}. Check 'VA notation in your file'.")
        elif fmt in ("decimal", "logmar") and p["n_out_of_range"] > 0:
            rng = "decimal acuity must be above 0" if fmt == "decimal" else \
                "logMAR must lie between -0.5 and 3"
            out.append(f"Visual-acuity column '{c}': {p['n_out_of_range']} value(s) out of range "
                       f"for {fmt}{eg} - {rng}. Check 'VA notation in your file'.")
        elif fmt == "etdrs":
            msg = etdrs_decimal_problem(df[c], c)
            if msg:
                out.append(msg)
    return out


def _r_format_numbers(vals: list[float]) -> list[str]:
    """Numbers as R's format() prints a vector of them: one common number of
    decimals (format(c(0.8, 1)) gives "0.8" "1.0")."""
    dec = 0
    for v in vals:
        s = str(_r_double_str(float(v)))
        if "e" not in s and "." in s:
            dec = max(dec, len(s.split(".", 1)[1]))
    return [f"{v:.{dec}f}" for v in vals]


def etdrs_decimal_problem(series: pd.Series, name: str) -> str | None:
    """Acuity declared as ETDRS letters but recorded as decimal (R4-7, the
    same rule as check_va_notation in R/pipeline.R): among the non-blank
    numeric cells, more than half are not whole numbers, or the largest value
    is below 5. Every decimal value 0.05-1.2 is a valid letter score, so the
    range check alone would read each eye as about 20/1000. None = plausible."""
    nums: list[float] = []
    for x in series.tolist():
        if _is_blank_cell(x):
            continue
        if isinstance(x, str) and (x.strip() == "" or x.strip().upper() in _VA_BLANK):
            continue
        v = _va_number(x)
        if math.isfinite(v):
            nums.append(v)
    if not nums:
        return None
    n_frac = sum(1 for v in nums if v != math.floor(v))
    if not (n_frac > 0.5 * len(nums) or max(nums) < 5):
        return None
    seen: list[float] = []
    for v in nums:
        if v not in seen:
            seen.append(v)
        if len(seen) == 2:
            break
    eg = ", ".join(_r_format_numbers(seen))
    return (f"VA column '{name}' is declared as ETDRS letters, but its values (e.g. {eg}) "
            "look like decimal acuity. Choose the notation the file uses (Variables tab).")


def va_format_notes(df: pd.DataFrame, cfg: dict) -> list[str]:
    """Non-blocking remarks on a declared notation the values make doubtful.
    Decimal acuity 0.05-2.0 also reads as logMAR, so a decimal file declared
    'logmar' cannot be refused - but after refractive or cataract surgery a
    logMAR median of 0.7 (about 20/100) with no negative value is far more
    likely decimal acuity around 1.0."""
    fmt = str(cfg.get("va_format") or "decimal").strip().lower()
    if fmt != "logmar":
        return []
    resolver = build_resolver(list(df.columns))
    roles = cfg.get("roles") or {}
    out = []
    for r in ("post_ucva", "post_bcva"):
        c = resolve(roles.get(r), resolver)
        if c is None or c not in df.columns:
            continue
        v = parse_va(df[c], "logmar")["decimal"]
        lm = (0.0 - np.log10(v.dropna())).round(6)
        if len(lm) >= 5 and float(lm.min()) >= 0 and float(lm.median()) >= 0.7:
            out.append(f"Visual-acuity column '{c}' ({r}) is declared logMAR, but its median is "
                       f"{float(lm.median()):.2f} with no negative value - typical of DECIMAL "
                       "acuity after surgery. Check 'VA notation in your file'.")
    return out


# values that are common in one notation and (almost) impossible in the other
_DECIMAL_ONLY = {0.05, 0.125, 0.25, 0.33, 0.67, 0.63, 1.25, 1.33, 1.5, 2.0}
_LOGMAR_ONLY = {0.02, 0.04, 0.06, 0.08, 0.18, 0.48, 0.54, 0.88}


def detect_va_format(series: pd.Series) -> str | None:
    """Best guess of the notation a VA column is recorded in, or None when
    the values do not tell (e.g. every value between 0.1 and 1.0)."""
    raw = [x.strip() if isinstance(x, str) else x for x in series.tolist()
           if not (x is None or x is pd.NA or (isinstance(x, float) and math.isnan(x)))]
    raw = [_va_text(x) if isinstance(x, str) else x for x in raw
           if not (isinstance(x, str) and (x == "" or x.upper() in _VA_BLANK))]
    vals = [x for x in raw if not (isinstance(x, str) and _OFFCHART_RE.match(x))]
    if not vals:
        return None
    n_frac = sum(1 for x in vals if isinstance(x, str) and _SNELLEN_RE.match(x))
    if n_frac >= 0.5 * len(vals):
        return "snellen"
    nums = [v for v in (_va_number(x) for x in vals) if math.isfinite(v)]
    if len(nums) < 0.5 * len(vals):
        return None
    lo, hi = min(nums), max(nums)
    if hi > 3:
        return "etdrs" if lo >= 0 and hi <= 100 else None
    if lo <= 0:
        return "logmar" if lo >= -0.5 else None
    if hi > 2.0:
        return "logmar"
    rounded = [round(v, 3) for v in nums]
    n_dec = sum(1 for v in rounded if v in _DECIMAL_ONLY)
    n_log = sum(1 for v in rounded if v in _LOGMAR_ONLY)
    if n_dec >= max(1, 0.05 * len(nums)) and n_log == 0:
        return "decimal"
    if n_log >= max(1, 0.05 * len(nums)) and n_dec == 0:
        return "logmar"
    return None


def va_columns(cfg: dict, resolver: Resolver) -> tuple[list, list]:
    """(every VA column: the four VA roles + logmar_cols_raw, the subset of
    them converted to LogMAR) as actual column names."""
    roles = cfg.get("roles") or {}
    cols: list = []
    for r in VA_ROLES:
        c = resolve(roles.get(r), resolver)
        if c is not None and c not in cols:
            cols.append(c)
    listed = [c for c in (resolve(x, resolver) for x in (cfg.get("logmar_cols_raw") or [])) if c is not None]
    for c in listed:
        if c not in cols:
            cols.append(c)
    converted = listed if cfg.get("do_logmar") else []
    return cols, converted


def apply_va_columns(df: pd.DataFrame, cfg: dict, resolver: Resolver) -> pd.DataFrame:
    """Analysis values of the VA columns (pipeline.R step A2b, AFTER filters):
    EVERY VA column becomes a number, as set_va_analysis_columns() makes it:
    va_format 'logmar' -> the recorded logMAR number (never a second
    conversion), missing where parse_va() could not read the cell; converted
    columns -> -log10(canonical decimal); every other column -> the canonical
    decimal (Snellen / ETDRS replaced by decimal, '0,8' read as 0.8, text such
    as 'CF' missing). A VA covariate therefore enters matching as a number in
    the preview exactly as in the run."""
    fmt = str(cfg.get("va_format") or "decimal").strip().lower()
    fmt = fmt if fmt in VA_FORMATS else "decimal"
    cols, converted = va_columns(cfg, resolver)
    cols = [c for c in cols if c in df.columns]
    if not cols:
        return df
    df = df.copy()
    for c in cols:
        dec = parse_va(df[c], fmt)["decimal"]
        if fmt == "logmar":
            rec = pd.Series([_va_number(x) if not (x is None or x is pd.NA or x is pd.NaT)
                             else float("nan") for x in df[c].tolist()],
                            index=df.index, dtype="float64")
            df[c] = rec.where(dec.notna())
        elif c in converted:
            df[c] = 0.0 - np.log10(dec)   # 0.0 - x: no '-0'
        else:
            df[c] = dec
    return df


def apply_mutations(df: pd.DataFrame, cfg: dict, resolver: Resolver) -> pd.DataFrame:
    """The value mutation that runs before exclude/filters, as in pipeline.R:
    minus-cyl standardization of every configured sph/cyl/axis triple. The
    VA conversion is NOT done here - filters see the acuity as recorded."""
    if cfg.get("do_standardize_minus_cyl"):
        triples = cfg.get("std_triples") or [{
            "sph": cfg.get("std_sph_raw"), "cyl": cfg.get("std_cyl_raw"),
            "axis": cfg.get("std_axis_raw"),
        }]
        for tr in triples:
            df = standardize_minus_cyl(
                df,
                resolve((tr or {}).get("sph"), resolver),
                resolve((tr or {}).get("cyl"), resolver),
                resolve((tr or {}).get("axis"), resolver),
            )
    return df


def standardize_minus_cyl(df: pd.DataFrame, sph_c, cyl_c, axis_c) -> pd.DataFrame:
    if not all(c in df.columns for c in (sph_c, cyl_c, axis_c) if c is not None):
        return df
    if sph_c is None or cyl_c is None or axis_c is None:
        return df
    df = df.copy()
    sph = as_numeric_safe(df[sph_c])
    cyl = as_numeric_safe(df[cyl_c])
    ax = as_numeric_safe(df[axis_c])
    # as in R/helpers.R: only plus-cylinder rows are transposed (axis + 90 into
    # 1-180); a row already in minus form keeps its axis exactly as recorded
    idx = pd.Series(np.isfinite(cyl.to_numpy()), index=cyl.index) & (cyl > 0)   # is.finite()
    sph = sph.where(~idx, sph + cyl)
    ax_new = ax.where(~idx, _wrap_axis_0_180(ax + 90))
    cyl = cyl.where(~idx, -cyl)
    df[sph_c], df[cyl_c], df[axis_c] = sph, cyl, ax_new
    return df


# ------------------------------------------------------------------- filters
class FilterProblem(ValueError):
    """A rule the R engine would refuse; the message is shown to the user.
    `kind` is 'incomplete' (a value is missing or half-filled) or 'invalid'
    (the rule cannot apply to this column)."""

    def __init__(self, message: str, kind: str = "invalid") -> None:
        super().__init__(message)
        self.kind = kind


def _numbers(value: Any, op: str, need: int) -> list[float]:
    """Exactly `need` finite numbers, as nums() in R's apply_filter_rules:
    the engine stops on anything else (it never recycles '1, 2' over rows)."""
    vals = list(value or []) if isinstance(value, (list, tuple)) else [value]
    out = []
    for x in vals:
        try:
            f = float(x)
        except (TypeError, ValueError):
            f = float("nan")
        out.append(f)
    if len(out) != need or not all(math.isfinite(f) for f in out):
        raise FilterProblem(f"'{op}' needs " + ("exactly one number" if need == 1
                                                else "both a lower and an upper limit"),
                            kind="incomplete")
    return out


def _apply_one_rule(df: pd.DataFrame, col: str, op: str, value: Any) -> pd.Series:
    op = str(op).lower()
    v = df[col]
    keep = pd.Series(True, index=df.index)

    if op in NUMERIC_OPS:
        vnum = as_numeric_safe(v)
        na = vnum.isna()
        present = ~_is_missing(v)
        # R stops here (helpers.R); a silent no-op would analyse the wrong cohort
        if bool(present.any()) and bool(vnum[present.values].isna().all()):
            raise FilterProblem(f"'{op}' is numeric but the column has no numeric values "
                                "- use in / notin / contains instead")
        if op == "between":
            lo, hi = _numbers(value, op, need=2)[:2]
            keep = na | ((vnum >= lo) & (vnum <= hi))
        elif op == "between_any":
            rngs = value if isinstance(value, (list, tuple)) else []
            if rngs and not isinstance(rngs[0], (list, tuple)):
                rngs = [rngs] if len(rngs) == 2 else []
            if not rngs:
                raise FilterProblem("'between_any' needs one or more [low, high] pairs",
                                    kind="incomplete")
            in_any = pd.Series(False, index=df.index)
            for rng in rngs:
                lo, hi = _numbers(rng, op, need=2)
                in_any = in_any | ((vnum >= lo) & (vnum <= hi))
            keep = na | in_any
        elif op == "abs_ge":
            keep = na | (vnum.abs() >= _numbers(value, op, need=1)[0])
        elif op == "abs_le":
            keep = na | (vnum.abs() <= _numbers(value, op, need=1)[0])
        elif op == "abs_between":
            lo, hi = _numbers(value, op, need=2)[:2]
            keep = na | ((vnum.abs() >= lo) & (vnum.abs() <= hi))
        else:
            vv = _numbers(value, op, need=1)[0]
            cmp = {
                "==": vnum == vv, "!=": vnum != vv,
                "<": vnum < vv, "<=": vnum <= vv,
                ">": vnum > vv, ">=": vnum >= vv,
            }[op]
            keep = na | cmp

    elif op in ("in", "notin"):
        # same value matching as R's match_values(): trimmed text, numeric
        # columns compared as numbers
        if op == "in" and not [x for x in _flat_values(value) if _r_str(x) is not pd.NA]:
            raise FilterProblem("no values selected", kind="incomplete")   # R stops here
        miss = _is_missing(v)
        isin = match_values(v, value)
        keep = miss | (isin if op == "in" else ~isin)

    elif op in ("contains", "not_contains"):
        # LITERAL substring, any of the given values, case-insensitive -
        # mirrors helpers.R exactly (which uses fixed = TRUE, no regex)
        parts = [value] if isinstance(value, str) else [str(x) for x in (value or [])]
        if not [p for p in parts if p.strip() != ""]:
            # refused before the run (validate_cfg): R would keep only the rows
            # where the column is blank ('contains') or change nothing
            raise FilterProblem("no text given", kind="incomplete")
        low = as_str(v).fillna("").str.lower()
        hit = pd.Series(False, index=v.index)
        for p in parts:
            if str(p):
                hit = hit | low.str.contains(str(p).lower(), regex=False, na=False)
        miss = _is_missing(v)
        keep = miss | (hit if op == "contains" else ~hit)

    elif op == "isna":
        keep = _is_missing(v)
    elif op == "notna":
        keep = ~_is_missing(v)

    return keep.fillna(True)


def apply_filter_rules(df: pd.DataFrame, rules: list[dict], resolver: dict[str, str],
                       group_col: str | None = None, groups: dict | None = None):
    """Return (filtered_df, steps) where steps lists remaining rows per rule.

    When group_col/groups are given, each step also carries per-group remaining
    counts, so the UI can show how many of each group a filter removed."""
    out = df

    def _gcounts(frame):
        if not group_col or group_col not in frame.columns or not groups:
            return None
        return {g: int(group_mask(frame[group_col], v).sum()) for g, v in groups.items()}

    steps = []
    for i, r in enumerate(rules or [], start=1):
        var = r.get("var")
        op = r.get("op")
        if not var or not op:
            continue
        # every skipped step carries `kind`, for the badge: missing_column |
        # ambiguous_column | incomplete (a value missing or half-filled) | invalid
        col = resolve(var, resolver)
        if col is None or col not in out.columns:
            amb = isinstance(resolver, Resolver) and len(resolver.candidates(var)) > 1
            steps.append({"rule": f"{var} {op}", "remaining": len(out), "skipped": True,
                          "kind": "ambiguous_column" if amb else "missing_column",
                          "reason": unresolved_reason(var, resolver), "groups": _gcounts(out)})
            continue
        try:
            value = parse_filter_value(op, r.get("value"))
            keep = _apply_one_rule(out, col, op, value)
        except FilterProblem as e:
            # the run would stop on this rule: say why instead of skipping silently
            steps.append({"rule": f"{var} {op}", "remaining": int(len(out)), "skipped": True,
                          "error": True, "invalid": True, "kind": e.kind, "reason": str(e),
                          "groups": _gcounts(out)})
            continue
        except Exception:
            steps.append({"rule": f"{var} {op}", "remaining": int(len(out)), "skipped": True,
                          "error": True, "invalid": True, "kind": "incomplete",
                          "reason": "value missing or not valid for this operator",
                          "groups": _gcounts(out)})
            continue
        out = out[keep.values]
        steps.append({"rule": f"{var} {op}", "remaining": int(len(out)), "skipped": False,
                      "groups": _gcounts(out)})
        if len(out) == 0:
            break
    return out, steps


# ------------------------------------------------------------------- grouping
def group_mask(series: pd.Series, values) -> pd.Series:
    """Boolean mask: rows whose value is in `values` — the same matching
    (match_values) the R engine uses to assign GroupLabel."""
    return match_values(series, values)


def assign_group_counts(df: pd.DataFrame, group_col: str | None, groups: dict) -> dict:
    counts: dict[str, int] = {}
    if group_col is None or group_col not in df.columns or not groups:
        return {"unassigned": int(len(df))}
    assigned = pd.Series(False, index=df.index)
    for gname, vals in groups.items():
        mask = group_mask(df[group_col], vals)
        counts[gname] = int(mask.sum())
        assigned = assigned | mask
    counts["unassigned"] = int((~assigned).sum())
    return counts


# ------------------------------------------------------------- filtered frame
def subset_column_missing(cfg: dict, resolver: Resolver) -> bool:
    """A subset column is named but not found: the R run stops on this, so
    the preview must not quietly fall back to all rows."""
    raw = cfg.get("subset_col_raw")
    return bool(raw and str(raw).strip()) and resolve(raw, resolver) is None


def subset_mode(cfg: dict) -> str:
    """'separate' | 'both' | 'pooled', read as build_runs() in R/pipeline.R
    reads it: an explicit subset_mode wins (trimmed, lower-case; any other
    word means separate runs, as in R and the GUI); a settings file older
    than the key carries only subset_pooled (true = pooled + separate runs).
    With neither, 'separate' - the default config_schema gives every run."""
    m = (cfg or {}).get("subset_mode")
    if isinstance(m, (list, tuple)):
        m = m[0] if m else None
    if m is not None and str(m).strip() != "":
        m = str(m).strip().lower()
        return m if m in ("separate", "pooled", "both") else "separate"
    legacy = (cfg or {}).get("subset_pooled")
    if isinstance(legacy, (list, tuple)):
        legacy = legacy[0] if legacy else None
    if legacy is None:
        return "separate"
    if isinstance(legacy, str):
        on = legacy.strip() in ("TRUE", "true", "True", "T")
    else:
        try:
            on = bool(legacy) and not (isinstance(legacy, float) and math.isnan(legacy))
        except (TypeError, ValueError):
            on = False
    return "both" if on else "separate"


def run_group_counts(df: pd.DataFrame, cfg: dict) -> list[tuple[str, dict]]:
    """Rows per group in every run the engine will make (same run plan as
    build_runs in R/pipeline.R): 'Pooled' and/or one run per subset when a
    subset column is set, else 'All'. Empty when no group column resolves."""
    resolver = build_resolver(list(df.columns))
    gcol = resolve(cfg.get("group_source_var"), resolver)
    groups = {k: v for k, v in (cfg.get("groups") or {}).items() if v}
    if gcol is None or not groups:
        return []
    work = filtered_frame(df, cfg, va=False)
    subset_col = resolve(cfg.get("subset_col_raw"), resolver)
    subsets = cfg.get("subsets") or {}
    mode = subset_mode(cfg)
    out: list[tuple[str, dict]] = []
    if subset_col is not None and subset_col in work.columns and subsets:
        if mode in ("pooled", "both"):
            out.append(("Pooled", assign_group_counts(work, gcol, groups)))
        if mode in ("separate", "both"):
            for sname, svals in subsets.items():
                sub = work[match_values(work[subset_col], svals).values]
                out.append((str(sname), assign_group_counts(sub, gcol, groups)))
    else:
        out.append(("All", assign_group_counts(work, gcol, groups)))
    return out


def subset_union_values(cfg: dict) -> list:
    return [x for vals in (cfg.get("subsets") or {}).values() for x in _flat_values(vals)]


def filtered_frame(df: pd.DataFrame, cfg: dict, va: bool = True,
                   subsets: bool = True) -> pd.DataFrame:
    """The rows that would enter the analysis under the current config:
    standardize -> exclude -> filters -> subset union -> VA analysis values.
    Mirrors compute_counts. va=False returns the acuity as recorded;
    subsets=False stops before the subset restriction (the rows R checks
    group values on, pipeline.R step D)."""
    resolver = build_resolver(list(df.columns))
    work = apply_mutations(df, cfg, resolver)
    excl_cols = [c for c in (resolve(e, resolver) for e in (cfg.get("exclude_params_raw") or [])) if c is not None]
    if excl_cols:
        mask = pd.Series(True, index=work.index)
        for c in excl_cols:
            mask = mask & ~_is_missing(work[c])
        work = work[mask.values]
    work, _ = apply_filter_rules(work, cfg.get("filter_rules") or [], resolver)
    subset_col = resolve(cfg.get("subset_col_raw"), resolver)
    sub_def = cfg.get("subsets") or {}
    if subsets and subset_col is not None and subset_col in work.columns and sub_def:
        work = work[match_values(work[subset_col], subset_union_values(cfg)).values]
    if va:
        work = apply_va_columns(work, cfg, resolver)
    return work


# --------------------------------------------------------- matching covariates
# Excel error spellings. A real error cell is missing in pandas and readxl
# alike; these strings only reach a frame not typed by readxl_types()
EXCEL_ERRORS = {"#VALUE!", "#DIV/0!", "#N/A", "#REF!", "#NAME?", "#NUM!", "#NULL!"}


def r_text_columns(df: pd.DataFrame, cols: list) -> set:
    """Columns readxl reads as TEXT: any non-missing cell holds text (a single
    text cell makes the whole column character in R). Blank text does not
    count: readxl reads it as missing.

    `df` must be the frame typed by readxl_types() (or read with dtype=object):
    pandas' default parse turns text cells that hold digits ('1', '2', '3')
    into numbers, which readxl does not. Excel error cells are already missing
    there (pandas reads them as NaN)."""
    out = set()
    for c in cols:
        if c in df.columns and not _is_r_numeric(df[c]) \
                and not pd.api.types.is_datetime64_any_dtype(df[c]) \
                and not pd.api.types.is_bool_dtype(df[c]):
            if df[c].map(lambda x: isinstance(x, str) and x.strip() != "").any():
                out.add(c)
    return out


CAT_PREFIX = "lvl:"


def psm_covariates(frame: pd.DataFrame, covs: list, text_covs: set,
                   min_share: float = 0.9, max_levels: int = 10) -> tuple[pd.DataFrame, list, list]:
    """The matching covariates as the run's propensity model sees them, for
    one pairing - mirrors coerce_numeric_covs() in R/helpers.R:
      text with >= 90 % numbers and (some non-numbers or > 10 levels) -> numeric
        (the non-numbers become missing);
      text where every cell is a number (<= 10 levels)              -> categorical;
      other text with > 10 levels                                   -> excluded;
      other text                                                    -> categorical.
    Categorical values are written with a text prefix so read.csv() in
    engine/match_preview.R cannot re-read '1', '2', '3' as one linear number;
    the prefix does not change the model (only which rows share a level).
    Dates are written as seconds (a linear rescaling, same model).
    Returns (encoded frame, excluded covariates, categorical covariates)."""
    enc: dict[str, pd.Series] = {}
    dropped: list = []
    categorical: list = []
    for c in covs:
        s = frame[c]
        if c not in text_covs:
            if pd.api.types.is_datetime64_any_dtype(s):
                try:
                    enc[c] = (s.dt.tz_localize(None) if s.dt.tz is not None else s) \
                        .sub(pd.Timestamp("1970-01-01")).dt.total_seconds()
                except (TypeError, ValueError, AttributeError):
                    enc[c] = s
            elif pd.api.types.is_object_dtype(s) and not s.map(
                    lambda x: isinstance(x, (bool, np.bool_)) or x is None
                    or (isinstance(x, float) and math.isnan(x))).all():
                # numbers mixed with Excel error cells: numeric in R, errors missing
                enc[c] = as_numeric_safe(s.map(
                    lambda x: np.nan if isinstance(x, str) and x.strip() in EXCEL_ERRORS else x))
            else:
                enc[c] = s
            continue
        # a real Excel error cell is already missing here (pandas and readxl);
        # '#VALUE!' typed as text is a non-numeric cell, as R counts it
        v = as_str(s).str.strip()
        nb = v.notna() & (v != "")
        x = as_numeric_safe(v)
        num_ok = nb & x.notna() & np.isfinite(x.fillna(0))
        n_nb = int(nb.sum())
        n_lev = int(v[nb].nunique())
        some_text = bool((nb & ~num_ok).any())
        if n_nb and int(num_ok.sum()) >= min_share * n_nb and (some_text or n_lev > max_levels):
            enc[c] = x.where(num_ok)
        elif n_nb and not some_text:
            categorical.append(c)
            enc[c] = (CAT_PREFIX + v).where(nb)
        elif n_lev > max_levels:
            dropped.append(c)
        else:
            categorical.append(c)
            enc[c] = (CAT_PREFIX + v).where(nb)
    out = pd.DataFrame(enc, index=frame.index)
    return out, dropped, categorical


# ------------------------------------------------------------------ histogram
def histogram(series_all: pd.Series, series_kept: pd.Series | None = None,
              bins: int = 20, top: int = 15) -> dict:
    """Distribution of a column: numeric -> shared-range bins; otherwise top
    category counts. 'all' = full dataset, 'kept' = rows passing the filters."""
    n_nonnull = int(series_all.notna().sum())
    num_all = as_numeric_safe(series_all)
    numeric = n_nonnull > 0 and int(num_all.notna().sum()) >= 0.5 * n_nonnull

    na_all = int(_is_missing(series_all).sum())
    na_kept = int(_is_missing(series_kept).sum()) if series_kept is not None else None

    if numeric:
        va = num_all.dropna().to_numpy()
        vk = (as_numeric_safe(series_kept).dropna().to_numpy()
              if series_kept is not None else None)
        if va.size == 0:
            return {"type": "numeric", "bins": [], "stats": None,
                    "na_all": na_all, "na_kept": na_kept}
        lo, hi = float(np.min(va)), float(np.max(va))
        if lo == hi:
            hi = lo + 1.0
        counts_all, edges = np.histogram(va, bins=bins, range=(lo, hi))
        counts_kept = (np.histogram(vk, bins=bins, range=(lo, hi))[0]
                       if vk is not None else None)
        out_bins = []
        for i in range(len(counts_all)):
            out_bins.append({
                "label": f"{edges[i]:.4g} – {edges[i+1]:.4g}",
                "all": int(counts_all[i]),
                "kept": int(counts_kept[i]) if counts_kept is not None else None,
            })
        def _stats(v):
            if v is None or v.size == 0:
                return None
            return {"n": int(v.size), "mean": float(np.mean(v)), "sd": float(np.std(v, ddof=1)) if v.size > 1 else 0.0,
                    "median": float(np.median(v)), "min": float(np.min(v)), "max": float(np.max(v))}
        return {"type": "numeric", "bins": out_bins,
                "stats": {"all": _stats(va), "kept": _stats(vk)},
                "na_all": na_all, "na_kept": na_kept}

    # categorical: top-N value counts, aligned between all/kept
    sa = as_str(series_all)
    vc_all = sa.dropna().value_counts()
    labels = [str(x) for x in vc_all.index[:top]]
    vc_kept = None
    if series_kept is not None:
        sk = as_str(series_kept)
        vc_kept = sk.dropna().value_counts()
    out_bins = []
    for lab in labels:
        out_bins.append({
            "label": lab,
            "all": int(vc_all.get(lab, 0)),
            "kept": int(vc_kept.get(lab, 0)) if vc_kept is not None else None,
        })
    other = int(vc_all.iloc[top:].sum()) if len(vc_all) > top else 0
    return {"type": "categorical", "bins": out_bins, "other_all": other,
            "n_levels": int(len(vc_all)), "stats": None,
            "na_all": na_all, "na_kept": na_kept}


# ------------------------------------------------------------- main entry point
def compute_counts(df: pd.DataFrame, cfg: dict) -> dict:
    """Live-feedback counts for the current config. Preview only."""
    resolver = build_resolver(list(df.columns))
    total = int(len(df))

    # (A) value mutation (minus-cyl standardization) -- no row change, but
    #     later filters may test the mutated values, so mirror it exactly.
    #     Visual acuity is NOT converted before filters (filters see it as recorded).
    work = apply_mutations(df, cfg, resolver)

    # (B) exclude rows missing required columns
    exclude = cfg.get("exclude_params_raw") or []
    excl_cols = [c for c in (resolve(e, resolver) for e in exclude) if c is not None]
    if excl_cols:
        mask = pd.Series(True, index=work.index)
        for c in excl_cols:
            mask = mask & ~_is_missing(work[c])
        work = work[mask.values]
    after_exclude = int(len(work))

    group_col = resolve(cfg.get("group_source_var"), resolver)
    groups = {k: v for k, v in (cfg.get("groups") or {}).items() if v}

    # per-group counts right after exclude = the baseline each filter erodes
    groups_after_exclude = ({g: int(group_mask(work[group_col], v).sum()) for g, v in groups.items()}
                            if (group_col and group_col in work.columns and groups) else None)

    # (C) filters (tracking per-group remaining at each step)
    work, steps = apply_filter_rules(work, cfg.get("filter_rules") or [], resolver,
                                     group_col=group_col, groups=groups)
    after_filters = int(len(work))

    # overall (pooled) group counts
    overall_groups = assign_group_counts(work, group_col, groups)

    # per-subset breakdown + union (subset selection acts as the final
    # filtration step: rows outside every subset are excluded from the runs)
    subset_col = resolve(cfg.get("subset_col_raw"), resolver)
    subsets = cfg.get("subsets") or {}
    subset_out: dict[str, Any] = {}
    after_subsets = None
    cohort = work
    if subset_col is not None and subset_col in work.columns and subsets:
        union_mask = match_values(work[subset_col], subset_union_values(cfg))
        after_subsets = int(union_mask.sum())
        cohort = work[union_mask.values]
        for sname, svals in subsets.items():
            sub = work[match_values(work[subset_col], svals).values]
            subset_out[sname] = {
                "total": int(len(sub)),
                "groups": assign_group_counts(sub, group_col, groups),
            }

    # the rows the engine analyses (decision D26): after the filters and the
    # subsets, only those that fall in a group - R/pipeline.R `analysed`, the
    # rows of the filtered-dataset CSV. None until a group is defined.
    in_analysis = None
    if group_col is not None and group_col in cohort.columns and groups:
        hit = pd.Series(False, index=cohort.index)
        for vals in groups.values():
            hit = hit | group_mask(cohort[group_col], vals)
        in_analysis = int(hit.sum())

    # patient identifiers: blank ids become one patient per eye in the run
    pid_col = resolve(cfg.get("patient_id_raw"), resolver)
    pid_missing = (int(_is_missing(cohort[pid_col]).sum())
                   if pid_col is not None and pid_col in cohort.columns else None)

    return {
        "total_rows": total,
        "after_exclude": after_exclude,
        "groups_after_exclude": groups_after_exclude,
        "filter_steps": steps,
        "after_filters": after_filters,
        "after_subsets": after_subsets,
        "in_analysis": in_analysis,
        "groups": overall_groups,
        "subsets": subset_out,
        "subset_col_missing": subset_column_missing(cfg, resolver),
        # rows (after filters and subsets) without a patient ID; None = no
        # ID column mapped (every row is then its own patient)
        "patient_id_missing": pid_missing,
        "patient_id_unmapped": pid_col is None,
    }
