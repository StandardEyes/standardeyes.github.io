"""
config_schema.py
----------------
One place that defines / normalizes the `cfg` dict the R engine consumes.
Keeps the front-end config and the R pipeline in lockstep. The cfg produced
here is interchangeable with the settings JSON of earlier releases.
"""

from __future__ import annotations

import math
import os
import re
from typing import Any

from preview import (NUMERIC_OPS, VA_FORMATS, as_str, build_resolver, match_values,
                     parse_filter_value, resolve, subset_mode, unresolved_reason)

# Keys the R pipeline reads, with safe defaults. (See R/pipeline.R.)
DEFAULTS: dict[str, Any] = {
    "sheet_name": 1,
    "guess_max": 120000,
    "patient_id_raw": "",
    "eye_id_raw": "",
    "group_source_var": "",
    "groups": {},
    "reference_group": "",
    "patient_level_params_raw": [],
    "exclude_params_raw": [],
    # nothing is inferred from column names: unmapped = row left blank
    "age_var": "",
    "fu_var": "",
    "sex_var": "",
    "baseline": {"continuous": [], "ordinal": [], "dichotomous": [], "categorical": []},
    "preop": {"continuous": [], "ordinal": [], "dichotomous": [], "categorical": []},
    "outcome": {"continuous": [], "ordinal": [], "dichotomous": [], "categorical": []},
    "filter_rules": [],
    "do_matching": True,
    "psm_covariates_raw": [],
    "psm_method": "nearest",
    "psm_distance": "logit",
    # 1:1 nearest-neighbour without replacement inside a 0.2-SD caliper of the
    # propensity score (logit model): the default (the example presets
    # use 2:1)
    "psm_ratio": 1,
    "psm_replace": False,
    "psm_caliper": 0.2,
    "psm_std_caliper": True,
    "psm_m_order": "largest",
    # fixed seed for MatchIt (m.order = "random" and ties), so identical
    # settings give identical matched cohorts in the run and the live preview
    "psm_seed": 20250601,
    "run_gee_stage": "matched",
    "run_main_gee": True,
    "run_adjusted_gee": False,
    "gee_adjust_covariates_raw": [],
    "use_matching_weights_in_gee": True,
    "add_gee_columns": True,
    "gee_source_for_tables": "main",
    "subset_col_raw": "",
    "subsets": {},
    "subset_mode": "separate",
    # legacy flag of settings files older than subset_mode (True = pooled +
    # separate runs); the engine reads it only when subset_mode is absent
    "subset_pooled": False,
    # GUI-only: every subset row incl. unticked ones, so a preset restores the
    # table exactly; the engine ignores it (it reads "subsets")
    "subset_rows": [],
    "within_group": {"continuous": [], "ordinal": [], "dichotomous": [], "categorical": []},
    # per within-group variable: "patient" collapses to one row per patient
    # before describing it, "eye" (the default) uses the rows as they are
    "within_group_levels": {},
    "do_standardize_minus_cyl": True,
    "std_triples": [],
    # GUI-only: true when the user emptied the set list on purpose, so the GUI
    # does not re-fill an empty std_triples from the roles; the engine ignores it
    "std_triples_user": False,
    "std_sph_raw": "",
    "std_cyl_raw": "",
    "std_axis_raw": "",
    # notation the visual acuity is RECORDED in, for every VA-role column and
    # every column in logmar_cols_raw: "decimal" | "logmar" | "snellen" | "etdrs"
    "va_format": "decimal",
    "do_logmar": False,
    "logmar_cols_raw": [],
    # stability-over-time figure: one entry per follow-up visit,
    # {label, time, seq, sph, cyl} -- SEQ is used when given, else sph + cyl/2
    "stability_figs": False,
    "stability_visits": [],
    "stability_include_preop": True,
    "stability_change_from": "",
    "stability_change_to": "",
    "stability_change_threshold": 0.50,
    "make_meyestro": True,
    "make_alpins": True,
    "make_pub_table": True,
    "make_graphs": True,
    # StandardEyes panel set, mirroring the published standard-graph sets:
    #   nine = everything; six = drops stability, DEQ and the vector panels;
    #   four = cataract set, additionally drops Safety and attempted-vs-achieved.
    "graph_set": "nine",
    "graph_formats": ["tiff"],
    "graph_dpi": 300,
    "group_colors": {},
    "group_labels": {},
    # astigmatism PANELS inside the standard set (refractive-astigmatism bar +
    # the vector panels). Off = a purely refractive figure set.
    "astig_panels": True,
    # JCRS-standard double-angle astigmatism plots and the grey single-angle
    # semicircle vector plots (TIA / SIA / DV / CI)
    "double_angle_figs": True,
    "semicircle_figs": True,
    # JCRS 2025 Part II says astigmatism data are usually NOT Gaussian, so the
    # nonparametric convex polygon is the default rather than the normal ellipse.
    "astig_region": "polygon",     # "normal" | "polygon" | "both"
    # follow the JCRS 2025 astigmatism working group: total SD, a bivariate
    # centroid test and Holm-adjusted group comparisons. Off = the older
    # behaviour (two separate univariate tests, no adjustment).
    "astig_jcrs_standard": True,
    # cluster the figure p-values by patient (GEE, robust SE) so both eyes of
    # one person are not counted as independent
    "cluster_by_patient": True,
    # bivariate centroid test: "robust" = trimmed Hotelling T2 calibrated by a
    # (cluster) bootstrap, which is what JCRS 2025 Part II describes;
    # "gee" = clustered GEE Wald; "classic" = Hotelling/MANOVA, eyes independent
    "astig_test_method": "robust",
    # optional pre-computed Alpins vectors {tia_mag,tia_axis,sia_mag,sia_axis,dv_mag,dv_axis};
    # anything not mapped is computed from sphere/cylinder/axis instead
    "astig_vector_cols": {},
    # optional intended/target refractive cylinder {cyl, axis}; when mapped, TIA
    # becomes the intended CHANGE rather than the preoperative astigmatism
    "astig_target": {},
    "do_regression": True,
    "regression_stage": "unmatched",
    "regression_covariates_raw": [],
    "roles": {},
    "dichotomous_preferred_level": {},
    "output_dir": "",
    "output_prefix": "StandardEyes",
    "export_filtered_csv": True,
}


def _coerce_dicho_pref(d: dict) -> dict:
    """Numeric-looking level values become numbers (e.g. '1' -> 1), matching
    the R pipeline's expected cfg shape."""
    out = {}
    for k, v in (d or {}).items():
        if isinstance(v, str):
            try:
                num = float(v)
                out[k] = int(num) if num.is_integer() else num
                continue
            except ValueError:
                pass
        out[k] = v
    return out


def _clean_filter_value(op: str, value: Any) -> Any:
    parsed = parse_filter_value(op, value)
    if isinstance(parsed, list):
        # drop NaNs that came from non-numeric input so JSON stays valid
        cleaned = [x for x in parsed if not (isinstance(x, float) and math.isnan(x))]
        return cleaned
    return parsed


GRAPH_FORMATS = ("png", "tiff", "jpeg", "pdf")
_FORMAT_ALIASES = {"jpg": "jpeg", "tif": "tiff"}
RESERVED_RUN_NAMES = ("pooled", "all")
SCALAR_NUMERIC_OPS = ("==", "!=", "<", "<=", ">", ">=", "abs_ge", "abs_le")


_WIN_DEVICES = {"CON", "PRN", "AUX", "NUL", "CONIN$", "CONOUT$",
                *(f"COM{i}" for i in range(1, 10)), *(f"LPT{i}" for i in range(1, 10))}


def safe_filename(x: Any, max_chars: int = 100, fallback: str = "StandardEyes") -> str:
    """Same rule as safe_filename() in R/helpers.R: every run of < > : " / \\
    | ? * becomes '_', control characters are dropped, surrounding spaces
    trimmed, at most `max_chars`, trailing dots / spaces trimmed. A reserved
    device name - judged, as Windows does, on the part before the FIRST dot
    with trailing spaces ignored (CON, NUL, COM1, CONIN$, ...) - gets '_'
    right after that part ('NUL.v2' -> 'NUL_.v2'), and an empty result
    becomes `fallback` (pass '' to see that nothing usable is left)."""
    s = re.sub(r'[<>:"/\\|?*]+', "_", str(x if x is not None else ""))
    s = re.sub(r"[\x00-\x1f\x7f-\x9f\u2028\u2029]+", "", s)
    s = s.strip(" \t\r\n")[:max_chars]
    s = re.sub(r"[. ]+$", "", s)
    if s.split(".", 1)[0].rstrip(" ").upper() in _WIN_DEVICES:
        s = re.sub(r"^([^. ]*)", r"\1_", s, count=1)
    return s or fallback


def _blank(v: Any) -> bool:
    return v is None or (isinstance(v, str) and v.strip() == "")


# MatchIt methods that run with the R packages bundled in runtime/Rlib (each
# checked through engine/match_preview.R). The others need a package the
# bundle does not ship, so the run would die inside matchit().
PSM_METHODS = ("nearest", "exact", "cem", "subclass")
PSM_METHOD_NEEDS = {
    "optimal": "the R package optmatch",
    "full": "the R package optmatch",
    "genetic": "the R packages Matching and rgenoud",
    "quick": "the R package quickmatch",
    "cardinality": "an optimisation solver (R package highs)",
}


def psm_method(cfg: dict) -> str:
    """The matching method as R receives it (blank = nearest, like .psm_settings())."""
    m = (cfg or {}).get("psm_method")
    return "nearest" if _blank(m) else str(m).strip().lower()


def psm_method_problem(cfg: dict) -> str | None:
    """Why the matching method cannot run with the bundled R library (None = it can)."""
    m = psm_method(cfg)
    if m in PSM_METHODS:
        return None
    ok = ", ".join(PSM_METHODS)
    if m in PSM_METHOD_NEEDS:
        return (f"Matching method '{m}' needs {PSM_METHOD_NEEDS[m]}, which StandardEyes' "
                f"bundled R library does not include - choose one of: {ok}.")
    return f"Matching method '{(cfg or {}).get('psm_method')}' is not supported - choose one of: {ok}."


# MatchIt distances that run with runtime/Rlib plus R's own library (each
# checked with matchit() under the engine's library path; gam and rpart use
# the recommended packages mgcv / rpart that every R install has). Others
# need a package the bundle does not ship, or an option the app never passes
# (nnet: a network size; the log link: starting values), so the run would die
# inside matchit().
PSM_DISTANCES = ("glm", "logit", "probit", "cloglog", "cauchit",
                 "linear.logit", "linear.probit", "linear.cloglog", "linear.cauchit",
                 "gam", "rpart",
                 "mahalanobis", "robust_mahalanobis", "euclidean", "scaled_euclidean")
_NOT_BUNDLED = "which StandardEyes' bundled R library does not include"
PSM_DISTANCE_NEEDS = {
    "log": "needs starting values for its log-link model, which StandardEyes does not pass",
    "linear.log": "needs starting values for its log-link model, which StandardEyes does not pass",
    "gbm": f"needs the R package gbm, {_NOT_BUNDLED}",
    "lasso": f"needs the R package glmnet, {_NOT_BUNDLED}",
    "ridge": f"needs the R package glmnet, {_NOT_BUNDLED}",
    "elasticnet": f"needs the R package glmnet, {_NOT_BUNDLED}",
    "randomforest": f"needs the R package randomForest, {_NOT_BUNDLED}",
    "cbps": f"needs the R package CBPS, {_NOT_BUNDLED}",
    "bart": f"needs the R package dbarts, {_NOT_BUNDLED}",
    "nnet": "needs a network size (MatchIt's distance.options), which StandardEyes does not pass",
}
# distances that are not propensity scores: MatchIt refuses them for subclass
_NON_PS_DISTANCES = ("mahalanobis", "robust_mahalanobis", "euclidean", "scaled_euclidean")


def psm_distance(cfg: dict) -> str:
    """The distance as R receives it (blank = glm, like .psm_settings();
    MatchIt itself ignores case)."""
    d = (cfg or {}).get("psm_distance")
    return "glm" if _blank(d) else str(d).strip().lower()


def psm_distance_problem(cfg: dict) -> str | None:
    """Why the propensity distance cannot run with the bundled R library for
    the chosen method (None = it can). exact and cem do not use a distance."""
    m = psm_method(cfg)
    if m not in ("nearest", "subclass"):
        return None
    d = psm_distance(cfg)
    ok = ", ".join(x for x in PSM_DISTANCES if m != "subclass" or x not in _NON_PS_DISTANCES)
    if d in PSM_DISTANCE_NEEDS:
        return f"Matching distance '{d}' {PSM_DISTANCE_NEEDS[d]} - choose one of: {ok}."
    if d not in PSM_DISTANCES:
        return (f"Matching distance '{(cfg or {}).get('psm_distance')}' is not supported - "
                f"choose one of: {ok}.")
    if m == "subclass" and d in _NON_PS_DISTANCES:
        return (f"Matching distance '{d}' cannot be used with the subclass method (it needs a "
                f"propensity score) - choose one of: {ok}.")
    if d in _NON_PS_DISTANCES and normalize_psm(cfg).get("psm_caliper") is not None:
        # MatchIt refuses an unnamed caliper when the distance is not a score
        return (f"Matching distance '{d}' is not a propensity score, so the caliper (in SDs of "
                "the score) cannot apply - leave the caliper empty or choose a propensity "
                "distance such as logit.")
    order = (cfg or {}).get("psm_m_order")
    if d in _NON_PS_DISTANCES and not _blank(order) and str(order).strip().lower() in ("largest", "smallest"):
        # 'largest' / 'smallest' order the eyes by their propensity score
        return (f"Match order '{str(order).strip()}' needs a propensity score; with distance "
                f"'{d}' choose the order data, random, closest or farthest.")
    return None


def normalize_psm(cfg: dict) -> dict:
    """The matching settings as R needs them. Alpine's x-model.number sends ''
    for a cleared box: an empty caliper means no caliper, an empty ratio the
    default ratio. Used by the run AND the live match preview, so both match
    the same way."""
    out = {}
    ratio = (cfg or {}).get("psm_ratio")
    try:
        r = float(ratio)
        out["psm_ratio"] = int(round(r)) if math.isfinite(r) and r >= 1 else DEFAULTS["psm_ratio"]
    except (TypeError, ValueError):
        out["psm_ratio"] = DEFAULTS["psm_ratio"]
    cal = (cfg or {}).get("psm_caliper")
    if _blank(cal):
        out["psm_caliper"] = None
    else:
        try:
            c = float(cal)
            out["psm_caliper"] = c if math.isfinite(c) and c > 0 else None
        except (TypeError, ValueError):
            out["psm_caliper"] = None
    seed = (cfg or {}).get("psm_seed")
    try:
        out["psm_seed"] = int(float(seed)) if not _blank(seed) else DEFAULTS["psm_seed"]
    except (TypeError, ValueError):
        out["psm_seed"] = DEFAULTS["psm_seed"]
    return out


def normalize_cfg(cfg: dict, file_path: str, sheet: Any = None) -> dict:
    """Fill defaults, wire in the data path, and type filter values for R."""
    out = {**DEFAULTS, **(cfg or {})}
    out["file_path"] = file_path
    if sheet is not None:
        out["sheet_name"] = sheet

    # filter rules: parse the UI value into typed arrays R expects
    rules = []
    for r in out.get("filter_rules") or []:
        var, op = r.get("var"), r.get("op")
        if not var or not op:
            continue
        rules.append({"var": var, "op": op, "value": _clean_filter_value(op, r.get("value"))})
    out["filter_rules"] = rules

    out["dichotomous_preferred_level"] = _coerce_dicho_pref(out.get("dichotomous_preferred_level"))

    out.update(normalize_psm(out))
    out["psm_method"] = psm_method(out)
    out["psm_distance"] = psm_distance(out)
    # read from the cfg AS SENT: the default above would hide a legacy
    # settings file's subset_pooled
    out["subset_mode"] = subset_mode(cfg or {})

    fmt = out.get("va_format")
    out["va_format"] = str(fmt).strip().lower() if not _blank(fmt) else "decimal"

    fmts = out.get("graph_formats") or []
    if isinstance(fmts, str):
        fmts = [fmts]
    norm_fmts: list[str] = []
    for f in fmts:
        f = _FORMAT_ALIASES.get(str(f).strip().lower(), str(f).strip().lower())
        if f and f not in norm_fmts:
            norm_fmts.append(f)
    out["graph_formats"] = norm_fmts or list(DEFAULTS["graph_formats"])

    prefix = safe_filename(out.get("output_prefix"))
    out["output_prefix"] = prefix or DEFAULTS["output_prefix"]

    if not out.get("output_dir"):
        out["output_dir"] = os.path.join(os.path.expanduser("~"), "StandardEyes_Output")
    out["output_dir"] = os.path.expandvars(os.path.expanduser(str(out["output_dir"]).strip().strip('"')))

    # bookkeeping the Shiny app also stored (harmless to R)
    out["num_groups"] = len(out.get("groups") or {})
    out["num_filters"] = len(out.get("filter_rules") or [])
    out["num_subsets"] = len(out.get("subsets") or {})
    return out


def overlapping_group_values(groups: dict, series: Any = None) -> list[str]:
    """Group values that put one row into two groups: the engine stops on
    them (add_group_label in R/pipeline.R).

    With the loaded group column (`series`) this is judged exactly as R
    judges it: rows that match_values() assigns to more than one group. On a
    numeric column '1' and '1.0' are then one code; on a text column '01' and
    '1' are two categories and never collide. Without data only a value
    listed twice in the same (trimmed) spelling is certain to collide."""
    groups = {g: v for g, v in (groups or {}).items() if v}
    dup: list[str] = []
    if series is not None:
        names = list(groups)
        masks = [match_values(series, groups[g]).to_numpy(dtype=bool) for g in names]
        if len(masks) < 2:
            return []
        multi = sum(m.astype(int) for m in masks) > 1
        if not multi.any():
            return []
        vals = as_str(series).str.strip()
        for v in _unique_in_order([x for x, hit in zip(vals.tolist(), multi) if hit])[:5]:
            rows = multi & (vals == v).fillna(False).to_numpy(dtype=bool)
            gs = [g for g, m in zip(names, masks) if (m & rows).any()]
            dup.append(f"'{v}' ({' and '.join(map(str, gs))})")
        return dup
    owner: dict[str, str] = {}
    for g, vals in groups.items():
        vals = vals if isinstance(vals, (list, tuple)) else [vals]
        for v in vals:
            if v is None or str(v).strip() == "":
                continue
            k = str(v).strip()
            if k in owner and owner[k] != g:
                msg = f"'{k}' ({owner[k]} and {g})"
                if msg not in dup:
                    dup.append(msg)
            else:
                owner.setdefault(k, g)
    return dup


def _unique_in_order(xs: list) -> list:
    """Distinct values in first-seen order."""
    seen: set = set()
    return [x for x in xs if not (x in seen or seen.add(x))]


def _names(x: Any) -> list[str]:
    if x is None:
        return []
    if isinstance(x, (list, tuple)):
        return [str(v) for v in x if v is not None and str(v).strip() != ""]
    return [str(x)] if str(x).strip() != "" else []


def column_problems(cfg: dict, columns: list) -> tuple[list[str], list[str]]:
    """Mapped column names that do not resolve in the loaded file (same
    exact-first resolver as R). Returns (errors, warnings):
    errors  - columns that define the statistical model (groups, patient
              clustering, matching / adjustment / regression covariates);
              R would silently drop them and still 'succeed';
    warnings - columns R skips with a log line (roles, table variables, VA
              and standardisation columns, stability visits, filters, ...).
    The subset column is checked separately by the caller."""
    resolver = build_resolver(list(columns))
    errs: list[str] = []
    warn: list[str] = []

    def bad(name: str) -> str | None:
        return None if resolve(name, resolver) is not None else unresolved_reason(name, resolver)

    def need(label: str, names: list[str]) -> None:
        for n in names:
            why = bad(n)
            if why:
                errs.append(f"{label} '{n}': {why} in this file - re-select it.")

    need("Group source column", _names(cfg.get("group_source_var")))
    need("Patient ID column", _names(cfg.get("patient_id_raw")))
    if cfg.get("do_matching"):
        need("Matching covariate", _names(cfg.get("psm_covariates_raw")))
    if cfg.get("run_adjusted_gee"):
        need("GEE adjustment covariate", _names(cfg.get("gee_adjust_covariates_raw")))
    if cfg.get("do_regression"):
        need("Regression covariate", _names(cfg.get("regression_covariates_raw")))

    soft: list[tuple[str, str]] = []           # (where, name)
    for k in ("eye_id_raw", "age_var", "fu_var", "sex_var"):
        soft += [(k, n) for n in _names(cfg.get(k))]
    for role, n in (cfg.get("roles") or {}).items():
        soft += [(f"role {role}", x) for x in _names(n)]
    for sec in ("baseline", "preop", "outcome", "within_group"):
        for kind, vals in (cfg.get(sec) or {}).items():
            if isinstance(vals, (list, tuple)):
                soft += [(f"{sec} {kind}", x) for x in _names(vals)]
    soft += [("excluded-if-missing", n) for n in _names(cfg.get("exclude_params_raw"))]
    soft += [("patient-level", n) for n in _names(cfg.get("patient_level_params_raw"))]
    soft += [("LogMAR", n) for n in _names(cfg.get("logmar_cols_raw"))]
    if cfg.get("do_standardize_minus_cyl"):
        triples = cfg.get("std_triples") or [{"sph": cfg.get("std_sph_raw"),
                                              "cyl": cfg.get("std_cyl_raw"),
                                              "axis": cfg.get("std_axis_raw")}]
        for tr in triples:
            for part in ("sph", "cyl", "axis"):
                soft += [(f"minus-cyl {part}", x) for x in _names((tr or {}).get(part))]
    if cfg.get("stability_figs"):
        for v in cfg.get("stability_visits") or []:
            for part in ("time", "seq", "sph", "cyl"):
                soft += [(f"stability visit {(v or {}).get('label', '')} {part}", x)
                         for x in _names((v or {}).get(part))]
    for key in ("astig_vector_cols", "astig_target"):
        for part, n in (cfg.get(key) or {}).items():
            soft += [(f"{key} {part}", x) for x in _names(n)]
    for r in cfg.get("filter_rules") or []:
        if r.get("op"):
            soft += [("filter", x) for x in _names(r.get("var"))]

    seen: set[tuple[str, str]] = set()
    for where, n in soft:
        why = bad(n)
        if why and (where, n) not in seen:
            seen.add((where, n))
            warn.append(f"'{n}' ({where}): {why}")
    if warn:
        warn = [f"{len(warn)} mapped column(s) are not in this file and will be skipped: "
                + "; ".join(warn) + "."]
    return errs, warn


def validate_cfg(cfg: dict, columns: list | None = None, data: Any = None) -> list[str]:
    """Return a list of human-readable problems (empty == OK to run).
    With `columns` (the loaded file's headers) every column that defines the
    statistical model must also resolve in that file. With `data` (the loaded
    frame) group values are checked on the group column as the engine does."""
    errs = []
    groups = cfg.get("groups") or {}
    non_empty = {k: v for k, v in groups.items() if v}
    if len(non_empty) < 2:
        errs.append("Define at least 2 groups, each with one or more source values.")
    gseries = None
    if data is not None and hasattr(data, "columns"):
        gcol = resolve(cfg.get("group_source_var"), build_resolver(list(data.columns)))
        if gcol is not None and gcol in data.columns:
            gseries = data[gcol]
    dup = overlapping_group_values(non_empty, gseries)
    if dup:
        errs.append("A source value can belong to one group only: " + ", ".join(dup) + ".")
    if columns is not None:
        errs += column_problems(cfg, columns)[0]
    if not cfg.get("group_source_var"):
        errs.append("Choose the group source column.")
    ref = cfg.get("reference_group")
    if not _blank(ref) and ref not in non_empty:
        errs.append(f"Reference group '{ref}' has no source values - add values to it "
                    "or pick another reference group.")
    if cfg.get("do_matching") and not (cfg.get("psm_covariates_raw") or []):
        errs.append("Matching is enabled but no matching covariates are selected.")
    if cfg.get("do_matching"):
        why = psm_method_problem(cfg) or psm_distance_problem(cfg)
        if why:
            errs.append(why)
    for r in cfg.get("filter_rules") or []:
        op = str(r.get("op", "")).lower()
        val = r.get("value")
        if op in ("between", "abs_between"):
            vals = val if isinstance(val, list) else []
            if len(vals) != 2:
                errs.append(f"Filter on '{r.get('var')}': '{op}' needs both a lower and an upper limit.")
        elif op in SCALAR_NUMERIC_OPS:
            # R compares the column with ONE number; an empty list would make
            # the engine die with an internal subscript error
            vals = val if isinstance(val, list) else [val]
            ok = len(vals) == 1 and all(
                isinstance(x, (int, float)) and not isinstance(x, bool) and math.isfinite(x)
                for x in vals)
            if not ok:
                errs.append(f"Filter on '{r.get('var')}': '{op}' needs exactly one number "
                            "(use in / notin / contains for text).")
        elif op in ("in", "notin") and not val:
            errs.append(f"Filter on '{r.get('var')}': pick at least one value for '{op}'.")
        elif op in ("contains", "not_contains") and not [
                x for x in (val if isinstance(val, (list, tuple)) else [val])
                if x is not None and str(x).strip() != ""]:
            # R matches no row on an empty text, so 'contains' would keep only
            # the rows where the column is blank (and 'not_contains' nothing)
            errs.append(f"Filter on '{r.get('var')}': enter the text to look for with '{op}'.")

    fmt = cfg.get("va_format", "decimal")
    if fmt not in VA_FORMATS:
        errs.append(f"Visual-acuity notation '{fmt}' is not supported - use one of: "
                    + ", ".join(VA_FORMATS) + ".")

    bad_fmts = [f for f in (cfg.get("graph_formats") or []) if f not in GRAPH_FORMATS]
    if bad_fmts:
        errs.append("Unsupported graph format(s): " + ", ".join(map(str, bad_fmts))
                    + ". Use " + ", ".join(GRAPH_FORMATS) + ".")

    # run labels become file names: reserved and colliding names would make
    # one run's workbook and figures overwrite another's
    subsets = cfg.get("subsets") or {}
    if cfg.get("subset_col_raw") and subsets:
        seen: dict[str, str] = {}
        for name in subsets:
            key = safe_filename(name, fallback="").lower()
            if key in RESERVED_RUN_NAMES:
                errs.append(f"Subset name '{name}' is reserved (Pooled / All name the "
                            "combined runs) - rename the subset.")
            elif not key:
                errs.append(f"Subset name '{name}' cannot be used as a file name - rename the subset.")
            elif key in seen:
                errs.append(f"Subsets '{seen[key]}' and '{name}' would get the same file "
                            "name - rename one of them.")
            else:
                seen[key] = name
    return errs


MEYESTRO_ROLES = ("pre_bcva", "post_ucva", "post_bcva", "pre_sph", "pre_cyl", "pre_axis",
                  "post_sph", "post_cyl", "post_axis", "post_seq")
ALPINS_ROLES = ("eye", "pre_cyl", "pre_axis", "post_cyl", "post_axis")


def run_notes(cfg: dict, columns: list | None = None) -> list[str]:
    """Non-blocking remarks shown in the run log before R starts: export tabs
    that were requested but whose roles are not (all) mapped to a column."""
    roles = cfg.get("roles") or {}
    resolver = build_resolver(list(columns)) if columns is not None else None

    def unmapped(keys):
        out = []
        for k in keys:
            v = roles.get(k)
            if _blank(v) or (resolver is not None and resolve(v, resolver) is None):
                out.append(k)
        return out

    notes = []
    for flag, keys, label in (("make_meyestro", MEYESTRO_ROLES, "mEYEstro"),
                              ("make_alpins", ALPINS_ROLES, "AstigMATIC")):
        if not cfg.get(flag):
            continue
        miss = unmapped(keys)
        if miss:
            notes.append(f"{label} tabs: no column mapped for {', '.join(miss)} - these columns "
                         "will be blank, or the tabs skipped when too few roles are mapped "
                         "(map them under Column roles).")
    return notes


def validate_raw(cfg: dict) -> list[str]:
    """Problems that normalize_cfg() would otherwise paper over: typed values
    that are not numbers. Checked on the cfg AS SENT by the UI."""
    cfg = cfg or {}
    errs: list[str] = []
    for r in cfg.get("filter_rules") or []:
        var, op = r.get("var"), str(r.get("op") or "").lower()
        if not var or op not in NUMERIC_OPS or op in ("between_any",):
            continue
        parsed = parse_filter_value(op, r.get("value"))
        if any(isinstance(x, float) and math.isnan(x) for x in parsed):
            errs.append(f"Filter on '{var}': '{r.get('value')}' is not a number.")
        elif op not in ("between", "abs_between") and len(parsed) == 0:
            errs.append(f"Filter on '{var}': '{op}' needs a number.")
    # ratio and caliper are nearest-neighbour settings: exact, cem and
    # subclass ignore them (the interface disables those inputs), so a stale
    # value there must not block the run
    if cfg.get("do_matching") and psm_method(cfg) == "nearest":
        ratio = cfg.get("psm_ratio")
        if not _blank(ratio):
            try:
                rv = float(ratio)
                ok = math.isfinite(rv) and rv >= 1 and float(rv).is_integer()
            except (TypeError, ValueError):
                ok = False
            if not ok:
                errs.append(f"Matching ratio '{ratio}' must be a whole number of 1 or more.")
        cal = cfg.get("psm_caliper")
        if not _blank(cal):
            try:
                cv = float(cal)
                ok = math.isfinite(cv) and cv > 0
            except (TypeError, ValueError):
                ok = False
            if not ok:
                errs.append(f"Caliper '{cal}' must be a positive number (leave it empty for no caliper).")
    return errs
