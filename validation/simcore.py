"""
simcore.py - shared building blocks of the StandardEyes v1.2.0 demonstration
datasets DS-A to DS-E (make_dataset_A.py ... make_dataset_E.py).

Simulated data - no real patients. The blocks below are the patient-effect
model of make_example_data.py (dataset version 2, v1.1.0), cut into reusable
pieces: patients with latent means, fellow eyes correlated through patient
effects, refraction on the 0.25 D grid with SEQ = sphere + cylinder/2 exactly,
mirror-symmetric axes, plus-format double-angle astigmatism, acuity on chart
lines (or letters), the ordinal CDVA-change model and UDVA from the residual
refraction. Every dataset script owns its constants and its README text.

Conventions
  * refraction is built in MINUS-cylinder form: sphere s, magnitude m >= 0 and
    the PLUS axis (steep meridian) p; the minus triple is (s, -m, p + 90) and
    SEQ = s - m/2. The plus triple is (s - m, +m, p).
  * astigmatism vectors are plus-format double-angle vectors (JCRS 2025):
    x = m cos 2p, y = m sin 2p, so with-the-rule (plus axis 90) lies at x < 0.
  * acuity is handled as a continuous latent logMAR per measurement, then
    recorded on a chart: whole 0.1-logMAR lines (decimal or logMAR notation),
    ETDRS letters (0.02 logMAR each), or a classic Snellen chart line with a
    letter suffix.
"""
from __future__ import annotations

import hashlib
import json
import os
import platform
import sys

import numpy as np
import pandas as pd

HERE = os.path.dirname(os.path.abspath(__file__))


def _out_folder():
    """The folder the dataset and its settings files are written to: the one
    after --out on the command line, else an "out" folder beside the scripts."""
    a = sys.argv[1:]
    if "--out" in a and a.index("--out") + 1 < len(a):
        return os.path.abspath(a[a.index("--out") + 1])
    return os.path.join(HERE, "out")


OUT = _out_folder()

# decimal acuity of each 0.1-logMAR chart line (logMAR -0.3 ... 1.7)
DEC_LINE = {-3: 2.0, -2: 1.6, -1: 1.25, 0: 1.0, 1: 0.8, 2: 0.63, 3: 0.5, 4: 0.4, 5: 0.32,
            6: 0.25, 7: 0.2, 8: 0.16, 9: 0.125, 10: 0.1, 11: 0.08, 12: 0.063, 13: 0.05,
            14: 0.04, 15: 0.032, 16: 0.025, 17: 0.02}
# Snellen denominators (20/x) of a classic Snellen chart (not a logMAR chart)
SNELLEN_CHART = (10, 15, 20, 25, 30, 40, 50, 60, 70, 80, 100, 200, 400)
SNELLEN_CF_ABOVE = 1.35        # logMAR worse than this: "CF" (counting fingers), off the chart

# the 0.1-logMAR change in CDVA (gain +) at the reference covariates, with the
# thin loss tails of contemporary myopic LASIK / PRK series (about 0.5% of eyes
# lose 2 or more lines, almost none 3); make_example_data.py (v1.1.0) had
# {-3: 0.003, -2: 0.012, -1: 0.060, 0: 0.605, 1: 0.265, 2: 0.050, 3: 0.005}
CDVA_CHANGE_P = {-3: 0.0005, -2: 0.004, -1: 0.045, 0: 0.62, 1: 0.27, 2: 0.055, 3: 0.0055}
CDVA_PRE = {"mean": -0.035, "per_D": 0.010, "sd_patient": 0.05, "sd_eye": 0.025}
UDVA_PRE = {"offset": -0.05, "per_D": 0.22, "per_cyl": 0.12, "sd": 0.07}
# the last line of a distance chart: logMAR 1.3 = decimal 0.05 = 20/400 = 20
# ETDRS letters; a poorer acuity is recorded as off the chart ("CF", counting
# fingers), which StandardEyes sets to missing and counts in the run log
CHART_LAST_LINE = 13
OFF_CHART = "CF"
UDVA_POST = {"offset": -0.03, "myopic": 0.30, "myopic_dz": 0.20, "hyperopic": 0.12, "hyperopic_dz": 0.37,
             "per_cyl": 0.15, "cyl_dz": 0.25, "sd_patient": 0.03, "sd_eye": 0.04}


# ---------------------------------------------------------------------------
# numerics
# ---------------------------------------------------------------------------
def expit(x):
    return 1.0 / (1.0 + np.exp(-x))


def norm_ppf(p):
    """Inverse standard normal (Acklam's rational approximation, |error| < 1.2e-9)."""
    p = np.atleast_1d(np.asarray(p, float))
    a = [-3.969683028665376e+01, 2.209460984245205e+02, -2.759285104469687e+02,
         1.383577518672690e+02, -3.066479806614716e+01, 2.506628277459239e+00]
    b = [-5.447609879822406e+01, 1.615858368580409e+02, -1.556989798598866e+02,
         6.680131188771972e+01, -1.328068155288572e+01]
    c = [-7.784894002430293e-03, -3.223964580411365e-01, -2.400758277161838e+00,
         -2.549732539343734e+00, 4.374664141464968e+00, 2.938163982698783e+00]
    d = [7.784695709041462e-03, 3.224671290700398e-01, 2.445134137142996e+00,
         3.754408661907416e+00]
    lo, hi = 0.02425, 1 - 0.02425
    out = np.empty_like(p)
    m = p < lo
    q = np.sqrt(-2 * np.log(p[m]))
    out[m] = (((((c[0]*q+c[1])*q+c[2])*q+c[3])*q+c[4])*q+c[5]) / ((((d[0]*q+d[1])*q+d[2])*q+d[3])*q+1)
    m = (p >= lo) & (p <= hi)
    q = p[m] - 0.5
    r = q * q
    out[m] = (((((a[0]*r+a[1])*r+a[2])*r+a[3])*r+a[4])*r+a[5])*q / (((((b[0]*r+b[1])*r+b[2])*r+b[3])*r+b[4])*r+1)
    m = p > hi
    q = np.sqrt(-2 * np.log(1 - p[m]))
    out[m] = -(((((c[0]*q+c[1])*q+c[2])*q+c[3])*q+c[4])*q+c[5]) / ((((d[0]*q+d[1])*q+d[2])*q+d[3])*q+1)
    return out


def norm_cdf(x):
    from math import erf, sqrt
    return 0.5 * (1.0 + np.vectorize(erf)(np.asarray(x, float) / sqrt(2.0)))


def q25(x):
    """Round to the 0.25 D grid (no negative zero)."""
    return np.round(np.asarray(x, float) * 4) / 4 + 0.0


def wrap_axis(a):
    """Any angle in degrees -> an integer axis 1..180 (0 is written as 180)."""
    a = np.round(np.asarray(a, float)) % 180
    return np.where(a == 0, 180, a)


def correlated_normal(rng, n, rho):
    """(n, 2) standard normals with correlation rho between the two eyes."""
    v = rng.normal(0, 1, (n, 1))
    e = rng.normal(0, 1, (n, 2))
    return np.sqrt(rho) * v + np.sqrt(1 - rho) * e


def vec(mag, plus_axis):
    t = np.radians(2 * np.asarray(plus_axis, float))
    return mag * np.cos(t), mag * np.sin(t)


def mag_axis(x, y):
    """Plus-format double-angle vector -> (magnitude, plus axis 0..180)."""
    return np.hypot(x, y), (np.degrees(np.arctan2(y, x)) / 2) % 180


# ---------------------------------------------------------------------------
# patients and eyes
# ---------------------------------------------------------------------------
def patients(rng, m, age_mean, age_sd, age_lo, age_hi, p_female, p_bilateral):
    """m candidate patients: age, sex and which eyes are treated (OD, OS)."""
    age = np.clip(np.round(rng.normal(age_mean, age_sd, m)), age_lo, age_hi)
    female = rng.random(m) < p_female
    bilateral = rng.random(m) < p_bilateral
    only_od = rng.random(m) < 0.5
    present = np.column_stack([bilateral | only_od, bilateral | ~only_od])
    return dict(age=age, female=female, present=present)


def axes(rng, m, type_p, patient_sd=12.0, eye_sd=4.0, mirror_sd=8.0):
    """PLUS axes (steep meridian) of both eyes: with-the-rule around 90,
    against-the-rule around 180, oblique 45 or 135 (type_p = WTR, ATR,
    oblique); the left eye is the mirror image of the right (180 - axis)."""
    atype = rng.choice(3, size=m, p=type_p)
    obl = np.where(rng.random(m) < 0.5, 45.0, 135.0)
    base = np.select([atype == 0, atype == 1], [90.0, 180.0], obl) + rng.normal(0, patient_sd, m)
    od = base + rng.normal(0, eye_sd, m)
    os_ = 180 - base + rng.normal(0, mirror_sd, m)
    return wrap_axis(np.column_stack([od, os_])), atype


def cylinder(rng, lcyl_p, sd_e, lo=0.0, hi=4.5):
    """Cylinder magnitude of both eyes: exp(patient log mean + N(0, sd_e)) on
    the 0.25 D grid, kept inside [lo, hi] by redrawing (no pile-up at the bounds)."""
    m = len(lcyl_p)
    out = np.exp(lcyl_p[:, None] + rng.normal(0, sd_e, (m, 2)))
    for _ in range(60):
        bad = (q25(out) < lo) | (q25(out) > hi)
        if not bad.any():
            break
        out[bad] = np.exp(np.broadcast_to(lcyl_p[:, None], out.shape)[bad] + rng.normal(0, sd_e, bad.sum()))
    return np.clip(q25(out), lo, hi)


def refraction_from_seq(seq_latent, mag):
    """Minus-cylinder sphere on the 0.25 D grid and the DERIVED SEQ."""
    sph = q25(seq_latent + mag / 2)
    return sph, sph - mag / 2


def minus_triple(sph, mag, plus_axis):
    return sph, -np.asarray(mag, float) + 0.0, wrap_axis(np.asarray(plus_axis, float) + 90)


def minus_recorded(sph, mag, plus_axis):
    """The minus triple as a clinic records it: a blank axis where the cylinder is 0."""
    s, c, a = minus_triple(sph, mag, plus_axis)
    a = np.asarray(a, float).copy()
    a[np.asarray(mag, float) == 0] = np.nan
    return s, c, a


def plus_triple(sph, mag, plus_axis):
    return np.asarray(sph, float) - mag, np.asarray(mag, float) + 0.0, wrap_axis(plus_axis)


# ---------------------------------------------------------------------------
# astigmatic correction
# ---------------------------------------------------------------------------
def astig_post(rng, pre_mag, pre_pax, tgt_mag, tgt_pax, ci_mean, ci_p_sd, ci_e_sd,
               aoe_mean, aoe_p_sd, aoe_e_sd, noise_p_sd, noise_e_sd, ci_min=0.3):
    """Postoperative astigmatism (plus-format double-angle vectors).
    TIA = T - PRE (the intended change); SIA = CI x R(2 AoE) x TIA;
    POST = PRE + SIA + N(0, noise_p) patient + N(0, noise_e) eye per component.
    CI = ci_mean + N(0, ci_p_sd) patient + N(0, ci_e_sd) eye (ci_mean may be an
    (n, 1) or (n, 2) array); AoE = aoe_mean + N(0, aoe_p_sd) patient (mirrored
    in OS) + N(0, aoe_e_sd) eye, in degrees - aoe_mean itself is NOT mirrored
    (a systematic rotation in the same direction in both eyes). The patient
    part of the residual is mirrored in OS (y changes sign).
    Returns dict of continuous vectors (pre, target, tia, sia, post) and ci, aoe."""
    n = pre_mag.shape[0]
    px, py = vec(pre_mag, pre_pax)
    tx, ty = vec(tgt_mag, tgt_pax)
    ix, iy = tx - px, ty - py
    ci = np.asarray(ci_mean, float) + rng.normal(0, ci_p_sd, (n, 1)) + rng.normal(0, ci_e_sd, (n, 2))
    ci = np.maximum(ci, ci_min)
    mirror = np.array([[1.0, -1.0]])
    aoe_deg = np.asarray(aoe_mean, float) + rng.normal(0, aoe_p_sd, (n, 1)) * mirror + rng.normal(0, aoe_e_sd, (n, 2))
    a = np.radians(aoe_deg)
    c2, s2 = np.cos(2 * a), np.sin(2 * a)
    sx, sy = ci * (c2 * ix - s2 * iy), ci * (s2 * ix + c2 * iy)
    qx = px + sx + rng.normal(0, noise_p_sd, (n, 1)) + rng.normal(0, noise_e_sd, (n, 2))
    qy = py + sy + rng.normal(0, noise_p_sd, (n, 1)) * mirror + rng.normal(0, noise_e_sd, (n, 2))
    return dict(px=px, py=py, tx=tx, ty=ty, ix=ix, iy=iy, sx=sx, sy=sy, qx=qx, qy=qy, ci=ci, aoe=aoe_deg)


def post_refraction(qx, qy, post_seq_latent, mag_max=4.5):
    """Postoperative magnitude (0.25 D grid), plus axis, minus sphere and SEQ."""
    pmag = np.minimum(q25(np.hypot(qx, qy)), mag_max)
    ppax = wrap_axis(np.degrees(np.arctan2(qy, qx)) / 2)
    psph, pseq = refraction_from_seq(post_seq_latent, pmag)
    return pmag, ppax, psph, pseq


# ---------------------------------------------------------------------------
# acuity
# ---------------------------------------------------------------------------
def cdva_pre_latent(rng, seq, n):
    """Continuous preoperative CDVA (logMAR): better in low myopia."""
    c = CDVA_PRE
    lat = (c["mean"] + c["per_D"] * (-seq - 4) + rng.normal(0, c["sd_patient"], (n, 1))
           + rng.normal(0, c["sd_eye"], (n, 2)))
    return np.clip(lat, -0.24, 0.34)


def udva_pre_latent(rng, cd_pre_lat, seq, mag, n, worst=2.5):
    """Continuous preoperative UDVA (logMAR), never better than CDVA. It is not
    held at the chart's last line: an eye beyond it is recorded off the chart
    (dec_or_off, letters_or_off), so no reading piles up at the chart floor."""
    u = UDVA_PRE
    lat = cd_pre_lat + u["offset"] + u["per_D"] * np.abs(seq) + u["per_cyl"] * mag + rng.normal(0, u["sd"], (n, 2))
    return np.clip(np.maximum(lat, cd_pre_lat), -0.24, worst)


def ordinal_change(rng, n, probs=CDVA_CHANGE_P, rho=0.75, shift=0.0):
    """Lines gained (+) / lost (-): ordinal probit with fellow-eye latent
    correlation rho; `shift` (n, 2) or scalar moves the latent (positive =
    more gain). At shift 0 the categories have the probabilities `probs`."""
    ks = sorted(probs)
    cuts = norm_ppf(np.cumsum([probs[k] for k in ks])[:-1])
    z = correlated_normal(rng, n, rho) + shift
    return (z[..., None] > cuts).sum(-1) + ks[0]


def udva_blur(seq, cyl_mag):
    """logMAR added to CDVA by a residual refraction (dead zones, then linear)."""
    u = UDVA_POST
    return (u["offset"] + u["myopic"] * np.maximum(-seq - u["myopic_dz"], 0)
            + u["hyperopic"] * np.maximum(seq - u["hyperopic_dz"], 0)
            + u["per_cyl"] * np.maximum(cyl_mag - u["cyl_dz"], 0))


def udva_post_latent(rng, cd_post_lat, pseq, pmag, n):
    u = UDVA_POST
    lat = (cd_post_lat + udva_blur(pseq, pmag) + rng.normal(0, u["sd_patient"], (n, 1))
           + rng.normal(0, u["sd_eye"], (n, 2)))
    return np.clip(np.maximum(lat, cd_post_lat), -0.24, 1.7)


def line_of(logmar):
    """logMAR -> whole chart line (0.1 logMAR steps), as an integer."""
    return np.round(np.asarray(logmar, float) * 10).astype(int)


def dec_of(line):
    return np.vectorize(DEC_LINE.get)(np.asarray(line, int)).astype(float)


def dec_or_off(line, last=CHART_LAST_LINE):
    """Decimal acuity of a chart line as recorded: OFF_CHART ("CF") beyond the
    chart's last line (logMAR 1.3, decimal 0.05); an object array."""
    ln = np.asarray(line, int)
    out = np.empty(ln.shape, dtype=object)
    on = ln <= last
    out[on] = dec_of(np.where(on, ln, 0))[on]
    out[~on] = OFF_CHART
    return out


def letters_of(logmar):
    """ETDRS letter score of a logMAR value (85 letters = logMAR 0.0), 0..100."""
    return np.clip(np.round(85 - 50 * np.asarray(logmar, float)), 0, 100).astype(int)


def cdva_change_text(probs=CDVA_CHANGE_P):
    """The CDVA-change distribution for a README row: '-3: 0.05%, -2: 0.4%, ...'."""
    return ", ".join(f"{k:+d}: {round(v * 100, 4):g}%" for k, v in probs.items())


def decimal_lines_text(last=CHART_LAST_LINE):
    """The decimal chart lines a README lists, and what is recorded beyond the last one."""
    vals = ", ".join(f"{DEC_LINE[k]:g}" for k in sorted(DEC_LINE) if k <= last)
    return (f"decimal notation of whole 0.1-logMAR chart lines: {vals}; an acuity poorer than the last line "
            f"({DEC_LINE[last]:g} = 20/400) is recorded as {OFF_CHART} (counting fingers), which StandardEyes sets to "
            "missing and counts in the run log")


# the README note on the preoperative UDVA model of every dataset
UDVA_OFF_NOTE = (f"never better than CDVA; poorer than logMAR {CHART_LAST_LINE / 10:.1f} (20/400, the chart's last line) "
                 f"recorded as {OFF_CHART}")


def letters_or_off(logmar, last=CHART_LAST_LINE):
    """ETDRS letters as recorded: OFF_CHART ("CF") below 85 - 5 x last letters
    (20 letters = logMAR 1.3); an object array."""
    let = letters_of(logmar)
    out = np.empty(let.shape, dtype=object)
    on = let >= 85 - 5 * last
    out[on] = [int(v) for v in let[on]]
    out[~on] = OFF_CHART
    return out


def snellen_of(logmar):
    """A classic Snellen chart reading as typed in a clinic record: the
    smallest chart line (20/x of SNELLEN_CHART) read with at most 2 letters
    missed, i.e. the line of logMAR v nearest below the acuity x with
    v >= x - 0.04; then "-k" for the k whole letters missed on it (k =
    trunc((x - v)/0.02)), or, when the acuity is better than that line,
    "+k" for the letters read on the next line (k = trunc((v - x)/0.02), at
    most 2); "CF" (counting fingers) beyond SNELLEN_CF_ABOVE logMAR."""
    lm = np.asarray(logmar, float)
    den = np.array(SNELLEN_CHART, float)
    v = np.log10(den / 20.0)                     # ascending
    out = np.empty(lm.shape, dtype=object)
    for idx, x in np.ndenumerate(lm):
        if not np.isfinite(x):
            out[idx] = None
            continue
        if x > SNELLEN_CF_ABOVE:
            out[idx] = "CF"
            continue
        cand = np.flatnonzero(v >= x - 0.04 - 1e-9)
        j = int(cand[0]) if len(cand) else len(v) - 1        # just above the CF cut: the 20/400 line
        d = x - v[j]
        k = int(min(2, np.trunc(abs(d) / 0.02 + 1e-9)))
        lab = f"20/{int(den[j])}" if float(den[j]).is_integer() else f"20/{den[j]:g}"
        out[idx] = lab + ("" if k == 0 else f"-{k}" if d > 0 else f"+{k}")
    return out


def snellen_logmar(txt):
    """logMAR of a Snellen reading as the app parses it (log10(b/a) + 0.02 k
    for "-k", - 0.02 k for "+k"); NaN for CF or blank."""
    out = []
    for s in np.asarray(txt, dtype=object).ravel():
        if not isinstance(s, str) or "/" not in s:
            out.append(np.nan)
            continue
        a, rest = s.split("/", 1)
        k = 0
        for sg in "+-":
            if sg in rest:
                rest, kk = rest.split(sg)
                k = int(kk) * (1 if sg == "-" else -1)
        out.append(np.log10(float(rest) / float(a)) + 0.02 * k)
    return np.array(out).reshape(np.shape(txt))


# ---------------------------------------------------------------------------
# binary outcomes with an exact population-averaged logistic model
# ---------------------------------------------------------------------------
def binary_copula(rng, n, p, rho):
    """Yes/No of both eyes with marginal probability p (n, 2) through a Gaussian
    copula with fellow-eye latent correlation rho: the logistic model that gives
    p is then the exact population-averaged (marginal) model."""
    z = correlated_normal(rng, n, rho)
    return norm_cdf(z) < p


# ---------------------------------------------------------------------------
# models for the README tables (sample estimates, crude vs adjusted)
# ---------------------------------------------------------------------------
def ols(y, X):
    b, *_ = np.linalg.lstsq(X, y, rcond=None)
    return b


def logit_fit(y, X, it=40):
    b = np.zeros(X.shape[1])
    for _ in range(it):
        p = expit(X @ b)
        w = p * (1 - p)
        b = b + np.linalg.solve(X.T @ (X * w[:, None]), X.T @ (y - p))
    return b


def cluster_se(X, r, w, cl):
    """Robust (sandwich) standard errors clustered by patient of the estimating
    equation sum X'r = 0 with working weights w (OLS: w = 1; logistic: p(1-p))."""
    bread = np.linalg.inv(X.T @ (X * w[:, None]))
    _, idx = np.unique(cl, return_inverse=True)
    S = np.zeros((idx.max() + 1, X.shape[1]))
    np.add.at(S, idx, X * r[:, None])
    return np.sqrt(np.diag(bread @ (S.T @ S) @ bread))


def contrast(y, group_dummies, cov, pid, logistic=False):
    """Estimate, 95% CI of each group dummy, crude and adjusted for cov
    (robust SE clustered by patient). Returns ([(est, lo, hi)] crude,
    [(est, lo, hi)] adjusted, eyes, patients); odds ratios when logistic."""
    G = np.column_stack(group_dummies)
    ok = np.isfinite(y) & np.isfinite(G).all(1) & (np.isfinite(cov).all(1) if cov is not None else True)
    one = np.ones(int(ok.sum()))
    fits = []
    for X in (np.column_stack([one, G[ok]]),
              np.column_stack([one, G[ok], cov[ok]]) if cov is not None else None):
        if X is None:
            fits.append(None)
            continue
        if logistic:
            b = logit_fit(y[ok], X)
            p = expit(X @ b)
            se = cluster_se(X, y[ok] - p, p * (1 - p), pid[ok])
        else:
            b = ols(y[ok], X)
            se = cluster_se(X, y[ok] - X @ b, np.ones(len(one)), pid[ok])
        ci = [(b[j], b[j] - 1.959964 * se[j], b[j] + 1.959964 * se[j]) for j in range(1, G.shape[1] + 1)]
        fits.append([tuple(np.exp(c)) for c in ci] if logistic else ci)
    return fits[0], fits[1], int(ok.sum()), len(set(pid[ok]))


def fellow_corr(df, col, value=None):
    """Pearson correlation between the right and the left eye of the patients
    with both eyes (first surgeries) of a numeric column (or of value(df))."""
    d = df.copy()
    d["_v"] = pd.to_numeric(d[col], errors="coerce") if value is None else value(d)
    w = d.pivot_table(index="PatientID", columns="Eye", values="_v", aggfunc="first")
    if not {"OD", "OS"} <= set(w.columns):
        return np.nan, 0
    w = w[["OD", "OS"]].dropna()
    if len(w) < 3:
        return np.nan, len(w)
    return float(np.corrcoef(w.OD, w.OS)[0, 1]), len(w)


# ---------------------------------------------------------------------------
# workbook, README, settings
# ---------------------------------------------------------------------------
class Readme:
    """Rows of the README sheet (four columns), in the style of
    Example_Refractive_Dataset.xlsx."""

    def __init__(self):
        self.rows = []

    def add(self, *x):
        self.rows.append([("" if v is None else v) for v in x] + [""] * (4 - len(x)))

    def blank(self):
        self.add("")


def data_sha(df):
    return hashlib.sha256(df.to_csv(index=False, lineterminator="\n").encode("utf-8")).hexdigest()


def software_line():
    return f"Python {platform.python_version()}, numpy {np.__version__}, pandas {pd.__version__}"


def source_rows(R, generator, seed, df, sha, extra_seed=None):
    R.add("SOURCE")
    R.add("Generator", f"{generator} (shipped in the validation folder of StandardEyes)",
          "shared building blocks in simcore.py beside it")
    R.add("Seed", str(seed), "numpy.random.default_rng" + (f"; {extra_seed}" if extra_seed else ""))
    R.add("Software", software_line(), "numpy does not guarantee identical random streams across versions")
    R.add("SHA-256 of the data", sha, "of the 'Surgeries' table written as CSV (UTF-8, LF, no index) before it "
          "is saved to Excel; rerun the generator to check it (a table read back from the .xlsx gives another "
          "hash: reading changes some column types)")


def write_workbook(df, readme, path, creator):
    from openpyxl.styles import Alignment, Font
    with pd.ExcelWriter(path, engine="openpyxl") as xw:
        df.to_excel(xw, sheet_name="Surgeries", index=False)
        pd.DataFrame(readme.rows if isinstance(readme, Readme) else readme).to_excel(
            xw, sheet_name="README", index=False, header=False)
        wb = xw.book
        wb.properties.creator = creator
        wb.properties.lastModifiedBy = creator
        ws = wb["README"]
        for col, w in zip("ABCD", (46, 64, 52, 70)):
            ws.column_dimensions[col].width = w
        ws["A1"].font = Font(bold=True, size=14)
        for row in ws.iter_rows():
            v = row[0].value
            if isinstance(v, str) and len(v) > 3 and v.split(" (")[0].isupper():
                for c in row:
                    c.font = Font(bold=True)
            for c in row:
                c.alignment = Alignment(wrap_text=True, vertical="top")
        wb["Surgeries"].freeze_panes = "C2"


def typed(df, ints=(), floats=()):
    """Integer columns with blanks as nullable Int64, numeric columns as float."""
    for c in ints:
        if c in df:
            df[c] = pd.to_numeric(df[c]).round().astype("Int64")
    for c in floats:
        if c in df:
            df[c] = pd.to_numeric(df[c]).astype(float)
    return df


def blank_out(rng, df, rates):
    """Missing values completely at random: column -> share of rows."""
    for col, frac in rates.items():
        hit = rng.random(len(df)) < frac
        df[col] = df[col].astype(object).where(~hit, np.nan)
    return df


def settings(data_file, **over):
    """A loadable settings file (the layout of the 1.1.0 example settings).
    Paths are relative to the app folder: the dataset ships in "Example Data"."""
    base = {
        "file_path": f"Example Data/{data_file}", "sheet_name": 1, "guess_max": 120000,
        "patient_id_raw": "PatientID", "eye_id_raw": "EyeID", "group_source_var": "",
        "patient_level_params_raw": ["Age_Years", "Sex"],
        "exclude_params_raw": [],
        "age_var": "Age_Years", "fu_var": "FollowUp_Days", "sex_var": "Sex",
        "baseline": {"continuous": [], "ordinal": [], "dichotomous": [], "categorical": []},
        "preop": {"continuous": [], "ordinal": [], "dichotomous": [], "categorical": []},
        "outcome": {"continuous": [], "ordinal": [], "dichotomous": [], "categorical": []},
        "within_group": {"continuous": [], "ordinal": [], "dichotomous": [], "categorical": []},
        "within_group_levels": {},
        "filter_rules": [{"var": "FollowUp_Days", "op": "between", "value": [150, 210]}],
        "groups": {}, "reference_group": "", "group_colors": {}, "group_labels": {},
        "do_matching": False, "psm_covariates_raw": [],
        "psm_method": "nearest", "psm_distance": "linear.logit", "psm_ratio": 1,
        "psm_replace": False, "psm_caliper": 0.2, "psm_std_caliper": True, "psm_m_order": "largest",
        "run_gee_stage": "none", "run_main_gee": False, "run_adjusted_gee": False,
        "gee_adjust_covariates_raw": [], "use_matching_weights_in_gee": True,
        "add_gee_columns": True, "gee_source_for_tables": "main",
        "do_regression": False, "regression_stage": "unmatched", "regression_covariates_raw": [],
        "subset_col_raw": "", "subsets": {}, "subset_mode": "separate",
        "do_standardize_minus_cyl": True,
        "std_triples": [
            {"sph": "Preop_Sphere_D", "cyl": "Preop_Cylinder_D", "axis": "Preop_Axis_Deg"},
            {"sph": "Postop_Sphere_D", "cyl": "Postop_Cylinder_D", "axis": "Postop_Axis_Deg"}],
        "std_sph_raw": "Preop_Sphere_D", "std_cyl_raw": "Preop_Cylinder_D", "std_axis_raw": "Preop_Axis_Deg",
        "va_format": "decimal",
        "do_logmar": True,
        "logmar_cols_raw": ["Preop_UDVA", "Preop_CDVA", "Postop_UDVA", "Postop_CDVA"],
        "make_meyestro": True, "make_alpins": True, "make_pub_table": True,
        "make_graphs": True, "graph_set": "nine", "graph_formats": ["tiff"], "graph_dpi": 300,
        "astig_panels": True, "double_angle_figs": True, "semicircle_figs": True,
        "astig_region": "polygon", "astig_jcrs_standard": True, "astig_test_method": "robust",
        "cluster_by_patient": True,
        "stability_figs": False, "stability_visits": [],
        "roles": {
            "pre_ucva": "Preop_UDVA", "pre_bcva": "Preop_CDVA",
            "post_ucva": "Postop_UDVA", "post_bcva": "Postop_CDVA",
            "pre_sph": "Preop_Sphere_D", "pre_cyl": "Preop_Cylinder_D", "pre_axis": "Preop_Axis_Deg",
            "post_sph": "Postop_Sphere_D", "post_cyl": "Postop_Cylinder_D", "post_axis": "Postop_Axis_Deg",
            "pre_seq": "Preop_SEQ_D", "post_seq": "Postop_SEQ_D", "target_seq": "Target_SEQ_D", "eye": "Eye"},
        "dichotomous_preferred_level": {"Sex": "F"},
        "output_dir": "Example Output", "export_filtered_csv": True,
        "output_prefix": "StandardEyes",
    }
    c = json.loads(json.dumps(base))
    for k, v in over.items():
        c[k] = v
    c["num_groups"] = len(c["groups"])
    c["num_filters"] = len(c["filter_rules"])
    c["num_subsets"] = len(c["subsets"])
    return c


# Clinical labels for print (setting display_labels, build se.12): every
# variable a settings file prints in a table, a model sheet, a balance table or
# a love plot gets one (acuity: analyzed in logMAR in every shipped settings
# file, whatever the recorded notation). A label equal to its column name is
# not written.
_MU = "\u00b5"
DISPLAY_LABELS = {
    "Age_Years": "Age (y)",
    "Preop_SEQ_D": "Preoperative SEQ (D)",
    "Preop_Sphere_D": "Preoperative sphere (D)",
    "Preop_Cylinder_D": "Preoperative cylinder (D)",
    "Preop_Pachymetry_um": f"Central pachymetry ({_MU}m)",
    "Preop_Mean_K_D": "Mean keratometry (D)",
    "Optical_Zone_mm": "Optical zone (mm)",
    "Ablation_Depth_um": f"Ablation depth ({_MU}m)",
    "Ablation_or_Lenticule_Depth_um": f"Ablation or lenticule depth ({_MU}m)",
    "FollowUp_Days": "Follow-up (d)",
    "Postop_SEQ_D": "Postoperative SEQ (D)",
    "Postop_Cylinder_D": "Postoperative cylinder (D)",
    "Postop_Haze_Grade": "Haze grade (0\u20134)",
    "DryEye_Symptoms": "Dry-eye symptoms",
    "Complication_Type": "Complication",
    "Retreatment_Indicated": "Retreatment indicated",
    "Glare_Halo_Symptoms": "Glare or halo symptoms",
}
for _when, _word in (("Preop", "Preoperative"), ("Postop", "Postoperative")):
    for _va in ("UDVA", "CDVA"):
        for _sfx in ("", "_logMAR", "_Snellen", "_Letters"):
            DISPLAY_LABELS[f"{_when}_{_va}{_sfx}"] = f"{_word} {_va} (logMAR)"


def display_labels_for(cfg):
    """The display labels of the variables a settings file prints."""
    seen = []
    for sec in ("baseline", "preop", "outcome", "within_group"):
        for ty in ("continuous", "ordinal", "dichotomous", "categorical"):
            seen += list((cfg.get(sec) or {}).get(ty) or [])
    for k in ("psm_covariates_raw", "gee_adjust_covariates_raw", "regression_covariates_raw"):
        seen += list(cfg.get(k) or [])
    out = {}
    for v in seen:
        if v in DISPLAY_LABELS and DISPLAY_LABELS[v] != v and v not in out:
            out[v] = DISPLAY_LABELS[v]
    return out


def write_settings(name, cfg, title, description, demonstrates, notes=None):
    # the display labels go after the preferred levels, as in the shipped files
    c = {}
    for k, v in cfg.items():
        if k == "display_labels":
            continue
        c[k] = v
        if k == "dichotomous_preferred_level":
            c["display_labels"] = display_labels_for(cfg)
    c.setdefault("display_labels", display_labels_for(cfg))
    c["_title"] = title
    c["_description"] = description
    c["_demonstrates"] = demonstrates
    if notes:
        c["_notes"] = notes
    os.makedirs(OUT, exist_ok=True)
    path = os.path.join(OUT, name)
    with open(path, "w", encoding="utf-8") as fh:
        json.dump(c, fh, indent=2, ensure_ascii=False)
    return path


def ids(letter, pidx, eye):
    """Patient and eye codes: '<letter>0001' and '<letter>0001-OD'."""
    pid = np.array([f"{letter}{p + 1:04d}" for p in pidx], dtype=object)
    return pid, np.array([f"{a}-{e}" for a, e in zip(pid, eye)], dtype=object)
