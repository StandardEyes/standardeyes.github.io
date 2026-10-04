"""
Validation workbook for the double-angle plots and the astigmatism statistics
(dataset version 2, v1.1.0). Simulated data - no real patients.

Convention (JCRS 2025 astigmatism standard, decision D1): every vector is built in
PLUS-cylinder format - magnitude M >= 0 at the PLUS axis (steep meridian),
x = M cos 2theta, y = M sin 2theta. With-the-rule astigmatism (plus axis 90, minus
axis 180) therefore plots on the LEFT (x < 0) and against-the-rule (plus axis 180,
minus axis 90) on the RIGHT (x > 0). A refraction typed -2.00 x 180 and one typed
+2.00 x 90 are the same vector.

Deliberately built to exercise every astigmatism option in one file:
  * 5 cohorts - four of usable size (N-group superimposition past 3) plus a
    2-eye cohort that cannot support a region ("Cohort D")
  * preoperative meridians concentrated differently per cohort so the doubled-angle
    centroids genuinely separate: with-the-rule / against-the-rule / oblique / wide
  * fellow eyes (mirror-symmetric axes) in the with-the-rule, against-the-rule and
    wide cohorts, so patient-level resampling has clusters to work on
  * astigmatic correction modelled per cohort: the prediction error (PE) centroid is
    close to zero in every cohort (about 0.02 D: the scatter of the angle of error
    alone leaves 1.5% of the preoperative centroid) except Cohort B, which is
    undercorrected (CI 0.85) and rotated (angle of error +5 deg): about 0.28 D
  * mixed notation: about a quarter of the rows typed in plus cylinder, some rows
    with the preop refraction in plus and the postop in minus, targets typed in
    either form - all of them must give the same vectors
  * supplied TIA / SIA / DV / PE magnitude + axis columns computed in the plus
    convention from the typed columns, so the "use my columns" path must reproduce
    the "compute" path; a deliberately perturbed TIA set proves the disagreement
    warning fires, and Cohort C's supplied TIA is entirely missing
  * a target refraction: plano for most eyes, +0.50 x 180 (= -0.50 x 90) for 200
  * edge cases: zero-cylinder eyes (TIA = 0 with a plano target), missing preop
    axis, missing postop cylinder

Vector definitions (D2): PRE = preop, POST = postop, T = target refractive astigmatism;
  SIA = POST - PRE; PE = POST - T; Alpins TIA (intended change) = T - PRE; DV = -PE.

Column names mirror Example_Refractive_Dataset.xlsx so the example settings apply.

Usage:  python make_astig_validation_data.py                 (writes into the app)
        python make_astig_validation_data.py --out <folder>  (writes <folder>\\Example Data\\...)
"""
from __future__ import annotations
import argparse, hashlib, os, platform
import numpy as np
import pandas as pd

HERE = os.path.dirname(os.path.abspath(__file__))


def default_app():
    """The app folder: the parent when this script ships inside the app's
    validation\\ folder, otherwise the sibling "App - StandardEyes"."""
    up = os.path.dirname(HERE)
    if os.path.basename(HERE).lower() == "validation" and os.path.isdir(os.path.join(up, "engine")):
        return up
    return os.path.join(up, "App - StandardEyes")


FNAME = "Astigmatism_Validation_Dataset.xlsx"
SEED = 4242
SEED_TRUTH = 4243          # Monte Carlo of the true PE centroids
N_TRUTH = 400_000          # eyes per cohort in that Monte Carlo

# cohort -> n eyes, mean PLUS axis, doubled-angle SD (deg), CI mean, AoE mean (deg),
#           share of patients with both eyes, description
COHORTS = {
    "Reference": (320, 90, 30, 1.00, 0.0, 0.6,
                  "with-the-rule: plus axis 90 (minus axis 180) -> centroid on the LEFT (x < 0)"),
    "Cohort A":  (220, 180, 34, 1.00, 0.0, 0.6,
                  "against-the-rule: plus axis 180 (minus axis 90) -> centroid on the RIGHT (x > 0)"),
    "Cohort B":  (180, 45, 38, 0.85, 5.0, 0.0,
                  "oblique: plus axis 45 -> centroid UP (y > 0); one eye per patient (the mirror image "
                  "would point down); undercorrected and rotated -> non-zero PE centroid"),
    "Cohort C":  (160, 90, 95, 1.00, 0.0, 0.5,
                  "wide spread: large data region, centroid near the origin; supplied TIA columns all missing"),
    "Cohort D":  (2, 90, 20, 1.00, 0.0, 1.0,
                  "two eyes of one patient: too small for a region or a test"),
}
CI_SD, AOE_SD, POST_SD = 0.10, 5.0, 0.15        # per eye; POST_SD in D per component
MAG_MEAN, MAG_SD, MAG_RHO, MAG_MAX = 1.5, 0.9, 0.6, 5.0
MIRROR_SD = 6.0
SEQ_MEAN, SEQ_P_SD, SEQ_E_SD, POSTSEQ_SD = -3.6, 1.8, 0.6, 0.30
N_ZERO_CYL, N_MISS_AXIS, N_MISS_POSTCYL, N_TARGET = 30, 12, 12, 200
TARGET_PLUS = (0.50, 180)                        # = -0.50 x 90 in minus form
P_PLUS_ROWS, P_MIXED_ROWS = 0.25, 0.04
TIA_BAD = (0.37, 11)                             # added magnitude (D) and axis (deg)

DEC_LINE = {-2: 1.6, -1: 1.25, 0: 1.0, 1: 0.8, 2: 0.63, 3: 0.5, 4: 0.4, 5: 0.32,
            6: 0.25, 7: 0.2, 8: 0.16, 9: 0.125, 10: 0.1, 11: 0.08, 12: 0.063, 13: 0.05,
            14: 0.04, 15: 0.032, 16: 0.025, 17: 0.02}


def q25(x):
    return np.round(np.asarray(x, float) * 4) / 4 + 0.0


def wrap_axis(a):
    """Any angle in degrees -> an integer axis 1..180 (0 is written as 180)."""
    a = np.round(np.asarray(a, float)) % 180
    return np.where(a == 0, 180, a)


def dec_of(line):
    return np.vectorize(DEC_LINE.get)(np.clip(np.asarray(line, int), -2, 17)).astype(float)


def dbl(mag, plus_axis):
    t = np.radians(2 * np.asarray(plus_axis, float))
    return mag * np.cos(t), mag * np.sin(t)


def plus_of(cyl, axis):
    """Typed cylinder + axis (either notation) -> magnitude and PLUS axis (0..180).
    A zero cylinder with a blank axis is the null vector at axis 0 (the engine's
    convention); a non-zero cylinder without an axis stays missing."""
    cyl = np.asarray(cyl, float); axis = np.asarray(axis, float)
    axis = np.where((cyl == 0) & ~np.isfinite(axis), 0.0, axis)
    return np.abs(cyl), np.where(cyl < 0, axis + 90, axis) % 180


def ax_of(x, y, fallback):
    """Plus axis of a double-angle vector; a null vector keeps the eye's own axis."""
    a = (np.degrees(np.arctan2(y, x)) / 2) % 180
    bad = ~np.isfinite(np.hypot(x, y)) | (np.hypot(x, y) < 1e-12)
    return np.where(bad, fallback, a)


def correct(rng, px, py, tx, ty, ci_mean, aoe_mean, n):
    """POST = PRE + CI R(2 AoE) (T - PRE) + noise: the astigmatic correction model."""
    ci = np.maximum(ci_mean + rng.normal(0, CI_SD, n), 0.3)
    a = np.radians(aoe_mean + rng.normal(0, AOE_SD, n))
    c2, s2 = np.cos(2 * a), np.sin(2 * a)
    ix, iy = tx - px, ty - py                                    # intended change
    sx, sy = ci * (c2 * ix - s2 * iy), ci * (s2 * ix + c2 * iy)  # achieved change (SIA)
    return px + sx + rng.normal(0, POST_SD, n), py + sy + rng.normal(0, POST_SD, n)


def build(rng):
    rows = []
    pid = 0
    for cohort, (n, mu, sd_d, ci_m, aoe_m, p_bil, _) in COHORTS.items():
        got = 0
        while got < n:
            pid += 1
            both = (rng.random() < p_bil) and (n - got >= 2)
            eyes = ["OD", "OS"] if both else [("OD" if rng.random() < 0.5 else "OS")]
            seq_p = rng.normal(SEQ_MEAN, SEQ_P_SD)
            v_mag = rng.normal()
            ax_od = (rng.normal(2 * mu, sd_d) % 360) / 2         # drawn on the DOUBLED angle
            for eye in eyes:
                ax = ax_od if eye == "OD" or not both else 180 - ax_od + rng.normal(0, MIRROR_SD)
                if not both:
                    ax = ax_od
                mag = MAG_MEAN + MAG_SD * (np.sqrt(MAG_RHO) * v_mag + np.sqrt(1 - MAG_RHO) * rng.normal())
                rows.append(dict(pid=pid, Eye=eye, Cohort=cohort, pre_mag=float(np.clip(q25(abs(mag)), 0, MAG_MAX)),
                                 pre_pax=float(wrap_axis(ax)),
                                 seq_lat=float(np.clip(seq_p + rng.normal(0, SEQ_E_SD), -10, -0.5))))
            got += len(eyes)
    df = pd.DataFrame(rows)
    n = len(df)

    # ---- edge cases and the target (disjoint sets of eyes; Cohort D untouched) ----
    idx = np.flatnonzero((df.Cohort != "Cohort D").to_numpy())
    idx = rng.permutation(idx)
    zero_cyl = idx[:N_ZERO_CYL]
    miss_axis = idx[N_ZERO_CYL:N_ZERO_CYL + N_MISS_AXIS]
    k = N_ZERO_CYL + N_MISS_AXIS
    miss_post = idx[k:k + N_MISS_POSTCYL]
    k += N_MISS_POSTCYL
    tgt = idx[k:k + N_TARGET]
    df.loc[zero_cyl, "pre_mag"] = 0.0
    tmag = np.zeros(n); tpax = np.full(n, 180.0)
    tmag[tgt] = TARGET_PLUS[0]; tpax[tgt] = TARGET_PLUS[1]

    # ---- the correction model, per cohort ----
    px, py = dbl(df.pre_mag.to_numpy(), df.pre_pax.to_numpy())
    tx, ty = dbl(tmag, tpax)
    qx, qy = np.empty(n), np.empty(n)
    for cohort, (_, _, _, ci_m, aoe_m, _, _) in COHORTS.items():
        m = (df.Cohort == cohort).to_numpy()
        qx[m], qy[m] = correct(rng, px[m], py[m], tx[m], ty[m], ci_m, aoe_m, int(m.sum()))
    qmag = q25(np.hypot(qx, qy))
    qpax = wrap_axis(np.degrees(np.arctan2(qy, qx)) / 2)

    # ---- typed refractions (minus form first) ----
    pre_sph = q25(df.seq_lat + df.pre_mag / 2)
    post_seq_lat = rng.normal(0, POSTSEQ_SD, n)
    post_sph = q25(post_seq_lat + qmag / 2)
    out = pd.DataFrame({
        "PatientID": [f"VP{p:04d}" for p in df.pid], "EyeID": [f"VP{p:04d}-{e}" for p, e in zip(df.pid, df.Eye)],
        "Eye": df.Eye, "SurgeryNumber": 1, "Cohort": df.Cohort,
    })
    pat = out.PatientID.to_numpy()
    upat, inv = np.unique(pat, return_inverse=True)
    np_ = len(upat)
    out["Age_Years"] = np.clip(np.round(rng.normal(35, 9, np_)), 18, 62)[inv].astype(int)
    out["Sex"] = np.where(rng.random(np_) < 0.54, "F", "M")[inv]
    out["Smoker"] = np.where(rng.random(np_) < 0.15, "Yes", "No")[inv]
    out["Surgeon"] = rng.choice(["Surgeon A", "Surgeon B", "Surgeon C"], size=np_)[inv]
    out["Procedure"] = rng.choice(["PRK", "LASIK", "KLEx"], size=np_, p=[0.40, 0.42, 0.18])[inv]
    out["Preop_Sphere_D"] = pre_sph
    out["Preop_Cylinder_D"] = -df.pre_mag.to_numpy() + 0.0
    out["Preop_Axis_Deg"] = wrap_axis(df.pre_pax + 90)
    out["Preop_SEQ_D"] = pre_sph - df.pre_mag.to_numpy() / 2
    cd_pre = np.clip(np.round(rng.normal(-0.4, 0.5, n)), -2, 3).astype(int)
    out["Preop_UDVA"] = dec_of(cd_pre + np.round(2.7 * np.abs(out.Preop_SEQ_D) + rng.normal(0, 0.7, n)))
    out["Preop_CDVA"] = dec_of(cd_pre)
    out["Preop_Pachymetry_um"] = np.round(np.clip(rng.normal(545, 27, n), 480, 620)).astype(int)
    out["Preop_Mean_K_D"] = np.round(np.clip(rng.normal(43.6, 1.4, n), 39.5, 47.5), 2)
    out["Preop_DryEye_Grade"] = rng.choice(4, size=n, p=[0.55, 0.30, 0.12, 0.03])
    out["Optical_Zone_mm"] = np.round(np.clip(rng.normal(6.5, 0.15, n), 6.0, 7.0), 1)
    depth = out.Optical_Zone_mm ** 2 / 3 * (np.abs(out.Preop_SEQ_D) + df.pre_mag / 2) + rng.normal(0, 3, n)
    out["Ablation_or_Lenticule_Depth_um"] = np.round(depth + np.where(out.Procedure == "KLEx", 15, 0)).astype(int)
    out["Target_SEQ_D"] = 0.0
    out["FollowUp_Days"] = (180 + rng.integers(-25, 26, n)).astype(int)
    out["Postop_Sphere_D"] = post_sph
    out["Postop_Cylinder_D"] = -qmag + 0.0
    out["Postop_Axis_Deg"] = wrap_axis(qpax + 90)
    out["Postop_SEQ_D"] = post_sph - qmag / 2
    cd_post = np.clip(cd_pre - rng.choice([-2, -1, 0, 1, 2], size=n, p=[0.01, 0.06, 0.62, 0.27, 0.04]), -2, 3)
    blur = 0.3 * np.maximum(np.abs(out.Postop_SEQ_D) - 0.2, 0) + 0.15 * np.maximum(qmag - 0.25, 0)
    out["Postop_UDVA"] = dec_of(np.maximum(cd_post, np.round(cd_post + 10 * blur + rng.normal(-0.2, 0.4, n))))
    out["Postop_CDVA"] = dec_of(cd_post)
    out["Postop_Haze_Grade"] = np.where(out.Procedure == "PRK", rng.choice(3, size=n, p=[0.75, 0.2, 0.05]), 0)
    out["Patient_Satisfaction_1to5"] = rng.choice([2, 3, 4, 5], size=n, p=[0.05, 0.15, 0.35, 0.45])
    out["DryEye_Symptoms"] = np.where(rng.random(n) < 0.18, "Yes", "No")
    out["Retreatment_Required"] = np.where(rng.random(n) < 0.05, "Yes", "No")
    comp = np.where(rng.random(n) < 0.06, np.where(out.Procedure == "PRK", "Corneal haze", "Epithelial defect"),
                    "No complication")
    out["Complication_Type"] = comp

    # ---- target typed half in plus form (+0.50 x 180), half in minus (-0.50 x 90) ----
    out["Target_Cylinder_D"] = 0.0
    out["Target_Axis_Deg"] = 180
    tnot = np.full(n, "plano", dtype=object)
    tplus = tgt[: N_TARGET // 2]; tminus = tgt[N_TARGET // 2:]
    out.loc[tplus, "Target_Cylinder_D"] = TARGET_PLUS[0]; out.loc[tplus, "Target_Axis_Deg"] = TARGET_PLUS[1]
    out.loc[tminus, "Target_Cylinder_D"] = -TARGET_PLUS[0]; out.loc[tminus, "Target_Axis_Deg"] = 90
    tnot[tplus] = "plus"; tnot[tminus] = "minus"

    # ---- row notation: a quarter all-plus, a few mixed (preop plus, postop minus) ----
    perm = rng.permutation(n)
    n_plus, n_mixed = int(round(P_PLUS_ROWS * n)), int(round(P_MIXED_ROWS * n))
    plus_rows, mixed_rows = perm[:n_plus], perm[n_plus:n_plus + n_mixed]
    note = np.full(n, "minus", dtype=object)
    note[plus_rows] = "plus"; note[mixed_rows] = "mixed (preop plus, postop minus)"

    def to_plus(rws, pre):
        s, c, a = f"{pre}_Sphere_D", f"{pre}_Cylinder_D", f"{pre}_Axis_Deg"
        sv, cv, av = (out.loc[rws, x].to_numpy(float) for x in (s, c, a))
        out.loc[rws, s] = sv + cv
        out.loc[rws, c] = -cv + 0.0
        out.loc[rws, a] = wrap_axis(av + 90)
    to_plus(plus_rows, "Preop"); to_plus(plus_rows, "Postop"); to_plus(mixed_rows, "Preop")
    out["Cylinder_Notation"] = note
    out["Target_Notation"] = tnot

    # ---- missing values of the edge cases (after the typing, so they stay missing) ----
    out["Preop_Axis_Deg"] = out.Preop_Axis_Deg.astype(float)
    out.loc[miss_axis, "Preop_Axis_Deg"] = np.nan
    out["Postop_Cylinder_D"] = out.Postop_Cylinder_D.astype(float)
    out.loc[miss_post, "Postop_Cylinder_D"] = np.nan

    # ---- supplied vectors, from the TYPED columns, in the plus convention ----
    pm, pa = plus_of(out.Preop_Cylinder_D, out.Preop_Axis_Deg)
    qm, qa = plus_of(out.Postop_Cylinder_D, out.Postop_Axis_Deg)
    tm, ta = plus_of(out.Target_Cylinder_D, out.Target_Axis_Deg)
    Px, Py = dbl(pm, pa); Qx, Qy = dbl(qm, qa); Tx, Ty = dbl(tm, ta)
    plano = tm == 0
    vecs = {}
    # TIA = T - PRE (with a plano target: magnitude = |PRE|, axis = PRE axis + 90, exactly)
    ix, iy = Tx - Px, Ty - Py
    vecs["TIA"] = (np.where(plano, pm, np.hypot(ix, iy)),
                   np.where(plano, (pa + 90) % 180, ax_of(ix, iy, (pa + 90) % 180)))
    sx, sy = Qx - Px, Qy - Py                                     # SIA = POST - PRE
    vecs["SIA"] = (np.hypot(sx, sy), ax_of(sx, sy, qa))
    dx, dy = Tx - Qx, Ty - Qy                                     # DV = -PE = T - POST
    vecs["DV"] = (np.where(plano, qm, np.hypot(dx, dy)),
                  np.where(plano, (qa + 90) % 180, ax_of(dx, dy, (qa + 90) % 180)))
    ex, ey = Qx - Tx, Qy - Ty                                     # PE = POST - T
    vecs["PE"] = (np.where(plano, qm, np.hypot(ex, ey)), np.where(plano, qa, ax_of(ex, ey, qa)))
    for v, (m, a) in vecs.items():
        bad = ~np.isfinite(m) | ~np.isfinite(a)                   # no axis -> no vector
        out[f"{v}_Mag_D"] = np.where(bad, np.nan, m)
        out[f"{v}_Axis_Deg"] = np.where(bad, np.nan, a)
    out.loc[out.Cohort == "Cohort C", ["TIA_Mag_D", "TIA_Axis_Deg"]] = np.nan
    out["TIAbad_Mag_D"] = out.TIA_Mag_D + TIA_BAD[0]
    out["TIAbad_Axis_Deg"] = (out.TIA_Axis_Deg + TIA_BAD[1]) % 180

    out["Preop_Axis_Deg"] = out.Preop_Axis_Deg.round().astype("Int64")
    out["Postop_Axis_Deg"] = out.Postop_Axis_Deg.round().astype("Int64")
    out["Target_Axis_Deg"] = out.Target_Axis_Deg.round().astype("Int64")
    edges = dict(zero_cyl=zero_cyl, miss_axis=miss_axis, miss_post=miss_post, tgt=tgt,
                 plus_rows=plus_rows, mixed_rows=mixed_rows)
    return out, edges


def expected(df):
    """Per cohort and vector: what a correct plus-format engine must report."""
    pm, pa = plus_of(df.Preop_Cylinder_D, df.Preop_Axis_Deg.astype(float))
    qm, qa = plus_of(df.Postop_Cylinder_D, df.Postop_Axis_Deg.astype(float))
    tm, ta = plus_of(df.Target_Cylinder_D, df.Target_Axis_Deg.astype(float))
    P = dbl(pm, pa); Q = dbl(qm, qa); T = dbl(tm, ta)
    V = {"PRE": P, "POST": Q, "SIA": (Q[0] - P[0], Q[1] - P[1]), "PE": (Q[0] - T[0], Q[1] - T[1]),
         "TIA (Alpins, T - PRE)": (T[0] - P[0], T[1] - P[1])}
    rows = []
    for vn, (x, y) in V.items():
        for c in COHORTS:
            m = (df.Cohort == c).to_numpy() & np.isfinite(x) & np.isfinite(y)
            if not m.any():
                continue
            xs, ys = x[m], y[m]
            cx, cy = xs.mean(), ys.mean()
            mag = np.hypot(xs, ys)
            rows.append(dict(vector=vn, cohort=c, n_eyes=int(m.sum()),
                             n_patients=int(df.loc[m, "PatientID"].nunique()),
                             centroid_x=cx, centroid_y=cy, centroid_mag=float(np.hypot(cx, cy)),
                             centroid_plus_axis=float((np.degrees(np.arctan2(cy, cx)) / 2) % 180),
                             total_sd_N=float(np.sqrt(((xs - cx) ** 2 + (ys - cy) ** 2).mean())),
                             mean_magnitude=float(mag.mean()), sd_magnitude=float(mag.std(ddof=1)) if m.sum() > 1 else np.nan,
                             median_magnitude=float(np.median(mag)), min_magnitude=float(mag.min()),
                             max_magnitude=float(mag.max())))
    return pd.DataFrame(rows)


def true_pe_centroids():
    """Monte Carlo (fixed seed) of each cohort's expected PE centroid, target plano."""
    rng = np.random.default_rng(SEED_TRUTH)
    out = {}
    for c, (_, mu, sd_d, ci_m, aoe_m, _, _) in COHORTS.items():
        n = N_TRUTH
        ax = (rng.normal(2 * mu, sd_d, n) % 360) / 2
        mag = np.clip(q25(np.abs(rng.normal(MAG_MEAN, MAG_SD, n))), 0, MAG_MAX)
        px, py = dbl(mag, ax)
        qx, qy = correct(rng, px, py, 0.0, 0.0, ci_m, aoe_m, n)
        out[c] = (qx.mean(), qy.mean())
    return out


COLUMNS = [
    ("PatientID / EyeID / Eye", "-", "Codes only (no names). Fellow eyes share the PatientID."),
    ("SurgeryNumber", "-", "Always 1."),
    ("Cohort", "-", "Reference, Cohort A, Cohort B, Cohort C, Cohort D (see the cohort table)."),
    ("Age_Years ... Complication_Type", "as in the example dataset", "Filler columns so the example settings apply; "
     "drawn independently, with no designed effect. Acuity in decimal notation of chart lines."),
    ("Preop_ / Postop_ Sphere_D, Cylinder_D, Axis_Deg", "D, D, degrees", "Typed refraction (0.25 D steps, axis 1-180), "
     "in the notation given by Cylinder_Notation."),
    ("Preop_SEQ_D / Postop_SEQ_D", "D", "sphere + cylinder/2 exactly."),
    ("Target_SEQ_D", "D", "0 (plano) for every eye."),
    ("Target_Cylinder_D / Target_Axis_Deg", "D / degrees", "Target astigmatism: 0 for most eyes; +0.50 x 180 or the "
     "same target typed -0.50 x 90 for 200 eyes (Target_Notation)."),
    ("Cylinder_Notation", "-", "minus, plus, or 'mixed (preop plus, postop minus)'."),
    ("Target_Notation", "-", "plano, plus or minus."),
    ("TIA_Mag_D / TIA_Axis_Deg", "D / degrees", "Alpins intended change T - PRE: magnitude and plus axis (0-180) of the "
     "plus-format vector. Missing for Cohort C and where the preop axis is missing."),
    ("SIA_Mag_D / SIA_Axis_Deg", "D / degrees", "Surgically induced astigmatism POST - PRE."),
    ("DV_Mag_D / DV_Axis_Deg", "D / degrees", "Alpins difference vector T - POST (= -PE)."),
    ("PE_Mag_D / PE_Axis_Deg", "D / degrees", "Prediction error POST - T (equals POST when the target is plano)."),
    ("TIAbad_Mag_D / TIAbad_Axis_Deg", "D / degrees", f"TIA + {TIA_BAD[0]} D and + {TIA_BAD[1]} deg: deliberately WRONG, "
     "to prove the disagreement warning fires."),
]


def readme_rows(df, edges, exp, truth, sha):
    R = []
    add = lambda *x: R.append(list(x) + [""] * (5 - len(x)))
    add("Simulated data - no real patients")
    add("Validation workbook for the double-angle plots and the astigmatism statistics. Every value was generated by a "
        "computer model; patients and eyes carry codes only.")
    add("")
    add("SOURCE")
    add("Generator", "make_astig_validation_data.py (shipped in the validation folder of the software)")
    add("Seed", str(SEED), f"numpy.random.default_rng; true PE centroids: seed {SEED_TRUTH}, {N_TRUTH:,} eyes per cohort")
    add("Software", f"Python {platform.python_version()}, numpy {np.__version__}, pandas {pd.__version__}")
    add("SHA-256 of the data", sha, "of the 'Surgeries' table written as CSV (UTF-8, LF, no index)")
    add("Size", f"{df.PatientID.nunique()} patients, {len(df)} eyes")
    add("")
    add("CONVENTION (JCRS 2025 astigmatism standard)")
    add("Vector space", "plus-cylinder format: magnitude M >= 0 at the plus axis (steep meridian); x = M cos 2θ, y = M sin 2θ")
    add("Orientation", "with-the-rule (plus axis 90 = minus axis 180) plots LEFT (x < 0); against-the-rule (plus axis 180 "
        "= minus axis 90) plots RIGHT (x > 0); oblique plus axis 45 plots UP")
    add("Notation", "a refraction typed -2.00 x 180 and one typed +2.00 x 90 are the same vector; every typed form below "
        "must give the same result")
    add("Vectors", "PRE = preop, POST = postop, T = target; SIA = POST - PRE; PE = POST - T; Alpins TIA = T - PRE; DV = -PE")
    add("")
    add("COHORTS", "eyes / patients", "preop plus axis (doubled-angle SD)", "correction model", "description")
    for c, (n, mu, sd_d, ci_m, aoe_m, p_bil, desc) in COHORTS.items():
        m = df.Cohort == c
        add(c, f"{int(m.sum())} / {df.loc[m, 'PatientID'].nunique()}", f"{mu} ({sd_d} deg)",
            f"CI {ci_m} (SD {CI_SD}), angle of error {aoe_m:+.0f} deg (SD {AOE_SD:.0f})", desc)
    add("All cohorts", "", f"magnitude |N({MAG_MEAN}, {MAG_SD})| on 0.25 D, <= {MAG_MAX}; fellow-eye correlation {MAG_RHO}; "
        f"OS axis = 180 - OD + N(0, {MIRROR_SD:.0f})",
        f"POST = PRE + CI R(2 AoE)(T - PRE) + N(0, {POST_SD}) per component; magnitude on 0.25 D", "")
    add("")
    add("TRUE PREDICTION-ERROR CENTROID (target plano)", "x", "y", "M @ plus axis", "")
    for c, (x, y) in truth.items():
        add(c, f"{x:+.3f}", f"{y:+.3f}", f"{np.hypot(x, y):.3f} D @ {(np.degrees(np.arctan2(y, x)) / 2) % 180:.0f}",
            "non-zero by design: undercorrected and rotated" if c == "Cohort B" else
            "close to zero: CI 1.00, the angle-of-error scatter alone (E cos 2AoE = 0.985) leaves 1.5% of the PRE centroid")
    add("")
    add("EDGE CASES", "eyes", "what must happen")
    add("Zero preop cylinder", len(edges["zero_cyl"]), "TIA = 0 with a plano target: angle of error, correction index and "
        "index of success are undefined (missing)")
    add("Missing preop axis", len(edges["miss_axis"]), "PRE, SIA and TIA are missing for these eyes")
    add("Missing postop cylinder", len(edges["miss_post"]), "POST, SIA, PE and DV are missing for these eyes")
    add("Non-zero target", len(edges["tgt"]), "half typed +0.50 x 180, half -0.50 x 90: identical vectors; TIA and PE "
        "differ from a target-of-zero run, SIA does not")
    add("Rows typed in plus", len(edges["plus_rows"]), "preop and postop in plus cylinder")
    add("Mixed rows", len(edges["mixed_rows"]), "preop in plus, postop in minus cylinder")
    add("Supplied TIA of Cohort C", int((df.Cohort == "Cohort C").sum()), "all missing: the computed vectors must be used")
    add("TIAbad columns", len(df), f"TIA + {TIA_BAD[0]} D, axis + {TIA_BAD[1]} deg: mapping them must raise the "
        "disagreement warning")
    add("")
    add("EXPECTED RESULTS", "see the sheet 'Expected': per vector and cohort, n eyes and patients, centroid (x, y, "
        "M @ plus axis), total SD with the N denominator, magnitude mean, SD, median and range - computed with numpy "
        "from the typed columns in the plus convention")
    add("Preop centroids", "; ".join(f"{r.cohort}: {r.centroid_mag:.3f} D @ {r.centroid_plus_axis:.0f} (x {r.centroid_x:+.3f})"
                                     for r in exp[exp.vector == "PRE"].itertuples()))
    add("")
    add("COLUMN DICTIONARY", "unit", "description")
    for name, unit, desc in COLUMNS:
        add(name, unit, desc)
    return R


def write_workbook(df, readme, exp, path):
    from openpyxl.styles import Alignment, Font
    with pd.ExcelWriter(path, engine="openpyxl") as xw:
        df.to_excel(xw, sheet_name="Surgeries", index=False)
        pd.DataFrame(readme).to_excel(xw, sheet_name="README", index=False, header=False)
        exp.to_excel(xw, sheet_name="Expected", index=False)
        wb = xw.book
        wb.properties.creator = "make_astig_validation_data.py"
        wb.properties.lastModifiedBy = "make_astig_validation_data.py"
        ws = wb["README"]
        for col, w in zip("ABCDE", (40, 34, 34, 44, 70)):
            ws.column_dimensions[col].width = w
        ws["A1"].font = Font(bold=True, size=14)
        for row in ws.iter_rows():
            v = row[0].value
            if isinstance(v, str) and (v.isupper() or v.startswith(("CONVENTION", "TRUE PREDICTION"))):
                for c in row:
                    c.font = Font(bold=True)
            for c in row:
                c.alignment = Alignment(wrap_text=True, vertical="top")
        wb["Surgeries"].freeze_panes = "C2"


def main():
    ap = argparse.ArgumentParser(description="Astigmatism validation workbook (simulated data).")
    ap.add_argument("--out", default=default_app(),
                    help="folder that receives 'Example Data\\" + FNAME + "' (default: the app folder)")
    a = ap.parse_args()
    ddir = os.path.join(a.out, "Example Data")
    os.makedirs(ddir, exist_ok=True)
    df, edges = build(np.random.default_rng(SEED))
    sha = hashlib.sha256(df.to_csv(index=False, lineterminator="\n").encode("utf-8")).hexdigest()
    exp = expected(df)
    truth = true_pe_centroids()
    p = os.path.join(ddir, FNAME)
    write_workbook(df, readme_rows(df, edges, exp, truth, sha), exp, p)
    print("saved:", p)
    print(f"\nrows: {len(df)}   patients: {df.PatientID.nunique()}   cohorts: {df.Cohort.value_counts().to_dict()}")
    print("notation:", df.Cylinder_Notation.value_counts().to_dict(), " target:", df.Target_Notation.value_counts().to_dict())
    print("\ncentroids in the PLUS convention (what the double-angle plot must show):")
    for r in exp[exp.vector.isin(["PRE", "PE"])].itertuples():
        print(f"  {r.vector:4s} {r.cohort:9s} n={r.n_eyes:4d}  centroid {r.centroid_mag:.3f} D @ {r.centroid_plus_axis:6.1f}"
              f"   (x={r.centroid_x:+.3f}, y={r.centroid_y:+.3f})  total SD {r.total_sd_N:.3f}")
    print("\ntrue PE centroids (Monte Carlo):", {c: f"({x:+.3f}, {y:+.3f})" for c, (x, y) in truth.items()})
    print("data SHA-256:", sha)


if __name__ == "__main__":
    main()
