"""
DS-D "Astigmatism" - simulated KLEx (keratorefractive lenticule extraction)
versus LASIK in eyes with 0.75 to 4.00 D of preoperative cylinder
(demonstration dataset D of StandardEyes v1.2.0). Simulated data - no real patients.

Demonstrates the JCRS 2025 astigmatism analysis: plus-cylinder double-angle
plots of the preoperative astigmatism, the SIA and the prediction error (and of
the postoperative astigmatism, because some targets are not plano), centroids,
total SD, the depth polygon and the centroid polygon, the Alpins graphs, the
AstigMATIC sheet and the WHWK input files.

  * preoperative cylinder 0.75-4.00 D, with-the-rule dominant (75%)
  * a planned residual cylinder (0.50 D at the preoperative steep meridian,
    SEQ plano) in eyes with 2.50 D or more of cylinder: the target refraction is
    not plano in those eyes, so TIA = target - preoperative and PE = postoperative
    - target
  * documented TRUE astigmatic correction: LASIK correction index 1.00 with no
    systematic rotation; KLEx undercorrects (correction index 0.85) and rotates
    the treatment by +6 degrees in both eyes (a systematic counterclockwise
    rotation), which leaves a non-zero prediction-error centroid in the KLEx group
  * procedure chosen per patient at random (no confounding)

Usage:  python make_dataset_D.py [--out <folder>]
        (writes Dataset_D_astigmatism.xlsx and its settings files into <folder>; without --out
        into an "out" folder beside this script)
"""
from __future__ import annotations

import os
import sys

import numpy as np
import pandas as pd

import simcore as sc

FNAME = "Dataset_D_astigmatism.xlsx"
GEN = os.path.basename(__file__)
SEED = 2026111107
SEED_POP = 2026111108
N_PATIENTS = 210
N_POP = 150_000

AGE = (31.0, 7.0, 19, 52)
P_FEMALE, P_BILATERAL = 0.53, 0.85
SEQ_P_MEAN, SEQ_P_SD, SEQ_E_SD, SEQ_MIN, SEQ_MAX = -4.2, 1.8, 0.55, -9.0, -1.0
PACHY_P_MEAN, PACHY_P_SD, PACHY_E_SD = 552.0, 26.0, 6.0
K_MEAN, K_P_SD, K_E_SD = 43.8, 1.4, 0.3
CYL_MEDIAN, CYL_LOG_P_SD, CYL_LOG_E_SD, CYL_MIN, CYL_MAX = 1.60, 0.40, 0.25, 0.75, 4.00
AXIS_TYPE_P = [0.75, 0.12, 0.13]                # WTR, ATR, oblique
AXIS_PATIENT_SD, AXIS_EYE_SD, AXIS_MIRROR_SD = 10.0, 4.0, 6.0
C_SEQ = -4.0
PROCS = ["LASIK", "KLEx"]
P_KLEX = 0.5
OZ = 6.5
KLEX_MIN_THICK_UM = 15.0
RESIDUAL_CYL = {"min_pre_cyl": 2.50, "p": 0.70, "mag": 0.50}

#              CI mean, AoE mean (deg, both eyes alike), AoE patient SD, AoE eye SD, SEQ bias
ASTIG = {"LASIK": (1.00, 0.0, 3.0, 5.0, 0.00),
         "KLEx": (0.85, 6.0, 3.0, 6.0, -0.08)}
CI_P_SD, CI_E_SD = 0.08, 0.05
NOISE_P_SD, NOISE_E_SD = 0.12, 0.10
SEQ_OUT = {"seq": 0.05, "sd_patient": 0.20, "sd_eye": 0.20}
CDVA_RHO, CDVA_SEQ_SHIFT = 0.75, 0.10
P_OUT_OF_WINDOW, P_MISSED_6M = 0.03, 0.06
MISSING = {"Preop_Pachymetry_um": 0.015, "Preop_Mean_K_D": 0.015, "Postop_UDVA": 0.01, "Postop_CDVA": 0.015}


def simulate(rng, n):
    P = sc.patients(rng, n, *AGE, P_FEMALE, P_BILATERAL)
    age, female, pres = P["age"], P["female"], P["present"]
    proc = (rng.random(n) < P_KLEX).astype(int)                 # 1 = KLEx
    seq_p = rng.normal(SEQ_P_MEAN, SEQ_P_SD, n)
    pachy_p = rng.normal(PACHY_P_MEAN, PACHY_P_SD, n)
    k_p = rng.normal(K_MEAN, K_P_SD, n)
    lcyl_p = rng.normal(np.log(CYL_MEDIAN), CYL_LOG_P_SD, n)
    pax, atype = sc.axes(rng, n, AXIS_TYPE_P, AXIS_PATIENT_SD, AXIS_EYE_SD, AXIS_MIRROR_SD)
    seq_lat = np.clip(seq_p[:, None] + rng.normal(0, SEQ_E_SD, (n, 2)), SEQ_MIN, SEQ_MAX)
    mag = sc.cylinder(rng, lcyl_p, CYL_LOG_E_SD, CYL_MIN, CYL_MAX)
    sph, seq = sc.refraction_from_seq(seq_lat, mag)
    pachy = np.round(pachy_p[:, None] + rng.normal(0, PACHY_E_SD, (n, 2)))
    kk = np.round(k_p[:, None] + rng.normal(0, K_E_SD, (n, 2)), 2)
    depth = np.round(OZ ** 2 / 3 * (np.abs(seq) + mag / 2) + rng.normal(0, 3, (n, 2))
                     + np.where(proc[:, None] == 1, KLEX_MIN_THICK_UM, 0.0))

    t_mag = np.zeros((n, 2)); t_sph = np.zeros((n, 2)); t_pax = pax.astype(float).copy()
    resid = (mag >= RESIDUAL_CYL["min_pre_cyl"]) & (rng.random((n, 2)) < RESIDUAL_CYL["p"])
    t_mag[resid] = RESIDUAL_CYL["mag"]
    t_sph[resid] = t_mag[resid] / 2
    t_seq = t_sph - t_mag / 2

    par = np.array([ASTIG[PROCS[k]] for k in proc])           # (n, 5)
    cd_pre = sc.cdva_pre_latent(rng, seq, n)
    ud_pre = sc.udva_pre_latent(rng, cd_pre, seq, mag, n)
    err = (par[:, [4]] + SEQ_OUT["seq"] * (seq - C_SEQ) + rng.normal(0, SEQ_OUT["sd_patient"], (n, 1))
           + rng.normal(0, SEQ_OUT["sd_eye"], (n, 2)))
    # AoE: the procedure's mean (same sign in both eyes) + patient part (mirrored in OS) + eye part,
    # with the procedure's SDs
    A = sc.astig_post(rng, mag, pax, t_mag, t_pax, par[:, [0]], CI_P_SD, CI_E_SD, par[:, [1]],
                      par[:, [2]], par[:, [3]], NOISE_P_SD, NOISE_E_SD)
    sx, sy, qx, qy, aoe = A["sx"], A["sy"], A["qx"], A["qy"], A["aoe"]
    pmag, ppax, psph, pseq = sc.post_refraction(qx, qy, t_seq + err, CYL_MAX)

    dl = sc.ordinal_change(rng, n, rho=CDVA_RHO, shift=CDVA_SEQ_SHIFT * (-seq - 4))
    cd_pre_line = np.clip(sc.line_of(cd_pre), -2, 3)
    cd_post_line = np.maximum(cd_pre_line - dl, -2)
    ud_pre_line = np.maximum(np.maximum(sc.line_of(ud_pre), cd_pre_line), -2)      # beyond line 13: recorded as CF
    ud_post = sc.udva_post_latent(rng, cd_post_line / 10, pseq, pmag, n)
    ud_post_line = np.clip(np.maximum(sc.line_of(ud_post), cd_post_line), -2, 17)

    pi, ei = np.nonzero(pres)
    f = lambda v: v[pi, ei]
    return pd.DataFrame(dict(
        pidx=pi, Eye=np.where(ei == 0, "OD", "OS"), Procedure=np.array(PROCS)[proc[pi]],
        Age_Years=age[pi].astype(int), Sex=np.where(female[pi], "F", "M"), axis_type=atype[pi],
        pre_sph=f(sph), pre_mag=f(mag), pre_pax=f(pax), Preop_SEQ_D=f(seq),
        pre_ud=f(ud_pre_line), pre_cd=f(cd_pre_line), Preop_Mean_K_D=f(kk), Preop_Pachymetry_um=f(pachy),
        Ablation_or_Lenticule_Depth_um=f(depth),
        t_sph=f(t_sph), t_mag=f(t_mag), t_pax=f(t_pax), Target_SEQ_D=f(t_seq),
        post_sph=f(psph), post_mag=f(pmag), post_pax=f(ppax), Postop_SEQ_D=f(pseq),
        post_ud=f(ud_post_line), post_cd=f(cd_post_line),
        px=f(A["px"]), py=f(A["py"]), tx=f(A["tx"]), ty=f(A["ty"]), sx=f(sx), sy=f(sy), qx=f(qx), qy=f(qy),
        ci=f(A["ci"]), aoe=f(aoe),
    ))


def triple(sph, mag, pax):
    s, c, a = sc.minus_triple(sph, mag, pax)
    a = a.astype(float)
    a[mag == 0] = np.nan
    return s, c, a


def build_dataset(seed=SEED):
    rng = np.random.default_rng(seed)
    e = simulate(rng, N_PATIENTS)
    n = len(e)
    pid = e.pidx.to_numpy()
    df = pd.DataFrame()
    df["PatientID"], df["EyeID"] = sc.ids("D", pid, e.Eye.to_numpy())
    df["Eye"] = e.Eye.to_numpy()
    for c in ("Procedure", "Age_Years", "Sex"):
        df[c] = e[c].to_numpy()
    df["Preop_Sphere_D"], df["Preop_Cylinder_D"], df["Preop_Axis_Deg"] = triple(
        e.pre_sph.to_numpy(), e.pre_mag.to_numpy(), e.pre_pax.to_numpy())
    df["Preop_SEQ_D"] = e.Preop_SEQ_D.to_numpy()
    df["Preop_UDVA"], df["Preop_CDVA"] = sc.dec_or_off(e.pre_ud), sc.dec_of(e.pre_cd)
    for c in ("Preop_Mean_K_D", "Preop_Pachymetry_um", "Ablation_or_Lenticule_Depth_um"):
        df[c] = e[c].to_numpy()
    df["Target_Sphere_D"], df["Target_Cylinder_D"], df["Target_Axis_Deg"] = triple(
        e.t_sph.to_numpy(), e.t_mag.to_numpy(), e.t_pax.to_numpy())
    df["Target_SEQ_D"] = e.Target_SEQ_D.to_numpy()
    day6 = 180 + rng.integers(-25, 26, n)
    oow = rng.random(n) < P_OUT_OF_WINDOW
    day6 = np.where(oow & (rng.random(n) < 0.5), rng.integers(125, 150, n), np.where(oow, rng.integers(211, 245, n), day6))
    df["FollowUp_Days"] = day6.astype(float)
    df["Postop_Sphere_D"], df["Postop_Cylinder_D"], df["Postop_Axis_Deg"] = triple(
        e.post_sph.to_numpy(), e.post_mag.to_numpy(), e.post_pax.to_numpy())
    df["Postop_SEQ_D"] = e.Postop_SEQ_D.to_numpy()
    df["Postop_UDVA"], df["Postop_CDVA"] = sc.dec_of(e.post_ud), sc.dec_of(e.post_cd)
    gone = (rng.random(pid.max() + 1) < P_MISSED_6M)[pid]
    for c in [c for c in df.columns if c.startswith("Postop_")] + ["FollowUp_Days"]:
        df[c] = df[c].astype(object).where(~gone, None)
    sc.blank_out(rng, df, MISSING)
    ints = ["Age_Years", "Preop_Axis_Deg", "Target_Axis_Deg", "Postop_Axis_Deg", "Preop_Pachymetry_um",
            "Ablation_or_Lenticule_Depth_um", "FollowUp_Days"]
    # Preop_UDVA keeps its "CF" cells (off the chart) beside the decimal numbers
    floats = [c for c in df.columns if c.endswith(("_D", "_um", "VA")) and c not in ints and c != "Preop_UDVA"]
    df = sc.typed(df, ints, floats)
    return df, e


def centroid(x, y):
    cx, cy = np.mean(x), np.mean(y)
    return np.hypot(cx, cy), (np.degrees(np.arctan2(cy, cx)) / 2) % 180, cx, cy


def population_truth():
    e = simulate(np.random.default_rng(SEED_POP), N_POP)
    out = {}
    for g in PROCS:
        d = e[e.Procedure == g]
        pex, pey = d.qx - d.tx, d.qy - d.ty
        tia = np.hypot(d.tx - d.px, d.ty - d.py)
        sia = np.hypot(d.sx, d.sy)
        out[g] = dict(pre=centroid(d.px, d.py), sia=centroid(d.sx, d.sy), pe=centroid(pex, pey),
                      post=centroid(d.qx, d.qy), ci_alpins=float(np.mean(sia / tia)), ci_model=float(d.ci.mean()),
                      aoe=float(d.aoe.mean()), cyl50=float((d.post_mag <= 0.5).mean()),
                      pe_total_sd=float(np.sqrt(np.var(pex) + np.var(pey))),
                      err=float((d.Postop_SEQ_D - d.Target_SEQ_D).mean()),
                      w50=float(((d.Postop_SEQ_D - d.Target_SEQ_D).abs() <= 0.5).mean()))
    return out


def sample_centroids(df):
    """PE centroid of the eyes in this file (as typed), plus format, target mapped."""
    d = df[(pd.to_numeric(df.FollowUp_Days).between(150, 210)) & df.Postop_Cylinder_D.notna()]
    out = {}
    for g in PROCS:
        x = d[d.Procedure == g]
        def v(cyl, ax):
            c = pd.to_numeric(x[cyl]).to_numpy(float)
            a = pd.to_numeric(x[ax]).to_numpy(float)
            a = np.where(np.isnan(a) & (c == 0), 0, a)
            return sc.vec(np.abs(c), np.where(c < 0, a + 90, a))
        qx, qy = v("Postop_Cylinder_D", "Postop_Axis_Deg")
        tx, ty = v("Target_Cylinder_D", "Target_Axis_Deg")
        m, ax, cx, cy = centroid(qx - tx, qy - ty)
        out[g] = (len(x), m, ax, cx, cy)
    return out


COLUMNS = [
    ("PatientID", "-", "Patient code (no names). Both eyes of a patient share it."),
    ("EyeID", "-", "Eye code = PatientID-Eye."),
    ("Eye", "-", "OD (right) or OS (left)."),
    ("Procedure", "-", "LASIK or KLEx (keratorefractive lenticule extraction; patient level)."),
    ("Age_Years", "years", "Age at surgery (patient level)."),
    ("Sex", "-", "F or M (patient level)."),
    ("Preop_Sphere_D / _Cylinder_D / _Axis_Deg", "D / D / degrees", "Preoperative manifest refraction, minus-cylinder notation (cylinder -0.75 to -4.00 D)."),
    ("Preop_SEQ_D", "D", "Preoperative SEQ = sphere + cylinder/2 exactly."),
    ("Preop_UDVA / Preop_CDVA", "decimal", "Preoperative uncorrected / corrected distance acuity, decimal value of a 0.1-logMAR chart line; "
     "an uncorrected acuity poorer than the chart's last line (0.05 = 20/400) is recorded as CF (counting fingers)."),
    ("Preop_Mean_K_D", "D", "Mean keratometry."),
    ("Preop_Pachymetry_um", "µm", "Central corneal thickness."),
    ("Ablation_or_Lenticule_Depth_um", "µm", "Maximum ablation depth (LASIK) or lenticule thickness (KLEx)."),
    ("Target_Sphere_D / _Cylinder_D / _Axis_Deg", "D / D / degrees", "Intended postoperative refraction, minus notation: 0 / 0 / blank "
     "(plano) or +0.25 / -0.50 / the preoperative minus axis (a planned residual cylinder of 0.50 D at the preoperative steep meridian, SEQ plano)."),
    ("Target_SEQ_D", "D", "Intended postoperative SEQ (0 for every eye)."),
    ("FollowUp_Days", "days", "Days from surgery to the 6-month (primary endpoint) visit; blank when that visit was missed."),
    ("Postop_Sphere_D / _Cylinder_D / _Axis_Deg", "D / D / degrees", "Refraction at 6 months, minus notation; blank axis when the cylinder is 0."),
    ("Postop_SEQ_D", "D", "SEQ at 6 months = sphere + cylinder/2 exactly."),
    ("Postop_UDVA / Postop_CDVA", "decimal", "Acuity at 6 months (chart lines)."),
]


def readme(df, truth, sha):
    R = sc.Readme()
    R.add("Simulated data - no real patients")
    R.add("Every value in the sheet 'Surgeries' was generated by a computer model. No person, clinic or real "
          "measurement is represented. Patients and eyes carry codes only; there are no names, dates of birth or dates.")
    R.add("Dataset D of the StandardEyes demonstration datasets: 'Astigmatism' - KLEx versus LASIK in eyes with 0.75 to 4.00 D "
          "of preoperative cylinder, a documented undercorrection and rotation in the KLEx group, and some non-plano targets.")
    R.blank()
    sc.source_rows(R, GEN, SEED, df, sha, f"population table: seed {SEED_POP}")
    R.add("Size", f"{df.PatientID.nunique()} patients, {len(df)} eyes (one row per eye)",
          "; ".join(f"{g}: {len(d)} eyes of {d.PatientID.nunique()} patients" for g, d in df.groupby("Procedure")))
    R.add("Settings files", "Settings_D_astigmatism.json", "groups LASIK (reference) and KLEx; the target refraction is mapped")
    R.blank()
    pc = pd.to_numeric(df.Preop_Cylinder_D).abs()
    pa = pd.to_numeric(df.Preop_Axis_Deg)
    plus_ax = (pa + 90) % 180
    wtr = (plus_ax >= 60) & (plus_ax <= 120)
    atr = (plus_ax <= 30) | (plus_ax >= 150)
    R.add("ASTIGMATISM OF THIS FILE")
    R.add("Preoperative cylinder", f"{pc.min():.2f} to {pc.max():.2f} D, median {pc.median():.2f} D",
          f"with-the-rule (plus axis 60-120) {wtr.mean():.0%}, against-the-rule (plus axis 0-30 / 150-180) {atr.mean():.0%}, "
          f"oblique {1 - wtr.mean() - atr.mean():.0%}")
    tc = pd.to_numeric(df.Target_Cylinder_D)
    R.add("Non-plano targets", f"{int((tc != 0).sum())} eyes ({(tc != 0).mean():.1%})",
          "planned residual cylinder 0.50 D at the preoperative steep meridian (target +0.25 -0.50 x preop minus axis), "
          "in eyes with a preoperative cylinder of 2.50 D or more")
    R.add("Notation", "minus cylinder throughout; the double-angle plots use plus-cylinder format (with-the-rule on the left)")
    R.blank()
    R.add("TRUE ASTIGMATIC CORRECTION", "LASIK", "KLEx", "note")
    R.add("Correction index (model mean)", f"{ASTIG['LASIK'][0]:.2f}", f"{ASTIG['KLEx'][0]:.2f}",
          f"+ N(0, {CI_P_SD}) patient + N(0, {CI_E_SD}) eye")
    R.add("Angle of error, systematic (deg)", f"{ASTIG['LASIK'][1]:+.0f}", f"{ASTIG['KLEx'][1]:+.0f}",
          "same direction in both eyes (counterclockwise in the double-angle plane)")
    R.add("Angle of error, random SD patient / eye (deg)", f"{ASTIG['LASIK'][2]:.0f} / {ASTIG['LASIK'][3]:.0f}",
          f"{ASTIG['KLEx'][2]:.0f} / {ASTIG['KLEx'][3]:.0f}", "patient part mirrored in OS")
    R.add("Postop SEQ - target, bias", f"{ASTIG['LASIK'][4]:+.2f} D", f"{ASTIG['KLEx'][4]:+.2f} D",
          "true difference KLEx - LASIK = -0.08 D")
    R.blank()
    R.add(f"EXPECTED VALUES (population of {N_POP:,} simulated patients, seed {SEED_POP}; plus format, before rounding)",
          "LASIK", "KLEx", "")
    fc = lambda c: f"{c[0]:.2f} D @ {c[1]:.0f} (x {c[2]:+.2f}, y {c[3]:+.2f})"
    for lab, k in (("Preoperative centroid", "pre"), ("SIA centroid", "sia"), ("Prediction-error centroid (PE = POST - T)", "pe"),
                   ("Postoperative centroid", "post")):
        R.add(lab, fc(truth["LASIK"][k]), fc(truth["KLEx"][k]), "")
    for lab, k, f in (("PE total SD (D)", "pe_total_sd", "{:.2f}"), ("Alpins correction index |SIA|/|TIA| (mean)", "ci_alpins", "{:.2f}"),
                      ("Angle of error (mean, deg)", "aoe", "{:+.1f}"), ("Postop cylinder <= 0.50 D", "cyl50", "{:.1%}"),
                      ("Postop SEQ - target (mean, D)", "err", "{:+.3f}"), ("SEQ within +/-0.50 D", "w50", "{:.1%}")):
        R.add(lab, f.format(truth["LASIK"][k]), f.format(truth["KLEx"][k]), "")
    R.blank()
    sc_ = sample_centroids(df)
    R.add("THIS FILE: prediction-error centroid", " / ".join(
        f"{g}: {sc_[g][1]:.2f} D @ {sc_[g][2]:.0f} (x {sc_[g][3]:+.2f}, y {sc_[g][4]:+.2f}; {sc_[g][0]} eyes)" for g in PROCS),
          "", "eyes with a 6-month refraction at 150-210 days; plus format, as typed (0.25 D steps)")
    R.blank()
    R.add("GENERATING MODEL", "parameter", "value", "meaning")
    P = [
        ("Patients", "age / female / both eyes", f"N({AGE[0]}, {AGE[1]}) rounded {AGE[2]}-{AGE[3]} / {P_FEMALE} / {P_BILATERAL}", ""),
        ("Patients", "procedure", f"KLEx with probability {P_KLEX}", "at random: no confounding"),
        ("Patients", "latent preop SEQ / pachymetry / mean K", f"N({SEQ_P_MEAN}, {SEQ_P_SD}) D / N({PACHY_P_MEAN:.0f}, {PACHY_P_SD:.0f}) µm / "
         f"N({K_MEAN}, {K_P_SD}) D", "patient means"),
        ("Patients", "latent log cylinder", f"N(log {CYL_MEDIAN}, {CYL_LOG_P_SD})", "patient mean"),
        ("Eyes", "preop SEQ", f"SEQp + N(0, {SEQ_E_SD}), limited to {SEQ_MIN} / {SEQ_MAX} D", "sphere on 0.25 D, SEQ derived"),
        ("Eyes", "cylinder magnitude", f"exp(log cyl + N(0, {CYL_LOG_E_SD})), 0.25 D steps, redrawn until {CYL_MIN:.2f}-{CYL_MAX:.2f} D", ""),
        ("Eyes", "plus axis", f"WTR / ATR / oblique {AXIS_TYPE_P}; patient SD {AXIS_PATIENT_SD:.0f}, eye SD {AXIS_EYE_SD:.0f} deg; "
         f"OS = 180 - OD + N(0, {AXIS_MIRROR_SD:.0f})", "mirror symmetry"),
        ("Eyes", "depth", f"{OZ}^2/3 (|SEQ| + |cyl|/2) + N(0, 3) µm; KLEx + {KLEX_MIN_THICK_UM:.0f} µm", "Munnerlyn"),
        ("Target", "planned residual cylinder", f"preop cylinder >= {RESIDUAL_CYL['min_pre_cyl']:.2f} D: probability {RESIDUAL_CYL['p']}, "
         f"{RESIDUAL_CYL['mag']:.2f} D at the preoperative steep meridian, SEQ plano", "otherwise plano"),
        ("Astigmatism", "correction", "TIA = T - PRE; SIA = CI R(2 AoE) TIA; POST = PRE + SIA + N(0, "
         f"{NOISE_P_SD}) patient (mirrored in OS) + N(0, {NOISE_E_SD}) eye per component", "plus-format double-angle vectors"),
        ("Postop SEQ", "model", f"target + bias[procedure] {SEQ_OUT['seq']:+}(preop SEQ+4) + u + e",
         f"u ~ N(0, {SEQ_OUT['sd_patient']}), e ~ N(0, {SEQ_OUT['sd_eye']})"),
        ("Acuity", "CDVA, UDVA", "dataset A model (decimal chart lines; preop UDVA from the refraction, postop UDVA from "
         "the residual SEQ and cylinder)", f"no procedure term; preop UDVA {sc.UDVA_OFF_NOTE}"),
        ("Visits", "6-month visit", f"180 +/- 25 days; outside 150-210 days in {P_OUT_OF_WINDOW:.0%}; missed by {P_MISSED_6M:.0%} of patients", ""),
        ("Missing values", "share of rows", ", ".join(f"{k} {v:.1%}" for k, v in MISSING.items()), ""),
    ]
    for p in P:
        R.add(*p)
    R.blank()
    R.add("COLUMN DICTIONARY", "unit", "description")
    for c in COLUMNS:
        R.add(*c)
    R.add("Acuity", sc.decimal_lines_text(), f"Preop_UDVA: {int((df.Preop_UDVA == sc.OFF_CHART).sum())} eyes CF")
    return R


def write_settings():
    cfg = sc.settings(
        FNAME, group_source_var="Procedure",
        baseline={"continuous": ["Age_Years", "Preop_SEQ_D", "Preop_Sphere_D", "Preop_Cylinder_D", "Preop_UDVA", "Preop_CDVA",
                                 "Preop_Mean_K_D", "Preop_Pachymetry_um"], "ordinal": [], "dichotomous": ["Sex"], "categorical": []},
        preop={"continuous": ["Ablation_or_Lenticule_Depth_um"], "ordinal": [], "dichotomous": [], "categorical": []},
        outcome={"continuous": ["FollowUp_Days", "Postop_SEQ_D", "Postop_Cylinder_D", "Postop_UDVA", "Postop_CDVA"],
                 "ordinal": [], "dichotomous": [], "categorical": []},
        groups={"LASIK": ["LASIK"], "KLEx": ["KLEx"]}, reference_group="LASIK",
        group_colors={"LASIK": "#E69F00", "KLEx": "#56B4E9"},
        std_triples=[{"sph": "Preop_Sphere_D", "cyl": "Preop_Cylinder_D", "axis": "Preop_Axis_Deg"},
                     {"sph": "Target_Sphere_D", "cyl": "Target_Cylinder_D", "axis": "Target_Axis_Deg"},
                     {"sph": "Postop_Sphere_D", "cyl": "Postop_Cylinder_D", "axis": "Postop_Axis_Deg"}],
        astig_target={"cyl": "Target_Cylinder_D", "axis": "Target_Axis_Deg"},
        astig_sheet=True, astig_whwk_files=True, astig_centroid_region="bootstrap",
        output_prefix="DS_D")
    return [sc.write_settings(
        "Settings_D_astigmatism.json", cfg, "Dataset D - astigmatism (KLEx vs LASIK)",
        "Two groups (LASIK, KLEx) with 0.75-4.00 D of preoperative cylinder; the target refraction is mapped, so TIA is the "
        "intended change and PE = postoperative - target.",
        "The JCRS 2025 astigmatism analysis: plus-cylinder double-angle plots of the preoperative astigmatism, SIA, prediction "
        "error and postoperative astigmatism with each group's centroid, total SD, the 95% depth polygon of the eyes and the "
        "bootstrap polygon of the centroid; the robust centroid tests; the Alpins semicircle graphs; the Astigmatism and "
        "AstigMATIC sheets; and the WHWK input files.")]


def main():
    os.makedirs(sc.OUT, exist_ok=True)
    df, e = build_dataset()
    sha = sc.data_sha(df)
    truth = population_truth()
    path = os.path.join(sc.OUT, FNAME)
    sc.write_workbook(df, readme(df, truth, sha), path, GEN)
    st = write_settings()
    print("saved:", path)
    print("settings:", ", ".join(os.path.basename(s) for s in st))
    print(df.groupby("Procedure").agg(eyes=("EyeID", "size"), patients=("PatientID", "nunique")).to_string())
    print("non-plano targets:", int((pd.to_numeric(df.Target_Cylinder_D) != 0).sum()))
    for g in PROCS:
        print(g, {k: (tuple(round(x, 3) for x in v) if isinstance(v, tuple) else round(v, 3)) for k, v in truth[g].items()})
    print("file PE centroids:", sample_centroids(df))
    print("data SHA-256:", sha)
    return 0


if __name__ == "__main__":
    sys.exit(main())
