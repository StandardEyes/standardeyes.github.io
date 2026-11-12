"""
DS-B "Notation harmonization" - simulated LASIK at two centers (demonstration
dataset B of StandardEyes v1.2.0). Simulated data - no real patients.

Demonstrates the built-in harmonization of notation:
  * cylinders typed in mixed plus and minus notation IN THE SAME COLUMNS: the
    preoperative refraction (and the target typed with it) in plus notation for
    about 40% of the patients, the postoperative refraction in plus notation
    for an independent 40% - so some rows mix the two forms. Columns
    Preop_Notation / Postop_Notation say which (information only: the app
    decides by the sign of each cylinder)
  * a target refraction (sphere, cylinder, axis) typed in either notation: plano
    for most eyes, myopic monovision in one eye of some patients aged 38+, and a
    planned residual cylinder of 0.50 D in eyes with 1.75 D or more of
    preoperative cylinder
  * the same acuities recorded twice: as logMAR line scores (the study form;
    *_logMAR columns) and as classic Snellen chart readings with letter suffixes
    as typed in the clinic record ("20/25-2", "20/15+1", "CF"; *_Snellen columns)
  * a zero cylinder has a blank axis cell (as most clinic exports write it)

StandardEyes reads ONE acuity notation per settings file (va_format applies
to every acuity column), so the dataset comes with two settings files: one
reading the logMAR columns, one reading the Snellen columns.

Documented true difference: Center 2's nomogram leaves eyes 0.15 D more myopic
(postop SEQ - target, Center 2 - Center 1 = -0.15 D); nothing else differs.

Usage:  python make_dataset_B.py [--out <folder>]
        (writes Dataset_B_notation.xlsx and its settings files into <folder>; without --out
        into an "out" folder beside this script)
"""
from __future__ import annotations

import os
import sys

import numpy as np
import pandas as pd

import simcore as sc

FNAME = "Dataset_B_notation.xlsx"
GEN = os.path.basename(__file__)
SEED = 2026111103
SEED_POP = 2026111104
N_PATIENTS = 190
N_POP = 100_000

AGE = (30.0, 7.0, 19, 52)
P_FEMALE, P_BILATERAL = 0.52, 0.80
SEQ_P_MEAN, SEQ_P_SD, SEQ_E_SD, SEQ_MIN, SEQ_MAX = -3.3, 1.5, 0.50, -7.0, -0.75
PACHY_P_MEAN, PACHY_P_SD, PACHY_E_SD = 548.0, 27.0, 6.0
CYL_MEDIAN, CYL_LOG_P_SD, CYL_LOG_E_SD, CYL_MAX = 0.80, 0.50, 0.40, 4.0
AXIS_TYPE_P = [0.60, 0.20, 0.20]
C_AGE, C_SEQ = 30.0, -4.0
CENTERS = ["Center 1", "Center 2"]
P_CENTER2 = 0.5
P_PLUS_PRE, P_PLUS_POST = 0.40, 0.40          # per patient, independently
MONOVISION = {"min_age": 38, "p": 0.35, "targets": (-0.50, -0.75), "p_targets": (0.5, 0.5)}
MONO_MIN_TREAT = 0.75                          # D: a monovision eye has preop SEQ <= target - 0.75
RESIDUAL_CYL = {"min_pre_cyl": 1.75, "p": 0.70, "mag": 0.50}   # planned residual cylinder

SEQ_OUT = {"Center 1": 0.03, "Center 2": -0.12, "seq": 0.05, "sd_patient": 0.22, "sd_eye": 0.20}
CI_MEAN, CI_P_SD, CI_E_SD = 0.96, 0.10, 0.04
AOE_P_SD, AOE_E_SD = 3.0, 6.0
NOISE_P_SD, NOISE_E_SD = 0.11, 0.06
CDVA_RHO, CDVA_SEQ_SHIFT = 0.75, 0.10
UDVA_PRE_WORST = 1.6                           # logMAR chart at 1 m: its last line; poorer is recorded as CF
P_OUT_OF_WINDOW, P_MISSED_6M = 0.03, 0.06
RETREAT = {"seq_err": 0.75, "cyl": 1.00}
MISSING = {"Preop_Pachymetry_um": 0.02}
VA_MISSING = 0.012                             # one postop acuity measurement not recorded (both notations)


def simulate(rng, n_patients):
    n = n_patients
    P = sc.patients(rng, n, *AGE, P_FEMALE, P_BILATERAL)
    age, female, pres = P["age"], P["female"], P["present"]
    center = (rng.random(n) < P_CENTER2).astype(int)
    seq_p = rng.normal(SEQ_P_MEAN, SEQ_P_SD, n)
    pachy_p = rng.normal(PACHY_P_MEAN, PACHY_P_SD, n)
    lcyl_p = rng.normal(np.log(CYL_MEDIAN), CYL_LOG_P_SD, n)
    pax, _ = sc.axes(rng, n, AXIS_TYPE_P)
    seq_lat = np.clip(seq_p[:, None] + rng.normal(0, SEQ_E_SD, (n, 2)), SEQ_MIN, SEQ_MAX)
    mag = sc.cylinder(rng, lcyl_p, CYL_LOG_E_SD, 0.0, CYL_MAX)
    sph, seq = sc.refraction_from_seq(seq_lat, mag)
    pachy = np.round(pachy_p[:, None] + rng.normal(0, PACHY_E_SD, (n, 2)))

    # target: monovision sphere, or a planned residual cylinder at the preop steep meridian (SEQ plano)
    t_sph = np.zeros((n, 2)); t_mag = np.zeros((n, 2)); t_pax = pax.astype(float).copy()
    mono = (age >= MONOVISION["min_age"]) & pres.all(1) & (rng.random(n) < MONOVISION["p"])
    me = (rng.random(n) < 0.5).astype(int)
    mt = rng.choice(MONOVISION["targets"], size=n, p=MONOVISION["p_targets"])
    mono &= seq[np.arange(n), me] <= mt - MONO_MIN_TREAT        # treated by at least 0.75 D, otherwise plano
    t_sph[np.flatnonzero(mono), me[mono]] = mt[mono]
    resid = (mag >= RESIDUAL_CYL["min_pre_cyl"]) & (rng.random((n, 2)) < RESIDUAL_CYL["p"])
    t_mag[resid] = RESIDUAL_CYL["mag"]
    t_sph[resid] = t_mag[resid] / 2          # minus sphere +0.25 with -0.50 cylinder: SEQ plano
    t_seq = t_sph - t_mag / 2

    cd_pre = sc.cdva_pre_latent(rng, seq, n)
    ud_pre = sc.udva_pre_latent(rng, cd_pre, seq, mag, n)
    err = (np.where(center[:, None] == 1, SEQ_OUT["Center 2"], SEQ_OUT["Center 1"]) + SEQ_OUT["seq"] * (seq - C_SEQ)
           + rng.normal(0, SEQ_OUT["sd_patient"], (n, 1)) + rng.normal(0, SEQ_OUT["sd_eye"], (n, 2)))
    A = sc.astig_post(rng, mag, pax, t_mag, t_pax, CI_MEAN, CI_P_SD, CI_E_SD, 0.0, AOE_P_SD, AOE_E_SD,
                      NOISE_P_SD, NOISE_E_SD)
    pmag, ppax, psph, pseq = sc.post_refraction(A["qx"], A["qy"], t_seq + err, CYL_MAX)
    dl = sc.ordinal_change(rng, n, rho=CDVA_RHO, shift=CDVA_SEQ_SHIFT * (-seq - 4))
    cd_post = np.maximum(cd_pre - 0.1 * dl, -0.22)
    ud_post = sc.udva_post_latent(rng, cd_post, pseq, pmag, n)
    retreat = (np.abs(pseq - t_seq) >= RETREAT["seq_err"]) | (pmag >= RETREAT["cyl"])
    plus_pre = rng.random(n) < P_PLUS_PRE
    plus_post = rng.random(n) < P_PLUS_POST

    pi, ei = np.nonzero(pres)
    f = lambda a: a[pi, ei]
    return pd.DataFrame(dict(
        pidx=pi, Eye=np.where(ei == 0, "OD", "OS"), Center=np.array(CENTERS)[center[pi]],
        Age_Years=age[pi].astype(int), Sex=np.where(female[pi], "F", "M"),
        plus_pre=plus_pre[pi], plus_post=plus_post[pi],
        pre_sph=f(sph), pre_mag=f(mag), pre_pax=f(pax), Preop_SEQ_D=f(seq),
        t_sph=f(t_sph), t_mag=f(t_mag), t_pax=f(t_pax), Target_SEQ_D=f(t_seq),
        cd_pre=f(cd_pre), ud_pre=f(ud_pre), cd_post=f(cd_post), ud_post=f(ud_post), dl=f(dl),
        Preop_Pachymetry_um=f(pachy), err=f(err),
        post_sph=f(psph), post_mag=f(pmag), post_pax=f(ppax), Postop_SEQ_D=f(pseq),
        post_qx=f(A["qx"]), post_qy=f(A["qy"]), retreat=f(retreat),
    ))


def triple(sph, mag, pax, plus):
    """The refraction as typed: plus or minus notation per row; blank axis for a zero cylinder."""
    ms, mc, ma = sc.minus_triple(sph, mag, pax)
    ps, pc, pa = sc.plus_triple(sph, mag, pax)
    s = np.where(plus, ps, ms) + 0.0
    c = np.where(plus, pc, mc) + 0.0
    a = np.where(plus, pa, ma).astype(float)
    a[mag == 0] = np.nan
    return s, c, a


def build_dataset(seed=SEED):
    rng = np.random.default_rng(seed)
    e = simulate(rng, N_PATIENTS)
    n = len(e)
    pid = e.pidx.to_numpy()
    df = pd.DataFrame()
    df["PatientID"], df["EyeID"] = sc.ids("B", pid, e.Eye.to_numpy())
    df["Eye"] = e.Eye.to_numpy()
    for c in ("Center", "Age_Years", "Sex"):
        df[c] = e[c].to_numpy()
    pp, po = e.plus_pre.to_numpy(), e.plus_post.to_numpy()
    df["Preop_Notation"] = np.where(pp, "plus", "minus")
    df["Preop_Sphere_D"], df["Preop_Cylinder_D"], df["Preop_Axis_Deg"] = triple(
        e.pre_sph.to_numpy(), e.pre_mag.to_numpy(), e.pre_pax.to_numpy(), pp)
    df["Preop_SEQ_D"] = e.Preop_SEQ_D.to_numpy()
    df["Target_Sphere_D"], df["Target_Cylinder_D"], df["Target_Axis_Deg"] = triple(
        e.t_sph.to_numpy(), e.t_mag.to_numpy(), e.t_pax.to_numpy(), pp)
    df["Target_SEQ_D"] = e.Target_SEQ_D.to_numpy()
    ud_line = sc.line_of(e.ud_pre)                 # beyond the 1-m chart's last line: CF
    on = ud_line <= round(UDVA_PRE_WORST * 10)
    lm = np.empty(n, dtype=object)
    lm[on], lm[~on] = ud_line[on] / 10, sc.OFF_CHART
    df["Preop_UDVA_logMAR"] = lm
    df["Preop_CDVA_logMAR"] = sc.line_of(e.cd_pre) / 10
    df["Preop_UDVA_Snellen"] = sc.snellen_of(e.ud_pre.to_numpy())
    df["Preop_CDVA_Snellen"] = sc.snellen_of(e.cd_pre.to_numpy())
    df["Preop_Pachymetry_um"] = e.Preop_Pachymetry_um.to_numpy()

    day6 = 180 + rng.integers(-25, 26, n)
    oow = rng.random(n) < P_OUT_OF_WINDOW
    day6 = np.where(oow & (rng.random(n) < 0.5), rng.integers(125, 150, n), np.where(oow, rng.integers(211, 245, n), day6))
    df["FollowUp_Days"] = day6.astype(float)
    df["Postop_Notation"] = np.where(po, "plus", "minus")
    df["Postop_Sphere_D"], df["Postop_Cylinder_D"], df["Postop_Axis_Deg"] = triple(
        e.post_sph.to_numpy(), e.post_mag.to_numpy(), e.post_pax.to_numpy(), po)
    df["Postop_SEQ_D"] = e.Postop_SEQ_D.to_numpy()
    ud_lm, cd_lm = sc.line_of(e.ud_post) / 10, sc.line_of(e.cd_post) / 10
    ud_sn, cd_sn = sc.snellen_of(e.ud_post.to_numpy()), sc.snellen_of(e.cd_post.to_numpy())
    miss_u = rng.random(n) < VA_MISSING
    miss_c = rng.random(n) < VA_MISSING
    df["Postop_UDVA_logMAR"] = np.where(miss_u, np.nan, ud_lm)
    df["Postop_CDVA_logMAR"] = np.where(miss_c, np.nan, cd_lm)
    df["Postop_UDVA_Snellen"] = np.where(miss_u, None, ud_sn)
    df["Postop_CDVA_Snellen"] = np.where(miss_c, None, cd_sn)
    df["Retreatment_Indicated"] = np.where(e.retreat, "Yes", "No")

    # an eye whose patient missed the 6-month visit has no postoperative data
    npat = pid.max() + 1
    gone = (rng.random(npat) < P_MISSED_6M)[pid]
    post_cols = [c for c in df.columns if c.startswith("Postop_") or c in ("FollowUp_Days", "Retreatment_Indicated")]
    for c in post_cols:
        df[c] = df[c].astype(object).where(~gone, None)
    sc.blank_out(rng, df, MISSING)
    ints = ["Age_Years", "Preop_Axis_Deg", "Target_Axis_Deg", "Postop_Axis_Deg", "Preop_Pachymetry_um", "FollowUp_Days"]
    # Preop_UDVA_logMAR keeps its "CF" cells (off the chart) beside the numbers
    floats = [c for c in df.columns if c.endswith(("_D", "_um", "_logMAR")) and c not in ints and c != "Preop_UDVA_logMAR"]
    df = sc.typed(df, ints, floats)
    return df, e


def population_truth():
    e = simulate(np.random.default_rng(SEED_POP), N_POP)
    out = {}
    for g in CENTERS:
        d = e[e.Center == g]
        err = d.Postop_SEQ_D - d.Target_SEQ_D
        out[g] = dict(err=err.mean(), sd=err.std(), w50=(err.abs() <= 0.5).mean(),
                      ud20=(sc.line_of(d.ud_post) <= 0).mean(),
                      eff=(10 ** -(sc.line_of(d.ud_post) / 10) / 10 ** -(sc.line_of(d.cd_pre) / 10)).mean(),
                      saf=(10 ** -(sc.line_of(d.cd_post) / 10) / 10 ** -(sc.line_of(d.cd_pre) / 10)).mean())
    # how close the two acuity notations of the same measurement are
    sn = sc.snellen_logmar(sc.snellen_of(e.cd_post.to_numpy()))
    lm = sc.line_of(e.cd_post) / 10
    out["notation_diff"] = (np.nanmean(sn - lm), np.nanstd(sn - lm))
    return out


def sample_estimates(df):
    d = df[pd.to_numeric(df.FollowUp_Days).between(150, 210) | df.FollowUp_Days.isna()]
    num = lambda c: pd.to_numeric(d[c]).astype(float).to_numpy()
    c2 = (d.Center == "Center 2").to_numpy(float)
    cov = np.column_stack([num("Preop_SEQ_D") - C_SEQ])
    pid = d.PatientID.astype(str).to_numpy()
    cr, ad, ne, npt = sc.contrast(num("Postop_SEQ_D") - num("Target_SEQ_D"), [c2], cov, pid)
    return len(d), [("Postop SEQ - target (6 months)", "D, Center 2 - Center 1",
                     SEQ_OUT["Center 2"] - SEQ_OUT["Center 1"], cr[0], ad[0], ne, npt)]


COLUMNS = [
    ("PatientID", "-", "Patient code (no names). Both eyes of a patient share it."),
    ("EyeID", "-", "Eye code = PatientID-Eye."),
    ("Eye", "-", "OD (right) or OS (left)."),
    ("Center", "-", "Center 1 or Center 2 (patient level). Every eye had LASIK."),
    ("Age_Years", "years", "Age at surgery (patient level)."),
    ("Sex", "-", "F or M (patient level)."),
    ("Preop_Notation", "-", "plus or minus: the notation of the preoperative AND the target refraction of this row "
     "(information only - StandardEyes decides by the sign of each cylinder)."),
    ("Preop_Sphere_D / _Cylinder_D / _Axis_Deg", "D / D / degrees",
     "Preoperative manifest refraction in the row's notation: minus = negative cylinder at the flat meridian, "
     "plus = positive cylinder at the steep meridian. Blank axis when the cylinder is 0."),
    ("Preop_SEQ_D", "D", "Preoperative SEQ = sphere + cylinder/2 exactly (the same in both notations)."),
    ("Target_Sphere_D / _Cylinder_D / _Axis_Deg", "D / D / degrees",
     "Intended postoperative refraction, typed in the preoperative notation of the row: 0 / 0 / blank (plano), "
     "a monovision sphere, or a planned residual cylinder of 0.50 D (minus +0.25 -0.50 x the preoperative minus axis, "
     "= plus -0.25 +0.50 x the preoperative plus axis)."),
    ("Target_SEQ_D", "D", "Intended postoperative SEQ = target sphere + target cylinder/2."),
    ("Preop_UDVA_logMAR / Preop_CDVA_logMAR", "logMAR", "Preoperative uncorrected / corrected distance acuity as a "
     "logMAR line score (0.1 steps; 0.0 = 20/20; logMAR chart at 4 m, at 1 m for the poorest uncorrected acuities); "
     "CF (counting fingers) when poorer than 1.6, the last line of the chart at 1 m."),
    ("Preop_UDVA_Snellen / Preop_CDVA_Snellen", "Snellen", "The same measurement as typed in the clinic record: a classic "
     "Snellen chart line 20/x (x = " + ", ".join(map(str, sc.SNELLEN_CHART)) + ") with a letter suffix -1, -2 "
     "(letters missed) or +1, +2 (letters read on the next line); CF (counting fingers) below the 20/400 line."),
    ("Preop_Pachymetry_um", "µm", "Central corneal thickness."),
    ("FollowUp_Days", "days", "Days from surgery to the 6-month (primary endpoint) visit; blank when that visit was missed."),
    ("Postop_Notation", "-", "plus or minus: the notation of the postoperative refraction of this row (chosen independently "
     "of the preoperative one, so some rows mix the two forms)."),
    ("Postop_Sphere_D / _Cylinder_D / _Axis_Deg", "D / D / degrees", "Refraction at 6 months in the row's postoperative notation."),
    ("Postop_SEQ_D", "D", "SEQ at 6 months = sphere + cylinder/2 exactly."),
    ("Postop_UDVA_logMAR / Postop_CDVA_logMAR", "logMAR", "Acuity at 6 months, logMAR line score."),
    ("Postop_UDVA_Snellen / Postop_CDVA_Snellen", "Snellen", "The same measurement as typed in the clinic record (Snellen with letter suffix)."),
    ("Retreatment_Indicated", "-", "Yes when |postop SEQ - target| >= 0.75 D or postop cylinder >= 1.00 D, else No."),
]


def readme(df, truth, sha):
    R = sc.Readme()
    R.add("Simulated data - no real patients")
    R.add("Every value in the sheet 'Surgeries' was generated by a computer model. No person, clinic or real "
          "measurement is represented. Patients and eyes carry codes only; there are no names, dates of birth or dates.")
    R.add("Dataset B of the StandardEyes demonstration datasets: 'Notation harmonization' - mixed plus / minus cylinder "
          "notation in the same columns (targets included), acuity in logMAR and in Snellen fractions with letter suffixes.")
    R.blank()
    sc.source_rows(R, GEN, SEED, df, sha, f"population table: seed {SEED_POP}")
    R.add("Size", f"{df.PatientID.nunique()} patients, {len(df)} eyes (one row per eye)",
          "; ".join(f"{g}: {len(d)} eyes of {d.PatientID.nunique()} patients" for g, d in df.groupby("Center")))
    R.add("Settings files", "Settings_B_notation_logMAR.json; Settings_B_notation_Snellen.json",
          "StandardEyes reads one acuity notation per settings file: the first reads the *_logMAR columns "
          "(notation logMAR), the second the *_Snellen columns (notation Snellen); everything else is identical")
    R.blank()
    pre_plus = df.Preop_Notation == "plus"
    post_plus = df.Postop_Notation == "plus"
    has_post = df.Postop_Sphere_D.notna()
    tc = pd.to_numeric(df.Target_Cylinder_D)
    R.add("WHICH ROWS ARE TYPED IN PLUS NOTATION")
    R.add("Preoperative and target", f"{int(pre_plus.sum())} of {len(df)} rows ({pre_plus.mean():.1%})",
          "column Preop_Notation = plus; per patient (both eyes alike), probability 0.40")
    R.add("Postoperative", f"{int((post_plus & has_post).sum())} of {int(has_post.sum())} rows with a 6-month refraction "
          f"({(post_plus & has_post).sum() / has_post.sum():.1%})",
          "column Postop_Notation = plus; per patient, independently of the preoperative notation (probability 0.40)")
    R.add("Rows mixing the two forms", f"{int((pre_plus != post_plus)[has_post].sum())} rows",
          "preoperative in one notation, postoperative in the other")
    for lab, col in (("Preop", "Preop_Cylinder_D"), ("Postop", "Postop_Cylinder_D"), ("Target", "Target_Cylinder_D")):
        v = pd.to_numeric(df[col])
        nz, nn = int((v == 0).sum()), int((v < 0).sum())
        R.add(f"{lab} cylinders that are positive", f"{int((v > 0).sum())}",
              f"the cells StandardEyes transposes (sphere + cylinder, -cylinder, axis +/- 90); {nz} zero "
              f"cylinder{'s' if nz != 1 else ''} (identical in both notations, blank axis) and {nn} negative")
    R.add("Targets", f"{int((tc != 0).sum())} eyes with a target cylinder (0.50 D planned residual cylinder), "
          f"{int(((tc == 0) & (pd.to_numeric(df.Target_SEQ_D) != 0)).sum())} monovision targets, the rest plano",
          "targets typed in the preoperative notation of the row")
    R.add("Conversion rule", "plus sphere = minus sphere + minus cylinder; plus cylinder = -(minus cylinder); "
          "plus axis = minus axis +/- 90 (1-180); SEQ is identical in both notations")
    R.blank()
    R.add("ACUITY NOTATION")
    snu = pd.concat([df[c] for c in df.columns if c.endswith("_Snellen")])
    snu = snu[snu.notna()].astype(str)
    lmu = pd.concat([df[c] for c in df.columns if c.endswith("_logMAR")])
    lmu = lmu[lmu.notna()]
    n_cf_lm = int((lmu.astype(str) == sc.OFF_CHART).sum())
    R.add("logMAR columns", "line scores 0.1 apart: " + ", ".join(f"{v:.1f}" for v in sorted(
        pd.to_numeric(lmu, errors="coerce").dropna().unique()))
          + f"; {n_cf_lm} CF (counting fingers, poorer than {UDVA_PRE_WORST:.1f}, the last line of the chart at 1 m; "
            "all preoperative uncorrected)", "CF is off the chart: StandardEyes sets it to missing and counts it in the run log")
    R.add("Snellen columns", f"{snu.str.contains(r'[+-][0-9]$').mean():.1%} of the readings carry a letter suffix; "
          f"{int((snu == 'CF').sum())} are CF (counting fingers, below the 20/400 line, all preoperative uncorrected)",
          "StandardEyes reads 20/x-k as log10(x/20) + 0.02 k logMAR and +k as - 0.02 k; CF is off the chart and set to missing")
    md, sd = truth["notation_diff"]
    R.add("Agreement of the two records", f"Snellen minus logMAR line score: mean {md:+.3f}, SD {sd:.3f} logMAR "
          "(postoperative CDVA, simulated population)", "the classic Snellen lines (20/15, 20/30, 20/60, 20/70) are not "
          "0.1 logMAR apart, and the letter suffixes add letter-level detail")
    R.blank()
    R.add("TRUE DIFFERENCE", "estimand", "true value", "note")
    R.add("Postop SEQ - target, Center 2 - Center 1", "mean difference given preop SEQ",
          f"{SEQ_OUT['Center 2'] - SEQ_OUT['Center 1']:+.2f} D", "Center 2's nomogram; centers assigned at random, nothing else differs")
    R.blank()
    R.add(f"EXPECTED VALUES (population of {N_POP:,} simulated patients, seed {SEED_POP}; acuity as logMAR lines)",
          CENTERS[0], CENTERS[1], "")
    for lab, k, f in (("mean postop SEQ - target (D)", "err", "{:+.3f}"), ("SD (D)", "sd", "{:.3f}"),
                      ("within +/-0.50 D of target", "w50", "{:.1%}"), ("postop UDVA 20/20 or better", "ud20", "{:.1%}"),
                      ("efficacy index", "eff", "{:.3f}"), ("safety index", "saf", "{:.3f}")):
        R.add(lab, f.format(truth[CENTERS[0]][k]), f.format(truth[CENTERS[1]][k]), "")
    R.blank()
    n_rows, est = sample_estimates(df)
    R.add(f"THIS FILE (the {n_rows} eyes the settings analyze)", "contrast", "crude / adjusted (95% CI)",
          "linear model of the eyes, robust SE clustered by patient; adjusted for preop SEQ")
    for lab, unit, t, c, a, ne, npt in est:
        R.add(lab, unit, " / ".join(f"{e:+.3f} ({lo:+.3f} to {hi:+.3f})" for e, lo, hi in (c, a)),
              f"true {t:+.3f}; {ne} eyes of {npt} patients")
    R.blank()
    R.add("GENERATING MODEL", "parameter", "value", "meaning")
    P = [
        ("Patients", "age / female / both eyes", f"N({AGE[0]}, {AGE[1]}) rounded {AGE[2]}-{AGE[3]} / {P_FEMALE} / {P_BILATERAL}", ""),
        ("Patients", "center", f"Center 2 with probability {P_CENTER2}", "at random: no confounding"),
        ("Patients", "latent preop SEQ / pachymetry / log cylinder",
         f"N({SEQ_P_MEAN}, {SEQ_P_SD}) D / N({PACHY_P_MEAN:.0f}, {PACHY_P_SD:.0f}) µm / N(log {CYL_MEDIAN}, {CYL_LOG_P_SD})", "patient means"),
        ("Eyes", "preop SEQ", f"SEQp + N(0, {SEQ_E_SD}), limited to {SEQ_MIN} / {SEQ_MAX} D", "sphere on 0.25 D, SEQ derived"),
        ("Eyes", "cylinder", f"exp(log cyl + N(0, {CYL_LOG_E_SD})), 0.25 D steps, <= {CYL_MAX}; plus axis WTR / ATR / oblique {AXIS_TYPE_P}", "OS mirror image of OD"),
        ("Target", "monovision", f"bilateral patients aged >= {MONOVISION['min_age']}, probability {MONOVISION['p']}, one eye, "
         f"{MONOVISION['targets']} D sphere", f"only when that eye's preop SEQ <= target - {MONO_MIN_TREAT:.2f} D; otherwise plano"),
        ("Target", "planned residual cylinder", f"eyes with preop cylinder >= {RESIDUAL_CYL['min_pre_cyl']:.2f} D, probability "
         f"{RESIDUAL_CYL['p']}: {RESIDUAL_CYL['mag']:.2f} D at the preoperative steep meridian, SEQ plano", "TIA = target - PRE"),
        ("Postop SEQ", "model", f"target + {SEQ_OUT['Center 1']:+}[Center 1] {SEQ_OUT['Center 2']:+}[Center 2] "
         f"{SEQ_OUT['seq']:+}(preop SEQ+4) + u + e", f"u ~ N(0, {SEQ_OUT['sd_patient']}), e ~ N(0, {SEQ_OUT['sd_eye']})"),
        ("Astigmatism", "correction", f"SIA = CI R(2 AoE) (T - PRE); CI = {CI_MEAN} + N(0, {CI_P_SD}) + N(0, {CI_E_SD}); "
         f"AoE = N(0, {AOE_P_SD}) + N(0, {AOE_E_SD}) deg; POST = PRE + SIA + N(0, {NOISE_P_SD}) + N(0, {NOISE_E_SD})",
         "plus-format double-angle vectors, same at both centers"),
        ("Acuity", "latent logMAR", "preop CDVA, preop UDVA (CF beyond 1.6 in the logMAR record and beyond 20/400 in the "
         "Snellen record), CDVA change and postop UDVA as in dataset A "
         f"(the acuity model of the 1.1.0 example dataset); fellow-eye correlation of the CDVA change {CDVA_RHO}",
         "each measurement recorded twice: logMAR line = round(latent, 1); Snellen = nearest classic chart line + letters"),
        ("Visits", "6-month visit", f"180 +/- 25 days; outside 150-210 days in {P_OUT_OF_WINDOW:.0%}; missed by {P_MISSED_6M:.0%} of patients", ""),
        ("Missing values", "share of rows", ", ".join(f"{k} {v:.1%}" for k, v in MISSING.items())
         + f"; each postop acuity {VA_MISSING:.1%} (blank in both notations)", ""),
    ]
    for p in P:
        R.add(*p)
    R.blank()
    R.add("COLUMN DICTIONARY", "unit", "description")
    for c in COLUMNS:
        R.add(*c)
    return R


def write_settings():
    common = dict(
        group_source_var="Center",
        baseline={"continuous": ["Age_Years", "Preop_SEQ_D", "Preop_Sphere_D", "Preop_Cylinder_D", "Preop_Pachymetry_um"],
                  "ordinal": [], "dichotomous": ["Sex"], "categorical": []},
        outcome={"continuous": ["FollowUp_Days", "Postop_SEQ_D", "Postop_Cylinder_D"],
                 "ordinal": [], "dichotomous": ["Retreatment_Indicated"], "categorical": []},
        groups={"Center 1": ["Center 1"], "Center 2": ["Center 2"]}, reference_group="Center 1",
        group_colors={"Center 1": "#0072B2", "Center 2": "#D55E00"},
        std_triples=[{"sph": "Preop_Sphere_D", "cyl": "Preop_Cylinder_D", "axis": "Preop_Axis_Deg"},
                     {"sph": "Target_Sphere_D", "cyl": "Target_Cylinder_D", "axis": "Target_Axis_Deg"},
                     {"sph": "Postop_Sphere_D", "cyl": "Postop_Cylinder_D", "axis": "Postop_Axis_Deg"}],
        astig_target={"cyl": "Target_Cylinder_D", "axis": "Target_Axis_Deg"},
        dichotomous_preferred_level={"Sex": "F", "Retreatment_Indicated": "Yes"})
    out = []
    for nota, fmt, title in (("logMAR", "logmar", "logMAR columns"), ("Snellen", "snellen", "Snellen columns")):
        va = [f"Preop_UDVA_{nota}", f"Preop_CDVA_{nota}", f"Postop_UDVA_{nota}", f"Postop_CDVA_{nota}"]
        c = dict(common)
        c["baseline"] = dict(common["baseline"], continuous=common["baseline"]["continuous"] + va[:2])
        c["outcome"] = dict(common["outcome"], continuous=common["outcome"]["continuous"] + va[2:])
        cfg = sc.settings(FNAME, **c, va_format=fmt, logmar_cols_raw=va,
                          roles={"pre_ucva": va[0], "pre_bcva": va[1], "post_ucva": va[2], "post_bcva": va[3],
                                 "pre_sph": "Preop_Sphere_D", "pre_cyl": "Preop_Cylinder_D", "pre_axis": "Preop_Axis_Deg",
                                 "post_sph": "Postop_Sphere_D", "post_cyl": "Postop_Cylinder_D", "post_axis": "Postop_Axis_Deg",
                                 "pre_seq": "Preop_SEQ_D", "post_seq": "Postop_SEQ_D", "target_seq": "Target_SEQ_D", "eye": "Eye"},
                          output_prefix=f"DS_B_{nota}")
        out.append(sc.write_settings(
            f"Settings_B_notation_{nota}.json", cfg, f"Dataset B - notation harmonization ({title})",
            f"Two centers; refraction typed in mixed plus and minus cylinder notation (targets too); acuity read from the "
            f"{title} (VA notation: {nota}).",
            "The built-in harmonization: every plus cylinder (preoperative, target, postoperative) is transposed to minus "
            "cylinder before the analysis, the astigmatism vectors are built in plus-cylinder format whatever the typed "
            "form, and acuity recorded as " + ("logMAR line scores is analyzed as recorded"
                                               if fmt == "logmar" else "Snellen fractions with letter suffixes is converted "
                                               "to logMAR (counting fingers set to missing)")
            + "; tables and figures show acuity as the Snellen geometric mean with the logMAR mean and SD.",
            notes="StandardEyes applies one acuity notation (va_format) to every acuity column, so the logMAR and "
                  "the Snellen columns of this dataset are read by two settings files."))
    return out


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
    print(df.groupby("Center").agg(eyes=("EyeID", "size"), patients=("PatientID", "nunique")).to_string())
    print("plus preop rows:", (df.Preop_Notation == "plus").mean(), " plus postop rows:", (df.Postop_Notation == "plus").mean())
    for col in ("Preop_Cylinder_D", "Postop_Cylinder_D", "Target_Cylinder_D"):
        v = pd.to_numeric(df[col])
        print(col, "positive", int((v > 0).sum()), "zero", int((v == 0).sum()), "negative", int((v < 0).sum()))
    print("CF:", int((df.Preop_UDVA_Snellen == "CF").sum()))
    print(truth)
    print(sample_estimates(df))
    print("data SHA-256:", sha)
    return 0


if __name__ == "__main__":
    sys.exit(main())
