"""
DS-C "Four platforms" - simulated LASIK with four excimer platforms
("Platform 1" to "Platform 4"; demonstration dataset C of StandardEyes v1.2.0).
Simulated data - no real patients.

Demonstrates the multigroup comparison on one chart: every group in its own
color, an omnibus test per panel and each platform against the reference
(Platform 1) with Holm-adjusted P values.

  * acuity recorded as ETDRS letter scores (85 letters = logMAR 0.0 = 20/20,
    one letter = 0.02 logMAR) for UDVA and CDVA - StandardEyes reads one
    acuity notation per settings file, so every acuity column is in letters
  * documented TRUE platform profiles (accuracy, predictability, astigmatic
    correction, safety, glare/halo symptoms) - see the README sheet:
      Platform 1  reference: on target, SD of the SEQ error 0.25 D
      Platform 2  systematic undercorrection: -0.25 D
      Platform 3  less predictable: SD 0.43 D
      Platform 4  less safe (CDVA latent shift -0.60: more lines lost), less
                  cylinder correction (CI 0.90), smaller optical zone (6.0 mm),
                  more glare/halo symptoms (OR 2.5)
  * platform is chosen per patient at random (no confounding)

Usage:  python make_dataset_C.py [--out <folder>]
        (writes Dataset_C_four_platforms.xlsx and its settings files into <folder>; without --out
        into an "out" folder beside this script)
"""
from __future__ import annotations

import os
import sys

import numpy as np
import pandas as pd

import simcore as sc

FNAME = "Dataset_C_four_platforms.xlsx"
GEN = os.path.basename(__file__)
SEED = 2026111105
SEED_POP = 2026111106
N_PATIENTS = 250
N_POP = 120_000

AGE = (32.0, 8.0, 19, 56)
P_FEMALE, P_BILATERAL = 0.54, 0.85
SEQ_P_MEAN, SEQ_P_SD, SEQ_E_SD, SEQ_MIN, SEQ_MAX = -3.8, 1.7, 0.55, -9.0, -0.75
PACHY_P_MEAN, PACHY_P_SD, PACHY_E_SD = 550.0, 27.0, 6.0
CYL_MEDIAN, CYL_LOG_P_SD, CYL_LOG_E_SD, CYL_MAX = 0.70, 0.50, 0.40, 4.0
AXIS_TYPE_P = [0.62, 0.18, 0.20]
C_SEQ, C_AGE = -4.0, 32.0
PLATFORMS = ["Platform 1", "Platform 2", "Platform 3", "Platform 4"]
P_PLATFORM = [0.27, 0.25, 0.24, 0.24]
#            SEQ bias, SD patient, SD eye, CI mean, CDVA latent shift, optical zone mm, glare log OR
PROFILE = {"Platform 1": (0.00, 0.18, 0.18, 1.00, 0.00, 6.5, 0.0),
           "Platform 2": (-0.25, 0.18, 0.18, 0.98, 0.00, 6.5, 0.0),
           "Platform 3": (0.05, 0.28, 0.32, 0.97, -0.05, 6.5, float(np.log(1.3))),
           "Platform 4": (0.00, 0.20, 0.20, 0.90, -0.60, 6.0, float(np.log(2.5)))}
SEQ_SLOPE = 0.05                         # per D of preop SEQ + 4 (high myopes undercorrected)
CI_P_SD, CI_E_SD, AOE_P_SD, AOE_E_SD = 0.10, 0.04, 3.0, 6.0
NOISE_P_SD, NOISE_E_SD = 0.11, 0.06
CDVA_RHO, CDVA_SEQ_SHIFT, LETTER_NOISE = 0.75, 0.10, 0.02      # letter noise in logMAR (1 letter)
GLARE = {"intercept": -2.0, "seq": -0.15, "rho": 0.6}           # + platform log OR; per D of preop SEQ + 4
MONOVISION = {"min_age": 40, "p": 0.35, "targets": (-0.50, -0.75, -1.00), "p_targets": (0.3, 0.5, 0.2)}
P_OUT_OF_WINDOW, P_MISSED_6M = 0.03, 0.06
MISSING = {"Preop_Pachymetry_um": 0.015, "Postop_UDVA_Letters": 0.010, "Postop_CDVA_Letters": 0.015,
           "Glare_Halo_Symptoms": 0.02}


def simulate(rng, n):
    P = sc.patients(rng, n, *AGE, P_FEMALE, P_BILATERAL)
    age, female, pres = P["age"], P["female"], P["present"]
    plat = rng.choice(4, size=n, p=P_PLATFORM)
    prof = np.array([PROFILE[PLATFORMS[k]] for k in plat])            # (n, 7)
    seq_p = rng.normal(SEQ_P_MEAN, SEQ_P_SD, n)
    pachy_p = rng.normal(PACHY_P_MEAN, PACHY_P_SD, n)
    lcyl_p = rng.normal(np.log(CYL_MEDIAN), CYL_LOG_P_SD, n)
    pax, _ = sc.axes(rng, n, AXIS_TYPE_P)
    seq_lat = np.clip(seq_p[:, None] + rng.normal(0, SEQ_E_SD, (n, 2)), SEQ_MIN, SEQ_MAX)
    mag = sc.cylinder(rng, lcyl_p, CYL_LOG_E_SD, 0.0, CYL_MAX)
    sph, seq = sc.refraction_from_seq(seq_lat, mag)
    pachy = np.round(pachy_p[:, None] + rng.normal(0, PACHY_E_SD, (n, 2)))
    oz = prof[:, 5]
    depth = np.round(oz[:, None] ** 2 / 3 * (np.abs(seq) + mag / 2) + rng.normal(0, 3, (n, 2)))

    mono = (age >= MONOVISION["min_age"]) & pres.all(1) & (rng.random(n) < MONOVISION["p"])
    me = (rng.random(n) < 0.5).astype(int)
    mt = rng.choice(MONOVISION["targets"], size=n, p=MONOVISION["p_targets"])
    target = np.zeros((n, 2))
    target[np.flatnonzero(mono), me[mono]] = mt[mono]

    cd_pre = sc.cdva_pre_latent(rng, seq, n)
    ud_pre = sc.udva_pre_latent(rng, cd_pre, seq, mag, n)
    err = (prof[:, [0]] + SEQ_SLOPE * (seq - C_SEQ) + rng.normal(0, 1, (n, 1)) * prof[:, [1]]
           + rng.normal(0, 1, (n, 2)) * prof[:, [2]])
    A = sc.astig_post(rng, mag, pax, np.zeros((n, 2)), np.zeros((n, 2)), prof[:, [3]], CI_P_SD, CI_E_SD,
                      0.0, AOE_P_SD, AOE_E_SD, NOISE_P_SD, NOISE_E_SD)
    pmag, ppax, psph, pseq = sc.post_refraction(A["qx"], A["qy"], target + err, CYL_MAX)
    dl = sc.ordinal_change(rng, n, rho=CDVA_RHO, shift=CDVA_SEQ_SHIFT * (-seq - 4) + prof[:, [4]])
    cd_post = np.maximum(cd_pre - 0.1 * dl + rng.normal(0, LETTER_NOISE, (n, 2)), -0.20)
    ud_post = sc.udva_post_latent(rng, cd_post, pseq, pmag, n)
    p_gl = sc.expit(GLARE["intercept"] + prof[:, [6]] + GLARE["seq"] * (seq - C_SEQ))
    glare = sc.binary_copula(rng, n, p_gl, GLARE["rho"])

    pi, ei = np.nonzero(pres)
    f = lambda a: a[pi, ei]
    return pd.DataFrame(dict(
        pidx=pi, Eye=np.where(ei == 0, "OD", "OS"), Platform=np.array(PLATFORMS)[plat[pi]],
        Age_Years=age[pi].astype(int), Sex=np.where(female[pi], "F", "M"),
        pre_sph=f(sph), pre_mag=f(mag), pre_pax=f(pax), Preop_SEQ_D=f(seq),
        cd_pre=f(cd_pre), ud_pre=f(ud_pre), cd_post=f(cd_post), ud_post=f(ud_post), dl=f(dl),
        Preop_Pachymetry_um=f(pachy), Optical_Zone_mm=oz[pi], Ablation_Depth_um=f(depth), Target_SEQ_D=f(target),
        err=f(err), post_sph=f(psph), post_mag=f(pmag), post_pax=f(ppax), Postop_SEQ_D=f(pseq),
        post_qx=f(A["qx"]), post_qy=f(A["qy"]), glare=f(glare), ci=f(A["ci"]),
    ))


def build_dataset(seed=SEED):
    rng = np.random.default_rng(seed)
    e = simulate(rng, N_PATIENTS)
    n = len(e)
    pid = e.pidx.to_numpy()
    df = pd.DataFrame()
    df["PatientID"], df["EyeID"] = sc.ids("C", pid, e.Eye.to_numpy())
    df["Eye"] = e.Eye.to_numpy()
    for c in ("Platform", "Age_Years", "Sex"):
        df[c] = e[c].to_numpy()
    df["Preop_Sphere_D"], df["Preop_Cylinder_D"], df["Preop_Axis_Deg"] = sc.minus_triple(
        e.pre_sph.to_numpy(), e.pre_mag.to_numpy(), e.pre_pax.to_numpy())
    df["Preop_SEQ_D"] = e.Preop_SEQ_D.to_numpy()
    df["Preop_UDVA_Letters"] = sc.letters_of(e.ud_pre)
    df["Preop_CDVA_Letters"] = sc.letters_of(e.cd_pre)
    for c in ("Preop_Pachymetry_um", "Optical_Zone_mm", "Ablation_Depth_um", "Target_SEQ_D"):
        df[c] = e[c].to_numpy()
    day6 = 180 + rng.integers(-25, 26, n)
    oow = rng.random(n) < P_OUT_OF_WINDOW
    day6 = np.where(oow & (rng.random(n) < 0.5), rng.integers(125, 150, n), np.where(oow, rng.integers(211, 245, n), day6))
    df["FollowUp_Days"] = day6.astype(float)
    df["Postop_Sphere_D"], df["Postop_Cylinder_D"], df["Postop_Axis_Deg"] = sc.minus_triple(
        e.post_sph.to_numpy(), e.post_mag.to_numpy(), e.post_pax.to_numpy())
    df["Postop_SEQ_D"] = e.Postop_SEQ_D.to_numpy()
    df["Postop_UDVA_Letters"] = sc.letters_of(e.ud_post)
    df["Postop_CDVA_Letters"] = sc.letters_of(e.cd_post)
    df["Glare_Halo_Symptoms"] = np.where(e.glare, "Yes", "No")
    gone = (rng.random(pid.max() + 1) < P_MISSED_6M)[pid]
    for c in [c for c in df.columns if c.startswith("Postop_")] + ["FollowUp_Days", "Glare_Halo_Symptoms"]:
        df[c] = df[c].astype(object).where(~gone, None)
    sc.blank_out(rng, df, MISSING)
    ints = ["Age_Years", "Preop_Axis_Deg", "Postop_Axis_Deg", "Preop_Pachymetry_um", "Ablation_Depth_um",
            "FollowUp_Days", "Preop_UDVA_Letters", "Preop_CDVA_Letters", "Postop_UDVA_Letters", "Postop_CDVA_Letters"]
    floats = [c for c in df.columns if c.endswith(("_D", "_mm")) and c not in ints]
    df = sc.typed(df, ints, floats)
    return df, e


def population_truth():
    e = simulate(np.random.default_rng(SEED_POP), N_POP)
    out = {}
    for g in PLATFORMS:
        d = e[e.Platform == g]
        err = d.Postop_SEQ_D - d.Target_SEQ_D
        lm = lambda x: (85 - sc.letters_of(x)) / 50          # the recorded letters as logMAR
        ud, cdp, cdo = lm(d.ud_post), lm(d.cd_pre), lm(d.cd_post)
        lines = np.trunc(np.round((cdp - cdo) / 0.1, 6) + 0.5 * np.sign(np.round((cdp - cdo) / 0.1, 6)))
        cx, cy = d.post_qx.mean(), d.post_qy.mean()
        out[g] = dict(err=err.mean(), sd=err.std(), w50=(err.abs() <= 0.5).mean(), w100=(err.abs() <= 1.0).mean(),
                      eff=(10 ** -ud / 10 ** -cdp).mean(), saf=(10 ** -cdo / 10 ** -cdp).mean(),
                      lost1=(lines <= -1).mean(), lost2=(lines <= -2).mean(), gain1=(lines >= 1).mean(),
                      ud20=(ud <= 0.0001).mean(), cyl50=(d.post_mag <= 0.5).mean(), ci=d.ci.mean(),
                      glare=d.glare.mean(), pe=(np.hypot(cx, cy), (np.degrees(np.arctan2(cy, cx)) / 2) % 180))
    return out


def sample_estimates(df):
    d = df[pd.to_numeric(df.FollowUp_Days).between(150, 210) | df.FollowUp_Days.isna()]
    num = lambda c: pd.to_numeric(d[c]).astype(float).to_numpy()
    G = [(d.Platform == p).to_numpy(float) for p in PLATFORMS[1:]]
    cov = np.column_stack([num("Preop_SEQ_D") - C_SEQ])
    pid = d.PatientID.astype(str).to_numpy()
    rows = []
    cr, ad, ne, npt = sc.contrast(num("Postop_SEQ_D") - num("Target_SEQ_D"), G, cov, pid)
    for j, p in enumerate(PLATFORMS[1:]):
        rows.append((f"Postop SEQ - target, {p} - Platform 1", "D", PROFILE[p][0], cr[j], ad[j], ne, npt))
    gl = d.Glare_Halo_Symptoms.map({"Yes": 1.0, "No": 0.0}).astype(float).to_numpy()
    cr, ad, ne, npt = sc.contrast(gl, G, cov, pid, logistic=True)
    for j, p in enumerate(PLATFORMS[1:]):
        rows.append((f"Glare/halo symptoms, {p} vs Platform 1", "odds ratio", float(np.exp(PROFILE[p][6])), cr[j], ad[j], ne, npt))
    return len(d), rows


COLUMNS = [
    ("PatientID", "-", "Patient code (no names). Both eyes of a patient share it."),
    ("EyeID", "-", "Eye code = PatientID-Eye."),
    ("Eye", "-", "OD (right) or OS (left)."),
    ("Platform", "-", "Excimer laser platform used for LASIK: Platform 1, 2, 3 or 4 (patient level)."),
    ("Age_Years", "years", "Age at surgery (patient level)."),
    ("Sex", "-", "F or M (patient level)."),
    ("Preop_Sphere_D / _Cylinder_D / _Axis_Deg", "D / D / degrees", "Preoperative manifest refraction, minus-cylinder notation, 0.25 D steps."),
    ("Preop_SEQ_D", "D", "Preoperative SEQ = sphere + cylinder/2 exactly."),
    ("Preop_UDVA_Letters / Preop_CDVA_Letters", "ETDRS letters", "Preoperative uncorrected / corrected distance acuity as an "
     "ETDRS letter score (0-100; 85 = 20/20 = logMAR 0.0; logMAR = 1.7 - 0.02 x letters)."),
    ("Preop_Pachymetry_um", "µm", "Central corneal thickness."),
    ("Optical_Zone_mm", "mm", "Optical zone (6.5 mm on Platforms 1-3, 6.0 mm on Platform 4)."),
    ("Ablation_Depth_um", "µm", "Maximum ablation depth."),
    ("Target_SEQ_D", "D", "Intended postoperative SEQ: plano, or a monovision target in one eye of some patients aged 40+."),
    ("FollowUp_Days", "days", "Days from surgery to the 6-month (primary endpoint) visit; blank when that visit was missed."),
    ("Postop_Sphere_D / _Cylinder_D / _Axis_Deg", "D / D / degrees", "Refraction at 6 months, minus notation."),
    ("Postop_SEQ_D", "D", "SEQ at 6 months = sphere + cylinder/2 exactly."),
    ("Postop_UDVA_Letters / Postop_CDVA_Letters", "ETDRS letters", "Acuity at 6 months, ETDRS letter score (at most 95 = 20/12.5 for CDVA)."),
    ("Glare_Halo_Symptoms", "-", "Glare or halo symptoms at 6 months: Yes or No."),
]


def readme(df, truth, sha):
    R = sc.Readme()
    R.add("Simulated data - no real patients")
    R.add("Every value in the sheet 'Surgeries' was generated by a computer model. No person, clinic, laser or real "
          "measurement is represented; the platforms are fictitious and numbered. Patients and eyes carry codes only.")
    R.add("Dataset C of the StandardEyes demonstration datasets: 'Four platforms' - four excimer platforms with documented "
          "true differences in accuracy and safety, acuity recorded as ETDRS letter scores.")
    R.blank()
    sc.source_rows(R, GEN, SEED, df, sha, f"population table: seed {SEED_POP}")
    R.add("Size", f"{df.PatientID.nunique()} patients, {len(df)} eyes (one row per eye)",
          "; ".join(f"{g}: {len(d)} eyes of {d.PatientID.nunique()} patients" for g, d in df.groupby("Platform")))
    R.add("Settings files", "Settings_C_four_platforms.json", "four groups on every chart, reference Platform 1")
    R.blank()
    R.add("NOTATION")
    R.add("Refraction", "minus-cylinder notation; SEQ = sphere + cylinder/2 exactly")
    R.add("Acuity", "ETDRS letter scores for UDVA and CDVA: 85 letters = 20/20 = logMAR 0.0, 5 letters per 0.1-logMAR line",
          "StandardEyes reads one acuity notation per settings file, so all acuity columns are letters")
    R.blank()
    R.add("TRUE PLATFORM PROFILES", "parameter", "value", "note")
    for p in PLATFORMS:
        b, sp, se, ci, sh, oz, lor = PROFILE[p]
        R.add(p, "SEQ error bias / SD (patient, eye)", f"{b:+.2f} D / {sp:.2f}, {se:.2f} D (total {np.hypot(sp, se):.2f} D)",
              f"correction index mean {ci:.2f}; CDVA latent shift {sh:+.2f}; optical zone {oz} mm; glare/halo OR {np.exp(lor):.1f}")
    R.blank()
    R.add(f"EXPECTED VALUES (population of {N_POP:,} simulated patients, seed {SEED_POP})", "P1 / P2 / P3 / P4", "", "")
    for lab, k, f in (("mean postop SEQ - target (D)", "err", "{:+.2f}"), ("SD of postop SEQ - target (D)", "sd", "{:.2f}"),
                      ("within +/-0.50 D", "w50", "{:.1%}"), ("within +/-1.00 D", "w100", "{:.1%}"),
                      ("postop UDVA 20/20 or better", "ud20", "{:.1%}"), ("efficacy index", "eff", "{:.2f}"),
                      ("safety index", "saf", "{:.2f}"), ("CDVA: 1 or more lines lost", "lost1", "{:.1%}"),
                      ("CDVA: 2 or more lines lost", "lost2", "{:.1%}"), ("CDVA: 1 or more lines gained", "gain1", "{:.1%}"),
                      ("postop cylinder <= 0.50 D", "cyl50", "{:.1%}"), ("correction index (mean)", "ci", "{:.2f}"),
                      ("glare/halo symptoms", "glare", "{:.1%}")):
        R.add(lab, " / ".join(f.format(truth[p][k]) for p in PLATFORMS))
    R.add("prediction-error centroid", " / ".join(f"{truth[p]['pe'][0]:.2f} D @ {truth[p]['pe'][1]:.0f}" for p in PLATFORMS),
          "", "plus format, before rounding; residual with-the-rule astigmatism where the correction index is below 1")
    R.blank()
    n_rows, est = sample_estimates(df)
    R.add(f"THIS FILE (the {n_rows} eyes the settings analyze)", "unit", "crude / adjusted (95% CI)",
          "linear or logistic model of the eyes, robust SE clustered by patient; adjusted for preop SEQ; not Holm-adjusted")
    for lab, unit, t, c, a, ne, npt in est:
        odds = unit.startswith("odds")
        f = (lambda v: f"{v:.2f}") if odds else (lambda v: f"{v:+.3f}")
        R.add(lab, unit, " / ".join(f"{f(e)} ({f(lo)} to {f(hi)})" for e, lo, hi in (c, a)), f"true {f(t)}; {ne} eyes of {npt} patients")
    R.blank()
    R.add("GENERATING MODEL", "parameter", "value", "meaning")
    P = [
        ("Patients", "age / female / both eyes", f"N({AGE[0]}, {AGE[1]}) rounded {AGE[2]}-{AGE[3]} / {P_FEMALE} / {P_BILATERAL}", ""),
        ("Patients", "platform", " / ".join(f"{p} {q}" for p, q in zip(PLATFORMS, P_PLATFORM)), "at random: no confounding"),
        ("Patients", "latent preop SEQ / pachymetry / log cylinder",
         f"N({SEQ_P_MEAN}, {SEQ_P_SD}) D / N({PACHY_P_MEAN:.0f}, {PACHY_P_SD:.0f}) µm / N(log {CYL_MEDIAN}, {CYL_LOG_P_SD})", "patient means"),
        ("Eyes", "preop SEQ", f"SEQp + N(0, {SEQ_E_SD}), limited to {SEQ_MIN} / {SEQ_MAX} D", "sphere on 0.25 D, SEQ derived"),
        ("Eyes", "cylinder", f"exp(log cyl + N(0, {CYL_LOG_E_SD})), 0.25 D steps, <= {CYL_MAX}; plus axis WTR / ATR / oblique {AXIS_TYPE_P}", "OS mirror image of OD"),
        ("Eyes", "depth", "OZ^2/3 (|SEQ| + |cyl|/2) + N(0, 3) µm", "Munnerlyn"),
        ("Target", "monovision", f"bilateral patients aged >= {MONOVISION['min_age']}, probability {MONOVISION['p']}, one eye, {MONOVISION['targets']} D", "others plano"),
        ("Postop SEQ", "model", f"target + bias[platform] {SEQ_SLOPE:+}(preop SEQ+4) + u + e", "u, e ~ N(0, the platform's SDs)"),
        ("Astigmatism", "correction", f"SIA = -CI R(2 AoE) PRE; CI = platform mean + N(0, {CI_P_SD}) + N(0, {CI_E_SD}); AoE = N(0, {AOE_P_SD}) + "
         f"N(0, {AOE_E_SD}) deg; POST = PRE + SIA + N(0, {NOISE_P_SD}) + N(0, {NOISE_E_SD}) per component", "plus-format double-angle vectors"),
        ("CDVA", "lines gained (+) / lost (-)", ", ".join(f"{k:+d}: {v:.1%}" for k, v in sc.CDVA_CHANGE_P.items()),
         f"at preop SEQ -4 D and platform shift 0; latent shift {CDVA_SEQ_SHIFT}(-preop SEQ-4) + platform shift; fellow-eye correlation "
         f"{CDVA_RHO}; postop latent + N(0, {LETTER_NOISE}) logMAR (about 1 letter); 95 letters at best"),
        ("UDVA", "postop", "CDVA + blur of the residual refraction (dataset A model)", "never better than CDVA"),
        ("Acuity", "recording", "letters = round(85 - 50 x latent logMAR), 0-100", "letter-level, not whole lines"),
        ("Glare / halo", "P(yes)", f"expit({GLARE['intercept']} + platform log OR {GLARE['seq']:+}(preop SEQ+4))",
         f"Gaussian copula, fellow-eye latent correlation {GLARE['rho']}: the ORs are exact population-averaged values"),
        ("Visits", "6-month visit", f"180 +/- 25 days; outside 150-210 days in {P_OUT_OF_WINDOW:.0%}; missed by {P_MISSED_6M:.0%} of patients", ""),
        ("Missing values", "share of rows", ", ".join(f"{k} {v:.1%}" for k, v in MISSING.items()), ""),
    ]
    for p in P:
        R.add(*p)
    R.blank()
    R.add("COLUMN DICTIONARY", "unit", "description")
    for c in COLUMNS:
        R.add(*c)
    return R


def write_settings():
    va = ["Preop_UDVA_Letters", "Preop_CDVA_Letters", "Postop_UDVA_Letters", "Postop_CDVA_Letters"]
    cfg = sc.settings(
        FNAME, group_source_var="Platform",
        baseline={"continuous": ["Age_Years", "Preop_SEQ_D", "Preop_Sphere_D", "Preop_Cylinder_D", va[0], va[1],
                                 "Preop_Pachymetry_um"], "ordinal": [], "dichotomous": ["Sex"], "categorical": []},
        preop={"continuous": ["Optical_Zone_mm", "Ablation_Depth_um"], "ordinal": [], "dichotomous": [], "categorical": []},
        outcome={"continuous": ["FollowUp_Days", "Postop_SEQ_D", "Postop_Cylinder_D", va[2], va[3]],
                 "ordinal": [], "dichotomous": ["Glare_Halo_Symptoms"], "categorical": []},
        groups={p: [p] for p in PLATFORMS}, reference_group="Platform 1",
        group_colors={"Platform 1": "#5B2C86", "Platform 2": "#F26522", "Platform 3": "#1B9E4B", "Platform 4": "#1F78B4"},
        run_gee_stage="unmatched", run_main_gee=True,
        va_format="etdrs", logmar_cols_raw=va,
        roles={"pre_ucva": va[0], "pre_bcva": va[1], "post_ucva": va[2], "post_bcva": va[3],
               "pre_sph": "Preop_Sphere_D", "pre_cyl": "Preop_Cylinder_D", "pre_axis": "Preop_Axis_Deg",
               "post_sph": "Postop_Sphere_D", "post_cyl": "Postop_Cylinder_D", "post_axis": "Postop_Axis_Deg",
               "pre_seq": "Preop_SEQ_D", "post_seq": "Postop_SEQ_D", "target_seq": "Target_SEQ_D", "eye": "Eye"},
        dichotomous_preferred_level={"Sex": "F", "Glare_Halo_Symptoms": "Yes"},
        output_prefix="DS_C_Four_platforms")
    return [sc.write_settings(
        "Settings_C_four_platforms.json", cfg, "Dataset C - four platforms",
        "Four groups (Platform 1 to 4) on every chart, Platform 1 the reference; acuity recorded as ETDRS letters.",
        "The multigroup comparison: every platform in its own color on one chart, an overall (omnibus) test per panel, "
        "each platform against the reference with Holm-adjusted P values, patient-clustered GEE in the tables, and "
        "ETDRS letter scores converted to logMAR.")]


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
    print(df.groupby("Platform").agg(eyes=("EyeID", "size"), patients=("PatientID", "nunique")).to_string())
    for p in PLATFORMS:
        print(p, {k: (round(v, 3) if not isinstance(v, tuple) else tuple(round(x, 2) for x in v)) for k, v in truth[p].items()})
    for r in sample_estimates(df)[1]:
        print(r[0], r[2], tuple(round(x, 3) for x in r[3]))
    print("data SHA-256:", sha)
    return 0


if __name__ == "__main__":
    sys.exit(main())
