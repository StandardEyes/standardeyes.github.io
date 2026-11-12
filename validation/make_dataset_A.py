"""
DS-A "Two techniques" - simulated PRK versus LASIK (demonstration dataset A of
StandardEyes v1.2.0). Simulated data - no real patients.

Demonstrates the basic workflow: two groups, the standard graphs, the
publication table, the mEYEstro and AstigMATIC compatible sheets, the
stability-over-time curve and the preview-only run mode.

  * PRK and LASIK only (no KLEx), decimal acuity on 0.1-logMAR chart lines,
    every refraction in MINUS-cylinder notation
  * the 6-month visit is the primary endpoint; visits at 1, 3, 6 and 12 months
    with monotone attrition; PRK has a larger early hyperopic overshoot
  * documented true differences at 6 months: postop SEQ PRK - LASIK = +0.14 D
    (given age and preop SEQ), CDVA latent shift -0.15 for PRK (slightly more
    line losses), dry-eye symptoms OR 1.8 for LASIK (exact, population averaged)
  * procedure choice depends on corneal thickness and on the tissue a flap
    would leave (LASIK needs a residual bed >= 280 µm and <= 40% tissue altered),
    so high myopes and thin corneas get PRK: the crude SEQ contrast is mildly
    confounded by preop SEQ

Usage:  python make_dataset_A.py [--out <folder>]
        (writes Dataset_A_two_techniques.xlsx and its settings files into <folder>; without --out
        into an "out" folder beside this script)
"""
from __future__ import annotations

import os
import sys

import numpy as np
import pandas as pd

import simcore as sc

FNAME = "Dataset_A_two_techniques.xlsx"
GEN = os.path.basename(__file__)
SEED = 2026111101
SEED_POP = 2026111102
N_PATIENTS = 215
N_POP = 100_000

AGE = (31.0, 7.0, 19, 55)
P_FEMALE, P_BILATERAL = 0.55, 0.85
SEQ_P_MEAN, SEQ_P_SD, SEQ_E_SD, SEQ_MIN, SEQ_MAX = -3.6, 1.7, 0.55, -9.0, -0.75
PACHY_P_MEAN, PACHY_P_SD, PACHY_E_SD = 545.0, 28.0, 6.0
K_MEAN, K_P_SD, K_E_SD = 43.6, 1.4, 0.3
CYL_MEDIAN, CYL_LOG_P_SD, CYL_LOG_E_SD, CYL_MAX = 0.65, 0.50, 0.40, 4.0
AXIS_TYPE_P = [0.62, 0.18, 0.20]
OZ_MEAN, OZ_PER_D, OZ_SD = 6.5, 0.06, 0.12
C_AGE, C_SEQ, C_PACHY = 31.0, -4.0, 545.0

PROCS = ["PRK", "LASIK"]
PRK_LOGIT = (-0.20, -0.45)          # logit PRK vs LASIK: intercept, per 10 µm patient pachymetry
FLAP_UM, RSB_MIN_UM, PTA_MAX, PRK_REMAIN_MIN_UM = 110.0, 280.0, 0.40, 400.0
MONOVISION = {"min_age": 40, "p": 0.40, "targets": (-0.50, -0.75, -1.00), "p_targets": (0.3, 0.5, 0.2)}
MONO_MIN_TREAT = 0.75               # D: a monovision eye is treated by at least this much (preop SEQ <= target - 0.75)

# 6-month outcomes
SEQ_OUT = {"PRK": 0.12, "LASIK": -0.02, "seq": 0.05, "age": 0.008, "sd_patient": 0.22, "sd_eye": 0.20}
CI_MEAN, CI_P_SD, CI_E_SD = 0.95, 0.10, 0.04
AOE_P_SD, AOE_E_SD = 3.0, 6.0
NOISE_P_SD, NOISE_E_SD = 0.11, 0.06
CDVA_RHO, CDVA_SEQ_SHIFT, CDVA_PRK_SHIFT = 0.75, 0.10, -0.15
DRY = {"intercept": -1.75, "LASIK": float(np.log(1.8)), "age": 0.03, "seq": -0.10, "rho": 0.60}
HAZE_RHO, HAZE_PER_UM, HAZE_PRK_CUM, HAZE_LASIK_P1 = 0.5, 0.012, (0.78, 0.96, 0.992, 0.999), 0.01
COMPLICATION_P = {"PRK": {"Delayed epithelial healing": 0.04},
                  "LASIK": {"Diffuse lamellar keratitis": 0.025, "Flap striae": 0.015,
                            "Epithelial ingrowth": 0.010}}

# visits: number, nominal day, jitter (days); visit 3 is the primary endpoint
VISITS = [(1, 30, 7), (2, 90, 14), (3, 180, 25), (4, 365, 30)]
ATTEND = [1.00, 0.96, 0.91, 0.74]
P_OUT_OF_WINDOW = 0.03
OVERSHOOT = {"PRK": (0.45, 75.0), "LASIK": (0.15, 45.0)}   # D at day 0, time constant (days)
REGRESSION = (-0.08, 0.02)                                 # D/year: intercept, per D of preop SEQ + 4
VISIT_SD_PATIENT, VISIT_SD_EYE, VISIT_VEC_SD = 0.06, 0.10, 0.08
MISSING = {"Preop_Pachymetry_um": 0.015, "Preop_Mean_K_D": 0.015, "Postop_UDVA": 0.010,
           "Postop_CDVA": 0.015, "DryEye_Symptoms": 0.03}


def simulate(rng, n_patients, batch=1.8):
    m = int(n_patients * batch) + 50
    P = sc.patients(rng, m, *AGE, P_FEMALE, P_BILATERAL)
    seq_p = rng.normal(SEQ_P_MEAN, SEQ_P_SD, m)
    pachy_p = rng.normal(PACHY_P_MEAN, PACHY_P_SD, m)
    k_p = rng.normal(K_MEAN, K_P_SD, m)
    lcyl_p = rng.normal(np.log(CYL_MEDIAN), CYL_LOG_P_SD, m)
    pax, _ = sc.axes(rng, m, AXIS_TYPE_P)
    proc = (rng.random(m) >= sc.expit(PRK_LOGIT[0] + PRK_LOGIT[1] * (pachy_p - C_PACHY) / 10)).astype(int)  # 0 PRK

    seq_lat = np.clip(seq_p[:, None] + rng.normal(0, SEQ_E_SD, (m, 2)), SEQ_MIN, SEQ_MAX)
    mag = sc.cylinder(rng, lcyl_p, CYL_LOG_E_SD, 0.0, CYL_MAX)
    sph, seq = sc.refraction_from_seq(seq_lat, mag)
    pachy = np.round(pachy_p[:, None] + rng.normal(0, PACHY_E_SD, (m, 2)))
    kk = np.round(k_p[:, None] + rng.normal(0, K_E_SD, (m, 2)), 2)
    oz = np.clip(np.round((OZ_MEAN - OZ_PER_D * (-seq_p - 4) + rng.normal(0, OZ_SD, m)) * 10) / 10, 5.8, 7.0)
    depth = oz[:, None] ** 2 / 3 * (np.abs(seq) + mag / 2) + rng.normal(0, 3, (m, 2))
    pres = P["present"]
    flap_ok = (((pachy - FLAP_UM - depth >= RSB_MIN_UM) & ((FLAP_UM + depth) / pachy <= PTA_MAX)) | ~pres).all(1)
    proc[(proc == 1) & ~flap_ok] = 0
    eligible = (((pachy - depth >= PRK_REMAIN_MIN_UM) | ~pres).all(1)) | (proc == 1)
    keep = np.flatnonzero(eligible)[:n_patients]
    if len(keep) < n_patients:
        raise RuntimeError("too few eligible candidates")
    n = n_patients
    age, female, pres = P["age"][keep], P["female"][keep], pres[keep]
    proc, oz, seq_p = proc[keep], oz[keep], seq_p[keep]
    sph, seq, mag, pax = sph[keep], seq[keep], mag[keep], pax[keep]
    pachy, kk, depth = pachy[keep], kk[keep], np.round(depth[keep])
    isP = (proc == 0)[:, None]

    # target: plano, or myopic monovision in one eye of some bilateral patients aged 40+ - only
    # when that eye's preop SEQ is at least MONO_MIN_TREAT more myopic than the target (otherwise plano)
    mono = (age >= MONOVISION["min_age"]) & pres.all(1) & (rng.random(n) < MONOVISION["p"])
    mono_eye = (rng.random(n) < 0.5).astype(int)
    mono_t = rng.choice(MONOVISION["targets"], size=n, p=MONOVISION["p_targets"])
    mono &= seq[np.arange(n), mono_eye] <= mono_t - MONO_MIN_TREAT
    target = np.zeros((n, 2))
    target[np.flatnonzero(mono), mono_eye[mono]] = mono_t[mono]

    cd_pre = sc.cdva_pre_latent(rng, seq, n)
    ud_pre = sc.udva_pre_latent(rng, cd_pre, seq, mag, n)

    err = (np.where(isP, SEQ_OUT["PRK"], SEQ_OUT["LASIK"]) + SEQ_OUT["seq"] * (seq - C_SEQ)
           + SEQ_OUT["age"] * (age - C_AGE)[:, None]
           + rng.normal(0, SEQ_OUT["sd_patient"], (n, 1)) + rng.normal(0, SEQ_OUT["sd_eye"], (n, 2)))
    post_lat = target + err
    A = sc.astig_post(rng, mag, pax, np.zeros((n, 2)), np.zeros((n, 2)), CI_MEAN, CI_P_SD, CI_E_SD,
                      0.0, AOE_P_SD, AOE_E_SD, NOISE_P_SD, NOISE_E_SD)
    pmag, ppax, psph, pseq = sc.post_refraction(A["qx"], A["qy"], post_lat, CYL_MAX)

    dl = sc.ordinal_change(rng, n, rho=CDVA_RHO,
                           shift=CDVA_SEQ_SHIFT * (-seq - 4) + np.where(isP, CDVA_PRK_SHIFT, 0.0))
    cd_pre_line = np.clip(sc.line_of(cd_pre), -2, 3)
    cd_post_line = np.maximum(cd_pre_line - dl, -2)
    ud_pre_line = np.maximum(np.maximum(sc.line_of(ud_pre), cd_pre_line), -2)      # beyond line 13: recorded as CF
    ud_post = sc.udva_post_latent(rng, cd_post_line / 10, pseq, pmag, n)
    ud_post_line = np.clip(np.maximum(sc.line_of(ud_post), cd_post_line), -2, 17)

    zh = sc.correlated_normal(rng, n, HAZE_RHO) + HAZE_PER_UM * (depth - 60)
    haze = np.digitize(zh, sc.norm_ppf(np.array(HAZE_PRK_CUM)))
    haze = np.where(isP, haze, (rng.random((n, 2)) < HAZE_LASIK_P1).astype(int))
    p_dry = sc.expit(DRY["intercept"] + DRY["LASIK"] * (~isP) + DRY["age"] * (age - C_AGE)[:, None]
                     + DRY["seq"] * (seq - C_SEQ))
    dry = sc.binary_copula(rng, n, p_dry, DRY["rho"])
    comp = np.full((n, 2), "No complication", dtype=object)
    u = rng.random((n, 2))
    for pi_, pname in enumerate(PROCS):
        acc = np.zeros((n, 2))
        for lev, p in COMPLICATION_P[pname].items():
            hit = (proc[:, None] == pi_) & (u >= acc) & (u < acc + p)
            comp[hit] = lev
            acc = acc + p
    comp[isP.repeat(2, 1) & (haze >= 2)] = "Corneal haze"

    pi, ei = np.nonzero(pres)
    return pd.DataFrame(dict(
        pidx=pi, Eye=np.where(ei == 0, "OD", "OS"), Procedure=np.array(PROCS)[proc[pi]],
        Age_Years=age[pi].astype(int), Sex=np.where(female[pi], "F", "M"),
        pre_sph=sph[pi, ei], pre_mag=mag[pi, ei], pre_pax=pax[pi, ei], Preop_SEQ_D=seq[pi, ei],
        pre_ud=ud_pre_line[pi, ei], pre_cd=cd_pre_line[pi, ei],
        Preop_Pachymetry_um=pachy[pi, ei], Preop_Mean_K_D=kk[pi, ei], Optical_Zone_mm=oz[pi],
        Ablation_Depth_um=depth[pi, ei], Target_SEQ_D=target[pi, ei],
        post_lat=post_lat[pi, ei], err=err[pi, ei],
        post_sph=psph[pi, ei], post_mag=pmag[pi, ei], post_pax=ppax[pi, ei], Postop_SEQ_D=pseq[pi, ei],
        post_qx=A["qx"][pi, ei], post_qy=A["qy"][pi, ei],
        post_ud=ud_post_line[pi, ei], post_cd=cd_post_line[pi, ei], cdva_lines=dl[pi, ei],
        Postop_Haze_Grade=haze[pi, ei], dry=dry[pi, ei], Complication_Type=comp[pi, ei],
    ))


def build_dataset(seed=SEED):
    rng = np.random.default_rng(seed)
    eyes = simulate(rng, N_PATIENTS)
    n = len(eyes)
    pid = eyes.pidx.to_numpy()
    npat = pid.max() + 1
    att = np.asarray(ATTEND)
    last = rng.choice(np.arange(1, len(VISITS) + 1), size=npat, p=np.append(att[:-1] - att[1:], att[-1]))[pid]

    df = pd.DataFrame()
    df["PatientID"], df["EyeID"] = sc.ids("A", pid, eyes.Eye.to_numpy())
    df["Eye"] = eyes.Eye.to_numpy()
    for c in ("Procedure", "Age_Years", "Sex"):
        df[c] = eyes[c].to_numpy()
    df["Preop_Sphere_D"], df["Preop_Cylinder_D"], df["Preop_Axis_Deg"] = sc.minus_recorded(
        eyes.pre_sph.to_numpy(), eyes.pre_mag.to_numpy(), eyes.pre_pax.to_numpy())
    df["Preop_SEQ_D"] = eyes.Preop_SEQ_D.to_numpy()
    df["Preop_UDVA"], df["Preop_CDVA"] = sc.dec_or_off(eyes.pre_ud), sc.dec_of(eyes.pre_cd)
    for c in ("Preop_Pachymetry_um", "Preop_Mean_K_D", "Optical_Zone_mm", "Ablation_Depth_um", "Target_SEQ_D"):
        df[c] = eyes[c].to_numpy()

    day6 = 180 + rng.integers(-VISITS[2][2], VISITS[2][2] + 1, n)
    oow = rng.random(n) < P_OUT_OF_WINDOW
    early = rng.random(n) < 0.5
    day6 = np.where(oow & early, rng.integers(125, 150, n), np.where(oow, rng.integers(211, 245, n), day6))
    df["FollowUp_Days"] = day6.astype(float)
    df["Postop_Sphere_D"], df["Postop_Cylinder_D"], df["Postop_Axis_Deg"] = sc.minus_recorded(
        eyes.post_sph.to_numpy(), eyes.post_mag.to_numpy(), eyes.post_pax.to_numpy())
    df["Postop_SEQ_D"] = eyes.Postop_SEQ_D.to_numpy()
    df["Postop_UDVA"], df["Postop_CDVA"] = sc.dec_of(eyes.post_ud), sc.dec_of(eyes.post_cd)
    df["Postop_Haze_Grade"] = eyes.Postop_Haze_Grade.to_numpy()
    df["DryEye_Symptoms"] = np.where(eyes.dry, "Yes", "No")
    df["Complication_Type"] = eyes.Complication_Type.to_numpy()

    preseq = eyes.Preop_SEQ_D.to_numpy()
    over = np.array([OVERSHOOT[p] for p in eyes.Procedure])
    rate = REGRESSION[0] + REGRESSION[1] * (preseq - C_SEQ)
    drift = lambda d: over[:, 0] * np.exp(-d / over[:, 1]) + rate * d / 365.0
    pat_visit = rng.normal(0, VISIT_SD_PATIENT, (npat, len(VISITS)))[pid]
    vcols = []
    for k, (vn, day, jit) in enumerate(VISITS):
        if vn == 3:
            sph, cyl, ax = (df[c].to_numpy() for c in ("Postop_Sphere_D", "Postop_Cylinder_D", "Postop_Axis_Deg"))
            seq, days = df.Postop_SEQ_D.to_numpy(), day6.astype(float)
        else:
            days = (day + rng.integers(-jit, jit + 1, n)).astype(float)
            lat = (eyes.post_lat.to_numpy() + drift(days) - drift(day6.astype(float))
                   + pat_visit[:, k] + rng.normal(0, VISIT_SD_EYE, n))
            vx = eyes.post_qx.to_numpy() + rng.normal(0, VISIT_VEC_SD, n)
            vy = eyes.post_qy.to_numpy() + rng.normal(0, VISIT_VEC_SD, n)
            mag = np.minimum(sc.q25(np.hypot(vx, vy)), CYL_MAX)
            sph, seq = sc.refraction_from_seq(lat, mag)
            cyl = -mag + 0.0
            ax = sc.wrap_axis(np.degrees(np.arctan2(vy, vx)) / 2 + 90).astype(float)
            ax[mag == 0] = np.nan                  # a zero cylinder has a blank axis
        gone = last < vn
        for name, v in ((f"Visit{vn}_Days_PostOp", days), (f"Visit{vn}_Sphere_D", sph),
                        (f"Visit{vn}_Cylinder_D", cyl), (f"Visit{vn}_Axis_Deg", ax), (f"Visit{vn}_SEQ_D", seq)):
            v = np.asarray(v, float).copy()
            v[gone] = np.nan
            df[name] = v
            vcols.append(name)
    no6 = last < 3
    for col in ("FollowUp_Days", "Postop_Sphere_D", "Postop_Cylinder_D", "Postop_Axis_Deg", "Postop_SEQ_D",
                "Postop_UDVA", "Postop_CDVA", "Postop_Haze_Grade", "DryEye_Symptoms"):
        df[col] = df[col].astype(object).where(~no6, np.nan)
    sc.blank_out(rng, df, MISSING)
    ints = ["Age_Years", "Preop_Axis_Deg", "Preop_Pachymetry_um", "Ablation_Depth_um", "FollowUp_Days",
            "Postop_Axis_Deg", "Postop_Haze_Grade"] + [c for c in vcols if c.endswith(("_Days_PostOp", "_Axis_Deg"))]
    # Preop_UDVA keeps its "CF" cells (off the chart) beside the decimal numbers
    floats = [c for c in df.columns if c.endswith(("_D", "_um", "_mm", "VA")) and c not in ints and c != "Preop_UDVA"]
    df = sc.typed(df, ints, floats)
    return df, eyes


# ---------------------------------------------------------------------------
def population_truth():
    pop = simulate(np.random.default_rng(SEED_POP), N_POP)
    out = {}
    for g in PROCS:
        d = pop[pop.Procedure == g]
        e = d.Postop_SEQ_D - d.Target_SEQ_D
        ud = sc.dec_of(d.post_ud); cdp = sc.dec_of(d.pre_cd); cdo = sc.dec_of(d.post_cd)
        out[g] = dict(share=len(d) / len(pop), err=e.mean(), sd=e.std(), w50=(e.abs() <= 0.5).mean(),
                      w100=(e.abs() <= 1.0).mean(), eff=(ud / cdp).mean(), saf=(cdo / cdp).mean(),
                      lost2=(d.cdva_lines <= -2).mean(), gain1=(d.cdva_lines >= 1).mean(),
                      ud20=(d.post_ud <= 0).mean(), cyl50=(d.post_mag <= 0.5).mean(), dry=d.dry.mean(),
                      seq=d.Preop_SEQ_D.mean(), age=d.Age_Years.mean(), haze1=(d.Postop_Haze_Grade >= 1).mean())
    return out


def sample_estimates(df):
    d = df[pd.to_numeric(df.FollowUp_Days).between(150, 210) | df.FollowUp_Days.isna()]
    num = lambda c: pd.to_numeric(d[c]).astype(float).to_numpy()
    L = (d.Procedure == "LASIK").to_numpy(float)
    cov = np.column_stack([num("Age_Years") - C_AGE, num("Preop_SEQ_D") - C_SEQ])
    pid = d.PatientID.astype(str).to_numpy()
    dry = d.DryEye_Symptoms.map({"Yes": 1.0, "No": 0.0}).astype(float).to_numpy()
    rows = []
    cr, ad, ne, npt = sc.contrast(num("Postop_SEQ_D") - num("Target_SEQ_D"), [L], cov, pid)
    rows.append(("Postop SEQ - target (6 months)", "D, LASIK - PRK", SEQ_OUT["LASIK"] - SEQ_OUT["PRK"], cr[0], ad[0], ne, npt))
    cr, ad, ne, npt = sc.contrast(dry, [L], cov, pid, logistic=True)
    rows.append(("Dry-eye symptoms (6 months)", "odds ratio, LASIK vs PRK", 1.8, cr[0], ad[0], ne, npt))
    cr, ad, ne, npt = sc.contrast(-np.log10(num("Postop_CDVA")) + np.log10(num("Preop_CDVA")), [L], cov, pid)
    rows.append(("Change in CDVA (logMAR, postop - preop)", "logMAR, LASIK - PRK", None, cr[0], ad[0], ne, npt))
    return len(d), rows


COLUMNS = [
    ("PatientID", "-", "Patient code (no names). Both eyes of a patient share it."),
    ("EyeID", "-", "Eye code = PatientID-Eye."),
    ("Eye", "-", "OD (right) or OS (left)."),
    ("Procedure", "-", "PRK or LASIK (patient level: both eyes of a patient had the same procedure)."),
    ("Age_Years", "years", "Age at surgery (patient level)."),
    ("Sex", "-", "F or M (patient level)."),
    ("Preop_Sphere_D", "D", "Preoperative manifest sphere, minus-cylinder notation, 0.25 D steps."),
    ("Preop_Cylinder_D", "D", "Preoperative manifest cylinder, minus notation (0 or negative), 0.25 D steps."),
    ("Preop_Axis_Deg", "degrees", "Axis of the minus cylinder (1-180); blank when the cylinder is 0."),
    ("Preop_SEQ_D", "D", "Preoperative spherical equivalent = sphere + cylinder/2 exactly."),
    ("Preop_UDVA", "decimal", "Preoperative uncorrected distance acuity, decimal value of a 0.1-logMAR chart line; "
     "CF (counting fingers) when poorer than the chart's last line (0.05 = 20/400)."),
    ("Preop_CDVA", "decimal", "Preoperative corrected distance acuity, decimal (chart line)."),
    ("Preop_Pachymetry_um", "µm", "Central corneal thickness."),
    ("Preop_Mean_K_D", "D", "Mean keratometry."),
    ("Optical_Zone_mm", "mm", "Optical zone (patient level)."),
    ("Ablation_Depth_um", "µm", "Maximum ablation depth."),
    ("Target_SEQ_D", "D", "Intended postoperative SEQ: plano, or a myopic monovision target in one eye of some patients aged 40+ "
     "(only in an eye whose preop SEQ is at least 0.75 D more myopic than the target)."),
    ("FollowUp_Days", "days", "Days from surgery to the 6-month (primary endpoint) visit; blank when that visit was missed."),
    ("Postop_Sphere_D / _Cylinder_D / _Axis_Deg", "D / D / degrees", "Manifest refraction at the 6-month visit, minus notation; "
     "blank axis when the cylinder is 0."),
    ("Postop_SEQ_D", "D", "SEQ at 6 months = sphere + cylinder/2 exactly."),
    ("Postop_UDVA / Postop_CDVA", "decimal", "Uncorrected / corrected distance acuity at 6 months (chart lines)."),
    ("Postop_Haze_Grade", "0-4", "Corneal haze grade at 6 months (ordinal)."),
    ("DryEye_Symptoms", "-", "Dry-eye symptoms at 6 months: Yes or No."),
    ("Complication_Type", "-", "No complication, Corneal haze (PRK haze grade >= 2; recorded also when the 6-month visit was "
     "missed, the grade then blank), Delayed epithelial healing (PRK), Diffuse lamellar keratitis, Flap striae, "
     "Epithelial ingrowth (LASIK)."),
    ("Visit<k>_Days_PostOp", "days", "Days from surgery to visit k (k = 1, 2, 3, 4 = 1, 3, 6, 12 months)."),
    ("Visit<k>_Sphere_D / _Cylinder_D / _Axis_Deg / _SEQ_D", "D / D / degrees / D",
     "Refraction at visit k (minus notation; blank axis when the cylinder is 0). Visit 3 = the 6-month primary endpoint "
     "(= Postop_*)."),
]


def readme(df, truth, sha):
    R = sc.Readme()
    R.add("Simulated data - no real patients")
    R.add("Every value in the sheet 'Surgeries' was generated by a computer model. No person, clinic or real "
          "measurement is represented. Patients and eyes carry codes only; there are no names, dates of birth or dates.")
    R.add("Dataset A of the StandardEyes demonstration datasets: 'Two techniques' - PRK versus LASIK, decimal "
          "acuity, minus-cylinder refraction, follow-up visits at 1, 3, 6 and 12 months with attrition, 6-month endpoint.")
    R.blank()
    sc.source_rows(R, GEN, SEED, df, sha, f"population table: seed {SEED_POP}")
    pr = df.groupby("Procedure")
    R.add("Size", f"{df.PatientID.nunique()} patients, {len(df)} eyes (one row per eye; primary surgeries only)",
          "; ".join(f"{g}: {len(d)} eyes of {d.PatientID.nunique()} patients" for g, d in pr))
    R.add("Settings files", "Settings_A_two_techniques.json", "loadable preset: groups PRK (reference) and LASIK")
    R.blank()
    R.add("NOTATION")
    R.add("Refraction", "every refraction in minus-cylinder notation (cylinder 0 or negative, with a blank axis when 0); "
          "SEQ = sphere + cylinder/2 exactly")
    R.add("Acuity", sc.decimal_lines_text(), f"Preop_UDVA: {int((df.Preop_UDVA == sc.OFF_CHART).sum())} eyes CF")
    R.blank()
    R.add("TRUE DIFFERENCES AT 6 MONTHS", "estimand", "true value", "note")
    R.add("Postop SEQ - target, LASIK - PRK", "mean difference given age and preop SEQ",
          f"{SEQ_OUT['LASIK'] - SEQ_OUT['PRK']:+.2f} D", "linear model; exact (PRK keeps a small hyperopic shift)")
    R.add("Dry-eye symptoms, LASIK vs PRK", "population-averaged odds ratio given age and preop SEQ", "1.8",
          "exact: drawn through a Gaussian copula from the logistic model")
    R.add("CDVA lines gained/lost", "latent shift of the ordinal model", f"{CDVA_PRK_SHIFT:+.2f} for PRK",
          "PRK eyes lose a line slightly more often")
    R.add("Astigmatic correction", "correction index, angle of error", "no difference",
          "same astigmatism model in both groups")
    R.blank()
    R.add(f"EXPECTED VALUES (population of {N_POP:,} simulated patients, same model, seed {SEED_POP})",
          "PRK", "LASIK", "")
    T = truth
    rowsx = [("share of eyes", "share", "{:.1%}"), ("mean preop SEQ (D)", "seq", "{:+.2f}"),
             ("mean postop SEQ - target (D)", "err", "{:+.3f}"), ("SD of postop SEQ - target (D)", "sd", "{:.3f}"),
             ("within +/-0.50 D of target", "w50", "{:.1%}"), ("within +/-1.00 D of target", "w100", "{:.1%}"),
             ("postop UDVA 20/20 or better", "ud20", "{:.1%}"), ("efficacy index (mean)", "eff", "{:.3f}"),
             ("safety index (mean)", "saf", "{:.3f}"), ("CDVA: 1 or more lines gained", "gain1", "{:.1%}"),
             ("CDVA: 2 or more lines lost", "lost2", "{:.1%}"), ("postop cylinder <= 0.50 D", "cyl50", "{:.1%}"),
             ("dry-eye symptoms", "dry", "{:.1%}"), ("haze grade >= 1", "haze1", "{:.1%}")]
    for lab, k, f in rowsx:
        R.add(lab, f.format(T["PRK"][k]), f.format(T["LASIK"][k]), "")
    R.blank()
    n_rows, est = sample_estimates(df)
    R.add(f"THIS FILE (the {n_rows} eyes the settings analyze: 6-month visit at 150-210 days or none)",
          "contrast", "crude / adjusted (95% CI)", "linear or logistic model of the eyes, robust SE clustered by "
          "patient; adjusted for age and preop SEQ (the model's covariates)")
    for lab, unit, t, c, a, ne, npt in est:
        odds = unit.startswith("odds")
        f = (lambda v: f"{v:.2f}") if odds else (lambda v: f"{v:+.3f}")
        R.add(lab, unit, " / ".join(f"{f(e)} ({f(lo)} to {f(hi)})" for e, lo, hi in (c, a)),
              (f"true {f(t)}; " if t is not None else "") + f"{ne} eyes of {npt} patients")
    R.blank()
    R.add("GENERATING MODEL", "parameter", "value", "meaning")
    P = [
        ("Patients", "age", f"N({AGE[0]}, {AGE[1]}) rounded, {AGE[2]}-{AGE[3]} years", ""),
        ("Patients", "female / both eyes treated", f"{P_FEMALE} / {P_BILATERAL}", "probabilities"),
        ("Patients", "latent preop SEQ", f"N({SEQ_P_MEAN}, {SEQ_P_SD}) D", "patient mean"),
        ("Patients", "latent pachymetry / mean K", f"N({PACHY_P_MEAN:.0f}, {PACHY_P_SD:.0f}) µm / N({K_MEAN}, {K_P_SD}) D", "patient means"),
        ("Patients", "latent log cylinder", f"N(log {CYL_MEDIAN}, {CYL_LOG_P_SD})", "patient mean"),
        ("Procedure", "logit PRK vs LASIK", f"{PRK_LOGIT[0]:+.2f} {PRK_LOGIT[1]:+.2f}(PACHYp-545)/10",
         f"then LASIK needs a residual bed >= {RSB_MIN_UM:.0f} µm under a {FLAP_UM:.0f} µm flap and <= {PTA_MAX:.0%} "
         f"tissue altered, otherwise PRK; PRK needs >= {PRK_REMAIN_MIN_UM:.0f} µm left, otherwise not treated"),
        ("Eyes", "preop SEQ", f"SEQp + N(0, {SEQ_E_SD}), limited to {SEQ_MIN} / {SEQ_MAX} D", "sphere on 0.25 D, SEQ derived"),
        ("Eyes", "cylinder magnitude", f"exp(log cyl + N(0, {CYL_LOG_E_SD})), 0.25 D steps, <= {CYL_MAX}", ""),
        ("Eyes", "plus axis", f"WTR / ATR / oblique {AXIS_TYPE_P}; patient SD 12, eye SD 4 deg; OS = 180 - OD + N(0, 8)", "mirror symmetry"),
        ("Eyes", "optical zone / depth", f"{OZ_MEAN} - {OZ_PER_D}(|SEQp|-4) + N(0, {OZ_SD}) mm; depth = OZ^2/3 (|SEQ| + |cyl|/2) + N(0, 3) µm", "Munnerlyn"),
        ("Eyes", "preop CDVA", f"line of {sc.CDVA_PRE['mean']} + {sc.CDVA_PRE['per_D']}(|SEQ|-4) + N(0, {sc.CDVA_PRE['sd_patient']}) patient "
         f"+ N(0, {sc.CDVA_PRE['sd_eye']}) eye logMAR, 20/12.5 to 20/40", ""),
        ("Eyes", "preop UDVA", f"line of CDVA {sc.UDVA_PRE['offset']:+} + {sc.UDVA_PRE['per_D']}|SEQ| + {sc.UDVA_PRE['per_cyl']}|cyl| + N(0, {sc.UDVA_PRE['sd']})", sc.UDVA_OFF_NOTE),
        ("Target", "monovision", f"bilateral patients aged >= {MONOVISION['min_age']}: probability {MONOVISION['p']}, one eye, "
         f"{MONOVISION['targets']} D with probabilities {MONOVISION['p_targets']}",
         f"only when that eye's preop SEQ <= target - {MONO_MIN_TREAT:.2f} D (no target within {MONO_MIN_TREAT:.2f} D of the "
         "preoperative refraction, no hyperopic treatment); all other eyes plano"),
        ("Postop SEQ", "model", f"target + {SEQ_OUT['PRK']:+}[PRK] {SEQ_OUT['LASIK']:+}[LASIK] {SEQ_OUT['seq']:+}(preop SEQ+4) "
         f"{SEQ_OUT['age']:+}(age-31) + u + e", f"u ~ N(0, {SEQ_OUT['sd_patient']}) patient, e ~ N(0, {SEQ_OUT['sd_eye']}) eye"),
        ("Astigmatism", "correction", f"SIA = -CI R(2 AoE) PRE; CI = {CI_MEAN} + N(0, {CI_P_SD}) + N(0, {CI_E_SD}); AoE = N(0, {AOE_P_SD}) "
         f"patient + N(0, {AOE_E_SD}) eye deg; POST = PRE + SIA + N(0, {NOISE_P_SD}) + N(0, {NOISE_E_SD}) per component",
         "plus-format double-angle vectors; patient parts mirrored in OS; same in both groups"),
        ("CDVA", "lines gained (+) / lost (-)", sc.cdva_change_text(),
         f"at preop SEQ -4 D; latent shift {CDVA_SEQ_SHIFT}(-preop SEQ-4) {CDVA_PRK_SHIFT:+}[PRK]; fellow-eye correlation {CDVA_RHO}; 20/12.5 at best"),
        ("UDVA", "postop", f"line of CDVA {sc.UDVA_POST['offset']:+} + {sc.UDVA_POST['myopic']}max(-SEQ-{sc.UDVA_POST['myopic_dz']},0) + "
         f"{sc.UDVA_POST['hyperopic']}max(SEQ-{sc.UDVA_POST['hyperopic_dz']},0) + {sc.UDVA_POST['per_cyl']}max(|cyl|-{sc.UDVA_POST['cyl_dz']},0) "
         f"+ N(0, {sc.UDVA_POST['sd_patient']}) patient + N(0, {sc.UDVA_POST['sd_eye']}) eye", "never better than CDVA"),
        ("Dry eye", "P(yes)", f"expit({DRY['intercept']} + ln1.8[LASIK] {DRY['age']:+}(age-31) {DRY['seq']:+}(preop SEQ+4))",
         f"Gaussian copula, fellow-eye latent correlation {DRY['rho']}"),
        ("Haze", "PRK", f"P(grade <= 0..3) = {HAZE_PRK_CUM} at 60 µm; latent +{HAZE_PER_UM} per µm deeper; fellow-eye correlation {HAZE_RHO}",
         f"LASIK: grade 1 with probability {HAZE_LASIK_P1}"),
        ("Complications", "per procedure", "; ".join(f"{p}: " + ", ".join(f"{k} {v}" for k, v in d.items()) for p, d in COMPLICATION_P.items()),
         "Corneal haze = PRK haze grade >= 2 (kept for an eye that missed the 6-month visit, whose grade is blank)"),
        ("Visits", "times", ", ".join(f"visit {v}: {d} +/- {j} days" for v, d, j in VISITS),
         f"6-month visit outside 150-210 days in {P_OUT_OF_WINDOW:.0%}"),
        ("Visits", "attendance", " / ".join(f"{a:.0%}" for a in ATTEND), "monotone, per patient, completely at random"),
        ("Visits", "course of SEQ", "overshoot " + ", ".join(f"{p} {a} D x exp(-days/{t:.0f})" for p, (a, t) in OVERSHOOT.items())
         + f"; regression {REGRESSION[0]} + {REGRESSION[1]}(preop SEQ+4) D/year",
         f"visit noise N(0, {VISIT_SD_PATIENT}) patient + N(0, {VISIT_SD_EYE}) eye; cylinder vector noise {VISIT_VEC_SD} D"),
        ("Missing values", "share of rows", ", ".join(f"{k} {v:.1%}" for k, v in MISSING.items()),
         "plus all 6-month data of eyes that missed that visit"),
    ]
    for p in P:
        R.add(*p)
    R.blank()
    R.add("COLUMN DICTIONARY", "unit", "description")
    for c in COLUMNS:
        R.add(*c)
    return R


def write_settings():
    visits = [{"label": lab, "time": f"Visit{k}_Days_PostOp", "sph": f"Visit{k}_Sphere_D",
               "cyl": f"Visit{k}_Cylinder_D", "seq": f"Visit{k}_SEQ_D"} for k, lab in ((1, "1"), (2, "3"), (3, "6"), (4, "12"))]
    cfg = sc.settings(
        FNAME, group_source_var="Procedure",
        baseline={"continuous": ["Age_Years", "Preop_SEQ_D", "Preop_Sphere_D", "Preop_Cylinder_D", "Preop_UDVA",
                                 "Preop_CDVA", "Preop_Pachymetry_um", "Preop_Mean_K_D"],
                  "ordinal": [], "dichotomous": ["Sex"], "categorical": []},
        preop={"continuous": ["Optical_Zone_mm", "Ablation_Depth_um"], "ordinal": [], "dichotomous": [], "categorical": []},
        outcome={"continuous": ["FollowUp_Days", "Postop_SEQ_D", "Postop_Cylinder_D", "Postop_UDVA", "Postop_CDVA"],
                 "ordinal": ["Postop_Haze_Grade"], "dichotomous": ["DryEye_Symptoms"], "categorical": ["Complication_Type"]},
        patient_level_params_raw=["Age_Years", "Sex", "Optical_Zone_mm"],
        groups={"PRK": ["PRK"], "LASIK": ["LASIK"]}, reference_group="PRK",
        group_colors={"PRK": "#0072B2", "LASIK": "#E69F00"},
        stability_figs=True, stability_visits=visits, stability_include_preop=True,
        stability_change_from="6", stability_change_to="12", stability_change_threshold=0.5,
        dichotomous_preferred_level={"Sex": "F", "DryEye_Symptoms": "Yes"},
        output_prefix="DS_A")
    return [sc.write_settings(
        "Settings_A_two_techniques.json", cfg, "Dataset A - two techniques (PRK vs LASIK)",
        "Two groups (PRK, LASIK) at the 6-month endpoint: the summary tables with patient-clustered tests, "
        "the standard graphs, the Publication Table, the mEYEstro and AstigMATIC tabs and the stability curve.",
        "The basic workflow on a two-group dataset: decimal acuity analyzed in logMAR, minus-cylinder refraction, "
        "the 6-month window as a filter, the stability over 1, 3, 6 and 12 months, and the export tabs for "
        "mEYEstro and AstigMATIC. Run it with 'Preview only' to see the results before saving them.")]


def main():
    os.makedirs(sc.OUT, exist_ok=True)
    df, eyes = build_dataset()
    sha = sc.data_sha(df)
    truth = population_truth()
    path = os.path.join(sc.OUT, FNAME)
    sc.write_workbook(df, readme(df, truth, sha), path, GEN)
    st = write_settings()
    print("saved:", path)
    print("settings:", ", ".join(os.path.basename(s) for s in st))
    print(df.groupby("Procedure").agg(eyes=("EyeID", "size"), patients=("PatientID", "nunique")).to_string())
    print("data SHA-256:", sha)
    for g in PROCS:
        print(g, {k: round(v, 3) for k, v in truth[g].items()})
    n_rows, est = sample_estimates(df)
    for r in est:
        print(r)
    return 0


if __name__ == "__main__":
    sys.exit(main())
