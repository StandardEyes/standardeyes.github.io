"""
Build the simulated example dataset of StandardEyes (dataset version 2, v1.1.0).

Simulated data - no real patients. Every value comes from the generating model
below, with a fixed seed, so a re-run writes the same data.

Design goals - it must exercise every feature of the app AND carry a known truth:
  * one row = one eye of one surgery; most patients contribute both eyes, and the
    fellow eyes are correlated through patient-level random effects
  * three cohorts ("Reference", "Cohort A", "Cohort B") with documented TRUE effects
  * measured confounding: age, preoperative SEQ and pachymetry drive both cohort
    membership and the outcomes, so crude and adjusted estimates differ
  * three procedures (PRK, LASIK, KLEx) for the subset analyses
  * all four variable types: continuous, ordinal, dichotomous, categorical
  * refraction on the 0.25 D grid with SEQ = sphere + cylinder/2 exactly; about a
    quarter of the rows typed in plus-cylinder form
  * acuity on chart lines (decimal notation), with lines of CDVA gained and lost
  * a 6-month primary endpoint, follow-up visits at 1, 3, 6, 12 and 24 months with
    attrition, retreatment rows, a target SEQ column and realistic missing values

THE GENERATING MODEL (every number is a constant below and is written, from the
same constants, into the README sheet of the workbook)

Patients (one draw per patient)
  age ~ N(34, 8.5) years, rounded, limited to 19-58; female 56%; smoker 15%;
  surgeon A/B/C 42/34/24%; 68% have both eyes treated.
  Latent patient means: preop SEQ ~ N(-4.0, 1.85) D; pachymetry ~ N(545, 30) µm;
  mean K ~ N(43.6, 1.4) D; log cylinder ~ N(log 0.70, 0.50).
  Cohort: multinomial logit against Reference (the confounding):
    logit A = -0.95 + 0.070 (age - 34) - 0.40 (SEQp + 4) - 0.30 (PACHYp - 545)/10
    logit B = -0.95 - 0.060 (age - 34) + 0.30 (SEQp + 4) + 0.20 (PACHYp - 545)/10
  Procedure: multinomial logit against LASIK:
    logit PRK = -0.35 - 0.45 (PACHYp - 545)/10 ; logit KLEx = -0.85 - 0.20 (SEQp + 4)
  Eligibility: LASIK flap 110 µm / KLEx cap 120 µm need a residual stromal bed
  >= 280 µm and a percent tissue altered <= 40%, otherwise the patient has PRK;
  PRK needs >= 400 µm of cornea left after the ablation, otherwise the patient is
  not treated (not in the dataset).

Eyes
  preop SEQ = SEQp + N(0, 0.65) (limited to -10.0 / -0.5), cylinder magnitude =
  exp(log cyl + N(0, 0.40)) on the 0.25 D grid (<= 4.50); plus axis: with-the-rule
  62% (around 90), against-the-rule 18% (around 180), oblique 20% (45 or 135),
  patient SD 12 deg; left eye = mirror image of the right (180 - axis) + N(0, 8).
  Sphere is rounded to 0.25 D and SEQ is DERIVED: SEQ = sphere + cylinder/2.
  pachymetry = PACHYp + N(0, 7); mean K = Kp + N(0, 0.3).
  Ablation / lenticule depth = OZ^2/3 x (|SEQ| + |cyl|/2) + N(0, 3) µm (Munnerlyn),
  + 15 µm minimum lenticule thickness for KLEx.

Outcomes at the 6-month primary endpoint (eye level; x = covariates of that eye)
  Postop SEQ = target + 0.05 - 0.20 [A] - 0.10 [B] - 0.010 (age - 34)
               + 0.06 (preop SEQ + 4) + 0.02 (pachymetry - 545)/10 + u + e,
               u ~ N(0, 0.25) per patient, e ~ N(0, 0.21) per eye.
  Astigmatism (plus format, double-angle vectors; no cohort term, no confounder):
               SIA = -CI x R(2 AoE) x PRE, POST = PRE + SIA + N(0, 0.11) patient
               + N(0, 0.06) eye per component, CI = 0.92 + N(0, 0.10) patient
               + N(0, 0.04) eye, AoE ~ N(0, 3) patient + N(0, 6) eye degrees (the
               patient parts mirrored in the left eye). Undercorrection of with-the-rule
               PRE leaves a small with-the-rule prediction-error centroid in EVERY cohort.
  CDVA lines (gain +): ordinal probit, latent = 0.10 (-(preop SEQ) - 4) + Z,
               Z ~ N(0, 1) with fellow-eye correlation 0.75, cut so that at
               preop SEQ -4.00 D: -3: 0.3%, -2: 1.2%, -1: 6%, 0: 60.5%, +1: 26.5%,
               +2: 5%, +3: 0.5%; postop CDVA never better than 20/12.5. No cohort term.
  UDVA         from the residual refraction: logMAR = postop CDVA - 0.03
               + 0.30 max(-SEQ - 0.20, 0) + 0.12 max(SEQ - 0.37, 0)
               + 0.15 max(|cyl| - 0.25, 0) + N(0, 0.03) patient + N(0, 0.04) eye,
               on chart lines, never better than CDVA.
               No direct cohort term (a cohort difference arises only through SEQ).
  Dry-eye symptoms: P(yes) = expit(-1.45 + ln 2 [A] - 0.10 (preop SEQ + 4)
               + 0.03 (age - 34) - 0.06 (pachymetry - 545)/10), drawn through a
               Gaussian copula (fellow-eye latent correlation 0.60), so this logistic
               model is the exact population-averaged model: true OR for Cohort A
               = 2.0 given age, preop SEQ and pachymetry; Cohort B: no effect.
  Haze (PRK): ordinal probit with fellow-eye correlation 0.5 and a shift of
               0.012 per µm of ablation depth above 60 µm; LASIK / KLEx: grade 1 in 1%.
  Satisfaction 1-5, complications, retreatment: see the README sheet.

Visits: 1, 3, 6, 12 and 24 months; the 6-month visit IS the primary endpoint
(Visit3_* = Postop_*). Early hyperopic overshoot (PRK 0.45 D, LASIK 0.15 D,
KLEx 0.10 D, decaying with a 90-day time constant) and myopic regression of
-0.06 + 0.02 (preop SEQ + 4) D per year; attendance 100 / 97 / 93 / 80 / 62%
(monotone, per patient, missing completely at random).

Usage:  python make_example_data.py                 (writes into the app)
        python make_example_data.py --out <folder>  (writes <folder>\\Example Data\\...)
"""
from __future__ import annotations
import argparse, hashlib, os, platform, sys
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


FNAME = "Example_Refractive_Dataset.xlsx"
SEED = 20261004            # the dataset
SEED_POPULATION = 20261005  # the large population used for the truth / crude table
N_PATIENTS = 320
N_POPULATION = 200_000

# ---- patients -------------------------------------------------------------------
AGE_MEAN, AGE_SD, AGE_MIN, AGE_MAX = 34.0, 8.5, 19, 58
P_FEMALE, P_SMOKER, P_BILATERAL = 0.56, 0.15, 0.68
SURGEONS, SURGEON_P = ["Surgeon A", "Surgeon B", "Surgeon C"], [0.42, 0.34, 0.24]
SEQ_P_MEAN, SEQ_P_SD, SEQ_E_SD = -4.0, 1.85, 0.65       # preop SEQ: ICC about 0.89
SEQ_MIN, SEQ_MAX = -10.0, -0.5
PACHY_P_MEAN, PACHY_P_SD, PACHY_E_SD = 545.0, 30.0, 7.0
K_MEAN, K_P_SD, K_E_SD = 43.6, 1.4, 0.3
CYL_MEDIAN, CYL_LOG_P_SD, CYL_LOG_E_SD, CYL_MAX = 0.70, 0.50, 0.40, 4.5
AXIS_TYPE_P = [0.62, 0.18, 0.20]                          # WTR, ATR, oblique
AXIS_PATIENT_SD, AXIS_EYE_SD, AXIS_MIRROR_SD = 12.0, 4.0, 8.0

# centring values of the covariates in every linear predictor
C_AGE, C_SEQ, C_PACHY = 34.0, -4.0, 545.0

# cohort membership (multinomial logit against Reference) - the confounding
COHORTS = ["Reference", "Cohort A", "Cohort B"]
COH_COEF = {   # intercept, per year of age, per D of patient SEQ, per 10 µm pachymetry
    "Cohort A": (-0.95, 0.070, -0.40, -0.30),
    "Cohort B": (-0.95, -0.060, 0.30, 0.20),
}

# procedure (multinomial logit against LASIK)
PROCS = ["PRK", "LASIK", "KLEx"]
PROC_COEF = {"PRK": (-0.35, -0.45, 0.0),     # intercept, per 10 µm pachymetry, per D SEQ
             "KLEx": (-0.85, 0.0, -0.20)}
FLAP_UM = {"LASIK": 110.0, "KLEx": 120.0}
RSB_MIN_UM, PTA_MAX, PRK_REMAIN_MIN_UM, KLEX_MIN_THICK_UM = 280.0, 0.40, 400.0, 15.0
OZ_MEAN, OZ_PER_D, OZ_SD = 6.5, 0.06, 0.12

# ---- outcomes (6-month primary endpoint) ------------------------------------------
SEQ_OUT = {"intercept": 0.05, "Cohort A": -0.20, "Cohort B": -0.10,
           "age": -0.010, "seq": 0.06, "pachy10": 0.02, "sd_patient": 0.25, "sd_eye": 0.21}
CI_MEAN, CI_P_SD, CI_E_SD = 0.92, 0.10, 0.04
AOE_P_SD, AOE_E_SD = 3.0, 6.0                # degrees; the patient part is mirrored in OS
POST_VEC_P_SD, POST_VEC_E_SD = 0.11, 0.06     # D per component; the patient part is mirrored in OS
CDVA_RHO, CDVA_SEQ_SHIFT = 0.75, 0.10
CDVA_CHANGE_P = {-3: 0.003, -2: 0.012, -1: 0.060, 0: 0.605, 1: 0.265, 2: 0.050, 3: 0.005}
CDVA_PRE = {"mean": -0.035, "per_D": 0.010, "sd_patient": 0.05, "sd_eye": 0.025}
UDVA_PRE = {"offset": -0.05, "per_D": 0.27, "per_cyl": 0.12, "sd": 0.07}
# postop UDVA = CDVA + blur of the residual refraction, each term with a dead zone (D)
UDVA_POST = {"offset": -0.03, "myopic": 0.30, "myopic_dz": 0.20, "hyperopic": 0.12, "hyperopic_dz": 0.37,
             "per_cyl": 0.15, "cyl_dz": 0.25, "sd_patient": 0.03, "sd_eye": 0.04}
DRY = {"intercept": -1.45, "Cohort A": float(np.log(2.0)), "Cohort B": 0.0,
       "seq": -0.10, "age": 0.03, "pachy10": -0.06, "rho": 0.60}
DRY_PRE = {"age": 0.02, "female": 0.35, "latent": -0.6, "sd": 0.6, "cuts": (0.45, 1.25, 2.0)}
HAZE_RHO, HAZE_PER_UM = 0.5, 0.012
HAZE_PRK_CUM = (0.70, 0.92, 0.985, 0.998)   # P(grade <= 0, 1, 2, 3) at 60 µm
HAZE_FLAP_P1 = 0.01
SATIS = {"intercept": 1.2, "udva": -3.0, "dry": -0.9, "sd_patient": 0.5, "sd": 0.7,
         "cuts": (-1.3, -0.6, 0.3, 1.1)}
COMPLICATION_P = {   # besides "Corneal haze" = PRK haze grade >= 2
    "PRK":   {"Epithelial defect": 0.04},
    "LASIK": {"Diffuse lamellar keratitis": 0.03, "Epithelial defect": 0.02,
              "Flap or cap complication": 0.015},
    "KLEx":  {"Diffuse lamellar keratitis": 0.02, "Epithelial defect": 0.01,
              "Flap or cap complication": 0.015},
}
RETREAT = {"seq_err": 0.75, "cyl": 1.0, "p_if_needed": 0.75, "p_base": 0.01, "p_row": 0.85}
MONOVISION = {"min_age": 45, "p": 0.5, "targets": (-0.50, -0.75, -1.00), "p_targets": (0.3, 0.5, 0.2)}

# ---- visits -------------------------------------------------------------------
VISITS = [(1, 30, 7), (2, 90, 14), (3, 180, 25), (4, 365, 30), (5, 730, 45)]   # n, day, jitter
SEQ_SUPPLIED = {1, 3}                    # the rest must be computed by the app
ATTEND = [1.00, 0.97, 0.93, 0.80, 0.62]  # share of patients attending each visit
P_OUT_OF_WINDOW = 0.04                   # 6-month visit outside 150-210 days
OVERSHOOT = {"PRK": 0.45, "LASIK": 0.15, "KLEx": 0.10}
OVERSHOOT_TAU = 90.0
REGRESSION = (-0.06, 0.02)               # D/year: intercept, per D of preop SEQ + 4
VISIT_SD_EYE, VISIT_SD_PATIENT, VISIT_VEC_SD = 0.10, 0.06, 0.08

P_PLUS_FORM = 0.25
MISSING = {"Preop_Pachymetry_um": 0.02, "Preop_Mean_K_D": 0.02, "Preop_DryEye_Grade": 0.01,
           "Postop_UDVA": 0.015, "Patient_Satisfaction_1to5": 0.06}
P_SMOKER_MISSING = 0.03

# decimal acuity of each 0.1-logMAR chart line (logMAR -0.3 ... 1.7)
DEC_LINE = {-3: 2.0, -2: 1.6, -1: 1.25, 0: 1.0, 1: 0.8, 2: 0.63, 3: 0.5, 4: 0.4, 5: 0.32,
            6: 0.25, 7: 0.2, 8: 0.16, 9: 0.125, 10: 0.1, 11: 0.08, 12: 0.063, 13: 0.05,
            14: 0.04, 15: 0.032, 16: 0.025, 17: 0.02}


def expit(x):
    return 1.0 / (1.0 + np.exp(-x))


def norm_ppf(p):
    """Inverse standard normal (Acklam's rational approximation, |error| < 1.2e-9)."""
    p = np.asarray(p, float)
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
    q = p[m] - 0.5; r = q * q
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


def line_of(logmar):
    """logMAR -> whole chart line (0.1 logMAR steps), as an integer."""
    return np.round(np.asarray(logmar, float) * 10).astype(int)


def dec_of(line):
    return np.vectorize(DEC_LINE.get)(np.asarray(line, int)).astype(float)


def correlated_normal(rng, n, rho):
    """(n, 2) standard normals with correlation rho between the two eyes."""
    v = rng.normal(0, 1, (n, 1))
    e = rng.normal(0, 1, (n, 2))
    return np.sqrt(rho) * v + np.sqrt(1 - rho) * e


def vec(mag, plus_axis):
    t = np.radians(2 * np.asarray(plus_axis, float))
    return mag * np.cos(t), mag * np.sin(t)


def udva_blur(seq, cyl_mag):
    """logMAR added to CDVA by a residual refraction (dead zones, then linear)."""
    u = UDVA_POST
    return (u["offset"] + u["myopic"] * np.maximum(-seq - u["myopic_dz"], 0)
            + u["hyperopic"] * np.maximum(seq - u["hyperopic_dz"], 0)
            + u["per_cyl"] * np.maximum(cyl_mag - u["cyl_dz"], 0))


# ==================================================================================
# The model. Returns one row per TREATED EYE (primary surgery) with every latent
# and complete outcome, before attrition, missing values and the plus-form typing.
# ==================================================================================
def simulate(rng, n_patients, batch_factor=1.6):
    m = int(n_patients * batch_factor) + 50
    # ---- patients (candidates; eligibility is applied below) ----
    age = np.clip(np.round(rng.normal(AGE_MEAN, AGE_SD, m)), AGE_MIN, AGE_MAX)
    female = rng.random(m) < P_FEMALE
    smoker = rng.random(m) < P_SMOKER
    surgeon = rng.choice(SURGEONS, size=m, p=SURGEON_P)
    seq_p = rng.normal(SEQ_P_MEAN, SEQ_P_SD, m)
    pachy_p = rng.normal(PACHY_P_MEAN, PACHY_P_SD, m)
    k_p = rng.normal(K_MEAN, K_P_SD, m)
    lcyl_p = rng.normal(np.log(CYL_MEDIAN), CYL_LOG_P_SD, m)
    atype = rng.choice(3, size=m, p=AXIS_TYPE_P)
    obl = np.where(rng.random(m) < 0.5, 45.0, 135.0)
    base_ax = np.select([atype == 0, atype == 1], [90.0, 180.0], obl) + rng.normal(0, AXIS_PATIENT_SD, m)
    bilateral = rng.random(m) < P_BILATERAL
    only_od = rng.random(m) < 0.5
    present = np.column_stack([bilateral | only_od, bilateral | ~only_od])   # OD, OS

    # cohort: multinomial logit against Reference
    eta = [np.zeros(m)]
    for c in COHORTS[1:]:
        b0, ba, bs, bp = COH_COEF[c]
        eta.append(b0 + ba * (age - C_AGE) + bs * (seq_p - C_SEQ) + bp * (pachy_p - C_PACHY) / 10)
    pr = np.exp(np.column_stack(eta)); pr /= pr.sum(1, keepdims=True)
    cohort = (rng.random(m)[:, None] > pr.cumsum(1)).sum(1)
    cohort = np.minimum(cohort, len(COHORTS) - 1)

    # procedure: multinomial logit against LASIK
    e_prk = PROC_COEF["PRK"][0] + PROC_COEF["PRK"][1] * (pachy_p - C_PACHY) / 10
    e_klx = PROC_COEF["KLEx"][0] + PROC_COEF["KLEx"][2] * (seq_p - C_SEQ)
    pp = np.exp(np.column_stack([e_prk, np.zeros(m), e_klx])); pp /= pp.sum(1, keepdims=True)
    proc = np.minimum((rng.random(m)[:, None] > pp.cumsum(1)).sum(1), 2)   # 0 PRK 1 LASIK 2 KLEx

    # ---- eyes ----
    seq_lat = np.clip(seq_p[:, None] + rng.normal(0, SEQ_E_SD, (m, 2)), SEQ_MIN, SEQ_MAX)
    mag = np.clip(q25(np.exp(lcyl_p[:, None] + rng.normal(0, CYL_LOG_E_SD, (m, 2)))), 0, CYL_MAX)
    ax_od = base_ax + rng.normal(0, AXIS_EYE_SD, m)
    ax_os = 180 - base_ax + rng.normal(0, AXIS_MIRROR_SD, m)
    pax = wrap_axis(np.column_stack([ax_od, ax_os]))            # PLUS axis (steep meridian)
    sph = q25(seq_lat + mag / 2)                                 # minus-cylinder sphere
    seq = sph - mag / 2                                          # SEQ = sphere + cyl/2, cyl = -mag
    pachy = np.round(pachy_p[:, None] + rng.normal(0, PACHY_E_SD, (m, 2)))
    kk = np.round(k_p[:, None] + rng.normal(0, K_E_SD, (m, 2)), 2)
    oz = np.clip(np.round((OZ_MEAN - OZ_PER_D * (-seq_p - 4) + rng.normal(0, OZ_SD, m)) * 10) / 10, 5.8, 7.0)
    depth0 = oz[:, None] ** 2 / 3 * (np.abs(seq) + mag / 2) + rng.normal(0, 3, (m, 2))

    # eligibility: LASIK / KLEx need the stromal bed, else PRK; PRK needs 400 µm left
    def flap_ok(p_idx):
        flap = FLAP_UM["LASIK"] if p_idx == 1 else FLAP_UM["KLEx"]
        d = depth0 + (KLEX_MIN_THICK_UM if p_idx == 2 else 0)
        ok = (pachy - flap - d >= RSB_MIN_UM) & ((flap + d) / pachy <= PTA_MAX)
        return (ok | ~present).all(1)
    for p_idx in (1, 2):
        bad = (proc == p_idx) & ~flap_ok(p_idx)
        proc[bad] = 0
    depth = depth0 + np.where(proc == 2, KLEX_MIN_THICK_UM, 0.0)[:, None]
    eligible = ((pachy - depth >= PRK_REMAIN_MIN_UM) | ~present).all(1) | (proc != 0)
    keep = np.flatnonzero(eligible)[:n_patients]
    if len(keep) < n_patients:
        raise RuntimeError("too few eligible candidates - raise batch_factor")

    P = dict(age=age, female=female, smoker=smoker, surgeon=surgeon, cohort=cohort, proc=proc,
             oz=oz, seq_p=seq_p, pachy_p=pachy_p)
    P = {k: v[keep] for k, v in P.items()}
    E = dict(present=present, seq=seq, sph=sph, mag=mag, pax=pax, pachy=pachy, k=kk,
             depth=np.round(depth))
    E = {k: v[keep] for k, v in E.items()}
    n = n_patients
    isA = (P["cohort"] == 1)[:, None] * 1.0
    isB = (P["cohort"] == 2)[:, None] * 1.0
    agec = (P["age"] - C_AGE)[:, None]
    seqc = E["seq"] - C_SEQ
    pac10 = (E["pachy"] - C_PACHY) / 10

    # ---- target (monovision for some presbyopic-age bilateral patients) ----
    mono = (P["age"] >= MONOVISION["min_age"]) & E["present"].all(1) & (rng.random(n) < MONOVISION["p"])
    mono_eye = (rng.random(n) < 0.5).astype(int)
    mono_t = rng.choice(MONOVISION["targets"], size=n, p=MONOVISION["p_targets"])
    target = np.zeros((n, 2))
    target[np.flatnonzero(mono), mono_eye[mono]] = mono_t[mono]

    # ---- preop acuity ----
    cd_pre = line_of(CDVA_PRE["mean"] + CDVA_PRE["per_D"] * (-E["seq"] - 4)
                     + rng.normal(0, CDVA_PRE["sd_patient"], (n, 1))
                     + rng.normal(0, CDVA_PRE["sd_eye"], (n, 2)))
    cd_pre = np.clip(cd_pre, -2, 3)
    ud_pre = line_of(cd_pre / 10 + UDVA_PRE["offset"] + UDVA_PRE["per_D"] * np.abs(E["seq"])
                     + UDVA_PRE["per_cyl"] * E["mag"] + rng.normal(0, UDVA_PRE["sd"], (n, 2)))
    ud_pre = np.clip(np.maximum(ud_pre, cd_pre), -2, 17)

    # ---- postop SEQ (the error relative to the target carries the cohort effect) ----
    err = (SEQ_OUT["intercept"] + SEQ_OUT["Cohort A"] * isA + SEQ_OUT["Cohort B"] * isB
           + SEQ_OUT["age"] * agec + SEQ_OUT["seq"] * seqc + SEQ_OUT["pachy10"] * pac10
           + rng.normal(0, SEQ_OUT["sd_patient"], (n, 1)) + rng.normal(0, SEQ_OUT["sd_eye"], (n, 2)))
    post_lat = target + err

    # ---- astigmatic correction (plus-format double-angle vectors) ----
    px, py = vec(E["mag"], E["pax"])
    ci = CI_MEAN + rng.normal(0, CI_P_SD, (n, 1)) + rng.normal(0, CI_E_SD, (n, 2))
    ci = np.maximum(ci, 0.3)
    mirror = np.array([[1.0, -1.0]])        # OS is the mirror image: angle and y change sign
    aoe = np.radians(rng.normal(0, AOE_P_SD, (n, 1)) * mirror + rng.normal(0, AOE_E_SD, (n, 2)))
    c2, s2 = np.cos(2 * aoe), np.sin(2 * aoe)
    sx, sy = -ci * (c2 * px - s2 * py), -ci * (s2 * px + c2 * py)       # SIA = -CI R PRE
    qx = px + sx + rng.normal(0, POST_VEC_P_SD, (n, 1)) + rng.normal(0, POST_VEC_E_SD, (n, 2))
    qy = py + sy + rng.normal(0, POST_VEC_P_SD, (n, 1)) * mirror + rng.normal(0, POST_VEC_E_SD, (n, 2))
    pmag = np.minimum(q25(np.hypot(qx, qy)), CYL_MAX)
    ppax = wrap_axis(np.degrees(np.arctan2(qy, qx)) / 2)
    psph = q25(post_lat + pmag / 2)
    pseq = psph - pmag / 2

    # ---- CDVA change in lines (gain +), ordinal probit with fellow-eye correlation ----
    ks = sorted(CDVA_CHANGE_P)
    cum = np.cumsum([CDVA_CHANGE_P[k] for k in ks])[:-1]
    cuts = norm_ppf(cum)
    z = correlated_normal(rng, n, CDVA_RHO) + CDVA_SEQ_SHIFT * (-E["seq"] - 4)
    dl = (z[..., None] > cuts).sum(-1) + ks[0]
    cd_post = np.maximum(cd_pre - dl, -2)                                # 20/12.5 at best

    # ---- UDVA from the residual refraction ----
    ud_post = line_of(cd_post / 10 + udva_blur(pseq, pmag)
                      + rng.normal(0, UDVA_POST["sd_patient"], (n, 1))
                      + rng.normal(0, UDVA_POST["sd_eye"], (n, 2)))
    ud_post = np.clip(np.maximum(ud_post, cd_post), -2, 17)

    # ---- dry-eye symptoms: logistic marginal model through a Gaussian copula ----
    zd = correlated_normal(rng, n, DRY["rho"])
    p_dry = expit(DRY["intercept"] + DRY["Cohort A"] * isA + DRY["Cohort B"] * isB
                  + DRY["seq"] * seqc + DRY["age"] * agec + DRY["pachy10"] * pac10)
    dry = norm_cdf(zd) < p_dry
    g = (DRY_PRE["age"] * agec + DRY_PRE["female"] * P["female"][:, None]
         + DRY_PRE["latent"] * zd + rng.normal(0, DRY_PRE["sd"], (n, 2)))
    dry_pre = np.digitize(g, DRY_PRE["cuts"])

    # ---- haze ----
    zh = correlated_normal(rng, n, HAZE_RHO) + HAZE_PER_UM * (E["depth"] - 60)   # deeper -> more haze
    haze = np.digitize(zh, norm_ppf(np.array(HAZE_PRK_CUM)))
    flap_proc = (P["proc"] != 0)[:, None]
    haze = np.where(flap_proc, (rng.random((n, 2)) < HAZE_FLAP_P1).astype(int), haze)

    # ---- complications (early, recorded for every treated eye) ----
    comp = np.full((n, 2), "No complication", dtype=object)
    u = rng.random((n, 2))
    for pi, pname in enumerate(PROCS):
        acc = np.zeros((n, 2))
        for lev, p in COMPLICATION_P[pname].items():
            hit = (P["proc"][:, None] == pi) & (u >= acc) & (u < acc + p)
            comp[hit] = lev
            acc = acc + p
    comp[(P["proc"][:, None] == 0) & (haze >= 2)] = "Corneal haze"

    # ---- satisfaction ----
    sl = (SATIS["intercept"] + SATIS["udva"] * ud_post / 10 + SATIS["dry"] * dry
          + rng.normal(0, SATIS["sd_patient"], (n, 1)) + rng.normal(0, SATIS["sd"], (n, 2)))
    satis = np.digitize(sl, SATIS["cuts"]) + 1

    # ---- retreatment decision at 6 months ----
    need = (np.abs(pseq - target) >= RETREAT["seq_err"]) | (pmag >= RETREAT["cyl"])
    ur = rng.random((n, 2))
    retreat = np.where(need, ur < RETREAT["p_if_needed"], ur < RETREAT["p_base"])

    # ---- flatten to one row per treated eye ----
    pi, ei = np.nonzero(E["present"])
    out = pd.DataFrame(dict(
        pidx=pi, Eye=np.where(ei == 0, "OD", "OS"),
        Cohort=np.array(COHORTS)[P["cohort"][pi]], Procedure=np.array(PROCS)[P["proc"][pi]],
        Surgeon=P["surgeon"][pi], Age_Years=P["age"][pi].astype(int),
        Sex=np.where(P["female"][pi], "F", "M"), Smoker=np.where(P["smoker"][pi], "Yes", "No"),
        pre_sph=E["sph"][pi, ei], pre_mag=E["mag"][pi, ei], pre_pax=E["pax"][pi, ei],
        Preop_SEQ_D=E["seq"][pi, ei],
        pre_ud=ud_pre[pi, ei], pre_cd=cd_pre[pi, ei],
        Preop_Pachymetry_um=E["pachy"][pi, ei], Preop_Mean_K_D=E["k"][pi, ei],
        Preop_DryEye_Grade=dry_pre[pi, ei], Optical_Zone_mm=P["oz"][pi],
        Ablation_or_Lenticule_Depth_um=E["depth"][pi, ei], Target_SEQ_D=target[pi, ei],
        post_lat=post_lat[pi, ei], err=err[pi, ei],
        post_sph=psph[pi, ei], post_mag=pmag[pi, ei], post_pax=ppax[pi, ei], Postop_SEQ_D=pseq[pi, ei],
        post_qx=qx[pi, ei], post_qy=qy[pi, ei],
        post_ud=ud_post[pi, ei], post_cd=cd_post[pi, ei], cdva_lines=cd_pre[pi, ei] - cd_post[pi, ei],
        Postop_Haze_Grade=haze[pi, ei], Patient_Satisfaction_1to5=satis[pi, ei],
        dry=dry[pi, ei], p_dry=p_dry[pi, ei], retreat=retreat[pi, ei], Complication_Type=comp[pi, ei],
    ))
    return out


# ==================================================================================
# The dataset: attrition, visits, retreatment rows, missing values, plus-form typing
# ==================================================================================
def to_minus_cols(sph_m, mag, plus_axis):
    """Minus-cylinder triple from the minus sphere, magnitude and PLUS axis."""
    return sph_m, -mag + 0.0, wrap_axis(np.asarray(plus_axis, float) + 90)


def build_dataset(seed=SEED):
    rng = np.random.default_rng(seed)
    eyes = simulate(rng, N_PATIENTS)
    n = len(eyes)
    pid = eyes.pidx.to_numpy()
    npat = pid.max() + 1

    # attendance: monotone, per patient
    att = np.asarray(ATTEND)
    p_last = np.append(att[:-1] - att[1:], att[-1])
    last = rng.choice(np.arange(1, 6), size=npat, p=p_last)[pid]

    df = pd.DataFrame({"PatientID": [f"P{p + 1:04d}" for p in pid]})
    df["EyeID"] = df.PatientID + "-" + eyes.Eye.to_numpy()
    df["Eye"] = eyes.Eye.to_numpy()
    df["SurgeryNumber"] = 1
    for c in ("Cohort", "Procedure", "Surgeon", "Age_Years", "Sex", "Smoker"):
        df[c] = eyes[c].to_numpy()
    s, c, a = to_minus_cols(eyes.pre_sph.to_numpy(), eyes.pre_mag.to_numpy(), eyes.pre_pax.to_numpy())
    df["Preop_Sphere_D"], df["Preop_Cylinder_D"], df["Preop_Axis_Deg"] = s, c, a
    df["Preop_SEQ_D"] = eyes.Preop_SEQ_D.to_numpy()
    df["Preop_UDVA"] = dec_of(eyes.pre_ud); df["Preop_CDVA"] = dec_of(eyes.pre_cd)
    for col in ("Preop_Pachymetry_um", "Preop_Mean_K_D", "Preop_DryEye_Grade", "Optical_Zone_mm",
                "Ablation_or_Lenticule_Depth_um", "Target_SEQ_D"):
        df[col] = eyes[col].to_numpy()

    # the 6-month visit = the primary endpoint
    day6 = 180 + rng.integers(-VISITS[2][2], VISITS[2][2] + 1, n)
    oow = rng.random(n) < P_OUT_OF_WINDOW
    early = rng.random(n) < 0.5
    day6 = np.where(oow & early, rng.integers(120, 150, n), np.where(oow, rng.integers(211, 251, n), day6))
    df["FollowUp_Days"] = day6.astype(float)
    s, c, a = to_minus_cols(eyes.post_sph.to_numpy(), eyes.post_mag.to_numpy(), eyes.post_pax.to_numpy())
    df["Postop_Sphere_D"], df["Postop_Cylinder_D"], df["Postop_Axis_Deg"] = s, c, a
    df["Postop_SEQ_D"] = eyes.Postop_SEQ_D.to_numpy()
    df["Postop_UDVA"] = dec_of(eyes.post_ud); df["Postop_CDVA"] = dec_of(eyes.post_cd)
    df["Postop_Haze_Grade"] = eyes.Postop_Haze_Grade.to_numpy()
    df["Patient_Satisfaction_1to5"] = eyes.Patient_Satisfaction_1to5.to_numpy()
    df["DryEye_Symptoms"] = np.where(eyes.dry, "Yes", "No")
    df["Retreatment_Required"] = np.where(eyes.retreat, "Yes", "No")
    df["Complication_Type"] = eyes.Complication_Type.to_numpy()

    # follow-up visits (minus form here; the plus-form typing below applies to them too)
    preseq = eyes.Preop_SEQ_D.to_numpy()
    proc = eyes.Procedure.to_numpy()
    over = np.vectorize(OVERSHOOT.get)(proc)
    rate = REGRESSION[0] + REGRESSION[1] * (preseq - C_SEQ)
    drift = lambda d: over * np.exp(-d / OVERSHOOT_TAU) + rate * d / 365.0
    pat_visit = rng.normal(0, VISIT_SD_PATIENT, (npat, len(VISITS)))[pid]
    vcols = []
    for k, (vn, day, jit) in enumerate(VISITS):
        if vn == 3:
            sph, cyl, ax = df.Postop_Sphere_D.to_numpy(), df.Postop_Cylinder_D.to_numpy(), df.Postop_Axis_Deg.to_numpy()
            seq, days = df.Postop_SEQ_D.to_numpy(), day6.astype(float)
        else:
            days = (day + rng.integers(-jit, jit + 1, n)).astype(float)
            lat = (eyes.post_lat.to_numpy() + drift(days) - drift(day6)
                   + pat_visit[:, k] + rng.normal(0, VISIT_SD_EYE, n))
            vx = eyes.post_qx.to_numpy() + rng.normal(0, VISIT_VEC_SD, n)
            vy = eyes.post_qy.to_numpy() + rng.normal(0, VISIT_VEC_SD, n)
            mag = np.minimum(q25(np.hypot(vx, vy)), CYL_MAX)
            sph = q25(lat + mag / 2)
            seq = sph - mag / 2
            cyl = -mag + 0.0
            ax = wrap_axis(np.degrees(np.arctan2(vy, vx)) / 2 + 90)
        gone = last < vn
        cols = {f"Visit{vn}_Days_PostOp": days, f"Visit{vn}_Sphere_D": sph,
                f"Visit{vn}_Cylinder_D": cyl, f"Visit{vn}_Axis_Deg": ax}
        if vn in SEQ_SUPPLIED:
            cols[f"Visit{vn}_SEQ_D"] = seq
        for name, v in cols.items():
            v = np.asarray(v, float).copy(); v[gone] = np.nan
            df[name] = v
            vcols.append(name)

    # no 6-month visit -> no primary-endpoint data (complications stay: they are early)
    no6 = last < 3
    for col in ("FollowUp_Days", "Postop_Sphere_D", "Postop_Cylinder_D", "Postop_Axis_Deg",
                "Postop_SEQ_D", "Postop_UDVA", "Postop_CDVA", "Postop_Haze_Grade",
                "Patient_Satisfaction_1to5", "DryEye_Symptoms", "Retreatment_Required"):
        df[col] = df[col].astype(object).where(~no6, np.nan)

    # ---- retreatment rows: a second surgery on eyes flagged at 6 months ----
    src = np.flatnonzero((df.Retreatment_Required == "Yes").to_numpy() & (rng.random(n) < RETREAT["p_row"]))
    re = df.iloc[src].copy()
    m = len(re)
    re["SurgeryNumber"] = 2
    re["Procedure"] = np.where((re.Procedure == "LASIK") & (rng.random(m) < 0.6), "LASIK", "PRK")
    for x in ("Sphere_D", "Cylinder_D", "Axis_Deg", "SEQ_D"):
        re[f"Preop_{x}"] = re[f"Postop_{x}"].astype(float)
    re["Preop_UDVA"], re["Preop_CDVA"] = re.Postop_UDVA, re.Postop_CDVA
    base = eyes.iloc[src]
    re["Preop_Pachymetry_um"] = np.round(base.Preop_Pachymetry_um.to_numpy() - base.Ablation_or_Lenticule_Depth_um.to_numpy()
                                         + rng.normal(0, 5, m))
    re["Preop_Mean_K_D"] = np.round(base.Preop_Mean_K_D.to_numpy() - 0.75 * np.abs(base.Preop_SEQ_D.to_numpy())
                                    + rng.normal(0, 0.2, m), 2)
    re["Preop_DryEye_Grade"] = np.minimum(base.Preop_DryEye_Grade.to_numpy() + base.dry.to_numpy(), 3)
    rmag = np.abs(re.Preop_Cylinder_D.to_numpy(float))
    re["Ablation_or_Lenticule_Depth_um"] = np.round(re.Optical_Zone_mm.to_numpy() ** 2 / 3
                                                    * (np.abs(re.Preop_SEQ_D.to_numpy(float)) + rmag / 2)
                                                    + rng.normal(0, 2, m)).clip(5, None)
    re["Target_SEQ_D"] = 0.0
    re["FollowUp_Days"] = (180 + rng.integers(-20, 21, m)).astype(float)
    r_seq = SEQ_OUT["intercept"] / 2 + rng.normal(0, 0.22, m)
    rpx, rpy = vec(rmag, wrap_axis(re.Preop_Axis_Deg.to_numpy(float) + 90))
    rqx = (1 - CI_MEAN) * rpx + rng.normal(0, 0.10, m)
    rqy = (1 - CI_MEAN) * rpy + rng.normal(0, 0.10, m)
    rq = np.minimum(q25(np.hypot(rqx, rqy)), CYL_MAX)
    rsph = q25(r_seq + rq / 2)
    re["Postop_Sphere_D"], re["Postop_Cylinder_D"] = rsph, -rq + 0.0
    re["Postop_Axis_Deg"] = wrap_axis(np.degrees(np.arctan2(rqy, rqx)) / 2 + 90)
    re["Postop_SEQ_D"] = rsph - rq / 2
    rcd_pre = line_of(-np.log10(re.Preop_CDVA.to_numpy(float)))
    rcd = np.maximum(rcd_pre - rng.choice([-1, 0, 1], size=m, p=[0.05, 0.85, 0.10]), -2)
    rud = line_of(rcd / 10 + udva_blur(re.Postop_SEQ_D.to_numpy(float), rq) + rng.normal(0, 0.05, m))
    re["Postop_CDVA"], re["Postop_UDVA"] = dec_of(rcd), dec_of(np.maximum(rud, rcd))
    re["Postop_Haze_Grade"] = np.where(re.Procedure == "PRK", (rng.random(m) < 0.15).astype(int), 0)
    re["Complication_Type"] = "No complication"
    re["Patient_Satisfaction_1to5"] = rng.choice([3, 4, 5], size=m, p=[0.2, 0.4, 0.4])
    re["Retreatment_Required"] = "No"
    re[vcols] = np.nan                      # the retreatment's own visits are not recorded
    df = pd.concat([df, re], ignore_index=True)

    # ---- realistic missing values ----
    for col, frac in MISSING.items():
        hit = rng.random(len(df)) < frac
        df[col] = df[col].astype(object).where(~hit, np.nan)
    pat_ids = df.PatientID.unique()
    no_smoke = set(pat_ids[rng.random(len(pat_ids)) < P_SMOKER_MISSING])
    df.loc[df.PatientID.isin(no_smoke), "Smoker"] = np.nan

    # ---- about a quarter of the rows typed in PLUS-cylinder form (whole row) ----
    flip = np.zeros(len(df), bool)                    # exactly round(P x rows) rows, chosen at random
    flip[rng.permutation(len(df))[:int(round(P_PLUS_FORM * len(df)))]] = True
    trip = [("Preop_Sphere_D", "Preop_Cylinder_D", "Preop_Axis_Deg"),
            ("Postop_Sphere_D", "Postop_Cylinder_D", "Postop_Axis_Deg")]
    trip += [(f"Visit{v}_Sphere_D", f"Visit{v}_Cylinder_D", f"Visit{v}_Axis_Deg") for v, _, _ in VISITS]
    for s, c, a in trip:
        sv, cv, av = (df[x].to_numpy(float) for x in (s, c, a))
        df[s] = np.where(flip, sv + cv, sv)
        df[c] = np.where(flip, -cv + 0.0, cv)
        df[a] = np.where(flip & np.isfinite(av), wrap_axis(av + 90), av)
    df["Cylinder_Notation"] = np.where(flip, "plus", "minus")

    df = df.sort_values(["PatientID", "SurgeryNumber", "Eye"], kind="stable").reset_index(drop=True)
    cols = ["PatientID", "EyeID", "Eye", "SurgeryNumber", "Cohort", "Procedure", "Surgeon",
            "Age_Years", "Sex", "Smoker",
            "Preop_Sphere_D", "Preop_Cylinder_D", "Preop_Axis_Deg", "Preop_SEQ_D",
            "Preop_UDVA", "Preop_CDVA", "Preop_Pachymetry_um", "Preop_Mean_K_D",
            "Preop_DryEye_Grade", "Optical_Zone_mm", "Ablation_or_Lenticule_Depth_um", "Target_SEQ_D",
            "FollowUp_Days", "Postop_Sphere_D", "Postop_Cylinder_D", "Postop_Axis_Deg",
            "Postop_SEQ_D", "Postop_UDVA", "Postop_CDVA",
            "Postop_Haze_Grade", "Patient_Satisfaction_1to5", "DryEye_Symptoms",
            "Retreatment_Required", "Complication_Type", "Cylinder_Notation"] + vcols
    df = df[cols]
    ints = ["SurgeryNumber", "Age_Years", "Preop_Axis_Deg", "Preop_Pachymetry_um", "Preop_DryEye_Grade",
            "Ablation_or_Lenticule_Depth_um", "FollowUp_Days", "Postop_Axis_Deg", "Postop_Haze_Grade",
            "Patient_Satisfaction_1to5"] + [c for c in vcols if c.endswith(("_Days_PostOp", "_Axis_Deg"))]
    for c in ints:
        df[c] = pd.to_numeric(df[c]).round().astype("Int64")
    for c in df.columns:
        if c not in ints and df[c].dtype == object and c.endswith(("_D", "_um", "_mm", "VA")):
            df[c] = pd.to_numeric(df[c]).astype(float)
    return df, eyes


# ==================================================================================
# Truth table: crude versus true effects in a LARGE population from the same model
# ==================================================================================
def ols(y, X):
    b, *_ = np.linalg.lstsq(X, y, rcond=None)
    return b


def logit_fit(y, X, it=30):
    b = np.zeros(X.shape[1])
    for _ in range(it):
        p = expit(X @ b)
        w = p * (1 - p)
        b = b + np.linalg.solve(X.T @ (X * w[:, None]), X.T @ (y - p))
    return b


def population_truth():
    pop = simulate(np.random.default_rng(SEED_POPULATION), N_POPULATION)
    A = (pop.Cohort == "Cohort A").to_numpy(float)
    B = (pop.Cohort == "Cohort B").to_numpy(float)
    ref = (pop.Cohort == "Reference").to_numpy()
    one = np.ones(len(pop))
    cov = np.column_stack([pop.Age_Years - C_AGE, pop.Preop_SEQ_D - C_SEQ,
                           (pop.Preop_Pachymetry_um - C_PACHY) / 10])
    X_crude = np.column_stack([one, A, B])
    X_adj = np.column_stack([one, A, B, cov])
    rows = []

    def lin(label, y, unit, truth_a, truth_b, extra=None):
        Xa = X_adj if extra is None else np.column_stack([X_adj, extra])
        bc, ba = ols(y, X_crude), ols(y, Xa)
        rows.append((label, unit, "Cohort A vs Reference", truth_a, bc[1], ba[1]))
        rows.append((label, unit, "Cohort B vs Reference", truth_b, bc[2], ba[2]))

    lin("Postop SEQ (6 months)", pop.Postop_SEQ_D.to_numpy(), "D (difference)",
        SEQ_OUT["Cohort A"], SEQ_OUT["Cohort B"], extra=pop.Target_SEQ_D.to_numpy())
    lin("Postop cylinder magnitude", pop.post_mag.to_numpy(), "D (difference)", 0.0, 0.0)
    lin("Postop CDVA", -np.log10(dec_of(pop.post_cd)), "logMAR (difference)", 0.0, 0.0)
    y = pop.dry.to_numpy(float)
    bc, ba = logit_fit(y, X_crude), logit_fit(y, X_adj)
    rows.append(("Dry-eye symptoms", "odds ratio", "Cohort A vs Reference", 2.0, np.exp(bc[1]), np.exp(ba[1])))
    rows.append(("Dry-eye symptoms", "odds ratio", "Cohort B vs Reference", 1.0, np.exp(bc[2]), np.exp(ba[2])))
    means = pop.groupby("Cohort")[["Age_Years", "Preop_SEQ_D", "Preop_Pachymetry_um"]].mean()
    share = pop.groupby("Cohort").size() / len(pop)
    # true prediction-error centroid (plus format, target plano: PE = POST), before rounding
    cent = {c: (pop.post_qx[pop.Cohort == c].mean(), pop.post_qy[pop.Cohort == c].mean()) for c in COHORTS}
    return rows, means, share, cent, len(pop)


# ==================================================================================
# The same contrasts in THIS file: sample estimates scatter around the true values
# ==================================================================================
def analysed_rows(df):
    """The rows the shipped example settings analyze (make_example_settings.py):
    first surgeries, age >= 18, the 6-month visit at 150-210 days, |preop
    cylinder| <= 5 D. As in the app, a blank cell passes a filter rule (an eye
    without a 6-month visit is kept, with missing outcomes)."""
    num = lambda c: pd.to_numeric(df[c]).astype(float).to_numpy()
    fu, age, cyl = num("FollowUp_Days"), num("Age_Years"), num("Preop_Cylinder_D")
    keep = ((num("SurgeryNumber") == 1) & (np.isnan(age) | (age >= 18))
            & (np.isnan(fu) | ((fu >= 150) & (fu <= 210))) & (np.isnan(cyl) | (np.abs(cyl) <= 5)))
    return df[keep]


def cluster_se(X, r, w, cl):
    """Robust (sandwich) standard errors clustered by patient of the
    estimating equation sum X'r = 0 with working weights w (OLS: w = 1;
    logistic: w = p(1 - p))."""
    bread = np.linalg.inv(X.T @ (X * w[:, None]))
    _, idx = np.unique(cl, return_inverse=True)
    S = np.zeros((idx.max() + 1, X.shape[1]))
    np.add.at(S, idx, X * r[:, None])
    return np.sqrt(np.diag(bread @ (S.T @ S) @ bread))


def sample_estimates(df):
    """The documented contrasts estimated in this file, crude and adjusted for
    the three measured confounders (the example settings' covariates), both on
    the eyes with the outcome and the confounders: (rows analyzed, [(label,
    unit, contrast, true value, (crude, lo, hi), (adjusted, lo, hi), eyes,
    patients)]). Linear or logistic models of the eyes with robust SE
    clustered by patient; 95% CI = estimate +/- 1.96 SE."""
    d = analysed_rows(df)
    num = lambda c: pd.to_numeric(d[c]).astype(float).to_numpy()
    A = (d.Cohort == "Cohort A").to_numpy(float)
    B = (d.Cohort == "Cohort B").to_numpy(float)
    cov = np.column_stack([num("Age_Years") - C_AGE, num("Preop_SEQ_D") - C_SEQ,
                           (num("Preop_Pachymetry_um") - C_PACHY) / 10])
    dry = d.DryEye_Symptoms.map({"Yes": 1.0, "No": 0.0}).astype(float).to_numpy()
    pid = d.PatientID.astype(str).to_numpy()
    outcomes = [("Postop SEQ (6 months)", "D (difference)", num("Postop_SEQ_D"), False,
                 SEQ_OUT["Cohort A"], SEQ_OUT["Cohort B"]),
                ("Postop cylinder magnitude", "D (difference)", np.abs(num("Postop_Cylinder_D")), False, 0.0, 0.0),
                ("Postop CDVA", "logMAR (difference)", -np.log10(num("Postop_CDVA")), False, 0.0, 0.0),
                ("Dry-eye symptoms", "odds ratio", dry, True, 2.0, 1.0)]
    rows = []
    for label, unit, y, logistic, true_a, true_b in outcomes:
        ok = np.isfinite(y) & np.isfinite(cov).all(1)
        one = np.ones(int(ok.sum()))
        fits = []
        for X in (np.column_stack([one, A[ok], B[ok]]), np.column_stack([one, A[ok], B[ok], cov[ok]])):
            if logistic:
                b = logit_fit(y[ok], X)
                p = expit(X @ b)
                se = cluster_se(X, y[ok] - p, p * (1 - p), pid[ok])
            else:
                b = ols(y[ok], X)
                se = cluster_se(X, y[ok] - X @ b, np.ones(len(one)), pid[ok])
            ci = [(b[j], b[j] - 1.959964 * se[j], b[j] + 1.959964 * se[j]) for j in (1, 2)]
            fits.append([tuple(np.exp(c)) for c in ci] if logistic else ci)
        for j, (con, t) in enumerate((("Cohort A vs Reference", true_a), ("Cohort B vs Reference", true_b))):
            rows.append((label, unit, con, t, fits[0][j], fits[1][j], int(ok.sum()), len(set(pid[ok]))))
    return len(d), rows


# ==================================================================================
# README sheet
# ==================================================================================
COLUMNS = [  # name, unit, description
    ("PatientID", "-", "Patient code (no names). Both eyes of a patient share it."),
    ("EyeID", "-", "Eye code = PatientID-Eye. A retreatment row repeats the EyeID of its eye."),
    ("Eye", "-", "OD (right) or OS (left)."),
    ("SurgeryNumber", "-", "1 = primary surgery, 2 = retreatment of the same eye."),
    ("Cohort", "-", "Simulated cohort: Reference, Cohort A or Cohort B (patient level)."),
    ("Procedure", "-", "PRK, LASIK or KLEx (keratorefractive lenticule extraction)."),
    ("Surgeon", "-", "Surgeon A, B or C (patient level; no effect on any outcome)."),
    ("Age_Years", "years", "Age at surgery (patient level)."),
    ("Sex", "-", "F or M (patient level)."),
    ("Smoker", "-", "Yes or No (patient level; no effect on any outcome)."),
    ("Preop_Sphere_D", "D", "Preoperative manifest sphere (0.25 D steps), in the row's cylinder notation."),
    ("Preop_Cylinder_D", "D", "Preoperative manifest cylinder (0.25 D steps): negative in minus notation, positive in plus notation."),
    ("Preop_Axis_Deg", "degrees", "Axis of that cylinder (1-180)."),
    ("Preop_SEQ_D", "D", "Preoperative spherical equivalent = sphere + cylinder/2 exactly (same in either notation)."),
    ("Preop_UDVA", "decimal", "Preoperative uncorrected distance acuity, decimal value of a 0.1-logMAR chart line."),
    ("Preop_CDVA", "decimal", "Preoperative corrected distance acuity, decimal value of a chart line."),
    ("Preop_Pachymetry_um", "µm", "Central corneal thickness."),
    ("Preop_Mean_K_D", "D", "Mean keratometry."),
    ("Preop_DryEye_Grade", "0-3", "Preoperative dry-eye grade (ordinal)."),
    ("Optical_Zone_mm", "mm", "Optical zone (patient level)."),
    ("Ablation_or_Lenticule_Depth_um", "µm", "Maximum ablation depth (PRK, LASIK) or lenticule thickness (KLEx)."),
    ("Target_SEQ_D", "D", "Intended postoperative SEQ: plano, or a myopic monovision target in one eye of some patients aged 45+."),
    ("FollowUp_Days", "days", "Days from surgery to the 6-month (primary endpoint) visit; blank when that visit was missed."),
    ("Postop_Sphere_D", "D", "Sphere at the 6-month visit (0.25 D steps), in the row's notation."),
    ("Postop_Cylinder_D", "D", "Cylinder at the 6-month visit (0.25 D steps)."),
    ("Postop_Axis_Deg", "degrees", "Axis of that cylinder (1-180)."),
    ("Postop_SEQ_D", "D", "SEQ at 6 months = sphere + cylinder/2 exactly."),
    ("Postop_UDVA", "decimal", "Uncorrected distance acuity at 6 months (chart line)."),
    ("Postop_CDVA", "decimal", "Corrected distance acuity at 6 months (chart line)."),
    ("Postop_Haze_Grade", "0-4", "Corneal haze grade at 6 months (ordinal)."),
    ("Patient_Satisfaction_1to5", "1-5", "Satisfaction score at 6 months (ordinal)."),
    ("DryEye_Symptoms", "-", "Dry-eye symptoms at 6 months: Yes or No."),
    ("Retreatment_Required", "-", "Retreatment indicated at 6 months: Yes or No."),
    ("Complication_Type", "-", "No complication, Corneal haze (PRK), Diffuse lamellar keratitis, Epithelial defect, Flap or cap complication."),
    ("Cylinder_Notation", "-", "minus or plus: the cylinder notation in which EVERY refraction of this row is typed."),
    ("Visit<k>_Days_PostOp", "days", "Days from surgery to visit k (k = 1, 2, 3, 4, 5 = 1, 3, 6, 12, 24 months)."),
    ("Visit<k>_Sphere_D / _Cylinder_D / _Axis_Deg", "D / D / degrees", "Refraction at visit k, in the row's notation. Visit 3 = the 6-month primary endpoint (= Postop_*)."),
    ("Visit1_SEQ_D, Visit3_SEQ_D", "D", "SEQ supplied for visits 1 and 3 only; for the other visits the app computes it from sphere and cylinder."),
]


def readme_rows(df, truth, data_sha):
    rows, means, share, cent, npop = truth
    R = []
    add = lambda *x: R.append(list(x) + [""] * (4 - len(x)))
    add("Simulated data - no real patients")
    add("Every value in the sheet 'Surgeries' was generated by a computer model. No person, clinic or real "
        "measurement is represented. Names are not used: patients and eyes carry codes only.")
    add("")
    add("SOURCE")
    add("Generator", "make_example_data.py (shipped in the validation folder of the software)")
    add("Seed", str(SEED), "numpy.random.default_rng; population table: seed " + str(SEED_POPULATION))
    add("Software", f"Python {platform.python_version()}, numpy {np.__version__}, pandas {pd.__version__}",
        "numpy does not guarantee identical random streams across versions")
    add("SHA-256 of the data", data_sha, "of the generator's 'Surgeries' table written as CSV (UTF-8, LF, no index) "
        "before it is saved to Excel; rerun the generator to check it (a table read back from the .xlsx gives "
        "another hash: reading changes some column types, e.g. integers with blank cells become decimals)")
    add("Size", f"{df.PatientID.nunique()} patients, {df.EyeID.nunique()} eyes, {len(df)} rows "
        f"({int((df.SurgeryNumber == 1).sum())} primary surgeries, {int((df.SurgeryNumber == 2).sum())} retreatments)")
    add("")
    add("CYLINDER NOTATION")
    add("Per row", "Column Cylinder_Notation says how EVERY refraction of the row (preop, postop, visits) is typed.")
    add("minus", "negative cylinder at the minus axis (flat meridian)")
    add("plus", "positive cylinder at the plus axis (steep meridian); plus sphere = minus sphere + minus cylinder, "
        "plus cylinder = -(minus cylinder), plus axis = minus axis +/- 90")
    add("Share", f"{(df.Cylinder_Notation == 'plus').mean() * 100:.1f}% of rows are typed in plus notation "
        f"(rows chosen at random)")
    add("SEQ", "sphere + cylinder/2, identical in both notations; every SEQ column equals it exactly")
    add("Astigmatism", "with-the-rule = plus axis 90 (minus axis 180); against-the-rule = plus axis 180 (minus axis 90)")
    add("")
    add("TRUE COHORT EFFECTS", "estimand", "true value", "note")
    add("Postop SEQ, Cohort A vs Reference", "mean difference given age, preop SEQ, pachymetry (and target)",
        f"{SEQ_OUT['Cohort A']:+.2f} D", "linear model; exact")
    add("Postop SEQ, Cohort B vs Reference", "same", f"{SEQ_OUT['Cohort B']:+.2f} D", "")
    add("Dry-eye symptoms, Cohort A vs Reference", "population-averaged odds ratio given age, preop SEQ, pachymetry",
        "2.0", "exact: drawn through a Gaussian copula from this logistic model")
    add("Dry-eye symptoms, Cohort B vs Reference", "same", "1.0 (no effect)", "")
    add("Postop cylinder (magnitude, vector)", "any", "no effect (0) for Cohort A and Cohort B",
        "the astigmatism model has no cohort term and no confounder")
    add("Postop CDVA and lines of CDVA gained/lost", "any, given preop SEQ", "no effect (0) for Cohort A and Cohort B",
        "the line change depends on preop SEQ only, which differs between cohorts (confounded null)")
    add("Postop UDVA, haze, satisfaction, complications, retreatment", "direct effect", "none",
        "UDVA, satisfaction and retreatment follow the residual refraction, so they differ through the SEQ effect")
    add("Cohort B", "", "SEQ -0.10 D and no other effect", "")
    add("")
    add(f"CRUDE VERSUS TRUE (population of {N_POPULATION:,} simulated patients, {npop:,} eyes, same model, seed {SEED_POPULATION})",
        "contrast", "true / crude / adjusted", "adjusted = linear or logistic model with age, preop SEQ, pachymetry")
    for lab, unit, con, t, c, a in rows:
        fmt = "{:.2f}" if unit == "odds ratio" else "{:+.3f}"
        add(f"{lab} ({unit})", con, " / ".join(fmt.format(v) for v in (t, c, a)),
            "SEQ model also adjusts for the target" if lab.startswith("Postop SEQ") else "")
    for c in COHORTS:
        add(f"Population means, {c}", f"share {share[c] * 100:.1f}%",
            f"age {means.loc[c, 'Age_Years']:.1f} y; preop SEQ {means.loc[c, 'Preop_SEQ_D']:+.2f} D; "
            f"pachymetry {means.loc[c, 'Preop_Pachymetry_um']:.0f} µm", "the measured confounders")
    for c in COHORTS:
        x, y = cent[c]
        add(f"True prediction-error centroid, {c}", "plus format, before rounding",
            f"{np.hypot(x, y):.3f} D @ {(np.degrees(np.arctan2(y, x)) / 2) % 180:.0f} deg (x {x:+.3f}, y {y:+.3f})",
            "identical model in every cohort: undercorrection of with-the-rule astigmatism")
    add("")
    n_rows, est = sample_estimates(df)
    add(f"THIS FILE ({n_rows} rows that the example settings analyze: first surgeries, age >= 18, 6-month visit "
        "at 150-210 days or none, |preop cylinder| <= 5 D)", "contrast", "crude / adjusted (95% CI)",
        "linear or logistic model of the eyes with the outcome and the three confounders, robust SE clustered by "
        "patient, 95% CI = estimate +/- 1.96 SE; adjusted = age, preop SEQ, pachymetry (the example settings' "
        "covariates; the SEQ model is not adjusted for the target); the app's exchangeable GEE with its "
        "small-sample correction gives slightly different values")
    missed = []
    for lab, unit, con, t, c, a, n_eyes, n_pat in est:
        odds = unit == "odds ratio"
        f = (lambda v: f"{v:.2f}") if odds else (lambda v: f"{v:+.3f}")
        add(f"{lab} ({unit})", con, " / ".join(f"{f(e)} ({f(lo)} to {f(hi)})" for e, lo, hi in (c, a)),
            f"true {f(t)}; {n_eyes} eyes of {n_pat} patients")
        if not a[1] <= t <= a[2]:
            missed.append((lab, unit, con, t, a, f))
    if missed:
        what = "; ".join(f"the adjusted {'OR' if u == 'odds ratio' else 'difference'} of {lab.lower()}, "
                         f"{con}, is {f(a[0])} (95% CI, {f(a[1])} to {f(a[2])}) although its true value is "
                         f"{f(t)}" for lab, u, con, t, a, f in missed)
        null = all(t == (1.0 if u == "odds ratio" else 0.0) for _, u, _, t, _, _ in missed)
        add("Sampling variation", "Sample estimates scatter around the true values. In this file " + what + (
            ": a chance finding of the kind that about 1 in 20 contrasts without a true effect give at the 5% "
            "level, so an adjusted model of this file reports it as statistically significant although there is "
            "no effect." if null else ": a 95% CI misses its true value in about 1 in 20 samples."))
    else:
        add("Sampling variation", "Sample estimates scatter around the true values; in this file every adjusted "
            "95% CI covers its true value.")
    add("")
    add("GENERATING MODEL", "parameter", "value", "meaning")
    P = [
        ("Patients", "age", f"N({AGE_MEAN}, {AGE_SD}) rounded, {AGE_MIN}-{AGE_MAX} years", ""),
        ("Patients", "female / smoker / both eyes", f"{P_FEMALE} / {P_SMOKER} / {P_BILATERAL}", "probabilities"),
        ("Patients", "surgeon A / B / C", " / ".join(map(str, SURGEON_P)), "probabilities"),
        ("Patients", "latent preop SEQ", f"N({SEQ_P_MEAN}, {SEQ_P_SD}) D", "patient mean"),
        ("Patients", "latent pachymetry", f"N({PACHY_P_MEAN:.0f}, {PACHY_P_SD:.0f}) µm", "patient mean"),
        ("Patients", "latent mean K", f"N({K_MEAN}, {K_P_SD}) D", "patient mean"),
        ("Patients", "latent log cylinder", f"N(log {CYL_MEDIAN}, {CYL_LOG_P_SD})", "patient mean"),
    ]
    for c in COHORTS[1:]:
        b0, ba, bs, bp = COH_COEF[c]
        P.append(("Cohort membership", f"logit {c} vs Reference",
                  f"{b0:+.2f} {ba:+.3f}(age-34) {bs:+.2f}(SEQp+4) {bp:+.2f}(PACHYp-545)/10",
                  "multinomial logit on the patient means - the measured confounding"))
    P += [
        ("Procedure", "logit PRK vs LASIK", f"{PROC_COEF['PRK'][0]:+.2f} {PROC_COEF['PRK'][1]:+.2f}(PACHYp-545)/10", ""),
        ("Procedure", "logit KLEx vs LASIK", f"{PROC_COEF['KLEx'][0]:+.2f} {PROC_COEF['KLEx'][2]:+.2f}(SEQp+4)", ""),
        ("Eligibility", "LASIK / KLEx", f"flap {FLAP_UM['LASIK']:.0f} / cap {FLAP_UM['KLEx']:.0f} µm; residual bed >= "
         f"{RSB_MIN_UM:.0f} µm and tissue altered <= {PTA_MAX:.0%}, otherwise PRK", ""),
        ("Eligibility", "PRK", f">= {PRK_REMAIN_MIN_UM:.0f} µm of cornea left, otherwise not treated", ""),
        ("Eyes", "preop SEQ", f"SEQp + N(0, {SEQ_E_SD}), limited to {SEQ_MIN} / {SEQ_MAX} D", "inter-eye correlation about 0.85"),
        ("Eyes", "cylinder magnitude", f"exp(log cyl + N(0, {CYL_LOG_E_SD})), 0.25 D steps, <= {CYL_MAX}", ""),
        ("Eyes", "plus axis", f"WTR / ATR / oblique {AXIS_TYPE_P}; patient SD {AXIS_PATIENT_SD:.0f}, eye SD "
         f"{AXIS_EYE_SD:.0f} deg; OS = 180 - OD + N(0, {AXIS_MIRROR_SD:.0f})", "mirror symmetry"),
        ("Eyes", "pachymetry / mean K", f"+ N(0, {PACHY_E_SD:.0f}) µm / + N(0, {K_E_SD}) D", ""),
        ("Eyes", "optical zone", f"{OZ_MEAN} - {OZ_PER_D}(|SEQp|-4) + N(0, {OZ_SD}) mm, 0.1 mm steps, 5.8-7.0", "patient level"),
        ("Eyes", "depth", f"OZ^2/3 (|SEQ| + |cyl|/2) + N(0, 3) µm; KLEx + {KLEX_MIN_THICK_UM:.0f} µm", "Munnerlyn"),
        ("Eyes", "preop CDVA", f"line of {CDVA_PRE['mean']} + {CDVA_PRE['per_D']}(|SEQ|-4) + N(0, {CDVA_PRE['sd_patient']}) "
         f"patient + N(0, {CDVA_PRE['sd_eye']}) eye logMAR, 20/12.5 to 20/40", ""),
        ("Eyes", "preop UDVA", f"line of CDVA {UDVA_PRE['offset']:+} + {UDVA_PRE['per_D']}|SEQ| + {UDVA_PRE['per_cyl']}|cyl| "
         f"+ N(0, {UDVA_PRE['sd']}) logMAR, never better than CDVA", ""),
        ("Eyes", "preop dry-eye grade", f"cuts {DRY_PRE['cuts']} of {DRY_PRE['age']}(age-34) + {DRY_PRE['female']}[F] "
         f"{DRY_PRE['latent']:+} x dry-eye latent + N(0, {DRY_PRE['sd']})", "linked to the postop dry-eye latent"),
        ("Target", "monovision", f"bilateral patients aged >= {MONOVISION['min_age']}: probability {MONOVISION['p']}, one eye, "
         f"target {MONOVISION['targets']} D with probabilities {MONOVISION['p_targets']}", "all other eyes plano"),
        ("Postop SEQ", "model", f"target {SEQ_OUT['intercept']:+} {SEQ_OUT['Cohort A']:+}[A] {SEQ_OUT['Cohort B']:+}[B] "
         f"{SEQ_OUT['age']:+}(age-34) {SEQ_OUT['seq']:+}(preop SEQ+4) {SEQ_OUT['pachy10']:+}(pachy-545)/10 + u + e", "eye covariates"),
        ("Postop SEQ", "random effects", f"u ~ N(0, {SEQ_OUT['sd_patient']}) patient, e ~ N(0, {SEQ_OUT['sd_eye']}) eye", ""),
        ("Astigmatism", "correction", f"SIA = -CI R(2 AoE) PRE; CI = {CI_MEAN} + N(0, {CI_P_SD}) + N(0, {CI_E_SD}); "
         f"AoE = N(0, {AOE_P_SD}) patient + N(0, {AOE_E_SD}) eye deg", "plus-format double-angle vectors, target plano; "
         "the patient part of AoE and of the residual is mirrored in OS"),
        ("Astigmatism", "postop", f"POST = PRE + SIA + N(0, {POST_VEC_P_SD}) patient + N(0, {POST_VEC_E_SD}) eye per component; "
         "magnitude on 0.25 D", "no cohort term, no confounder"),
        ("CDVA", "lines gained (+) / lost (-)", ", ".join(f"{k:+d}: {v:.1%}" for k, v in CDVA_CHANGE_P.items()),
         f"at preop SEQ -4 D; latent shift {CDVA_SEQ_SHIFT}(-preop SEQ-4); fellow-eye correlation {CDVA_RHO}; postop 20/12.5 at best"),
        ("UDVA", "postop", f"line of CDVA {UDVA_POST['offset']:+} + {UDVA_POST['myopic']}max(-SEQ-{UDVA_POST['myopic_dz']},0) + "
         f"{UDVA_POST['hyperopic']}max(SEQ-{UDVA_POST['hyperopic_dz']},0) + {UDVA_POST['per_cyl']}max(|cyl|-{UDVA_POST['cyl_dz']},0) "
         f"+ N(0, {UDVA_POST['sd_patient']}) patient + N(0, {UDVA_POST['sd_eye']}) eye logMAR", "never better than CDVA"),
        ("Dry eye", "P(yes)", f"expit({DRY['intercept']} + ln2[A] + 0[B] {DRY['seq']:+}(preop SEQ+4) {DRY['age']:+}(age-34) "
         f"{DRY['pachy10']:+}(pachy-545)/10)", f"Gaussian copula, fellow-eye latent correlation {DRY['rho']}"),
        ("Haze", "PRK", f"P(grade <= 0..3) = {HAZE_PRK_CUM} at 60 µm; latent shift +{HAZE_PER_UM} per µm deeper; "
         f"fellow-eye correlation {HAZE_RHO}", f"LASIK / KLEx: grade 1 with probability {HAZE_FLAP_P1}"),
        ("Complications", "per procedure", "; ".join(f"{p}: " + ", ".join(f"{k} {v}" for k, v in d.items())
                                                     for p, d in COMPLICATION_P.items()),
         "Corneal haze = PRK haze grade >= 2"),
        ("Satisfaction", "1-5", f"cuts {SATIS['cuts']} of {SATIS['intercept']} {SATIS['udva']}(UDVA logMAR) "
         f"{SATIS['dry']}[dry eye] + N(0, {SATIS['sd_patient']}) + N(0, {SATIS['sd']})", ""),
        ("Retreatment", "indicated", f"|SEQ - target| >= {RETREAT['seq_err']} D or cylinder >= {RETREAT['cyl']} D: "
         f"p = {RETREAT['p_if_needed']}; otherwise {RETREAT['p_base']}", f"{RETREAT['p_row']:.0%} of them have a retreatment row"),
        ("Retreatment rows", "SurgeryNumber 2", "preop = the 6-month refraction and acuity; LASIK eyes 60% LASIK, all others PRK; "
         "residual SEQ N(+0.025, 0.22) D; no visits recorded", ""),
        ("Visits", "times", ", ".join(f"visit {v}: {d} +/- {j} days" for v, d, j in VISITS),
         f"6-month visit outside 150-210 days in {P_OUT_OF_WINDOW:.0%}"),
        ("Visits", "attendance", " / ".join(f"{a:.0%}" for a in ATTEND), "monotone, per patient, completely at random"),
        ("Visits", "course of SEQ", f"overshoot {OVERSHOOT} x exp(-days/{OVERSHOOT_TAU:.0f}); regression "
         f"{REGRESSION[0]} + {REGRESSION[1]}(preop SEQ+4) D/year", f"visit noise N(0, {VISIT_SD_PATIENT}) patient + N(0, {VISIT_SD_EYE}) eye"),
        ("Missing values", "share of rows", ", ".join(f"{k} {v:.1%}" for k, v in MISSING.items())
         + f"; Smoker {P_SMOKER_MISSING:.0%} of patients", "plus all 6-month data of eyes that missed that visit"),
    ]
    for p in P:
        add(*p)
    add("")
    add("COLUMN DICTIONARY", "unit", "description")
    for name, unit, desc in COLUMNS:
        add(name, unit, desc)
    add("")
    add("Acuity", "decimal notation of whole 0.1-logMAR lines: " + ", ".join(f"{v:g}" for v in DEC_LINE.values()))
    return R


def write_workbook(df, readme, path):
    from openpyxl.styles import Alignment, Font
    with pd.ExcelWriter(path, engine="openpyxl") as xw:
        df.to_excel(xw, sheet_name="Surgeries", index=False)
        pd.DataFrame(readme).to_excel(xw, sheet_name="README", index=False, header=False)
        wb = xw.book
        wb.properties.creator = "make_example_data.py"
        wb.properties.lastModifiedBy = "make_example_data.py"
        ws = wb["README"]
        for col, w in zip("ABCD", (44, 62, 46, 70)):
            ws.column_dimensions[col].width = w
        ws["A1"].font = Font(bold=True, size=14)
        for row in ws.iter_rows():
            v = row[0].value
            if isinstance(v, str) and v.isupper() or (isinstance(v, str) and v.startswith(("CRUDE VERSUS", "THIS FILE"))):
                for c in row:
                    c.font = Font(bold=True)
            for c in row:
                c.alignment = Alignment(wrap_text=True, vertical="top")
        ws.freeze_panes = None
        wb["Surgeries"].freeze_panes = "C2"


def main():
    ap = argparse.ArgumentParser(description=__doc__.split("\n")[1])
    ap.add_argument("--out", default=default_app(),
                    help="folder that receives 'Example Data\\" + FNAME + "' (default: the app folder)")
    a = ap.parse_args()
    ddir = os.path.join(a.out, "Example Data")
    os.makedirs(ddir, exist_ok=True)
    df, eyes = build_dataset()
    sha = hashlib.sha256(df.to_csv(index=False, lineterminator="\n").encode("utf-8")).hexdigest()
    truth = population_truth()
    out = os.path.join(ddir, FNAME)
    write_workbook(df, readme_rows(df, truth, sha), out)

    prim = df[df.SurgeryNumber == 1]
    print("rows (eyes x surgeries):", len(df))
    print("patients:", df.PatientID.nunique(), " eyes:", df.EyeID.nunique(),
          " both eyes:", int((prim.groupby("PatientID").Eye.nunique() == 2).sum()), "patients")
    print("primary rows:", len(prim), " retreatments:", int((df.SurgeryNumber == 2).sum()))
    print("cohort x procedure (primary eyes):")
    print(pd.crosstab(prim.Cohort, prim.Procedure, margins=True).to_string())
    print("plus-notation rows:", int((df.Cylinder_Notation == "plus").sum()),
          f"({(df.Cylinder_Notation == 'plus').mean() * 100:.1f}%)")
    print("--- crude versus true (population) ---")
    for lab, unit, con, t, c, ad in truth[0]:
        print(f"  {lab:28s} {con:22s} true {t:+.3f}  crude {c:+.3f}  adjusted {ad:+.3f}  ({unit})")
    n_rows, est = sample_estimates(df)
    print(f"--- this file ({n_rows} analyzed rows; adjusted estimate and 95% CI) ---")
    for lab, unit, con, t, c, ad, n_eyes, _ in est:
        print(f"  {lab:28s} {con:22s} true {t:+.3f}  adjusted {ad[0]:+.3f} ({ad[1]:+.3f} to {ad[2]:+.3f})  "
              f"{n_eyes} eyes  ({unit})")
    print("data SHA-256:", sha)
    print("saved:", out)


if __name__ == "__main__":
    main()
