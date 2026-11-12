"""
DS-E "Confounded cohorts" - simulated LASIK in a systemic "Condition" cohort
versus "Control" (demonstration dataset E of StandardEyes v1.2.0).
Simulated data - no real patients.

Demonstrates patient-clustered tests, covariate-adjusted GEE, propensity-score
matching at the eye level and at the patient level, the love plot, the matched
tables, and how crude, adjusted and matched estimates compare with the truth.

  * both eyes of most patients (88%), fellow eyes correlated through patient effects
  * MEASURED CONFOUNDING: age, preoperative spherical equivalent and pachymetry
    drive cohort membership (Condition patients are older, less myopic and have
    thinner corneas) AND the outcomes
  * documented TRUE effects of the Condition (given age, preop SEQ, pachymetry):
      postoperative SEQ          -0.20 D (exact, linear model)
      dry-eye symptoms (binary)  odds ratio 2.0 (exact population-averaged)
      postoperative cylinder     no effect (no cohort term; older eyes get a
                                 slightly lower correction index, a negligible bias)
      CDVA lines gained/lost     no effect (depends on preop SEQ only)
  * the crude SEQ contrast is biased toward zero (the confounders push the
    Condition eyes toward hyperopia), the crude odds ratio upward

Usage:  python make_dataset_E.py [--out <folder>]
        (writes Dataset_E_confounded_cohorts.xlsx and its settings files into <folder>; without --out
        into an "out" folder beside this script)
"""
from __future__ import annotations

import os
import sys

import numpy as np
import pandas as pd

import simcore as sc

FNAME = "Dataset_E_confounded_cohorts.xlsx"
GEN = os.path.basename(__file__)
SEED = 2026111109
SEED_POP = 2026111110
N_PATIENTS = 275
N_POP = 200_000

AGE = (37.0, 9.0, 20, 62)
P_FEMALE, P_BILATERAL = 0.55, 0.88
SEQ_P_MEAN, SEQ_P_SD, SEQ_E_SD, SEQ_MIN, SEQ_MAX = -3.8, 1.8, 0.55, -9.0, -0.75
PACHY_P_MEAN, PACHY_P_SD, PACHY_E_SD = 540.0, 28.0, 6.0
K_MEAN, K_P_SD, K_E_SD = 43.7, 1.4, 0.3
CYL_MEDIAN, CYL_LOG_P_SD, CYL_LOG_E_SD, CYL_MAX = 0.65, 0.50, 0.40, 4.0
AXIS_TYPE_P = [0.62, 0.18, 0.20]
C_AGE, C_SEQ, C_PACHY = 37.0, -3.8, 540.0
COHORTS = ["Control", "Condition"]
# logit P(Condition): intercept, per year of age, per D of patient SEQ, per 10 µm of patient pachymetry
COHORT_LOGIT = (-0.80, 0.08, 0.35, -0.30)
OZ = 6.5

# outcomes at the 6-month endpoint (eye level)
SEQ_OUT = {"intercept": 0.05, "Condition": -0.20, "age": 0.015, "seq": 0.08, "pachy10": 0.03,
           "sd_patient": 0.22, "sd_eye": 0.20}
DRY = {"intercept": -1.70, "Condition": float(np.log(2.0)), "age": 0.04, "seq": -0.12, "pachy10": -0.10, "rho": 0.60}
CI = {"mean": 0.95, "age": -0.004, "sd_patient": 0.10, "sd_eye": 0.04}
AOE_P_SD, AOE_E_SD = 3.0, 6.0
NOISE_P_SD, NOISE_E_SD = 0.11, 0.06
CDVA_RHO, CDVA_SEQ_SHIFT = 0.75, 0.10
P_OUT_OF_WINDOW, P_MISSED_6M = 0.03, 0.06
MISSING = {"Preop_Pachymetry_um": 0.01, "Preop_Mean_K_D": 0.015, "Postop_UDVA": 0.01, "Postop_CDVA": 0.015,
           "DryEye_Symptoms": 0.02}

# The matched results StandardEyes 1.2.0 writes for this file with the two settings files (copied from its
# workbooks: PSM_Condition_vs_Control, GEE_M_ and GEE_Adj_M_Condition_vs_Control; not computed here). The
# gate demo_datasets of the tools folder reruns both settings and checks every value below against the run.
APP_MATCHED = {
    "patient": dict(settings="Settings_E_confounded_patient_matching.json", units="patients",
                    matched="74 Condition and 74 Control patients (137 and 140 eyes)",
                    seq="β = -0.17 (95% CI, -0.28 to -0.06)", seq_adj="β = -0.18 (95% CI, -0.28 to -0.08)",
                    dry="OR = 2.01 (95% CI, 1.02 to 3.98)", dry_adj="OR = 2.17 (95% CI, 1.06 to 4.44)",
                    smd=(("Age_Years", 0.09), ("Preop_SEQ_D", 0.17), ("Preop_Pachymetry_um", 0.00)),
                    left=("Preop_SEQ_D", "preop SEQ")),
    "eye": dict(settings="Settings_E_confounded_eye_matching.json", units="eyes",
                matched="144 Condition and 144 Control eyes (88 and 90 patients)",
                seq="β = -0.21 (95% CI, -0.32 to -0.09)", seq_adj="β = -0.21 (95% CI, -0.31 to -0.12)",
                dry="OR = 2.28 (95% CI, 1.15 to 4.49)", dry_adj="OR = 2.32 (95% CI, 1.15 to 4.69)",
                smd=(("Age_Years", 0.11), ("Preop_SEQ_D", 0.07), ("Preop_Pachymetry_um", -0.03)),
                left=("Age_Years", "age")),
}


def matched_note(unit):
    """The sentence on the residual imbalance of one settings file (README and _demonstrates)."""
    m = APP_MATCHED[unit]
    col, word = m["left"]
    smd = dict(m["smd"])[col]
    adj = m["seq_adj"].split(" (")[0].replace("β = ", "")
    return (f"On this file, 1:1 matching of {m['units']} with a caliper of 0.2 SD of the logit leaves {word} at a "
            f"standardized difference of {smd:.2f} ({'patient units' if unit == 'patient' else 'eyes'}; beyond the "
            "love plot's 0.1 line, and the PSM sheet marks it 'no'); the covariate-adjusted matched GEE "
            f"({adj} D for postop SEQ, true -0.20 D) removes that residual imbalance. The caliper is the default and "
            "was not tuned to balance this sample.")


def simulate(rng, n):
    P = sc.patients(rng, n, *AGE, P_FEMALE, P_BILATERAL)
    age, female, pres = P["age"], P["female"], P["present"]
    seq_p = rng.normal(SEQ_P_MEAN, SEQ_P_SD, n)
    pachy_p = rng.normal(PACHY_P_MEAN, PACHY_P_SD, n)
    k_p = rng.normal(K_MEAN, K_P_SD, n)
    lcyl_p = rng.normal(np.log(CYL_MEDIAN), CYL_LOG_P_SD, n)
    pax, _ = sc.axes(rng, n, AXIS_TYPE_P)
    b0, ba, bs, bp = COHORT_LOGIT
    p_cond = sc.expit(b0 + ba * (age - C_AGE) + bs * (seq_p - C_SEQ) + bp * (pachy_p - C_PACHY) / 10)
    cond = (rng.random(n) < p_cond).astype(int)

    seq_lat = np.clip(seq_p[:, None] + rng.normal(0, SEQ_E_SD, (n, 2)), SEQ_MIN, SEQ_MAX)
    mag = sc.cylinder(rng, lcyl_p, CYL_LOG_E_SD, 0.0, CYL_MAX)
    sph, seq = sc.refraction_from_seq(seq_lat, mag)
    pachy = np.round(pachy_p[:, None] + rng.normal(0, PACHY_E_SD, (n, 2)))
    kk = np.round(k_p[:, None] + rng.normal(0, K_E_SD, (n, 2)), 2)
    depth = np.round(OZ ** 2 / 3 * (np.abs(seq) + mag / 2) + rng.normal(0, 3, (n, 2)))
    C = cond[:, None] * 1.0
    agec = (age - C_AGE)[:, None]
    seqc = seq - C_SEQ
    pac10 = (pachy - C_PACHY) / 10

    cd_pre = sc.cdva_pre_latent(rng, seq, n)
    ud_pre = sc.udva_pre_latent(rng, cd_pre, seq, mag, n)
    err = (SEQ_OUT["intercept"] + SEQ_OUT["Condition"] * C + SEQ_OUT["age"] * agec + SEQ_OUT["seq"] * seqc
           + SEQ_OUT["pachy10"] * pac10 + rng.normal(0, SEQ_OUT["sd_patient"], (n, 1))
           + rng.normal(0, SEQ_OUT["sd_eye"], (n, 2)))
    A = sc.astig_post(rng, mag, pax, np.zeros((n, 2)), np.zeros((n, 2)), CI["mean"] + CI["age"] * agec,
                      CI["sd_patient"], CI["sd_eye"], 0.0, AOE_P_SD, AOE_E_SD, NOISE_P_SD, NOISE_E_SD)
    pmag, ppax, psph, pseq = sc.post_refraction(A["qx"], A["qy"], err, CYL_MAX)
    dl = sc.ordinal_change(rng, n, rho=CDVA_RHO, shift=CDVA_SEQ_SHIFT * (-seq - 4))
    cd_pre_line = np.clip(sc.line_of(cd_pre), -2, 3)
    cd_post_line = np.maximum(cd_pre_line - dl, -2)
    ud_pre_line = np.maximum(np.maximum(sc.line_of(ud_pre), cd_pre_line), -2)      # beyond line 13: recorded as CF
    ud_post = sc.udva_post_latent(rng, cd_post_line / 10, pseq, pmag, n)
    ud_post_line = np.clip(np.maximum(sc.line_of(ud_post), cd_post_line), -2, 17)
    p_dry = sc.expit(DRY["intercept"] + DRY["Condition"] * C + DRY["age"] * agec + DRY["seq"] * seqc + DRY["pachy10"] * pac10)
    dry = sc.binary_copula(rng, n, p_dry, DRY["rho"])

    pi, ei = np.nonzero(pres)
    f = lambda v: v[pi, ei]
    return pd.DataFrame(dict(
        pidx=pi, Eye=np.where(ei == 0, "OD", "OS"), Cohort=np.array(COHORTS)[cond[pi]],
        Age_Years=age[pi].astype(int), Sex=np.where(female[pi], "F", "M"), p_cond=p_cond[pi],
        pre_sph=f(sph), pre_mag=f(mag), pre_pax=f(pax), Preop_SEQ_D=f(seq),
        pre_ud=f(ud_pre_line), pre_cd=f(cd_pre_line), Preop_Pachymetry_um=f(pachy), Preop_Mean_K_D=f(kk),
        Ablation_Depth_um=f(depth), err=f(err),
        post_sph=f(psph), post_mag=f(pmag), post_pax=f(ppax), Postop_SEQ_D=f(pseq),
        post_qx=f(A["qx"]), post_qy=f(A["qy"]), post_ud=f(ud_post_line), post_cd=f(cd_post_line), dl=f(dl),
        dry=f(dry),
    ))


def build_dataset(seed=SEED):
    rng = np.random.default_rng(seed)
    e = simulate(rng, N_PATIENTS)
    n = len(e)
    pid = e.pidx.to_numpy()
    df = pd.DataFrame()
    df["PatientID"], df["EyeID"] = sc.ids("E", pid, e.Eye.to_numpy())
    df["Eye"] = e.Eye.to_numpy()
    for c in ("Cohort", "Age_Years", "Sex"):
        df[c] = e[c].to_numpy()
    df["Preop_Sphere_D"], df["Preop_Cylinder_D"], df["Preop_Axis_Deg"] = sc.minus_recorded(
        e.pre_sph.to_numpy(), e.pre_mag.to_numpy(), e.pre_pax.to_numpy())
    df["Preop_SEQ_D"] = e.Preop_SEQ_D.to_numpy()
    df["Preop_UDVA"], df["Preop_CDVA"] = sc.dec_or_off(e.pre_ud), sc.dec_of(e.pre_cd)
    for c in ("Preop_Pachymetry_um", "Preop_Mean_K_D", "Ablation_Depth_um"):
        df[c] = e[c].to_numpy()
    df["Target_SEQ_D"] = 0.0
    day6 = 180 + rng.integers(-25, 26, n)
    oow = rng.random(n) < P_OUT_OF_WINDOW
    day6 = np.where(oow & (rng.random(n) < 0.5), rng.integers(125, 150, n), np.where(oow, rng.integers(211, 245, n), day6))
    df["FollowUp_Days"] = day6.astype(float)
    df["Postop_Sphere_D"], df["Postop_Cylinder_D"], df["Postop_Axis_Deg"] = sc.minus_recorded(
        e.post_sph.to_numpy(), e.post_mag.to_numpy(), e.post_pax.to_numpy())
    df["Postop_SEQ_D"] = e.Postop_SEQ_D.to_numpy()
    df["Postop_UDVA"], df["Postop_CDVA"] = sc.dec_of(e.post_ud), sc.dec_of(e.post_cd)
    df["DryEye_Symptoms"] = np.where(e.dry, "Yes", "No")
    gone = (rng.random(pid.max() + 1) < P_MISSED_6M)[pid]
    for c in [c for c in df.columns if c.startswith("Postop_")] + ["FollowUp_Days", "DryEye_Symptoms"]:
        df[c] = df[c].astype(object).where(~gone, None)
    sc.blank_out(rng, df, MISSING)
    ints = ["Age_Years", "Preop_Axis_Deg", "Postop_Axis_Deg", "Preop_Pachymetry_um", "Ablation_Depth_um", "FollowUp_Days"]
    # Preop_UDVA keeps its "CF" cells (off the chart) beside the decimal numbers
    floats = [c for c in df.columns if c.endswith(("_D", "_um", "VA")) and c not in ints and c != "Preop_UDVA"]
    df = sc.typed(df, ints, floats)
    return df, e


def population_truth():
    pop = simulate(np.random.default_rng(SEED_POP), N_POP)
    C = (pop.Cohort == "Condition").to_numpy(float)
    one = np.ones(len(pop))
    cov = np.column_stack([pop.Age_Years - C_AGE, pop.Preop_SEQ_D - C_SEQ, (pop.Preop_Pachymetry_um - C_PACHY) / 10])
    Xc, Xa = np.column_stack([one, C]), np.column_stack([one, C, cov])
    rows = []
    for lab, y, unit, t in (("Postop SEQ", pop.Postop_SEQ_D.to_numpy(), "D", SEQ_OUT["Condition"]),
                            ("Postop cylinder magnitude", pop.post_mag.to_numpy(), "D", 0.0),
                            ("Postop CDVA", -np.log10(sc.dec_of(pop.post_cd)), "logMAR", 0.0),
                            ("Postop UDVA", -np.log10(sc.dec_of(pop.post_ud)), "logMAR", None)):
        rows.append((lab, unit, t, sc.ols(y, Xc)[1], sc.ols(y, Xa)[1]))
    y = pop.dry.to_numpy(float)
    rows.append(("Dry-eye symptoms", "odds ratio", 2.0, float(np.exp(sc.logit_fit(y, Xc)[1])),
                 float(np.exp(sc.logit_fit(y, Xa)[1]))))
    means = pop.groupby("Cohort")[["Age_Years", "Preop_SEQ_D", "Preop_Pachymetry_um"]].mean()
    sds = pop[["Age_Years", "Preop_SEQ_D", "Preop_Pachymetry_um"]].std()
    share = (pop.groupby("Cohort").pidx.nunique() / pop.pidx.nunique())
    smd = {c: (means.loc["Condition", c] - means.loc["Control", c]) / sds[c] for c in sds.index}
    return dict(rows=rows, means=means, share=share, smd=smd, n_eyes=len(pop))


def sample_estimates(df):
    d = df[pd.to_numeric(df.FollowUp_Days).between(150, 210) | df.FollowUp_Days.isna()]
    num = lambda c: pd.to_numeric(d[c]).astype(float).to_numpy()
    C = (d.Cohort == "Condition").to_numpy(float)
    cov = np.column_stack([num("Age_Years") - C_AGE, num("Preop_SEQ_D") - C_SEQ, (num("Preop_Pachymetry_um") - C_PACHY) / 10])
    pid = d.PatientID.astype(str).to_numpy()
    dry = d.DryEye_Symptoms.map({"Yes": 1.0, "No": 0.0}).astype(float).to_numpy()
    rows = []
    for lab, unit, y, lg, t in (("Postop SEQ", "D", num("Postop_SEQ_D"), False, SEQ_OUT["Condition"]),
                                ("Postop cylinder (signed minus cylinder, as the app analyzes it)", "D", num("Postop_Cylinder_D"), False, 0.0),
                                ("Postop CDVA", "logMAR", -np.log10(num("Postop_CDVA")), False, 0.0),
                                ("Dry-eye symptoms", "odds ratio", dry, True, 2.0)):
        cr, ad, ne, npt = sc.contrast(y, [C], cov, pid, logistic=lg)
        rows.append((lab, unit, t, cr[0], ad[0], ne, npt))
    return len(d), rows


COLUMNS = [
    ("PatientID", "-", "Patient code (no names). Both eyes of a patient share it."),
    ("EyeID", "-", "Eye code = PatientID-Eye."),
    ("Eye", "-", "OD (right) or OS (left)."),
    ("Cohort", "-", "Condition (patients with a simulated systemic condition) or Control (patient level)."),
    ("Age_Years", "years", "Age at surgery (patient level) - a confounder."),
    ("Sex", "-", "F or M (patient level; no effect)."),
    ("Preop_Sphere_D / _Cylinder_D / _Axis_Deg", "D / D / degrees", "Preoperative manifest refraction, minus-cylinder notation; "
     "blank axis when the cylinder is 0."),
    ("Preop_SEQ_D", "D", "Preoperative SEQ = sphere + cylinder/2 exactly - a confounder."),
    ("Preop_UDVA / Preop_CDVA", "decimal", "Preoperative uncorrected / corrected distance acuity (decimal chart lines); an "
     "uncorrected acuity poorer than the chart's last line (0.05 = 20/400) is recorded as CF (counting fingers)."),
    ("Preop_Pachymetry_um", "µm", "Central corneal thickness - a confounder."),
    ("Preop_Mean_K_D", "D", "Mean keratometry (no effect)."),
    ("Ablation_Depth_um", "µm", "Maximum ablation depth (LASIK, 6.5 mm optical zone)."),
    ("Target_SEQ_D", "D", "Intended postoperative SEQ: plano (0) for every eye."),
    ("FollowUp_Days", "days", "Days from surgery to the 6-month (primary endpoint) visit; blank when that visit was missed."),
    ("Postop_Sphere_D / _Cylinder_D / _Axis_Deg", "D / D / degrees", "Refraction at 6 months, minus notation; blank axis "
     "when the cylinder is 0."),
    ("Postop_SEQ_D", "D", "SEQ at 6 months = sphere + cylinder/2 exactly."),
    ("Postop_UDVA / Postop_CDVA", "decimal", "Acuity at 6 months (chart lines)."),
    ("DryEye_Symptoms", "-", "Dry-eye symptoms at 6 months: Yes or No."),
]


def readme(df, truth, sha):
    R = sc.Readme()
    R.add("Simulated data - no real patients")
    R.add("Every value in the sheet 'Surgeries' was generated by a computer model. No person, clinic, disease or real "
          "measurement is represented; 'Condition' is a generic simulated systemic condition. Patients and eyes carry codes only.")
    R.add("Dataset E of the StandardEyes demonstration datasets: 'Confounded cohorts' - a systemic Condition cohort versus "
          "Control, both eyes of most patients, measured confounding, documented true effects.")
    R.blank()
    sc.source_rows(R, GEN, SEED, df, sha, f"population table: seed {SEED_POP}")
    g = df.groupby("Cohort")
    bil = df.groupby("PatientID").Eye.nunique()
    R.add("Size", f"{df.PatientID.nunique()} patients, {len(df)} eyes (one row per eye); {(bil == 2).sum()} patients "
          f"({(bil == 2).mean():.0%}) contribute both eyes",
          "; ".join(f"{k}: {len(d)} eyes of {d.PatientID.nunique()} patients" for k, d in g))
    R.add("Settings files", "Settings_E_confounded_eye_matching.json; Settings_E_confounded_patient_matching.json",
          "identical except psm_unit: 'eye' (each eye a matching unit) or 'patient' (both eyes of a patient matched "
          "together, StandardEyes 1.2.0); StandardEyes 1.1.0 does not know psm_unit and matches eyes in both")
    R.blank()
    R.add("TRUE EFFECTS OF THE CONDITION", "estimand", "true value", "note")
    R.add("Postop SEQ", "mean difference given age, preop SEQ and pachymetry", f"{SEQ_OUT['Condition']:+.2f} D", "linear model; exact")
    R.add("Dry-eye symptoms", "population-averaged odds ratio given age, preop SEQ and pachymetry", "2.0",
          "exact: drawn through a Gaussian copula from the logistic model")
    R.add("Postop cylinder (magnitude, vector)", "any", "no effect (0)",
          f"no cohort term; the correction index falls by {abs(CI['age'])} per year of age, too little to make the crude "
          "difference visibly non-zero (see the population table)")
    R.add("CDVA lines gained / lost, postop CDVA", "any", "no effect (0)", "depends on preop SEQ only (confounded null)")
    R.add("Postop UDVA", "direct effect", "none", "follows the residual refraction, so it differs through the SEQ effect")
    R.blank()
    R.add(f"CONFOUNDING AND CRUDE VERSUS TRUE (population of {N_POP:,} simulated patients, {truth['n_eyes']:,} eyes, seed {SEED_POP})",
          "true / crude / adjusted", "", "adjusted = linear or logistic model with age, preop SEQ and pachymetry (eyes as units)")
    for lab, unit, t, c, a in truth["rows"]:
        f = (lambda v: f"{v:.2f}") if unit == "odds ratio" else (lambda v: f"{v:+.3f}")
        R.add(f"{lab} ({unit})", " / ".join(f(v) if v is not None else "-" for v in (t, c, a)), "",
              "UDVA has no direct effect; its true total effect runs through the SEQ effect" if lab == "Postop UDVA" else "")
    for c in COHORTS:
        m = truth["means"].loc[c]
        R.add(f"Population means, {c}", f"share of patients {truth['share'][c]:.1%}",
              f"age {m.Age_Years:.1f} y; preop SEQ {m.Preop_SEQ_D:+.2f} D; pachymetry {m.Preop_Pachymetry_um:.0f} µm",
              "the measured confounders")
    R.add("Standardized differences (Condition - Control)", ", ".join(f"{k} {v:+.2f}" for k, v in truth["smd"].items()), "",
          "population, eye level")
    R.blank()
    n_rows, est = sample_estimates(df)
    R.add(f"THIS FILE (the {n_rows} eyes the settings analyze: 6-month visit at 150-210 days or none)", "unit",
          "crude / adjusted (95% CI)", "linear or logistic model of the eyes, robust SE clustered by patient, 95% CI = estimate "
          "+/- 1.96 SE, adjusted for age, preop SEQ and pachymetry (the settings' covariates); the app's GEE with its small-sample "
          "correction gives slightly different intervals; the matched estimates are the app's own output (next block)")
    for lab, unit, t, c, a, ne, npt in est:
        odds = unit == "odds ratio"
        f = (lambda v: f"{v:.2f}") if odds else (lambda v: f"{v:+.3f}")
        R.add(lab, unit, " / ".join(f"{f(e)} ({f(lo)} to {f(hi)})" for e, lo, hi in (c, a)), f"true {f(t)}; {ne} eyes of {npt} patients")
    R.blank()
    R.add("MATCHED ESTIMATES (StandardEyes 1.2.0 output with the two settings files; copied from its workbooks)",
          "matched GEE / covariate-adjusted matched GEE", "standardized differences after matching", "note")
    for unit in ("eye", "patient"):
        m = APP_MATCHED[unit]
        smd = ", ".join(f"{c} {v:.2f}" for c, v in m["smd"])
        R.add(f"{m['settings']} ({m['units']} matched)",
              f"Postop SEQ: {m['seq']} / {m['seq_adj']} D (true -0.20 D); dry-eye symptoms: {m['dry']} / {m['dry_adj']} "
              "(true 2.0)", f"{smd} ({'on the patient units' if unit == 'patient' else 'on the eyes'})",
              f"{m['matched']} matched. " + matched_note(unit))
    R.blank()
    R.add("GENERATING MODEL", "parameter", "value", "meaning")
    b0, ba, bs, bp = COHORT_LOGIT
    P = [
        ("Patients", "age / female / both eyes", f"N({AGE[0]}, {AGE[1]}) rounded {AGE[2]}-{AGE[3]} / {P_FEMALE} / {P_BILATERAL}", ""),
        ("Patients", "latent preop SEQ / pachymetry / mean K", f"N({SEQ_P_MEAN}, {SEQ_P_SD}) D / N({PACHY_P_MEAN:.0f}, {PACHY_P_SD:.0f}) µm / "
         f"N({K_MEAN}, {K_P_SD}) D", "patient means"),
        ("Patients", "latent log cylinder", f"N(log {CYL_MEDIAN}, {CYL_LOG_P_SD})", "patient mean"),
        ("Cohort membership", "logit P(Condition)", f"{b0:+.2f} {ba:+.2f}(age-37) {bs:+.2f}(SEQp+3.8) {bp:+.2f}(PACHYp-540)/10",
         "on the patient means - the measured confounding"),
        ("Eyes", "preop SEQ", f"SEQp + N(0, {SEQ_E_SD}), limited to {SEQ_MIN} / {SEQ_MAX} D", "sphere on 0.25 D, SEQ derived"),
        ("Eyes", "cylinder / axis", f"exp(log cyl + N(0, {CYL_LOG_E_SD})), 0.25 D, <= {CYL_MAX}; plus axis WTR / ATR / oblique {AXIS_TYPE_P}", "OS mirror image"),
        ("Eyes", "pachymetry / K / depth", f"+ N(0, {PACHY_E_SD:.0f}) µm / + N(0, {K_E_SD}) D / {OZ}^2/3 (|SEQ| + |cyl|/2) + N(0, 3) µm", ""),
        ("Postop SEQ", "model", f"{SEQ_OUT['intercept']:+} {SEQ_OUT['Condition']:+}[Condition] {SEQ_OUT['age']:+}(age-37) "
         f"{SEQ_OUT['seq']:+}(preop SEQ+3.8) {SEQ_OUT['pachy10']:+}(pachy-540)/10 + u + e (target plano)",
         f"u ~ N(0, {SEQ_OUT['sd_patient']}) patient, e ~ N(0, {SEQ_OUT['sd_eye']}) eye; eye-level covariates"),
        ("Dry eye", "P(yes)", f"expit({DRY['intercept']} + ln2[Condition] {DRY['age']:+}(age-37) {DRY['seq']:+}(preop SEQ+3.8) "
         f"{DRY['pachy10']:+}(pachy-540)/10)", f"Gaussian copula, fellow-eye latent correlation {DRY['rho']}"),
        ("Astigmatism", "correction", f"SIA = -CI R(2 AoE) PRE; CI = {CI['mean']} {CI['age']:+}(age-37) + N(0, {CI['sd_patient']}) + "
         f"N(0, {CI['sd_eye']}); AoE = N(0, {AOE_P_SD}) + N(0, {AOE_E_SD}) deg; POST = PRE + SIA + N(0, {NOISE_P_SD}) + N(0, {NOISE_E_SD})",
         "no cohort term"),
        ("CDVA", "lines gained (+) / lost (-)", sc.cdva_change_text(),
         f"at preop SEQ -4 D; latent shift {CDVA_SEQ_SHIFT}(-preop SEQ-4); fellow-eye correlation {CDVA_RHO}; no cohort term"),
        ("UDVA", "preop / postop", "dataset A model: preop from CDVA and the refraction; postop CDVA + blur of the residual "
         "SEQ and cylinder", f"no direct cohort term; preop UDVA {sc.UDVA_OFF_NOTE}"),
        ("Visits", "6-month visit", f"180 +/- 25 days; outside 150-210 days in {P_OUT_OF_WINDOW:.0%}; missed by {P_MISSED_6M:.0%} of patients", ""),
        ("Missing values", "share of rows", ", ".join(f"{k} {v:.1%}" for k, v in MISSING.items()),
         "completely at random; an eye without pachymetry drops out of the matching and the adjusted models"),
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
    conf = ["Age_Years", "Preop_SEQ_D", "Preop_Pachymetry_um"]
    out = []
    for unit, lab in (("eye", "eye"), ("patient", "patient")):
        cfg = sc.settings(
            FNAME, group_source_var="Cohort",
            baseline={"continuous": ["Age_Years", "Preop_SEQ_D", "Preop_Sphere_D", "Preop_Cylinder_D", "Preop_UDVA", "Preop_CDVA",
                                     "Preop_Pachymetry_um", "Preop_Mean_K_D"], "ordinal": [], "dichotomous": ["Sex"], "categorical": []},
            preop={"continuous": ["Ablation_Depth_um"], "ordinal": [], "dichotomous": [], "categorical": []},
            outcome={"continuous": ["FollowUp_Days", "Postop_SEQ_D", "Postop_Cylinder_D", "Postop_UDVA", "Postop_CDVA"],
                     "ordinal": [], "dichotomous": ["DryEye_Symptoms"], "categorical": []},
            groups={"Control": ["Control"], "Condition": ["Condition"]}, reference_group="Control",
            group_colors={"Control": "#0072B2", "Condition": "#D55E00"},
            do_matching=True, psm_covariates_raw=list(conf), psm_method="nearest", psm_distance="linear.logit",
            psm_ratio=1, psm_replace=False, psm_caliper=0.2, psm_std_caliper=True, psm_m_order="largest",
            psm_unit=unit,
            run_gee_stage="both", run_main_gee=True, run_adjusted_gee=True, gee_adjust_covariates_raw=list(conf),
            use_matching_weights_in_gee=True, do_regression=True, regression_stage="both",
            regression_covariates_raw=list(conf),
            dichotomous_preferred_level={"Sex": "F", "DryEye_Symptoms": "Yes"},
            output_prefix="DS_E_pat" if unit == "patient" else "DS_E_eye")
        out.append(sc.write_settings(
            f"Settings_E_confounded_{lab}_matching.json", cfg,
            f"Dataset E - confounded cohorts, {lab}-level matching",
            "Condition versus Control with measured confounding (age, preop SEQ, pachymetry): crude tables, covariate-adjusted "
            "GEE and regression, and 1:1 propensity-score matching " + ("of eyes." if unit == "eye" else
                                                                        "of patients (both eyes kept together)."),
            "Patient-clustered tests, the adjusted GEE, propensity-score matching (nearest neighbor 1:1 without replacement, "
            "caliper 0.2 SD of the logit of the score) with the love plot and the matched tables, and crude, adjusted and matched "
            "estimates side by side against the documented true effects (SEQ -0.20 D, dry-eye OR 2.0, no cylinder effect). "
            + matched_note(unit),
            notes=("psm_unit = 'patient' matches (patient, group) units with both eyes kept together; it is a StandardEyes 1.2.0 "
                   "setting - StandardEyes 1.1.0 ignores it and matches eyes." if unit == "patient" else
                   "psm_unit = 'eye' (the default, and the 1.1.0 behavior) makes each eye a matching unit.")))
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
    print(df.groupby("Cohort").agg(eyes=("EyeID", "size"), patients=("PatientID", "nunique"), age=("Age_Years", "mean"),
                                   seq=("Preop_SEQ_D", "mean"), pachy=("Preop_Pachymetry_um", "mean")).to_string())
    for r in truth["rows"]:
        print("population", r)
    print(truth["means"], truth["share"].to_dict(), truth["smd"])
    for r in sample_estimates(df)[1]:
        print("file", r[0], r[2], tuple(round(x, 3) for x in r[3]), tuple(round(x, 3) for x in r[4]))
    print("data SHA-256:", sha)
    return 0


if __name__ == "__main__":
    sys.exit(main())
