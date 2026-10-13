"""
Single source for the worked example configurations that ship with the app.

Writes each one twice:
  Example Data\\Settings_<name>.json      - standalone, runnable from the CLI
  webapp\\presets\\Example - <name>.json  - clickable inside the app

Paths are RELATIVE to the app folder, which is the working directory for both
the launcher and the Rscript subprocess, so the files keep working wherever the
folder is copied. Preset filenames avoid ( ) & which _safe_preset_name rewrites.

Usage:  python make_example_settings.py [fmt]               (writes into the app)
        python make_example_settings.py [fmt] --out <folder>
            (staging: <folder>\\Example Data\\Settings_*.json and <folder>\\presets\\Example - *.json)

Dataset version 2 (v1.1.0): cohorts "Reference", "Cohort A", "Cohort B"; the 6-month
primary endpoint (FollowUp_Days in a 150-210 day window); the target SEQ column is
mapped; Ablation_or_Lenticule_Depth_um replaces Ablation_Depth_um; the adjusted
models use the three measured confounders (age, preop SEQ, pachymetry).
"""
from __future__ import annotations
import argparse, json, os

HERE = os.path.dirname(os.path.abspath(__file__))


def default_app():
    """The app folder: the parent when this script ships inside the app's
    validation\\ folder, otherwise the sibling "App - StandardEyes"."""
    up = os.path.dirname(HERE)
    if os.path.basename(HERE).lower() == "validation" and os.path.isdir(os.path.join(up, "engine")):
        return up
    return os.path.join(up, "App - StandardEyes")


APP = default_app()
DATA_REL = "Example Data/Example_Refractive_Dataset.xlsx"
CONFOUNDERS = ["Age_Years", "Preop_SEQ_D", "Preop_Pachymetry_um"]

# ---- the five mapped follow-up visits -----------------------------------------
# SEQ is supplied for visits 1 and 3 and omitted for the rest, so a single run
# exercises both the "use the SEQ column" and "compute from sphere + cylinder" paths.
def visit(n, label, seq=False):
    v = {"label": label, "time": f"Visit{n}_Days_PostOp",
         "sph": f"Visit{n}_Sphere_D", "cyl": f"Visit{n}_Cylinder_D", "seq": ""}
    if seq:
        v["seq"] = f"Visit{n}_SEQ_D"
    return v

VISITS = [visit(1, "1", seq=True), visit(2, "3"), visit(3, "6", seq=True),
          visit(4, "12"), visit(5, "24")]

STABILITY_ON = {
    "stability_figs": True,
    "stability_visits": VISITS, "stability_include_preop": True,
    "stability_change_from": "6", "stability_change_to": "24",
    "stability_change_threshold": 0.50,
}

BASE = {
    "file_path": DATA_REL, "sheet_name": 1, "guess_max": 120000,
    "patient_id_raw": "PatientID", "eye_id_raw": "EyeID", "group_source_var": "Cohort",
    # recorded once per person (the dataset's README sheet): the optical zone and
    # the surgeon are patient level in the generating model, so they are analyzed
    # per patient too (the surgeon by the chi-squared test of one row per patient)
    "patient_level_params_raw": ["Age_Years", "Sex", "Smoker", "Optical_Zone_mm", "Surgeon"],
    "exclude_params_raw": [],
    "age_var": "Age_Years", "fu_var": "FollowUp_Days", "sex_var": "Sex",
    "baseline": {
        "continuous": ["Age_Years", "Preop_SEQ_D", "Preop_Sphere_D", "Preop_Cylinder_D",
                       "Preop_UDVA", "Preop_CDVA", "Preop_Pachymetry_um", "Preop_Mean_K_D"],
        "ordinal": ["Preop_DryEye_Grade"], "dichotomous": ["Sex", "Smoker"],
        "categorical": ["Surgeon"]},
    "preop": {"continuous": ["Optical_Zone_mm", "Ablation_or_Lenticule_Depth_um"],
              "ordinal": [], "dichotomous": [], "categorical": ["Procedure"]},
    "outcome": {
        "continuous": ["FollowUp_Days", "Postop_SEQ_D", "Postop_Cylinder_D",
                       "Postop_UDVA", "Postop_CDVA"],
        "ordinal": ["Postop_Haze_Grade", "Patient_Satisfaction_1to5"],
        "dichotomous": ["DryEye_Symptoms", "Retreatment_Required"],
        "categorical": ["Complication_Type"]},
    "within_group": {"continuous": [], "ordinal": [], "dichotomous": [], "categorical": []},
    "within_group_levels": {},
    # the 6-month primary endpoint: a visit outside 150-210 days is excluded (an eye
    # with no 6-month visit has a blank FollowUp_Days and is kept, with missing outcomes)
    "filter_rules": [
        {"var": "Age_Years", "op": ">=", "value": [18]},
        {"var": "FollowUp_Days", "op": "between", "value": [150, 210]},
        {"var": "SurgeryNumber", "op": "==", "value": [1]},
        {"var": "Preop_Cylinder_D", "op": "abs_le", "value": [5]}],
    "groups": {"Reference": ["Reference"], "Cohort A": ["Cohort A"]},
    "reference_group": "Reference",
    "group_colors": {"Reference": "#5B2C86", "Cohort A": "#F26522", "Cohort B": "#1B9E4B"},
    "group_labels": {},
    "do_matching": False, "psm_covariates_raw": [],
    "psm_method": "nearest", "psm_distance": "linear.logit", "psm_ratio": 2,
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
    "std_sph_raw": "Preop_Sphere_D", "std_cyl_raw": "Preop_Cylinder_D",
    "std_axis_raw": "Preop_Axis_Deg",
    "do_logmar": True,
    "logmar_cols_raw": ["Preop_UDVA", "Preop_CDVA", "Postop_UDVA", "Postop_CDVA"],
    "make_meyestro": True, "make_alpins": True, "make_pub_table": True,
    "make_graphs": True, "graph_set": "nine", "graph_formats": ["tiff"], "graph_dpi": 300,
    # the StandardEyes figure options (all groups on one chart; JCRS astigmatism)
    "astig_panels": True, "double_angle_figs": True, "semicircle_figs": True,
    "astig_region": "polygon",         # JCRS 2025: nonparametric, not the ellipse
    "astig_jcrs_standard": True, "astig_test_method": "robust",
    "cluster_by_patient": True,
    "stability_figs": False, "stability_visits": [],
    "roles": {
        "pre_ucva": "Preop_UDVA", "pre_bcva": "Preop_CDVA",
        "post_ucva": "Postop_UDVA", "post_bcva": "Postop_CDVA",
        "pre_sph": "Preop_Sphere_D", "pre_cyl": "Preop_Cylinder_D", "pre_axis": "Preop_Axis_Deg",
        "post_sph": "Postop_Sphere_D", "post_cyl": "Postop_Cylinder_D",
        "post_axis": "Postop_Axis_Deg", "pre_seq": "Preop_SEQ_D",
        "post_seq": "Postop_SEQ_D", "target_seq": "Target_SEQ_D", "eye": "Eye"},
    "dichotomous_preferred_level": {
        "Sex": "F", "Smoker": "Yes", "DryEye_Symptoms": "Yes", "Retreatment_Required": "Yes"},
    "output_dir": "Example Output", "export_filtered_csv": True,
}

FULL_STATS = {
    "do_matching": True,
    "psm_covariates_raw": ["Age_Years", "Preop_SEQ_D", "Preop_Pachymetry_um", "Preop_UDVA"],
    "run_gee_stage": "both", "run_main_gee": True, "run_adjusted_gee": True,
    # the three measured confounders of the simulated dataset (its README sheet)
    "gee_adjust_covariates_raw": list(CONFOUNDERS),
    "do_regression": True, "regression_stage": "both",
    "regression_covariates_raw": list(CONFOUNDERS),
}

THREE = {"groups": {"Reference": ["Reference"], "Cohort A": ["Cohort A"],
                    "Cohort B": ["Cohort B"]}}

# Within-group distribution tables, showing the per-variable unit of analysis:
# the two variables recorded once per PERSON are summarised per patient (so a
# patient with both eyes is counted once); the two eye-level measurements stay
# per eye. Every variable type is represented.
WITHIN = {
    "within_group": {
        "continuous": ["Preop_Pachymetry_um"],
        "ordinal": ["Preop_DryEye_Grade"],
        "dichotomous": ["Smoker"],
        "categorical": ["Surgeon"],
        "order": [],
    },
    "within_group_levels": {"Smoker": "patient", "Surgeon": "patient"},
}


def cfg(**over):
    c = json.loads(json.dumps(BASE))
    for k, v in over.items():
        c[k] = v
    c["num_groups"] = len(c["groups"])
    c["num_filters"] = len(c["filter_rules"])
    c["num_subsets"] = len(c["subsets"])
    return c


EXAMPLES = [   # name, title, what it is, what it shows, settings
    ("01_Two_cohorts_simple",
     "Two groups, tables and figures only",
     "Two groups compared with the standard tests (P values clustered by patient). No matching, "
     "GEE sheets or regression.",
     "The smallest useful run: the group summary tables and the comparative figure set.",
     cfg(output_prefix="Ex01_Simple")),

    ("02_Two_cohorts_full",
     "Two groups, full analysis",
     "Two groups with every statistical step and every figure option.",
     "Propensity-score matching, GEE clustered on the patient, covariate-adjusted "
     "regression, the full comparative figure set with double-angle plots and the "
     "stability panel, and within-group tables analyzed per patient or per eye.",
     cfg(output_prefix="Ex02_Full", **FULL_STATS, **STABILITY_ON, **WITHIN)),

    ("03_Three_cohorts",
     "Three groups",
     "Three groups: an overall test for each variable, and one adjusted estimate for "
     "each group against the reference group.",
     "Shows the omnibus tests with three groups (patient-clustered GEE; Welch ANOVA or the "
     "chi-squared test for the per-patient variables), each group against the reference "
     "Holm-adjusted, the adjusted effect of each comparison, and all three groups on every "
     "figure in their own colors.",
     cfg(output_prefix="Ex03_Three", **THREE, **FULL_STATS, **STABILITY_ON)),

    ("04_Subsets_PRK_LASIK",
     "Subsets: PRK and LASIK",
     "One preset that produces a complete, separate analysis for each procedure.",
     "The subset column splits the run into a full PRK analysis and a full LASIK analysis.",
     cfg(output_prefix="Ex04_Subsets", subset_col_raw="Procedure",
         subsets={"PRK": ["PRK"], "LASIK": ["LASIK"]}, **FULL_STATS, **STABILITY_ON)),

    ("05_Stability_focus",
     "Stability over time",
     "The stability-over-time curve as its own figure panel.",
     "Five mapped follow-up visits and the preoperative point. The spherical equivalent "
     "is read from a column for visits 1 and 3, and computed from sphere and cylinder "
     "for the others.",
     cfg(output_prefix="Ex05_Stability", **STABILITY_ON)),

    ("06_Figures_only_no_stats",
     "Figure options",
     "Figure switches: astigmatism panels and vector plots off, stability on.",
     "Shows the combined figure without the astigmatism panels: seven panels (vision, "
     "efficacy, safety, SEQ accuracy, attempted vs achieved, defocus equivalent, "
     "stability), lettered A to G. No matching, GEE or regression sheets.",
     cfg(output_prefix="Ex06_Figures", astig_panels=False,
         double_angle_figs=False, semicircle_figs=False, **STABILITY_ON)),

    ("07_Full_figure_suite",
     "All figures, three groups",
     "All three groups on one chart, and double-angle astigmatism plots.",
     "Every group in its own color, an overall test and each group against the "
     "reference group on every panel, P values clustered by patient, and double-angle "
     "plots with each group's centroid, its 95% confidence region and a 95% region of "
     "the eyes, following the JCRS 2025 astigmatism standard.",
     cfg(output_prefix="Ex07_Suite", **THREE, **FULL_STATS, **STABILITY_ON)),
]


def main(app=APP, fmt=None, quiet=False, out=None):
    """Write into the app (default) or, with `out`, into a staging folder:
    out\\Example Data\\Settings_*.json and out\\presets\\Example - *.json (no
    other copy is written then)."""
    staging = out is not None
    ddir = os.path.join(out if staging else app, "Example Data")
    pdir = os.path.join(out, "presets") if staging else os.path.join(app, "webapp", "presets")
    os.makedirs(ddir, exist_ok=True); os.makedirs(pdir, exist_ok=True)
    # clear previously shipped examples so renames cannot leave orphans
    for d, pref in ((ddir, "Settings_"), (pdir, "Example - ")):
        for f in os.listdir(d):
            if f.startswith(pref) and f.endswith(".json"):
                os.remove(os.path.join(d, f))
    written = []
    for name, title, desc, demo, c in EXAMPLES:
        c = dict(c)
        c["_title"] = title
        c["_description"] = desc
        c["_demonstrates"] = demo
        if fmt:
            c["graph_formats"] = [fmt]
            c["graph_dpi"] = 150
        paths = [os.path.join(ddir, f"Settings_{name}.json"),
                 os.path.join(pdir, f"Example - {name}.json")]
        # the manuscript's copy follows the shipped app only, and only in the
        # project tree (a copy run from a release's validation\ folder has no
        # Publication folder beside the app and must not create one)
        pub = os.path.abspath(os.path.join(app, "..", "Publication"))
        if not staging and os.path.isdir(pub):
            proj = os.path.join(pub, "3 - Settings")
            os.makedirs(proj, exist_ok=True)
            paths.append(os.path.join(proj, f"Settings_{name}.json"))
        for path in paths:
            with open(path, "w", encoding="utf-8") as fh:
                json.dump(c, fh, indent=2)
        written.append(name)
        if not quiet:
            print(f"  {name:26s} groups={len(c['groups'])} subsets={len(c['subsets'])} "
                  f"stability={'on' if c.get('stability_figs') else 'off'}")
    return written


if __name__ == "__main__":
    ap = argparse.ArgumentParser(description="Write the example settings and webapp presets.")
    ap.add_argument("fmt", nargs="?", default=None, help="optional graph format for every example (e.g. png)")
    ap.add_argument("--out", default=None,
                    help="staging folder (default: write into the app folder, as before)")
    a = ap.parse_args()
    print("wrote example settings (x2 each):")
    main(fmt=a.fmt, out=a.out)
