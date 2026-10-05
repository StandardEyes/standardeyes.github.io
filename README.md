# StandardEyes

**Standardized comparative analysis and outcomes reporting for refractive surgery — free, local, reproducible.**

StandardEyes takes a spreadsheet with **one row per eye per surgery** and produces publication-ready
comparative outcomes for any number of groups: group tables with automatic test selection,
propensity-score matching with balance reporting (matching weights carried into the matched tables
and figures), GEE clustered on the patient, covariate-adjusted regression, Excel workbooks, and the
StandardEyes figures — the published standardized-graph metrics with every cohort superimposed on one
chart, plus JCRS-2025 double-angle astigmatism plots. Visual acuity is read in the notation the file
uses — decimal, logMAR, Snellen (20/x or 6/x) or ETDRS letters — and is analysed in logMAR by default
and reported as the Snellen geometric mean with the logMAR mean ± SD. P values follow the journal style
(.27, <.001), and the figures are drawn in a journal style (no frames or shading; titles and abbreviations in
a legend file beside each figure) as TIFF, PNG, JPEG, PDF or EPS up to 1200 dpi, optionally at a journal
column width (a combined figure then has two panels per row - one at 8.76 cm - and is split into pages
`_p1`, `_p2` when taller than a 23-cm page, the panel letters continuing). In EPS files the
semi-transparent marks are embedded as images at 1200 dpi or more, since PostScript has no transparency.

Everything runs on your own computer. Nothing is uploaded, and no internet connection is needed.

**➡ Website and download: https://standardeyes.github.io**

## What is new in 1.1.0

Version 1.1.0 changes several defaults, so the same data and settings can give different numbers than
1.0.0. A settings file or preset saved by 1.0.0 still loads and runs; a setting it does not contain takes
the 1.1.0 default, and every workbook's Manifest sheet records the settings used. Each change, and how to
get the 1.0.0 behaviour back (interface tab, then the settings-file key):

| What changed in 1.1.0 | To get the 1.0.0 behaviour |
|---|---|
| **Tests in the tables are clustered by patient.** When a patient has two analysed eyes, the P value of every per-eye row of the summary tables and the Publication Table comes from a patient-clustered GEE test - the same P value as the figure of that outcome. | GEE tab, *Summary-table tests* = classical (`table_tests: "classical"`); switching off *Cluster p-values by patient* also does it. |
| **Small-sample correction of the clustered tests.** Every GEE test and interval uses the Mancl-DeRouen bias-corrected robust standard error with a t reference (F for joint tests; df = patients minus model coefficients). | GEE tab, *GEE small-sample inference* = none (`gee_small_sample: "none"`). |
| **The caliper is on the logit scale.** The propensity score is matched on the logit scale, so the default caliper is 0.2 SD of the logit of the propensity score (Austin 2011). | Matching tab, *Distance* = propensity score, probability scale (`psm_distance: "logit"`). |
| **Visual acuity is analysed in logMAR and shown as Snellen.** The mapped acuity columns are analysed in logMAR; a cell reads "20/20 (0.00 ± 0.05 logMAR)" (Snellen geometric mean, logMAR mean ± SD) with the range in Snellen. | Variables & Types tab, untick *Analyse visual acuity in logMAR* (`do_logmar: false`). |
| **P values in journal style**: .27, .003, <.001, >.99. | GEE tab, *P value style* = classic (`p_style: "classic"`). |
| **Journal-style figures**: white, no frames, section bands, captions or shaded wedges; each figure's title, method notes and abbreviations go to `<figure>_legend.txt` beside it. | Special outputs tab, *Figure style* = annotated (`figure_style: "annotated"`). |
| **The astigmatism plots follow the JCRS 2025 standard** in plus-cylinder format (with-the-rule on the left): the double-angle set is preoperative astigmatism, SIA and the prediction error (PE = postoperative - target), plus postoperative astigmatism when a target is not plano; the correction-index polar plots are gone (its histogram stays). The 95% region of the eyes is the depth-based convex polygon and the centroid's 95% confidence region the bootstrap convex polygon of the centroid, both built as in the WHWK functions (the journal's reference tool); each centroid is tested against zero by the bootstrap projection-distance test of the same functions (P = .001 is its floor with 999 resamples); the robust tests resample patients, and their 20% trimming and winsorizing follow the standard order-statistic definitions (Wilcox). | Plus-cylinder vectors and the panel set have no switch (1.0.0 mishandled the cylinder sign). Special outputs tab: *95% region of the eyes* = normal (`astig_region`), *Centroid 95% confidence region* = hotelling (`astig_centroid_region`), *Astigmatism tests* = classic (`astig_test_method`). Hull peeling is no longer offered. |
| **New sheets**: "Astigmatism (JCRS 2025)" (centroids, total SD, magnitudes, intervals and the standard's tests) and "Astigmatism vectors" (per eye); the "Astigmatism (JCRS 2025)" sheet is also written as a CSV beside the workbook. | Special outputs tab, untick *Astigmatism (JCRS 2025) sheet* (`astig_sheet: false`). |
| **Input files for the WHWK functions**: with the double-angle plots, a `WHWK input` folder inside the run's figure folder (`Graphs_<prefix>_<run>_<time>`, beside the figures; the run log gives its full path) holds, for each plotted vector, `<vector>_vector.txt` (`<Cohort>.x`, `<Cohort>.y`, plus-cylinder double-angle form) and `<vector>_magnitude.txt`, tab-delimited, with a README. The Wilcox-Holladay-Wang-Koch R functions are obtained from https://osf.io/xhe8u/; they treat eyes as independent and refuse values beyond 6 D. | Special outputs tab, untick *Input files for the WHWK functions* (`astig_whwk_files: false`). |
| **GEE models of an outcome that hardly varies within a patient.** A variable marked "per patient" is modeled on one row per patient by an ordinary linear or logistic model on the GEE and Regression sheets, never by an eye-level GEE. Any other exchangeable GEE is kept only if it converged, its working correlation is below 0.95 and one further scoring step moves no coefficient by more than 0.01 SE; otherwise it is refitted with an independence working correlation (robust SE still clustered by patient). The sheets, tables, figures and run log name the model used. | No switch. |

Also new: EPS figure files, up to 1200 dpi, and a figure width in cm (`figure_width_cm`; 8.76, 12.69 and 17.77 cm
are the journal column widths). Without a switch: comparisons with the reference in the tables are
Holm-adjusted; tables use fixed decimals and one effect style; matched cohorts use an independence working
correlation; the Regression sheet also gives the crude estimate on the same eyes; workbooks name
"StandardEyes" as their author.

## Download and run (Windows)

1. Install [R 4.5.2 for Windows](https://cran.r-project.org/bin/windows/base/old/4.5.2/) if R 4.5 is not already installed. The main CRAN
   page now offers R 4.6, which may not load the bundled packages; several R versions can be installed side by side.
2. Download `StandardEyes-windows.zip` from the [latest release](https://github.com/StandardEyes/standardeyes.github.io/releases/latest)
   and extract the **whole folder** to a folder you can write to (e.g. `C:\StandardEyes` or Documents —
   not `C:\Program Files`). Unblocking the ZIP first (right-click → Properties → Unblock) avoids the
   SmartScreen prompt on first start.
3. Double-click **`Start StandardEyes.bat`**. Your browser opens on a free local port.

The release ZIP is for Windows 10/11 (64-bit) only. The Python runtime for the interface and all 48 R
packages the analysis needs are inside it, together with their licence texts (`licenses/` and
`THIRD_PARTY_NOTICES.md`); nothing is installed. See [`README.txt`](README.txt) for the full user notes and
troubleshooting, and the `Example Data` folder for a made-up dataset with seven worked settings files
(also included in the app as the seven *Example - …* presets).

The app shows its version as `v1.1.0 (build <id>)`: the release number used on the website and in
`CITATION.cff`, then the internal build id. Every workbook's Manifest sheet records both - also when the
engine runs from the command line, since the engine reads them from `webapp/server/app.py` - with the run duration.

## What is in this repository

| Folder | Contents |
|---|---|
| `R/` | The statistical engine (analysis, pipeline, export, figures) |
| `engine/` | Headless R entry points (`run_pipeline.R`, `match_preview.R`) and the decimal-to-Snellen lookup |
| `webapp/` | The local FastAPI server, the Alpine.js interface and the seven *Example - …* presets |
| `Example Data/` | Simulated dataset, astigmatism validation dataset and the seven example settings files (`Settings_01_Two_cohorts_simple.json` … `Settings_07_Full_figure_suite.json`) |
| `docs/` | The project website (GitHub Pages) |
| `validation/` | The generators of the example and astigmatism-validation datasets and of the seven settings files, and the fellow-eye simulation behind the clustered tests (the script and its results) |

The bundled runtime (`runtime/`: a private copy of Python and the R library) is **not** in the repository — it is
attached to each release as part of the ZIP, which is Windows-only (`Start StandardEyes.bat` and
`runtime\python` do not work on macOS or Linux).

### Running from source (any operating system)

- **Python 3.12** with `fastapi`, `uvicorn`, `pandas`, `numpy`, `openpyxl` and `pydantic`. `tkinter`
  (part of most Python installers) is used only for the native file and folder pickers.
- **R 4.5.x** with these packages: `readxl`, `dplyr`, `tibble`, `openxlsx`, `stringr`, `MatchIt`,
  `cobalt`, `geepack`, `rlang` and `jsonlite`. Their own dependencies are installed with them:
  `cobalt` brings `ggplot2` and `gtable`, which the figures use, and `openxlsx` brings `zip`,
  which the workbook export uses (`grid` ships with R). All of them are required for every
  run: the engine checks them before it starts and stops, naming the missing ones ("R
  package(s) not installed: ..."), so without `ggplot2` no tables are produced either.
- Start the interface with `python webapp/server/launch.py` from the application folder. The app uses the
  Rscript saved on its R Setup screen, else the `RSCRIPT_PATH` environment variable, else the standard
  Windows install locations, else `Rscript` on the `PATH`.
- The engine also runs without the interface:
  `Rscript --vanilla engine/run_pipeline.R "Example Data/Settings_02_Two_cohorts_full.json"`.

## Citing

If StandardEyes contributes to a publication, please cite:

> Noyman DBE, Mimouni M, Safir M. StandardEyes: free software for standardized comparative analysis and
> outcomes reporting in refractive surgery. *Manuscript in preparation* (2026).

See [`CITATION.cff`](CITATION.cff).

## Acknowledgements

StandardEyes follows the standardized-graph conventions of the JRS/JCRS editorial standards, the Alpins
vector framework and the JCRS 2025 astigmatism standards, and is complementary to the single-cohort
tools AstigMATIC and mEYEstro, whose worksheet formats it can write for cross-checking.

## License

MIT — see [`LICENSE`](LICENSE). The bundled third-party components keep their own licences
(see `THIRD_PARTY_NOTICES.md` and the `licenses/` folder in the release ZIP). The GPL-licensed R
packages are redistributed unmodified as compiled binaries; the corresponding source for the exact
bundled versions is available from CRAN (`https://cran.r-project.org/src/contrib/Archive/<package>/`)
and on request from the authors.
