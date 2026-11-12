# StandardEyes

**Standardized comparative analysis and outcomes reporting for refractive surgery — free, local, reproducible.**

StandardEyes takes a spreadsheet with **one row per eye per surgery** and produces publication-ready
comparative outcomes for any number of groups: group tables with automatic test selection,
propensity-score matching of eyes or of patients (both eyes kept together; `psm_unit`) with balance
reporting (matching weights carried into the matched tables and figures), GEE clustered on the patient, covariate-adjusted regression, Excel workbooks, and the
StandardEyes figures — the published standardized-graph metrics with every cohort superimposed on one
chart, plus JCRS-2025 double-angle astigmatism plots. Visual acuity is read in the notation the file
uses — decimal, logMAR, Snellen (20/x or 6/x) or ETDRS letters — and is analyzed in logMAR by default
and reported as the Snellen geometric mean with the logMAR mean ± SD. P values follow the journal style
(.27, <.001), and the figures are drawn in a journal style (no frames or shading; titles and abbreviations in
a legend file beside each figure) as TIFF, PNG, JPEG, PDF or EPS up to 1200 dpi, optionally at a journal
column width (a combined figure then has two panels per row - one at 8.76 cm - and is split into pages
`_p1`, `_p2` when taller than a 23-cm page, the panel letters continuing). In EPS files the
semi-transparent marks are embedded as images at 1200 dpi or more, since PostScript has no transparency.

Everything runs on your own computer. Nothing is uploaded, and no internet connection is needed.

**➡ Website and download: https://standardeyes.github.io**

## What is new in 1.2.0

Version 1.2.0 changes no statistical default: a preset or settings file saved by 1.1.0 (or 1.0.0) loads and
runs as before, and it matches eyes. The table rows are named in a print-ready form (see *Display labels* below).

- **Patient-level matching (option).** Matching tab, *Match* = patients (both eyes together); settings-file key
  `psm_unit: "patient"` (the default `"eye"` matches eyes, as in 1.1.0). Each patient's analyzed eyes in a
  group form one matching unit: eye-level covariates are averaged over those eyes, per-patient covariates are
  taken once, and the propensity model, caliper, ratio and order work on these units (exact and coarsened exact
  matching form their strata, subclassification its subclasses, on the units). Every eye of a matched
  patient enters the matched cohort with the patient's matching weight, so both eyes enter or leave together;
  balance (love plot, PSM sheet) is computed on the patients. A patient with eyes in two groups forms one unit
  in each group (the run log and the PSM sheet count them). The live match preview uses the same units, and the
  PSM sheet, the Manifest, the figure legends and the run log say "patients matched (both eyes kept together)".
  It needs a patient ID column. The outcome models do not change.
- **The independence-refit label states its reason.** When an exchangeable GEE is not accepted and is refitted
  with an independence working correlation, its label in the tables and on the GEE sheets, and the run log, say
  why: "the exchangeable fit was not accepted: working correlation 0.984", "... did not converge" or "...
  unstable step". The outcome is called constant within patients only when it is; the figure legends point to
  the run log, which counts the reasons by kind. The models do not change.
- **Per-patient rows when a patient has eyes in two groups.** A variable marked "per patient" is modeled on
  one row per patient per group; when a patient has a row in each of two groups those rows are not independent,
  so on an unmatched cohort they are now fitted by an independence GEE clustered by patient (the matched
  cohort's model) instead of an ordinary linear or logistic model, and the Model column says so and counts rows
  and patients ("294 rows from 275 patients"). The summary tables test such a row with the same clustered model
  instead of a Welch t-test or chi-squared test on the duplicated patient rows, so the row's P value and its GEE
  column agree; the Test column gives the reason ("clustered: 19 patients have eyes in more than one group"), and
  the Manifest and the run log say so. Data without such patients give the 1.1.0 numbers, and the matched tables
  do not change.
- **GEE fits converge more tightly.** Every GEE now iterates until no coefficient changes by more than 1e-8
  (geepack's default, used up to 1.1.0, stops at 1e-4, which left one model of example dataset E about 1.4e-4 of a
  standard error short of its solution in the independent validation). Full-precision GEE values on the sheets
  can change by up to about 5e-5 (about the fifth significant digit); in the 14 example runs of the release gate
  no printed value changed.
- **The run log counts the converted cylinders.** For every sphere / cylinder / axis set it counts the plus
  cylinders transposed to minus cylinder (and those already minus, zero or blank), beside its count, per acuity
  column, of the off-chart entries (CF, HM, LP, NLP) set to missing. The love plot's key reads "Before matching" /
  "After matching".
- **A warning for a long output folder.** When the output folder's full path is longer than 100 characters, the
  pre-run summary (Run & Results tab) and the run log warn, before a run with figures, that some figure files may
  pass the 260-character limit Windows sets on a full path, and suggest a folder near the drive root. Nothing
  else changes.
- **The mEYEstro and AstigMATIC tabs are laid out in the input format of those programs.** The mEYEstro tab
  holds the 20 columns of mEYEstro's data file in its order (row 1 its headers, one row per eye from row 2):
  refraction in minus cylinder, vertex distance 12 mm, acuity as the Snellen denominator (text), and the
  postoperative SEQ at up to five stability visits in its last five columns (blank without stability visits),
  so the block pastes into the data file as one range. Columns 1-15 are required by mEYEstro, so an eye without
  a refraction or target value is left out of the tab, and a missing acuity (an off-chart CF, HM, LP or NLP
  included) is a text cell holding one space, as mEYEstro data files hold it; the run log and the Contents sheet
  count both. The AstigMATIC tab holds its 5 input columns. A zero cylinder gets the axis 180 in both (a blank
  axis made AstigMATIC drop the eye); the mEYEstro paper writes a plano target as 0, 0, 0, and the axis of a
  zero cylinder enters no calculation. AstigMATIC assumes a plano astigmatic target and discards a row with a
  missing value, so eyes with a non-plano astigmatic target or a missing cylinder, axis or eye code are left
  out of its tab, and the tab's note, the Contents sheet and the run log count them ("179 eyes kept, 43 left
  out: 39 non-plano astigmatic target, 4 missing cylinder, axis or eye"). The Contents sheet gives the cells
  to copy for each tab.
  **Correction:** in 1.1.0 the mEYEstro tab placed the postoperative CDVA before the UDVA. mEYEstro reads the
  UDVA first, so mEYEstro graphs made from a 1.1.0 tab show the two swapped: export the tab again with 1.2.0.
- **NE when a variable does not vary within the groups.** A continuous or ordinal variable that is constant
  within each compared group but differs between them (one optical zone per platform) leaves no variability to
  estimate: its GEE and adjusted-regression comparisons, unmatched and matched, print NE in the effect, CI and P
  cells instead of β = 0.00 (95% CI, 0.00 to 0.00) and P < .001, with the footnote "NE, not estimable: no
  variation within groups", and so does a table test that needs a within-group variance (Welch, the weighted
  Wald test); a rank test or the permutation test over patients needs none and keeps its P value. With three or
  more groups, a comparison of two groups that both do not vary is NE while the other comparisons are
  estimated, and it stays out of the Holm adjustment. The run log names the rows. Every other value is
  unchanged.
- **The identity and zero lines are never a group color.** The y = x line of the two scatters and the zero line
  of the stability graph are solid black (dark gray when a group is drawn close to black); the legends name it.
  The ±0.50 D and ±1.00 D lines of the scatters stay green and magenta unless a group is drawn in a color close
  to them; then they take another color far from every group color (olive or dark green; violet or pink), and
  the legends name it. They are also told apart by their pattern, so a reader with a color-vision deficiency,
  for whom they can take on the color of a group, can still read them: the ±0.50 D lines are long-dashed and
  the ±1.00 D lines dot-dashed (the identity line is solid, each group's regression line dotted).
- **An enlarged centroid panel beside every double-angle plot.** At print size the 95% confidence region of a
  centroid is often smaller than its marker, so each double-angle plot (preoperative astigmatism, SIA,
  prediction error and postoperative astigmatism) now has a small panel with each cohort's centroid and the
  dashed 95% confidence region of the centroid, enlarged on the plot's own axes (x and y in diopters) at a scale
  set by the regions. It is drawn at the right of the plot, or under it when the figure is too narrow (a
  combined figure 12.69 cm wide); the legend file says which. The centroid markers of the plot are smaller, so
  a larger region shows around them there too. No number changes. Special outputs tab, untick *Enlarged
  centroid panels* (`astig_centroid_panels: false`) to leave the panels out.
- **Matched tables without the weights (option).** Matching tab, *Matched tables* = unweighted; settings-file
  key `matched_table_weights: "none"` (the default `"weights"` keeps the 1.1.0 rule). Every matched eye then
  counts once in the matched descriptives and table tests; the matched GEE and regression models still follow
  *Use matching weights in matched GEE* (GEE tab). The note above each matched table, the Publication Table
  footnote, the Manifest and the figure legends say which weighting the matched tables use. It needs
  nearest-neighbor matching without replacement: with exact or coarsened exact matching, subclassification or
  replacement the weights are the matching itself, so the interface refuses the run and the matched tables of
  a settings file asking for it keep the weights (the run log says why).
- **Options to reproduce an earlier analysis.** GEE tab, the collapsed section *Reproduce an earlier analysis
  (advanced)*, to repeat the numbers of an analysis script that used other rules. The first value of each
  settings-file key is the default and keeps the 1.1.0 rule, so a settings file without the keys runs as
  before: `matched_gee_corstr` (`"independence"` | `"exchangeable"`), the working correlation of the matched
  GEE models (GEE sheets and GEE columns; the clustered table tests and figure P values stay independence);
  `gee_refit` (`"on"` | `"off"`), where off keeps every exchangeable GEE that converged, whatever its working
  correlation and without the scoring-step check (one that did not converge is still refitted and labeled); a
  fit the default rule would refit is labeled with that rule's reason ("exchangeable working correlation kept
  with gee_refit = off (not accepted by the default rule: working correlation 1.020 - 1 or more, not a valid
  correlation)") and counted in the run log; `separated_estimates` (`"ne"` | `"report"`), where report prints,
  for an outcome with no events (or only events) in one group, the estimate, CI and P the GEE returns for that
  group's comparison (fitted as geepack fits it by default, on the rows in the data's order), each marked †
  with the footnote "no events in one group: estimate unreliable" (or "only events ...") and counted in the
  run log - the other comparisons, the intercept and the covariates still come from the converged model
  without that group, and when there is none (two groups, or the reference group affected) every row of the
  fit is marked †; `std_axis_range` (`"as_recorded"` | `"0-179"`), where 0-179 reduces every axis of a
  converted refraction modulo 180, so an axis of 180 reads 0 (the axis rows and the export tabs change;
  sphere, cylinder and SEQ do not); and `smd_binary` (`"standardized"` | `"raw"`), where raw gives the balance
  of a binary matching covariate as the raw difference in proportions (cobalt's own default) in the PSM sheet
  and the love plot. *Matched tables* (above) stays on the Matching tab. The Manifest and the run log name
  every value that is not the default; the Publication Table footnotes, the GEE sheet notes, the PSM sheet
  and the figure and love-plot legends state each rule where it changes what they show (the mEYEstro and
  AstigMATIC tabs carry no notes: the axis rule is in the Manifest).
- **An R version notice.** When the R found is not 4.5.x (the version the bundled R packages were built for),
  the interface shows a notice with the link to R 4.5.2 for Windows
  (https://cran.r-project.org/bin/windows/base/old/4.5.2/), and so do the console window, the run log and the
  R setup screen. The analysis still runs.
- **Color-blind-safe example colors.** The seven example settings files and presets draw their groups in
  Okabe-Ito colors (blue, orange, vermillion, sky blue, reddish purple), which stay distinct from one another
  under the common color-vision deficiencies, chosen so that at normal color vision the black identity line
  and the green / magenta ±0.50 D / ±1.00 D lines stay clear of every group. Under a deficiency those two
  line sets can take on a group's color, which is why they are told apart by their pattern too (long dashes,
  dot-dash; above). The software's own default colors are unchanged.
- **Display labels for print-ready tables and figures.** Variables & Types tab, *Output order*: type a label
  beside a variable, such as "Age (y)" or "Central pachymetry (µm)", and the tables, model sheets, balance
  tables and love plots print it instead of the column name (settings-file key `display_labels`, column → label;
  empty = the column name). The data sheets, the mEYEstro / AstigMATIC tabs, the filtered CSV and the WHWK files
  keep the column names. Rows now read "Age (y), per patient", "Glare or halo symptoms, n/N (%)" and "Sex: F, per
  patient, n/N (%)" instead of "Age_Years [per patient]" and "Sex (F) [per patient]". A counted level is left
  out of the row's name only when it is the "yes" of a natural pair (Yes / No, Y / N, True / False, 1 / 0,
  Present / Absent, Positive / Negative); a variable coded 1 / 2 reads "Sex: 1, per patient, n/N (%)". A label
  that is another variable's printed name or another column's header is not used (the run log says so). The
  seven example settings files carry clinical labels.
- **The sheet viewer wraps long cells.** On the Run & Results tab a sheet opens as the workbook holds it (no
  "Unnamed: 1" headers), long cells wrap and every column is bounded, so the group columns of a Publication
  Table stay in view.
- **A review build for blinded peer review.** The release can also be built as `StandardEyes-review.zip`: the
  same program with the author names and every link to the website and repository withheld (the program name
  stays); every workbook's Manifest then records "review build".
- **Five demonstration datasets** replace the single example dataset in `Example Data` - simulated (no real
  patients; patients and eyes carry codes only), each with a README sheet (the generating model with every true
  effect, this file's own estimates, the seed and a column dictionary) and its settings files, also included in the app as the seven
  *Example - …* presets: **A** two techniques (PRK vs LASIK, the standard graphs and the stability curve),
  **B** notation harmonization (mixed plus and minus cylinder, logMAR and Snellen acuity; two settings files),
  **C** four platforms (four groups on one chart, Holm-adjusted comparisons), **D** astigmatism (KLEx vs LASIK,
  the JCRS 2025 analysis and the Alpins graphs) and **E** confounded cohorts (crude, adjusted and matched
  estimates; eye-level and patient-level matching in two settings files). Their generators are in
  `validation/`, where the astigmatism validation workbook moved too.

## What is new in 1.1.0

Version 1.1.0 changes several defaults, so the same data and settings can give different numbers than
1.0.0. A settings file or preset saved by 1.0.0 still loads and runs; a setting it does not contain takes
the 1.1.0 default, and every workbook's Manifest sheet records the settings used. Each change, and how to
get the 1.0.0 behavior back (interface tab, then the settings-file key):

| What changed in 1.1.0 | To get the 1.0.0 behavior |
|---|---|
| **Tests in the tables are clustered by patient.** When a patient has two analyzed eyes, the P value of every per-eye row of the summary tables and the Publication Table comes from a patient-clustered GEE test - the same P value as the figure of that outcome. Where that GEE cannot be fitted (a group with no events or only events, for example), the table and the figure use a permutation test over patients (9999 permutations, fixed seed), which keeps each patient's eyes together - the eyes of a patient with eyes in two groups exchange their labels within the patient, so a contralateral pairing is kept - labeled "GEE not fitted". | GEE tab, *Summary-table tests* = classical (`table_tests: "classical"`) - the figures stay clustered, as in 1.0.0, and the Publication Table and the figure legend files say that a table and its figure can then differ; switching off *Cluster p-values by patient* makes both classical. |
| **Small-sample correction of the clustered tests.** Every GEE test and interval uses the Mancl-DeRouen bias-corrected robust standard error with a t reference (F for joint tests; df = patients minus model coefficients). | GEE tab, *GEE small-sample inference* = none (`gee_small_sample: "none"`). |
| **The caliper is on the logit scale.** The propensity score is matched on the logit scale, so the default caliper is 0.2 SD of the logit of the propensity score (Austin 2011). | Matching tab, *Distance* = propensity score, probability scale (`psm_distance: "logit"`). |
| **Visual acuity is analyzed in logMAR and shown as Snellen.** The mapped acuity columns are analyzed in logMAR; a cell reads "20/20 (0.00 ± 0.05 logMAR)" (Snellen geometric mean, logMAR mean ± SD) with the range in Snellen. | Variables & Types tab, untick *Analyze visual acuity in logMAR* (`do_logmar: false`). |
| **P values in journal style**: .27, .003, <.001, >.99; an exact half is rounded up (.065 is printed .07). | GEE tab, *P value style* = classic (`p_style: "classic"`). |
| **Journal-style figures**: white, no frames, section bands, captions or shaded wedges; each figure's title, method notes and abbreviations go to `<figure>_legend.txt` beside it. | Special outputs tab, *Figure style* = annotated (`figure_style: "annotated"`). |
| **The astigmatism plots follow the JCRS 2025 standard** in plus-cylinder format (with-the-rule on the left): the double-angle set is preoperative astigmatism, SIA and the prediction error (PE = postoperative - target), plus postoperative astigmatism when a target is not plano; the correction-index polar plots are gone (its histogram stays). The 95% region of the eyes is the depth-based convex polygon and the centroid's 95% confidence region the bootstrap convex polygon of the centroid, both built as in the WHWK functions (the journal's reference tool) - in a cohort of up to about 180 eyes the depth rule often removes no eye, and the polygon is then the convex hull of all eyes (the sheet and the legend file say so); each centroid is tested against zero by the bootstrap projection-distance test of the same functions (P = .001 is its floor with 999 resamples); the robust tests resample patients, and their 20% trimming and winsorizing follow the standard order-statistic definitions (Wilcox); where the trimmed statistic is not defined (common with quarter-diopter data) the same bootstrap test runs untrimmed and is labeled so. | Plus-cylinder vectors and the panel set have no switch (1.0.0 mishandled the cylinder sign). Special outputs tab: *95% region of the eyes* = normal (`astig_region`), *Centroid 95% confidence region* = hotelling (`astig_centroid_region`), *Astigmatism tests* = classic (`astig_test_method`). Hull peeling is no longer offered. |
| **New sheets**: "Astigmatism (JCRS 2025)" (centroids, total SD, magnitudes, intervals and the standard's tests) and "Astigmatism vectors" (per eye); the "Astigmatism (JCRS 2025)" sheet is also written as a CSV beside the workbook. | Special outputs tab, untick *Astigmatism (JCRS 2025) sheet* (`astig_sheet: false`). |
| **Input files for the WHWK functions**: with the double-angle plots, a `WHWK input` folder inside the run's figure folder (`Graphs_<prefix>_<run>_<time>`, beside the figures; the run log gives its full path) holds, for each plotted vector, `<vector>_vector.txt` (`<Cohort>.x`, `<Cohort>.y`, plus-cylinder double-angle form) and `<vector>_magnitude.txt`, tab-delimited, with a README. The Wilcox-Holladay-Wang-Koch R functions are obtained from https://osf.io/xhe8u/; they treat eyes as independent and refuse values beyond 6 D. | Special outputs tab, untick *Input files for the WHWK functions* (`astig_whwk_files: false`). |
| **GEE models of an outcome that hardly varies within a patient.** A variable marked "per patient" is modeled on one row per patient by an ordinary linear or logistic model on the GEE and Regression sheets (on a matched cohort by a GEE with an independence working correlation and robust SE clustered by patient, like every matched model, weighted by the patient's mean matching weight over all of its eyes - the same model as the matched table's P value of that row; since 1.2.0 also when a patient has eyes in more than one group, whose rows are then not independent), never by an eye-level GEE. The Model column counts the rows and the patients ("184 rows from 169 patients" when a patient has a row in each of two groups). Any other exchangeable GEE is kept only if it converged, its working correlation is below 0.95 and one further scoring step moves no coefficient by more than 0.01 SE; otherwise it is refitted with an independence working correlation (robust SE still clustered by patient). The sheets, tables, figures and run log name the model used. | No switch. |

Also new: EPS figure files, up to 1200 dpi, and a figure width in cm (`figure_width_cm`; 8.76, 12.69 and 17.77 cm
are the journal column widths). Without a switch: comparisons with the reference in the tables are
Holm-adjusted; tables use fixed decimals and one effect style; matched cohorts use an independence working
correlation (in the matched tables too: a per-patient row is tested on one row per patient, and with unequal
matching weights a categorical row with more than two levels gets a weighted Wald test on its level
indicators); a GEE P value from a group of 2-9 patients carries a caution in the tables (Interpretation
column, a Publication Table footnote), the GEE sheets and the figures; the Regression sheet also gives the
crude estimate on the same eyes; workbooks name "StandardEyes" as their author.

## Download and run (Windows)

1. Install [R 4.5.2 for Windows](https://cran.r-project.org/bin/windows/base/old/4.5.2/) if R 4.5 is not already installed. The main CRAN
   page now offers R 4.6, which may not load the bundled packages; several R versions can be installed side by side.
2. Download `StandardEyes-windows.zip` from the [latest release](https://github.com/StandardEyes/standardeyes.github.io/releases/latest)
   and extract the **whole folder** to a folder you can write to (e.g. `C:\StandardEyes` or Documents —
   not `C:\Program Files`). A folder near the drive root, such as `C:\StandardEyes`, is best: Windows cannot write a
   file whose full path passes 260 characters, and the figures of a matched run have long names.
   Unblocking the ZIP first (right-click → Properties → Unblock) avoids the
   SmartScreen prompt on first start.
3. Double-click **`Start StandardEyes.bat`**. Your browser opens on a free local port.

The release ZIP is for Windows 10/11 (64-bit) only. The Python runtime for the interface and all 48 R
packages the analysis needs are inside it, together with their license texts (`licenses/` and
`THIRD_PARTY_NOTICES.md`); nothing is installed. See [`README.txt`](README.txt) for the full user notes and
troubleshooting, and the `Example Data` folder for five simulated datasets with seven worked settings files
(also included in the app as the seven *Example - …* presets).

The app shows its version as `v1.2.0 (build <id>)`: the release number used on the website and in
`CITATION.cff`, then the internal build id. Every workbook's Manifest sheet records both - also when the
engine runs from the command line, since the engine reads them from `webapp/server/app.py` - with the run duration.

## What is in this repository

| Folder | Contents |
|---|---|
| `R/` | The statistical engine (analysis, pipeline, export, figures) |
| `engine/` | Headless R entry points (`run_pipeline.R`, `match_preview.R`) and the decimal-to-Snellen lookup |
| `webapp/` | The local FastAPI server, the Alpine.js interface and the seven *Example - …* presets |
| `Example Data/` | The five simulated demonstration datasets (`Dataset_A_two_techniques.xlsx` … `Dataset_E_confounded_cohorts.xlsx`) and their seven settings files (`Settings_A_two_techniques.json` … `Settings_E_confounded_patient_matching.json`) |
| `docs/` | The project website (GitHub Pages) |
| `validation/` | The generators of the demonstration datasets and their settings files (`make_dataset_A.py`, `make_dataset_B.py`, `make_dataset_C.py`, `make_dataset_D.py` and `make_dataset_E.py`, with their shared `simcore.py`), the astigmatism validation workbook and its generator (`Astigmatism_Validation_Dataset.xlsx`, `make_astig_validation_data.py`) - with Python 3, numpy, pandas and openpyxl, `python <script> --out <folder>` writes a copy into that folder - and the fellow-eye simulation behind the clustered tests of the figures (the script and its results) |

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
  `Rscript --vanilla engine/run_pipeline.R "Example Data/Settings_A_two_techniques.json"`.

## Citing

If StandardEyes contributes to a publication, please cite:

> Ben Ephraim Noyman D, Mimouni M, Safir M. StandardEyes: free software for standardized comparative analysis and
> outcomes reporting in refractive surgery. *Manuscript in preparation* (2026).

See [`CITATION.cff`](CITATION.cff).

## Acknowledgements

StandardEyes follows the standardized-graph conventions of the JRS/JCRS editorial standards, the Alpins
vector framework and the JCRS 2025 astigmatism standards, and is complementary to the single-cohort
tools AstigMATIC and mEYEstro, in whose input formats it can lay out worksheet tabs for cross-checking.

## License

MIT — see [`LICENSE`](LICENSE). The bundled third-party components keep their own licenses
(see `THIRD_PARTY_NOTICES.md` and the `licenses/` folder in the release ZIP). The GPL-licensed R
packages are redistributed unmodified as compiled binaries; the corresponding source for the exact
bundled versions is available from CRAN (`https://cran.r-project.org/src/contrib/Archive/<package>/`)
and on request from the authors.
