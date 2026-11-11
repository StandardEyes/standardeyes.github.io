===========================================================
 StandardEyes
 By Ben Ephraim Noyman D, Mimouni M, Safir M
===========================================================

WHAT IT IS
----------
A local app for comparative refractive-surgery analysis:
group summary tables, propensity-score matching, clustered
GEE, adjusted regression, and the StandardEyes figures -
every group superimposed on one publication-ready chart,
plus JCRS-standard double-angle astigmatism plots.

Everything runs ON YOUR OWN COMPUTER. No data is uploaded
anywhere, and the app needs no internet connection.

This ready-to-run folder is for Windows 10/11 (64-bit).
On macOS or Linux, run it from source (see README.md).


-----------------------------------------------------------
 WHAT IS NEW IN 1.2.0
-----------------------------------------------------------
Version 1.2.0 changes no statistical default: a preset or
settings file saved by 1.1.0 (or 1.0.0) loads and runs as
before, and it matches eyes. The table rows are named in
a print-ready form (see "Display labels" below).

* Patient-level matching (option). Matching tab, "Match"
  = patients (both eyes together); settings file key
  psm_unit = "patient" (the default "eye" matches eyes, as
  in 1.1.0). Each patient's analyzed eyes in a group form
  one matching unit: eye-level covariates are averaged over
  those eyes, per-patient covariates are taken once, and
  the propensity model, caliper, ratio and order work on
  these units (exact and coarsened exact matching form
  their strata, subclassification its subclasses, on the
  units). Every eye of a matched patient enters the
  matched cohort with the patient's matching weight, so
  both eyes enter or leave together; balance (love plot,
  PSM sheet) is computed on the patients. The outcome
  models do not change. Details under OUTPUT below
  ("Matching unit").

* The independence-refit label states its reason. When an
  exchangeable GEE is not accepted and is refitted with an
  independence working correlation, its label in the
  tables and on the GEE sheets, and the run log, say why:
  "the exchangeable fit was not accepted: working
  correlation 0.984", "... did not converge" or "...
  unstable step". The outcome is called constant within
  patients only when it is; the figure legends point to
  the run log, which counts the reasons by kind. The
  models do not change.

* Per-patient rows when a patient has eyes in two groups.
  A variable marked "per patient" is modeled on one row
  per patient per group; a patient with a row in each of
  two groups makes those rows dependent, so on an
  unmatched cohort they are now fitted by an independence
  GEE clustered by patient (the matched cohort's model)
  instead of an ordinary linear or logistic model. The
  Model column says so and counts rows and patients ("294
  rows from 275 patients"). The summary tables test such
  a row with the same clustered model instead of a Welch
  t-test or chi-squared test on the duplicated patient
  rows, so the row's P value and its GEE column agree;
  the Test column gives the reason ("clustered: 19
  patients have eyes in more than one group"), and the
  Manifest and the run log say so. Data without such
  patients give the 1.1.0 numbers, and the matched tables
  do not change.

* GEE fits converge more tightly. Every GEE now iterates
  until no coefficient changes by more than 1e-8
  (geepack's default, used up to 1.1.0, stops at 1e-4,
  which left one model of example dataset E about 1.4e-4
  of a standard error short of its solution in the
  independent validation). Full-precision GEE values on
  the sheets can change by up to about 5e-5 (about the
  fifth significant digit); in the 14 example runs of the
  release gate no printed value changed.

* The run log counts the converted cylinders. For every
  sphere / cylinder / axis set it counts the plus
  cylinders transposed to minus cylinder (and those
  already minus, zero or blank), beside its count, per
  acuity column, of the off-chart entries (CF, HM, LP,
  NLP) set to missing. The love plot's key reads "Before
  matching" / "After matching".

* A warning for a long output folder. When the output
  folder's full path is longer than 100 characters, the
  pre-run summary (Run & Results tab) and the run log
  warn, before a run with figures, that some figure files
  may pass the 260-character limit Windows sets on a full
  path, and suggest a folder near the drive root. Nothing
  else changes.

* The mEYEstro and AstigMATIC tabs are laid out in the
  input format of those programs. The mEYEstro tab holds
  the 20 columns of mEYEstro's data file in its order (row
  1 its headers, one row per eye from row 2): minus
  cylinder, vertex distance 12 mm, acuity as the Snellen
  denominator (text), and the postoperative SEQ at up to
  five stability visits in its last five columns (blank
  without stability visits), so the block pastes into the
  data file as one range. The AstigMATIC tab holds its 5
  input columns. A zero cylinder gets the axis 180 in both
  (a blank axis made AstigMATIC drop the eye); an unknown
  cylinder stays blank. AstigMATIC assumes a plano
  astigmatic target, so eyes with a non-plano astigmatic
  target are left out of its tab; the tab's note, the
  Contents sheet and the run log count them ("183 eyes
  kept, 39 left out: non-plano astigmatic target"). The
  Contents sheet gives the cells to copy for each tab.

* NE when a variable does not vary within the groups. A
  continuous or ordinal variable that is constant within
  each compared group but differs between them (one
  optical zone per platform) leaves no variability to test
  against: its table test, GEE and adjusted-regression
  comparisons, unmatched and matched, print NE in the
  effect, CI and P cells instead of beta 0.00 (0.00 to
  0.00) and P < .001, with the footnote "NE, not
  estimable: no variation within groups"; the run log
  names the rows. Every other value is unchanged.

* The identity and zero lines are never a group color.
  The y = x line of the two scatters and the zero line of
  the stability graph are solid black (dark gray when a
  group is drawn close to black); the legends name it.

* An enlarged centroid panel beside every double-angle
  plot. At print size the 95% confidence region of a
  centroid is often smaller than its marker, so each
  double-angle plot (preoperative astigmatism, SIA,
  prediction error and postoperative astigmatism) now has
  a small panel with each cohort's centroid and the dashed
  95% confidence region of the centroid, enlarged on the
  plot's own axes (x and y in diopters) at a scale set by
  the regions. It is drawn at the right of the plot, or
  under it when the figure is too narrow (a combined
  figure 12.69 cm wide); the legend file says which. The centroid markers of the plot are
  smaller, so a larger region shows around them there
  too. No number changes. Special outputs tab, untick
  "Enlarged centroid panels" (settings file key
  astig_centroid_panels = false) to leave the panels out.

* Matched tables without the weights (option). Matching
  tab, "Matched tables" = unweighted; settings file key
  matched_table_weights = "none" (the default "weights"
  keeps the 1.1.0 rule). Every matched eye then counts
  once in the matched descriptives and table tests; the
  matched GEE and regression still follow "Use matching
  weights in matched GEE" (GEE tab). The note above each
  matched table, the Publication Table footnote, the
  Manifest and the figure legends say which weighting the
  matched tables use.

* Display labels for print-ready tables and figures.
  Variables & Types tab, Output order: type a label beside
  a variable, such as "Age (y)" or "Central pachymetry
  (µm)", and the tables, model sheets, balance tables and
  love plots print it instead of the column name (settings
  file key display_labels, column -> label; empty = the
  column name). The data sheets, the mEYEstro / AstigMATIC
  tabs, the filtered CSV and the WHWK files keep the
  column names. Rows now read "Age (y), per patient",
  "Glare or halo symptoms, n/N (%)" and "Sex: F, per
  patient, n/N (%)" instead of "Age_Years [per patient]"
  and "Sex (F) [per patient]". The seven example settings
  files carry clinical labels.

* The sheet viewer wraps long cells. On the Run & Results
  tab a sheet opens as the workbook holds it (no
  "Unnamed: 1" headers), long cells wrap and every column
  is bounded, so the group columns of a Publication Table
  stay in view.

* A review build for blinded peer review. The release can
  also be built as StandardEyes-review.zip: the same
  program with the author names and every link to the
  website and repository withheld (the program name
  stays); every workbook's Manifest then records "review
  build".

* Five demonstration datasets replace the single example
  dataset in "Example Data": A two techniques, B notation
  harmonization, C four platforms, D astigmatism and E
  confounded cohorts, with seven settings files and the
  seven "Example - ..." presets (see TRY IT FIRST below).
  The astigmatism validation workbook moved to the
  "validation" folder, beside the generators.


-----------------------------------------------------------
 WHAT IS NEW IN 1.1.0
-----------------------------------------------------------
Version 1.1.0 changes several defaults, so the same data
and settings can give different numbers than 1.0.0. A
preset or settings file saved by 1.0.0 still loads and
runs; a setting it does not contain takes the new default.
Each workbook's Manifest sheet records the settings used.
What changed, and how to get the 1.0.0 behavior back:

* Tests in the tables are clustered by patient. When a
  patient has two analyzed eyes, each per-eye row of the
  summary tables and the Publication Table gets its P value
  from a patient-clustered GEE test - the same P value as
  the figure of that outcome. Where that GEE cannot be
  fitted (a group with no events or only events, for
  example), the table and the figure use a permutation
  test over patients (9999 permutations, fixed seed),
  which keeps each patient's eyes together - the eyes of
  a patient with eyes in two groups exchange their labels
  within the patient, so a contralateral pairing is kept -
  labeled "GEE not fitted". A GEE P value from a group of
  2-9 patients carries a caution (Interpretation column,
  a Publication Table footnote). Matched tables use the
  independence GEE of the matched analyses: a per-patient
  row on one row per patient, and with unequal weights a
  categorical row with more than two levels by a weighted
  Wald test on its level indicators.
  Back: GEE tab, "Summary-table tests" = classical
  (table_tests); the figures stay clustered, as in 1.0.0,
  and the Publication Table and the figure legend files
  say that a table and its figure can then differ.
  Turning off "Cluster p-values by patient" makes both
  classical.

* Small-sample correction of the clustered tests: every GEE
  test and interval uses the Mancl-DeRouen corrected robust
  SE with a t reference (F for joint tests).
  Back: GEE tab, "GEE small-sample inference" = none
  (gee_small_sample).

* The caliper is on the logit scale: 0.2 SD of the logit of
  the propensity score (Austin 2011).
  Back: Matching tab, Distance = "propensity score,
  probability scale" (psm_distance = logit).

* Visual acuity is analyzed in logMAR and shown as Snellen:
  "20/20 (0.00 ± 0.05 logMAR)" - the Snellen geometric mean
  with the logMAR mean and SD - and the range in Snellen.
  Back: Variables tab, untick "Analyze visual acuity in
  logMAR" (do_logmar = false).

* P values in journal style: .27, .003, <.001, >.99; an
  exact half is rounded up (.065 is printed .07).
  Back: GEE tab, "P value style" = classic (p_style).

* Journal-style figures: white, no frames, section bands,
  captions or shaded wedges; titles, method notes and
  abbreviations go to <figure>_legend.txt beside each
  figure.
  Back: Special outputs tab, "Figure style" = annotated
  (figure_style).

* The astigmatism plots follow the JCRS 2025 standard, in
  plus-cylinder format (with-the-rule on the left). The
  double-angle plots show preoperative astigmatism, SIA and
  the prediction error (PE = postoperative - target), plus
  postoperative astigmatism when a target is not plano. The
  correction-index polar plots are gone (its histogram
  stays). The 95% region of the eyes is the depth-based
  convex polygon and the centroid's 95% confidence region
  the bootstrap convex polygon of the centroid, both built
  as in the WHWK functions (the journal's reference tool).
  In a cohort of up to about 180 eyes the depth rule often
  removes no eye; the polygon is then the convex hull of
  all eyes, and the sheet and the legend file say so.
  Each centroid is tested against zero by the bootstrap
  projection-distance test of the same functions (with
  999 resamples P = .001 is its floor). The robust tests
  resample patients; their 20% trimming and winsorizing
  follow the standard order-statistic definitions
  (Wilcox). Where the trimmed statistic is not defined
  (common with quarter-diopter data) the same bootstrap
  test runs untrimmed and is labeled so.
  Back: the plus-cylinder vectors and the plot set have no
  switch (1.0.0 mishandled the cylinder sign). On the
  Special outputs tab, "95% region of the eyes" = normal,
  "Centroid 95% confidence region" = Hotelling and
  "Astigmatism tests" = classic give the parametric
  versions. Hull peeling is no longer offered.

* New sheets "Astigmatism (JCRS 2025)" (centroids, total
  SD, magnitudes, intervals and the standard's tests) and
  "Astigmatism vectors" (per eye); the "Astigmatism
  (JCRS 2025)" sheet is also written as a CSV beside the
  workbook.
  Back: Special outputs tab, untick "Astigmatism (JCRS
  2025) sheet" (astig_sheet = false).

* Input files for the WHWK functions: with the
  double-angle plots, a "WHWK input" folder inside the
  run's figure folder (Graphs_<prefix>_<run>_<time>, beside
  the figures it belongs to; the run log gives its full
  path) holds, for each plotted vector,
  <vector>_vector.txt (columns <Cohort>.x, <Cohort>.y,
  plus-cylinder double-angle form) and
  <vector>_magnitude.txt, tab-delimited, with a README.
  The Wilcox-Holladay-Wang-Koch R functions are obtained
  from https://osf.io/xhe8u/; they treat eyes as
  independent and refuse values beyond 6 D.
  Back: Special outputs tab, untick "Input files for the
  WHWK functions" (astig_whwk_files = false).

* GEE models of an outcome that hardly varies within a
  patient: a variable marked "per patient" is modeled on
  one row per patient by an ordinary linear or logistic
  model on the GEE and Regression sheets (on a matched
  cohort by a GEE with an independence working
  correlation and robust SE clustered by patient, like
  every matched model; since 1.2.0 also when a patient has
  eyes in more than one group), never by an eye-level
  GEE. Any
  other exchangeable GEE is kept only
  if it converged, its working correlation is below 0.95
  and one further scoring step moves no coefficient by
  more than 0.01 SE; otherwise it is refitted with an
  independence working correlation (robust SE still
  clustered by patient). Sheets, tables, figures and the
  run log name the model used. No switch.

Also new: EPS figures, up to 1200 dpi, and a figure width
in cm (8.76, 12.69 or 17.77 = the journal column widths).
Without a switch: comparisons with the reference in the
tables are Holm-adjusted; tables use fixed decimals and
one effect style; every model of a matched cohort uses an
independence working correlation; the Regression sheet
also gives the crude estimate on the same eyes.


-----------------------------------------------------------
 HOW TO START
-----------------------------------------------------------
1. Copy this WHOLE folder to your computer, to a folder
   you can write to (e.g. C:\StandardEyes or Documents -
   NOT C:\Program Files). Keep all subfolders.
   Do not run it from inside a ZIP file.
   Keep the folder near the drive root (C:\StandardEyes
   is ideal): Windows cannot write a file whose full path
   is longer than 260 characters, and the figure files of
   a matched run carry long names - in a deeply nested
   folder they are not written (the run log says so).

   Tip: before extracting a downloaded ZIP, right-click
   it, choose Properties, tick "Unblock", then extract.
   This avoids the Windows warning described under
   TROUBLESHOOTING.

2. Double-click:      Start StandardEyes.bat

3. Your browser opens automatically.

   The app picks a free port itself (usually 8760) and
   prints the address in the console window, so it can
   never clash with another program or a second copy.

   Keep the black console window OPEN while you work.
   It is titled "StandardEyes - close this window to
   stop the app"; closing that window stops the app.


-----------------------------------------------------------
 REQUIREMENT: R
-----------------------------------------------------------
The statistics run in R, so R must be on your computer.

  >>> R version 4.5.x  (e.g. 4.5.2)  <<<

Everything ELSE is already inside this folder - the Python
runtime and all 48 R packages the analysis needs. You do
NOT need to install any R packages, and the app never
downloads anything.

* If R is already installed, the app finds it automatically.
* If not, the app opens a Setup screen where you can pick
  your R installation or browse to Rscript.exe yourself.
* You can reopen it any time with the "R setup" button
  at the top right.

Where is Rscript.exe? Usually:
  C:\Program Files\R\R-4.5.2\bin\x64\Rscript.exe

Don't have R 4.5? Install "R 4.5.2 for Windows" from
cran.r-project.org/bin/windows/base/old/4.5.2/
- or ask your IT department. (The main CRAN page now
offers R 4.6. Several R versions can be installed side
by side.) Nothing else is needed: this app brings its
own R packages.

NOTE: the bundled packages are built for R 4.5.x. Other
versions (4.3, 4.4, 4.6...) may fail to load them.


-----------------------------------------------------------
 TRY IT FIRST: THE EXAMPLE DATASETS
-----------------------------------------------------------
The folder "Example Data" contains five ready-made,
simulated datasets and seven worked settings files, so you
can see a full analysis before touching your own data.
They contain no patient information: patients and eyes
carry codes only. Each dataset's README sheet gives the
generating model with every true effect, this file's own
estimates of them (which scatter around the true values),
the seed and a column dictionary.

The datasets and their settings files are written by the
generators in the "validation" folder (make_dataset_A.py,
make_dataset_B.py, make_dataset_C.py, make_dataset_D.py
and make_dataset_E.py, with their shared simcore.py), and
the astigmatism validation workbook there by
make_astig_validation_data.py. With Python 3 (numpy,
pandas, openpyxl), "python <script> --out <folder>" writes
a fresh copy into <folder>, so you can check or extend
them.

  1. Load  Example Data\Dataset_A_two_techniques.xlsx
  2. Click the preset "Example - A Two techniques"
  3. Tick "Preview only", then press RUN

The five datasets and their examples:

  A Two techniques (Dataset_A_two_techniques.xlsx)
      PRK versus LASIK at 6 months: the summary tables
      (patient-clustered P values), the standard graphs,
      the Publication Table, the mEYEstro and AstigMATIC
      tabs, and the stability curve over the visits at 1,
      3, 6 and 12 months. Decimal acuity, minus-cylinder
      refraction.
      Preset: Example - A Two techniques

  B Notation harmonization (Dataset_B_notation.xlsx)
      Two centers. Refraction typed in mixed plus and minus
      cylinder notation (the targets too), transposed to
      minus cylinder before the analysis; acuity recorded
      both in logMAR and as Snellen fractions with letter
      suffixes. The app reads one acuity notation per
      settings file, so the same eyes come with two: one
      reads the logMAR columns, one the Snellen columns
      (the run log counts, for each refraction, the plus
      cylinders transposed and the minus, zero and blank
      ones, and the acuity values read and those set to
      missing).
      Presets: Example - B Notation (logMAR),
               Example - B Notation (Snellen)

  C Four platforms (Dataset_C_four_platforms.xlsx)
      Four groups on one chart, each in its own color: an
      overall test on every panel and each platform against
      Platform 1 with Holm-adjusted P values. Acuity as
      ETDRS letter scores.
      Preset: Example - C Four platforms

  D Astigmatism (Dataset_D_astigmatism.xlsx)
      KLEx versus LASIK in eyes with 0.75-4.00 D of
      cylinder and some non-plano targets: the JCRS 2025
      double-angle plots (preoperative astigmatism, SIA,
      prediction error, postoperative astigmatism) with the
      centroids, the 95% regions and the centroid tests,
      the Alpins graphs, the AstigMATIC sheet and the WHWK
      input files.
      Preset: Example - D Astigmatism

  E Confounded cohorts (Dataset_E_confounded_cohorts.xlsx)
      A Condition cohort versus Control with measured
      confounding (age, preoperative SEQ, pachymetry):
      crude tables, covariate-adjusted GEE and regression,
      and 1:1 propensity-score matching with the love plot
      and the matched tables - matching eyes in one
      example, patients (both eyes together) in the other.
      The README sheet gives the true effects, to compare
      the crude, adjusted and matched estimates with, and
      the app's matched results: 1:1 matching with a 0.2 SD
      caliper leaves one covariate slightly unbalanced in
      each example, which the covariate-adjusted matched
      GEE takes into account.
      Presets: Example - E Confounded cohorts
               (eye matching), and Example - E Confounded
               cohorts (patient matching)

Each is supplied BOTH as a clickable preset inside the app
and as a settings file you can run directly:

  Example Data\Settings_A_two_techniques.json
  Example Data\Settings_B_notation_logMAR.json
  Example Data\Settings_B_notation_Snellen.json
  Example Data\Settings_C_four_platforms.json
  Example Data\Settings_D_astigmatism.json
  Example Data\Settings_E_confounded_eye_matching.json
  Example Data\Settings_E_confounded_patient_matching.json

Use them in either of two ways:

  * In the app - click "Load config JSON" (bottom left)
    and pick one, then load its dataset.

  * Without the app - run the engine directly from THIS
    folder (results appear in "Example Output"):

      "C:\Program Files\R\R-4.5.2\bin\Rscript.exe" --vanilla ^
         engine\run_pipeline.R ^
         "Example Data\Settings_A_two_techniques.json"

All of them use paths RELATIVE to this folder, so they keep
working wherever you copy the app to.

Between them the datasets include both eyes of most
patients, mixed plus and minus cylinder notation, decimal,
logMAR, Snellen and ETDRS-letter acuity, follow-up visits
with drop-out, continuous, ordinal, two-level and
multi-level variables, and missing values at low rates -
so every feature of the app has something to act on.


-----------------------------------------------------------
 TYPICAL WORKFLOW
-----------------------------------------------------------
 1. Data & Groups .... load your Excel file (.xlsx or
                       .xlsm), define the groups to
                       compare, pick the reference
 2. Variables ........ assign each variable to Baseline /
                       Preop / Outcome and set its type;
                       plus->minus cylinder conversion (the
                       pre-op and post-op sets fill in once
                       those roles are mapped) and the
                       visual-acuity notation (below)
 3. Filters .......... inclusion rules (age, follow-up,
                       first treatment...) + PRK/LASIK subsets
 4. Matching ......... nearest-neighbor, exact, coarsened
                       exact (cem) or subclassification
                       matching of eyes or of patients
                       (both eyes together), with a live
                       matched-count preview (nearest)
 5. Regression ....... covariate-adjusted GEE
 6. GEE .............. clustered comparison (both eyes safe)
 7. Special outputs .. the StandardEyes figures and the
                       mEYEstro / AstigMATIC export tabs
 8. Run & Results .... run, preview, look at the figures,
                       then save where you want

Live row counts update as you edit, so you can see exactly
how many eyes each rule removes BEFORE running anything.
One analysis runs at a time; the Cancel button stops it.


-----------------------------------------------------------
 VISUAL ACUITY: TELL THE APP HOW YOUR FILE RECORDS IT
-----------------------------------------------------------
On the Variables tab, "VA notation in your file" says how
EVERY acuity column is written. Choose one:

  Decimal (default) .. 1.0 = 20/20, 0.5 = 20/40
  logMAR ............. 0.0 = 20/20, 0.3 = 20/40
  Snellen ............ 20/40 or 6/12, also 20/25-2, 6/9+1
  ETDRS letters ...... 85 = 20/20

* CF, HM, LP and NLP become missing (and are counted in the
  log) - they are never given an invented acuity. The
  mEYEstro tab shows the label (CF, HM, LP, NLP) instead.
* The run log says, per column, how many values were read
  and how many could not be. If most of a column cannot be
  read in the chosen notation (or a decimal value is 0 or
  negative), the run STOPS and tells you why.
* Decimal acuity declared as ETDRS letters is caught: if
  most values in a column set to "ETDRS letters" are not
  whole numbers, or the largest is below 5, the run STOPS
  ("VA column '...' is declared as ETDRS letters, but its
  values (e.g. 0.8, 1.0) look like decimal acuity").
* The app warns you if a column looks like a different
  notation from the one you chose.
* Filters use the values exactly as written in your file.
* Excel turns a typed 6/12 into a date: keep Snellen
  columns formatted as Text.
* "Analyze in logMAR" is on by default for the acuity
  columns you map. If your file is already in logMAR, the
  values are used as they are (never converted twice).
  Tables and figures then show the mean acuity in Snellen
  (the geometric mean) with the logMAR mean and SD, e.g.
  "20/20 (0.00 ± 0.05 logMAR)", and the tables add the
  range in Snellen. The figures always work from decimal
  acuity, whatever you choose.


-----------------------------------------------------------
 PRESETS (studies)
-----------------------------------------------------------
A preset stores EVERY setting of one study. Save one per
project, then reload it later in one click. Presets live in
the "webapp\presets" folder of this app and can be shared
with a colleague by copying the .json file.

The seven "Example - ..." presets described above are
included; presets you save appear beside them.


-----------------------------------------------------------
 OUTPUT
-----------------------------------------------------------
RUN & SAVE (the default) writes every result straight into
the output folder. With "Preview only" ticked, the results
are held inside the app so you can look at them first, and
nothing is written to your output folder until you choose
to save. Until then a temporary copy is kept in
webapp\.runs inside this folder; it is deleted when the
next run starts or when you close the app (or, after a
crash, the next time the app starts). You get:

  * Excel workbooks (summary, matching, GEE, regression),
    with optional tabs laid out in the input format of
    mEYEstro and AstigMATIC (the Contents sheet gives the
    cells to copy into each program's data file)
  * A matched-only workbook with the matched rows of every
    pairing
  * A covariate-balance (love) plot for each matched
    pairing, in the figure formats you chose, named like
    its workbook so a later run into the same folder never
    replaces it:
      LovePlot_<prefix>_<run>_<pairing>_<time stamp>.<format>
    (one file per chosen format, .tiff by default), with
    its legend file (<same name>_legend.txt) beside it;
    for example (a run without subsets is named "All";
    with subsets the pooled run is named "Pooled" and each
    subset run by its subset name):
      LovePlot_DS_E_eye_All_Condition_vs_Control_20261111_111221.tiff
      LovePlot_DS_E_eye_All_Condition_vs_Control_20261111_111221_legend.txt
  * Figure folders: each panel as its own TIFF plus the
    combined multi-panel figure, the double-angle plots
    and the semicircle vector plots (PNG, JPEG, PDF and
    EPS are also available, at up to 1200 dpi). Preview
    images of every combined figure are shown on the
    Run & Results tab, so you can check the figures
    without leaving the app.
    Figure style "journal" (the default) draws the figures
    on white with no frames, section bands, captions or
    shaded wedges; each figure's title, method notes and
    abbreviations are written beside it in
    <figure>_legend.txt. "annotated" keeps them inside the
    images (the look of version 1.0.0). A figure width
    (8.76, 12.69 or 17.77 cm, the journal column widths)
    lays every figure out at that width, so its text keeps
    its size. A combined figure then has two panels per row
    (one at 8.76 cm), rows about 7.6 cm high at 17.77 cm,
    and text of at least 6 pt; statistics lines that do not
    fit a panel at that size are listed in the legend file.
    A combined figure taller than a page (23 cm) is written
    as pages (_p1, _p2 ...), each with its own legend file,
    the panel letters continuing: at 17.77 cm the six-panel
    set is one page with up to four cohorts and two with
    five or more. EPS is vector, but
    PostScript has no
    transparency: the semi-transparent points and regions
    inside it are embedded as images at 1200 dpi (or the
    chosen resolution, if higher). Colors are RGB: convert
    to CMYK outside the app if a journal asks for it (R
    cannot write CMYK). The style, width, formats and
    resolution are set on the Special outputs tab, the P
    value style on the GEE tab (settings-file keys
    figure_style, figure_width_cm, graph_formats,
    graph_dpi and p_style).
    With the double-angle plots, the sub-folder
    "WHWK input" of this figure folder (one per run, so
    runs that share an output folder keep their own)
    holds tab-delimited input files for the
    Wilcox-Holladay-Wang-Koch R functions (one pair per
    plotted vector, see its README.txt; astig_whwk_files).
  * Optionally the filtered dataset as CSV: the rows that
    are analyzed (in a group, and in a subset when subsets
    are defined), with a GroupLabel column (and Subset)
    leading. It holds the values as analyzed, so a header
    carries a suffix where a column no longer holds what
    was recorded: acuity converted to logMAR (_logMAR; the
    recorded acuity is not repeated), acuity text read as
    decimal (_decimal), and refraction transposed to minus
    cylinder when the conversion is on (_minus_cyl, sphere,
    cylinder and axis of each converted set). A missing
    value is an empty cell. Matched_Data (the matched-only
    workbook) uses the same headers, with GroupLabel and
    MatchIt's own columns at the end: distance (the
    propensity score, on the scale the Manifest names),
    weights (the matching weight) and subclass (the
    matched set), then Pairing. "Export filtered rows
    (CSV)" on the Data & Groups tab writes the same rows
    and headers without running the analysis.

P values are printed in the journal style by default
(.27, .003, <.001, >.99); "classic" keeps 0.266 / <0.001.
Each workbook's Manifest sheet records the app version
and build, the settings used and the run duration.

Tables, sheets, figures and the log name every column by
its header exactly as typed in your file (Hebrew and other
scripts included).

Matched cohorts: when the matching weights are not all 1
(e.g. 2:1 matching with a caliper, matching with
replacement, or exact / cem / subclassification
matching), the matched tables and figure P values use
those weights, and each matched sheet says so. So do the
matched GEE / regression when "Use matching weights" is
on. With "Matched tables" = unweighted (Matching tab;
matched_table_weights = "none") the matched descriptives
and table tests count every matched eye once instead,
whatever the weights; the matched GEE / regression still
follow "Use matching weights", and each matched sheet,
the Manifest and the figure legends say which weighting
the tables use. Every model of a matched cohort, weighted or not, uses
an independence working correlation with robust SE
clustered on the patient (the approach MatchIt
recommends), so a weighted estimate reproduces the
weighted means and proportions shown in the table; matched
sets are not used as clusters. Unmatched GEE keeps the
exchangeable correlation, refitted with an independence
working correlation when the exchangeable fit is not
accepted; per-patient variables are modeled on one row
per patient.

Matching unit (Matching tab, "Match"): eyes (the default)
or patients (both eyes together). With patients, each
patient's analyzed eyes in a group form one matching unit:
eye-level covariates are averaged over those eyes,
per-patient covariates are taken once, and the propensity
model, caliper, ratio and order work on these units (exact
and coarsened exact matching form their strata,
subclassification its subclasses, on the units). A
covariate an eye lacks is taken from the patient's other
eye in the group (the run log counts these patients), so a
patient is left out of matching only when all of its eyes
in the group lack a covariate. Every
eye of a matched patient enters the matched cohort with
the patient's matching weight, so both eyes enter or leave
together; balance (love plot, PSM sheet) is computed on
the patients. A patient with eyes in two groups forms one
unit in each group (the run log and the PSM sheet count
them). Patients need a patient ID column. The PSM sheet,
the Manifest, the figure legends and the run log say
"patients matched (both eyes kept together)". Settings
file key: psm_unit = "eye" or "patient".

Small groups: a clustered model (GEE, adjusted regression,
the figures' clustered P values) needs at least 6 usable
eyes and 3 patients; below that the output says "Not
estimable - too few eyes for this model". A group with 2-9
patients gets its P value with a caution, and the run log
lists those groups once under "Check before using the
results".


-----------------------------------------------------------
 TROUBLESHOOTING
-----------------------------------------------------------
"Windows protected your PC" on first start
    Windows shows this for files from a downloaded ZIP.
    Click "More info", then "Run anyway". To avoid it,
    right-click the downloaded ZIP, choose Properties, tick
    "Unblock", then extract it again.

"Python runtime is missing" or
"the bundled runtime is incomplete"
    The folder was copied incompletely, or you are running
    it from inside a ZIP. Extract/copy the whole folder.

"StandardEyes cannot write to its own folder"
    The folder is read-only for you - typically because it
    is under C:\Program Files or on a read-only share. Move
    the whole folder somewhere you can write to (Documents
    or C:\StandardEyes) and start it from there. Copying it
    again to the same place will not help.

"Figure NOT written ... the full path is ... characters"
    Windows refuses a file path longer than 260 characters
    (unless long paths are enabled), and the figures of a
    matched run have long names. Move the app folder near
    the drive root (C:\StandardEyes), or choose a shorter
    output folder or output prefix, and run again.

"Windows security blocked a component of StandardEyes"
    Smart App Control, AppLocker or an antivirus program
    refused to load part of the bundled runtime. Ask your
    IT department to allow this folder (or turn off Smart
    App Control). Copying the folder again will not help.

The browser shows nothing / cannot connect
    Give it a few seconds on first start, then reload the
    address printed in the console window. That console
    also shows the version and any startup error.

Am I looking at the right app?
    The header says "StandardEyes - By Ben Ephraim Noyman D,
    Mimouni M, Safir M" followed by its version in the
    form "v1.2.0 (build <date>-se.<n>)": the release
    number, then the internal build id, which contains
    "-se".
    The console window prints both at startup. Quote both
    when you report a problem.

"VA column ... is declared as ..."
    The acuity values do not match the notation chosen
    under "VA notation in your file" (Variables tab).
    Choose the notation your file really uses, or correct
    the column - see VISUAL ACUITY above.

"WARNING: Matching skipped - no matching covariate found
in the file"
    None of the matching covariates names a column of the
    loaded file (renamed or removed column, or a preset
    made for another file). Choose the covariates again on
    the Matching tab. If only some are missing, the others
    are used and the missing ones are listed as "not found
    in the file (ignored)".

"... date-formatted cell(s) in numeric column ... set to
missing"
    A cell in a column of numbers is formatted as a date in
    Excel. Its hidden serial number (a count of days) would
    pass numeric filters and distort means, so the cell is
    treated as missing. Correct the cell in Excel and run
    again.

"An analysis is already running"
    Only one run at a time. Wait for it to finish or press
    Cancel.

"R is not set up yet"
    Click "R setup" (top right) and point it at
    Rscript.exe, or install R 4.5.x.

A red "out of date" bar appears
    Close the console window and start the app again,
    then press Ctrl+F5 in the browser.

Analysis fails right after starting
    Check your R version is 4.5.x - the Setup screen
    reports the version it found.

Two people, one shared drive
    Each person should COPY the folder to their own
    computer. Running the same copy from a network share
    at the same time is not supported.


-----------------------------------------------------------
 LICENSE
-----------------------------------------------------------
StandardEyes is free software under the MIT license (see
LICENSE). The bundled Python, R packages and JavaScript
keep their own licenses: see THIRD_PARTY_NOTICES.md and
the "licenses" folder of the release.


-----------------------------------------------------------
 StandardEyes
 By Ben Ephraim Noyman D, Mimouni M, Safir M
-----------------------------------------------------------
