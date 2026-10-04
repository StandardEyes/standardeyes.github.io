===========================================================
 StandardEyes
 By Noyman DBE, Mimouni M, Safir M
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
 WHAT IS NEW IN 1.1.0
-----------------------------------------------------------
Version 1.1.0 changes several defaults, so the same data
and settings can give different numbers than 1.0.0. A
preset or settings file saved by 1.0.0 still loads and
runs; a setting it does not contain takes the new default.
Each workbook's Manifest sheet records the settings used.
What changed, and how to get the 1.0.0 behaviour back:

* Tests in the tables are clustered by patient. When a
  patient has two analysed eyes, each per-eye row of the
  summary tables and the Publication Table gets its P value
  from a patient-clustered GEE test - the same P value as
  the figure of that outcome.
  Back: GEE tab, "Summary-table tests" = classical
  (table_tests). Turning off "Cluster p-values by patient"
  also does it.

* Small-sample correction of the clustered tests: every GEE
  test and interval uses the Mancl-DeRouen corrected robust
  SE with a t reference (F for joint tests).
  Back: GEE tab, "GEE small-sample inference" = none
  (gee_small_sample).

* The caliper is on the logit scale: 0.2 SD of the logit of
  the propensity score (Austin 2011).
  Back: Matching tab, Distance = "propensity score,
  probability scale" (psm_distance = logit).

* Visual acuity is analysed in logMAR and shown as Snellen:
  "20/20 (0.00 ± 0.05 logMAR)" - the Snellen geometric mean
  with the logMAR mean and SD - and the range in Snellen.
  Back: Variables tab, untick "Analyse visual acuity in
  logMAR" (do_logmar = false).

* P values in journal style: .27, .003, <.001, >.99.
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
  stays). The 95% region of the eyes is the standard's
  depth polygon, the centroid's 95% confidence region a
  bootstrap polygon, and the robust tests resample
  patients.
  Back: the plus-cylinder vectors and the plot set have no
  switch (1.0.0 mishandled the cylinder sign). On the
  Special outputs tab, "95% region of the eyes" = normal,
  "Centroid 95% confidence region" = Hotelling and
  "Astigmatism tests" = classic give the parametric
  versions. Hull peeling is no longer offered.

* New sheets "Astigmatism (JCRS 2025)" (centroids, total
  SD, magnitudes, intervals and the standard's tests) and
  "Astigmatism vectors" (per eye), with a CSV copy.
  Back: Special outputs tab, untick "Astigmatism (JCRS
  2025) sheet" (astig_sheet = false).

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
 TRY IT FIRST: THE EXAMPLE DATASET
-----------------------------------------------------------
The folder "Example Data" contains a ready-made, made-up
dataset (Example_Refractive_Dataset.xlsx) plus a library of
seven worked examples, so you can see a full analysis before
touching your own data. It contains no patient information.

  1. Load  Example Data\Example_Refractive_Dataset.xlsx
  2. Click one of the "Example - ..." presets (below)
  3. Tick "Preview only", then press RUN

The seven examples, each showing a different capability:

  01_Two_cohorts_simple
      The minimum useful run: two groups, no matching, GEE
      or regression - just the summary tables and figures.

  02_Two_cohorts_full
      The showcase. Propensity-score matching, clustered
      GEE, covariate-adjusted regression, the full figure
      suite with the stability-over-time panel, and the
      within-group distribution tables - with smoking and
      surgeon counted once per PATIENT while the two eye
      measurements stay per eye.

  03_Three_cohorts
      More than two groups: omnibus tests, one adjusted
      estimate per group against the reference, and every
      cohort superimposed on the same figures.

  04_Subsets_PRK_LASIK
      One configuration producing a complete, independent
      analysis for PRK and for LASIK.

  05_Stability_focus
      The stability-over-time curve as its own panel.
      Five follow-up visits are mapped; SEQ is taken from a
      column for two of them and computed from sphere +
      cylinder for the other three, so one run exercises
      both routes.

  06_Figures_only_no_stats
      Figure toggles - astigmatism panels and vector plots
      off, stability on - showing the composite adapt its
      section headers.

  07_Full_figure_suite
      The full suite on three cohorts: every group on ONE
      chart in its own colour, omnibus + vs-reference stat
      lines, p-values clustered by patient, and double-angle
      astigmatism plots - the presentation JCRS now
      requires - with each group's centroid, a 95% region
      and a 95% confidence interval of the centroid.

Each is supplied BOTH as a clickable preset inside the app
and as a settings file you can run directly:

  Example Data\Settings_01_Two_cohorts_simple.json
  Example Data\Settings_02_Two_cohorts_full.json
  Example Data\Settings_03_Three_cohorts.json
  Example Data\Settings_04_Subsets_PRK_LASIK.json
  Example Data\Settings_05_Stability_focus.json
  Example Data\Settings_06_Figures_only_no_stats.json
  Example Data\Settings_07_Full_figure_suite.json

Use them in either of two ways:

  * In the app - click "Load config JSON" (bottom left)
    and pick one, then load the example spreadsheet.

  * Without the app - run the engine directly from THIS
    folder (results appear in "Example Output"):

      "C:\Program Files\R\R-4.5.2\bin\Rscript.exe" --vanilla ^
         engine\run_pipeline.R ^
         "Example Data\Settings_02_Two_cohorts_full.json"

All of them use paths RELATIVE to this folder, so they keep
working wherever you copy the app to.

The example deliberately includes both eyes of many
patients, retreatment rows, mixed plus/minus cylinder
notation, five consecutive follow-up visits with realistic
drop-out, all four variable types and missing values - so
every feature of the app has something to act on.


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
 4. Matching ......... nearest-neighbour, exact, coarsened
                       exact (cem) or subclassification
                       matching, with a live matched-count
                       preview (nearest)
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
* "Analyse in logMAR" is on by default for the acuity
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
Results are held inside the app first so you can preview
them. Nothing is written to your output folder until you
choose to save. Until then a temporary copy is kept in
webapp\.runs inside this folder; it is deleted when the
next run starts or when you close the app (or, after a
crash, the next time the app starts). You get:

  * Excel workbooks (summary, matching, GEE, regression),
    with optional tabs formatted for direct entry into
    mEYEstro and AstigMATIC
  * A matched-only workbook with the matched rows of every
    pairing
  * A covariate-balance (love) plot for each matched
    pairing, in the figure formats you chose, named like
    its workbook so a later run into the same folder never
    replaces it:
      LovePlot_<prefix>_<run>_<pairing>_<time stamp>.png
    for example (a run without subsets is named "All";
    with subsets the pooled run is named "Pooled" and each
    subset run by its subset name):
      LovePlot_Ex02_Full_All_Cohort A_vs_Reference_20261004_185353.png
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
    its size; a combined figure taller than a page (23 cm)
    is then written as pages (_1, _2 ...), each with its
    own legend file. EPS is vector, but PostScript has no
    transparency: the semi-transparent points and regions
    inside it are embedded as images at 1200 dpi (or the
    chosen resolution, if higher). Colors are RGB: convert
    to CMYK outside the app if a journal asks for it (R
    cannot write CMYK). The style, width, formats and
    resolution are set on the Special outputs tab, the P
    value style on the GEE tab (settings-file keys
    figure_style, figure_width_cm, graph_formats,
    graph_dpi and p_style).
  * Optionally the filtered dataset as CSV: the rows that
    are analysed (in a group, and in a subset when subsets
    are defined), with a GroupLabel column (and Subset)

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
on. Every model of a matched cohort, weighted or not, uses
an independence working correlation with robust SE
clustered on the patient (the approach MatchIt
recommends), so a weighted estimate reproduces the
weighted means and proportions shown in the table; matched
sets are not used as clusters. Unmatched GEE keeps the
exchangeable correlation.

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
    The header says "StandardEyes - By Noyman DBE,
    Mimouni M, Safir M" followed by its version in the
    form "v1.1.0 (build <date>-se.<n>)": the release
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
 LICENCE
-----------------------------------------------------------
StandardEyes is free software under the MIT licence (see
LICENSE). The bundled Python, R packages and JavaScript
keep their own licences: see THIRD_PARTY_NOTICES.md and
the "licenses" folder of the release.


-----------------------------------------------------------
 StandardEyes
 By Noyman DBE, Mimouni M, Safir M
-----------------------------------------------------------
