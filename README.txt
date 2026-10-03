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
  LogMAR ............. 0.0 = 20/20, 0.3 = 20/40
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
* "Analyse in LogMAR" is optional. If your file is already
  in logMAR, the values are used as they are (never
  converted twice). The figures always work from decimal
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
    pairing, named like its workbook so a later run into
    the same folder never replaces it:
      LovePlot_<prefix>_<subset>_<pairing>_<time stamp>.png
    for example (the pooled run's subset is "All"):
      LovePlot_Ex02_Full_All_Diabetes_vs_Control_20261002_205654.png
  * Figure folders: each panel as its own TIFF plus the
    combined multi-panel figure, the double-angle plots
    and the semicircle vector plots (PNG, JPEG and PDF
    are also available). Preview images of every combined
    figure are shown on the Run & Results tab, so you can
    check the figures without leaving the app.
  * Optionally the filtered dataset as CSV

Tables, sheets, figures and the log name every column by
its header exactly as typed in your file (Hebrew and other
scripts included).

Matched cohorts: when the matching weights are not all 1
(e.g. 2:1 matching with a caliper, matching with
replacement, or exact / cem / subclassification
matching), the matched tables and figure P values use
those weights, and each matched sheet says so. So do the
matched GEE / regression when "Use matching weights" is
on. Every weighted model uses an independence working
correlation with robust SE clustered on the patient (the
approach MatchIt recommends), so its estimate reproduces
the weighted means and proportions shown in the table.
Unweighted GEE keeps the exchangeable correlation.

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
    form "v1.0.0 (build <date>-se.<n>)": the release
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
