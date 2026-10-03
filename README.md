# StandardEyes

**Standardized comparative analysis and outcomes reporting for refractive surgery — free, local, reproducible.**

StandardEyes takes a spreadsheet with **one row per eye per surgery** and produces publication-ready
comparative outcomes for any number of groups: group tables with automatic test selection,
propensity-score matching with balance reporting (matching weights carried into the matched tables
and figures), GEE clustered on the patient, covariate-adjusted regression, Excel workbooks, and the
StandardEyes figures — the published standardized-graph metrics with every cohort superimposed on one
chart, plus JCRS-2025 double-angle astigmatism plots. Visual acuity is read in the notation the file
uses — decimal, logMAR, Snellen (20/x or 6/x) or ETDRS letters — and can be analysed in logMAR.

Everything runs on your own computer. Nothing is uploaded, and no internet connection is needed.

**➡ Website and download: https://standardeyes.github.io**

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

The app shows its version as `v1.0.0 (build <id>)`: the release number used on the website and in
`CITATION.cff`, then the internal build id. The workbook's Manifest sheet records both.

## What is in this repository

| Folder | Contents |
|---|---|
| `R/` | The statistical engine (analysis, pipeline, export, figures) |
| `engine/` | Headless R entry points (`run_pipeline.R`, `match_preview.R`) and the decimal-to-Snellen lookup |
| `webapp/` | The local FastAPI server, the Alpine.js interface and the seven *Example - …* presets |
| `Example Data/` | Simulated dataset, astigmatism validation dataset and the seven example settings files (`Settings_01_Two_cohorts_simple.json` … `Settings_07_Full_figure_suite.json`) |
| `docs/` | The project website (GitHub Pages) |

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
