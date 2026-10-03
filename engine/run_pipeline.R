# ============================================================
# engine/run_pipeline.R
# Headless CLI wrapper around the EXISTING R analysis engine.
# Usage:  Rscript engine/run_pipeline.R <config.json>
#
# Sources R/helpers.R, R/analysis.R, R/export.R, R/graphs.R and
# R/pipeline.R, reads the cfg the web GUI builds, runs
# run_analysis_pipeline(), and streams a simple line protocol
# to stdout so a parent process (the Python web backend) can
# show progress and collect the output file list:
#
#   PROGRESS <fraction 0..1> <message>
#   LOG <message>
#   RESULT <json: {files, matched_files, graph_dirs, output_dir}>
#                              (absolute paths, backslashes on Windows)
#   ERROR <message>            (on failure; exit status 1)
#
# Every event is exactly ONE line: line breaks inside a message
# are folded into " | ". A run that produces no workbook ends
# with ERROR (exit status 1), never with an empty RESULT.
#
# This file contains no analysis logic of its own.
# ============================================================

# ---- Locate the app root (folder that contains R/ and engine/) ----
find_script_path <- function() {
  args <- commandArgs(trailingOnly = FALSE)
  hit <- grep("^--file=", args, value = TRUE)
  if (length(hit) > 0) return(normalizePath(sub("^--file=", "", hit[1]), mustWork = FALSE))
  # Fallback: assume run from the app root.
  normalizePath(file.path(getwd(), "engine", "run_pipeline.R"), mustWork = FALSE)
}

script_path <- find_script_path()
app_root    <- dirname(dirname(script_path))   # the app folder
# the engine's own data files (engine/data/snellen_lookup.csv) are found from
# here, whatever the working directory of the caller
options(standardeyes.app_root = app_root)

# a null default device, so no stray Rplots.pdf ever lands in the app folder
# (every real figure is written through ggsave/its own device)
grDevices::pdf(NULL)

# one protocol event = one line: a multi-line message (an R condition with
# its cause on the next line) is folded, so nothing is demoted to noise
emit <- function(...) {
  line <- paste(vapply(list(...), function(z) paste(as.character(z), collapse = " "), character(1)),
                collapse = "")
  line <- sub("[ \t]*[\r\n][\r\n \t]*$", "", line)          # trailing line breaks
  line <- gsub("[ \t]*[\r\n]+[ \t]*", " | ", line)
  cat(line, "\n", sep = "")
  flush(stdout())
}

# ---- use the BUNDLED R packages ----
# runtime/Rlib ships every package the engine needs, so nothing has to be
# installed on the user's machine (their network may block CRAN). It is put
# FIRST on the search path but the user's own library is still available.
bundled_lib <- file.path(app_root, "runtime", "Rlib")
if (dir.exists(bundled_lib)) {
  .libPaths(c(bundled_lib, .libPaths()))
  rv <- paste(R.version$major, strsplit(R.version$minor, ".", fixed = TRUE)[[1]][1], sep = ".")
  if (!identical(rv, "4.5"))
    emit("LOG NOTE: bundled packages were built for R 4.5.x but this is R ", rv,
         ". If the analysis fails, install R 4.5.x.")
}

# ---- Args ----
args_only <- commandArgs(trailingOnly = TRUE)
if (length(args_only) < 1) { emit("ERROR No config file argument supplied."); quit(status = 1) }
cfg_path <- args_only[1]
if (!file.exists(cfg_path)) { emit("ERROR Config file not found: ", cfg_path); quit(status = 1) }

# ---- Libraries (same set the original GUI attaches) ----
# Checked by name first, so a missing package is named in plain words. The
# figure packages are needed for every run, workbooks included: cobalt
# imports ggplot2 and gtable. zip writes every workbook (openxlsx saves
# through it, and sanitize_xlsx rewrites the saved file with it).
need_pkgs <- c("readxl", "dplyr", "tibble", "openxlsx", "zip", "stringr", "MatchIt", "cobalt",
               "geepack", "rlang", "jsonlite", "ggplot2", "gtable")
pkgs_missing <- need_pkgs[!nzchar(vapply(need_pkgs, function(p) system.file(package = p), character(1)))]
if (length(pkgs_missing)) {
  emit("ERROR R package(s) not installed: ", paste(pkgs_missing, collapse = ", "),
       ". StandardEyes needs them for every run (cobalt imports ggplot2 and gtable; ",
       "openxlsx writes the workbooks through zip) - ",
       "install them, or use the bundled packages in runtime/Rlib.")
  quit(status = 1)
}
ok <- tryCatch({
  suppressWarnings(suppressPackageStartupMessages({
    library(readxl); library(dplyr); library(tibble); library(openxlsx)
    library(stringr); library(MatchIt); library(cobalt); library(geepack)
    library(rlang); library(jsonlite)
  }))
  TRUE
}, error = function(e) { emit("ERROR Failed to load R packages: ", conditionMessage(e)); FALSE })
if (!isTRUE(ok)) quit(status = 1)

# ---- Source the existing engine modules ----
# a module that fails to load ends the run with an ERROR line, never with a
# bare R error the GUI cannot show
for (f in c("helpers.R", "analysis.R", "export.R", "graphs.R", "pipeline.R")) {
  fp <- file.path(app_root, "R", f)
  if (!file.exists(fp)) { emit("ERROR Missing engine module: ", fp); quit(status = 1) }
  ok <- tryCatch({ source(fp, local = FALSE); TRUE },
                 error = function(e) { emit("ERROR Engine module ", f, " could not be loaded: ",
                                            conditionMessage(e)); FALSE })
  if (!isTRUE(ok)) quit(status = 1)
}

# ---- Read config (keep filter_rules etc. as lists, not data frames) ----
cfg <- tryCatch(
  jsonlite::fromJSON(cfg_path, simplifyVector = TRUE,
                     simplifyDataFrame = FALSE, simplifyMatrix = FALSE),
  error = function(e) { emit("ERROR Could not parse config JSON: ", conditionMessage(e)); NULL })
if (is.null(cfg)) quit(status = 1)

# Relative paths in a settings file are relative to the app folder (the
# shipped presets name "Example Data/..." and "Example Output"; the GUI server
# resolves them the same way). A relative data path is still tried from the
# working directory first.
is_relative <- function(p) is.character(p) && length(p) == 1 && !is.na(p) && nzchar(p) &&
  !grepl("^(~|[A-Za-z]:|[\\/])", p)
if (is_relative(cfg$file_path) && !file.exists(cfg$file_path) &&
    file.exists(file.path(app_root, cfg$file_path)))
  cfg$file_path <- normalizePath(file.path(app_root, cfg$file_path), mustWork = FALSE)
if (is_relative(cfg$output_dir))
  cfg$output_dir <- normalizePath(file.path(app_root, cfg$output_dir), mustWork = FALSE)

if (is.null(cfg$file_path) || !nzchar(cfg$file_path) || !file.exists(cfg$file_path)) {
  emit("ERROR Data file not found: ", cfg$file_path %||% "<none>"); quit(status = 1)
}

# ---- Run, streaming progress + logs ----
logger   <- function(m) emit("LOG ", paste(as.character(m), collapse = " "))
progress <- function(frac, msg) emit("PROGRESS ", format(frac, nsmall = 3), " ", msg)

result <- tryCatch(
  run_analysis_pipeline(cfg, logger = logger, progress = progress),
  error = function(e) { emit("ERROR ", conditionMessage(e)); NULL })

if (is.null(result)) quit(status = 1)

# every path in the RESULT line in one separator style (backslashes on
# Windows), the same as the "Saved:" LOG lines
os_path <- function(p) {
  p <- as.character(p)
  if (!length(p)) return(character(0))
  p <- normalizePath(p, winslash = "\\", mustWork = FALSE)
  if (.Platform$OS.type == "windows") gsub("/", "\\", p, fixed = TRUE) else p
}

# I() keeps one-element vectors as JSON arrays despite auto_unbox
emit("RESULT ", jsonlite::toJSON(
  list(files = I(os_path(result$files %||% character(0))),
       matched_files = I(os_path(result$matched_files %||% character(0))),
       graph_dirs = I(os_path(result$graph_dirs %||% character(0))),
       output_dir = if (nzchar(as.character(result$output_dir %||% "")[1]))
                      os_path(result$output_dir)[1] else ""),
  auto_unbox = TRUE, null = "null"))

quit(status = 0)
