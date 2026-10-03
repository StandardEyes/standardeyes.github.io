# ============================================================
# engine/match_preview.R
# Fast matched-count preview: runs the REAL MatchIt (same settings
# the pipeline uses) on slim per-pairing CSVs (GroupLabel + covariates
# only) prepared by the web backend, and reports counts.
# The settings are normalised by the pipeline's own .psm_settings()
# and every matchit() call runs under the same locally fixed seed
# (psm$seed), so a random matching order draws exactly what the run
# draws. Covariates the backend lists in run$text_covs are read as
# text (then factors, as in the run), never re-guessed as numbers.
# Usage: Rscript engine/match_preview.R <preview_config.json>
# Emits: RESULT <json>  |  ERROR <msg>   (one line each)
# ============================================================
emit <- function(...) {
  line <- paste(vapply(list(...), function(z) paste(as.character(z), collapse = " "), character(1)),
                collapse = "")
  line <- sub("[ \t]*[\r\n][\r\n \t]*$", "", line)          # trailing line breaks
  line <- gsub("[ \t]*[\r\n]+[ \t]*", " | ", line)
  cat(line, "\n", sep = "")
  flush(stdout())
}

# ---- app root; use the BUNDLED R packages (see run_pipeline.R) ----
app_root <- local({
  a <- commandArgs(trailingOnly = FALSE)
  h <- grep("^--file=", a, value = TRUE)
  if (length(h)) dirname(dirname(normalizePath(sub("^--file=", "", h[1]), mustWork = FALSE))) else getwd()
})
local({
  bl <- file.path(app_root, "runtime", "Rlib")
  if (dir.exists(bl)) .libPaths(c(bl, .libPaths()))
})

ok <- tryCatch({
  suppressWarnings(suppressPackageStartupMessages({ library(MatchIt); library(jsonlite) }))
  TRUE
}, error = function(e) { emit("ERROR packages: ", conditionMessage(e)); FALSE })
if (!isTRUE(ok)) quit(status = 1)

# the engine's own helpers: with_local_seed() and .psm_settings() (both files
# only define functions, so no other package is needed to source them)
for (f in c("helpers.R", "analysis.R")) {
  fp <- file.path(app_root, "R", f)
  if (!file.exists(fp)) { emit("ERROR Missing engine module: ", fp); quit(status = 1) }
  ok <- tryCatch({ source(fp, local = FALSE); TRUE },
                 error = function(e) { emit("ERROR ", f, ": ", conditionMessage(e)); FALSE })
  if (!isTRUE(ok)) quit(status = 1)
}

args <- commandArgs(trailingOnly = TRUE)
if (length(args) < 1 || !file.exists(args[1])) { emit("ERROR no config"); quit(status = 1) }
cfg <- jsonlite::fromJSON(args[1], simplifyVector = TRUE,
                          simplifyDataFrame = FALSE, simplifyMatrix = FALSE)
psm <- .psm_settings(if (is.null(cfg$psm)) list() else cfg$psm)
ref <- as.character(cfg$reference_group)

out_runs <- list()
CAP <- 300  # max unmatched IDs to return per side

pids <- function(sub, rn) {
  if (!("PatientID" %in% names(sub)) || length(rn) == 0) return(character(0))
  ids <- unique(as.character(sub[rn, "PatientID"]))
  ids[!is.na(ids)]
}

for (run in cfg$runs) {
  # text covariates stay text (then factors, as in the run): read.csv would
  # otherwise re-guess codes like 1/2/3 as one linear numeric term
  text_covs <- as.character(unlist(run$text_covs))
  cc_classes <- if (length(text_covs)) stats::setNames(rep("character", length(text_covs)), text_covs) else NA
  d <- tryCatch(utils::read.csv(run$csv, check.names = FALSE, stringsAsFactors = FALSE,
                                fileEncoding = "UTF-8-BOM", colClasses = cc_classes),
                error = function(e) NULL)
  if (is.null(d) || !("GroupLabel" %in% names(d))) next
  d$GroupLabel <- as.character(d$GroupLabel)
  covs <- setdiff(names(d), c("GroupLabel", "PatientID", "RowID"))
  for (cv in intersect(text_covs, covs)) d[[cv]] <- as.character(d[[cv]])
  pairings <- list()

  for (tg in setdiff(unique(d$GroupLabel), ref)) {
    sub <- d[d$GroupLabel %in% c(ref, tg), , drop = FALSE]
    rownames(sub) <- as.character(seq_len(nrow(sub)))
    sub$.treat <- as.integer(sub$GroupLabel == tg)
    cc <- stats::complete.cases(sub[, covs, drop = FALSE])
    res <- list(treated = tg, reference = ref,
                treated_total = sum(sub$.treat == 1),
                treated_cc    = sum(sub$.treat == 1 & cc),
                ref_total     = sum(sub$.treat == 0),
                ref_cc        = sum(sub$.treat == 0 & cc),
                matched_treated = NA, controls_used = NA, error = NULL,
                unmatched_treated_n = NA, unmatched_control_n = NA,
                unmatched_treated_ids = character(0), unmatched_control_ids = character(0),
                unmatched_treated_ids_total = NA, unmatched_control_ids_total = NA)

    if (res$treated_cc > 0 && res$ref_cc > 0) {
      subcc <- sub[cc, , drop = FALSE]
      for (cv in covs) if (is.character(subcc[[cv]])) subcc[[cv]] <- factor(subcc[[cv]])
      f <- stats::as.formula(paste(".treat ~", paste(sprintf("`%s`", covs), collapse = " + ")))
      # cem on categorical covariates only runs as exact matching, as in the run
      m <- with_local_seed(psm$seed, tryCatch(
        MatchIt::matchit(f, data = subcc, method = .psm_method_for(psm$method, subcc, covs),
                         distance = psm$distance,
                         ratio = psm$ratio, replace = psm$replace, caliper = psm$caliper,
                         std.caliper = psm$std_caliper, m.order = psm$m_order),
        error = function(e) e))
      if (inherits(m, "error")) {
        res$error <- conditionMessage(m)
      } else {
        md <- MatchIt::match.data(m)
        res$matched_treated <- sum(md$.treat == 1)
        res$controls_used   <- sum(md$.treat == 0)
        matched_rn <- rownames(md)
        t_un <- setdiff(rownames(subcc)[subcc$.treat == 1], matched_rn)
        c_un <- setdiff(rownames(subcc)[subcc$.treat == 0], matched_rn)
        res$unmatched_treated_n <- length(t_un)
        res$unmatched_control_n <- length(c_un)
        t_ids <- pids(subcc, t_un); c_ids <- pids(subcc, c_un)
        res$unmatched_treated_ids_total <- length(t_ids)
        res$unmatched_control_ids_total <- length(c_ids)
        # I() keeps one-element vectors as JSON arrays despite auto_unbox
        res$unmatched_treated_ids <- I(utils::head(t_ids, CAP))
        res$unmatched_control_ids <- I(utils::head(c_ids, CAP))
        # matched rows for the live data preview (RowID = 0-based position in
        # the backend's run frame); weights + PS aligned with the same order
        if ("RowID" %in% names(subcc)) {
          ROWCAP <- 1000
          sel <- utils::head(matched_rn, ROWCAP)
          res$matched_rows <- I(as.integer(subcc[sel, "RowID"]))
          res$matched_w    <- I(if ("weights"  %in% names(md)) as.numeric(utils::head(md$weights, ROWCAP)) else rep(1, length(sel)))
          res$matched_ps   <- I(if ("distance" %in% names(md)) round(as.numeric(utils::head(md$distance, ROWCAP)), 5) else rep(NA_real_, length(sel)))
        }
      }
    }
    pairings[[length(pairings) + 1]] <- res
  }
  out_runs[[length(out_runs) + 1]] <- list(label = run$label, pairings = pairings)
}

emit("RESULT ", jsonlite::toJSON(list(runs = out_runs), auto_unbox = TRUE, null = "null"))
quit(status = 0)
