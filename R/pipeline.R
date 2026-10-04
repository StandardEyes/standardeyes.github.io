# ============================================================
# pipeline.R
# Turns a GUI config (cfg) into resolved column names, prepares
# the data (standardize / visual acuity / exclude / filter), splits
# by subset (e.g. PRK / LASIK), builds the group factor, and writes
# one output workbook per run.
# ============================================================

`%||%` <- function(a, b) if (is.null(a) || length(a) == 0 || (length(a) == 1 && is.na(a))) b else a

# TRUE for NULL, NA, an empty vector/list or blank text.
.pl_blank <- function(x) {
  if (is.null(x) || length(x) == 0) return(TRUE)
  x <- as.character(unlist(x))
  !length(x) || all(is.na(x) | !nzchar(trimws(x)))
}

# Resolve all raw names in cfg -> cleaned names against the file header.
resolve_config <- function(cfg, raw_names, logger = message) {
  R <- function(x) resolve_raw_to_clean(raw_names, x)
  V <- function(x, lab) resolve_vec(raw_names, x, lab, logger)
  # One optional column: NA when not set. A name that IS set but does not
  # resolve (missing, or ambiguous between two headers) is logged, so a
  # mapping never disappears silently.
  O <- function(x, label, consequence = "") {
    if (.pl_blank(x)) return(NA_character_)
    x <- as.character(unlist(x))[1]
    k <- R(x)
    if (is.na(k))
      logger(paste0(unresolved_name_message(raw_names, x, label),
                    if (nzchar(consequence)) paste0(" ", consequence) else ""))
    as.character(k)
  }

  res <- list()
  if (.pl_blank(cfg$group_source_var)) stop("No group source column is set.", call. = FALSE)
  res$group_source_clean <- R(cfg$group_source_var)
  if (is.na(res$group_source_clean))
    stop(unresolved_name_message(raw_names, as.character(unlist(cfg$group_source_var))[1],
                                 "Group source column"), call. = FALSE)
  res$patient_id_clean <- O(cfg$patient_id_raw, "Patient ID column",
                            "Each eye is treated as its own patient.")
  res$eye_id_clean     <- O(cfg$eye_id_raw, "Eye ID column")

  res$patient_level_params_clean <- V(cfg$patient_level_params_raw, "patient_level_params")

  # Selected params per section, honoring an optional user-defined display
  # order (cfg$<section>$order). Params selected but absent from the order
  # list are appended, so the two can never drift apart.
  TYPE_KEYS <- c("continuous","ordinal","dichotomous","categorical")
  sec_params <- function(sec_cfg, label) {
    sel <- unique(unlist(sec_cfg[TYPE_KEYS]))
    ord <- unlist(sec_cfg$order)
    raw <- c(ord[ord %in% sel], setdiff(sel, ord))
    V(raw, label)
  }
  res$baseline_params_clean <- sec_params(cfg$baseline, "baseline_params")
  res$preop_params_clean    <- sec_params(cfg$preop,    "preop_params")
  res$outcome_params_clean  <- sec_params(cfg$outcome,  "outcome_params")
  res$within_params_clean   <- sec_params(cfg$within_group, "within_group_params")

  res$baseline_type_map <- build_type_map(raw_names,
    cfg$baseline$continuous, cfg$baseline$ordinal, cfg$baseline$dichotomous, cfg$baseline$categorical, logger)
  res$preop_type_map <- build_type_map(raw_names,
    cfg$preop$continuous, cfg$preop$ordinal, cfg$preop$dichotomous, cfg$preop$categorical, logger)
  res$outcome_type_map <- build_type_map(raw_names,
    cfg$outcome$continuous, cfg$outcome$ordinal, cfg$outcome$dichotomous, cfg$outcome$categorical, logger)
  res$within_type_map <- build_type_map(raw_names,
    cfg$within_group$continuous, cfg$within_group$ordinal,
    cfg$within_group$dichotomous, cfg$within_group$categorical, logger)

  res$psm_covariates_clean <- V(cfg$psm_covariates_raw, "psm_covariates")
  res$gee_adjust_clean     <- V(cfg$gee_adjust_covariates_raw, "gee_adjust_covariates")
  res$regression_covs_clean <- V(cfg$regression_covariates_raw, "regression_covariates")
  # the configured covariate names that did not resolve (not in the file, or
  # ambiguous), as typed: the workbook lists them as not found, apart from the
  # covariates that were used
  unresolved <- function(x) {
    x <- as.character(unlist(x))
    x <- unique(x[!is.na(x) & nzchar(trimws(x))])
    if (!length(x)) return(character(0))
    x[is.na(resolve_raw_to_clean(raw_names, x))]
  }
  res$psm_covariates_unresolved  <- unresolved(cfg$psm_covariates_raw)
  res$gee_adjust_unresolved      <- unresolved(cfg$gee_adjust_covariates_raw)
  res$regression_covs_unresolved <- unresolved(cfg$regression_covariates_raw)

  # Within-group unit of analysis, per variable: "patient" collapses to one row
  # per patient before describing, "eye" (the default) uses the rows as they are.
  res$within_levels_clean <- list()
  for (nm in names(cfg$within_group_levels %||% list())) {
    k <- R(nm)
    if (!is.na(k)) res$within_levels_clean[[k]] <- as.character(cfg$within_group_levels[[nm]])
  }
  if (length(res$within_levels_clean)) {
    np <- sum(vapply(res$within_levels_clean, function(u) identical(u, "patient"), logical(1)))
    if (np > 0 && is.na(res$patient_id_clean))
      logger("Within-group: patient-level analysis was requested but no patient/cluster ID column is set - falling back to per eye.")
  }

  # Optional astigmatism vector columns (magnitude + plus axis; Alpins TIA, SIA,
  # DV and the prediction error PE) and target refraction for the figures and
  # the astigmatism sheet. Kept OUT of res$roles for the same reason as
  # stability_visits: the roles loop below expects one scalar per role.
  res$astig_vector_cols <- local({
    out <- list()
    for (nm in c("tia_mag", "tia_axis", "sia_mag", "sia_axis", "dv_mag", "dv_axis",
                 "pe_mag", "pe_axis")) {
      v <- (cfg$astig_vector_cols %||% list())[[nm]]
      if (!.pl_blank(v)) out[[nm]] <- O(v, paste0("Astigmatism vector (", nm, ") column"))
    }
    out
  })
  res$astig_target <- local({
    out <- list()
    for (nm in c("cyl", "axis")) {
      v <- (cfg$astig_target %||% list())[[nm]]
      if (!.pl_blank(v)) out[[nm]] <- O(v, paste0("Astigmatism target (", nm, ") column"))
    }
    out <- out[!is.na(unlist(out))]
    if (length(out) == 1)
      logger("Astigmatism target: both a cylinder AND an axis column are needed - ignoring the partial mapping.")
    if (length(out) < 2) list() else out
  })

  res$dicho_pref_clean <- list()
  if (!is.null(cfg$dichotomous_preferred_level)) {
    for (nm in names(cfg$dichotomous_preferred_level)) {
      k <- R(nm); if (!is.na(k)) res$dicho_pref_clean[[k]] <- cfg$dichotomous_preferred_level[[nm]]
    }
  }

  # Initial Summary columns: no built-in default name (nothing is inferred
  # from column names) - an unmapped one leaves its row blank, and says so.
  res$age_clean <- O(cfg$age_var, "Initial Summary age column", "The age row is left blank.")
  res$fu_clean  <- O(cfg$fu_var,  "Initial Summary follow-up column", "The follow-up row is left blank.")
  res$sex_clean <- O(cfg$sex_var, "Initial Summary sex column", "The sex row is left blank.")
  unmapped <- c(age = .pl_blank(cfg$age_var), "follow-up" = .pl_blank(cfg$fu_var),
                sex = .pl_blank(cfg$sex_var))
  if (any(unmapped))
    logger(paste0("Initial Summary: no ", paste(names(unmapped)[unmapped], collapse = " / "),
                  " column mapped - ", if (sum(unmapped) == 1) "that row is" else "those rows are",
                  " left blank."))

  # mEYEstro / Alpins / figure role -> clean name (NA when unmapped or not
  # found). EVERY known role key is present, so a settings file that leaves a
  # key out reads as "not mapped" everywhere - never as NULL. Keep ROLE_KEYS in
  # step with ROLE_LABELS in webapp/static/app.js; an unknown key is kept.
  ROLE_KEYS <- c("pre_ucva", "pre_bcva", "post_ucva", "post_bcva",
                 "pre_sph", "pre_cyl", "pre_axis", "post_sph", "post_cyl", "post_axis",
                 "pre_seq", "post_seq", "target_seq", "eye")
  rl <- cfg$roles %||% list()
  if (!is.list(rl)) rl <- as.list(rl)
  extra <- setdiff(names(rl), ROLE_KEYS)
  role_keys <- c(ROLE_KEYS, extra[!is.na(extra) & nzchar(extra)])
  res$roles <- stats::setNames(lapply(role_keys, function(k)
    O(rl[[k]], paste0("Role '", k, "': column"),
      "The figures and export tabs that need this role are skipped.")), role_keys)

  # Stability visits: one entry per follow-up. Each field is resolved to a clean
  # column name (NA when not supplied). A visit is usable if it has a time column
  # AND either a SEQ column or a complete sphere+cylinder pair; SEQ wins when both
  # are present. Kept OUT of res$roles because that path assumes scalar values.
  # The figure keys its points and the change pair by label, so the labels of
  # the usable visits are made unique (a repeat gets " (2)", " (3)", ...; the
  # first visit keeps the label, so a change pair naming it means that visit),
  # and "Pre" stays reserved for the preoperative point when that is drawn.
  res$stability_visits <- local({
    vis <- cfg$stability_visits %||% list()
    out <- lapply(vis, function(v) {
      lab <- as.character(unlist(v$label %||% ""))[1]
      if (is.na(lab)) lab <- ""
      rr <- function(x, f) O(x, paste0("Stability visit '", lab, "' ", f, " column"))
      list(label = lab,
           time  = rr(v$time, "time"), seq = rr(v$seq, "SEQ"),
           sph   = rr(v$sph, "sphere"),  cyl = rr(v$cyl, "cylinder"))
    })
    usable <- function(v, has) has(v$time) && (has(v$seq) || (has(v$sph) && has(v$cyl)))
    ok <- vapply(out, function(v) usable(v, function(x) !is.na(x)), logical(1))
    asked <- vapply(vis, function(v) usable(v, function(x) !.pl_blank(x)), logical(1))
    name_of <- function(i) if (nzchar(out[[i]]$label)) paste0("'", out[[i]]$label, "'") else paste0("#", i)
    incomplete <- which(!ok & !asked)
    not_found  <- which(!ok & asked)
    if (length(incomplete))
      logger(paste0("Stability: visit(s) ", paste(vapply(incomplete, name_of, character(1)), collapse = ", "),
                    " skipped - each visit needs a time column plus SEQ, or sphere + cylinder."))
    if (length(not_found))
      logger(paste0("Stability: visit(s) ", paste(vapply(not_found, name_of, character(1)), collapse = ", "),
                    " skipped - a mapped column was not found in the file (see above)."))
    out <- out[ok]
    labs <- vapply(out, function(v) v$label, character(1))
    # the figure draws a "Pre" point from pre_seq, else from pre sphere + cylinder
    rk <- function(k) !is.na(res$roles[[k]] %||% NA_character_)
    pre_on <- isTRUE(cfg$stability_include_preop %||% TRUE) &&
      (rk("pre_seq") || (rk("pre_sph") && rk("pre_cyl")))
    taken <- if (pre_on) "Pre" else character(0)
    for (i in seq_along(out)) {
      lab <- labs[i]
      if (!nzchar(lab)) next
      if (lab %in% taken) {
        k <- 2L
        while (paste0(lab, " (", k, ")") %in% c(taken, labs)) k <- k + 1L
        new <- paste0(lab, " (", k, ")")
        owner <- if (pre_on && lab == "Pre") "the preoperative point" else "the first visit with that label"
        logger(paste0("Stability: visit label '", lab, "' is already used by ", owner,
                      " - this visit is shown as '", new, "', and a change pair naming '", lab,
                      "' means ", owner, ". Give every visit its own label."))
        labs[i] <- new
        out[[i]]$label <- new
      }
      taken <- c(taken, labs[i])
    }
    out
  })

  res$exclude_params_clean <- V(cfg$exclude_params_raw, "exclude_params")
  res
}

# The groups a value belongs to: a logical matrix (values x groups) from
# match_values(), so a numeric code matches however the GUI spells it
# ("1" = "1.0"). A row claimed by two groups is an error, never silently
# given to the later group.
group_hits <- function(v, groups) {
  gnames <- names(groups)
  if (!length(gnames)) stop("No groups are defined.", call. = FALSE)
  hits <- do.call(cbind, lapply(gnames, function(g) match_values(v, groups[[g]])))
  hits <- matrix(hits, nrow = length(v), ncol = length(gnames))
  multi <- rowSums(hits) > 1
  if (any(multi)) {
    vals <- unique(trimws(as.character(v[multi])))
    gs <- gnames[colSums(hits[multi, , drop = FALSE]) > 0]
    stop("Group value(s) ", paste0("'", utils::head(vals, 5), "'", collapse = ", "),
         " belong to more than one group (", paste(gs, collapse = ", "),
         ") - each value may be listed under one group only.", call. = FALSE)
  }
  hits
}

# Build the GroupLabel factor from cfg$groups (named list name -> values),
# through group_hits(). A reference group that is not defined, or has no rows
# in this run, is replaced by the first group present - and that is logged,
# because every "vs reference" contrast changes. display_names: the clean key
# -> header map (res$display_names), so the messages name the column as typed.
add_group_label <- function(dat, group_source_clean, groups, reference_group,
                            logger = message, run_label = NULL, display_names = NULL) {
  gnames <- names(groups)
  where <- if (.pl_blank(run_label)) "" else paste0(" in run '", run_label, "'")
  src_lab <- disp_name(group_source_clean, display_names)
  v <- dat[[group_source_clean]]
  hits <- group_hits(v, groups)
  label <- rep(NA_character_, nrow(dat))
  for (j in seq_along(gnames)) label[hits[, j]] <- gnames[j]
  # rows whose group value belongs to no group are left out of the analysis:
  # said once per run, with the values concerned, so it is never silent
  out_rows <- is.na(label)
  if (any(out_rows) && any(!out_rows)) {
    ov <- v[out_rows]
    vals <- unique(trimws(as.character(ov[!is.na(ov)])))
    n_blank <- sum(is.na(ov) | !nzchar(trimws(as.character(ov))))
    vals <- vals[nzchar(vals)]
    logger(paste0("Grouping", where, ": ", sum(out_rows), " of ", nrow(dat),
                  " rows belong to no group and are not analysed (",
                  paste(c(if (length(vals)) paste0("'", src_lab, "' = ",
                                                   paste(utils::head(vals, 8), collapse = ", "),
                                                   if (length(vals) > 8) ", ..." else ""),
                          if (n_blank) paste0(n_blank, " with a blank group value")),
                        collapse = "; "), ")."))
  }
  dat$GroupLabel <- label
  dat <- dat[!is.na(dat$GroupLabel), , drop = FALSE]
  if (nrow(dat) == 0) {
    req <- unique(trimws(as.character(unlist(groups))))
    have <- unique(trimws(as.character(v[!is.na(v)])))
    stop("No rows matched any group definition", where, ". Requested: ",
         paste(utils::head(req, 8), collapse = ", "), "; present in column '",
         src_lab, "': ",
         if (length(have)) paste0(paste(utils::head(have, 8), collapse = ", "),
                                  if (length(have) > 8) ", ..." else "") else "(no values)",
         ".", call. = FALSE)
  }
  present <- gnames[gnames %in% unique(dat$GroupLabel)]
  ref_ok <- !.pl_blank(reference_group) && as.character(reference_group)[1] %in% present
  ref <- if (ref_ok) as.character(reference_group)[1] else present[1]
  if (!ref_ok && length(present) >= 2) {
    why <- if (.pl_blank(reference_group)) "no reference group is set"
           else if (!(as.character(reference_group)[1] %in% gnames))
             paste0("the reference group '", reference_group, "' is not one of the defined groups (",
                    paste(gnames, collapse = ", "), ")")
           else paste0("the reference group '", reference_group, "' has no rows", where)
    logger(paste0("WARNING: ", why, " - '", ref, "' is used as the reference group",
                  if (grepl(where, why, fixed = TRUE)) "" else where, " for every comparison."))
  }
  ordered_levels <- c(ref, setdiff(present, ref))
  dat$GroupLabel <- droplevels(factor(dat$GroupLabel, levels = ordered_levels))
  if (nlevels(dat$GroupLabel) < 2)
    stop("Only one group ('", levels(dat$GroupLabel)[1], "', ", nrow(dat), " rows) has data",
         where, " - at least two groups are needed for a comparison.", call. = FALSE)
  dat
}

# Determine the list of runs based on subset config. The built-in runs
# ("All", "Pooled") are flagged so their names stay reserved. A subset with no
# values is left out (the caller logs it): otherwise a "Pooled" run over only
# empty subsets would silently analyse every row.
build_runs <- function(cfg) {
  subs <- cfg$subsets
  if (length(subs)) subs <- subs[vapply(subs, function(v) !.pl_blank(v), logical(1))]
  # subset_mode: "separate" | "both" | "pooled". A settings file older than the
  # key carries only subset_pooled (TRUE = pooled + separate runs, FALSE =
  # separate runs), read the same way as the GUI reads it; with neither key
  # the GUI's default (separate runs) applies, so a settings file gives the
  # same runs headless as when it is loaded into the GUI.
  mode <- if (!.pl_blank(cfg$subset_mode)) cfg$subset_mode
          else if (!is.null(cfg$subset_pooled)) (if (isTRUE(as.logical(unlist(cfg$subset_pooled))[1])) "both" else "separate")
          else "separate"
  mode <- tolower(trimws(as.character(unlist(mode))[1]))
  if (.pl_blank(cfg$subset_col_raw) || is.null(subs) || length(subs) == 0) {
    return(list(list(label = "All", values = NULL, builtin = TRUE)))
  }
  sel <- names(subs)
  pooled <- list(label = "Pooled", values = unique(as.character(unlist(subs))), builtin = TRUE)
  sep <- lapply(sel, function(nm)
    list(label = nm, values = as.character(unlist(subs[[nm]])), builtin = FALSE))
  if (mode == "pooled") return(list(pooled))
  if (mode == "both")   return(c(list(pooled), sep))
  sep                   # "separate" (an unknown mode reads as the GUI reads it)
}

# File-name stem per run: safe_filename(), then unique without regard to case
# (Windows file names ignore case), so two runs can never overwrite each
# other's workbook or figure folder. "Pooled" and "All" belong to the built-in
# runs: a subset called that gets a suffix. A run's matched-only workbook is
# "<stem>_MatchedOnly", so that name is taken too (a subset called
# "X_MatchedOnly" next to a subset "X" would otherwise overwrite it).
run_stems <- function(runs, max_chars = 40) {
  used <- character(0)
  out <- character(length(runs))
  taken <- function(s) any(tolower(c(s, paste0(s, "_MatchedOnly"))) %in% used)
  for (i in seq_along(runs)) {
    base <- safe_filename(runs[[i]]$label, max_chars = max_chars)
    if (!isTRUE(runs[[i]]$builtin) && tolower(base) %in% c("pooled", "all"))
      base <- paste0(base, "_subset")
    s <- base
    k <- 1L
    while (taken(s)) { k <- k + 1L; s <- paste0(base, "_", k) }
    used <- c(used, tolower(c(s, paste0(s, "_MatchedOnly"))))
    out[i] <- s
  }
  out
}

# One separator style for every path the run reports (the "Saved:" LOG lines
# and the RESULT line): backslashes on Windows. normalizePath() leaves a path
# it cannot resolve (not created yet, or too long) as typed, so the slashes
# are converted explicitly as well.
.pl_os_path <- function(p) {
  p <- normalizePath(p, winslash = "\\", mustWork = FALSE)
  if (.Platform$OS.type == "windows") gsub("/", "\\", p, fixed = TRUE) else p
}

# Windows (without long-path support) cannot create a file whose full path is
# longer than 259 characters, even inside a folder that could be created.
.pl_max_path <- 259L

# A workbook that was not written must never be reported: stop, naming the
# path, and say so when the path is over the Windows limit.
.pl_check_written <- function(path, why = "", what = "workbook") {
  if (file.exists(path) && isTRUE(file.size(path) > 0)) return(invisible(path))
  # the path is named once, in front - not again inside openxlsx's reason
  for (p in unique(c(path, gsub("\\", "/", path, fixed = TRUE))))
    why <- gsub(p, "", gsub(paste0("'", p, "'"), "", why, fixed = TRUE), fixed = TRUE)
  why <- trimws(gsub("\\s+([,.;:])", "\\1", gsub("\\s{2,}", " ", why)))
  if (!nzchar(why)) why <- "the file was not created"
  n <- nchar(path)
  stop("The ", what, " was not written: ", path, " (", why, "). ",
       if (n > .pl_max_path)
         sprintf("Its full path is %d characters long, over the Windows limit of %d characters - choose a shorter output folder (or output prefix) and run again.",
                 n, .pl_max_path)
       else "Check that the output folder is writable and the file is not open in Excel, then run again.",
       call. = FALSE)
}

# Save a workbook and confirm the file is really there: openxlsx only WARNS
# when it cannot create a file (illegal name, path too long, file open in
# Excel, unwritable folder). Stops when the file was not written.
save_workbook_checked <- function(wb, path, what = "workbook") {
  why <- ""
  ok <- tryCatch(withCallingHandlers(
      openxlsx::saveWorkbook(wb, path, overwrite = TRUE, returnValue = TRUE),
      warning = function(w) { why <<- conditionMessage(w); invokeRestart("muffleWarning") }),
    error = function(e) { why <<- conditionMessage(e); FALSE })
  if (!isTRUE(ok) && !nzchar(why)) why <- "openxlsx reported a failure"
  if (!isTRUE(ok) && file.exists(path)) {
    # never leave (or report) a half-written file
    unlink(path)
  }
  .pl_check_written(path, why, what)
}

# CSV in UTF-8 WITH a byte-order mark, as the GUI's own CSV export writes it:
# without the BOM Excel on Windows opens non-ASCII text (Hebrew, accents) as
# mojibake. (R cannot write "UTF-8-BOM" through fileEncoding.)
write_csv_utf8_bom <- function(d, path) {
  con <- file(path, open = "w", encoding = "UTF-8")
  on.exit(close(con))
  cat("﻿", file = con)
  # no scientific notation: a round numeric patient ID such as 100000 must
  # not be written as 1e+05
  old <- options(scipen = 100)
  on.exit(options(old), add = TRUE)
  utils::write.csv(d, con, row.names = FALSE)
  invisible(TRUE)
}

# Column headers of exported data (the filtered-dataset CSV, Matched_Data):
# every column read from the file carries its header as typed
# (display_names: clean key -> header; contract R4-3), then `suffix` where one
# is given (named by clean key, e.g. "_logMAR"); the columns the engine adds
# (GroupLabel, Pairing, weights, ...) keep their names. Two headers that would
# then be equal - Excel compares headers without regard to case, and an Excel
# table refuses repeats - are told apart the way the sheet reader numbers a
# repeated header (pandas_like_names): name, name.1, name.2, ...
export_headers <- function(keys, display_names = NULL, suffix = NULL) {
  out <- disp_name(keys, display_names)
  if (length(suffix)) {
    j <- match(names(suffix), keys)
    ok <- !is.na(j)
    cand <- paste0(out[j[ok]], suffix[ok])
    # a header the file already has keeps its own column: the suffix is left
    # off a converted column whose suffixed name would take that header
    free <- !(tolower(cand) %in% tolower(out))
    out[j[ok][free]] <- cand[free]
  }
  low <- tolower(out)
  used <- character(0)
  for (i in seq_along(out)) {
    if (low[i] %in% used) {
      k <- 1L
      repeat {
        cand <- paste0(out[i], ".", k)
        if (!(tolower(cand) %in% c(used, low[-i]))) break
        k <- k + 1L
      }
      out[i] <- cand
    }
    used <- c(used, tolower(out[i]))
  }
  out
}

# Headers that differ only by case or punctuation ("Pre UCVA" / "Pre-UCVA")
# can be told apart only by their exact text - said once, when the file is read.
log_header_collisions <- function(raw_names, logger = message, max_groups = 6) {
  if (!exists(".name_key", mode = "function")) return(invisible(NULL))
  keys <- .name_key(raw_names)
  dk <- unique(keys[nzchar(keys) & duplicated(keys)])
  if (!length(dk)) return(invisible(NULL))
  grp <- vapply(dk, function(k) paste0("'", raw_names[keys == k], "'", collapse = " / "), character(1))
  more <- length(grp) - max_groups
  logger(paste0("Note: column headers that differ only by case or punctuation: ",
                paste(utils::head(grp, max_groups), collapse = "; "),
                if (more > 0) sprintf("; and %d more", more) else "",
                ". Each is used only when a setting names it exactly; a name that matches them only loosely is ambiguous and is not used."))
  invisible(NULL)
}

# ------------------------------------------------------------
# Reading the sheet. The header row is read as recorded (.name_repair =
# "minimal") and named exactly as pandas names it in the GUI
# (pandas_like_names), so a duplicate or blank header resolves to the column
# the user picked. readxl's cell warnings are logged per column.
# ------------------------------------------------------------

# readxl picks .xls or .xlsx from the file extension, so an .xlsx workbook
# saved under an .xls name fails with "Unable to open file" although the GUI
# (openpyxl) loads it. The format is taken from the file's signature when
# readxl can tell it, from the extension otherwise.
.pl_read_excel <- function(path, ...) {
  fmt <- tryCatch(readxl::format_from_signature(path), error = function(e) NA_character_)
  if (identical(fmt, "xlsx")) readxl::read_xlsx(path, ...)
  else if (identical(fmt, "xls")) readxl::read_xls(path, ...)
  else readxl::read_excel(path, ...)
}

read_input_sheet <- function(cfg, logger = message) {
  sheet <- cfg$sheet_name %||% 1
  ws <- character(0)
  raw <- withCallingHandlers(
    .pl_read_excel(cfg$file_path, sheet = sheet, guess_max = cfg$guess_max %||% 120000,
                   .name_repair = "minimal"),
    warning = function(w) { ws <<- c(ws, conditionMessage(w)); invokeRestart("muffleWarning") })
  hdr <- names(raw)
  # readxl trims the spaces around header text (trim_ws); pandas keeps them, so
  # ' Age' next to 'Age' would otherwise become 'Age' / 'Age.1' here but stay
  # ' Age' / 'Age' in the GUI. The header row is re-read untrimmed and used when
  # it describes the same columns.
  hdr_raw <- tryCatch(names(.pl_read_excel(cfg$file_path, sheet = sheet, n_max = 0,
                                            trim_ws = FALSE, .name_repair = "minimal")),
                      error = function(e) NULL)
  if (length(hdr_raw) == length(hdr)) {
    h0 <- hdr; h0[is.na(h0)] <- ""
    h1 <- hdr_raw; h1[is.na(h1)] <- ""
    if (all(trimws(h1) == trimws(h0))) hdr <- h1
  }
  # pandas keeps empty columns before the data (as "Unnamed: 0", ...) where
  # readxl drops them, so a blank header is numbered from the sheet's column A
  off <- if (any(is.na(hdr) | !nzchar(trimws(hdr)))) .pl_leading_empty_cols(cfg$file_path, sheet) else 0L
  raw_names <- pandas_like_names(c(rep("", off), hdr))
  if (off > 0) raw_names <- raw_names[-seq_len(off)]
  # A date-formatted cell in a numeric column is a data-entry error: readxl
  # keeps it as the date's serial number (e.g. 46087), which passes numeric
  # filters and distorts means, so it is set to missing (and logged per column).
  date_na <- NULL
  if (any(grepl("got a date", ws, fixed = TRUE))) {
    date_na <- .pl_blank_date_cells(raw, cfg$file_path, sheet)
    raw <- date_na$data
  }
  log_read_warnings(ws, raw_names, cfg$file_path, sheet, logger, date_na = date_na)
  list(data = raw, raw_names = raw_names)
}

# The date cells of the numeric columns, found by re-reading the sheet with
# every cell in its own type (col_types = "list": a date cell is a POSIXct),
# and set to missing. Same sheet, same header row, so the rows line up; when
# they do not, nothing is changed (ok = FALSE) and the caller says so.
# Returns list(data, ok, counts = <named by column index>, serials = <one
# example serial number per column>).
.pl_blank_date_cells <- function(raw, path, sheet) {
  out <- list(data = raw, ok = FALSE, counts = integer(0), serials = numeric(0))
  num <- which(vapply(raw, function(v) is.numeric(v) && !inherits(v, c("POSIXt", "Date")), logical(1)))
  if (!length(num)) return(out)
  lst <- tryCatch(suppressWarnings(.pl_read_excel(path, sheet = sheet, col_types = "list",
                                                   .name_repair = "minimal")),
                  error = function(e) NULL)
  if (is.null(lst) || nrow(lst) != nrow(raw) || ncol(lst) != ncol(raw)) return(out)
  for (j in num) {
    is_dt <- vapply(lst[[j]], function(z) inherits(z, c("POSIXt", "Date")), logical(1))
    if (!any(is_dt)) next
    k <- as.character(j)
    out$counts[k] <- sum(is_dt)
    out$serials[k] <- raw[[j]][which(is_dt)[1]]
    raw[[j]][is_dt] <- NA
  }
  out$data <- raw
  out$ok <- TRUE
  out
}

# readxl leaves out leading empty columns, but its warnings give SHEET
# columns: the number of empty columns before the data.
.pl_leading_empty_cols <- function(path, sheet) {
  tryCatch({
    full <- suppressWarnings(.pl_read_excel(path, sheet = sheet,
      range = cellranger::cell_cols(c(1, NA)), col_names = FALSE, col_types = "text",
      .name_repair = "minimal"))
    used <- which(vapply(full, function(v) any(!is.na(v)), logical(1)))
    if (length(used)) as.integer(used[1] - 1L) else 0L
  }, error = function(e) 0L)
}

# readxl warnings -> LOG lines, one per column and kind. A cell readxl cannot
# read in its column's type becomes missing, which passes every numeric
# filter, so the column is named, with a count and an example cell. A date in
# a numeric column ("Expecting numeric in W5 / R5C23: got a date") has been
# set to missing by .pl_blank_date_cells (date_na): its line gives the count
# actually set to missing and why.
log_read_warnings <- function(ws, raw_names, path, sheet, logger = message, max_lines = 12,
                              date_na = NULL) {
  if (!length(ws)) return(invisible(NULL))
  date_line <- function(nm, n, cell, serial) {
    s <- if (n == 1) "" else "s"
    sprintf("Reading: %d date-formatted cell%s in numeric column %s %s set to missing%s - a date where a number is expected is a data-entry error, and its serial number%s would pass numeric filters and distort means. Correct the cell%s in Excel.",
            n, s, nm, if (n == 1) "was" else "were",
            if (nzchar(cell)) paste0(" (e.g. cell ", cell, ")") else "",
            if (!is.na(serial)) paste0(" (e.g. ", format(serial, scientific = FALSE), ")") else "", s)
  }
  date_done <- character(0)
  dlines <- character(0)
  ws <- gsub("[\r\n]+", " ", ws)
  re <- "^(Expecting|Coercing) (.+?) in ([A-Z]+[0-9]+) / R([0-9]+)C([0-9]+):? ?(.*)$"
  m <- regmatches(ws, regexec(re, ws, perl = TRUE))
  parsed <- lengths(m) == 7
  lines <- character(0)
  if (any(parsed)) {
    pm <- do.call(rbind, m[parsed])
    off <- .pl_leading_empty_cols(path, sheet)
    col <- as.integer(pm[, 6]) - off
    key <- paste(pm[, 2], pm[, 3], col, sep = "\r")
    for (k in unique(key)) {
      i <- which(key == k)
      j <- i[1]
      n <- length(i)
      nm <- if (!is.na(col[j]) && col[j] >= 1 && col[j] <= length(raw_names))
        paste0("'", raw_names[col[j]], "'") else paste0("at cell ", pm[j, 4])
      eg <- paste0("cell ", pm[j, 4], if (nzchar(pm[j, 7])) paste0(": ", pm[j, 7]) else "")
      s <- if (n == 1) "" else "s"
      is_date <- pm[j, 2] == "Expecting" && pm[j, 3] == "numeric" && pm[j, 7] == "got a date"
      ck <- as.character(col[j])
      if (is_date && isTRUE(date_na$ok)) {
        # said in full (never cut by max_lines): these cells changed the data
        if (ck %in% names(date_na$counts) && !(ck %in% date_done)) {
          date_done <- c(date_done, ck)
          dlines <- c(dlines, date_line(nm, date_na$counts[[ck]], pm[j, 4], date_na$serials[[ck]]))
        }
        next
      }
      lines <- c(lines,
        if (is_date)
          # the re-read did not line up: the serial number is still in the data
          sprintf("Reading: column %s - %d cell%s formatted as a date where numbers are expected (e.g. cell %s) could not be set to missing and %s read as the date's serial number, which passes numeric filters - correct the cell%s in Excel.",
                  nm, n, s, pm[j, 4], if (n == 1) "is" else "are", s)
        else if (pm[j, 2] == "Expecting")
          sprintf("Reading: column %s - %d cell%s could not be read as %s and %s treated as missing (e.g. %s). A missing value passes every numeric filter; correct the cell%s in Excel.",
                  nm, n, s, pm[j, 3], if (n == 1) "is" else "are", eg, s)
        else
          sprintf("Reading: column %s - %d cell%s converted (%s), e.g. %s.", nm, n, s, pm[j, 3], eg))
    }
  }
  # date cells set to missing in a column no parsed warning named
  for (ck in setdiff(names(date_na$counts %||% integer(0)), date_done)) {
    jj <- as.integer(ck)
    nm <- if (jj >= 1 && jj <= length(raw_names)) paste0("'", raw_names[jj], "'") else paste0("#", ck)
    dlines <- c(dlines, date_line(nm, date_na$counts[[ck]], "", date_na$serials[[ck]]))
  }
  if (any(!parsed)) lines <- c(lines, paste0("Reading: ", unique(ws[!parsed])))
  if (length(lines) > max_lines) {
    extra <- length(lines) - max_lines
    lines <- c(lines[seq_len(max_lines)],
               sprintf("Reading: %d further warning line%s not shown.", extra, if (extra == 1) "" else "s"))
  }
  for (l in c(dlines, lines)) logger(l)
  invisible(NULL)
}

# ------------------------------------------------------------
# Visual acuity. cfg$va_format says how EVERY VA column is recorded: the
# four VA roles and every column in logmar_cols_raw.
#  Step 1 (prepare_va_columns, before any exclusion or filter): parse each
#  column with parse_va() and keep the canonical DECIMAL acuity in
#  .pre_logmar__<col> - always, whatever do_logmar says - and the off-chart
#  label of each cell (CF, HM, LP or NLP; NA otherwise) in
#  .va_offchart__<col>. The figures and the mEYEstro / AstigMATIC tabs read
#  VA only through these. The parse counts are logged for the whole sheet.
#  Step 2 (check_va_notation, after exclusions, filters, grouping and subset
#  selection): the run stops when the rows that are analysed contradict the
#  declared notation.
#  Step 3 (set_va_analysis_columns): the analysis column.
# ------------------------------------------------------------
.pl_va_label <- c(decimal = "decimal acuity", logmar = "logMAR",
                  snellen = "Snellen", etdrs = "ETDRS letters")

va_format_of <- function(cfg) {
  fmt <- if (.pl_blank(cfg$va_format)) "decimal" else tolower(trimws(as.character(unlist(cfg$va_format))[1]))
  if (!(fmt %in% names(.pl_va_label)))
    stop("Unknown VA notation '", fmt, "': use decimal, logmar, snellen or etdrs.", call. = FALSE)
  fmt
}

# The VA columns: the four VA roles plus logmar_cols_raw (clean names).
# `listed` - the columns the logMAR conversion applies to - is logmar_cols_raw;
# a settings file without that key converts the mapped VA role columns
# (decision D16: logMAR by default for the mapped acuity columns). A list
# that is present, even empty, is used as it is.
va_columns <- function(res, cfg, raw_names, logger = message) {
  roles <- unlist(res$roles[intersect(c("pre_ucva", "pre_bcva", "post_ucva", "post_bcva"),
                                      names(res$roles))])
  roles <- as.character(roles[!is.na(roles)])
  listed <- if (is.null(cfg$logmar_cols_raw)) roles
            else resolve_vec(raw_names, unlist(cfg$logmar_cols_raw), "logmar_cols", logger)
  list(all = unique(c(roles, listed)), listed = listed)
}

.pl_va_examples <- function(ex) {
  if (length(ex)) paste0(" (e.g. ", paste0("'", ex, "'", collapse = ", "), ")") else ""
}

# Why the parse counts `p` contradict the declared notation, or NULL: more
# than half of the non-blank cells unreadable, or (decimal) a value <= 0, or
# (logMAR) a value outside -0.5..3.
.pl_va_contradiction <- function(p, fmt, eg = .pl_va_examples(p$examples)) {
  n_oor <- p$n_out_of_range %||% 0L
  if (fmt == "decimal" && n_oor > 0)
    sprintf("%d value%s <= 0%s, which decimal acuity cannot have (decimal 1.0 = 20/20; logMAR 0.0 = 20/20)",
            n_oor, if (n_oor == 1) "" else "s", eg)
  else if (fmt == "logmar" && n_oor > 0)
    sprintf("%d value%s outside the logMAR range -0.5 to 3%s", n_oor, if (n_oor == 1) "" else "s", eg)
  else if (p$n_nonblank > 0 && p$n_lost > p$n_nonblank / 2)
    sprintf("%d of %d non-blank values that cannot be read as %s%s", p$n_lost, p$n_nonblank,
            .pl_va_label[[fmt]], eg)
  else NULL
}

# Up to 3 cells (as recorded) holding a number the notation cannot have -
# decimal <= 0, logMAR outside -0.5..3: the cells parse_va() counts in
# n_out_of_range (its `examples` mix them with unreadable text).
.pl_va_oor_examples <- function(x, fmt) {
  if (!(fmt %in% c("decimal", "logmar"))) return(character(0))
  rec <- .pl_recorded_number(x)
  bad <- is.finite(rec) & (if (fmt == "decimal") rec <= 0 else rec < -0.5 | rec > 3)
  utils::head(unique(trimws(as.character(x[bad]))), 3)
}

# Letter scores that look like decimal acuity: among the non-blank numeric
# cells, more than half are not whole numbers, or the largest is below 5 (a
# letter score of 0-4 is almost no vision). Returns up to 2 example cells (as
# recorded, the non-integers first), or NULL when the cells look like letters.
.pl_etdrs_looks_decimal <- function(x) {
  rec <- .pl_recorded_number(x)
  ok <- is.finite(rec)
  if (!any(ok)) return(NULL)
  frac <- ok & abs(rec - round(rec)) > 1e-9
  if (!(sum(frac) > sum(ok) / 2 || max(rec[ok]) < 5)) return(NULL)
  shown <- trimws(as.character(x))
  utils::head(unique(c(shown[frac], shown[ok])), 2)
}

# Parses every row (the stashes must line up with the sheet) and logs the
# counts of the whole sheet. Never stops: whether the notation is contradicted
# is judged later, on the rows that are analysed (check_va_notation).
# Returns list(data, stats): stats[[col]] holds that column's parse_va()
# counts over the whole sheet.
prepare_va_columns <- function(dat, cols, fmt, label_of, logger = message) {
  stats <- list()
  for (cc in cols) {
    if (!(cc %in% names(dat))) next
    p <- parse_va(dat[[cc]], fmt)
    dat[[paste0(".pre_logmar__", cc)]] <- p$decimal
    off <- p$offchart
    dat[[paste0(".va_offchart__", cc)]] <- if (length(off) == nrow(dat)) as.character(off)
                                           else rep(NA_character_, nrow(dat))
    logger(sprintf("VA column '%s' read as %s: %d value%s, %d could not be parsed%s, %d off-chart entr%s set to missing.",
                   label_of(cc), fmt, p$n_nonblank, if (p$n_nonblank == 1) "" else "s",
                   p$n_lost, .pl_va_examples(p$examples), p$n_offchart,
                   if (p$n_offchart == 1) "y" else "ies"))
    p$decimal <- NULL
    p$offchart <- NULL
    p$oor_examples <- .pl_va_oor_examples(dat[[cc]], fmt)
    stats[[cc]] <- p
  }
  list(data = dat, stats = stats)
}

# The declared notation, judged on the rows that are analysed (`dat` = the
# rows left after the exclusions, the filter rules, the grouping and the
# subset selection): a cell
# the notation cannot have stops the run only when such a row holds it. A
# contradiction found only in rows that are left out is logged; those cells
# stay missing. sheet_stats: prepare_va_columns()$stats. postop: the
# postoperative VA role columns (checked for decimal acuity declared as logMAR).
check_va_notation <- function(dat, cols, fmt, label_of, sheet_stats = list(),
                              postop = character(0), logger = message) {
  fl <- .pl_va_label[[fmt]]
  for (cc in cols) {
    if (!(cc %in% names(dat))) next
    lab <- label_of(cc)
    p <- parse_va(dat[[cc]], fmt)
    why <- .pl_va_contradiction(p, fmt)
    if (!is.null(why))
      stop(sprintf("VA column '%s' is declared as %s, but it holds %s. Set 'VA notation in your file' to the notation the file really uses (decimal, logMAR, Snellen or ETDRS letters), or correct the column.",
                   lab, fl, why), call. = FALSE)
    # decimal acuity (0.05-2.0) is a valid letter score too, and would read as
    # about 20/1000 for every eye: refused (the server's va-detect warns alike)
    if (fmt == "etdrs") {
      ex <- .pl_etdrs_looks_decimal(dat[[cc]])
      if (!is.null(ex))
        stop(sprintf("VA column '%s' is declared as ETDRS letters, but its values (e.g. %s) look like decimal acuity. Choose the notation the file uses (Variables tab).",
                     lab, paste(ex, collapse = ", ")), call. = FALSE)
    }
    whole <- sheet_stats[[cc]]
    if (!is.null(whole) && !is.null(.pl_va_contradiction(whole, fmt))) {
      # the count the sentence names: for decimal / logMAR the numbers that
      # notation cannot have (what judged the sheet contradicted), else the
      # cells that could not be read
      oor <- fmt %in% c("decimal", "logmar") && (whole$n_out_of_range %||% 0L) > 0
      k <- if (oor) max(0L, (whole$n_out_of_range %||% 0L) - (p$n_out_of_range %||% 0L))
           else max(0L, whole$n_lost - p$n_lost)
      ex <- if (oor) whole$oor_examples %||% character(0) else setdiff(whole$examples, p$examples)
      if (k > 0)
        logger(sprintf("VA column '%s': %d value%s that %s cannot have%s %s only in rows that are not analysed (excluded, filtered out, or in no group or subset) - %s missing and %s not stop the run.",
                       lab, k, if (k == 1) "" else "s", fl, .pl_va_examples(ex),
                       if (k == 1) "is" else "are", if (k == 1) "it stays" else "they stay",
                       if (k == 1) "does" else "do"))
    }
    if (fmt == "decimal" && any(p$decimal > 4, na.rm = TRUE))
      logger(sprintf("WARNING: VA column '%s' has decimal values above 4.0 (max %s), which is not a plausible decimal acuity - check the VA notation (ETDRS letters?).",
                     lab, format(max(p$decimal, na.rm = TRUE))))
    # decimal acuity declared as logMAR reads as poor vision but is never
    # refused (0.02-1.3 fit both notations): a postoperative median of logMAR
    # 0.7 (20/100) or worse with no value below 0 is flagged
    if (fmt == "logmar" && cc %in% postop) {
      rec <- .pl_recorded_number(dat[[cc]])[!is.na(p$decimal)]
      if (length(rec) >= 10 && min(rec) >= 0 && stats::median(rec) >= 0.7)
        logger(sprintf("WARNING: VA column '%s' is declared as logMAR, but its values look like decimal acuity (median %s, none below 0; a postoperative median of logMAR 0.7 = 20/100 or worse is unusual) - check 'VA notation in your file'.",
                       lab, format(round(stats::median(rec), 2))))
    }
  }
  invisible(NULL)
}

# Numbers as recorded ("0,3" counts as 0.3); NA otherwise.
.pl_recorded_number <- function(x) {
  if (is.numeric(x)) return(as.numeric(x))
  s <- gsub("−", "-", trimws(as.character(x)), fixed = TRUE)
  as_numeric_safe(sub("^([-+]?[0-9]*),([0-9]+)$", "\\1.\\2", s))
}

# Analysis columns, after filtering:
#  - va_format "logmar": the recorded logMAR numbers (never converted twice;
#    do_logmar is a no-op);
#  - do_logmar on and the column listed: -log10(canonical decimal);
#  - otherwise: the canonical decimal (for Snellen / ETDRS the recorded text
#    or letters are replaced by decimal acuity).
# Returns list(data, converted, as_logmar, as_decimal).
set_va_analysis_columns <- function(dat, va, fmt, do_logmar, label_of, logger = message) {
  conv_req <- if (isTRUE(do_logmar)) va$listed else character(0)
  converted <- character(0); as_logmar <- character(0); as_decimal <- character(0)
  for (cc in va$all) {
    st <- paste0(".pre_logmar__", cc)
    if (!(cc %in% names(dat)) || !(st %in% names(dat))) next
    dec <- dat[[st]]
    if (fmt == "logmar") {
      rec <- .pl_recorded_number(dat[[cc]])
      rec[is.na(dec)] <- NA_real_
      dat[[cc]] <- rec
      as_logmar <- c(as_logmar, cc)
    } else if (cc %in% conv_req) {
      dat[[cc]] <- -log10(dec)
      converted <- c(converted, cc)
    } else {
      dat[[cc]] <- dec
      as_decimal <- c(as_decimal, cc)
    }
  }
  labs <- function(x) paste(vapply(x, label_of, character(1)), collapse = ", ")
  if (length(as_logmar))
    logger(paste0(if (isTRUE(do_logmar)) "logMAR conversion not applied: the VA notation is logMAR, so " else "",
                  "VA columns analysed in logMAR as recorded: ", labs(as_logmar), "."))
  if (length(converted))
    logger(paste0("Converted to logMAR (-log10 of decimal acuity): ", labs(converted), "."))
  if (fmt %in% c("snellen", "etdrs") && length(as_decimal))
    logger(paste0("VA columns recorded as ", .pl_va_label[[fmt]],
                  " are analysed as decimal acuity (not converted to logMAR): ", labs(as_decimal), "."))
  list(data = dat, converted = converted, as_logmar = as_logmar, as_decimal = as_decimal)
}

# A preferred (event) level of a dichotomous variable that does not match the
# filtered data is logged once here, with the level actually used. Only the
# variables analysed as dichotomous (dicho_vars) are checked: the GUI keeps a
# preference after its variable leaves every section. display_names: clean key
# -> header (res$display_names), so the line names the column as typed - and
# matches the tables' own line, which log_once() then does not repeat.
check_dicho_prefs <- function(dat, prefs, logger = message, dicho_vars = names(prefs),
                              display_names = NULL) {
  for (k in intersect(names(prefs), dicho_vars)) {
    if (!(k %in% names(dat))) next
    pref <- prefs[[k]]
    if (.pl_blank(pref)) next
    vv <- unique(dat[[k]][!is.na(dat[[k]])])
    if (!length(vv)) next
    lev <- resolve_positive_level(pref, vv)
    if (length(lev) == 1 && !is.na(lev)) next
    lab <- disp_name(k, display_names)
    if (length(vv) == 1)
      logger(sprintf("'%s': preferred level '%s' does not occur in the filtered data (only '%s') - its rows read 0 events.",
                     lab, as.character(unlist(pref))[1], as.character(vv)))
    else
      log_once(logger, .pref_fallback_msg(lab, pref, vv, default_positive_level(vv)))
  }
  invisible(NULL)
}

# ------------------------------------------------------------
# Main entry point.
# progress: optional function(fraction, message)
# Returns list(files = character(), matched_files = character(),
#              output_dir = character(1), graph_dirs = character()).
# Stops (with every run's reason) when no workbook at all was produced, and
# at once when a workbook could not be written (save_workbook_checked).
# Every reported path uses backslashes on Windows (.pl_os_path).
# ------------------------------------------------------------
run_analysis_pipeline <- function(cfg, logger = message, progress = NULL) {
  bump <- function(frac, msg){ logger(msg); if (!is.null(progress)) progress(frac, msg) }
  run_started <- Sys.time()          # the Manifest's run duration (D25)
  # visual acuity in logMAR unless the settings say otherwise (D16): a
  # settings file without the key gets the v1.1.0 default
  if (is.null(cfg$do_logmar)) cfg$do_logmar <- TRUE
  # one P value style for the tables, sheets, logs and figures of this run (D15)
  set_p_style(cfg$p_style, logger)
  # the astigmatism statistics shared by the sheet and the figures are cached
  # for one run only (graphs.R astig_vector_stats)
  if (exists("astig_cache_reset", mode = "function")) astig_cache_reset()
  # an unknown astigmatism setting is named once, and its default used
  for (k in c("astig_test_method", "astig_region", "astig_centroid_region")) {
    v <- tolower(trimws(as.character(unlist(cfg[[k]]))[1]))
    okv <- switch(k, astig_test_method = c("robust", "gee", "classic"),
                  astig_region = c("polygon", "depth", "normal", "both"),
                  astig_centroid_region = c("bootstrap", "hotelling"))
    if (length(v) && !is.na(v) && nzchar(v) && !(v %in% okv))
      logger(sprintf("Astigmatism setting %s = '%s' is not known (use %s) - %s is used.", k,
                     as.character(unlist(cfg[[k]]))[1], paste(okv, collapse = ", "), okv[1]))
  }

  bump(0.02, "Reading Excel file ...")
  rd <- read_input_sheet(cfg, logger)
  raw <- rd$data
  raw_names <- rd$raw_names
  names(raw) <- clean_names(raw_names)
  # Clean key -> the header as typed in the file (contract R4-3). The keys stay
  # internal (formulas and lookups depend on them); everything the user reads
  # names the column by its header: the log lines here, the exported data, and
  # - through res$display_names and disp_name() - the tables, models, PSM
  # sheets, love plots and figures.
  disp_map <- stats::setNames(as.character(raw_names), names(raw))
  label_of <- function(cc) disp_name(cc, disp_map)

  bump(0.08, "Resolving column names ...")
  log_header_collisions(raw_names, logger)
  res <- resolve_config(cfg, raw_names, logger)
  res$display_names <- disp_map
  res$run_started <- run_started
  logger(paste0("P values: ", p_style_text(), "."))

  # GEE small-sample inference of every model, table test and figure P value
  # of this run (decision D11; helpers.R set_gee_small_sample - a settings
  # file without the key gets the default "md"), and the table tests (D12)
  set_gee_small_sample(cfg$gee_small_sample, logger)
  logger(paste0("GEE inference: ",
                gee_inference_text(unit = if (length(res$patient_id_clean) && !is.na(res$patient_id_clean[1]))
                                            "patient" else "eye"), "."))
  tt_req <- tolower(trimws(as.character(unlist(cfg$table_tests %||% "auto"))[1]))
  if (!(tt_req %in% c("auto", "classical")))
    logger(sprintf("Table tests setting '%s' is not known (use auto or classical) - auto is used.",
                   as.character(unlist(cfg$table_tests))[1]))
  logger(paste0("Table tests: ", table_tests_text(cfg, has_pid = !is.na(res$patient_id_clean)), "."))

  # matching on, but none of its covariates is in the file: said once, here
  # (the Manifest records the skip); export.R builds no matched cohort then
  if (isTRUE(cfg$do_matching) && !length(res$psm_covariates_clean)) {
    asked <- res$psm_covariates_unresolved
    logger(paste0("WARNING: Matching skipped - ",
                  if (length(asked)) paste0("no matching covariate found in the file (",
                                            paste0("'", asked, "'", collapse = ", "), ")")
                  else "no matching covariate is set",
                  ". No matched cohort is built: no matched sheets, love plots, matched figures or matched-only workbook."))
  }

  # Subset column: when subsets are defined it MUST resolve - otherwise every
  # subset run would analyse the whole file under the subset's name.
  res_subset_col <- NA_character_
  if (!.pl_blank(cfg$subset_col_raw)) {
    sraw <- as.character(unlist(cfg$subset_col_raw))[1]
    res_subset_col <- resolve_raw_to_clean(raw_names, sraw)
    subs <- cfg$subsets %||% list()
    empty <- names(subs)[!vapply(subs, function(v) !.pl_blank(v), logical(1))]
    if (length(empty))
      logger(paste0("Subset(s) with no values ignored: ", paste0("'", empty, "'", collapse = ", "), "."))
    if (is.na(res_subset_col)) {
      if (length(subs) > length(empty))
        stop(unresolved_name_message(raw_names, sraw, "Subset column"),
             " No subset can be selected without it - re-select the subset column, or clear it to analyse all rows.",
             call. = FALSE)
      logger(paste0(unresolved_name_message(raw_names, sraw, "Subset column"),
                    " No subsets are defined, so all rows are analysed."))
    }
  }

  dat <- raw
  # each row's position in the sheet (1 = first data row, as the GUI preview
  # numbers it), kept through the exclusions and filters for the patient-ID
  # tokens below, then dropped
  ROWPOS <- ".se_sheet_row__"
  dat[[ROWPOS]] <- seq_len(nrow(dat))

  # (A) Standardize minus-cyl — applied to EVERY configured sph/cyl/axis
  # triple (pre-op, intra-op, post-op ...). Legacy configs with a single
  # std_sph_raw/std_cyl_raw/std_axis_raw still work.
  if (isTRUE(cfg$do_standardize_minus_cyl)) {
    bump(0.12, "Standardizing refraction (plus -> minus cyl) ...")
    triples <- cfg$std_triples
    if (is.null(triples) || length(triples) == 0)
      triples <- list(list(sph = cfg$std_sph_raw, cyl = cfg$std_cyl_raw, axis = cfg$std_axis_raw))
    n_sets <- 0L
    for (tr in triples) {
      nm <- vapply(c(sphere = "sph", cylinder = "cyl", axis = "axis"), function(f) {
        x <- as.character(unlist(tr[[f]]))[1]
        if (length(x) == 0 || is.na(x)) "" else x
      }, character(1))
      if (!any(nzchar(trimws(nm)))) next          # an empty row of the GUI table
      n_sets <- n_sets + 1L
      cl <- vapply(nm, function(x) as.character(resolve_raw_to_clean(raw_names, x))[1], character(1))
      for (f in names(nm)[is.na(cl)])
        logger(if (nzchar(trimws(nm[[f]])))
                 unresolved_name_message(raw_names, nm[[f]], paste0("Standardization ", f, " column"))
               else paste0("Standardization: no ", f, " column set for the triple '",
                           paste(nm[nzchar(trimws(nm))], collapse = " / "), "'."))
      dat <- standardize_minus_cyl(dat, cl[["sphere"]], cl[["cylinder"]], cl[["axis"]], logger,
                                   display_names = disp_map)
    }
    # the switch alone converts nothing: said, so plus-cylinder data are never
    # analysed unconverted without a word (a warning when cylinder roles are
    # mapped, i.e. there is refraction that could need converting)
    if (n_sets == 0L) {
      cyl_roles <- unlist(res$roles[intersect(c("pre_cyl", "post_cyl"), names(res$roles))])
      cyl_roles <- cyl_roles[!is.na(cyl_roles)]
      logger(paste0(if (length(cyl_roles)) "WARNING: " else "",
                    "'Convert plus-cylinder to minus-cylinder' is on, but no sphere / cylinder / axis set is defined - no refraction was converted",
                    if (length(cyl_roles)) paste0(" (cylinder column(s) mapped: ",
                                                  paste0("'", vapply(cyl_roles, label_of, character(1)), "'", collapse = ", "),
                                                  "). Add the sets to convert, or switch the conversion off.")
                    else "."))
    }
  }

  # (A2a) Visual acuity: canonical decimal and off-chart stashes for every VA
  # column (all rows), read in the declared notation. Exclusions and filters
  # below see the values AS RECORDED, so a filter typed in the file's own
  # units means what it says.
  va_fmt <- va_format_of(cfg)
  va <- va_columns(res, cfg, raw_names, logger)
  va_stats <- list()
  if (length(va$all)) {
    bump(0.14, paste0("Reading visual acuity as ", .pl_va_label[[va_fmt]], " ..."))
    pv <- prepare_va_columns(dat, va$all, va_fmt, label_of, logger)
    dat <- pv$data
    va_stats <- pv$stats
  }

  # (B) Exclude rows missing required columns
  if (length(res$exclude_params_clean) > 0) {
    n0 <- nrow(dat)
    dat <- dat %>% dplyr::filter(complete.cases(dplyr::across(dplyr::all_of(res$exclude_params_clean))))
    logger(paste0("Exclusion (missing values in ", paste(vapply(res$exclude_params_clean, label_of, character(1)), collapse = ", "),
                  "): removed ", n0 - nrow(dat), "; remaining rows: ", nrow(dat)))
    if (nrow(dat) == 0) stop("All rows removed after exclude filtering.")
  }

  # (C) Filter rules
  bump(0.18, "Applying filter rules ...")
  dat <- apply_filter_rules(dat, raw_names, cfg$filter_rules, logger)
  if (nrow(dat) == 0) stop("All rows removed after filter rules.")

  # (D) Group membership of the remaining rows (each run labels its own rows
  # in add_group_label): a value listed under two groups is refused here,
  # before any run, instead of only in the runs whose rows hold it.
  # `analysed`: the rows some run analyses - in a group and, when subsets are
  # defined, in one of them (the rows the GUI's pre-run checks look at).
  analysed <- if (length(names(cfg$groups)))
    rowSums(group_hits(dat[[res$group_source_clean]], cfg$groups)) > 0
    else rep(TRUE, nrow(dat))
  if (!is.na(res_subset_col) && res_subset_col %in% names(dat)) {
    subs <- cfg$subsets %||% list()
    sub_vals <- unlist(subs[vapply(subs, function(v) !.pl_blank(v), logical(1))])
    if (length(sub_vals)) analysed <- analysed & match_values(dat[[res_subset_col]], sub_vals)
  }

  # (E) Patient ID: a blank id becomes its own token "(no ID) row <n>" (n = the
  # row's position in the sheet), once and for every consumer, so each such
  # eye is its own patient (its own cluster) in the tables, GEE, figure P
  # values and the bootstrap - never one shared NA "patient" and never
  # silently dropped. Only now, so that an exclusion or an isna / notna rule
  # on the patient-ID column still sees the blanks. (Same tokens as
  # fill_missing_ids(), numbered by sheet row rather than by remaining row.)
  pid <- res$patient_id_clean
  if (!is.na(pid) && pid %in% names(dat)) {
    # numeric ids as they were read (id_text: 100000, never "1e+05"; no ".0")
    v <- trimws(id_text(dat[[pid]]))
    miss <- is.na(v) | !nzchar(v)
    if (any(miss)) {
      v[miss] <- paste0("(no ID) row ", dat[[ROWPOS]][miss])
      dat[[pid]] <- v
      if (any(miss & analysed))
        logger(sprintf("Patient ID: %d of the %d rows analysed have no patient ID in '%s'. Each such eye is counted as its own patient (its own cluster) in the tables, models, figure P values and bootstrap - if two of them belong to one patient they are counted twice; fill in the IDs to avoid this.",
                       sum(miss & analysed), sum(analysed), label_of(pid)))
    }
  }
  dat[[ROWPOS]] <- NULL

  # (F) The declared VA notation, judged on the rows that are analysed only
  # (the parse counts above are for the whole sheet); the server's pre-run
  # check (preview.va_format_problems) looks at the same rows
  if (length(va$all)) {
    postop <- unlist(res$roles[intersect(c("post_ucva", "post_bcva"), names(res$roles))])
    check_va_notation(dat[analysed, , drop = FALSE], va$all, va_fmt, label_of, va_stats,
                      postop = as.character(postop[!is.na(postop)]), logger = logger)
  }

  # (A2b) Visual acuity: analysis columns (logMAR conversion), after filtering
  res$va_format <- va_fmt
  res$logmar_cols_clean <- character(0)
  if (length(va$all)) {
    sv <- set_va_analysis_columns(dat, va, va_fmt, cfg$do_logmar, label_of, logger)
    dat <- sv$data
    res$logmar_cols_clean <- c(sv$converted, sv$as_logmar)   # analysed on the logMAR scale
    res$va_decimal_from_text <- if (va_fmt %in% c("snellen", "etdrs")) sv$as_decimal else character(0)
  }

  dicho_vars <- unlist(lapply(list(res$baseline_type_map, res$preop_type_map,
                                   res$outcome_type_map, res$within_type_map),
                              function(m) names(m)[m %in% "dichotomous"]))
  check_dicho_prefs(dat[analysed, , drop = FALSE], res$dicho_pref_clean, logger,
                    dicho_vars = unique(as.character(dicho_vars)),
                    display_names = res$display_names)

  output_dir <- cfg$output_dir %||% dirname(cfg$file_path)
  if (.pl_blank(output_dir)) output_dir <- dirname(cfg$file_path)
  if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
  if (!dir.exists(output_dir))
    stop("The output folder could not be created: ", output_dir, " - choose a folder you can write to.",
         call. = FALSE)
  # absolute from here on: the reported file paths must not depend on the
  # working directory (sanitize_xlsx, the GUI and a CLI caller all reuse them)
  output_dir <- .pl_os_path(output_dir)
  stamp <- format(Sys.time(), "%Y%m%d_%H%M%S")
  prefix <- safe_filename(cfg$output_prefix %||% "StandardEyes")

  # the VA stashes (.pre_logmar__<col>, .va_offchart__<col>) are internal:
  # never part of exported data
  drop_stash_cols <- function(d)
    d[, !grepl("^\\.(pre_logmar|va_offchart)__", names(d)), drop = FALSE]

  # the scale each exported VA column holds, added to its header in the
  # filtered-dataset CSV and in Matched_Data alike (contract R4-3)
  sfx <- c(stats::setNames(rep("_logMAR", length(res$logmar_cols_clean)), res$logmar_cols_clean),
           stats::setNames(rep("_decimal", length(res$va_decimal_from_text)), res$va_decimal_from_text))

  if (isTRUE(cfg$export_filtered_csv)) {
    fp <- .pl_os_path(file.path(output_dir, paste0("Filtered_Dataset_", stamp, ".csv")))
    # the rows that are analysed (D26): in a group and, when subsets are
    # defined, in one of them - with the group each row is in (GroupLabel) and
    # its subset (Subset) leading. The headers are as typed in the file, with
    # the scale each VA column holds.
    out <- drop_stash_cols(dat[analysed, , drop = FALSE])
    names(out) <- export_headers(names(out), res$display_names, sfx)
    lead <- list()
    if (length(names(cfg$groups))) {
      gh <- group_hits(dat[[res$group_source_clean]][analysed], cfg$groups)
      lead$GroupLabel <- if (nrow(out)) names(cfg$groups)[max.col(gh, ties.method = "first")] else character(0)
    }
    if (!is.na(res_subset_col) && res_subset_col %in% names(dat) && length(names(cfg$subsets))) {
      sv <- dat[[res_subset_col]][analysed]
      subs <- cfg$subsets[vapply(cfg$subsets, function(v) !.pl_blank(v), logical(1))]
      sh <- vapply(names(subs), function(s) match_values(sv, subs[[s]]), logical(length(sv)))
      sh <- matrix(sh, nrow = length(sv))
      lead$Subset <- if (nrow(out)) apply(sh, 1, function(r) paste(names(subs)[r], collapse = "; ")) else character(0)
    }
    if (length(lead)) {
      lead <- as.data.frame(lead, check.names = FALSE, stringsAsFactors = FALSE)
      # unique headers: a file column of the same name is listed as "GroupLabel.1"
      hdr <- export_headers(c(names(lead), names(out)))
      out <- data.frame(lead, as.data.frame(out, check.names = FALSE), check.names = FALSE,
                        stringsAsFactors = FALSE)
      names(out) <- hdr
    }
    err <- ""; warn <- ""
    tryCatch(withCallingHandlers(write_csv_utf8_bom(out, fp),
                                 warning = function(w) { warn <<- conditionMessage(w); invokeRestart("muffleWarning") }),
             error = function(e) err <<- conditionMessage(e))
    if (nzchar(err) || !file.exists(fp)) {
      why <- unique(c(warn, err)[nzchar(c(warn, err))])
      logger(paste0("WARNING: filtered dataset NOT exported (",
                    if (length(why)) paste(why, collapse = "; ") else "the file was not created",
                    ") - check that the output folder is writable and the file is not open in Excel: ", fp))
    } else logger(paste0("Filtered dataset exported (", nrow(out), " analysed rows): ", fp,
                         if (nzchar(warn)) paste0(" (warning: ", warn, ")") else ""))
  }

  runs <- build_runs(cfg)
  stems <- run_stems(runs)
  files <- character(0); matched_files <- character(0); graph_dirs <- character(0)
  skipped <- character(0)
  n_runs <- length(runs)

  for (i in seq_along(runs)) {
    run <- runs[[i]]
    stem <- stems[i]
    run$stem <- stem        # unique, file-safe: for any file named after the run
    # the run's workbook is <prefix>_<stem>_<stamp>.xlsx: export.R names the
    # love plots with the same three parts (contract R4-4)
    run$prefix <- prefix
    run$stamp <- stamp
    # the few-patients cautions of this run are logged once, at its end
    few_patients_reset()
    base_frac <- 0.20 + 0.75 * (i - 1) / n_runs
    bump(base_frac, paste0("Run ", i, "/", n_runs, ": ", run$label))

    dat_run <- dat
    if (!is.null(run$values)) {
      if (is.na(res_subset_col) || !(res_subset_col %in% names(dat_run)))
        stop("Subset column '", as.character(unlist(cfg$subset_col_raw))[1],
             "' is not available for run '", run$label, "'.", call. = FALSE)
      keep <- match_values(dat_run[[res_subset_col]], run$values)
      dat_run <- dat_run[keep, , drop = FALSE]
      logger(sprintf("Run '%s': %d of %d rows (%s in {%s}).", run$label, nrow(dat_run), nrow(dat),
                     label_of(res_subset_col), paste(run$values, collapse = ", ")))
    }
    if (nrow(dat_run) == 0) {
      logger(paste0("Run '", run$label, "' skipped: no rows."))
      skipped <- c(skipped, paste0("run '", run$label, "': no rows"))
      next
    }

    err <- ""
    dat_run <- tryCatch(
      add_group_label(dat_run, res$group_source_clean, cfg$groups, cfg$reference_group, logger,
                      run_label = run$label, display_names = res$display_names),
      error = function(e) { err <<- conditionMessage(e); NULL })
    if (is.null(dat_run)) {
      logger(paste0("Run '", run$label, "' skipped - grouping failed: ", err))
      skipped <- c(skipped, if (grepl(paste0("run '", run$label, "'"), err, fixed = TRUE)) err
                            else paste0("run '", run$label, "': ", err))
      next
    }

    # the run (label + subset values) is recorded in the workbook's Manifest
    built <- build_workbook_for_data(dat_run, cfg, res, logger, loveplot_dir = output_dir, run = run)

    out_xlsx <- .pl_os_path(file.path(output_dir, paste0(prefix, "_", stem, "_", stamp, ".xlsx")))
    save_workbook_checked(built$wb, out_xlsx, paste0("workbook of run '", run$label, "'"))
    sanitize_xlsx(out_xlsx, logger)
    .pl_check_written(out_xlsx, "it was lost while being sanitized", paste0("workbook of run '", run$label, "'"))
    files <- c(files, out_xlsx)
    logger(paste0("Saved: ", out_xlsx))

    # Standard graphs (Reinstein/JRS + Alpins double-angle): unmatched cohort
    # per group, then each matched pairing's cohort (with its weights column).
    # The unmatched call (tag "") is the first of every run and the only one
    # with an empty tag: graphs.R logs its once-per-run reports (SEQ target,
    # stability visits) on that call only.
    if (isTRUE(cfg$make_graphs %||% TRUE)) {
      gdir <- .pl_os_path(file.path(output_dir, paste0("Graphs_", prefix, "_", stem, "_", stamp)))
      if (!dir.exists(gdir)) dir.create(gdir, recursive = TRUE, showWarnings = FALSE)
      if (!dir.exists(gdir)) {
        logger(paste0("Graphs skipped: the folder could not be created: ", gdir))
      } else {
        gf <- tryCatch(generate_standard_graphs(dat_run, cfg, res, gdir, tag = "", logger = logger),
                       error = function(e) { logger(paste0("Graphs failed: ", conditionMessage(e))); character(0) })
        if (!is.null(built$matched_only)) {
          for (plab in unique(built$matched_only$Pairing)) {
            mdp <- built$matched_only[built$matched_only$Pairing == plab, , drop = FALSE]
            tryCatch(generate_standard_graphs(mdp, cfg, res, gdir, tag = paste0("_Matched_", plab), logger = logger),
                     error = function(e) logger(paste0("Matched graphs failed: ", conditionMessage(e))))
          }
        }
        if (length(list.files(gdir, all.files = TRUE, no.. = TRUE)) > 0) graph_dirs <- c(graph_dirs, gdir)
        else unlink(gdir, recursive = TRUE)
      }
    }

    # Matched-only workbook: the matched rows of every pairing, with the same
    # Manifest / Contents as the main workbook (run, input checksum, matching
    # settings and weights), so it can be tied to the run that produced it.
    if (!is.null(built$matched_only)) {
      mo <- drop_stash_cols(built$matched_only)
      # MatchIt's treatment indicator is internal (GroupLabel and Pairing say
      # which arm each row is in); distance, weights and subclass are kept
      mo$.treat <- NULL
      if ("subclass" %in% names(mo))      # MatchIt's pair id: a number, not text
        mo$subclass <- suppressWarnings(as.integer(as.character(mo$subclass)))
      # the headers as typed in the file (unique, as an Excel table needs),
      # with the same scale suffix on the VA columns as the filtered CSV
      names(mo) <- export_headers(names(mo), res$display_names, sfx)
      wbm <- openxlsx::createWorkbook(creator = WORKBOOK_CREATOR)   # never the user name (D25)
      md_sheet <- add_or_replace_worksheet(wbm, "Matched_Data")
      openxlsx::writeDataTable(wbm, md_sheet, mo)
      # built$matched_only stacks the pairings (a reference eye matched twice
      # is in it twice): the Manifest counts it per pairing and as distinct eyes
      add_contents_and_manifest(wbm, built$matched_only, cfg, res, logger, run = run,
                                match_failed = built$match_failed,
                                match_methods = built$match_methods)
      mfile <- .pl_os_path(file.path(output_dir, paste0(prefix, "_", stem, "_MatchedOnly_", stamp, ".xlsx")))
      mwhat <- paste0("matched-only workbook of run '", run$label, "'")
      save_workbook_checked(wbm, mfile, mwhat)
      sanitize_xlsx(mfile, logger)
      .pl_check_written(mfile, "it was lost while being sanitized", mwhat)
      matched_files <- c(matched_files, mfile)
      logger(paste0("Saved matched-only: ", mfile))
    }
    few_patients_flush(logger, if (n_runs > 1) paste0(" (run '", run$label, "')") else "")
  }

  skipped <- sub("[. ]+$", "", skipped)
  if (length(files) == 0)
    stop("No workbook was produced - ", if (length(skipped)) paste(skipped, collapse = "; ")
         else "no run had data", ".", call. = FALSE)
  if (length(skipped))
    logger(paste0("WARNING: ", length(skipped), " of ", n_runs, " run(s) produced no workbook - ",
                  paste(skipped, collapse = "; "), "."))

  # The run duration (D25) covers the whole run - tables, workbooks AND the
  # figures drawn after them: the Manifest row written with each workbook (the
  # time up to that workbook) is replaced by the total now; where that cannot
  # be done the row keeps its own, labelled, time and the log says so.
  dur <- as.numeric(difftime(Sys.time(), run_started, units = "secs"))
  logger(sprintf("Run duration: %.1f s (tables, workbooks and figures).", dur))
  for (f in c(files, matched_files))
    xlsx_replace_text(f, MANIFEST_DURATION_RE,
                      sprintf("%.1f (seconds, the whole run: tables, workbooks and figures)", dur), logger)

  bump(1.0, "Done.")
  list(files = files, matched_files = matched_files, output_dir = output_dir,
       graph_dirs = graph_dirs)
}
