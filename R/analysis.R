# ============================================================
# analysis.R
# Generalized statistical engine (N groups).
#   - Type-aware summary tables with one column per group
#     and an omnibus test across groups (group_test() in
#     helpers.R; patient-clustered GEE tests when a patient
#     has two analysed eyes; weighted GEE Wald tests on
#     matched cohorts with non-unit matching weights).
#   - Initial summary.
#   - Clustered GEE (Gaussian / binomial) with a factor
#     group predictor (handles >2 groups via contrasts).
#     Without a patient ID every row is its own cluster.
#   - Reference-group pairwise PSM orchestration.
# When exactly 2 groups are supplied the numbers reproduce
# the original two-group script.
# ============================================================

# TRUE when `nm` names one column of df (NA / NULL / "" are "no column").
.has_col <- function(df, nm) {
  length(nm) == 1 && !is.na(nm) && nzchar(nm) && nm %in% names(df)
}

# One row per (patient, group) for a variable analysed per patient: the first
# non-missing value is kept. Rows without a patient ID are their own patient.
# A patient whose eyes disagree (data-entry error) is logged once. With
# matching weights, the patient carries the mean weight of its eyes (.w).
collapse_patient_level <- function(df, pn, group_var, patient_id_clean,
                                   weights = NULL, logger = NULL, display_names = NULL) {
  d <- data.frame(.id = fill_missing_ids(df[[patient_id_clean]]), stringsAsFactors = FALSE)
  d$.grp <- df[[group_var]]
  d$val  <- df[[pn]]
  d$.w   <- if (is.null(weights)) 1 else as.numeric(weights)
  out <- d %>%
    dplyr::group_by(.data$.id, .data$.grp) %>%
    dplyr::summarise(n_vals = dplyr::n_distinct(stats::na.omit(.data$val)),
                     val = dplyr::first(stats::na.omit(.data$val)),
                     .w = mean(.data$.w), .groups = "drop")
  conf <- unique(out$.id[out$n_vals > 1])
  if (length(conf))
    log_once(logger, sprintf(
      "'%s' is summarized per patient, but %d patient(s) have conflicting values across their eyes (e.g. %s); the first recorded value is used.",
      disp_name(pn, display_names), length(conf), paste(utils::head(conf, 5), collapse = ", ")))
  out$n_vals <- NULL
  names(out)[names(out) == ".id"]  <- patient_id_clean
  names(out)[names(out) == ".grp"] <- group_var
  out
}

# ---- weighted descriptives (matching weights) ----------------
# Weighted mean and SD; the SD uses the reliability-weights correction, so with
# unit weights it equals sd().
.wmean_sd <- function(x, w) {
  ok <- is.finite(x) & is.finite(w) & w > 0
  x <- x[ok]; w <- w[ok]
  if (!length(x)) return(c(NA_real_, NA_real_))
  m <- sum(w * x) / sum(w)
  sw <- sum(w); sw2 <- sum(w^2)
  s <- if (length(x) > 1 && sw^2 > sw2) sqrt(sum(w * (x - m)^2) * sw / (sw^2 - sw2)) else NA_real_
  c(m, s)
}

# Weighted quantile: the smallest value whose cumulative weight share reaches p.
.wquantile <- function(x, w, probs) {
  ok <- is.finite(x) & is.finite(w) & w > 0
  x <- x[ok]; w <- w[ok]
  if (!length(x)) return(rep(NA_real_, length(probs)))
  o <- order(x); x <- x[o]; cw <- cumsum(w[o]) / sum(w)
  vapply(probs, function(p) x[which(cw >= p - 1e-10)[1]], numeric(1))
}

# A weighted count: whole numbers as integers, otherwise one decimal.
.fmt_w <- function(z) ifelse(is.finite(z) & abs(z - round(z)) < 1e-9,
                             as.character(round(z)), sprintf("%.1f", z))

# Group comparison under matching weights: a WEIGHTED GEE of y on the group
# with an INDEPENDENCE working correlation and the robust sandwich SE clustered
# by patient (or by row) - the estimator MatchIt recommends after matching
# (contract R4-1). Its estimate is exactly the difference of the weighted means
# / proportions printed in the same row (an exchangeable working correlation
# would test another contrast). Wald test on all group terms. Returns
# list(p, test, note) like group_test(). Nothing is fitted - and the note says
# why, in the words the figures' weighted GEE uses, checked in the same order -
# when:
#   1. p_value_guard() (helpers.R) objects (a single group, a constant
#      variable, "B has n = 1");
#   2. a group holds one patient (cluster_guard_note, helpers.R: "B has 1 patient");
#   3. the data are below the minimum for any clustered model
#      (model_data_guard, helpers.R: "too few eyes for this model (n = 5)");
#   4. a group of a 0/1 outcome has no events or only events: the weighted
#      model has no finite estimate then, and there is deliberately no
#      unweighted or exact fallback (it would answer a different question under
#      the matched-cohort heading); the note says which group.
# levs: optional labels of the 0 and 1 outcome (a two-level categorical), named
# in that note instead of "events". unit: "eye" when `id` is the row (no
# patient ID: each eye its own cluster), so the notes count eyes.
# The Wald test takes the run's small-sample inference (helpers.R gee_wald;
# decision D11: Mancl-DeRouen with a t reference for one comparison, F for
# several), and the label says so: "Weighted Wald test (GEE, robust SE
# (Mancl-DeRouen), t reference)"; with gee_small_sample "none" the v1.0.0
# label "Weighted Wald test (GEE, robust SE)".
.weighted_wald <- function(y, g, wt, id, family = c("gaussian", "binomial"), ranks = FALSE,
                           levs = NULL, unit = "patient") {
  family <- match.arg(family)
  y <- suppressWarnings(as.numeric(y))
  wt <- suppressWarnings(as.numeric(wt))
  glv <- if (is.factor(g)) levels(g) else unique(as.character(g[!is.na(g)]))
  ok <- is.finite(y) & !is.na(g) & is.finite(wt) & wt > 0 & !is.na(id)
  if (ranks) y[ok] <- rank(y[ok])
  d <- data.frame(.y = y[ok], .g = droplevels(factor(as.character(g[ok]), levels = glv)),
                  .w = wt[ok], .id = as.character(id[ok]), stringsAsFactors = FALSE)
  lab <- paste0(if (ranks) "Weighted Wald test on ranks" else "Weighted Wald test",
                " (GEE, ", gee_se_label(max(1L, nlevels(d$.g) - 1L)), ")")
  res <- function(p, note = "", test = lab) {
    p <- suppressWarnings(as.numeric(p))
    if (length(p) != 1 || !is.finite(p))
      list(p = NA_real_, test = test,
           note = if (nzchar(note)) note else "the weighted test could not be computed")
    else list(p = p, test = test, note = "")
  }
  why <- p_value_guard(d$.y, d$.g, if (family == "binomial") "dichotomous" else "continuous")
  if (nzchar(why)) {
    # a constant variable / a single group: no test is named that never ran
    na_lab <- p_guard_label(why)
    return(res(NA, why, test = if (identical(na_lab, "Not tested")) lab else na_lab))
  }
  d$.cid <- as.integer(factor(d$.id, levels = unique(d$.id)))
  # a group whose eyes all belong to one patient (cluster) leaves the robust SE
  # nothing to estimate its variance from: no P (the one implementation of this
  # guard, shared with the GEE models and the figures)
  one_pt <- cluster_guard_note(d$.id, d$.g, unit)
  if (nzchar(one_pt)) return(res(NA, one_pt))
  # the minimum data for any clustered model (R4-10): y ~ group has one
  # coefficient per group
  small <- model_data_guard(nrow(d), length(unique(d$.cid)), nlevels(d$.g), unit)
  if (!is.null(small)) return(res(NA, small))
  if (family == "binomial") {
    s0 <- tapply(d$.y, d$.g, function(z) all(z == 0))
    s1 <- tapply(d$.y, d$.g, function(z) all(z == 1))
    sep <- names(s0)[s0 | s1]
    if (length(sep)) {
      what <- vapply(sep, function(gn) {
        if (!is.null(levs)) sprintf("every eye in %s is '%s'", gn, levs[if (s0[[gn]]) 1 else 2])
        else paste0(if (s0[[gn]]) "no events in " else "only events in ", gn)
      }, character(1))
      return(res(NA, paste0("no weighted test possible: ", paste(what, collapse = "; ")),
                 test = "Not available with matching weights"))
    }
  }
  d <- d[order(d$.cid), , drop = FALSE]          # geepack needs rows sorted by cluster
  fam <- if (family == "binomial") stats::binomial() else stats::gaussian()
  f <- .y ~ .g
  # independence working correlation (R4-1); anything geepack prints stays off
  # the console (quiet_eval, helpers.R)
  fit <- quiet_eval(suppressWarnings(geepack::geeglm(
           f, data = d, id = .cid, weights = .w, family = fam, corstr = "independence")))$value
  if (is.null(fit)) return(res(NA))
  # joint Wald test of the group terms (helpers.R gee_wald, the figures' test):
  # a numerically singular robust covariance gives no P value - the figures'
  # weighted GEE refuses it by the same rule (helpers.R gee_vcov_singular)
  wd <- gee_wald(fit, grep("^\\.g", names(stats::coef(fit))), unit)
  res(wd$p, wd$why)
}

# Patient-clustered table test of a per-eye row (decision D12), run when a
# patient ID is in scope, the classical tests were not chosen and at least one
# patient contributes two of the row's analysed eyes. The models are the ones
# behind the figures' clustered P values (helpers.R gee_group_p), so a table
# and a figure of one outcome give one P value:
#   continuous  -> Gaussian GEE of the value on the group;
#   ordinal     -> Gaussian GEE on the ranks;
#   dichotomous -> binomial GEE of the event (v: TRUE / FALSE / NA);
#   categorical -> two levels: binomial GEE of the second level; more: the
#                  joint Wald test on stacked level indicators
#                  (helpers.R gee_categorical_p).
# Every group term is tested jointly (more than two groups: the omnibus test),
# with the run's small-sample inference (decision D11). Working correlation:
# exchangeable; independence on a matched cohort (matched = TRUE, decision
# D18). Nothing is fitted - and the note says why, in the figures' words -
# when a group has one patient, the data are below the minimum of a clustered
# model, or there are too few patients for the t reference. A model that was
# fitted but failed (no convergence, a numerically singular covariance) or a
# 0/1 outcome without events (or with only events) in a group: the classical
# test runs instead and its label says "(GEE not fitted)", exactly as on the
# figures. Returns list(p, test, note) like group_test().
.clustered_test <- function(v, g, id, kind = c("continuous", "ordinal", "dichotomous", "categorical"),
                            matched = FALSE, unit = "patient") {
  kind <- match.arg(kind)
  cs <- if (isTRUE(matched)) "independence" else NULL
  num <- kind %in% c("continuous", "ordinal")
  x <- if (num) as_numeric_safe(v) else v
  use <- (if (num) is.finite(x) else !is.na(x)) & !is.na(g)
  k <- length(unique(as.character(g[use])))
  q <- max(1L, k - 1L)
  if (kind == "categorical") {
    vv <- as.character(v)
    levs <- sort(unique(stats::na.omit(vv[use])))
    if (length(levs) > 2) {
      base <- "GEE on level indicators"
      q <- max(1L, (length(levs) - 1L) * (k - 1L))
      p <- gee_categorical_p(vv, g, id, unit = unit)
    } else {
      base <- "Binomial GEE"
      p <- gee_group_p(if (length(levs) == 2) vv == levs[2] else vv == levs[1], g, id, "proportion",
                       unit = unit, corstr = cs)
    }
  } else if (kind == "dichotomous") {
    base <- "Binomial GEE"
    p <- gee_group_p(as.logical(v), g, id, "proportion", unit = unit, corstr = cs)
  } else {
    base <- if (kind == "ordinal") "GEE on ranks" else "GEE"
    p <- gee_group_p(x, g, id, "continuous", unit = unit, ranks = kind == "ordinal", corstr = cs)
  }
  # an exchangeable fit that was not accepted: the independence refit is named (R3-4)
  if (nzchar(as.character(attr(p, "refit") %||% "")[1])) base <- paste0(base, ", ", GEE_INDEP_REFIT)
  lab <- paste0(base, ", clustered by ", if (identical(unit, "eye")) "eye" else "patient",
                " (", gee_se_label(q), ")")
  # a single group, a constant variable, "B has n = 1": no test, and no test
  # is named that never ran (the weighted test's rule)
  why0 <- p_value_guard(if (num) x else as.character(v), g, if (num) kind else "dichotomous")
  if (nzchar(why0)) {
    na_lab <- p_guard_label(why0)
    return(list(p = NA_real_, test = if (identical(na_lab, "Not tested")) lab else na_lab, note = why0))
  }
  if (is.finite(p)) return(list(p = as.numeric(p), test = lab, note = ""))
  why <- as.character(attr(p, "why") %||% "")[1]
  if (!is.na(why) && nzchar(why)) return(list(p = NA_real_, test = lab, note = why))
  # the classical test, with the eyes treated as independent: said in the
  # label, and the cause in the note (printed after the interpretation, and
  # listed in the run log by the table writer: .gee_fallback_take)
  gt <- group_test(v, g, kind)
  gt$test <- paste0(gt$test, ", eyes treated as independent (GEE not fitted)")
  if (is.finite(gt$p)) {
    gt$note <- paste0("GEE not fitted: ", .gee_fail_cause(v, g, kind), "; the P value treats the eyes as independent")
    .gee_fallback_rows$n <- .gee_fallback_rows$n + 1L
  }
  gt
}

# Why a patient-clustered GEE of a table row could not be fitted, in words: a
# group whose eyes all fall in one category (separation: no finite estimate),
# else no convergence or a numerically singular covariance
.gee_fail_cause <- function(v, g, kind) {
  ok <- !is.na(v) & !is.na(g)
  if (kind %in% c("dichotomous", "categorical")) {
    vv <- as.character(v[ok]); gg <- as.character(g[ok])
    if (kind == "dichotomous") vv <- ifelse(vv %in% c("TRUE", "1"), "with the event", "without the event")
    levs <- sort(unique(vv))
    what <- unlist(lapply(unique(gg), function(gn) {
      miss <- setdiff(levs, vv[gg == gn])
      if (length(miss)) sprintf("no eye %s in %s",
                                if (kind == "dichotomous") miss else sprintf("with '%s'", miss), gn)
    }))
    if (length(what)) return(paste0(paste(utils::head(what, 3), collapse = "; "),
                                    if (length(what) > 3) "; ..." else "", " (separation)"))
  }
  "the model did not converge or its robust covariance is singular"
}
# rows of the summary tables whose clustered GEE could not be fitted in this
# run (counted by .clustered_test, logged once per table set)
.gee_fallback_rows <- new.env()
.gee_fallback_rows$n <- 0L
.gee_fallback_take <- function() { n <- .gee_fallback_rows$n; .gee_fallback_rows$n <- 0L; n }

# ------------------------------------------------------------
# N-group summary table.
# glevels: character vector of group levels, in display order
#          (first = reference). One data column per level.
# Tests come from group_test() (helpers.R), the same selector the
# figures use. weights: optional matching weights (vector, or the
# name of a column); when any weight differs from 1 every cell is
# weighted (mean ± SD, proportions, median [IQR]), the test is a
# weighted GEE Wald test and the header shows the effective sample
# size. Rows of variables analysed per patient carry "[per patient]"
# and the header then shows both eye and patient counts.
# display_names: optional clean key -> original header map (R4-3); the
# Parameter labels then name each variable by its header as typed in the file.
# The returned tibble carries attr "row_key" (the clean key of the variable of
# each row) and attr "row_var" (its display label), so the GEE / regression
# merges find a variable's rows exactly, whatever its header looks like.
# table_tests (decision D12): "auto" (default) - with a patient ID, every
# per-eye row in which at least one patient contributes two analysed eyes gets
# the patient-clustered test (.clustered_test: the model of the figures'
# clustered P value, so a table and a figure of one outcome agree); the other
# rows - no patient ID, one analysed eye per patient, [per patient] rows -
# keep the classical tests of group_test(). "classical": group_test() for
# every unweighted row (the v1.0.0 tables). Weighted rows always get the
# weighted test. matched = TRUE: a matched cohort, whose clustered tests use
# an independence working correlation (decision D18).
# Cells (decision D24): means, SDs and medians with two decimals, percentages
# with one. va_cols: the visual-acuity columns analysed in logMAR (export.R
# va_info()$in_logmar); their continuous cells read "20/20 (0.00 ± 0.05
# logMAR); range 20/16 to 20/32" (decision D16, helpers.R fmt_va).
# ------------------------------------------------------------
create_summary_table <- function(df, params_clean, type_map,
                                 glevels,
                                 group_var = "GroupLabel",
                                 patient_id_clean = NA_character_,
                                 patient_level_params_clean = character(0),
                                 dicho_pref_clean = list(),
                                 weights = NULL,
                                 logger = NULL,
                                 display_names = NULL,
                                 table_tests = "auto",
                                 matched = FALSE,
                                 va_cols = character(0)) {

  if (length(params_clean) == 0) return(tibble::tibble())
  dn <- function(x) disp_name(x, display_names)

  gv <- droplevels(factor(as.character(df[[group_var]]), levels = glevels))
  df[[group_var]] <- gv
  glev <- levels(gv)
  k <- length(glev)
  if (k < 2) stop("create_summary_table(): need >= 2 groups.")

  w_all <- NULL
  if (!is.null(weights)) {
    w_all <- if (is.character(weights) && length(weights) == 1) df[[weights]] else weights
    w_all <- suppressWarnings(as.numeric(w_all))
    if (length(w_all) != nrow(df)) stop("create_summary_table(): `weights` needs one value per row.")
    if (all(!is.na(w_all) & abs(w_all - 1) < 1e-8)) w_all <- NULL
  }
  weighted <- !is.null(w_all)

  has_pid <- .has_col(df, patient_id_clean)
  if (has_pid) df[[patient_id_clean]] <- fill_missing_ids(df[[patient_id_clean]])
  # cluster of each row for the weighted tests: the patient, else the row (each
  # eye its own cluster, and the notes then count eyes)
  row_id <- if (has_pid) df[[patient_id_clean]] else paste0("row ", seq_len(nrow(df)))
  cl_unit <- if (has_pid) "patient" else "eye"
  # patient-clustered tests of the unweighted per-eye rows (decision D12)
  tt_mode <- tolower(trimws(as.character(table_tests %||% "auto")[1]))
  cl_tests <- has_pid && !identical(tt_mode, "classical")
  # a row is tested clustered when one of its patients has two analysed eyes
  clustered_row <- function(use, ids, unit_tag)
    cl_tests && !nzchar(unit_tag) && fellow_eyes_present(ids[use])

  in_grp <- function(gvec, g) !is.na(gvec) & gvec == g
  n_eye <- vapply(glev, function(g) sum(in_grp(gv, g)), integer(1))
  n_pat <- setNames(rep(NA_integer_, k), glev)
  if (has_pid) n_pat[] <- vapply(glev, function(g) length(unique(row_id[in_grp(gv, g)])), integer(1))

  # the same header for every table of one cohort (the Publication Table
  # stacks them), so it depends on the configuration, not on this table's rows
  any_pat <- has_pid && length(patient_level_params_clean) > 0
  # (singular for one: "2 eyes; 1 patient", T1-15)
  hdr_n <- if (any_pat) paste0(n_eye, ifelse(n_eye == 1, " eye; ", " eyes; "), n_pat,
                               ifelse(n_pat == 1, " patient", " patients")) else as.character(n_eye)
  if (weighted) {
    ess <- vapply(glev, function(g) {
      w <- w_all[in_grp(gv, g)]; w <- w[is.finite(w) & w > 0]
      if (!length(w)) NA_real_ else sum(w)^2 / sum(w^2)
    }, numeric(1))
    hdr_n <- paste0(hdr_n, ", ESS = ", round(ess))
  }
  col_names <- safe_colname_vec(paste0(glev, " (n = ", hdr_n, ")"), max_chars = 100)

  interp_word <- if (k > 2) "among groups" else "between groups"

  out <- lapply(params_clean, function(pn){
    var_type <- get_var_type(pn, type_map)
    if (is.na(var_type)) var_type <- "continuous"

    v <- df[[pn]]
    gvec <- df[[group_var]]
    w <- w_all
    ids <- row_id
    denom <- n_eye
    unit_tag <- ""

    if (has_pid && pn %in% patient_level_params_clean && pn %in% names(df)) {
      tmp <- collapse_patient_level(df, pn, group_var, patient_id_clean,
                                    weights = w_all, logger = logger, display_names = display_names)
      v <- tmp$val
      gvec <- droplevels(factor(as.character(tmp[[group_var]]), levels = glev))
      w <- if (weighted) tmp$.w else NULL
      ids <- as.character(tmp[[patient_id_clean]])
      denom <- n_pat
      unit_tag <- " [per patient]"
    }

    pl <- dn(pn)                              # the variable as the user named it
    mk_row <- function(cells, pval, test, param_label = pl, note = "") {
      row <- tibble::tibble(Parameter = paste0(param_label, unit_tag))
      for (j in seq_along(col_names)) row[[col_names[j]]] <- cells[j]
      row$p_value <- fmt_p(pval)
      row$Test <- test
      # Three states, not two: an NA p means the test could NOT be performed,
      # which must never be reported as "not statistically significant".
      row$`Practical Interpretation` <- if (is.na(pval)) {
        if (nzchar(note)) paste0("Not estimable - ", note, ".")
        else "Not estimable - the test could not be performed (insufficient or missing data)."
      } else if (nzchar(note)) {
        # a P that came with a note (a clustered GEE not fitted): the cause
        paste0(p_interpretation(pval, interp_word), " Note: ", note, ".")
      } else p_interpretation(pval, interp_word)
      row
    }

    if (is.null(v))
      return(mk_row(rep(NA_character_, k), NA_real_, "Not applicable", note = "column not found"))

    # Count of usable values per group. Shown in a continuous / ordinal cell
    # whenever it differs from the header denominator, so heavy missingness is
    # visible (the header n counts every eye of the group).
    with_n <- function(cells, nn) {
      show <- (is.na(denom) | nn != denom) & nn > 0 & !grepl("(n = ", cells, fixed = TRUE)
      cells[show] <- paste0(cells[show], " (n = ", nn[show], ")")
      cells
    }

    if (var_type %in% c("continuous", "ordinal")) {
      v_num <- as_numeric_safe(v)
      fin <- is.finite(v_num)
      nn <- vapply(glev, function(g) sum(fin & in_grp(gvec, g)), integer(1))
      is_va <- var_type == "continuous" && pn %in% va_cols
      cells <- vapply(glev, function(g) {
        sel <- fin & in_grp(gvec, g)
        x <- v_num[sel]
        if (!length(x)) return("no data")
        if (var_type == "continuous") {
          ms <- if (weighted) .wmean_sd(x, w[sel])
                else c(mean(x), if (length(x) > 1) stats::sd(x) else NA_real_)
          # acuity in logMAR: the Snellen geometric mean, the logMAR mean ± SD
          # and the range in Snellen (D16); a weighted cell's range is over
          # the eyes with a positive weight
          if (is_va) {
            xr <- if (weighted) x[is.finite(w[sel]) & w[sel] > 0] else x
            return(fmt_va(ms[1], ms[2], if (length(xr)) min(xr) else NA, if (length(xr)) max(xr) else NA))
          }
          # a single value has no SD: "37.00 (n = 1)", never "37.00 ± n/a" (T1-15)
          if (is.finite(ms[2])) paste0(fmt_fix(ms[1]), " ± ", fmt_fix(ms[2]))
          else paste0(fmt_fix(ms[1]), " (n = ", length(x), ")")
        } else {
          q <- if (weighted) .wquantile(x, w[sel], c(.5, .25, .75))
               else c(stats::median(x), stats::quantile(x, .25), stats::quantile(x, .75))
          fmt_quartiles(q)
        }
      }, character(1))
      cells <- with_n(cells, nn)
      tt <- if (weighted) .weighted_wald(v_num, gvec, w, ids, "gaussian", ranks = var_type == "ordinal",
                                         unit = cl_unit)
            else if (clustered_row(fin & !is.na(gvec), ids, unit_tag))
              .clustered_test(v_num, gvec, ids, var_type, matched = matched)
            else group_test(v_num, gvec, var_type)
      return(mk_row(cells, tt$p, tt$test, note = tt$note))
    }

    if (var_type == "dichotomous") {
      lev <- pick_display_level(pn, v, dicho_pref_clean, logger, display_names = display_names)
      present <- !is.na(v)
      ev <- present & as.character(v) == as.character(lev)
      ev[is.na(ev)] <- FALSE
      # n/N (%): N = non-missing, the same population the test uses
      cells <- vapply(seq_along(glev), function(j) {
        sel <- present & in_grp(gvec, glev[j])
        if (weighted) {
          wn <- sum(w[sel], na.rm = TRUE); wc <- sum(w[sel & ev], na.rm = TRUE)
          paste0(.fmt_w(wc), "/", .fmt_w(wn), " (", fmt_pct(wc, wn), ")")
        } else {
          nn <- sum(sel); c_ <- sum(sel & ev)
          paste0(c_, "/", nn, " (", fmt_pct(c_, nn), ")")
        }
      }, character(1))
      y <- ifelse(present, ev, NA)
      tt <- if (weighted) .weighted_wald(as.numeric(y), gvec, w, ids, "binomial", unit = cl_unit)
            else if (clustered_row(present & !is.na(gvec), ids, unit_tag))
              .clustered_test(y, gvec, ids, "dichotomous", matched = matched)
            else group_test(y, gvec, "dichotomous")
      return(mk_row(cells, tt$p, tt$test, param_label = paste0(pl, " (", lev, ")"), note = tt$note))
    }

    if (var_type == "categorical") {
      vv <- as.character(v)
      levs <- sort(unique(stats::na.omit(vv)))
      if (length(levs) == 0)
        return(mk_row(rep(NA_character_, k), NA_real_, "Not applicable",
                      note = "no recorded values"))
      # weighted: a two-level variable is a weighted 0/1 comparison; there is
      # no weighted test for more than two levels - and every untested row
      # states its true reason (a constant variable is "constant" here too)
      why_w <- if (weighted) p_value_guard(vv, gvec, "categorical") else ""
      tt <- if (!weighted && clustered_row(!is.na(vv) & !is.na(gvec), ids, unit_tag))
              .clustered_test(vv, gvec, ids, "categorical", matched = matched)
            else if (!weighted) group_test(vv, gvec, "categorical")
            else if (nzchar(why_w)) list(p = NA_real_, test = p_guard_label(why_w), note = why_w)
            else if (length(levs) == 2)
              .weighted_wald(ifelse(is.na(vv), NA, as.numeric(vv == levs[2])), gvec, w, ids, "binomial",
                             levs = levs, unit = cl_unit)
            else list(p = NA_real_, test = "Not available with matching weights",
                      note = "no weighted test for a categorical variable with more than two levels")
      rows <- lapply(seq_along(levs), function(i){
        lev <- levs[i]
        cells <- vapply(seq_along(glev), function(j){
          sel <- !is.na(vv) & in_grp(gvec, glev[j])
          if (weighted) {
            wn <- sum(w[sel], na.rm = TRUE); wc <- sum(w[sel & vv == lev], na.rm = TRUE)
            paste0(.fmt_w(wc), "/", .fmt_w(wn), " (", fmt_pct(wc, wn), ")")
          } else {
            nn <- sum(sel); c_ <- sum(sel & vv == lev)
            paste0(c_, "/", nn, " (", fmt_pct(c_, nn), ")")
          }
        }, character(1))
        r <- mk_row(cells, if (i == 1) tt$p else NA_real_,
                    if (i == 1) tt$test else "", param_label = paste0(pl, ": ", lev),
                    note = tt$note)
        if (i != 1) { r$p_value <- ""; r$`Practical Interpretation` <- "" }
        r
      })
      return(dplyr::bind_rows(rows))
    }

    mk_row(rep(NA_character_, k), NA_real_, "Not applicable")
  })

  keys <- rep(as.character(params_clean), vapply(out, nrow, integer(1)))
  out <- dplyr::bind_rows(out)
  attr(out, "row_key") <- keys
  attr(out, "row_var") <- dn(keys)
  out
}

safe_colname_vec <- function(x, max_chars = 60) vapply(x, safe_colname, character(1), max_chars = max_chars)

# The table tests a configuration runs (decision D12): "classical" when
# cfg$table_tests is "classical" or the patient clustering is switched off
# (cluster_by_patient = FALSE, which leaves the figures unclustered too, so a
# table and a figure still agree); otherwise "auto" (the default; a missing or
# unknown value reads as "auto").
table_tests_of <- function(cfg) {
  if (!isTRUE(as.logical(unlist(cfg$cluster_by_patient %||% TRUE))[1])) return("classical")
  tt <- tolower(trimws(as.character(unlist(cfg$table_tests %||% "auto"))[1]))
  if (identical(tt, "classical")) "classical" else "auto"
}
# The same in words, for the run log and the Manifest.
table_tests_text <- function(cfg, has_pid = TRUE) {
  classical <- "Welch t-test / Welch ANOVA, Wilcoxon / Kruskal-Wallis, chi-squared / Fisher's exact"
  if (table_tests_of(cfg) == "classical")
    return(paste0("classical (", classical, ") for every unweighted row",
                  if (!isTRUE(as.logical(unlist(cfg$cluster_by_patient %||% TRUE))[1]))
                    " - patient clustering is switched off" else ""))
  if (!isTRUE(has_pid))
    return(paste0("classical (", classical, ") - no patient ID is mapped, so eyes are treated as independent"))
  paste0("auto - patient-clustered GEE (continuous: Gaussian; ordinal: on ranks; dichotomous: ",
         "binomial; categorical: stacked level indicators; more than two groups: joint test) for ",
         "every per-eye row in which a patient has two analyzed eyes; classical (", classical,
         ") for one eye per patient and for per-patient rows")
}

# ------------------------------------------------------------
# Initial summary (patients, eyes, age, follow-up, sex, groups)
# Every row states its unit. Age is summarised per patient when a
# patient ID is mapped and each patient has one recorded age (the
# same unit as "Number of Patients"); otherwise per eye. Follow-up
# belongs to the operated eye and is always per eye. Sex is per
# patient; patients whose rows disagree get their own row. An
# unmapped or absent column (NA, NULL, "") is left blank.
# Returns list(initial, gender, groups).
# ------------------------------------------------------------
make_initial_summary <- function(df, patient_id_clean, age_clean, fu_clean, sex_clean,
                                 logger = NULL) {
  has_pid <- .has_col(df, patient_id_clean)
  # rows without a patient ID are separate patients, never one shared NA patient
  ids <- if (has_pid) fill_missing_ids(df[[patient_id_clean]]) else NULL
  num_patients <- if (has_pid) length(unique(ids)) else NA_integer_
  num_eyes <- nrow(df)
  age <- if (.has_col(df, age_clean)) as_numeric_safe(df[[age_clean]]) else rep(NA_real_, nrow(df))
  fu  <- if (.has_col(df, fu_clean))  as_numeric_safe(df[[fu_clean]])  else rep(NA_real_, nrow(df))

  age_unit <- "per eye"
  if (has_pid && any(is.finite(age))) {
    per_pt <- split(age, ids)
    n_vals <- vapply(per_pt, function(a) length(unique(a[is.finite(a)])), integer(1))
    if (all(n_vals <= 1)) {
      age <- vapply(per_pt, function(a) { a <- a[is.finite(a)]; if (length(a)) a[1] else NA_real_ },
                    numeric(1))
      age_unit <- "per patient"
    } else {
      log_once(logger, sprintf(
        "Initial Summary: age differs between the eyes of %d patient(s) (e.g. %s); age is summarized per eye.",
        sum(n_vals > 1), paste(utils::head(names(per_pt)[n_vals > 1], 5), collapse = ", ")))
    }
  }

  # fixed decimals (D24): two for means and SDs, as in the summary tables
  fx <- function(v) if (is.finite(v)) fmt_fix(v) else NA
  age_mean <- fx(mean(age, na.rm = TRUE))
  age_sd   <- fx(suppressWarnings(sd(age, na.rm = TRUE)))
  age_min  <- suppressWarnings(min(age, na.rm = TRUE)); age_max <- suppressWarnings(max(age, na.rm = TRUE))
  fu_mean  <- fx(mean(fu, na.rm = TRUE))
  fu_sd    <- fx(suppressWarnings(sd(fu, na.rm = TRUE)))
  fu_med   <- ifelse(is.finite(median(fu, na.rm = TRUE)), round(median(fu, na.rm = TRUE), 2), NA)
  fu_q1    <- ifelse(any(is.finite(fu)), round(quantile(fu, .25, na.rm = TRUE), 2), NA)
  fu_q3    <- ifelse(any(is.finite(fu)), round(quantile(fu, .75, na.rm = TRUE), 2), NA)

  au <- paste0(" (", age_unit, ")")
  initial <- data.frame(
    Description = c("Number of Patients","Number of Eyes",
                    paste0(c("Mean Age", "SD Age", "Age Range"), au),
                    "Mean Follow-up Time (per eye)","SD Follow-up Time (per eye)",
                    "Median Follow-up Time [IQR] (per eye)"),
    Value = c(num_patients, num_eyes, age_mean, age_sd,
              ifelse(is.finite(age_min) && is.finite(age_max), paste(age_min, "-", age_max), NA),
              fu_mean, fu_sd,
              # two decimals, like the mean and SD above, whatever the data (D24,
              # T1-11: "181 [169–193]" and "180.00 [168.00–192.75]" were mixed)
              ifelse(is.finite(fu_med) && is.finite(fu_q1) && is.finite(fu_q3),
                     paste0(fmt_fix(fu_med), " [", fmt_fix(fu_q1), "–", fmt_fix(fu_q3), "]"), NA)),
    stringsAsFactors = FALSE
  )

  # Sex per patient: ONE value per patient (the first recorded), so the
  # categories add up to 100 %. Values are compared after trimming spaces
  # ("F " is "F"); a blank cell is missing. A patient recorded with different
  # sexes on different rows is a data-entry error: counted once, and reported.
  gender <- data.frame()
  conflicts <- character(0)
  if (has_pid && .has_col(df, sex_clean) &&
      is.finite(num_patients) && num_patients > 0) {
    sx <- trimws(as.character(df[[sex_clean]]))
    sx[!is.na(sx) & !nzchar(sx)] <- NA_character_
    pt <- data.frame(id = ids, sex = sx, stringsAsFactors = FALSE) %>%
      dplyr::group_by(.data$id) %>%
      dplyr::summarise(n_vals = dplyr::n_distinct(stats::na.omit(.data$sex)),
                       Sex = dplyr::first(stats::na.omit(.data$sex)), .groups = "drop")
    gender <- pt %>%
      dplyr::count(.data$Sex, name = "Count") %>%
      dplyr::mutate(Percentage = round(100 * .data$Count / num_patients, 1))
    names(gender)[1] <- "Sex"
    gender <- as.data.frame(gender)
    miss <- is.na(gender$Sex)
    gender$Sex[miss] <- "(not recorded)"
    gender <- gender[order(miss), , drop = FALSE]
    rownames(gender) <- NULL
    conflicts <- pt$id[pt$n_vals > 1]
    if (length(conflicts)) {
      log_once(logger, sprintf(
        "Sex: %d patient(s) have conflicting values across their rows (e.g. %s); the first recorded value is used.",
        length(conflicts), paste(utils::head(conflicts, 5), collapse = ", ")))
      # stated in the table itself, next to the counts it affects
      initial <- rbind(initial, data.frame(
        Description = "Patients with conflicting sex entries (first recorded value counted)",
        Value = as.character(length(conflicts)), stringsAsFactors = FALSE))
    }
  }

  groups <- df %>%
    dplyr::group_by(GroupLabel) %>%
    dplyr::summarise(Count = dplyr::n(), Percentage = round(100 * dplyr::n() / nrow(df), 1),
                     .groups = "drop")

  list(initial = initial, gender = gender, groups = groups)
}

# ------------------------------------------------------------
# GEE
# ------------------------------------------------------------
# 0/1 outcome for the logistic models. The event level is the preferred level
# resolved against the data (case, spaces, 1 = 1.0 = TRUE) or, when there is
# none or it cannot be resolved, default_positive_level() - the same rule the
# summary table uses for its displayed level. A fallback is logged once.
make_binary_01 <- function(x, positive_level = NULL, logger = NULL, var = "") {
  ux <- unique(x[!is.na(x)])
  if (length(ux) != 2) return(rep(NA_real_, length(x)))
  lev <- if (.has_pref(positive_level)) resolve_positive_level(positive_level, ux) else NA
  if (length(lev) != 1 || is.na(lev)) {
    fb <- default_positive_level(ux)
    if (.has_pref(positive_level))
      log_once(logger, .pref_fallback_msg(var, positive_level, ux, fb))
    lev <- fb
  }
  as.numeric(as.character(x) == as.character(lev))
}

# Cluster column for the clustered models. Rows without a patient ID become
# their own cluster; with NO patient-ID column every row is its own cluster
# (eyes treated as independent), the returned note says so and `unit` is "eye"
# (the few-clusters texts then count eyes, not patients).
.cluster_setup <- function(df, patient_id_clean, logger = NULL) {
  if (.has_col(df, patient_id_clean)) {
    df[[patient_id_clean]] <- fill_missing_ids(df[[patient_id_clean]])
    return(list(df = df, id = patient_id_clean, note = "", unit = "patient"))
  }
  log_once(logger, paste0("No patient ID column: GEE and adjusted regression treat each eye ",
                          "as its own cluster (eyes assumed independent)."))
  df$.row_cluster <- seq_len(nrow(df))
  list(df = df, id = ".row_cluster", unit = "eye",
       note = "no patient ID column - each eye is its own cluster (eyes assumed independent)")
}

# Levels of each categorical predictor in which a 0/1 outcome is constant
# (no events, or only events): complete separation, so a logistic model
# returns an absurd OR (e.g. 7e-20, CI [0, 0], p = 0) for that term.
# Returns a named list predictor -> c("no events in <level>", ...).
.separation_levels <- function(df2, outc, preds) {
  out <- list()
  y <- df2[[outc]]
  for (p in preds) {
    x <- df2[[p]]
    if (is.numeric(x)) next
    x <- droplevels(factor(as.character(x)))
    s <- tapply(y, x, function(z) if (all(z == 0)) "no events" else if (all(z == 1)) "only events" else "")
    s <- s[!is.na(s) & nzchar(s)]
    if (length(s)) out[[p]] <- paste0(s, " in ", names(s))
  }
  out
}

# Groups whose comparison with the reference cannot be estimated in a model of
# the outcome on the group: a 0/1 outcome with no events or only events in the
# group (complete separation), or a continuous outcome with a single usable
# value in the group (its residual is 0, so the robust SE ignores it and the P
# is degenerate - contract R3-1). Named character: group -> reason
# ("no events in C", "C has n = 1"); empty when every comparison can be estimated.
.unestimable_groups <- function(y, g, binary) {
  ok <- !is.na(y) & !is.na(g)
  y <- y[ok]
  g <- droplevels(factor(as.character(g[ok]),
                         levels = if (is.factor(g)) levels(g) else unique(as.character(g[ok]))))
  if (!length(y)) return(character(0))
  if (binary) {
    s <- tapply(y, g, function(z) if (all(z == 0)) "no events" else if (all(z == 1)) "only events" else "")
    s <- s[!is.na(s) & nzchar(s)]
    if (!length(s)) return(character(0))
    return(stats::setNames(paste0(s, " in ", names(s)), names(s)))
  }
  cnt <- table(g)
  small <- cnt[cnt < 2]
  if (!length(small)) return(character(0))
  stats::setNames(paste0(names(small), " has n = ", as.integer(small)), names(small))
}

# Complete separation (or a one-eye group) makes ONLY the affected comparison
# "not estimable" (contract R3-6): the rows of those non-reference groups are
# left out of the model, so every other comparison of the same outcome is still
# estimated - with the separated group in the model its term diverges and its
# rows carry no information about the other terms anyway. The whole outcome is
# skipped (skip = the reasons) only when the reference group itself is
# affected or fewer than two groups would remain.
# Returns list(df, skip = <reason or "">, out = <group -> reason>,
#              n = <group -> eyes>, events = <group -> eyes with the event>).
.drop_unestimable_groups <- function(df2, outc, ref, binary) {
  y <- df2[[outc]]; g <- df2$GroupLabel
  bad <- .unestimable_groups(y, g, binary)
  none <- list(df = df2, skip = "", out = character(0), n = integer(0), events = integer(0))
  if (!length(bad)) return(none)
  present <- unique(as.character(g[!is.na(g) & !is.na(y)]))
  if (ref %in% names(bad) || length(setdiff(present, names(bad))) < 2) {
    none$skip <- paste(bad, collapse = ", ")
    return(none)
  }
  gi <- as.character(g)
  n_g <- vapply(names(bad), function(b) sum(gi == b & !is.na(y), na.rm = TRUE), integer(1))
  ev_g <- vapply(names(bad), function(b) as.integer(sum(y[gi == b & !is.na(y)] == 1, na.rm = TRUE)), integer(1))
  df2 <- df2[!(gi %in% names(bad)), , drop = FALSE]
  # only the levels that still have rows (geepack refuses unused levels), in
  # their order - the reference, which is never left out here, stays first
  if (is.factor(g))
    df2$GroupLabel <- factor(as.character(df2$GroupLabel),
                             levels = intersect(setdiff(levels(g), names(bad)),
                                                unique(as.character(df2$GroupLabel))))
  list(df = df2, skip = "", out = bad, n = n_g, events = ev_g)
}

# The distinct values recorded for an outcome - the values make_binary_01()
# and the summary table see.
.outcome_levels <- function(x) {
  v <- if (is.factor(x)) as.character(x) else x
  unique(v[!is.na(v)])
}

# Why a dichotomous outcome is not modelled when it does not have exactly two
# recorded values (contract R4-2): "outcome is constant in this cohort (only
# 'No' recorded)" / "outcome has 3 recorded values (a dichotomous outcome needs
# exactly two)". "" when it has two. analysed = TRUE: the values of the eyes
# with complete data for the model only ("only 'No' in the eyes with complete
# data").
.dicho_outcome_reason <- function(lv, analysed = FALSE) {
  if (length(lv) == 1)
    return(paste0("outcome is constant in this cohort (only '", as.character(lv[1]), "' ",
                  if (analysed) "in the eyes with complete data" else "recorded", ")"))
  if (length(lv) > 2)
    return(sprintf("outcome has %d recorded values (a dichotomous outcome needs exactly two)", length(lv)))
  ""
}

# The model note of a group with no usable row for this outcome (R4-2).
.no_data_text <- function(g) paste0("Not estimable - ", g, " has no data for this outcome")

# Model terms named by the user's headers (R4-3): a term of covariate `k`
# ("k", "`k`", or "k<level>" for a factor) gets the display name of `k` in
# place of the key - "<header>: <level>" for a factor level, so a renamed
# column never runs into its level; the group terms (GroupLabel<g>) and the
# intercept stay as they are, and so does every term whose key IS its display
# name. Longest keys first, so "Age_2" is never read as "Age" + "_2".
.display_terms <- function(term, keys, map) {
  if (is.null(map) || !length(map) || !length(keys) || !length(term)) return(term)
  out <- as.character(term)
  done <- is.na(out) | out == "(Intercept)" | startsWith(out, "GroupLabel")
  keys <- unique(as.character(keys))
  keys <- keys[order(-nchar(keys))]
  for (k in keys) {
    d <- disp_name(k, map)
    for (pre in c(paste0("`", k, "`"), k)) {
      hit <- !done & startsWith(out, pre)
      if (any(hit) && !identical(d, k)) {
        rest <- substring(out[hit], nchar(pre) + 1L)
        out[hit] <- ifelse(nzchar(rest), paste0(d, ": ", rest), d)
      }
      done <- done | hit
    }
  }
  out
}

# TRUE when matching weights are applied to the clustered models of `df` and
# are not all 1: those models are weighted fits with an INDEPENDENCE working
# correlation (contract R4-1, the MatchIt-recommended estimator). Every model
# of a matched cohort uses the independence working correlation too, weighted
# or not (decision D18: robust SE clustered by patient; matched sets are not
# clusters); the unmatched models keep the exchangeable one.
gee_weighted_independence <- function(df, use_weights = FALSE) {
  isTRUE(use_weights) && "weights" %in% names(df) && weights_nonunit(df$weights)
}
# The working correlation of those models, for the GEE sheets' note and the
# Manifest: "independence (weighted)", "independence (matched cohort)" or
# "exchangeable".
gee_corstr_label <- function(df, use_weights = FALSE, matched = FALSE) {
  if (gee_weighted_independence(df, use_weights)) "independence (weighted)"
  else if (isTRUE(matched)) "independence (matched cohort)"
  else "exchangeable"
}
# The output-note sentence of decision D18. It says "clustered by patient" only
# when a patient identifier is mapped; without one each eye is its own cluster
# (decision R2-7). The pipeline records the run's case once
# (set_matched_note_pid); the notes, sheets and legends read it.
.matched_note <- new.env(parent = emptyenv())
.matched_note$has_pid <- TRUE
set_matched_note_pid <- function(has_pid) .matched_note$has_pid <- isTRUE(has_pid)
matched_analysis_note <- function(clustered = .matched_note$has_pid) paste0(
  "Matched analyses use an independence working correlation with robust standard errors",
  if (isTRUE(clustered)) " clustered by patient" else ", each eye its own cluster (no patient ID mapped)",
  "; matched sets (pairs) are not used as clusters.")
MATCHED_ANALYSIS_NOTE <- matched_analysis_note(TRUE)

# The reference (baseline) level the model of a group factor uses.
.ref_level <- function(g) {
  if (is.factor(g)) levels(g)[1] else sort(unique(as.character(g[!is.na(g)])))[1]
}

# The groups that occur in `g`, in level (else sorted) order.
.groups_with_data <- function(g) {
  if (is.factor(g)) levels(droplevels(g)) else sort(unique(as.character(g[!is.na(g)])))
}

# Model-note text for the groups left out of one outcome's model.
.left_out_note <- function(out) {
  if (!length(out)) return("")
  paste0("Left out of this model: ", paste(names(out), collapse = ", "), " (",
         paste(out, collapse = "; "), "). ")
}

# Why one comparison is not estimable, for its own row.
.unestimable_text <- function(reason, binary) {
  paste0("Not estimable - ", reason, .unestimable_suffix(reason, binary))
}
.unestimable_suffix <- function(reason, binary) {
  if (grepl(" has 1 patient$", reason)) " (a group needs at least 2 patients for the robust standard error)"
  else if (grepl(" has 1 eye$", reason)) " (a group needs at least 2 eyes for the robust standard error)"
  else if (binary) " (complete separation; compare the raw counts in the summary table)"
  else " (a group needs at least 2 eyes)"
}

# Groups of the analysed rows with a single patient (cluster): no comparison
# involving them is estimated (one_patient_groups / cluster_guard_note,
# helpers.R - the one implementation of this guard). Unlike the
# groups .drop_unestimable_groups() leaves out, they STAY in the model, so
# every other comparison is estimated exactly as without this rule; only
# their own rows are reported as not estimable. Named character: group ->
# "<group> has 1 patient" ("<group> has 1 eye" when unit = "eye": no patient ID,
# each eye its own cluster).
.one_patient_groups <- function(df2, id_col, unit = "patient") {
  one_patient_groups(df2[[id_col]], df2$GroupLabel, unit)
}

# The few-patients caution of the comparison of group `g` with the reference
# `ref` in the analysed rows (few_patients_caution, helpers.R); "" when both
# groups hold GEE_FEW_PATIENTS patients (eyes, for unit = "eye") or more.
.pair_caution <- function(df2, id_col, ref, g, unit = "patient") {
  k <- !is.na(df2$GroupLabel) & as.character(df2$GroupLabel) %in% c(ref, g)
  few_patients_caution(df2[[id_col]][k],
                       factor(as.character(df2$GroupLabel[k]), levels = c(ref, g)), unit = unit)
}

# Two note texts as one ("A. B").
.join_note <- function(a, b) {
  if (!nzchar(a)) return(b)
  if (!nzchar(b)) return(a)
  paste0(sub("[. ]+$", "", a), ". ", b)
}

# One GEE-table row per comparison left out of the model (term GroupLabel<g>,
# no estimate), so build_gee_lookup() shows "<g>: not estimable".
.unestimable_rows <- function(out, outc, eff, binary, na_row) {
  if (!length(out)) return(NULL)
  dplyr::bind_rows(lapply(names(out), function(gn) {
    r <- na_row(outc, eff, .unestimable_text(out[[gn]], binary))
    r$term <- paste0("GroupLabel", gn)
    r
  }))
}

# Every term of a fitted GEE with its robust SE, test and 95% CI, in the run's
# small-sample inference (decision D11; helpers.R gee_vcov):
#   "md"   (default): Mancl-DeRouen SE; statistic = estimate / SE with a t
#          reference on df = patients - coefficients; CI = estimate +- t(.975,
#          df) SE;
#   "none": geepack's uncorrected SE, its Wald chi-square statistic and P, and
#          CI = estimate +- 1.96 SE (v1.0.0).
# The columns df (NA for "none") and Inference ("robust SE (Mancl-DeRouen), t
# reference" / "robust SE") name it on the GEE and regression sheets.
# unit: what a cluster is, for the reason below. When the corrected covariance
# cannot be computed, or the model has no more patients than coefficients
# (no t reference), the SE, P and CI are NA and attr "why" says why; the
# callers report the model as not estimable.
extract_gee_tidy <- function(fit, family_name = c("gaussian","binomial"),
                             outcome_name = NA_character_, unit = "patient") {
  family_name <- match.arg(family_name)
  mode <- gee_small_sample()
  why <- ""
  lone_attr <- character(0)
  if (identical(mode, "none")) {
    sm <- summary(fit)
    cf <- as.data.frame(sm$coefficients)
    cf$term <- rownames(cf); rownames(cf) <- NULL
    est_col  <- grep("Estimate", names(cf), value = TRUE)[1]
    se_col   <- grep("Std", names(cf), value = TRUE)[1]
    stat_col <- grep("Wald|Robust z|z value|t value", names(cf), value = TRUE)[1]
    p_col    <- grep("Pr\\(", names(cf), value = TRUE)[1]
    out <- tibble::tibble(
      Outcome = outcome_name, term = cf$term,
      estimate  = suppressWarnings(as.numeric(cf[[est_col]])),
      std.error = suppressWarnings(as.numeric(cf[[se_col]])),
      statistic = suppressWarnings(as.numeric(cf[[stat_col]])),
      p.value   = suppressWarnings(as.numeric(cf[[p_col]])),
      df = NA_real_)
  } else {
    b <- stats::coef(fit)
    out <- tibble::tibble(Outcome = outcome_name, term = names(b), estimate = unname(b),
                          std.error = NA_real_, statistic = NA_real_, p.value = NA_real_,
                          df = NA_real_)
    nd <- gee_df_note(length(unique(fit$id)), length(b), unit, mode)
    V <- if (is.null(nd)) gee_vcov(fit, mode) else NULL
    why <- if (!is.null(nd)) nd else if (is.null(V)) .gee_inf$err else ""
    if (!is.null(V)) {
      dfr <- gee_ref_df(fit)
      se <- sqrt(pmax(diag(V), 0))
      out$std.error <- unname(se)
      out$statistic <- unname(b / se)
      out$df <- dfr
      out$p.value <- 2 * stats::pt(-abs(out$statistic), dfr)
      # the term of a level held by one patient (leverage 1; helpers.R
      # gee_lone_terms) has a degenerate SE: no SE, P or CI; attr "lone" names
      # them for the callers' notes (a one-patient group's term is blanked by
      # the callers' own rule)
      lone <- intersect(gee_lone_terms(fit), out$term)
      if (length(lone)) {
        k <- out$term %in% lone
        out$std.error[k] <- NA_real_; out$statistic[k] <- NA_real_; out$p.value[k] <- NA_real_
        lone_attr <- lone
      }
    }
  }
  mult <- gee_ci_mult(out$df[1], mode)
  # CI: the effect as the tables print it (decision D24), "β = 30.93 (95% CI,
  # 29.95 to 31.91)" / "OR = 3.02 (95% CI, 1.21 to 7.57)", on the sheet under
  # the header "Effect (95% CI)"; the numeric columns stay for machine reading
  # (a covariate's per-unit effect keeps two significant digits: effect_digits)
  eff_txt <- function(kind, v, lo, hi, term) ifelse(is.finite(v), fmt_effect(kind, v, lo, hi,
                                                       effect_digits(term, kind, v, lo, hi)), NA_character_)
  if (family_name == "binomial") {
    out <- out %>% dplyr::mutate(
      Effect = "OR", EffectValue = exp(estimate),
      CI_low = exp(estimate - mult * std.error), CI_high = exp(estimate + mult * std.error),
      CI = eff_txt("OR", EffectValue, CI_low, CI_high, term))
  } else {
    out <- out %>% dplyr::mutate(
      Effect = "β", EffectValue = estimate,
      CI_low = estimate - mult * std.error, CI_high = estimate + mult * std.error,
      CI = eff_txt("beta", EffectValue, CI_low, CI_high, term))
  }
  out$Inference <- gee_se_label(1, mode)
  if (nzchar(why)) attr(out, "why") <- why
  if (length(lone_attr)) attr(out, "lone") <- lone_attr
  out
}

# The model note of a covariate term held by one patient (extract_gee_tidy
# attr "lone"): its estimate stays, its SE, P and CI are not reported.
GEE_LONE_NOTE <- "SE not estimable - this level is held by 1 patient"
.lone_note <- function(unit = "patient") {
  if (identical(unit, "eye")) sub("1 patient", "1 eye", GEE_LONE_NOTE, fixed = TRUE) else GEE_LONE_NOTE
}

# A variable flagged as a per-patient parameter as the outcome of a model
# (decision R3-4): never an eye-level GEE, whose working correlation would be
# near 1. One row per patient (per patient and group, as collapse_patient_level
# counts them): the outcome of the patient's first analysed eye (the value is
# the patient's), numeric covariates averaged over the patient's analysed eyes,
# any other covariate the first eye's, matching weights (wtd = TRUE) averaged;
# then an ordinary model - linear (model-based SE, t reference on the residual
# df) or logistic (model-based SE, Wald z reference). f: the model formula;
# df2: the complete-case eyes. Returns list(tidy = the rows in the GEE sheets'
# columns (extract_gee_tidy), n = patients, note = what was averaged, why = ""
# or why nothing was fitted).
.per_patient_model <- function(f, df2, preds, id_col, bin, wtd = FALSE, outcome_name = NA_character_) {
  grp <- if ("GroupLabel" %in% names(df2)) as.character(df2$GroupLabel) else ""
  k <- factor(paste(df2[[id_col]], grp, sep = "\r"))
  k <- factor(k, levels = unique(as.character(k)))
  pd <- df2[!duplicated(k), , drop = FALSE]
  avg <- character(0)
  for (p in setdiff(preds, "GroupLabel")) if (is.numeric(df2[[p]])) {
    pd[[p]] <- as.numeric(tapply(df2[[p]], k, mean))
    if (any(tapply(df2[[p]], k, function(z) length(unique(z)) > 1))) avg <- c(avg, p)
  }
  pd$.pp_w <- if (isTRUE(wtd) && "weights" %in% names(df2)) as.numeric(tapply(as.numeric(df2$weights), k, mean)) else 1
  n_par <- tryCatch(ncol(stats::model.matrix(f, pd)), error = function(e) NA_integer_)
  out <- list(tidy = NULL, n = nrow(pd), avg = avg, why = "")
  if (is.finite(n_par) && nrow(pd) - n_par < 1) {
    out$why <- sprintf("too few patients for this model (%d patients, %d coefficients)", nrow(pd), n_par)
    return(out)
  }
  fit <- tryCatch(suppressWarnings(
    if (bin) stats::glm(f, data = pd, family = stats::binomial(), weights = .pp_w)
    else stats::lm(f, data = pd, weights = .pp_w)), error = function(e) e)
  if (inherits(fit, "error")) { out$why <- paste0("the per-patient model failed: ", conditionMessage(fit)); return(out) }
  if (bin && !isTRUE(fit$converged)) { out$why <- "the per-patient logistic model did not converge"; return(out) }
  # glm stops at a relative deviance change of 1e-8 and takes its model-based SE
  # from the weights of that last step, up to ~1e-4 off those at the maximum:
  # a few more steps from its estimates make them the maximum's (kept only when
  # they converge)
  if (bin) {
    f2 <- tryCatch(suppressWarnings(stats::glm(f, data = pd, family = stats::binomial(), weights = .pp_w,
                                               start = stats::coef(fit),
                                               control = stats::glm.control(epsilon = 1e-12, maxit = 50))),
                   error = function(e) NULL)
    if (!is.null(f2) && isTRUE(f2$converged) && all(is.finite(stats::coef(f2)))) fit <- f2
  }
  cf <- summary(fit)$coefficients
  dfr <- if (bin) NA_real_ else fit$df.residual
  mult <- if (bin) stats::qnorm(0.975) else stats::qt(0.975, dfr)
  td <- tibble::tibble(Outcome = outcome_name, term = rownames(cf), estimate = unname(cf[, 1]),
                       std.error = unname(cf[, 2]), statistic = unname(cf[, 3]), p.value = unname(cf[, 4]),
                       df = dfr)
  # (a covariate's per-unit effect keeps two significant digits: effect_digits)
  eff_txt <- function(kind, v, lo, hi, term) ifelse(is.finite(v), fmt_effect(kind, v, lo, hi,
                                                       effect_digits(term, kind, v, lo, hi)), NA_character_)
  td <- if (bin) dplyr::mutate(td, Effect = "OR", EffectValue = exp(estimate),
                               CI_low = exp(estimate - mult * std.error), CI_high = exp(estimate + mult * std.error),
                               CI = eff_txt("OR", EffectValue, CI_low, CI_high, term))
        else dplyr::mutate(td, Effect = "β", EffectValue = estimate,
                           CI_low = estimate - mult * std.error, CI_high = estimate + mult * std.error,
                           CI = eff_txt("beta", EffectValue, CI_low, CI_high, term))
  td$Inference <- if (bin) "model-based SE, Wald z reference" else "model-based SE, t reference"
  out$tidy <- td
  out
}

# The Model text of a per-patient model (.per_patient_model)
.per_patient_label <- function(pp, bin, display_names = NULL)
  paste0(if (bin) "Logistic regression" else "Linear regression", ", ", GEE_PER_PATIENT, ", ", pp$n, " patients",
         if (length(pp$avg)) paste0("; ", paste(disp_name(pp$avg, display_names), collapse = ", "),
                                    " averaged over each patient's eyes") else "")

# Clustered GEE of each outcome on the predictors (the group factor and any
# adjustment covariates), one model per outcome. Every outcome either gets its
# model, or rows that say why not, one per comparison (term GroupLabel<g>), so
# the summary and Publication tables read "not estimable" instead of a blank:
#  - the groups and the reference are fixed from the WHOLE cohort before any
#    row is dropped, and the reference is never replaced silently (R4-2): when
#    it has no usable row for an outcome, every comparison reads "Skipped: not
#    estimable - <reference> has no data for this outcome";
#  - a non-reference group with no usable row gets its own row "Not estimable -
#    <group> has no data for this outcome"; the unused level is dropped before
#    fitting and the other comparisons are estimated;
#  - a dichotomous outcome with a single recorded value: "outcome is constant
#    in this cohort (only '<value>' recorded)";
#  - one minimum-data rule for every clustered model (model_data_guard,
#    helpers.R; R4-10): "too few eyes for this model (n = <n>)";
#  - a group of one patient, complete separation, a one-eye group: as before.
# use_weights: fit with the matching weights when the data carry them; weights
# that are not all 1 make the model a weighted fit with an INDEPENDENCE working
# correlation (R4-1, see gee_weighted_independence), otherwise exchangeable.
# matched = TRUE: a matched cohort - independence working correlation whether
# weighted or not, robust SE clustered by patient (decision D18).
# SE, P and CI take the run's small-sample inference (extract_gee_tidy; D11).
# display_names: optional clean key -> original header map (R4-3) for the
# Outcome and Covariates columns, the model notes and the covariate terms.
# Decision R3-4: an outcome in `patient_level` (the variables flagged as
# per-patient parameters) is never the outcome of an eye-level GEE when a
# patient ID is mapped - it is modeled on one row per patient by an ordinary
# linear or logistic model (.per_patient_model), labeled GEE_PER_PATIENT; an
# exchangeable fit is kept only when helpers.R gee_exch_reject() accepts it,
# otherwise the model is refitted with an independence working correlation and
# labeled GEE_INDEP_REFIT (logged with the reason), or reported as not
# estimable when that fails too.
run_gee_models <- function(df, outcomes_clean, outcome_type_map, predictors_clean,
                           patient_id_clean, dicho_pref_clean = list(),
                           use_weights = FALSE, logger = message, display_names = NULL,
                           matched = FALSE, patient_level = character(0)) {
  dn <- function(x) disp_name(x, display_names)
  cs <- .cluster_setup(df, patient_id_clean, logger)
  df <- cs$df
  id_col <- cs$id
  cl_note <- if (nzchar(cs$note)) paste0(" [", cs$note, "]") else ""
  # the groups of this cohort and its reference, BEFORE any row is dropped
  has_g <- "GroupLabel" %in% names(df)
  if (has_g && !is.factor(df$GroupLabel))
    df$GroupLabel <- factor(as.character(df$GroupLabel),
                            levels = sort(unique(as.character(df$GroupLabel[!is.na(df$GroupLabel)]))))
  ref_g <- if (has_g) .ref_level(df$GroupLabel) else NA_character_
  grp_all <- if (has_g) .groups_with_data(df$GroupLabel) else character(0)
  lev_all <- if (has_g) levels(df$GroupLabel) else character(0)
  w_ind <- gee_weighted_independence(df, use_weights)
  ind <- w_ind || isTRUE(matched)
  res_list <- list()
  # Covariates = the adjustment set actually in that row's model; Note = the
  # few-patients caution of a reported comparison (see helpers.R)
  na_row <- function(outc, eff, model) tibble::tibble(
    Outcome = dn(outc), term = NA_character_, estimate = NA_real_, std.error = NA_real_,
    statistic = NA_real_, p.value = NA_real_, df = NA_real_, Effect = eff, EffectValue = NA_real_,
    CI_low = NA_real_, CI_high = NA_real_, CI = NA_character_, Inference = NA_character_,
    Model = model, Covariates = NA_character_, Note = NA_character_)
  # one row per comparison `cmp` (term GroupLabel<g>), all with the same text
  cmp_rows <- function(outc, eff, why, cmp) {
    if (!length(cmp)) return(na_row(outc, eff, why))
    dplyr::bind_rows(lapply(cmp, function(gn) {
      r <- na_row(outc, eff, why); r$term <- paste0("GroupLabel", gn); r }))
  }
  # the comparisons whose group has no usable row: their own rows
  no_data_rows <- function(outc, eff, empty) {
    if (!length(empty)) return(NULL)
    dplyr::bind_rows(lapply(empty, function(gn) {
      r <- na_row(outc, eff, .no_data_text(gn)); r$term <- paste0("GroupLabel", gn); r }))
  }

  for (outc in outcomes_clean) {
    if (!(outc %in% names(df))) next
    out_type <- get_var_type(outc, outcome_type_map); if (is.na(out_type)) out_type <- "continuous"
    if (out_type %in% c("ordinal","categorical")) {
      # no effect estimate here; its table P comes from the clustered test of
      # its type (D12), said so rather than seeming to contradict the tables
      res_list[[outc]] <- na_row(outc, NA_character_,
        paste0("Skipped: ", out_type, " outcome not modeled in GEE (no effect estimate); its P value in the ",
               "summary tables comes from ",
               if (out_type == "ordinal") "a GEE on ranks, clustered by patient (the Wilcoxon / Kruskal-Wallis test with one eye per patient)"
               else "a joint clustered Wald test on its level indicators (the chi-squared / Fisher's exact test with one eye per patient)",
               " - see the Test column there")); next
    }
    preds <- predictors_clean[predictors_clean %in% names(df)]
    # a baseline variable that is also an adjustment covariate is never
    # adjusted for itself (R would drop the term with a warning)
    self_cov <- outc %in% preds
    preds <- setdiff(preds, outc)
    if (length(preds) == 0) next
    bin <- out_type == "dichotomous"
    eff <- if (bin) "OR" else "β"
    fam <- if (bin) "binomial" else "gaussian"
    grp_in <- has_g && "GroupLabel" %in% preds
    cmp_all <- if (grp_in) setdiff(grp_all, ref_g) else character(0)
    notes <- if (self_cov) "Not adjusted for itself (own value omitted from the covariates). " else character(0)
    # no comparison of this outcome is estimated: one row per comparison
    # (except the groups in `except`, which get their own rows)
    skip_all <- function(reason, except = character(0)) {
      why <- paste0(paste(notes, collapse = ""), "Skipped: not estimable - ", reason)
      if (grp_in) cmp_rows(outc, eff, why, setdiff(cmp_all, except)) else na_row(outc, eff, why)
    }

    df2 <- df
    if (bin) {
      # a 0/1 model needs exactly two recorded values (R4-2)
      why_lv <- .dicho_outcome_reason(.outcome_levels(df2[[outc]]))
      if (nzchar(why_lv)) { res_list[[outc]] <- skip_all(why_lv); next }
      df2[[outc]] <- make_binary_01(df2[[outc]], dicho_pref_clean[[outc]], logger, dn(outc))
    } else df2[[outc]] <- as_numeric_safe(df2[[outc]])
    # covariates read as text: numeric when they are numbers (see helpers.R)
    cv <- coerce_numeric_covs(df2, setdiff(preds, "GroupLabel"), logger, display_names = display_names)
    df2 <- cv$data
    if (length(cv$dropped)) {
      notes <- c(notes, paste0("Covariate(s) excluded (text with many distinct values): ",
                               paste(dn(cv$dropped), collapse = ", "), ". "))
      preds <- setdiff(preds, cv$dropped)
    }
    keep <- complete.cases(df2[, c(outc, preds, id_col), drop = FALSE]) & is.finite(df2[[outc]])
    df2 <- df2[keep, , drop = FALSE]

    # groups without a usable row for this outcome (R4-2): the reference is
    # never replaced; an empty non-reference group gets its own row, and its
    # unused level is dropped before fitting (geepack refuses unused levels)
    empty <- character(0)
    if (grp_in) {
      present <- intersect(grp_all, unique(as.character(df2$GroupLabel)))
      if (!(ref_g %in% present)) {
        res_list[[outc]] <- skip_all(paste0(ref_g, " has no data for this outcome")); next
      }
      empty <- setdiff(cmp_all, present)
      df2$GroupLabel <- factor(as.character(df2$GroupLabel), levels = intersect(lev_all, present))
      if (length(present) < 2) {
        res_list[[outc]] <- if (length(empty)) no_data_rows(outc, eff, empty)
                            else na_row(outc, eff, "Skipped: not estimable - only one group in this cohort")
        next
      }
    }
    empty_rows <- no_data_rows(outc, eff, empty)
    if (bin && length(unique(df2[[outc]])) < 2) {
      res_list[[outc]] <- dplyr::bind_rows(
        skip_all(.dicho_outcome_reason(.outcome_levels(df[[outc]][keep]), analysed = TRUE),
                 except = empty), empty_rows)
      next
    }

    # a predictor that is constant in the analysed rows breaks the model
    # matrix ("contrasts can be applied only to factors with 2 or more levels")
    is_const <- function(p) length(unique(as.character(df2[[p]]))) < 2
    # a comparison that cannot be estimated (a group with no events / only
    # events, or a single eye) is reported as such; the others are still fitted
    un <- list(out = character(0))
    one_pt <- character(0)
    if (grp_in) {
      un <- .drop_unestimable_groups(df2, outc, ref_g, bin)
      if (nzchar(un$skip)) {
        # no comparison of this outcome can be estimated: one row per
        # comparison (term GroupLabel<g>), so the summary and Publication
        # tables read "not estimable" instead of an empty GEE cell
        res_list[[outc]] <- dplyr::bind_rows(
          skip_all(paste0(un$skip, if (bin) " (complete separation; compare the raw counts in the summary table)"
                                   else " (a group needs at least 2 eyes)"), except = empty),
          empty_rows)
        next
      }
      if (length(un$out)) {
        df2 <- un$df
        notes <- c(notes, .left_out_note(un$out))
      }
      # a group of ONE patient: its comparison is not estimated (a reference of
      # one patient: none is); the group stays in the model
      one_pt <- .one_patient_groups(df2, id_col, cs$unit)
      if (ref_g %in% names(one_pt)) {
        res_list[[outc]] <- dplyr::bind_rows(
          skip_all(paste0(one_pt[[ref_g]], .unestimable_suffix(one_pt[[ref_g]], bin)),
                   except = c(empty, names(un$out))),
          .unestimable_rows(un$out, outc, eff, bin, na_row), empty_rows)
        next
      }
    }
    const <- preds[vapply(preds, is_const, logical(1))]
    if (length(const)) {
      notes <- c(notes, paste0("Covariate(s) dropped (constant in this cohort): ",
                               paste(dn(const), collapse = ", "), ". "))
      preds <- setdiff(preds, const)
    }
    if (bin) {
      sep <- .separation_levels(df2, outc, setdiff(preds, "GroupLabel"))
      sep_cov <- names(sep)
      if (length(sep_cov)) {
        notes <- c(notes, paste0("Covariate(s) dropped (a level has no events or only events): ",
                                 paste(dn(sep_cov), collapse = ", "), ". "))
        preds <- setdiff(preds, sep_cov)
      }
    }
    note <- paste(notes, collapse = "")
    left_out <- .unestimable_rows(un$out, outc, eff, bin, na_row)
    if (length(preds) == 0) {
      res_list[[outc]] <- dplyr::bind_rows(
        na_row(outc, eff, paste0(note, "Skipped: no non-constant predictors")), left_out, empty_rows)
      next
    }

    f <- as.formula(paste0("`", outc, "` ~ ", paste(sprintf("`%s`", preds), collapse = " + ")))
    # one minimum-data rule for every clustered model (R4-10): the eyes and
    # patients actually in this model, and its coefficients
    n_par <- tryCatch(ncol(stats::model.matrix(f, df2)), error = function(e) NA_integer_)
    small <- model_data_guard(nrow(df2), length(unique(df2[[id_col]])), n_par, cs$unit)
    if (!is.null(small)) {
      res_list[[outc]] <- dplyr::bind_rows(
        skip_all(small, except = c(empty, names(un$out))), left_out, empty_rows)
      next
    }
    # a per-patient variable: one row per patient, an ordinary model (R3-4)
    if (identical(cs$unit, "patient") && outc %in% patient_level) {
      pp <- .per_patient_model(f, df2, preds, id_col, bin, w_ind, dn(outc))
      log_once(logger, sprintf(paste0("GEE sheets: '%s' is a per-patient variable - modeled on one row per ",
                                      "patient by an ordinary %s model, never by an eye-level GEE."),
                               dn(outc), if (bin) "logistic" else "linear"))
      if (nzchar(pp$why)) {
        res_list[[outc]] <- dplyr::bind_rows(skip_all(pp$why, except = c(empty, names(un$out))),
                                             left_out, empty_rows)
        next
      }
      t <- pp$tidy
      model_txt <- .per_patient_label(pp, bin, display_names)
    } else {
      # an exchangeable fit only when it is accepted, else independence (R3-4)
      fc <- gee_fit_checked(function(cs_) fit_gee(f, df2, id_col, fam, use_weights, corstr = cs_),
                            if (ind) "independence" else "exchangeable")
      fit <- fc$fit
      if (is.null(fit)) {
        res_list[[outc]] <- dplyr::bind_rows(na_row(outc, eff, paste0(note, "Model failed: ",
          if (nzchar(fc$refit)) fc$why
          else if (nzchar(.gee_err$msg %||% "")) .gee_err$msg else "did not converge")), left_out, empty_rows)
        if (nzchar(fc$refit))
          logger(sprintf("GEE '%s' (covariates: %s): not estimable - %s.", dn(outc),
                         if (length(setdiff(preds, "GroupLabel"))) paste(dn(setdiff(preds, "GroupLabel")), collapse = ", ")
                         else "none", fc$why))
        next
      }
      if (nzchar(fc$refit))
        logger(sprintf("GEE '%s' (covariates: %s): exchangeable fit not accepted - %s; refitted with an %s.",
                       dn(outc), if (length(setdiff(preds, "GroupLabel"))) paste(dn(setdiff(preds, "GroupLabel")), collapse = ", ")
                                 else "none", fc$refit, GEE_INDEP_REFIT))
      t <- extract_gee_tidy(fit, fam, dn(outc), cs$unit)
      # no corrected covariance or no t reference (too few patients for the
      # coefficients): no comparison of this outcome is estimated, and every
      # row says why
      why_t <- attr(t, "why") %||% ""
      if (nzchar(why_t)) {
        res_list[[outc]] <- dplyr::bind_rows(skip_all(why_t, except = c(empty, names(un$out))),
                                             left_out, empty_rows)
        next
      }
      model_txt <- if (w_ind) paste0(if (bin) "GEE Binomial" else "GEE Gaussian",
                                     ", working correlation: independence (weighted)")
                   else if (ind) paste0(if (bin) "GEE Binomial" else "GEE Gaussian",
                                        ", working correlation: independence (matched cohort)")
                   else if (nzchar(fc$refit)) paste0(if (bin) "GEE Binomial" else "GEE Gaussian", ", ",
                                                     GEE_INDEP_REFIT, " - exchangeable fit not accepted: ", fc$refit)
                   else if (bin) "GEE Binomial (exchangeable)" else "GEE Gaussian (exchangeable)"
    }
    # covariate levels held by one patient: estimate only (extract_gee_tidy)
    lone <- setdiff(attr(t, "lone") %||% character(0), paste0("GroupLabel", names(one_pt)))
    t$Model <- paste0(note, model_txt, cl_note)
    adj <- setdiff(preds, "GroupLabel")
    t$Covariates <- if (length(adj)) paste(dn(adj), collapse = ", ") else "none"
    if (bin) {
      # backstop for quasi-separation the level checks cannot see: a group or
      # categorical-covariate OR beyond 1e-4..1e4 is not a usable estimate. A
      # continuous covariate's OR is per unit of its own scale, so a large one
      # can be genuine and is never blanked for its size (contract R3-6).
      # Any term whose OR or CI is not a finite positive number (exp() overflows
      # beyond |log OR| ~ 709: a diverged fit) cannot be reported either.
      num_p <- adj[vapply(adj, function(p) is.numeric(df2[[p]]), logical(1))]
      big <- abs(t$estimate) > log(1e4) & !(t$term %in% c(num_p, paste0("`", num_p, "`")))
      unrep <- !is.finite(t$EffectValue) | !is.finite(t$CI_low) | !is.finite(t$CI_high) |
        t$EffectValue <= 0 | t$CI_low <= 0
      # (a level held by one patient has no SE by design: only its size counts)
      bad <- !is.na(t$term) & t$term != "(Intercept)" &
        (big | (!(t$term %in% lone) & (!is.finite(t$estimate) | !is.finite(t$std.error) | unrep)))
      bad[is.na(bad)] <- FALSE
      if (any(bad)) {
        t$statistic[bad] <- NA_real_; t$p.value[bad] <- NA_real_
        t$EffectValue[bad] <- NA_real_; t$CI_low[bad] <- NA_real_; t$CI_high[bad] <- NA_real_
        t$CI[bad] <- NA_character_
        t$Model[bad] <- paste0(note, "Skipped: unstable estimate (quasi-separation) - effect not reported")
      }
    }
    t$Note <- NA_character_
    k_lone <- !is.na(t$term) & t$term %in% lone & is.finite(t$EffectValue)
    if (any(k_lone)) t$Model[k_lone] <- paste0(t$Model[k_lone], ". ", .lone_note(cs$unit))
    if ("GroupLabel" %in% preds) {
      # the comparison with a one-patient group is not reported (the other
      # terms of the same fit are, unchanged)
      for (gn in names(one_pt)) {
        k <- !is.na(t$term) & t$term == paste0("GroupLabel", gn)
        if (!any(k)) next
        t$estimate[k] <- NA_real_; t$std.error[k] <- NA_real_; t$statistic[k] <- NA_real_
        t$p.value[k] <- NA_real_; t$EffectValue[k] <- NA_real_
        t$CI_low[k] <- NA_real_; t$CI_high[k] <- NA_real_; t$CI[k] <- NA_character_
        t$Model[k] <- paste0(note, .unestimable_text(one_pt[[gn]], bin))
      }
      # a reported comparison involving a group of few patients carries the caution
      for (gn in setdiff(.groups_with_data(df2$GroupLabel), c(ref_g, names(one_pt)))) {
        k <- !is.na(t$term) & t$term == paste0("GroupLabel", gn) & is.finite(t$p.value)
        few <- if (any(k)) .pair_caution(df2, id_col, ref_g, gn, cs$unit) else ""
        if (nzchar(few)) t$Note[k] <- few
      }
    }
    # covariate terms named by the user's headers (the group terms keep the
    # GroupLabel<g> form the table merges look up)
    t$term <- .display_terms(t$term, adj, display_names)
    if (!is.null(left_out)) left_out$Covariates <- t$Covariates[1]
    if (!is.null(empty_rows)) empty_rows$Covariates <- t$Covariates[1]
    res_list[[outc]] <- dplyr::bind_rows(t, left_out, empty_rows)
  }
  dplyr::bind_rows(res_list)
}

.gee_err <- new.env(parent = emptyenv())

# corstr: the working correlation - "exchangeable" (default: unmatched models)
# or "independence" (every model of a matched cohort, D18, and models weighted
# by matching weights that are not all 1, R4-1). The fit's console output never
# reaches stdout (quiet_eval); a failure's message, with what geepack printed,
# is left in .gee_err$msg on one line.
fit_gee <- function(f, df2, patient_id_clean, family_name, use_weights, corstr = "exchangeable") {
  .gee_err$msg <- ""
  fam <- if (family_name == "binomial") binomial else gaussian
  ord <- order(df2[[patient_id_clean]])
  df2 <- df2[ord, , drop = FALSE]
  # geepack needs a NUMERIC id: a character id (e.g. "PT0001") gets coerced to
  # NA internally, silently destroying the clustering and the robust SEs.
  # It also resolves `id`/`weights` inside the FORMULA's environment, so they
  # must live in `data` itself to be found reliably.
  idv <- df2[[patient_id_clean]]
  df2$.gee_id <- as.integer(factor(idv, levels = unique(idv)))
  environment(f) <- environment()
  q <- quiet_eval({
    if (use_weights && "weights" %in% names(df2)) {
      df2$.gee_w <- as.numeric(df2$weights)
      # non-integer matching weights on a 0/1 outcome are expected (the GEE is
      # a weighted quasi-likelihood fit); glm's start-value warning is noise
      withCallingHandlers(
        geepack::geeglm(formula = f, data = df2, id = .gee_id,
                        family = fam, corstr = corstr, weights = .gee_w),
        warning = function(w) if (grepl("non-integer #successes", conditionMessage(w)))
          invokeRestart("muffleWarning"))
    } else {
      geepack::geeglm(formula = f, data = df2, id = .gee_id,
                      family = fam, corstr = corstr)
    }
  })
  if (is.null(q$value)) .gee_err$msg <- quiet_error_text(q)
  q$value
}

# Effect + p label per outcome for the group term(s). group_term may name
# several terms (e.g. c("GroupLabelB", "GroupLabelC") with > 2 groups): each
# outcome then gets one line per comparison, prefixed with the group name, and
# its P values are Holm-adjusted across that outcome's comparisons with the
# reference and tagged "[Holm]" (decision D14), as on the figures; an outcome
# with a single estimable comparison prints it as is. The GEE sheets keep the
# nominal P.
build_gee_lookup <- function(gee_df, group_term) {
  empty <- tibble::tibble(Outcome = character(0), gee_effect_label = character(0),
                          gee_p_label = character(0))
  if (is.null(gee_df) || nrow(gee_df) == 0 || !("term" %in% names(gee_df))) return(empty)
  rows <- gee_df %>%
    dplyr::filter(!is.na(Outcome), !is.na(term), term %in% group_term)
  p_shown <- rows$p.value
  holm <- rep(FALSE, nrow(rows))
  if (length(group_term) > 1) for (oc in unique(rows$Outcome)) {
    k <- which(rows$Outcome == oc & is.finite(rows$p.value) & !is.na(rows$EffectValue))
    if (length(k) >= 2) {
      p_shown[k] <- stats::p.adjust(rows$p.value[k], method = "holm")
      holm[k] <- TRUE
    }
  }
  # a model that is not the exchangeable GEE of the column is named in its
  # cell (decision R3-4): a per-patient variable's ordinary model, or an
  # independence refit
  mdl <- if ("Model" %in% names(rows)) as.character(rows$Model) else rep("", nrow(rows))
  mdl[is.na(mdl)] <- ""
  tag <- ifelse(grepl(GEE_PER_PATIENT, mdl, fixed = TRUE), " [one row per patient, ordinary model]",
                ifelse(grepl(GEE_INDEP_REFIT, mdl, fixed = TRUE), " [independence working correlation]", ""))
  rows <- rows %>%
    dplyr::mutate(
      # one effect style for every table (D24): "β = -9.98 (95% CI, -29.04
      # to 9.08)" / "OR = 3.02 (95% CI, 1.21 to 7.57)"
      gee_effect_label = dplyr::case_when(
        is.na(EffectValue) ~ "not estimable",
        Effect == "OR" ~ fmt_effect("OR", EffectValue, CI_low, CI_high),
        TRUE ~ fmt_effect("beta", EffectValue, CI_low, CI_high)),
      gee_p_label = paste0(dplyr::coalesce(fmt_p(p_shown), ""), ifelse(holm, " [Holm]", ""),
                           ifelse(is.na(EffectValue), "", tag)))
  if (length(group_term) > 1 && nrow(rows) > 0) {
    rows <- rows %>%
      dplyr::mutate(.cmp = sub("^GroupLabel", "", term), .ord = match(term, group_term)) %>%
      dplyr::arrange(match(Outcome, unique(Outcome)), .ord) %>%
      dplyr::group_by(Outcome) %>%
      dplyr::summarise(gee_effect_label = paste0(.cmp, ": ", gee_effect_label, collapse = "\n"),
                       gee_p_label = paste0(.cmp, ": ", gee_p_label, collapse = "\n"),
                       .groups = "drop")
  }
  rows %>% dplyr::select(Outcome, gee_effect_label, gee_p_label)
}

# Summary-table rows that belong to a variable: "X", "X (level)", "X: level",
# and "X [per patient]" / "X (level) [per patient]".
.rows_of <- function(param_labels, var) {
  which(param_labels == var |
          startsWith(param_labels, paste0(var, " (")) |
          startsWith(param_labels, paste0(var, ": ")) |
          startsWith(param_labels, paste0(var, " [")))
}

# The rows of variable `var` (its display label - or its clean key, when the
# models were run without the display map) in a table from
# create_summary_table(): exactly, through the table's "row_var" / "row_key"
# attributes, so a header such as "K1" never claims the rows of "K1 (flat)";
# by label (.rows_of) when the attributes are gone.
.rows_for <- function(tbl, var) {
  n <- nrow(tbl)
  rv <- attr(tbl, "row_var"); rk <- attr(tbl, "row_key")
  if (length(rv) == n && length(rk) == n && n > 0) {
    hit <- which(rv == var)
    if (!length(hit)) hit <- which(rk == var)
    return(hit)
  }
  .rows_of(tbl$Parameter, var)
}

merge_gee_into_summary_table <- function(summary_tbl, gee_lookup) {
  if (is.null(summary_tbl) || nrow(summary_tbl) == 0) return(summary_tbl)
  out <- summary_tbl
  if (!("GEE effect (95% CI)" %in% names(out))) out[["GEE effect (95% CI)"]] <- ""
  if (!("GEE p value" %in% names(out))) out[["GEE p value"]] <- ""
  if (is.null(gee_lookup) || nrow(gee_lookup) == 0) return(out)
  for (i in seq_len(nrow(gee_lookup))) {
    outcome_i <- gee_lookup$Outcome[i]
    hit <- .rows_for(out, outcome_i)
    if (length(hit) > 0) {
      out[hit, "GEE effect (95% CI)"] <- gee_lookup$gee_effect_label[i]
      out[hit, "GEE p value"]         <- gee_lookup$gee_p_label[i]
    }
  }
  out
}

# ------------------------------------------------------------
# Convenient multivariable regression (GEE-based so eyes stay
# clustered by patient). One model per outcome:
#   outcome ~ GroupLabel (factor) + covariates
# Continuous outcome -> linear GEE (beta); dichotomous -> logistic
# GEE (OR). Ordinal/categorical outcomes are skipped with a note.
# SE, P and CI take the run's small-sample inference (extract_gee_tidy; D11).
# Every adjusted comparison also gets the crude (unadjusted) GEE estimate
# fitted on the SAME complete-case eyes (decision D17: "Crude effect, same
# eyes (95% CI)", "Crude P, same eyes"), so the crude-versus-adjusted contrast
# shows confounding, not case selection; "n" is the adjusted model's eyes.
# With more than two groups the column "P value (Holm)" adjusts each outcome's
# comparisons with the reference (decision D14; blank when fewer than two of
# them are estimable); "P value" stays nominal. The crude P values get their
# own Holm column, "Crude P (Holm), same eyes", on the same footing.
# matched = TRUE: a matched cohort - independence working correlation (D18).
# Returns list(pub = publish-ready table, full = all model terms,
#              n_flagged = # models with a small-sample warning).
# ------------------------------------------------------------
REG_CRUDE_EFF <- "Crude effect, same eyes (95% CI)"
REG_CRUDE_P <- "Crude P, same eyes"
REG_HOLM_P <- "P value (Holm)"
REG_CRUDE_HOLM_P <- "Crude P (Holm), same eyes"
run_regression_models <- function(df, outcomes_clean, outcome_type_map, covs_clean,
                                  patient_id_clean, dicho_pref_clean = list(),
                                  glevels, use_weights = FALSE, logger = message,
                                  display_names = NULL, matched = FALSE,
                                  patient_level = character(0)) {
  dn <- function(x) disp_name(x, display_names)
  ref <- glevels[1]
  pub <- list(); full <- list(); n_flagged <- 0
  cs <- .cluster_setup(df, patient_id_clean, logger)
  df <- cs$df
  id_col <- cs$id
  cl_note <- if (nzchar(cs$note)) paste0("Note: ", cs$note, ". ") else ""
  # the compared groups of this cohort, fixed BEFORE any row is dropped (the
  # reference, glevels[1], is never replaced silently: R4-2)
  grp_present <- unique(as.character(df$GroupLabel[!is.na(df$GroupLabel)]))
  lev_all <- unique(c(as.character(glevels), sort(grp_present)))
  cmp_all <- setdiff(intersect(lev_all, grp_present), ref)
  w_ind <- gee_weighted_independence(df, use_weights)
  ind <- w_ind || isTRUE(matched)
  multi_cmp <- length(cmp_all) > 1
  pub_cols <- c("Outcome", "Comparison", "Adjusted effect (95% CI)", "P value",
                if (multi_cmp) REG_HOLM_P, REG_CRUDE_EFF, REG_CRUDE_P,
                if (multi_cmp) REG_CRUDE_HOLM_P, "n", "Note")
  skip_row <- function(outc, n, why) data.frame(
    Outcome = dn(outc), Comparison = "", `Adjusted effect (95% CI)` = "", `P value` = "",
    n = as.character(n), Note = why, check.names = FALSE)
  # one "not estimable" row per comparison, so the merged summary and
  # Publication tables say so instead of leaving the cells empty
  cmp_rows <- function(outc, cmp, n_txt, why) data.frame(
    Outcome = dn(outc), Comparison = paste(cmp, "vs", ref),
    `Adjusted effect (95% CI)` = "not estimable", `P value` = "",
    n = n_txt, Note = why, check.names = FALSE, row.names = NULL)
  # the rows of one outcome: a row for the whole outcome first, then one per
  # comparison in the order of the groups; Holm across the outcome's estimable
  # comparisons (the nominal P of each is in its hidden .p_num; the crude one's
  # in .cp_num, adjusted the same way)
  add_rows <- function(rows) {
    rows <- Filter(function(r) !is.null(r) && nrow(r) > 0, rows)
    if (!length(rows)) return(invisible(NULL))
    r <- dplyr::bind_rows(rows)
    o <- match(r$Comparison, paste(cmp_all, "vs", ref))
    r <- r[order(!is.na(o), o), , drop = FALSE]
    rownames(r) <- NULL
    num <- function(col) if (col %in% names(r)) suppressWarnings(as.numeric(r[[col]])) else rep(NA_real_, nrow(r))
    if (multi_cmp) {
      for (hp in list(c(".p_num", REG_HOLM_P), c(".cp_num", REG_CRUDE_HOLM_P))) {
        pn <- num(hp[1])
        ok <- is.finite(pn)
        r[[hp[2]]] <- ""
        if (sum(ok) >= 2) r[[hp[2]]][ok] <- fmt_p(stats::p.adjust(pn[ok], method = "holm"))
      }
    }
    r$.p_num <- NULL
    r$.cp_num <- NULL
    pub[[length(pub) + 1]] <<- r
    invisible(NULL)
  }

  for (outc in outcomes_clean) {
    if (!(outc %in% names(df))) next
    ot <- get_var_type(outc, outcome_type_map); if (is.na(ot)) ot <- "continuous"
    if (ot %in% c("ordinal", "categorical")) {
      pub[[length(pub) + 1]] <- data.frame(
        Outcome = dn(outc), Comparison = "", `Adjusted effect (95% CI)` = "",
        `P value` = "", n = NA_character_, Note = paste0("Skipped: ", ot, " outcome"),
        check.names = FALSE)
      next
    }
    dich <- ot == "dichotomous"
    covs <- covs_clean[covs_clean %in% names(df)]
    # an outcome is never adjusted for itself (R would silently drop the term)
    self_note <- if (outc %in% covs) "Not adjusted for itself (own value omitted from the covariates). " else ""
    covs <- setdiff(covs, outc)
    # no comparison of this outcome is estimated: one row per comparison
    # (except the groups in `except`, which get their own rows)
    whole <- function(why, n_txt, except = character(0)) {
      cmp <- setdiff(cmp_all, except)
      if (!length(cmp)) skip_row(outc, n_txt, why) else cmp_rows(outc, cmp, n_txt, why)
    }
    df2 <- df
    if (dich) {
      # a 0/1 model needs exactly two recorded values (R4-2): "outcome is
      # constant in this cohort", never "only one group has data"
      lv <- .outcome_levels(df2[[outc]])
      why_lv <- .dicho_outcome_reason(lv)
      if (nzchar(why_lv)) {
        add_rows(list(whole(paste0(cl_note, self_note, "Skipped: not estimable - ", why_lv),
                            as.character(sum(!is.na(df2[[outc]]))))))
        next
      }
      df2[[outc]] <- make_binary_01(df2[[outc]], dicho_pref_clean[[outc]], logger, dn(outc))
    } else df2[[outc]] <- as_numeric_safe(df2[[outc]])
    # covariates read as text become numeric when they are numbers, instead of
    # one dummy per distinct value (see coerce_numeric_covs in helpers.R)
    cv <- coerce_numeric_covs(df2, covs, logger, display_names = display_names)
    df2 <- cv$data
    covs <- setdiff(covs, cv$dropped)
    excl_note <- paste0(self_note, if (length(cv$dropped))
      paste0("Covariate(s) excluded (text with many distinct values): ",
             paste(dn(cv$dropped), collapse = ", "), ". ") else "")
    keep <- complete.cases(df2[, c(outc, "GroupLabel", covs, id_col), drop = FALSE]) &
      is.finite(df2[[outc]])
    df2 <- df2[keep, , drop = FALSE]
    n_txt0 <- function() if (dich)
      sprintf("%d (%d events)", nrow(df2), as.integer(sum(df2[[outc]] == 1, na.rm = TRUE)))
      else as.character(nrow(df2))

    # groups without a usable row for this outcome (R4-2): the reference is
    # never replaced; an empty group gets its own row and its unused level is
    # dropped before fitting (geepack refuses unused levels)
    present <- intersect(lev_all, unique(as.character(df2$GroupLabel)))
    if (!(ref %in% present)) {
      add_rows(list(whole(paste0(cl_note, excl_note, "Skipped: not estimable - ", ref,
                                 " has no data for this outcome"), n_txt0())))
      next
    }
    empty <- setdiff(cmp_all, present)
    empty_rows <- if (length(empty)) data.frame(
      Outcome = dn(outc), Comparison = paste(empty, "vs", ref),
      `Adjusted effect (95% CI)` = "not estimable", `P value` = "",
      n = if (dich) "0 (0 events)" else "0", Note = .no_data_text(empty),
      check.names = FALSE, row.names = NULL) else NULL
    df2$GroupLabel <- factor(as.character(df2$GroupLabel), levels = present)
    if (length(present) < 2) {
      add_rows(list(if (is.null(empty_rows))
        skip_row(outc, n_txt0(), "Skipped: not estimable - only one group in this cohort") else empty_rows))
      next
    }
    if (dich && length(unique(df2[[outc]])) < 2) {
      add_rows(list(whole(paste0(cl_note, excl_note, "Skipped: not estimable - ",
                                 .dicho_outcome_reason(.outcome_levels(df[[outc]][keep]), analysed = TRUE)),
                          n_txt0(), except = empty), empty_rows))
      next
    }
    # drop covariates that are constant in this cohort (they crash the model
    # matrix — the usual cause of every model "failing" in small samples)
    dropped <- covs[vapply(covs, function(cv)
      length(unique(na.omit(as.character(df2[[cv]])))) < 2, logical(1))]
    covs <- setdiff(covs, dropped)
    drop_note <- paste0(cl_note, excl_note, if (length(dropped))
      paste0("Covariate(s) dropped (constant in this cohort): ",
             paste(dn(dropped), collapse = ", "), ". ") else "")
    # a comparison that cannot be estimated (a group with no events / only
    # events, or a single eye) gets its own "not estimable" row; the others
    # are still estimated (see .drop_unestimable_groups)
    un <- .drop_unestimable_groups(df2, outc, ref, dich)
    if (nzchar(un$skip)) {
      why <- paste0(drop_note, "Skipped: not estimable - ", un$skip,
                    if (dich) " (complete separation; compare the raw counts in the summary table)"
                    else " (a group needs at least 2 eyes)")
      add_rows(list(whole(why, n_txt0(), except = empty), empty_rows))
      next
    }
    if (length(un$out)) {
      df2 <- un$df
      drop_note <- paste0(drop_note, .left_out_note(un$out))
      more <- covs[vapply(covs, function(cv)
        length(unique(na.omit(as.character(df2[[cv]])))) < 2, logical(1))]
      if (length(more)) {
        covs <- setdiff(covs, more)
        drop_note <- paste0(drop_note, "Covariate(s) dropped (constant in this cohort): ",
                            paste(dn(more), collapse = ", "), ". ")
      }
    }
    unest_rows <- function() if (length(un$out)) data.frame(
      Outcome = dn(outc), Comparison = paste(names(un$out), "vs", ref),
      `Adjusted effect (95% CI)` = "not estimable", `P value` = "",
      n = if (dich) sprintf("%d (%d events)", un$n, un$events) else as.character(un$n),
      Note = vapply(un$out, .unestimable_text, character(1), binary = dich),
      check.names = FALSE, row.names = NULL)
    # a group of ONE patient: its comparison is not estimated (a reference of
    # one patient: none is); the group stays in the model
    one_pt <- .one_patient_groups(df2, id_col, cs$unit)
    if (ref %in% names(one_pt)) {
      why <- paste0(drop_note, "Skipped: not estimable - ", one_pt[[ref]],
                    .unestimable_suffix(one_pt[[ref]], dich))
      add_rows(list(whole(why, n_txt0(), except = c(empty, names(un$out))), unest_rows(), empty_rows))
      next
    }
    if (dich) {
      sep <- .separation_levels(df2, outc, covs)
      sep_cov <- names(sep)
      if (length(sep_cov)) {
        covs <- setdiff(covs, sep_cov)
        drop_note <- paste0(drop_note, "Covariate(s) dropped (a level has no events or only events): ",
                            paste(dn(sep_cov), collapse = ", "), ". ")
      }
    }
    preds <- c("GroupLabel", covs)
    df2 <- df2 %>% dplyr::mutate(dplyr::across(dplyr::where(is.character) &
                                                 !dplyr::any_of(id_col), as.factor))
    n <- nrow(df2)
    fmla <- as.formula(paste0("`", outc, "` ~ ", paste(sprintf("`%s`", preds), collapse = " + ")))
    n_par <- tryCatch(ncol(stats::model.matrix(fmla, df2)) - 1, error = function(e) NA)
    # n_events = eyes WITH the event (printed in the n column); epv_n = the
    # smaller outcome class, which is what limits a logistic model (EPV check)
    n_events <- if (dich) sum(df2[[outc]] == 1, na.rm = TRUE) else NA
    epv_n <- if (dich) min(n_events, sum(df2[[outc]] == 0, na.rm = TRUE)) else NA
    n_txt <- if (dich) sprintf("%d (%d events)", n, n_events) else as.character(n)

    # one minimum-data rule for every clustered model (R4-10; n_par excludes
    # the intercept, the guard counts it). A model that is skipped never
    # counts as "flagged".
    small <- model_data_guard(n, length(unique(df2[[id_col]])), n_par + 1, cs$unit)
    if (!is.null(small)) {
      add_rows(list(whole(paste0(drop_note, "Skipped: not estimable - ", small), n_txt,
                          except = c(empty, names(un$out))), unest_rows(), empty_rows))
      next
    }

    caution <- ""
    if (dich && is.finite(epv_n) && is.finite(n_par) && epv_n < 10 * n_par) {
      caution <- sprintf("CAUTION: only %d eyes in the smaller outcome class for %d parameters (<10 per parameter) - possible overfitting",
                         epv_n, n_par)
    } else if (ot == "continuous" && is.finite(n_par) && n < 10 * n_par) {
      caution <- sprintf("CAUTION: n=%d for %d parameters (<10 per parameter)", n, n_par)
    }
    note <- paste0(drop_note, caution)

    fam_r <- if (dich) "binomial" else "gaussian"
    cstr <- if (ind) "independence" else "exchangeable"
    # decision R3-4: a per-patient variable is modeled on one row per patient
    # by an ordinary model; an exchangeable GEE is kept only when it is
    # accepted (helpers.R gee_fit_checked), else refitted with independence
    pp <- NULL; fc <- list(refit = "", why = "")
    if (identical(cs$unit, "patient") && outc %in% patient_level) {
      pp <- .per_patient_model(fmla, df2, preds, id_col, dich, w_ind, dn(outc))
      fit <- if (nzchar(pp$why)) NULL else pp
      n_txt <- sprintf("%d patients (%d eyes)", pp$n, n)
    } else {
      fc <- gee_fit_checked(function(cs_) fit_gee(fmla, df2, id_col, fam_r, use_weights, corstr = cs_), cstr)
      fit <- fc$fit
      if (nzchar(fc$refit))
        logger(sprintf("Regression '%s': exchangeable fit not accepted - %s; %s.", dn(outc), fc$refit,
                       if (is.null(fit)) paste0("not estimable (", fc$why, ")") else paste0("refitted with an ", GEE_INDEP_REFIT)))
    }
    # one count per model, whether it carries a caution, failed, or both
    if (nzchar(caution) || is.null(fit)) n_flagged <- n_flagged + 1
    if (is.null(fit)) {
      add_rows(list(skip_row(outc, n, paste0(
        drop_note, if (nzchar(caution)) paste0(caution, ". ") else "", "Model failed: ",
        if (!is.null(pp)) pp$why else if (nzchar(fc$refit)) fc$why
        else ifelse(nzchar(.gee_err$msg %||% ""), .gee_err$msg, "did not converge"))),
        unest_rows(), empty_rows))
      next
    }
    m_note <- if (!is.null(pp)) .per_patient_label(pp, dich, display_names)
              else if (nzchar(fc$refit)) paste0(GEE_INDEP_REFIT, " - exchangeable fit not accepted: ", fc$refit)
              else ""
    note <- .join_note(note, m_note)
    td <- if (!is.null(pp)) pp$tidy else extract_gee_tidy(fit, fam_r, dn(outc), cs$unit)
    # no corrected covariance, or too few patients for the t reference: the
    # model is fitted but none of its comparisons can be tested (D11)
    why_t <- attr(td, "why") %||% ""
    if (nzchar(why_t)) {
      add_rows(list(whole(paste0(drop_note, "Skipped: not estimable - ", why_t), n_txt,
                          except = c(empty, names(un$out))), unest_rows(), empty_rows))
      next
    }
    td$Model <- if (!is.null(pp)) m_note
                else paste0(if (dich) "Logistic GEE (OR)" else "Linear GEE (β)",
                            if (w_ind) ", working correlation: independence (weighted)"
                            else if (ind) ", working correlation: independence (matched cohort)"
                            else if (nzchar(fc$refit)) paste0(", ", m_note)
                            # an accepted exchangeable fit is named too (R3-4, T1-04)
                            else ", working correlation: exchangeable")
    # covariate levels held by one patient: estimate only (extract_gee_tidy)
    k_lone <- !is.na(td$term) & td$term %in% setdiff(attr(td, "lone") %||% character(0),
                                                     paste0("GroupLabel", names(one_pt)))
    if (any(k_lone)) td$Model[k_lone] <- paste0(td$Model[k_lone], ". ", .lone_note(cs$unit))
    # the crude comparison on the SAME eyes (decision D17): the group alone, on
    # the adjusted model's complete cases, with its weights and working
    # correlation (independence when the adjusted model was refitted; its own
    # acceptance check otherwise, R3-4); per patient for a per-patient
    # variable; NULL when it cannot be fitted or tested
    tcr <- NULL
    fml_c <- as.formula(paste0("`", outc, "` ~ GroupLabel"))
    if (!is.null(pp)) {
      pc <- .per_patient_model(fml_c, df2, "GroupLabel", id_col, dich, w_ind, dn(outc))
      if (!nzchar(pc$why)) tcr <- pc$tidy
    } else {
      fcc <- gee_fit_checked(function(cs_) fit_gee(fml_c, df2, id_col, fam_r, use_weights, corstr = cs_),
                             if (nzchar(fc$refit)) "independence" else cstr)
      fit_c <- fcc$fit
      if (nzchar(fcc$refit) && !is.null(fit_c))
        note <- .join_note(note, paste0("crude model: ", GEE_INDEP_REFIT))
      if (!is.null(fit_c)) {
        tcr <- extract_gee_tidy(fit_c, fam_r, dn(outc), cs$unit)
        if (nzchar(attr(tcr, "why") %||% "")) tcr <- NULL
      }
    }
    # the term of a one-patient group is not reported (the other terms are)
    for (gn in names(one_pt)) {
      k <- !is.na(td$term) & td$term == paste0("GroupLabel", gn)
      td$estimate[k] <- NA_real_; td$std.error[k] <- NA_real_; td$statistic[k] <- NA_real_
      td$p.value[k] <- NA_real_; td$EffectValue[k] <- NA_real_
      td$CI_low[k] <- NA_real_; td$CI_high[k] <- NA_real_; td$CI[k] <- NA_character_
      td$Model[k] <- .unestimable_text(one_pt[[gn]], dich)
    }
    tfull <- td
    tfull$term <- .display_terms(tfull$term, covs, display_names)
    full[[length(full) + 1]] <- tfull

    ur <- unest_rows()
    few_flag <- FALSE
    rows <- list()
    for (g in cmp_all) {
      if (g %in% empty) {
        rows[[length(rows) + 1]] <- empty_rows[empty == g, , drop = FALSE]
        next
      }
      if (g %in% names(un$out)) {
        rows[[length(rows) + 1]] <- ur[names(un$out) == g, , drop = FALSE]
        next
      }
      row <- td[td$term == paste0("GroupLabel", g), , drop = FALSE]
      if (nrow(row) == 0) next
      if (g %in% names(one_pt)) {
        rows[[length(rows) + 1]] <- data.frame(
          Outcome = dn(outc), Comparison = paste(g, "vs", ref),
          `Adjusted effect (95% CI)` = "not estimable", `P value` = "",
          n = n_txt, Note = .join_note(note, .unestimable_text(one_pt[[g]], dich)), check.names = FALSE)
        next
      }
      # the same backstop as the GEE tables: a group OR beyond 1e-4..1e4
      # (quasi-separation the level check cannot see) is not a usable estimate
      unstable <- dich && (!is.finite(row$estimate[1]) || !is.finite(row$std.error[1]) ||
                             abs(row$estimate[1]) > log(1e4))
      eff <- if (unstable) "not estimable"
             else fmt_effect(if (dich) "OR" else "beta", row$EffectValue[1], row$CI_low[1], row$CI_high[1])
      # a reported comparison involving a group of few patients carries the caution
      few <- if (unstable || !is.finite(row$p.value[1])) "" else .pair_caution(df2, id_col, ref, g, cs$unit)
      if (nzchar(few)) few_flag <- TRUE
      # the crude estimate on the same eyes, with the same backstop
      crow <- if (is.null(tcr)) NULL else tcr[tcr$term == paste0("GroupLabel", g), , drop = FALSE]
      c_ok <- !is.null(crow) && nrow(crow) == 1 && is.finite(crow$estimate[1]) &&
        is.finite(crow$std.error[1]) && !(dich && abs(crow$estimate[1]) > log(1e4))
      rows[[length(rows) + 1]] <- data.frame(
        Outcome = dn(outc), Comparison = paste(g, "vs", ref),
        `Adjusted effect (95% CI)` = eff, `P value` = if (unstable) "" else fmt_p(row$p.value[1]),
        .p_num = if (unstable) NA_real_ else row$p.value[1],
        crude_eff = if (!c_ok) "not estimable"
                    else fmt_effect(if (dich) "OR" else "beta", crow$EffectValue[1],
                                    crow$CI_low[1], crow$CI_high[1]),
        crude_p = if (c_ok) fmt_p(crow$p.value[1]) else "",
        .cp_num = if (c_ok) crow$p.value[1] else NA_real_,
        n = n_txt,
        Note = if (unstable) paste0(note, if (nzchar(caution)) ". " else "",
                                    "Unstable estimate (quasi-separation) - effect not reported")
               else .join_note(note, few), check.names = FALSE)
      names(rows[[length(rows)]])[match(c("crude_eff", "crude_p"), names(rows[[length(rows)]]))] <-
        c(REG_CRUDE_EFF, REG_CRUDE_P)
    }
    add_rows(rows)
    # the few-patients caution is a small-sample caution too (counted once per model)
    if (few_flag && !nzchar(caution)) n_flagged <- n_flagged + 1
  }
  pub_df <- if (length(pub)) dplyr::bind_rows(pub) else data.frame()
  if (nrow(pub_df)) {
    # one column order for every row; a cell a row does not fill is blank
    for (cc in setdiff(pub_cols, names(pub_df))) pub_df[[cc]] <- ""
    for (cc in c(REG_HOLM_P, REG_CRUDE_EFF, REG_CRUDE_P, REG_CRUDE_HOLM_P))
      if (cc %in% names(pub_df)) pub_df[[cc]][is.na(pub_df[[cc]])] <- ""
    pub_df <- pub_df[, c(pub_cols, setdiff(names(pub_df), pub_cols)), drop = FALSE]
  }
  list(pub = pub_df,
       full = if (length(full)) dplyr::bind_rows(full) else data.frame(),
       n_flagged = n_flagged)
}

# Merge the adjusted-regression results into a summary table as extra columns
# (one line per group comparison when there are >2 groups): the effect, its P
# - Holm-adjusted across the outcome's comparisons with the reference and
# tagged "[Holm]" when two or more are estimable (decision D14) - and the
# number of eyes of the adjusted model (decision D17: the crude columns of the
# same table may count more eyes).
merge_regression_into_summary <- function(summary_tbl, reg_pub) {
  if (is.null(summary_tbl) || nrow(summary_tbl) == 0) return(summary_tbl)
  out <- summary_tbl
  out[["Adjusted effect (95% CI)"]] <- ""
  out[["Adjusted P"]] <- ""
  out[["Adjusted model n (eyes)"]] <- ""
  if (is.null(reg_pub) || nrow(reg_pub) == 0 || !("Outcome" %in% names(reg_pub))) return(out)
  rp <- reg_pub[nzchar(reg_pub$Comparison %||% ""), , drop = FALSE]
  if (nrow(rp) == 0) return(out)
  multi <- length(unique(rp$Comparison)) > 1
  for (oc in unique(rp$Outcome)) {
    rows <- rp[rp$Outcome == oc, , drop = FALSE]
    pre <- if (multi) paste0(rows$Comparison, ": ") else ""
    eff <- paste0(pre, rows[["Adjusted effect (95% CI)"]], collapse = "\n")
    ph <- if (REG_HOLM_P %in% names(rows)) as.character(rows[[REG_HOLM_P]]) else rep("", nrow(rows))
    ph[is.na(ph)] <- ""
    pv  <- paste0(pre, ifelse(nzchar(ph), paste0(ph, " [Holm]"), rows[["P value"]]), collapse = "\n")
    # the eyes of the adjusted model: the n of its estimated comparisons
    pvv <- as.character(rows[["P value"]])
    est <- !is.na(pvv) & nzchar(pvv)
    nn <- unique(as.character(rows$n[est & !is.na(rows$n)]))
    hit <- .rows_for(out, oc)
    if (length(hit)) {
      out[hit, "Adjusted effect (95% CI)"] <- eff; out[hit, "Adjusted P"] <- pv
      out[hit, "Adjusted model n (eyes)"] <- paste(nn, collapse = "; ")
    }
  }
  out
}

# ------------------------------------------------------------
# Reference-group pairwise PSM.
# Returns a list of pairings; each element:
#   $label, $treated (group name), $reference, $matched (data.frame),
#   $m.out (MatchIt object), $bal (bal.tab), $method (the method run).
# For k == 2 there is a single pairing (identical to original).
# Every matchit() call runs under a LOCALLY fixed seed (psm$seed,
# default 20250601; the caller's random stream is restored), so a
# random matching order gives the same matched cohort on every run
# and each pairing is independent of the loop order.
# attr(, "failed"): named character, pairing label -> why no matched
# cohort was produced (for the workbook to list next to the pairings).
# attr(, "settings"): the settings actually used (.psm_settings(): a
# setting the method ignores is NULL and named in $not_used; $method_used:
# pairing label -> the method each matched pairing ran).
# ------------------------------------------------------------
PSM_DEFAULT_SEED <- 20250601L

# ---- propensity-score scale (decision D13) -------------------
# psm_distance names a MatchIt distance as v1.0.0 did; the links are handed to
# matchit() the way MatchIt 4 asks for them (distance = "glm", link = ...), and
# the run and the live match preview (engine/match_preview.R) make the same
# call:
#   "linear.logit" (the default) -> glm, link "linear.logit": the score on
#       the LOGIT scale, so the default caliper is 0.2 SD of the logit of the
#       propensity score (Austin 2011);
#   "logit" / "probit" / "cloglog" / "cauchit", and "glm" -> glm with that
#       link (glm: logit): the score on the probability scale;
#   "linear.probit" ... -> the linear predictor of that link;
#   any other distance (gam, rpart, Mahalanobis, ...) as it is, with
#       MatchIt's default link; NULL (exact / cem) stays NULL.
PSM_DEFAULT_DISTANCE <- "linear.logit"
PSM_LINKS <- c("logit", "probit", "cloglog", "cauchit")
PSM_NON_SCORE <- c("mahalanobis", "robust_mahalanobis", "euclidean", "scaled_euclidean")
.psm_distance_args <- function(distance) {
  if (is.null(distance)) return(list(distance = NULL, link = "logit"))
  d <- tolower(trimws(as.character(distance)[1]))
  if (is.na(d) || !nzchar(d)) d <- PSM_DEFAULT_DISTANCE
  if (d == "glm") return(list(distance = "glm", link = "logit"))
  if (d %in% c(PSM_LINKS, paste0("linear.", PSM_LINKS))) return(list(distance = "glm", link = d))
  list(distance = d, link = "logit")
}

# The scale of the score a distance matches on, in words: "logit of the
# propensity score" (linear.logit), "linear predictor (probit link) of the
# propensity score", "propensity score on the probability scale"; NA for a
# distance that is not a propensity score (Mahalanobis, Euclidean ...).
psm_score_scale <- function(distance) {
  d <- tolower(trimws(as.character(distance %||% PSM_DEFAULT_DISTANCE)[1]))
  if (is.na(d) || !nzchar(d)) d <- PSM_DEFAULT_DISTANCE
  if (d %in% PSM_NON_SCORE) return(NA_character_)
  if (startsWith(d, "linear.")) {
    lk <- sub("^linear\\.", "", d)
    return(if (lk == "logit") "logit of the propensity score"
           else paste0("linear predictor (", lk, " link) of the propensity score"))
  }
  "propensity score on the probability scale"
}

# The propensity-score row of the balance table and love plot (cobalt calls it
# "distance"): "Propensity score (logit)", "Propensity score (probability
# scale)", "Propensity score (probit linear predictor)"; "distance" when the
# distance is not a propensity score.
psm_score_label <- function(distance) {
  s <- psm_score_scale(distance)
  if (is.na(s)) return("distance")
  if (startsWith(s, "logit")) return("Propensity score (logit)")
  if (startsWith(s, "linear predictor"))
    return(paste0("Propensity score (", sub("^linear predictor \\((.*) link\\).*$", "\\1", s),
                  " linear predictor)"))
  "Propensity score (probability scale)"
}

# The caliper in words, for the PSM sheet and the Manifest:
#   "0.2 SD of the logit of the propensity score" (standardized; raw = the
#   width on that scale, MatchIt's m.out$caliper: " (= 0.185 on that scale)")
#   "0.2 on the logit of the propensity score"     (not standardized)
#   "none"                                         (no caliper)
psm_caliper_text <- function(caliper, std_caliper = TRUE, distance = NULL, raw = NULL) {
  cal <- suppressWarnings(as.numeric(unlist(caliper))[1])
  if (!length(cal) || !is.finite(cal)) return("none")
  s <- psm_score_scale(distance)
  if (is.na(s)) s <- "distance"
  r <- suppressWarnings(as.numeric(unlist(raw))[1])
  paste0(format(cal), if (isTRUE(std_caliper)) " SD of the " else " on the ", s,
         if (isTRUE(std_caliper) && length(r) && is.finite(r))
           paste0(" (= ", format(signif(r, 3)), " on that scale)") else "")
}

# Matching settings as matchit() needs them. A blank / non-numeric ratio
# (e.g. a cleared GUI box) becomes 1 and a blank caliper means no caliper;
# both are logged. Only the settings the method uses are kept: exact, cem and
# subclass ignore the ratio, the replacement flag, the caliper (and its
# standardisation) and the match order, and exact / cem use no distance at
# all. Those settings are NULL here and named in `not_used`, so the PSM
# settings sheet and the Manifest never report a value matchit() did not use
# (matchit() would only warn and drop it); one that was actually set is
# logged once.
.psm_settings <- function(psm, logger = NULL) {
  blank <- function(x) is.null(x) || length(x) == 0 || is.na(x[[1]]) || !nzchar(trimws(as.character(x[[1]])))
  num1 <- function(x) suppressWarnings(as.numeric(as.character(x[[1]])))
  txt1 <- function(x) trimws(as.character(x[[1]]))
  out <- psm
  # matchit(method = NULL) means "no matching", so a blank method is nearest;
  # trimmed and lower-cased like the server's config_schema.psm_method()
  out$method <- if (blank(psm$method)) "nearest" else tolower(txt1(psm$method))
  not_used <- switch(out$method,
    exact = , cem = c("distance", "ratio", "replace", "caliper", "std_caliper", "m_order"),
    subclass = c("ratio", "replace", "caliper", "std_caliper", "m_order"),
    character(0))
  uses <- function(s) !(s %in% not_used)

  r <- if (blank(psm$ratio)) NA_real_ else num1(psm$ratio)
  if (uses("ratio") && (!is.finite(r) || r < 1)) {
    log_once(logger, sprintf("Matching ratio '%s' is not a number >= 1; ratio 1 is used.",
                             if (blank(psm$ratio)) "" else txt1(psm$ratio)))
    r <- 1
  }
  out$ratio <- r
  cal <- if (blank(psm$caliper)) NA_real_ else num1(psm$caliper)
  if (uses("caliper") && !blank(psm$caliper) && (!is.finite(cal) || cal <= 0)) {
    log_once(logger, sprintf("Matching caliper '%s' is not a positive number; matching runs without a caliper.",
                             txt1(psm$caliper)))
    cal <- NA_real_
  }
  out["caliper"] <- list(if (is.finite(cal)) cal else NULL)
  # blank distance = the default: the logit of the propensity score (D13)
  out$distance <- if (blank(psm$distance)) PSM_DEFAULT_DISTANCE else tolower(txt1(psm$distance))
  if (blank(psm$m_order)) out["m_order"] <- list(NULL)
  out$replace <- isTRUE(as.logical(psm$replace))
  out$std_caliper <- if (blank(psm$std_caliper)) TRUE else isTRUE(as.logical(psm$std_caliper))

  # settings the user actually set (a value other than the default) that this
  # method ignores: named once in the log
  set <- c(distance = !blank(psm$distance) &&
             !(tolower(txt1(psm$distance)) %in% c("glm", PSM_DEFAULT_DISTANCE)),
           ratio = !blank(psm$ratio) && !identical(num1(psm$ratio), 1),
           replace = isTRUE(out$replace),
           caliper = !blank(psm$caliper),
           m_order = !blank(psm$m_order))
  shown <- c(distance = paste0("distance '", if (blank(psm$distance)) "" else txt1(psm$distance), "'"),
             ratio = paste0("ratio ", if (blank(psm$ratio)) "" else txt1(psm$ratio)),
             replace = "matching with replacement",
             caliper = paste0("caliper ", if (blank(psm$caliper)) "" else txt1(psm$caliper)),
             m_order = paste0("match order '", if (blank(psm$m_order)) "" else txt1(psm$m_order), "'"))
  ign <- intersect(not_used, names(set)[set])
  if (length(ign))
    log_once(logger, sprintf("Matching method '%s' ignores %s: %s.", out$method,
                             if (length(ign) == 1) "this setting" else "these settings",
                             paste(shown[ign], collapse = ", ")))
  for (s in not_used) out[s] <- list(NULL)
  out$not_used <- not_used
  sd <- if (blank(psm$seed)) NA_integer_ else suppressWarnings(as.integer(num1(psm$seed)))
  out$seed <- if (is.na(sd)) PSM_DEFAULT_SEED else sd
  out
}

# The method matchit() is given for the covariates `covs` of `data`. Coarsened
# exact matching coarsens only numeric covariates: when every matching
# covariate is categorical (factor, text or logical) there is nothing to
# coarsen and cem IS exact matching - and MatchIt's cem stops on that case
# (it needs cutpoints for a numeric variable), so method "exact" runs instead.
# Every other method, and cem with a numeric covariate, is returned unchanged.
.psm_method_for <- function(method, data, covs) {
  if (!identical(method, "cem") || !length(covs)) return(method)
  num <- vapply(covs, function(cv) is.numeric(data[[cv]]), logical(1))
  if (any(num)) method else "exact"
}
PSM_CEM_AS_EXACT <- paste0(
  "every matching covariate is categorical, so coarsened exact matching is ",
  "exact matching (nothing to coarsen): method 'exact' was run")

# display_names: optional clean key -> original header map (R4-3) for the log
# lines that name a covariate.
run_reference_pairwise_matching <- function(dat, glevels, reference_group,
                                            psm_covariates_clean, psm,
                                            logger = message, display_names = NULL) {
  psm <- .psm_settings(psm, logger)
  treated_groups <- setdiff(glevels, reference_group)
  results <- list()
  failed <- character(0)
  methods_used <- character(0)
  fail <- function(tg, why) {
    lab <- paste0(tg, "_vs_", reference_group)
    logger(paste0("Matching for ", tg, " vs ", reference_group, " produced no matched cohort: ",
                  why, " The matched tables and figures for this pairing are not produced."))
    failed[[lab]] <<- why
  }
  for (tg in treated_groups) {
    sub <- dat[as.character(dat$GroupLabel) %in% c(reference_group, tg), , drop = FALSE]
    # treated = 1 for tg, 0 for reference
    sub$.treat <- as.integer(as.character(sub$GroupLabel) == tg)
    # the rows as recorded: the coerced copies below only feed the propensity
    # model, the matched cohort keeps every column's own values and types (a
    # text covariate may be coerced in one pairing and not in another, and the
    # pairings' matched rows are stacked into one table)
    sub$.psm_row <- seq_len(nrow(sub))
    sub_raw <- sub
    # a covariate read as text would otherwise saturate the propensity model
    # with one dummy per distinct value (see coerce_numeric_covs in helpers.R)
    cv <- coerce_numeric_covs(sub, psm_covariates_clean, logger, display_names = display_names)
    sub <- cv$data
    covs <- setdiff(psm_covariates_clean, cv$dropped)
    if (length(covs) == 0) {
      fail(tg, "no usable matching covariate."); next
    }
    ps_df <- sub %>% dplyr::select(.treat, dplyr::all_of(covs)) %>% as.data.frame()
    cc <- complete.cases(ps_df)
    sub_ps <- sub[cc, , drop = FALSE]
    if (nrow(sub_ps) == 0 || length(unique(sub_ps$.treat)) < 2) {
      fail(tg, "no complete cases on the matching covariates in one of the two groups."); next
    }
    sub_ps <- sub_ps %>% dplyr::mutate(dplyr::across(dplyr::where(is.character), as.factor))
    f_ps <- as.formula(paste(".treat ~", paste(sprintf("`%s`", covs), collapse = " + ")))
    meth <- .psm_method_for(psm$method, sub_ps, covs)
    if (!identical(meth, psm$method))
      log_once(logger, sprintf("Matching method 'cem' with %s only: %s instead (the method actually used is recorded in the PSM settings and the Manifest).",
                               paste0("'", disp_name(covs, display_names), "'", collapse = ", "),
                               PSM_CEM_AS_EXACT))
    err <- ""
    # the score's scale (D13): "linear.logit" -> distance "glm", link "linear.logit"
    da <- .psm_distance_args(psm$distance)
    m.out <- with_local_seed(psm$seed, tryCatch(
      MatchIt::matchit(formula = f_ps, data = sub_ps,
                       method = meth, distance = da$distance, link = da$link, ratio = psm$ratio,
                       replace = psm$replace, caliper = psm$caliper,
                       std.caliper = psm$std_caliper, m.order = psm$m_order),
      error = function(e) { err <<- conditionMessage(e); NULL }))
    if (is.null(m.out)) {
      fail(tg, paste0("matchit failed (", err, ").")); next
    }
    md <- MatchIt::match.data(m.out)
    if (nrow(md) == 0 || length(unique(md$.treat)) < 2) {
      fail(tg, "no eye could be matched with these settings (e.g. the caliper is too narrow)."); next
    }
    # matched rows as recorded, plus what matching added (distance, weights,
    # subclass), in match.data()'s row order
    added <- setdiff(names(md), names(sub_raw))
    md <- cbind(sub_raw[match(md$.psm_row, sub_raw$.psm_row), , drop = FALSE],
                md[, added, drop = FALSE])
    md$.psm_row <- NULL
    rownames(md) <- NULL
    md$GroupLabel <- droplevels(factor(as.character(md$GroupLabel),
                                       levels = c(reference_group, tg)))
    # standardized differences for binary AND continuous covariates, so every
    # row is an SMD judged against the same 0.1 threshold (cobalt's default
    # for binary covariates is the raw difference in proportions). The SD in
    # the denominator is the treated (non-reference) group's - the standard
    # for the ATT that matchit() estimates here - stated explicitly so the
    # documentation and the numbers cannot drift apart.
    bal <- tryCatch(cobalt::bal.tab(m.out, un = TRUE, binary = "std", continuous = "std",
                                    s.d.denom = "treated", m.threshold = 0.1),
                    error = function(e) NULL)
    results[[tg]] <- list(label = paste0(tg, "_vs_", reference_group),
                          treated = tg, reference = reference_group,
                          matched = md, m.out = m.out, bal = bal, method = meth)
    methods_used[[results[[tg]]$label]] <- meth
  }
  # the method each matched pairing actually ran (it differs from psm$method
  # only for cem on categorical covariates, see .psm_method_for)
  if (length(methods_used)) psm$method_used <- methods_used
  attr(results, "failed") <- failed
  attr(results, "settings") <- psm
  results
}
