# ============================================================
# helpers.R
# Name cleaning, IO, formatting, variable typing, and the shared
# helpers the other engine files call: visual-acuity parsing,
# value / level matching, sheet and file names, the single
# between-group test selector used by tables AND figures, and the
# small-sample GEE inference (covariance, t / F reference, the
# clustered group tests) every model, table and figure shares.
# Ported from the user's original two-group script and
# generalized where needed. No two-group assumptions here.
# Base R only at source time (match_preview.R may source this
# file without the tidyverse attached).
# ============================================================

# ------------------------------------------------------------
# Column names
# ------------------------------------------------------------

# The header names exactly as pandas shows them in the GUI, built from the
# header row as recorded (readxl .name_repair = "minimal"): a blank header is
# "Unnamed: <0-based position>", and a repeated header becomes name, name.1,
# name.2, ... - named columns are numbered before unnamed ones and a suffix that
# is already taken by another header is skipped, as pandas' Excel parser does.
pandas_like_names <- function(nms) {
  cols <- as.character(nms)
  cols[is.na(cols)] <- ""
  unnamed <- which(!nzchar(cols))
  cols[unnamed] <- paste0("Unnamed: ", unnamed - 1L)
  counts <- new.env(hash = TRUE, parent = emptyenv())
  cnt <- function(k) { v <- counts[[k]]; if (is.null(v)) 0L else v }
  for (i in c(setdiff(seq_along(cols), unnamed), unnamed)) {
    col <- cols[i]; old <- col; cur <- cnt(col)
    while (cur > 0L) {
      assign(old, cur + 1L, envir = counts)
      col <- paste0(old, ".", cur)
      cur <- if (col %in% cols) cur + 1L else cnt(col)
    }
    cols[i] <- col
    assign(col, cur + 1L, envir = counts)
  }
  cols
}

# ASCII letters/digits joined by "_" - the part of a header a cleaned name keeps.
.clean_stem <- function(x) {
  x <- trimws(as.character(x))
  x[is.na(x)] <- ""
  x <- gsub("[^A-Za-z0-9]+", "_", x)
  gsub("^_|_$", "", x)
}

# Syntactic, UNIQUE column keys. A header with no ASCII letter or digit
# (Hebrew, Cyrillic, CJK, symbols only) is named by its position, col<index>
# (1-based), instead of collapsing to "X", "X.1", ... in file order.
clean_names <- function(x) {
  s <- .clean_stem(x)
  blank <- !nzchar(s)
  s[blank] <- paste0("col", seq_along(s)[blank])
  make.names(s, unique = TRUE)
}

# Case- and punctuation-insensitive lookup key ("" when the name has no ASCII
# letter or digit, so such names never match each other by key).
.name_key <- function(x) {
  s <- .clean_stem(x)
  k <- tolower(make.names(s))
  k[!nzchar(s)] <- ""
  k
}

# One requested name -> list(clean = <clean name or NA>, ambiguous = <raw headers>).
# Tiers, the same as the GUI's server resolver (webapp/server/preview.py
# Resolver): (1) the exact header; (2) the request without its surrounding
# spaces, again as an exact header; (3) the case/punctuation-insensitive key,
# ONLY when exactly one header has it - two or more = ambiguous = NA. A
# cleaned name ("Pre_UCVA") is matched through its key like any other
# request, so it can never pick one of two near-duplicate headers silently.
.resolve_one <- function(df_raw_names, req) {
  out <- list(clean = NA_character_, ambiguous = character(0))
  if (is.null(req) || length(req) == 0) return(out)
  req <- as.character(unlist(req))[1]
  if (is.na(req) || !nzchar(trimws(req))) return(out)
  raw <- as.character(df_raw_names); raw[is.na(raw)] <- ""
  cl <- clean_names(raw)
  i <- match(req, raw)
  if (!is.na(i)) { out$clean <- cl[i]; return(out) }
  i <- match(trimws(req), raw)
  if (!is.na(i)) { out$clean <- cl[i]; return(out) }
  key <- .name_key(req)
  if (!nzchar(key)) return(out)
  hits <- which(.name_key(raw) == key)
  if (length(hits) == 1) out$clean <- cl[hits]
  else if (length(hits) > 1) out$ambiguous <- raw[hits]
  out
}

# Map requested raw column names -> cleaned names actually present in df.
# A missing OR ambiguous name returns NA; an ambiguous one carries
# attr(, "ambiguous") = the competing headers, so the caller can log why
# (see unresolved_name_message()).
resolve_raw_to_clean <- function(df_raw_names, requested_raw) {
  if (is.null(requested_raw) || length(requested_raw) == 0) return(NA_character_)
  if (length(requested_raw) > 1)
    return(vapply(as.list(requested_raw), function(r) resolve_raw_to_clean(df_raw_names, r),
                  character(1), USE.NAMES = FALSE))
  r <- .resolve_one(df_raw_names, requested_raw)
  out <- r$clean
  if (length(r$ambiguous)) attr(out, "ambiguous") <- r$ambiguous
  out
}

# Why a requested column did not resolve, for the run log.
unresolved_name_message <- function(df_raw_names, requested_raw, label = "Column") {
  r <- .resolve_one(df_raw_names, requested_raw)
  if (length(r$ambiguous))
    return(paste0(label, " '", requested_raw, "' is ambiguous: it matches ",
                  paste0("'", r$ambiguous, "'", collapse = ", "),
                  " - rename one of these columns in the file."))
  paste0(label, " '", requested_raw, "' not found.")
}

resolve_vec <- function(df_raw_names, vec_raw, label = "vars", logger = message) {
  vec_raw <- as.character(unlist(vec_raw))
  vec_raw <- vec_raw[!is.na(vec_raw) & nzchar(trimws(vec_raw))]
  if (length(vec_raw) == 0) return(character(0))
  rs <- lapply(vec_raw, function(v) .resolve_one(df_raw_names, v))
  out <- vapply(rs, function(r) r$clean, character(1))
  amb <- vapply(rs, function(r) length(r$ambiguous) > 0, logical(1))
  miss <- vec_raw[is.na(out) & !amb]
  if (length(miss) > 0) logger(paste0(label, " not found (dropped): ",
                                      paste(miss, collapse = ", ")))
  for (j in which(amb))
    logger(paste0(label, ": column name '", vec_raw[j], "' is ambiguous (matches ",
                  paste0("'", rs[[j]]$ambiguous, "'", collapse = ", "),
                  ") - dropped; rename one of these columns in the file."))
  unique(out[!is.na(out)])
}

# ------------------------------------------------------------
# Display names (contract R4-3). Internal keys stay the ASCII clean names
# (formulas and lookups depend on them); every label the user reads shows the
# header as typed in the file. `map` is a named character vector (or a named
# list) clean key -> original header, normally res$display_names built once by
# pipeline.R. A key the map does not name - and every key when the map is NULL
# or empty - is returned unchanged, so a caller that passes no map prints the
# clean key exactly as before. Vectorised over `key`; NA stays NA.
# ------------------------------------------------------------
disp_name <- function(key, map = NULL) {
  out <- as.character(key)
  if (!length(out) || is.null(map) || !length(map) || is.null(names(map))) return(out)
  val <- if (is.list(map))
    vapply(map, function(z) if (length(z) && !is.null(z)) as.character(z)[1] else NA_character_,
           character(1), USE.NAMES = FALSE)
  else as.character(map)
  i <- match(out, names(map))
  ok <- !is.na(out) & !is.na(i)
  if (any(ok)) {
    v <- val[i[ok]]
    good <- !is.na(v) & nzchar(v)
    tmp <- out[ok]; tmp[good] <- v[good]; out[ok] <- tmp
  }
  out
}

# ------------------------------------------------------------
# Sheet names and file names
# ------------------------------------------------------------

# Excel sheet-name sanitizer: no : \ / ? * [ ], no control characters, no
# leading/trailing apostrophe (Excel forbids both), at most `max_chars`.
safe_sheet <- function(x, max_chars = 31) {
  x <- as.character(x)
  x[is.na(x)] <- ""
  # perl: in the default (TRE) engine a class starting "[:" is read as a POSIX
  # class name, so the old pattern never replaced anything
  x <- gsub("[:\\\\/?*\\[\\]]", "_", x, perl = TRUE)
  x <- gsub("[[:cntrl:]]", "", x)
  x <- gsub("^'+|'+$", "", x)
  x <- ifelse(nchar(x) > max_chars, substr(x, 1, max_chars), x)
  gsub("'+$", "", x)
}

# Intended (untruncated) title of every sheet created through unique_sheet(),
# keyed by the sheet name actually used, for the Contents index.
.sheet_titles <- new.env(parent = emptyenv())
sheet_full_name <- function(nm) {
  vapply(as.character(nm), function(s) {
    t <- .sheet_titles[[s]]
    if (is.null(t)) s else t
  }, character(1), USE.NAMES = FALSE)
}

# A sheet name <= 31 characters that differs (case-insensitively, as Excel
# compares) from every sheet already in `wb`: on a collision - typically two
# long names that truncate to the same 31 characters - a suffix ~2, ~3, ...
# replaces the tail. `wb` may also be a character vector of names in use.
unique_sheet <- function(wb, name) {
  have <- tolower(if (is.character(wb)) wb else names(wb))
  full <- as.character(name)[1]
  if (is.na(full)) full <- ""
  nm <- safe_sheet(full)
  if (!nzchar(nm)) { full <- "Sheet"; nm <- "Sheet" }
  k <- 1L
  while (tolower(nm) %in% have) {
    k <- k + 1L
    sfx <- paste0("~", k)
    nm <- paste0(safe_sheet(full, 31 - nchar(sfx)), sfx)
  }
  assign(nm, full, envir = .sheet_titles)
  nm
}

# Adds a NEW sheet and returns the name used. It never removes an existing
# sheet: a name collision (e.g. after 31-character truncation) gets a unique
# suffix instead of silently replacing a different table.
add_or_replace_worksheet <- function(wb, sheet_name) {
  nm <- unique_sheet(wb, sheet_name)
  openxlsx::addWorksheet(wb, nm)
  nm
}

# File-name sanitizer (distinct from the sheet sanitizer): every run of
# < > : " / \ | ? * becomes "_", control characters are dropped, trailing dots
# and spaces are trimmed (Windows rejects them), a reserved device name (CON,
# NUL, COM1, ...) gets "_" after the device name, and an empty result becomes
# "StandardEyes". Windows judges reservation on the part before the FIRST dot,
# trailing spaces ignored ("NUL.v2", "COM1.txt" and "CON .x" are all devices),
# so the "_" goes there: "NUL_.v2". Unicode letters are kept.
safe_filename <- function(x, max_chars = 100) {
  x <- as.character(x)
  x[is.na(x)] <- ""
  x <- gsub('[<>:"/\\\\|?*]+', "_", x)
  x <- gsub("[[:cntrl:]]+", "", x)
  x <- trimws(x)
  x <- ifelse(nchar(x) > max_chars, substr(x, 1, max_chars), x)
  x <- sub("[. ]+$", "", x)
  stem <- toupper(sub(" +$", "", sub("[.].*$", "", x)))
  reserved <- stem %in% c("CON", "PRN", "AUX", "NUL", "CONIN$", "CONOUT$",
                          paste0("COM", 1:9), paste0("LPT", 1:9))
  x[reserved] <- sub("^([^. ]*)", "\\1_", x[reserved])
  x[!nzchar(x)] <- "StandardEyes"
  x
}

# ------------------------------------------------------------
# Numbers, ids, logging
# ------------------------------------------------------------

as_numeric_safe <- function(x) {
  x <- as.character(x)
  x[x %in% c("", " ", "NA", "#VALUE!", "#DIV/0!")] <- NA
  suppressWarnings(as.numeric(x))
}

# Patient ids as text, written as they were read: a numeric id keeps its
# digits (100000 -> "100000", never "1e+05"; 7 -> "7", never "7.0"), anything
# else goes through as.character(). NA stays NA.
id_text <- function(x) {
  if (is.factor(x)) return(as.character(x))
  if (!is.numeric(x)) return(as.character(x))
  out <- rep(NA_character_, length(x))
  ok <- !is.na(x)
  if (any(ok)) out[ok] <- trimws(formatC(as.numeric(x[ok]), format = "fg", digits = 15))
  out
}

# Blank / missing patient ids become unique tokens "(no ID) row <n>", so each
# such eye is its own patient (singleton cluster) - never one shared NA group.
# The one implementation: tables, models and figures (graphs.R) all call it.
fill_missing_ids <- function(x) {
  v <- trimws(id_text(x))
  miss <- is.na(v) | !nzchar(v)
  if (any(miss)) v[miss] <- paste0("(no ID) row ", which(miss))
  v
}

# TRUE when `w` holds (matching) weights that are not all 1; missing weights
# are ignored. A weighted model is fitted with an independence working
# correlation exactly when this holds (contract R4-1).
weights_nonunit <- function(w) {
  if (is.null(w)) return(FALSE)
  w <- suppressWarnings(as.numeric(w))
  any(is.finite(w) & abs(w - 1) > 1e-8)
}

# Evaluate `expr` with everything it PRINTS captured instead of reaching the
# console: geepack writes e.g. "Factors not allowed to have unused levels..."
# straight to stdout, outside the engine's LOG protocol (run_pipeline.R).
# Returns list(value = <the result, NULL on error>, output = <captured lines>,
# error = <the error message, "" when none>). Warnings pass through untouched
# (callers muffle the ones they expect).
quiet_eval <- function(expr) {
  err <- ""
  val <- NULL
  out <- utils::capture.output(
    val <- tryCatch(expr, error = function(e) { err <<- conditionMessage(e); NULL }),
    type = "output")
  list(value = val, output = out, error = err)
}

# One readable line for a model that could not be fitted: the error message
# with the text geepack printed before it ("Factors not allowed to have unused
# levels... Can not continue..."), line breaks folded, trailing ones dropped.
quiet_error_text <- function(q) {
  txt <- c(q$output, q$error)
  txt <- trimws(gsub("[[:space:]]+", " ", paste(txt[nzchar(trimws(txt))], collapse = " ")))
  if (nzchar(txt)) txt else ""
}

# TRUE when the robust (sandwich) covariance of the tested terms `idx` of a
# fitted geeglm is numerically singular (contract R4-6): its reciprocal
# condition number is below WALD_RCOND_MIN, or a term's robust variance is below
# WALD_RCOND_MIN times its model-based variance (a 1 x 1 block always has rcond
# 1, however degenerate). solve() still returns on such a matrix and the Wald
# statistic explodes into a spurious "P < 0.001". The one rule for the figures'
# GEE (graphs.R gr_wald_p) and the weighted table test (analysis.R
# .weighted_wald), so a figure and its matched table never disagree on it.
# V: the robust covariance the test uses (gee_vcov(): bias-corrected in the
# default "md" mode); NULL = geepack's own (uncorrected) one.
WALD_RCOND_MIN <- 1e-8
gee_vcov_singular <- function(fit, idx, V = NULL) {
  if (is.null(V)) V <- tryCatch(fit$geese$vbeta, error = function(e) NULL)
  if (is.null(V) || !length(idx) || any(dim(V) < max(idx))) return(TRUE)
  Vg <- V[idx, idx, drop = FALSE]
  if (any(!is.finite(Vg))) return(TRUE)
  rc <- tryCatch(rcond(Vg), error = function(e) NA_real_)
  Vn <- tryCatch(fit$geese$vbeta.naiv, error = function(e) NULL)
  dn <- if (is.null(Vn) || any(dim(Vn) < max(idx))) NULL else diag(Vn)[idx]
  !is.finite(rc) || rc < WALD_RCOND_MIN ||
    (!is.null(dn) && any(is.finite(dn) & dn > 0 & diag(Vg) < WALD_RCOND_MIN * dn))
}

# ------------------------------------------------------------
# Small-sample GEE inference (decision D11). Every GEE-based test and interval
# - the table tests, the GEE and regression sheets, the weighted tests of the
# matched tables, the clustered P values of the figures and the clustered
# astigmatism tests that use a GEE - takes its covariance and its reference
# distribution from here:
#   "md"   (default): the Mancl-DeRouen (2001) bias-corrected sandwich
#          covariance; a single contrast or interval has a t reference and a
#          joint test an F reference, df = clusters (patients) - model
#          coefficients;
#   "none": geepack's own uncorrected sandwich with a normal (chi-square)
#          reference - the v1.0.0 behaviour.
# The pipeline sets the mode of a run from cfg$gee_small_sample
# (set_gee_small_sample); the models read it through gee_vcov(), gee_wald(),
# gee_ci_mult() and gee_se_label().
# ------------------------------------------------------------
GEE_SMALL_SAMPLE_MODES <- c("md", "none")
.gee_inf <- new.env(parent = emptyenv())
.gee_inf$mode <- "md"
.gee_inf$err <- ""

# The mode of the models that follow: a blank value is the default ("md"), an
# unknown one is logged and the default used. Returns the mode.
set_gee_small_sample <- function(mode = "md", logger = NULL) {
  m <- if (is.null(mode) || !length(mode) || is.na(mode[[1]])) "" else tolower(trimws(as.character(mode[[1]])))
  if (!nzchar(m)) m <- "md"
  if (!(m %in% GEE_SMALL_SAMPLE_MODES)) {
    if (!is.null(logger))
      logger(sprintf("GEE small-sample setting '%s' is not known (use md or none) - md is used.",
                     as.character(mode[[1]])))
    m <- "md"
  }
  .gee_inf$mode <- m
  invisible(m)
}
gee_small_sample <- function() .gee_inf$mode %||% "md"

# Bias-corrected sandwich covariance of a geepack::geeglm fit (Gaussian with
# the identity link or binomial with the logit link; independence or
# exchangeable working correlation; optional prior weights):
#   B^-1 [sum_i D_i' V_i^-1 A_i r_i r_i' A_i' V_i^-1 D_i] B^-1,
#   B = sum_i D_i' V_i^-1 D_i, H_i = D_i B^-1 D_i' V_i^-1,
# with r_i the residuals of cluster i and A_i = I for "CR0" (geepack's own
# robust covariance, reproduced exactly - kept as a self-check),
# (I - H_i)^-1 for "MD" (Mancl & DeRouen 2001) or (I - H_i)^-1/2 for "KC"
# (Kauermann & Carroll 2001). Prior weights enter as W^1/2 V^-1 W^1/2, as in
# geepack. The scale parameter cancels. A cluster the model fits exactly in
# some direction (leverage 1: the only patient of a group or of a covariate
# level) has a singular I - H_i and no residual in that direction; the
# generalized (Moore-Penrose) inverse of I - H_i then leaves that direction
# out - it contributes nothing there, as in the uncorrected sandwich, whose
# residual there is zero - and corrects the others (the generalized inverse of
# Pustejovsky & Tipton 2018 for the same case). attr "leverage1": the number
# of such clusters. The term that only such a cluster informs (its group's or
# its covariate level's) has a degenerate variance: the callers never report
# it (one_patient_groups; gee_lone_terms).
gee_vcov_bc <- function(fit, type = c("MD", "KC", "CR0")) {
  type <- match.arg(type)
  X <- stats::model.matrix(fit)
  y <- as.numeric(fit$y)
  mu <- as.numeric(stats::fitted(fit))
  id <- fit$id
  w <- fit$prior.weights
  if (is.null(w)) w <- rep(1, length(y))
  fam <- fit$family$family; lnk <- fit$family$link
  if (!((fam == "gaussian" && lnk == "identity") || (fam == "binomial" && lnk == "logit")))
    stop("only Gaussian (identity link) and binomial (logit link) models are supported")
  v <- if (fam == "binomial") mu * (1 - mu) else rep(1, length(mu))
  dmu <- v                         # canonical links: d mu / d eta = the variance function
  cs <- fit$corstr %||% fit$geese$model$corstr %||% "independence"
  if (!(cs %in% c("exchangeable", "independence")))
    stop("working correlation '", cs, "' is not supported")
  alpha <- if (identical(cs, "exchangeable")) as.numeric(fit$geese$alpha[1]) else 0
  idx <- split(seq_along(y), factor(id, levels = unique(id)))
  p <- ncol(X)
  Bm <- matrix(0, p, p)
  parts <- vector("list", length(idx))
  for (k in seq_along(idx)) {
    i <- idx[[k]]; ni <- length(i)
    Di <- X[i, , drop = FALSE] * dmu[i]
    R <- matrix(alpha, ni, ni); diag(R) <- 1
    sA <- sqrt(v[i])
    Wh <- sqrt(w[i])
    Vinv <- (Wh %o% Wh) * solve((sA %o% sA) * R)
    parts[[k]] <- list(D = Di, Vinv = Vinv, r = y[i] - mu[i])
    Bm <- Bm + t(Di) %*% Vinv %*% Di
  }
  Binv <- solve(Bm)
  M <- matrix(0, p, p)
  n_lev1 <- 0L
  for (pt in parts) {
    r <- pt$r
    if (type != "CR0") {
      IH <- diag(length(r)) - pt$D %*% Binv %*% t(pt$D) %*% pt$Vinv
      if (any(!is.finite(IH))) stop("the leverage of a patient could not be computed")
      if (type == "MD") {
        s <- svd(IH)
        ok <- s$d > 1e-8 * max(s$d, 1)
        if (!all(ok)) n_lev1 <- n_lev1 + 1L
        r <- s$v[, ok, drop = FALSE] %*% ((t(s$u[, ok, drop = FALSE]) %*% r) / s$d[ok])
      } else {
        e <- eigen(IH)
        ev <- Re(e$values)
        ok <- ev > 1e-8
        if (!all(ok)) n_lev1 <- n_lev1 + 1L
        r <- Re(e$vectors %*% diag(ifelse(ok, 1 / sqrt(pmax(ev, 1e-8)), 0), length(r)) %*%
                  solve(e$vectors) %*% r)
      }
    }
    u <- t(pt$D) %*% pt$Vinv %*% r
    M <- M + u %*% t(u)
  }
  V <- Binv %*% M %*% Binv
  dimnames(V) <- list(colnames(X), colnames(X))
  attr(V, "leverage1") <- n_lev1
  V
}

# The terms of a fitted geeglm (intercept excepted) whose design column is
# non-zero in ONE cluster only - the dummy of a group or of a covariate level
# held by a single patient (eye, without patient ids). That cluster has
# leverage 1 and its own term has no between-patient variance to estimate: its
# robust SE is degenerate. A group's term is never reported (contract R4-9,
# one_patient_groups); a covariate level's SE, P and CI are not reported in the
# "md" mode (extract_gee_tidy; "none" keeps the v1.0.0 output). character(0)
# when there is none.
gee_lone_terms <- function(fit) {
  X <- tryCatch(stats::model.matrix(fit), error = function(e) NULL)
  id <- fit$id
  if (is.null(X) || length(id) != nrow(X)) return(character(0))
  n_cl <- vapply(seq_len(ncol(X)), function(j) length(unique(id[X[, j] != 0])), integer(1))
  setdiff(colnames(X)[n_cl == 1L], "(Intercept)")
}

# The robust covariance of a fit in the current mode: bias-corrected ("md") or
# geepack's own ("none"). NULL when it cannot be computed; the reason is left
# in .gee_inf$err ("" otherwise).
gee_vcov <- function(fit, mode = gee_small_sample()) {
  .gee_inf$err <- ""
  if (identical(mode, "none")) return(tryCatch(fit$geese$vbeta, error = function(e) NULL))
  tryCatch(gee_vcov_bc(fit, "MD"), error = function(e) {
    .gee_inf$err <- paste0("the bias-corrected (Mancl-DeRouen) covariance could not be computed: ",
                           conditionMessage(e))
    NULL
  })
}

# Degrees of freedom of the t / F reference: clusters (patients; eyes when each
# eye is its own cluster) minus the model's coefficients, intercept included.
gee_ref_df <- function(fit) length(unique(fit$id)) - length(stats::coef(fit))

# Why a model has no t / F reference in the "md" mode - no more clusters than
# coefficients: "too few patients for this model (4 patients, 4 coefficients)";
# NULL when it has one (and always in the "none" mode).
gee_df_note <- function(n_clusters, n_coef, unit = "patient", mode = gee_small_sample()) {
  if (!identical(mode, "md")) return(NULL)
  k <- suppressWarnings(as.integer(n_clusters[1])); p <- suppressWarnings(as.integer(n_coef[1]))
  if (!length(k) || !length(p) || is.na(k) || is.na(p) || k - p >= 1) return(NULL)
  sprintf("too few %s for this model (%d %s, %d coefficients)", cluster_unit_word(unit), k,
          cluster_unit_word(unit, k), p)
}

# Multiplier of a 95% interval: the t quantile with the model's df ("md"), or
# the 1.96 of v1.0.0 ("none").
gee_ci_mult <- function(df, mode = gee_small_sample()) {
  if (identical(mode, "none")) return(1.96)
  if (!is.finite(df) || df < 1) return(NA_real_)
  stats::qt(0.975, df)
}

# Joint Wald test of the coefficients `idx` of a fitted geeglm in the current
# mode. Returns list(p, stat, df1, df2, singular, why):
#  - "md": F = W / q on (q, clusters - coefficients) df, W = b' V^-1 b with the
#    bias-corrected V (a single coefficient: the two-sided t test);
#  - "none": W on q df of the chi-square, geepack's own V (v1.0.0).
# p is NA - never a P value - when the coefficients or V are not finite, when V
# could not be computed (why), when its tested block is numerically singular
# (singular = TRUE; contract R4-6, gee_vcov_singular), or when there are no
# more clusters than coefficients (why, gee_df_note). unit: what a cluster is.
gee_wald <- function(fit, idx, unit = "patient", mode = gee_small_sample()) {
  out <- list(p = NA_real_, stat = NA_real_, df1 = length(idx), df2 = NA_real_,
              singular = FALSE, why = "")
  b <- stats::coef(fit)
  if (!length(idx) || any(!is.finite(b[idx]))) return(out)
  V <- gee_vcov(fit, mode)
  if (is.null(V)) { out$why <- .gee_inf$err; return(out) }
  Vg <- V[idx, idx, drop = FALSE]
  if (any(!is.finite(Vg))) return(out)
  if (gee_vcov_singular(fit, idx, V)) { out$singular <- TRUE; return(out) }
  W <- tryCatch(as.numeric(t(b[idx]) %*% solve(Vg) %*% b[idx]), error = function(e) NA_real_)
  if (!is.finite(W)) return(out)
  q <- length(idx)
  if (identical(mode, "none")) {
    out$stat <- W
    out$p <- stats::pchisq(W, df = q, lower.tail = FALSE)
    return(out)
  }
  why <- gee_df_note(length(unique(fit$id)), length(b), unit, mode)
  if (!is.null(why)) { out$why <- why; return(out) }
  out$df2 <- gee_ref_df(fit)
  out$stat <- W / q
  out$p <- stats::pf(W / q, q, out$df2, lower.tail = FALSE)
  out
}

# The inference named beside every GEE P value and interval; q = the number of
# coefficients tested together (1: a single comparison or interval).
#   "robust SE (Mancl-DeRouen), t reference" / "..., F reference"   ("md")
#   "robust SE"                                                      ("none", the v1.0.0 label)
gee_se_label <- function(q = 1, mode = gee_small_sample()) {
  if (identical(mode, "none")) return("robust SE")
  paste0("robust SE (Mancl-DeRouen), ", if (isTRUE(q > 1)) "F" else "t", " reference")
}

# One sentence for the run log, the sheets' notes and the Manifest. unit: what
# a cluster is - "eye" when no patient ID is in scope (each eye its own
# cluster: df = eyes - model coefficients).
gee_inference_text <- function(mode = gee_small_sample(), unit = "patient") {
  if (identical(mode, "none"))
    "uncorrected robust (sandwich) standard errors with a normal reference (chi-square for joint tests), as in v1.0.0"
  else paste0("Mancl-DeRouen bias-corrected robust standard errors with a t reference for single ",
              "comparisons and intervals and an F reference for joint tests ",
              "(df = ", if (identical(unit, "eye")) "eyes" else "patients", " - model coefficients)")
}

# ------------------------------------------------------------
# Exchangeable GEE fits whose outcome hardly varies within a patient (decision
# R3-4). An exchangeable working correlation estimated near or above 1 (age or
# sex as an outcome, the optical zone) makes the estimating equations
# ill-conditioned, and geepack can stop far from a solution. An exchangeable
# fit is accepted only if it converged, its working correlation is below
# GEE_EXCH_ALPHA_MAX and one further Fisher-scoring step moves no coefficient
# by more than GEE_EXCH_STEP_MAX of its robust SE; otherwise the model is
# refitted with an independence working correlation (the patient-clustered
# robust SE stay valid) and labeled GEE_INDEP_REFIT; if that fails too, the
# model is not estimable, with the reason. Nothing is replaced silently: the
# callers name the working correlation used on every sheet, table, figure and
# in the run log. (A variable flagged as a per-patient parameter is never the
# outcome of an eye-level GEE: analysis.R fits it on one row per patient,
# GEE_PER_PATIENT.)
# ------------------------------------------------------------
GEE_EXCH_ALPHA_MAX <- 0.95
GEE_EXCH_STEP_MAX <- 0.01
GEE_INDEP_REFIT <- "independence working correlation (the outcome is almost constant within patients)"
GEE_PER_PATIENT <- "per patient (one row per patient)"
.gee_inf$refits <- character(0)

# The move of each coefficient of a fitted geeglm (Gaussian identity or
# binomial logit; exchangeable or independence; prior weights as geepack) that
# one further Fisher-scoring step at its estimates would make, with the
# working correlation held at its estimate, in units of geepack's robust SE:
# |B^-1 sum_i D_i' V_i^-1 (y_i - mu_i)| / SE, B = sum_i D_i' V_i^-1 D_i (the
# scale cancels).
gee_scoring_step <- function(fit) {
  X <- stats::model.matrix(fit)
  y <- as.numeric(fit$y)
  mu <- as.numeric(stats::fitted(fit))
  w <- fit$prior.weights
  if (is.null(w)) w <- rep(1, length(y))
  fam <- fit$family$family
  v <- if (identical(fam, "binomial")) mu * (1 - mu) else rep(1, length(mu))
  cs <- fit$corstr %||% fit$geese$model$corstr %||% "independence"
  alpha <- if (identical(cs, "exchangeable")) as.numeric(fit$geese$alpha[1]) else 0
  idx <- split(seq_along(y), factor(fit$id, levels = unique(fit$id)))
  p <- ncol(X)
  Bm <- matrix(0, p, p); U <- numeric(p)
  for (i in idx) {
    ni <- length(i)
    Di <- X[i, , drop = FALSE] * v[i]
    R <- matrix(alpha, ni, ni); diag(R) <- 1
    sA <- sqrt(v[i]); Wh <- sqrt(w[i])
    Vinv <- (Wh %o% Wh) * solve((sA %o% sA) * R)
    Bm <- Bm + t(Di) %*% Vinv %*% Di
    U <- U + as.numeric(t(Di) %*% Vinv %*% (y[i] - mu[i]))
  }
  se <- sqrt(diag(fit$geese$vbeta))
  st <- abs(as.numeric(solve(Bm, U)))
  out <- st / se
  # a coefficient with a robust SE of 0 (e.g. the intercept when the reference
  # group's outcome is the same value in every eye) has no SE to scale by: it
  # passes when the step is nothing next to the coefficient itself
  z <- !is.finite(se) | se <= 0
  if (any(z)) out[z] <- ifelse(st[z] <= 1e-8 * pmax(1, abs(as.numeric(stats::coef(fit))[z])), 0, Inf)
  out
}

# "" when an exchangeable fit is accepted, else why not (in the words of the
# sheets and the log)
gee_exch_reject <- function(fit) {
  if (is.null(fit)) return("the exchangeable fit failed")
  err <- suppressWarnings(as.numeric(fit$geese$error))
  if (length(err) && !isTRUE(err[1] == 0)) return("the exchangeable fit did not converge")
  a <- suppressWarnings(as.numeric(fit$geese$alpha[1]))
  if (!length(a) || !is.finite(a)) return("the exchangeable working correlation could not be estimated")
  if (a >= GEE_EXCH_ALPHA_MAX)
    return(sprintf("estimated working correlation %.3f (%.2f or more)", a, GEE_EXCH_ALPHA_MAX))
  st <- tryCatch(max(gee_scoring_step(fit)), error = function(e) NA_real_)
  if (!is.finite(st)) return("its scoring check could not be computed")
  if (st > GEE_EXCH_STEP_MAX)
    return(sprintf("not at its solution (one further scoring step moves a coefficient by %.2g SE)", st))
  ""
}

# Fit a GEE with the working correlation `corstr`, and an exchangeable one
# only when gee_exch_reject() accepts it: fit_fun(corstr) returns the geeglm
# (or NULL). Returns list(fit, corstr = the working correlation of `fit`,
# refit = why the exchangeable fit was rejected ("" when it was not), why = why
# no fit is returned ("" when one is)).
gee_fit_checked <- function(fit_fun, corstr = "exchangeable") {
  if (!identical(corstr, "exchangeable")) {
    f <- fit_fun(corstr)
    return(list(fit = f, corstr = corstr, refit = "", why = if (is.null(f)) "the model could not be fitted" else ""))
  }
  f1 <- fit_fun("exchangeable")
  why <- gee_exch_reject(f1)
  if (!nzchar(why)) return(list(fit = f1, corstr = "exchangeable", refit = "", why = ""))
  f2 <- fit_fun("independence")
  err <- if (is.null(f2)) NA else suppressWarnings(as.numeric(f2$geese$error))[1]
  if (is.null(f2) || !isTRUE(err == 0))
    return(list(fit = NULL, corstr = "independence", refit = why,
                why = paste0("the exchangeable fit was not accepted (", why, ") and the independence refit ",
                             if (is.null(f2)) "failed" else "did not converge")))
  list(fit = f2, corstr = "independence", refit = why, why = "")
}

# The clustered tests of the tables and figures (gee_group_p) record each
# refit here; the pipeline and the figure pass log how many (and reset).
gee_refit_note <- function(what) .gee_inf$refits <- c(.gee_inf$refits, what)
gee_refit_take <- function() { r <- .gee_inf$refits %||% character(0); .gee_inf$refits <- character(0); r }
gee_refit_log_text <- function(n, where)
  sprintf(paste0("%s: in %d clustered test(s) the exchangeable GEE was not accepted (working correlation %.2f or more, ",
                 "no convergence, or not at its solution) - refitted with an %s; the test name says so."),
          where, n, GEE_EXCH_ALPHA_MAX, GEE_INDEP_REFIT)

# TRUE when at least one patient contributes more than one of these eyes (the
# usable eyes of one variable). Only then is a patient-clustered test run in
# the tables and figures (decision D12); with one eye per patient the
# classical tests are. Blank ids are each their own patient.
fellow_eyes_present <- function(id) {
  if (!length(id)) return(FALSE)
  anyDuplicated(fill_missing_ids(id)) > 0
}

# Patient-clustered P value of the group term(s): a GEE of the outcome on the
# group, every group term tested jointly (gee_wald; more than two groups: the
# omnibus test). THE one implementation behind the clustered and weighted
# figure P values (graphs.R p_omnibus) and the clustered table tests
# (analysis.R .clustered_test), so a table and a figure of one outcome give one
# P value. kind "proportion" -> binomial, else Gaussian; ranks = TRUE: the
# Gaussian GEE on the ranks of the usable values (ordinal outcomes).
# w: optional matching weights (one per element of x); eyes with a missing or
# non-positive weight are left out, as in analysis.R's weighted tables. With
# weights that are not all 1 the model is analysis.R's weighted model: an
# INDEPENDENCE working correlation with the robust SE clustered by `id`, whose
# estimate is the difference of the weighted means / proportions (contract
# R4-1); otherwise exchangeable. corstr: forces the working correlation
# ("independence" on every matched cohort, decision D18). unit: "eye" when `id`
# is the row (no patient ids: each eye its own cluster), so the reasons count
# eyes.
# Returns the P value, or NA:
#  - with attr "why" when the data cannot support the model - nothing is fitted
#    and the reason is the tables' one, checked in .weighted_wald's order:
#    fewer than two groups / "B has n = 1" (p_value_guard, small_group_note),
#    "B has 1 patient" (cluster_guard_note), "too few eyes for this model (n =
#    5)" (model_data_guard: the one minimum-data rule of every clustered model,
#    contract R4-10), too few patients for the t reference (gee_df_note) and,
#    on a weighted 0/1 outcome, "no weighted test possible: no events in B";
#    also when the fitted model has no corrected covariance or no t / F
#    reference (gee_wald's why, the GEE sheets' reason);
#  - without "why" when a model was fitted but failed (no convergence, a
#    numerically singular covariance - attr "singular") or, unweighted, a group
#    of a 0/1 outcome has no events or only events (no finite estimate): the
#    caller falls back (unweighted) or says the weighted test could not be
#    computed.
# each = TRUE: the same model, but each group's coefficient against the
# reference (the first group of `g`) is tested on its own (decision D14: the
# figures' "vs reference" lines then come from the model the tables' GEE
# column reports, one number per comparison). Returns the nominal P values
# named by group (NA for a group whose coefficient could not be tested), or
# the NA of the rules above when nothing could be fitted.
gee_group_p <- function(x, g, id, kind = "continuous", event = NULL, w = NULL,
                        unit = "patient", ranks = FALSE, corstr = NULL, each = FALSE) {
  na_why <- function(why) structure(NA_real_, why = why)
  if (!requireNamespace("geepack", quietly = TRUE)) return(NA_real_)
  # the groups in their given order, for the reasons; the model's factor stays
  # alphabetical (the joint Wald test does not depend on the reference level),
  # except for each = TRUE, whose coefficients are against the first group
  glv <- if (is.factor(g)) levels(g) else unique(as.character(g[!is.na(g)]))
  g <- if (isTRUE(each)) factor(as.character(g), levels = glv) else factor(as.character(g))
  y <- if (identical(kind, "proportion")) {
    as.numeric(if (is.null(event)) as.logical(x) else as.character(x) == as.character(event))
  } else as_numeric_safe(x)
  id <- fill_missing_ids(id)
  weighted <- !is.null(w)                       # a matched cohort's weighted test
  cs <- if (!is.null(corstr)) corstr
        else if (weighted && weights_nonunit(w)) "independence" else "exchangeable"
  wt <- if (is.null(w)) rep(1, length(y)) else suppressWarnings(as.numeric(w))
  ok <- is.finite(y) & !is.na(g) & is.finite(wt) & wt > 0
  if (isTRUE(ranks) && !identical(kind, "proportion")) y[ok] <- rank(y[ok])
  d <- data.frame(y = y[ok], g = droplevels(g[ok]), .w = wt[ok],
                  cid = as.integer(factor(as.character(id[ok]))))
  gtx <- droplevels(factor(as.character(d$g), levels = glv))   # for the texts
  # never a model for data that cannot support a P value (p_omnibus says why
  # before it gets here; these are the same guards, in the same order)
  if (nlevels(d$g) < 2) return(na_why("fewer than two groups have data for this variable"))
  if (length(unique(d$y)) < 2) return(na_why("variable is constant in this cohort"))
  # a group of one eye or one patient leaves the sandwich SE nothing to estimate
  # its variance from, so its mean counts as known exactly (a proportion needs
  # one eye per group in p_value_guard; its one-eye group is "B has 1 patient")
  if (!identical(kind, "proportion")) {
    why <- small_group_note(gtx)
    if (nzchar(why)) return(na_why(why))
  }
  why <- cluster_guard_note(id[ok], gtx, unit)
  if (nzchar(why)) return(na_why(why))
  # the minimum data for any clustered model (y ~ group: one coefficient per group)
  why <- model_data_guard(nrow(d), length(unique(d$cid)), nlevels(d$g), unit)
  if (!is.null(why)) return(na_why(why))
  why <- gee_df_note(length(unique(d$cid)), nlevels(d$g), unit)
  if (!is.null(why)) return(na_why(why))
  # a 0/1 outcome constant within a group has no finite estimate: a weighted
  # comparison says which group, as the matched table does; an unweighted one
  # falls back to the classical test
  if (identical(kind, "proportion")) {
    s0 <- tapply(d$y, gtx, function(z) all(z == 0))
    s1 <- tapply(d$y, gtx, function(z) all(z == 1))
    sep <- names(s0)[s0 | s1]
    if (length(sep)) {
      if (!weighted) return(NA_real_)
      return(na_why(paste0("no weighted test possible: ",
                           paste(paste0(ifelse(s0[sep], "no events in ", "only events in "), sep),
                                 collapse = "; "))))
    }
  }
  d <- d[order(d$cid), , drop = FALSE]          # geepack needs rows sorted by cluster
  fam <- if (identical(kind, "proportion")) stats::binomial() else stats::gaussian()
  # anything geepack prints stays off the console (stdout is the engine's LOG
  # protocol): quiet_eval. An exchangeable fit is kept only when it is
  # accepted; otherwise the independence refit, recorded and labeled through
  # attr "refit" (decision R3-4; gee_fit_checked)
  fc <- gee_fit_checked(function(cs_) quiet_eval(suppressWarnings(
           if (is.null(w)) geepack::geeglm(y ~ g, id = cid, data = d, family = fam, corstr = cs_)
           else geepack::geeglm(y ~ g, id = cid, data = d, weights = .w, family = fam,
                                corstr = cs_)))$value, cs)
  fit <- fc$fit
  if (is.null(fit)) return(if (nzchar(fc$refit)) na_why(fc$why) else NA_real_)
  if (nzchar(fc$refit)) gee_refit_note(fc$refit)
  tag <- function(p) if (nzchar(fc$refit)) structure(p, refit = fc$refit, corstr = fc$corstr) else p
  if (isTRUE(each)) {
    if (!identical(levels(d$g)[1], glv[1])) return(na_why(paste0(glv[1], " has no data for this variable")))
    cn <- names(stats::coef(fit))
    out <- stats::setNames(rep(NA_real_, length(glv) - 1L), glv[-1])
    for (l in intersect(glv[-1], levels(d$g))) {
      j <- match(paste0("g", l), cn)
      if (!is.na(j)) out[[l]] <- gee_wald(fit, j, unit)$p
    }
    return(tag(out))
  }
  wt <- gee_wald(fit, grep("^g", names(stats::coef(fit))), unit)
  if (isTRUE(wt$singular)) return(structure(NA_real_, singular = TRUE))
  # no corrected covariance or no t / F reference: the GEE sheets' reason,
  # never a silent classical fallback
  if (!is.finite(wt$p) && nzchar(wt$why)) return(na_why(wt$why))
  tag(wt$p)
}

# Patient-clustered test of a categorical variable with more than two levels
# (decision D12): every eye contributes one record per level after the first
# (1 = the eye has that level), the records are stacked, and a Gaussian GEE
# of the indicator on level * group - INDEPENDENCE working correlation, robust
# SE clustered by `id` - tests every group and level:group term jointly, i.e.
# the same distribution over the levels in every group (the clustered
# analogue of the chi-squared test; the stacked design is the one
# astig_bivariate_p uses for the two vector components). q = (levels - 1) x
# (groups - 1) coefficients. v: the level of each eye (NA = missing); g: its
# group; id: its patient. Returns the P value, or NA with attr "why" (the
# guards of gee_group_p, the model counting (levels - 1) x groups
# coefficients; gee_wald's why) or without it when the fit failed or its covariance is
# singular (the caller falls back to the classical test, labelled).
gee_categorical_p <- function(v, g, id, unit = "patient") {
  na_why <- function(why) structure(NA_real_, why = why)
  if (!requireNamespace("geepack", quietly = TRUE)) return(NA_real_)
  glv <- if (is.factor(g)) levels(g) else unique(as.character(g[!is.na(g)]))
  vv <- as.character(v)
  ok <- !is.na(vv) & !is.na(g)
  vv <- vv[ok]
  gg <- droplevels(factor(as.character(g[ok]), levels = glv))
  idk <- fill_missing_ids(id)[ok]
  levs <- sort(unique(vv))
  if (nlevels(gg) < 2) return(na_why("fewer than two groups have data for this variable"))
  if (length(levs) < 2) return(na_why("variable is constant in this cohort"))
  why <- cluster_guard_note(idk, gg, unit)
  if (nzchar(why)) return(na_why(why))
  n_coef <- (length(levs) - 1L) * nlevels(gg)
  why <- model_data_guard(length(vv), length(unique(idk)), n_coef, unit)
  if (!is.null(why)) return(na_why(why))
  why <- gee_df_note(length(unique(idk)), n_coef, unit)
  if (!is.null(why)) return(na_why(why))
  cid <- as.integer(factor(idk))
  d <- do.call(rbind, lapply(levs[-1], function(l)
    data.frame(ind = as.numeric(vv == l), lev = l, g = gg, cid = cid, stringsAsFactors = FALSE)))
  d$lev <- factor(d$lev, levels = levs[-1])
  d <- d[order(d$cid), , drop = FALSE]
  f <- if (length(levs) > 2) ind ~ lev * g else ind ~ g
  fit <- quiet_eval(suppressWarnings(
           geepack::geeglm(f, id = cid, data = d, family = stats::gaussian(),
                           corstr = "independence")))$value
  if (is.null(fit)) return(NA_real_)
  # the group and level:group terms, found through the model's term
  # assignment (a level name can never be mistaken for a group term)
  asg <- attr(stats::model.matrix(fit), "assign")
  tl <- attr(stats::terms(fit), "term.labels")
  wt <- gee_wald(fit, which(asg > 0 & tl[pmax(asg, 1L)] %in% c("g", "lev:g")), unit)
  if (isTRUE(wt$singular)) return(structure(NA_real_, singular = TRUE))
  if (!is.finite(wt$p) && nzchar(wt$why)) return(na_why(wt$why))
  wt$p
}

# Log a message once per R session (the same notice is otherwise repeated by
# every table and model that meets the same column).
.logged_once <- new.env(parent = emptyenv())
log_once <- function(logger, msg) {
  if (is.null(logger) || !is.function(logger) || !length(msg) || !nzchar(msg[1]))
    return(invisible(FALSE))
  if (!is.null(.logged_once[[msg[1]]])) return(invisible(FALSE))
  assign(msg[1], TRUE, envir = .logged_once)
  logger(msg[1])
  invisible(TRUE)
}

# ------------------------------------------------------------
# Value matching (group definitions, subsets, in / notin filters)
# ------------------------------------------------------------

# TRUE where a cell equals one of the requested values. Numeric columns compare
# numerically ("1", "1.0" and "1e+00" all match 1 - the GUI may send pandas'
# "1.0"); logical columns accept TRUE/True/true/1/1.0 (and FALSE/0); dates match
# "YYYY-MM-DD" or "YYYY-MM-DD HH:MM:SS"; text compares trimmed strings exactly
# (no "007" -> "7"). Missing cells never match.
match_values <- function(v, vals) {
  vals <- trimws(as.character(unlist(vals)))
  vals <- vals[!is.na(vals)]
  n <- length(v)
  if (n == 0) return(logical(0))
  if (!length(vals)) return(rep(FALSE, n))
  if (is.logical(v)) {
    up <- toupper(vals)
    nv <- suppressWarnings(as.numeric(vals))
    want <- c(if (any(up == "TRUE" | (!is.na(nv) & nv == 1))) TRUE,
              if (any(up == "FALSE" | (!is.na(nv) & nv == 0))) FALSE)
    hit <- v %in% want
  } else if (is.numeric(v)) {
    nv <- suppressWarnings(as.numeric(vals))
    hit <- v %in% nv[!is.na(nv)]
    txt <- vals[is.na(nv)]
    if (length(txt)) hit <- hit | trimws(as.character(v)) %in% txt
  } else if (inherits(v, c("Date", "POSIXt"))) {
    hit <- format(v, "%Y-%m-%d") %in% vals | format(v, "%Y-%m-%d %H:%M:%S") %in% vals
  } else {
    hit <- trimws(as.character(v)) %in% vals
  }
  hit[is.na(v)] <- FALSE
  hit
}

# Normalised key for a level: case and surrounding spaces ignored, numbers in
# canonical form ("1.0" -> "1"), TRUE/FALSE spelled as 1/0.
.level_key <- function(z) {
  s <- toupper(trimws(as.character(z)))
  n <- suppressWarnings(as.numeric(s))
  num <- !is.na(n)
  s[num] <- as.character(n[num])
  s[s == "TRUE"] <- "1"
  s[s == "FALSE"] <- "0"
  s
}

.has_pref <- function(pref) {
  !is.null(pref) && length(pref) > 0 && !is.na(pref[[1]]) &&
    nzchar(trimws(as.character(pref[[1]])))
}

# A preference as it should read in a row label ("1.0" -> "1").
.pref_label <- function(pref) {
  p <- trimws(as.character(pref[[1]]))
  n <- suppressWarnings(as.numeric(p))
  if (!is.na(n)) as.character(n) else p
}

# The ACTUAL data level a preferred (event) level refers to, or NA when it does
# not resolve to exactly one level. Tried in order: exact text, case/space
# -insensitive text, then the normalised key (1 = 1.0 = TRUE = True).
resolve_positive_level <- function(pref, values) {
  if (!.has_pref(pref)) return(NA)
  p <- as.character(pref[[1]])
  vv <- unique(values[!is.na(values)])
  if (!length(vv)) return(NA)
  ch <- as.character(vv)
  i <- which(ch == p)
  if (length(i) == 1) return(vv[i])
  i <- which(toupper(trimws(ch)) == toupper(trimws(p)))
  if (length(i) == 1) return(vv[i])
  i <- which(.level_key(ch) == .level_key(p))
  if (length(i) == 1) return(vv[i])
  NA
}

# The documented default event level when none is preferred (or the preferred
# one cannot be resolved) - the same guess the GUI writes into the config
# (DICHO_PREFS / DICHO_NEG in webapp/static/app.js), so a headless run picks
# the level the GUI would have shown: (1) the first known event coding that is
# present (F / Female / 1 / Yes / Y / True / Present / Positive / Abnormal /
# M / Male); (2) with exactly two levels of which exactly one is a known
# non-event coding (0 / No / N / None / Normal / False / Absent / Negative),
# the other level; (3) otherwise the second level in sorted order. Tables
# (pick_display_level) and models (make_binary_01) both use it.
default_positive_level <- function(values) {
  vv <- unique(values[!is.na(values)])
  if (!length(vv)) return(NA)
  prefs <- c("F", "Female", "female", "1", "Yes", "yes", "YES", "Y", "y",
             "True", "true", "TRUE", "Present", "present", "Positive", "positive",
             "Abnormal", "abnormal", "M", "Male", "male")
  negs <- c("0", "No", "no", "NO", "N", "n", "None", "none", "Normal", "normal",
            "False", "false", "FALSE", "Absent", "absent", "Negative", "negative")
  ch <- trimws(as.character(vv))
  for (p in prefs) { i <- match(p, ch); if (!is.na(i)) return(vv[i]) }
  if (length(vv) == 2 && sum(ch %in% negs) == 1) return(vv[!(ch %in% negs)])
  if (length(vv) >= 2) return(sort(vv)[2])
  vv[1]
}

.pref_fallback_msg <- function(var, pref, levels, chosen) {
  sprintf("'%s': preferred level '%s' was not found in the data (levels present: %s); '%s' is used as the event level in tables and models.",
          var, as.character(pref[[1]]), paste(as.character(levels), collapse = ", "),
          as.character(chosen))
}

# ------------------------------------------------------------
# Visual acuity
# ------------------------------------------------------------

# Parse a visual-acuity column recorded in `format` into DECIMAL acuity.
#   decimal: numbers (a comma decimal "0,8" is accepted); <= 0 is lost
#   logmar : decimal = 10^(-x); values outside [-0.5, 3] are lost
#   snellen: "a/b" with an optional letter suffix ("20/25-2", "6/9+1"); any
#            a/b (20/x, 6/x, ...); logMAR = log10(b/a) + 0.02*k for "-k",
#            - 0.02*k for "+k"
#   etdrs  : letter score 0..100; logMAR = 1.7 - 0.02 * letters
# Off-chart entries (CF, HM, LP, NLP, "counting fingers", "No LP", "NPL", ...)
# become NA and are counted in n_offchart - never given an invented acuity;
# `offchart` names each one as CF, HM, LP or NLP (NA for every other cell), so
# the export can still show what was recorded. They are NOT counted in n_lost
# (cells that could not be read in the declared format, including out-of-range
# numbers, also counted in n_out_of_range).
# Returns list(decimal, n_nonblank, n_lost, examples (<= 3 lost raw values),
# n_offchart, n_out_of_range, offchart).
parse_va <- function(x, format = c("decimal", "logmar", "snellen", "etdrs")) {
  format <- match.arg(format)
  s <- trimws(as.character(x))
  n <- length(s)
  blank <- is.na(s) | !nzchar(s) | toupper(s) %in% c("NA", "N/A", "#N/A")
  t <- gsub("−", "-", s, fixed = TRUE)         # Unicode minus sign
  t <- gsub("\\s+", " ", t)
  # one pattern per label, tried in this order (NLP before LP: "no light
  # perception" also contains "light perception")
  off_res <- c(
    NLP = paste0("^(n\\s*l\\s*p|n\\s*p\\s*l|no\\s*(l\\s*p|p\\s*l))(?![a-z])",
                 "|^no\\s+(light\\s+perception|perception\\s+of\\s+light)"),
    LP  = "^(l\\s*p|p\\s*l)(?![a-z])|^(light\\s+perception|perception\\s+of\\s+light)",
    CF  = "^(c\\s*f|f\\s*c)(?![a-z])|^(counting|count)\\s+fingers?|^fingers?\\s+count",
    HM  = "^h\\s*m(?![a-z])|^hand\\s+(motion|movement)s?")
  offchart <- rep(NA_character_, n)
  for (lab in names(off_res)) {
    hit <- !blank & is.na(offchart) & grepl(off_res[[lab]], t, ignore.case = TRUE, perl = TRUE)
    offchart[hit] <- lab
  }
  off <- !is.na(offchart)
  todo <- !blank & !off
  t <- ifelse(grepl("^[-+]?[0-9]+,[0-9]+$", t), sub(",", ".", t, fixed = TRUE), t)

  t[is.na(t)] <- ""
  dec <- rep(NA_real_, n)
  oor <- rep(FALSE, n)
  if (format == "snellen") {
    re <- "^([0-9]+(?:[.,][0-9]+)?) ?/ ?([0-9]+(?:[.,][0-9]+)?) ?(?:([+-]) ?([0-9]+))?$"
    m <- regmatches(t, regexec(re, t, perl = TRUE))
    for (i in which(todo & lengths(m) == 5)) {
      a <- as.numeric(sub(",", ".", m[[i]][2], fixed = TRUE))
      b <- as.numeric(sub(",", ".", m[[i]][3], fixed = TRUE))
      if (!is.finite(a) || !is.finite(b) || a <= 0 || b <= 0) { oor[i] <- TRUE; next }
      k <- if (nzchar(m[[i]][5])) as.numeric(m[[i]][5]) else 0
      adj <- if (identical(m[[i]][4], "-")) 0.02 * k else if (identical(m[[i]][4], "+")) -0.02 * k else 0
      dec[i] <- 10^(-(log10(b / a) + adj))
    }
  } else {
    num <- rep(NA_real_, n)
    num[todo] <- suppressWarnings(as.numeric(t[todo]))
    num[!is.finite(num)] <- NA_real_
    ok <- switch(format,
      decimal = !is.na(num) & num > 0,
      logmar  = !is.na(num) & num >= -0.5 & num <= 3,
      etdrs   = !is.na(num) & num >= 0 & num <= 100)
    oor <- !is.na(num) & !ok
    dec[ok] <- switch(format,
      decimal = num[ok],
      logmar  = 10^(-num[ok]),
      etdrs   = 10^(-(1.7 - 0.02 * num[ok])))
  }
  lost <- todo & is.na(dec)
  list(decimal = dec,
       n_nonblank = sum(!blank),
       n_lost = sum(lost),
       examples = utils::head(unique(s[lost]), 3),
       n_offchart = sum(off),
       n_out_of_range = sum(lost & oor),
       offchart = offchart)
}

safe_colname <- function(x, max_chars = 60) {
  x <- as.character(x)
  if (length(x) == 0 || is.na(x[1])) return("Group")
  x <- x[1]
  x <- gsub("[\r\n\t]", " ", x)
  x <- trimws(x)
  substr(x, 1, max_chars)
}

# ------------------------------------------------------------
# P values (decision D15): ONE formatter for the tables, sheets, logs and
# figures. The style of a run is set once by the pipeline (set_p_style, from
# cfg$p_style):
#   "ama" (default, the JCRS / AMA style): two decimals; three when P < .01;
#         more when fewer would round a value below .05 up to ".05" (so the
#         printed value never sits on the wrong side of .05); no leading zero;
#         "<.001" and ">.99" - never 0 or 1. Test vector (reporting.md C01):
#         4e-5 .0049 .0123 .0498 .051 .5118 .996 1 ->
#         <.001 .005 .01 .0498 .05 .51 >.99 >.99
#   "classic": the v1.0.0 formats - three decimals with a leading zero in the
#         tables ("<0.001", "0.266"), four on the figure panels ("P = 0.5118",
#         "P < 0.0001").
# A missing or non-finite P is NA (the tables print the reason instead).
# ------------------------------------------------------------
P_STYLES <- c("ama", "classic")
.p_fmt <- new.env(parent = emptyenv())
.p_fmt$style <- "ama"

# The P style of what follows: blank = the default ("ama"); an unknown value is
# logged and the default used. Returns the style.
set_p_style <- function(style = "ama", logger = NULL) {
  s <- if (is.null(style) || !length(style) || is.na(style[[1]])) "" else tolower(trimws(as.character(style[[1]])))
  if (!nzchar(s)) s <- "ama"
  if (s == "jcrs") s <- "ama"
  if (!(s %in% P_STYLES)) {
    if (!is.null(logger))
      logger(sprintf("P value style '%s' is not known (use ama or classic) - ama is used.",
                     as.character(style[[1]])))
    s <- "ama"
  }
  .p_fmt$style <- s
  invisible(s)
}
p_style <- function() .p_fmt$style %||% "ama"

# The P style in words, for the run log and the Manifest.
p_style_text <- function(style = p_style()) {
  if (identical(style, "classic"))
    "classic - three decimals with a leading zero in the tables, four on the figures (as v1.0.0)"
  else paste0("ama - two decimals, three below .01 (more only where needed to stay below .05), ",
              "no leading zero, <.001 and >.99")
}

# one P in the "ama" style (p finite)
.fmt_p_ama1 <- function(p) {
  if (p < 0.001) return("<.001")
  if (p > 0.99) return(">.99")
  d <- if (p < 0.01) 3L else 2L
  s <- sprintf(paste0("%.", d, "f"), p)
  # a value below .05 is never printed as ".05" (or ".050"): add a decimal
  while (p < 0.05 && as.numeric(s) >= 0.05 && d < 10L) {
    d <- d + 1L
    s <- sprintf(paste0("%.", d, "f"), p)
  }
  sub("^0", "", s)
}

# The P column of a table: "<.001", ".27", ">.99" ("ama"); NA when p is missing.
fmt_p <- function(p, style = p_style()) {
  if (identical(style, "classic")) return(.fmt_p_classic(p))
  pn <- suppressWarnings(as.numeric(p))
  out <- rep(NA_character_, length(pn))
  for (i in which(is.finite(pn))) out[i] <- .fmt_p_ama1(pn[i])
  out
}

# A P value with its relation, as printed beside a test: "P = .27", "P < .001",
# "P > .99" ("ama"). classic: the v1.0.0 text of the figure panels ("P =
# 0.5118", "P < 0.0001"; where = "figure") or of the double-angle plots and
# tables ("P = 0.261", "P < 0.001"; where = "table"). "" when p is missing.
p_eq <- function(p, style = p_style(), where = c("figure", "table")) {
  where <- match.arg(where)
  p <- suppressWarnings(as.numeric(p)[1])
  if (!length(p) || !is.finite(p)) return("")
  if (identical(style, "classic")) {
    if (where == "figure") return(if (p < 0.0001) "P < 0.0001" else sprintf("P = %.4f", p))
    return(if (p < 0.001) "P < 0.001" else paste0("P = ", .fmt_p_classic(p)))
  }
  s <- .fmt_p_ama1(p)
  if (startsWith(s, "<")) paste0("P < ", substring(s, 2))
  else if (startsWith(s, ">")) paste0("P > ", substring(s, 2))
  else paste0("P = ", s)
}

# The interpretation of a P value (decision D15): "Statistically significant
# (between groups)." / "Not statistically significant (between groups)."
# (word: "between groups" / "among groups").
p_interpretation <- function(p, word = "between groups") {
  paste0(if (isTRUE(p < 0.05)) "Statistically significant" else "Not statistically significant",
         " (", word, ").")
}

# The v1.0.0 table format ("classic").
.fmt_p_classic <- function(p){
  p <- suppressWarnings(as.numeric(p))
  out <- rep(NA_character_, length(p))
  out[!is.na(p) & p < 0.001]  <- "<0.001"
  k <- !is.na(p) & p >= 0.001
  out[k] <- sprintf("%.3f", p[k])
  # 3 decimals print BOTH 0.0498 and 0.0501 as "0.050" - ambiguous exactly at
  # the significance threshold. Disambiguate with a 4th decimal only there.
  amb <- k & !is.na(out) & out == "0.050"
  out[amb] <- sprintf("%.4f", p[amb])
  # a p just below 0.05 can still round UP to "0.0500" and then read as
  # non-significant next to "Significant": add digits until the printed value
  # stays below 0.05 (p >= 0.05 printed as 0.0500 is already on the right side)
  for (i in which(amb & out == "0.0500" & p < 0.05)) {
    s <- NA_character_
    for (d in 5:10) {
      s <- sprintf(paste0("%.", d, "f"), p[i])
      if (as.numeric(s) < 0.05) break
    }
    out[i] <- if (as.numeric(s) < 0.05) s else "<0.05"
  }
  out
}

fmt_num <- function(x, digits = 3){
  ifelse(is.na(x), NA_character_, formatC(x, digits = digits, format = "f"))
}

fmt_ci <- function(lo, hi, digits = 3){
  ifelse(is.na(lo) | is.na(hi), NA_character_,
         paste0("[", fmt_num(lo, digits), ", ", fmt_num(hi, digits), "]"))
}

# ------------------------------------------------------------
# Table cells (decision D24): fixed decimals - means, SDs, medians and effects
# with two, percentages with one - and a rounded negative zero printed as 0
# ("0.00", never "-0.00"). "n/a" where the value is missing.
# ------------------------------------------------------------
fmt_fix <- function(x, digits = 2) {
  x <- suppressWarnings(as.numeric(x))
  out <- rep("n/a", length(x))
  ok <- is.finite(x)
  out[ok] <- sprintf(paste0("%.", digits, "f"), round(x[ok], digits) + 0)
  out
}
fmt_pct <- function(num, den) {
  ifelse(is.finite(den) & den > 0, paste0(fmt_fix(100 * num / den, 1), "%"), "n/a")
}
# A median [IQR] cell: whole numbers when all three are whole (an ordinal
# score of 1-5), otherwise two decimals for all three.
fmt_quartiles <- function(q) {
  q <- suppressWarnings(as.numeric(q))
  d <- if (all(is.finite(q)) && all(abs(q - round(q)) < 1e-9)) 0 else 2
  paste0(fmt_fix(q[1], d), " [", fmt_fix(q[2], d), "–", fmt_fix(q[3], d), "]")
}

# One effect style for every table (decision D24):
#   "β = -9.98 (95% CI, -29.04 to 9.08)"   kind "beta" (linear models)
#   "OR = 3.02 (95% CI, 1.21 to 7.57)"     kind "OR"   (logistic models)
# without an interval (lo / hi missing) only the estimate is given. digits: one
# number, or one per row (effect_digits).
fmt_effect <- function(kind, est, lo = NA_real_, hi = NA_real_, digits = 2) {
  if (length(digits) > 1) {
    n <- length(digits)
    kind <- rep_len(kind, n); est <- rep_len(est, n); lo <- rep_len(lo, n); hi <- rep_len(hi, n)
    return(vapply(seq_len(n), function(i) fmt_effect(kind[i], est[i], lo[i], hi[i], digits[i]), character(1)))
  }
  sym <- ifelse(toupper(kind) == "OR", "OR", "β")
  ci <- ifelse(is.finite(lo) & is.finite(hi),
               paste0(" (95% CI, ", fmt_fix(lo, digits), " to ", fmt_fix(hi, digits), ")"), "")
  paste0(sym, " = ", fmt_fix(est, digits), ci)
}

# The decimals of each model term's effect text (finding T1-05): two for the
# group comparisons and the intercept (D24); for a covariate - an effect per
# unit, often small - as many as keep two significant digits of the larger of
# the estimate and the half-width of its interval, between two and four (an
# odds ratio judged on the log scale), so a significant per-unit effect never
# prints as "β = 0.00 (95% CI, 0.00 to 0.00)".
effect_digits <- function(term, kind, est, lo = NA_real_, hi = NA_real_) {
  n <- length(est)
  term <- rep_len(as.character(term), n); kind <- rep_len(kind, n)
  lo <- rep_len(lo, n); hi <- rep_len(hi, n)
  vapply(seq_len(n), function(i) {
    if (is.na(term[i]) || grepl("^GroupLabel", term[i]) || term[i] == "(Intercept)") return(2L)
    or <- toupper(kind[i]) == "OR"
    f <- function(v) if (or) suppressWarnings(log(v)) else v
    s <- c(abs(f(est[i])), (f(hi[i]) - f(lo[i])) / 2)
    s <- s[is.finite(s)]
    ref <- if (length(s)) max(s) else NA_real_
    if (!is.finite(ref) || ref <= 0 || ref >= 0.1) 2L
    else as.integer(min(4, max(2, 1 - floor(log10(ref)))))
  }, integer(1))
}

# ------------------------------------------------------------
# Visual acuity analysed in logMAR (decision D16): a cell reads
#   "20/20 (0.00 ± 0.05 logMAR); range 20/16 to 20/32"
# - the Snellen value is the geometric mean, 20/(20 x 10^mean logMAR); the
# range runs from the best to the worst eye. "logMAR" is always written with a
# lower-case l.
# ------------------------------------------------------------
# The standard label of each 0.1-logMAR chart line from -0.3 (20/10) to 1.7
# (20/1000), the preferred-number series of logMAR charts
VA_SNELLEN_LINES <- c("10", "12.5", "16", "20", "25", "32", "40", "50", "63", "80", "100",
                      "125", "160", "200", "250", "320", "400", "500", "630", "800", "1000")
VA_LINE_TOL <- 0.005     # logMAR: a value this close to a chart line reads as that line
# "20/x" of each logMAR value v, NA when missing (decision R2-1; never a chart
# letter suffix such as "20/16-3" or "20/20+"): when v is within VA_LINE_TOL of
# a chart line - a multiple of 0.1 logMAR that the decimal-to-Snellen chart
# (export.R decimal_to_snellen) carries - that line's standard label (20/16,
# 20/20, 20/25 ... 20/63 ... 20/200); otherwise "20/" and the denominator 20 x
# 10^v rounded to a whole number (one decimal below 10: 20/8.9). The geometric
# mean and the range ends of the tables and figures read this way; the per-eye
# mEYEstro tabs keep the chart's lettered notation (decimal_to_snellen).
va_snellen <- function(lm) {
  lm <- suppressWarnings(as.numeric(lm))
  out <- rep(NA_character_, length(lm))
  ok <- is.finite(lm)
  if (!any(ok)) return(out)
  v <- lm[ok]
  k <- round(v * 10)                                   # the nearest 0.1-logMAR line
  line <- abs(v - k / 10) <= VA_LINE_TOL + 1e-9 & k >= -3 & k <= 17
  # only the lines the chart carries (any reading at that key, lettered or not)
  tbl <- if (exists("get_snellen_table", mode = "function")) get_snellen_table() else NULL
  if (!is.null(tbl)) line <- line & (as.integer(k) * 10L) %in% tbl$key100[!tbl$offchart]
  den <- 20 * 10^v
  # whole number, halves up; one decimal below 10
  calc <- ifelse(floor(den * 10 + 0.5) / 10 < 9.95, sprintf("%.1f", floor(den * 10 + 0.5) / 10),
                 format(floor(den + 0.5), scientific = FALSE, trim = TRUE))
  out[ok] <- paste0("20/", ifelse(line, VA_SNELLEN_LINES[pmin(pmax(k, -3), 17) + 4], calc))
  out
}
# m, s: mean and SD in logMAR; lo, hi: the best and worst eye (NA = no range)
fmt_va <- function(m, s, lo = NA_real_, hi = NA_real_, range = TRUE) {
  if (!is.finite(m)) return("no data")
  paste0(va_snellen(m), " (", fmt_fix(m, 2), " ± ", fmt_fix(s, 2), " logMAR)",
         if (isTRUE(range) && is.finite(lo) && is.finite(hi))
           paste0("; range ", va_snellen(lo), " to ", va_snellen(hi)) else "")
}

# ------------------------------------------------------------
# Column headers of the written tables (decision D24: the header "P"
# everywhere). The tables keep their internal names (the models and merges
# look them up); the writers rename them on the sheet.
# ------------------------------------------------------------
# "Adjusted P" is the P of the covariate-adjusted model, not a multiplicity
# adjustment: it is named so beside "P (Holm)".
TABLE_HEADER_NAMES <- c("p_value" = "P", "p value" = "P", "P value" = "P", "p.value" = "P",
                        "GEE p value" = "GEE P", "P value (Holm)" = "P (Holm)",
                        "Adjusted P" = "P (adjusted model)", "CI" = "Effect (95% CI)")
table_headers <- function(nms) {
  nms <- as.character(nms)
  hit <- nms %in% names(TABLE_HEADER_NAMES)
  nms[hit] <- unname(TABLE_HEADER_NAMES[nms[hit]])
  nms
}

# ------------------------------------------------------------
# Variable type map: clean_name -> one of
# continuous | ordinal | dichotomous | categorical
# A variable listed under more than one type keeps the most specific
# one (categorical > dichotomous > ordinal > continuous), and the
# conflict is logged so it never changes the analysis silently.
# ------------------------------------------------------------
build_type_map <- function(df_raw_names,
                           continuous_raw  = character(0),
                           ordinal_raw     = character(0),
                           dichotomous_raw = character(0),
                           categorical_raw = character(0),
                           logger = message) {
  lists <- list(continuous = continuous_raw, ordinal = ordinal_raw,
                dichotomous = dichotomous_raw, categorical = categorical_raw)
  out <- c()
  claimed <- list()
  for (ty in names(lists)) {
    if (length(lists[[ty]]) == 0) next
    tmp <- resolve_vec(df_raw_names, lists[[ty]], paste0(ty, "_vars"), logger)
    for (v in tmp) claimed[[v]] <- c(claimed[[v]], ty)
    out[tmp] <- ty
  }
  dup <- names(claimed)[vapply(claimed, function(t) length(unique(t)) > 1, logical(1))]
  # named by the header as typed in the file (contract R4-3)
  raw <- as.character(df_raw_names); raw[is.na(raw)] <- ""
  hdr <- function(v) { i <- match(v, clean_names(raw)); if (is.na(i)) v else raw[i] }
  for (v in dup)
    logger(sprintf(
      "Warning: variable '%s' is listed under more than one statistical type (%s); it is analyzed as %s. Keep it in one type list only.",
      hdr(v), paste(unique(claimed[[v]]), collapse = ", "), out[[v]]))
  out
}

get_var_type <- function(param_clean, type_map) {
  if (!is.null(type_map) && param_clean %in% names(type_map)) return(type_map[[param_clean]])
  NA_character_
}

# ------------------------------------------------------------
# Refraction standardization: plus-cyl -> minus-cyl
# ------------------------------------------------------------
# Axes in the clinical 1-180 range: 0 and 180 are the same meridian, and 180 is
# how it is recorded (and what the mEYEstro / AstigMATIC tabs expect).
wrap_axis_0_180 <- function(ax) {
  ax2 <- if (is.numeric(ax)) as.numeric(ax) else suppressWarnings(as.numeric(as.character(ax)))
  ax2 <- ax2 %% 180
  ax2[!is.na(ax2) & ax2 == 0] <- 180
  ax2
}

# Only rows recorded in plus-cylinder form are transposed (sph + cyl, -cyl,
# axis rotated by 90 into 1-180). Rows already in minus form keep their axis
# exactly as recorded (180 stays 180, 0 stays 0); an axis outside 0-180 is a
# data-entry problem and is logged, not rewritten (the figures reduce every
# axis modulo 180 themselves). The three columns become numeric, so a text
# cell becomes missing - and is counted in the log.
# display_names: optional clean -> original header map, for the log lines.
standardize_minus_cyl <- function(df, sph_clean, cyl_clean, axis_clean, logger = message,
                                  display_names = NULL) {
  if (any(is.na(c(sph_clean, cyl_clean, axis_clean)))) {
    logger("Standardization skipped: one or more columns not found."); return(df)
  }
  if (!(all(c(sph_clean, cyl_clean, axis_clean) %in% names(df)))) {
    logger("Standardization skipped: one or more columns missing in df."); return(df)
  }
  as_num_logged <- function(col) {
    raw <- df[[col]]
    x <- as_numeric_safe(raw)
    txt <- trimws(as.character(raw))
    bad <- sum(!is.na(txt) & nzchar(txt) & !(toupper(txt) %in% c("NA", "#VALUE!", "#DIV/0!")) & is.na(x))
    if (bad > 0)
      logger(sprintf("Standardization: %d non-numeric value(s) in '%s' set to missing.", bad,
                     disp_name(col, display_names)))
    x
  }
  sph <- as_num_logged(sph_clean)
  cyl <- as_num_logged(cyl_clean)
  ax  <- as_num_logged(axis_clean)
  idx <- which(is.finite(cyl) & cyl > 0)
  if (length(idx) > 0) {
    sph[idx] <- sph[idx] + cyl[idx]
    cyl[idx] <- -cyl[idx]
    ax[idx]  <- wrap_axis_0_180(ax[idx] + 90)
  }
  odd <- setdiff(which(is.finite(ax) & (ax < 0 | ax > 180)), idx)
  if (length(odd) > 0)
    logger(sprintf(
      "Standardization: %d axis value(s) in '%s' outside 0-180 left as recorded (e.g. %s) - check the data.",
      length(odd), disp_name(axis_clean, display_names),
      paste(utils::head(unique(ax[odd]), 3), collapse = ", ")))
  df[[sph_clean]]  <- sph
  df[[cyl_clean]]  <- cyl
  df[[axis_clean]] <- ax
  df
}

# ------------------------------------------------------------
# Refractive astigmatism in PLUS-cylinder format (JCRS 2025 astigmatism
# standard, decision D1): magnitude M >= 0 at the PLUS axis (the steep
# meridian), whatever form the refraction was typed in. A minus cylinder is
# transposed (axis + 90), a plus cylinder keeps its axis, so -2.00 x 180 and
# +2.00 x 90 are the same astigmatism (2.00 @ 90), row by row - also in a row
# that mixes the two forms - and with the minus-cylinder switch on or off. The
# vector code (graphs.R alpins_vectors_target) builds every double-angle
# vector from this, x = M cos 2θ, y = M sin 2θ, so with-the-rule astigmatism
# (plus axis 90) lies on the LEFT of a double-angle plot.
# Returns list(mag, axis, noaxis): the plus axis in [0, 180); NA where the
# cylinder is missing, or where a non-zero cylinder has no axis (no meridian:
# no vector; `noaxis` marks those eyes, for the log). A ZERO cylinder is the
# null vector whatever its axis cell holds - plano refractions are often
# exported with a blank axis - so it is kept, at axis 0 when the axis is blank.
# ------------------------------------------------------------
astig_plus <- function(cyl, axis) {
  num <- function(v) if (is.numeric(v)) as.numeric(v) else as_numeric_safe(v)
  c0 <- num(cyl); a0 <- num(axis)
  n <- max(length(c0), length(a0))
  c0 <- rep_len(c0, n); a0 <- rep_len(a0, n)
  zero <- is.finite(c0) & c0 == 0
  a0[zero & !is.finite(a0)] <- 0
  ok <- is.finite(c0) & is.finite(a0)
  mag <- ifelse(ok, abs(c0), NA_real_)
  ax <- ifelse(ok, ifelse(c0 < 0, a0 + 90, a0) %% 180, NA_real_)
  list(mag = mag, axis = ax, noaxis = is.finite(c0) & !zero & !is.finite(a0))
}

# ------------------------------------------------------------
# Filter engine (op-based). Same semantics as original script:
# a row whose value is missing is kept by every op except isna/notna.
# Numeric ops need numeric operands and a column with numbers (a clear
# error otherwise); in / notin match values via match_values().
# rules: list of list(var=, op=, value=)
# ------------------------------------------------------------
apply_filter_rules <- function(df, raw_names, rules, logger = message) {
  if (is.null(rules) || length(rules) == 0) return(df)
  out <- df
  num_ops <- c("==","!=","<","<=",">",">=","between","between_any","abs_ge","abs_le","abs_between")
  for (i in seq_along(rules)) {
    r <- rules[[i]]
    if (is.null(r$var) || is.null(r$op)) next
    var_clean <- resolve_raw_to_clean(raw_names, r$var)
    if (is.na(var_clean) || !(var_clean %in% names(out))) {
      why <- if (length(attr(var_clean, "ambiguous"))) paste0("column name is ambiguous - matches ",
        paste0("'", attr(var_clean, "ambiguous"), "'", collapse = ", ")) else "var not found"
      logger(paste0("Filter rule ", i, " skipped (", why, "): ", r$var)); next
    }
    op  <- tolower(trimws(as.character(r$op)))
    val <- r$value
    v <- out[[var_clean]]
    v_chr <- as.character(v)
    keep <- rep(TRUE, nrow(out))
    lab <- paste0("Filter rule ", i, " (", r$var, " ", r$op, ")")
    # numeric operands: exactly `n` finite numbers, or a clear error - never a
    # silently recycled vector, an NA that drops every row, or a zero-length keep
    nums <- function(x, n) {
      vv <- suppressWarnings(as.numeric(trimws(as.character(unlist(x)))))
      if (length(vv) != n || any(!is.finite(vv)))
        stop(lab, ": needs ", if (n == 1) "exactly one number" else paste(n, "numbers"),
             "; got '", paste(unlist(x), collapse = ", "),
             "'. For text columns use in / notin / contains.", call. = FALSE)
      vv
    }
    n_missing <- sum(is.na(v))

    if (op %in% num_ops) {
      v_num <- as_numeric_safe(v)
      if (any(!is.na(v)) && !any(is.finite(v_num)))
        stop(lab, ": column '", r$var, "' has no numeric values; '", r$op,
             "' compares numbers - use in / notin / contains for text columns.", call. = FALSE)
      n_missing <- sum(is.na(v_num))
      if (op == "between") {
        b <- nums(val, 2)
        keep <- is.na(v_num) | (v_num >= b[1] & v_num <= b[2])
      } else if (op == "between_any") {
        rngs <- if (is.list(val)) val else if (length(val) == 2) list(val) else list()
        if (length(rngs) == 0)
          stop(lab, ": 'between_any' needs value = list(c(low, high), ...).", call. = FALSE)
        in_any <- rep(FALSE, length(v_num))
        for (rng in rngs) {
          b <- nums(rng, 2)
          in_any <- in_any | (!is.na(v_num) & v_num >= b[1] & v_num <= b[2])
        }
        keep <- is.na(v_num) | in_any
      } else if (op == "abs_ge") {
        keep <- is.na(v_num) | (abs(v_num) >= nums(val, 1))
      } else if (op == "abs_le") {
        keep <- is.na(v_num) | (abs(v_num) <= nums(val, 1))
      } else if (op == "abs_between") {
        b <- nums(val, 2)
        keep <- is.na(v_num) | (abs(v_num) >= b[1] & abs(v_num) <= b[2])
      } else {
        vv <- nums(val, 1)
        if (op == "==") keep <- is.na(v_num) | (v_num == vv)
        if (op == "!=") keep <- is.na(v_num) | (v_num != vv)
        if (op == "<")  keep <- is.na(v_num) | (v_num <  vv)
        if (op == "<=") keep <- is.na(v_num) | (v_num <= vv)
        if (op == ">")  keep <- is.na(v_num) | (v_num >  vv)
        if (op == ">=") keep <- is.na(v_num) | (v_num >= vv)
      }
    } else if (op %in% c("in","notin")) {
      if (op == "in" && !length(stats::na.omit(unlist(val))))
        stop(lab, ": no values selected.", call. = FALSE)
      hit <- match_values(v, val)
      if (op == "in") keep <- is.na(v) | hit
      else            keep <- is.na(v) | !hit
    } else if (op %in% c("contains","not_contains")) {
      # LITERAL substring match (any of the given values), case-insensitive.
      # This used to go through grepl() as a raw regex, so "(OD)" or "+1.5"
      # silently got regex semantics - and could disagree with the Python
      # preview. fixed = TRUE avoids regex (and its escaping) entirely.
      parts <- tolower(as.character(val))
      v_low <- tolower(v_chr)
      hit <- rep(FALSE, length(v_low))
      for (p_ in parts) if (nzchar(p_)) hit <- hit | grepl(p_, v_low, fixed = TRUE)
      if (op == "contains")     keep <- is.na(v) | hit
      if (op == "not_contains") keep <- is.na(v) | !hit
    } else if (op == "isna") {
      keep <- is.na(v); n_missing <- 0
    } else if (op == "notna") {
      keep <- !is.na(v); n_missing <- 0
    } else {
      stop("Unsupported filter op in rule ", i, ": ", r$op)
    }

    if (length(keep) != nrow(out))
      stop(lab, ": internal error - the rule produced ", length(keep),
           " decisions for ", nrow(out), " rows.", call. = FALSE)
    n_before <- nrow(out)
    out <- out[keep %in% TRUE, , drop = FALSE]
    shown <- unlist(val)
    logger(paste0("Filter rule ", i, " applied: ", r$var, " ", r$op,
                  if (length(shown)) paste0(" ", paste(shown, collapse = ", ")) else "",
                  " -> removed ", n_before - nrow(out),
                  if (n_missing > 0) paste0(", kept ", n_missing,
                                            " with a missing or non-numeric value") else "",
                  "; remaining rows: ", nrow(out)))
    if (nrow(out) == 0) break
  }
  out
}

# Positive (displayed) level for a dichotomous variable. The preferred level is
# matched through resolve_positive_level() (case, spaces, 1 = 1.0 = TRUE). When
# it is absent because this cohort has only the other level (zero events after
# filtering or matching) the preference is still shown, so the row reads
# "(Yes) 0/30" instead of flipping to the other level. When it cannot be
# resolved although two levels are present, the documented default is used
# (as make_binary_01 does for the models) and the fallback is logged once.
# display_names: optional clean -> original header map, for the log line.
pick_display_level <- function(param_clean, v, dicho_pref_clean = list(), logger = NULL,
                               display_names = NULL) {
  vv <- unique(v[!is.na(v)])
  pref <- dicho_pref_clean[[param_clean]]
  if (length(vv) == 0) return(if (.has_pref(pref)) .pref_label(pref) else NA)
  if (.has_pref(pref)) {
    lev <- resolve_positive_level(pref, vv)
    if (length(lev) == 1 && !is.na(lev)) return(lev)
    if (length(vv) == 1) return(.pref_label(pref))
    fb <- default_positive_level(vv)
    log_once(logger, .pref_fallback_msg(disp_name(param_clean, display_names), pref, vv, fb))
    return(fb)
  }
  default_positive_level(vv)
}

# ------------------------------------------------------------
# Whether a between-group P value can be computed at all (table or
# figure; classical, clustered GEE or weighted GEE), checked BEFORE
# any test or model is fitted, so every path gives the same reason.
# Returns "" when it can, otherwise the reason:
#   "fewer than two groups have data for this variable"
#   "variable is constant in this cohort"
#   "B has n = 1" (continuous / ordinal: every group that has data
#                  needs >= 2 usable values; see small_group_note())
# A proportion ("dichotomous" / "proportion" / "categorical") needs
# one eye with data per group and both outcomes present overall.
# v: the values (numbers for continuous / ordinal; the event 0/1,
# TRUE/FALSE or the levels otherwise); g: the group of each value.
# A group with no usable value is not compared and is not named.
# ------------------------------------------------------------
p_value_guard <- function(v, g, kind = "continuous") {
  num <- kind %in% c("continuous", "ordinal")
  x <- if (!num) as.character(v) else if (is.numeric(v)) as.numeric(v) else as_numeric_safe(v)
  ok <- (if (num) is.finite(x) else !is.na(x)) & !is.na(g)
  gg <- as.character(g)[ok]
  if (length(unique(gg)) < 2) return("fewer than two groups have data for this variable")
  if (length(unique(x[ok])) < 2) return("variable is constant in this cohort")
  if (num) return(small_group_note(if (is.factor(g)) droplevels(g[ok]) else gg))
  ""
}

# "" when every group present in `g` (the group of each USABLE value) has at
# least two values, else "B has n = 1" naming each smaller group, in level
# order ("B has n = 1; C has n = 1").
small_group_note <- function(g) {
  g <- g[!is.na(g)]
  if (!length(g)) return("")
  lv <- if (is.factor(g)) levels(droplevels(g)) else unique(as.character(g))
  cnt <- table(factor(as.character(g), levels = lv))
  small <- cnt[cnt < 2]
  if (!length(small)) return("")
  paste0(names(small), " has n = ", as.integer(small), collapse = "; ")
}

# ------------------------------------------------------------
# Few patients per group in the clustered models: the GEE and adjusted-
# regression comparisons (robust sandwich SE, clustered by patient - or by eye
# when no patient ID is mapped) and the clustered / weighted figure P values.
# The robust SE estimates each group's variance from its patients (clusters):
#  - a group with ONE patient leaves nothing to estimate it from, so no
#    comparison involving it is estimated: "B has 1 patient"
#    (cluster_guard_note), the wording of the weighted tables and figures;
#  - with 2 to GEE_FEW_PATIENTS - 1 patients the robust SE can be too small, so
#    the result is reported with a caution (few_patients_caution) and the run
#    log names those groups once per run (few_patients_flush).
# id: the patient (cluster) of each usable value - a blank id is its own
# patient; g: the group of each usable value. Groups without data are not named.
# unit: what a cluster is - "patient", or "eye" when no patient ID is in scope
# and each eye is its own cluster; every count and text then says "eye(s)".
# ------------------------------------------------------------
GEE_FEW_PATIENTS <- 10L

# "patient" / "patients" (or "eye" / "eyes") for a count of n clusters.
cluster_unit_word <- function(unit = "patient", n = 2L) {
  u <- if (identical(unit, "eye")) "eye" else "patient"
  if (length(n) == 1 && !is.na(n) && n == 1) u else paste0(u, "s")
}

# Distinct patients per group, in level (else first-seen) order.
patients_per_group <- function(id, g) {
  ok <- !is.na(g)
  if (!any(ok)) return(stats::setNames(integer(0), character(0)))
  gg <- if (is.factor(g)) droplevels(g[ok])
        else factor(as.character(g[ok]), levels = unique(as.character(g[ok])))
  idv <- fill_missing_ids(id[ok])
  vapply(levels(gg), function(l) length(unique(idv[gg == l])), integer(1))
}

# The groups with a single patient (cluster): no comparison involving them is
# estimated. Named character, group -> "<group> has 1 patient" ("<group> has
# 1 eye" when unit = "eye"), in level (else first-seen) order; empty when every
# group holds at least two patients.
one_patient_groups <- function(id, g, unit = "patient") {
  npt <- patients_per_group(id, g)
  one <- names(npt)[npt < 2]
  if (!length(one)) return(stats::setNames(character(0), character(0)))
  stats::setNames(paste0(one, " has 1 ", cluster_unit_word(unit, 1L)), one)
}

# THE one-patient guard (contract R4-9: one implementation, used by the weighted
# table test, the GEE / regression models and the figures): "" when every group
# holds at least two patients, else "B has 1 patient" for each group that does
# not ("B has 1 patient; C has 1 patient"; "B has 1 eye" when unit = "eye").
# id: the patient of each usable value (blank = its own patient); g: its group.
cluster_guard_note <- function(id, g, unit = "patient") {
  one <- one_patient_groups(id, g, unit)
  if (!length(one)) "" else paste(unname(one), collapse = "; ")
}

# ------------------------------------------------------------
# One minimum-data rule for every clustered model (contract R4-10): the GEE
# sheets, the adjusted regression, the weighted table test (.weighted_wald)
# and the clustered table and figure GEE (gee_group_p). A model needs at least
# MODEL_MIN_EYES usable eyes, at least MODEL_MIN_CLUSTERS clusters overall,
# and more usable eyes than its parameters + 2.
#   n_rows    : the usable eyes (rows) the model would be fitted on
#   n_clusters: their distinct patients (clusters; = n_rows without patient ID)
#   n_params  : the model's coefficients INCLUDING the intercept (ncol of its
#               model matrix; for y ~ group: the number of groups)
#   unit      : "patient", or "eye" when each eye is its own cluster
# Returns NULL when the model may be fitted, else the reason, which callers
# print as "Not estimable - <reason>":
#   "too few eyes for this model (n = 5)"
#   "too few patients for this model (2 patients)" - fewer than 3 clusters
#     although the eyes suffice; the per-group rule "B has 1 patient"
#     (cluster_guard_note), checked first, normally catches this case.
# The per-group rules - 2 usable eyes (p_value_guard / small_group_note) and
# 2 patients (cluster_guard_note) per compared group - are separate and keep
# their own wording; so does the few-patients caution (2-9 patients). With the
# small-sample correction on (gee_small_sample "md") a model also needs more
# patients than coefficients for its t / F reference (gee_df_note).
# ------------------------------------------------------------
MODEL_MIN_EYES <- 6L
MODEL_MIN_CLUSTERS <- 3L
model_data_guard <- function(n_rows, n_clusters, n_params, unit = "patient") {
  n <- suppressWarnings(as.integer(n_rows[1]))
  if (!length(n) || is.na(n)) n <- 0L
  k <- suppressWarnings(as.integer(n_clusters[1]))
  if (!length(k) || is.na(k)) k <- n
  p <- suppressWarnings(as.numeric(n_params[1]))
  if (n < MODEL_MIN_EYES || (length(p) && is.finite(p) && n <= p + 2))
    return(sprintf("too few eyes for this model (n = %d)", n))
  if (k < MODEL_MIN_CLUSTERS)
    return(sprintf("too few %s for this model (%d %s)", cluster_unit_word(unit), k,
                   cluster_unit_word(unit, k)))
  NULL
}

# The groups with 2 to GEE_FEW_PATIENTS - 1 patients (named counts; attr
# "unit" says what was counted).
few_patient_groups <- function(id, g, unit = "patient") {
  npt <- patients_per_group(id, g)
  out <- npt[npt >= 2 & npt < GEE_FEW_PATIENTS]
  if (length(out)) attr(out, "unit") <- if (identical(unit, "eye")) "eye" else "patient"
  out
}

# "" or the caution for a reported comparison of these groups:
#   "Caution: B has 7 patients - robust standard errors can be too small with few patients"
# short = TRUE: the figure form, "Caution: B has 7 patients - robust SE can be too small".
# bootstrap = TRUE: the robust centroid test with the patient bootstrap
#   (contract R4-6), "Caution: B has 7 patients - a P value from so few
#   patients can be too small".
# With unit "eye" (attr "unit" of `few`): "Caution: B has 8 eyes - robust
# standard errors can be too small with few eyes".
# Every caution handed out is recorded for the run log (few_patients_flush),
# with the output that carries it (`source`): "tables" (the Note column of the
# GEE / regression sheets), "figures" (a figure's clustered or weighted GEE P)
# or "bootstrap" (the robust centroid test). By default: "bootstrap" when
# bootstrap = TRUE, "figures" for the short form, "tables" otherwise.
few_patients_caution <- function(id, g, short = FALSE, unit = "patient", source = NULL) {
  few <- few_patient_groups(id, g, unit)
  few_patients_text(few, short, source = source)
}
few_patients_text <- function(few, short = FALSE, bootstrap = FALSE, source = NULL) {
  if (!length(few)) return("")
  unit <- attr(few, "unit") %||% "patient"
  src <- source %||% (if (isTRUE(bootstrap)) "bootstrap" else if (isTRUE(short)) "figures" else "tables")
  few_patients_record(few, unit, src)
  paste0("Caution: ", paste0(names(few), " has ", as.integer(few), " ",
                             cluster_unit_word(unit), collapse = ", "),
         if (isTRUE(bootstrap)) paste0(" - a P value from so few ", cluster_unit_word(unit),
                                       " can be too small")
         else if (isTRUE(short)) " - robust SE can be too small"
         else paste0(" - robust standard errors can be too small with few ", cluster_unit_word(unit)))
}

# Run-level record of the cautions handed out (unit -> group -> counts seen,
# and the outputs that carry them), logged once per run by
# few_patients_flush(), which also clears it.
.few_patients <- new.env(parent = emptyenv())
.few_patients$seen <- list()
.few_patients$src <- character(0)
few_patients_record <- function(few, unit = "patient", source = "tables") {
  u <- if (identical(unit, "eye")) "eye" else "patient"
  s <- .few_patients$seen[[u]] %||% list()
  for (g in names(few)) s[[g]] <- sort(unique(c(s[[g]], as.integer(few[[g]]))))
  .few_patients$seen[[u]] <- s
  .few_patients$src <- union(.few_patients$src, as.character(source %||% "tables"))
  invisible(NULL)
}
few_patients_reset <- function() {
  .few_patients$seen <- list(); .few_patients$src <- character(0); invisible(NULL)
}
# One WARNING line (the interface lists the engine's WARNING lines under "Check
# before using the results"), naming each group and its counts, and ONLY the
# outputs that actually carry a caution (contract R4-10): without a patient ID
# the figures use classical tests and carry none, so they are not named then.
# where: e.g. " (run 'PRK')" when a configuration has several runs.
few_patients_flush <- function(logger = NULL, where = "") {
  seen <- .few_patients$seen
  src <- .few_patients$src
  few_patients_reset()
  if (!length(seen) || is.null(logger)) return(invisible(FALSE))
  units <- intersect(c("patient", "eye"), names(seen))
  what <- unlist(lapply(units, function(u) vapply(names(seen[[u]]), function(g) {
    k <- seen[[u]][[g]]
    paste0(g, " has ", if (length(k) > 1) paste0(min(k), "-", max(k)) else k, " ", cluster_unit_word(u))
  }, character(1))))
  per <- if (identical(units, "eye")) "eyes per group (each eye is its own cluster when no patient ID is in scope)"
         else if (identical(units, "patient")) "patients per group"
         else "patients (or eyes, where each eye is its own cluster) per group"
  se <- if (all(c("tables", "figures") %in% src)) "GEE, adjusted-regression and clustered figure P values"
        else if ("tables" %in% src) "GEE and adjusted-regression P values"
        else if ("figures" %in% src) "clustered figure P values"
  outs <- c(if (length(se)) paste0(se, " (robust standard errors)"),
            if ("bootstrap" %in% src) "the bootstrap P value of the robust centroid test")
  if (!length(outs)) outs <- "P values with robust standard errors"
  places <- c(if ("tables" %in% src) "in the Note column of the GEE / regression sheets",
              if (any(c("figures", "bootstrap") %in% src)) "under the figure's P values")
  if (!length(places)) places <- "next to those P values"
  logger(sprintf(paste0("WARNING%s: %s in a clustered comparison - %s can be too small with fewer ",
                        "than %d %s. Those results carry a caution %s."),
                 where, paste(what, collapse = ", "), paste(outs, collapse = " and "),
                 GEE_FEW_PATIENTS, per, paste(places, collapse = " and ")))
  invisible(TRUE)
}

# The Test-column label for a proportion that was not tested, from the reason
# p_value_guard() gave: no test is named that never ran.
p_guard_label <- function(why) {
  if (identical(why, "variable is constant in this cohort")) "Not applicable (variable constant in cohort)"
  else if (identical(why, "fewer than two groups have data for this variable"))
    "Not applicable (fewer than two groups have data)"
  else "Not tested"
}

# ------------------------------------------------------------
# Between-group test selector, shared by the summary tables
# (create_summary_table) and the figures (p_omnibus), so a table
# and a figure can never disagree when clustering is off.
# kind: "continuous" | "ordinal" | "dichotomous" | "categorical"
#       ("proportion" is accepted as "dichotomous").
# event: optional event level for a dichotomous variable (v is then
#        tested as event vs not-event); a logical v is used as is.
# Nothing is tested when p_value_guard() objects (e.g. "B has n = 1").
# Returns list(p = <numeric or NA>, test = <label of the test run>,
#              note = <reason when p is NA, else "">).
# ------------------------------------------------------------
group_test <- function(v, g, kind = c("continuous", "ordinal", "dichotomous", "categorical"),
                       event = NULL) {
  if (identical(kind, "proportion")) kind <- "dichotomous"
  kind <- match.arg(kind)
  done <- function(p, test, note) {
    p <- suppressWarnings(as.numeric(p))
    if (length(p) != 1 || !is.finite(p)) {
      p <- NA_real_
      if (!nzchar(note)) note <- "the test could not be computed"
    } else note <- ""
    list(p = p, test = test, note = note)
  }
  gf <- if (is.factor(g)) g else factor(as.character(g))
  k_all <- length(unique(as.character(gf[!is.na(gf)])))

  if (kind %in% c("continuous", "ordinal")) {
    x <- as_numeric_safe(v)
    ok <- is.finite(x) & !is.na(gf)
    x <- x[ok]
    gg <- droplevels(gf[ok])
    k <- nlevels(gg)
    many <- if (k >= 2) k > 2 else k_all > 2
    # no test at all when a group cannot support one (R3-1): the label still
    # names the test that would have run, the note says why it did not
    why <- p_value_guard(x, gg, kind)
    if (kind == "continuous") {
      lab <- if (many) "Welch ANOVA" else "Welch t-test"
      if (nzchar(why)) return(done(NA, lab, why))
      sds <- tapply(x, gg, stats::sd)
      if (k == 2) {
        if (all(sds == 0))
          return(done(NA, lab, "both groups have zero variance (each group is constant)"))
        # t.test stays defined when ONE group is constant (oneway.test -> NaN)
        p <- tryCatch(stats::t.test(x ~ gg, var.equal = FALSE)$p.value, error = function(e) NA_real_)
        return(done(p, "Welch t-test", ""))
      }
      if (all(sds > 0)) {
        p <- tryCatch(stats::oneway.test(x ~ gg, var.equal = FALSE)$p.value, error = function(e) NA_real_)
        return(done(p, "Welch ANOVA", ""))
      }
      # Welch ANOVA weights each group by n/s^2: undefined with a zero-variance group
      p <- tryCatch(stats::kruskal.test(x ~ gg)$p.value, error = function(e) NA_real_)
      return(done(p, "Kruskal-Wallis test (Welch ANOVA undefined: a group has zero variance)", ""))
    }
    lab <- if (many) "Kruskal-Wallis test" else "Wilcoxon rank-sum test"
    if (nzchar(why)) return(done(NA, lab, why))
    p <- if (k == 2)
      tryCatch(suppressWarnings(stats::wilcox.test(x ~ gg))$p.value, error = function(e) NA_real_)
    else tryCatch(stats::kruskal.test(x ~ gg)$p.value, error = function(e) NA_real_)
    return(done(p, lab, ""))
  }

  # dichotomous / categorical: contingency table
  if (kind == "dichotomous" && .has_pref(event)) {
    lev <- resolve_positive_level(event, v)
    lev <- if (length(lev) == 1 && !is.na(lev)) as.character(lev) else as.character(event[[1]])
    y <- ifelse(is.na(v), NA, as.character(v) == lev)
  } else if (is.logical(v)) {
    y <- v
  } else {
    y <- as.character(v)
  }
  why <- p_value_guard(y, gf, "dichotomous")
  if (nzchar(why)) return(done(NA, p_guard_label(why), why))
  ok <- !is.na(y) & !is.na(gf)
  tab <- table(y[ok], droplevels(gf[ok]))
  tab <- tab[rowSums(tab) > 0, colSums(tab) > 0, drop = FALSE]
  ex <- tryCatch(suppressWarnings(stats::chisq.test(tab)$expected), error = function(e) NULL)
  if (!is.null(ex) && all(ex >= 5)) {
    p <- tryCatch(suppressWarnings(stats::chisq.test(tab, correct = FALSE))$p.value,
                  error = function(e) NA_real_)
    return(done(p, "Chi-squared test", ""))
  }
  p <- tryCatch(stats::fisher.test(tab)$p.value, error = function(e) NULL)
  if (!is.null(p)) return(done(p, "Fisher's exact test", ""))
  # table too large for the exact network algorithm: Monte-Carlo Fisher with a
  # LOCALLY fixed seed, so identical runs give identical p values and the
  # caller's random stream is left exactly as it was
  B <- 20000L
  p <- with_local_seed(20250601L,
    tryCatch(stats::fisher.test(tab, simulate.p.value = TRUE, B = B)$p.value,
             error = function(e) NA_real_))
  done(p, "Fisher's exact test (Monte Carlo, 20,000 replicates)", "")
}

# Evaluate `expr` under set.seed(seed) and restore the global RNG state after.
with_local_seed <- function(seed, expr) {
  genv <- globalenv()
  had <- exists(".Random.seed", envir = genv, inherits = FALSE)
  old <- if (had) get(".Random.seed", envir = genv, inherits = FALSE) else NULL
  on.exit({
    if (had) assign(".Random.seed", old, envir = genv)
    else if (exists(".Random.seed", envir = genv, inherits = FALSE))
      rm(".Random.seed", envir = genv)
  }, add = TRUE)
  set.seed(seed)
  expr
}

# ------------------------------------------------------------
# Covariates read as text. readxl makes a whole column text when a single
# cell holds '#VALUE!', 'NA', '<0.5' or a stray space; such a covariate would
# enter a model as one dummy per distinct value. A text covariate whose
# non-blank cells are >= 90 % numbers is coerced to numeric (the rest become
# missing) - unless EVERY cell is a number and there are <= 10 distinct
# values, which reads as a deliberately text-coded category (surgeon 1/2/3)
# and stays categorical. A text covariate with more than `max_levels` levels
# that is not numeric is excluded. Every decision is logged once.
# Returns list(data, dropped = <excluded covariates>, notes).
# ------------------------------------------------------------
# display_names: optional clean -> original header map; the notes name the
# column by its header as typed in the file (contract R4-3).
coerce_numeric_covs <- function(df, covs, logger = NULL, min_share = 0.9, max_levels = 10,
                                display_names = NULL) {
  notes <- character(0); dropped <- character(0)
  for (cv in intersect(as.character(covs), names(df))) {
    v <- df[[cv]]
    cvn <- disp_name(cv, display_names)
    if (!is.character(v)) next
    nb <- !is.na(v) & nzchar(trimws(v))
    if (!any(nb)) next
    x <- as_numeric_safe(v)
    num_ok <- nb & is.finite(x)
    n_lev <- length(unique(trimws(v[nb])))
    if (sum(num_ok) >= min_share * sum(nb) &&
        (any(nb & !num_ok) || n_lev > max_levels)) {
      bad <- sum(nb & !num_ok)
      x[!num_ok] <- NA_real_
      df[[cv]] <- x
      notes <- c(notes, sprintf(
        "Covariate '%s' was read as text; used as numeric (%d non-numeric cell%s set to missing).",
        cvn, bad, if (bad == 1) "" else "s"))
    } else if (all(!nb | num_ok)) {
      notes <- c(notes, sprintf(
        "Covariate '%s' is stored as text with %d numeric codes; treated as categorical (store it as numbers in Excel to use it as a numeric covariate).",
        cvn, n_lev))
    } else if (n_lev > max_levels) {
      dropped <- c(dropped, cv)
      notes <- c(notes, sprintf(
        "Covariate '%s' has %d distinct text values; excluded from the model - check that the column is numeric.",
        cvn, n_lev))
    }
  }
  for (m in notes) log_once(logger, m)
  list(data = df, dropped = dropped, notes = notes)
}

# ------------------------------------------------------------
# openxlsx writes, for EVERY worksheet, a .rels file declaring a drawing and a
# vmlDrawing relationship - and matching [Content_Types] overrides - even when
# no drawing part is ever created. Excel tolerates the dangling references;
# openpyxl (and other strict OOXML readers) resolve them eagerly and fail, so
# every workbook this app produced crashed a plain openpyxl.load_workbook().
# Rewrite the archive, dropping ONLY drawing/vmlDrawing references whose
# target part does not exist. Everything else is untouched. The path is made
# absolute first (the archive is built from inside a temp folder), and the new
# archive is written next to the workbook and swapped in only once it is
# complete, so a failure can never leave a truncated workbook behind.
# ------------------------------------------------------------
sanitize_xlsx <- function(path, logger = NULL) {
  ok <- tryCatch({
    path <- normalizePath(path, winslash = "/", mustWork = TRUE)
    td <- tempfile("xlsx_fix_")
    dir.create(td)
    on.exit(unlink(td, recursive = TRUE), add = TRUE)
    utils::unzip(path, exdir = td)
    members <- gsub("\\\\", "/", list.files(td, recursive = TRUE, all.files = TRUE))

    rels_dir <- file.path(td, "xl", "worksheets", "_rels")
    n_fixed <- 0L
    for (f in list.files(rels_dir, full.names = TRUE, pattern = "[.]rels$")) {
      txt <- paste(readLines(f, warn = FALSE, encoding = "UTF-8"), collapse = "\n")
      pat <- '<Relationship[^>]*Type="[^"]*(/drawing|/vmlDrawing)"[^>]*/>'
      hits <- regmatches(txt, gregexpr(pat, txt))[[1]]
      changed <- FALSE
      for (h in hits) {
        tgt <- sub('.*Target="([^"]+)".*', "\\1", h)
        resolved <- sub("^([.][.]/)+", "xl/", tgt)   # ../drawings/x -> xl/drawings/x
        if (!(resolved %in% members)) {
          txt <- sub(h, "", txt, fixed = TRUE)
          changed <- TRUE
        }
      }
      if (changed) { writeLines(txt, f, useBytes = TRUE); n_fixed <- n_fixed + 1L }
    }

    # docProps/app.xml: openxlsx names "Microsoft Excel" as the application
    # that wrote the file; it is StandardEyes (D25, finding T1-16)
    n_app <- 0L
    apf <- file.path(td, "docProps", "app.xml")
    if (file.exists(apf)) {
      ax <- paste(readLines(apf, warn = FALSE, encoding = "UTF-8"), collapse = "\n")
      ver <- tryCatch(as.character(standardeyes_version()$public)[1], error = function(e) NA_character_)
      app_name <- paste0("StandardEyes", if (length(ver) && !is.na(ver)) paste0(" ", ver) else "")
      ax2 <- sub("<Application>[^<]*</Application>", paste0("<Application>", app_name, "</Application>"), ax)
      if (!identical(ax2, ax)) { writeLines(ax2, apf, useBytes = TRUE); n_app <- 1L }
    }

    ctf <- file.path(td, "[Content_Types].xml")
    if (n_fixed > 0 && file.exists(ctf)) {
      ct <- paste(readLines(ctf, warn = FALSE, encoding = "UTF-8"), collapse = "\n")
      pat <- '<Override[^>]*PartName="/xl/drawings/[^"]+"[^>]*/>'
      hits <- regmatches(ct, gregexpr(pat, ct))[[1]]
      changed <- FALSE
      for (h in hits) {
        part <- sub('.*PartName="/([^"]+)".*', "\\1", h)
        if (!(part %in% members)) { ct <- sub(h, "", ct, fixed = TRUE); changed <- TRUE }
      }
      if (changed) writeLines(ct, ctf, useBytes = TRUE)
    }

    if (n_fixed > 0 || n_app > 0) {
      tmp <- file.path(dirname(path), paste0(".~sanitize_", basename(path), ".tmp"))
      on.exit(if (file.exists(tmp)) unlink(tmp), add = TRUE)
      zip::zip(zipfile = tmp,
               files = list.files(td, recursive = TRUE, all.files = TRUE),
               recurse = FALSE, include_directories = FALSE, root = td)
      n_in <- nrow(utils::unzip(tmp, list = TRUE))
      if (n_in != length(members))
        stop("rebuilt archive has ", n_in, " of ", length(members), " parts")
      if (!suppressWarnings(file.rename(tmp, path)) &&
          !suppressWarnings(file.copy(tmp, path, overwrite = TRUE)))
        stop("could not replace the workbook (is it open in another program?)")
      if (!is.null(logger) && n_fixed > 0)
        logger(paste0("Workbook sanitized for interoperability (",
                      n_fixed, " sheet rels cleaned)."))
    }
    TRUE
  }, error = function(e) {
    if (!is.null(logger)) logger(paste0("Workbook sanitize skipped: ", conditionMessage(e)))
    FALSE
  })
  invisible(ok)
}

# ------------------------------------------------------------
# Replace a text cell of a written workbook in place: the first match of the
# regular expression `pattern` in its shared strings (and inline strings) becomes
# `replacement`. Nothing else in the archive is touched - no reload through
# openxlsx, which could change styles or the author - and the new archive is
# swapped in only once complete, as in sanitize_xlsx. Used for the Manifest's
# run duration, known only after the figures are drawn (decision D25). Returns
# TRUE when a cell was replaced; never stops a run (a failure is logged).
# ------------------------------------------------------------
xlsx_replace_text <- function(path, pattern, replacement, logger = NULL) {
  ok <- tryCatch({
    path <- normalizePath(path, winslash = "/", mustWork = TRUE)
    td <- tempfile("xlsx_txt_")
    dir.create(td)
    on.exit(unlink(td, recursive = TRUE), add = TRUE)
    utils::unzip(path, exdir = td)
    members <- gsub("\\\\", "/", list.files(td, recursive = TRUE, all.files = TRUE))
    parts <- members[members == "xl/sharedStrings.xml" | grepl("^xl/worksheets/[^/]+[.]xml$", members)]
    hit <- FALSE
    for (m in parts) {
      f <- file.path(td, m)
      txt <- paste(readLines(f, warn = FALSE, encoding = "UTF-8"), collapse = "\n")
      if (!grepl(pattern, txt, perl = TRUE)) next
      new <- sub(pattern, replacement, txt, perl = TRUE)
      con <- file(f, open = "wb")
      writeBin(charToRaw(enc2utf8(new)), con)
      close(con)
      hit <- TRUE
      break
    }
    if (hit) {
      tmp <- file.path(dirname(path), paste0(".~text_", basename(path), ".tmp"))
      on.exit(if (file.exists(tmp)) unlink(tmp), add = TRUE)
      zip::zip(zipfile = tmp, files = members, recurse = FALSE, include_directories = FALSE, root = td)
      n_in <- nrow(utils::unzip(tmp, list = TRUE))
      if (n_in != length(members))
        stop("rebuilt archive has ", n_in, " of ", length(members), " parts")
      if (!suppressWarnings(file.rename(tmp, path)) &&
          !suppressWarnings(file.copy(tmp, path, overwrite = TRUE)))
        stop("could not replace the workbook (is it open in another program?)")
    }
    hit
  }, error = function(e) {
    if (!is.null(logger)) logger(paste0("Workbook text not updated (", basename(path), "): ", conditionMessage(e)))
    FALSE
  })
  invisible(isTRUE(ok))
}
