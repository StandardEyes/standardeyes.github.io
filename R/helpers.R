# ============================================================
# helpers.R
# Name cleaning, IO, formatting, variable typing, and the shared
# helpers the other engine files call: visual-acuity parsing,
# value / level matching, sheet and file names, and the single
# between-group test selector used by tables AND figures.
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
WALD_RCOND_MIN <- 1e-8
gee_vcov_singular <- function(fit, idx) {
  V <- tryCatch(fit$geese$vbeta, error = function(e) NULL)
  if (is.null(V) || !length(idx) || any(dim(V) < max(idx))) return(TRUE)
  Vg <- V[idx, idx, drop = FALSE]
  if (any(!is.finite(Vg))) return(TRUE)
  rc <- tryCatch(rcond(Vg), error = function(e) NA_real_)
  Vn <- tryCatch(fit$geese$vbeta.naiv, error = function(e) NULL)
  dn <- if (is.null(Vn) || any(dim(Vn) < max(idx))) NULL else diag(Vn)[idx]
  !is.finite(rc) || rc < WALD_RCOND_MIN ||
    (!is.null(dn) && any(is.finite(dn) & dn > 0 & diag(Vg) < WALD_RCOND_MIN * dn))
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

fmt_p <- function(p){
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
      "Warning: variable '%s' is listed under more than one statistical type (%s); it is analysed as %s. Keep it in one type list only.",
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
# and the figure GEE (graphs.R gee_group_p). A model needs at least
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
# their own wording; so does the few-patients caution (2-9 patients).
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

    if (n_fixed > 0) {
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
      if (!is.null(logger))
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
