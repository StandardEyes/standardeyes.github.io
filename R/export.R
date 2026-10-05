# ============================================================
# export.R
# Excel export: mEYEstro / Alpins tabs and the workbook builder
# that assembles one full output workbook for a prepared,
# grouped dataset (N groups; reference-pairwise matching).
# ============================================================

# ------------------------------------------------------------
# Embedded Decimal -> Snellen conversion (replaces the old
# external 'Decimal to Snellen.xlsx' VLOOKUP formulas).
# Table = Sheet2!A2:B194 of that workbook, embedded at
# engine/data/snellen_lookup.csv, plus 15 rows appended after it for the
# keys that table lacked (logMAR 0.78-0.80 -> 125, 1.38-1.52 -> 500 / 630,
# 1.68-1.76 -> 1000). First match wins, like VLOOKUP, so an appended row
# never replaces a line the original table already gives.
# ------------------------------------------------------------
.snellen_tbl_cache <- new.env(parent = emptyenv())

# Folder this file was sourced from (NA when it cannot be told), so the lookup
# table is found whatever the working directory of the R process is.
.export_src_dir <- local({
  d <- NA_character_
  for (i in rev(seq_len(sys.nframe()))) {
    f <- tryCatch(get0("ofile", envir = sys.frame(i), inherits = FALSE), error = function(e) NULL)
    if (is.character(f) && length(f) == 1 && nzchar(f)) {
      d <- dirname(normalizePath(f, winslash = "/", mustWork = FALSE)); break
    }
  }
  d
})

# Looked up, in order, under: the app root set by engine/run_pipeline.R
# (option standardeyes.app_root), the app folder this file lives in, then the
# working directory and its parent. NULL when none has it (the caller logs it).
get_snellen_table <- function() {
  if (!is.null(.snellen_tbl_cache$tbl)) return(.snellen_tbl_cache$tbl)
  roots <- c(getOption("standardeyes.app_root", NA_character_),
             if (!is.na(.export_src_dir)) dirname(.export_src_dir) else NA_character_,
             ".", "..")
  roots <- roots[!is.na(roots) & nzchar(roots)]
  candidates <- file.path(roots, "engine", "data", "snellen_lookup.csv")
  fp <- candidates[file.exists(candidates)][1]
  if (is.na(fp)) return(NULL)
  tbl <- tryCatch(utils::read.csv(fp, stringsAsFactors = FALSE, colClasses = "character"),
                  error = function(e) NULL)
  if (is.null(tbl) || !all(c("logmar", "snellen") %in% names(tbl))) return(NULL)
  tbl$key100 <- as.integer(round(as.numeric(tbl$logmar) * 100))
  # CF / HM / LP / NLP rows (label not a Snellen denominator): never matched
  # by a numeric acuity, only by a cell recorded as that text
  tbl$offchart <- !grepl("^[0-9]", trimws(tbl$snellen))
  .snellen_tbl_cache$tbl <- tbl
  tbl
}

# ------------------------------------------------------------
# Version of the app (decision D25). ONE source, read by the server and the
# engine alike: the constants PUBLIC_VERSION (release number) and APP_VERSION
# (build id) in webapp/server/app.py, which the release tools also read. The
# engine reads them from that file here, so a run from the command line
# records its version and build in the Manifest exactly as a run from the
# interface does. Looked up under the same roots as the Snellen chart.
# Returns list(public, build); NA for a value that cannot be read.
# ------------------------------------------------------------
.version_cache <- new.env(parent = emptyenv())
standardeyes_version <- function() {
  if (!is.null(.version_cache$v)) return(.version_cache$v)
  roots <- c(getOption("standardeyes.app_root", NA_character_),
             if (!is.na(.export_src_dir)) dirname(.export_src_dir) else NA_character_,
             ".", "..")
  roots <- roots[!is.na(roots) & nzchar(roots)]
  fp <- file.path(roots, "webapp", "server", "app.py")
  fp <- fp[file.exists(fp)][1]
  v <- list(public = NA_character_, build = NA_character_)
  if (!is.na(fp)) {
    txt <- tryCatch(readLines(fp, warn = FALSE, encoding = "UTF-8"), error = function(e) character(0))
    get1 <- function(nm) {
      hit <- regmatches(txt, regexec(paste0("^", nm, "\\s*=\\s*[\"']([^\"']+)[\"']"), txt))
      hit <- Filter(length, hit)
      if (length(hit)) hit[[1]][2] else NA_character_
    }
    v <- list(public = get1("PUBLIC_VERSION"), build = get1("APP_VERSION"))
  }
  .version_cache$v <- v
  v
}

# The author recorded in every workbook (docProps/core.xml creator and last
# modified by): the app, never the Windows user name (decision D25).
WORKBOOK_CREATOR <- "StandardEyes"
# The Manifest's "Run duration (s)" cell as written with the workbook (the time
# up to that workbook); pipeline.R replaces it with the whole run's duration
# after the figures (helpers.R xlsx_replace_text)
MANIFEST_DURATION_RE <- "[0-9]+[.][0-9] \\(seconds from the start of the run to the writing of this workbook[^)<]*\\)"

# Excel-style ROUND(x, 0): halves round AWAY from zero (R's round() rounds
# half to even, which can land one table row off at boundaries).
excel_round0 <- function(x) trunc(x + 0.5 * sign(x))

# The key the original Excel formula built, TRUNC(ROUND(-LOG10(va)/0.02, 0)
# * 0.02, 2), in integer hundredths of logMAR: ROUND(.) * 2. (Multiplying by
# 0.02 and truncating in floating point loses a step at some keys, e.g.
# 29 * 0.02 * 100 = 57.999...) A key the chart does not list - far beyond
# either end - gets its computed Snellen denominator. Acuities are never
# floored: 0.02 reads 20/1000, not the 20/350 line. Only the logMAR range the
# pipeline accepts (-0.5 to 3.0) is converted; an acuity outside it has no
# meaningful Snellen line and stays blank (attr "n_out_of_range" counts them).
# A numeric acuity never receives an off-chart label: logMAR 1.80 / 2.30 /
# 2.80 / 3.00 get their computed denominator like their neighbours. CF / HM /
# LP / NLP are written only for a cell recorded as that text: `offchart` is
# the label parse_va() gave the recorded cell (the pipeline's
# .va_offchart__<col> stash), used where the decimal acuity is missing. Text
# input without that vector is read by parse_va() here.
SNELLEN_K100_RANGE <- c(-50L, 300L)
decimal_to_snellen <- function(va, offchart = NULL) {
  tbl <- get_snellen_table()
  va_num <- as_numeric_safe(va)
  out <- rep("", length(va_num))
  attr(out, "n_out_of_range") <- 0L
  if (is.null(tbl)) return(out)
  if (is.null(offchart) && (is.character(va) || is.factor(va)) && exists("parse_va", mode = "function"))
    offchart <- parse_va(va, "decimal")$offchart
  if (length(offchart) == length(va_num)) {
    lab <- as.character(offchart)
    keep <- !is.finite(va_num) & !is.na(lab) & lab %in% tbl$snellen[tbl$offchart]
    out[keep] <- lab[keep]
  }
  ok <- is.finite(va_num) & va_num > 0
  if (!any(ok)) return(out)
  k100 <- as.integer(excel_round0(-log10(va_num[ok]) / 0.02) * 2)
  inr <- k100 >= SNELLEN_K100_RANGE[1] & k100 <= SNELLEN_K100_RANGE[2]
  hit <- match(k100, ifelse(tbl$offchart, NA_integer_, tbl$key100))
  calc <- format(round(20 * 10^(k100 / 100)), scientific = FALSE, trim = TRUE)
  out[ok] <- ifelse(!inr, "", ifelse(is.na(hit), calc, tbl$snellen[hit]))
  attr(out, "n_out_of_range") <- sum(!inr)
  out
}

# Clean column of a role, or NA when the role is absent, empty or unmapped.
.role_col <- function(roles, nm) {
  cc <- roles[[nm]]
  if (is.null(cc) || length(cc) == 0 || is.na(cc[1]) || !nzchar(as.character(cc[1])))
    return(NA_character_)
  as.character(cc[1])
}

# Groups in level order (reference first), skipping empty ones.
.group_levels <- function(df) {
  if (is.factor(df$GroupLabel)) levels(droplevels(df$GroupLabel))
  else unique(as.character(stats::na.omit(df$GroupLabel)))
}

# Intended postoperative refraction per eye for the mEYEstro tab: SEQ from the
# target_seq role (absent = plano), cylinder and axis from the astigmatism
# target (absent = none). Sphere = SEQ - cyl / 2, written in minus-cylinder
# form (a positive target cylinder is transposed), so the intended SEQ is the
# same one the figures use.
# Only a cylinder target mapped (astigmatism target cyl + axis, no target_seq
# role): the intended SEQ is 0, i.e. intended sphere = -cyl / 2 (plano
# spherical equivalent with the targeted cylinder). A cylinder column without
# an axis column is dropped by the pipeline, so the intended refraction is
# then plano with no cylinder.
# A blank target cell is read as the astigmatism vectors read it (graphs.R
# alpins_vectors_target): a blank SEQ target is plano, a blank or zero target
# cylinder is no cylinder (its axis irrelevant); a non-zero target cylinder
# WITHOUT an axis is no target - that eye's intended sphere, cylinder and axis
# are left blank (the SEQ cannot be split without the cylinder's meridian),
# never written as a cylinder at axis 0. Each count is logged once per tab.
meyestro_intended <- function(dfg, seq_col, target = list(), logger = NULL, what = "") {
  n <- nrow(dfg)
  has <- function(cc) !is.null(cc) && length(cc) == 1 && !is.na(cc) && cc %in% names(dfg)
  from_seq <- has(seq_col)
  from_ast <- length(target) == 2 && has(target$cyl) && has(target$axis)
  s  <- if (from_seq) as_numeric_safe(dfg[[seq_col]]) else rep(0, n)
  cy <- if (from_ast) as_numeric_safe(dfg[[target$cyl]]) else rep(0, n)
  ax <- if (from_ast) as_numeric_safe(dfg[[target$axis]]) else rep(0, n)
  tab <- if (nzchar(what)) paste0(" '", what, "'") else ""
  noaxis <- is.finite(cy) & cy != 0 & !is.finite(ax)
  blank <- (!is.finite(s) | !is.finite(cy)) & !noaxis
  if (any(blank) && !is.null(logger))
    log_once(logger, sprintf("mEYEstro tab%s: %d eye(s) with a blank target cell written as plano (blank SEQ target = 0, blank target cylinder = no cylinder), as the astigmatism vectors read them.",
                             tab, sum(blank)))
  if (any(noaxis) && !is.null(logger))
    log_once(logger, sprintf("mEYEstro tab%s: %d eye(s) have a target cylinder without an axis - intended refraction left blank.",
                             tab, sum(noaxis)))
  s[!is.finite(s)] <- 0
  cy[!is.finite(cy)] <- 0
  # no intended cylinder: the axis is meaningless and written as 0 (R4-9)
  ax[cy == 0] <- 0
  sp <- s - cy / 2
  pos <- is.finite(cy) & cy > 0
  sp[pos] <- sp[pos] + cy[pos]
  cy[pos] <- -cy[pos]
  ax[pos] <- wrap_axis_0_180(ax[pos] + 90)
  sp[noaxis] <- NA_real_; cy[noaxis] <- NA_real_; ax[noaxis] <- NA_real_
  list(sph = sp, cyl = cy, axis = ax, from_seq = from_seq, from_astig = from_ast)
}

# Where the mEYEstro Intended_* columns come from, for the log and Manifest.
# display_names: optional clean key -> header map (R4-3) naming the columns.
meyestro_intended_source <- function(roles, target = list(), display_names = NULL) {
  dn <- function(x) disp_name(x, display_names)
  sc <- .role_col(roles, "target_seq")
  ast <- length(target) == 2
  parts <- c(if (!is.na(sc)) paste0("SEQ from '", dn(sc), "'") else "SEQ plano (no target SEQ mapped)",
             if (ast) paste0("cylinder/axis from '", dn(target$cyl), "' / '", dn(target$axis), "'")
             else "no cylinder (no astigmatism target mapped)")
  paste0(paste(parts, collapse = "; "), "; vertex distance 12 mm assumed")
}

# One mEYEstro-format tab per group. pairing = NULL for the full cohort, else
# the matched pairing label: the format and the matched marker lead the sheet
# name ("mEYEstro_<group>", "mEYEstro_M_<group>_<pairing>") so they survive
# Excel's 31-character truncation. Columns A-D (group, decimal acuities) are
# for reference; E-S are the 15 columns of the mEYEstro template in its
# order; T (postop SEQ) is for reference. Missing roles leave a blank column
# (logged); with fewer than 6 of the 10 roles the tabs are skipped (logged).
# Acuities are written as the canonical decimal values (no floor); a cell
# recorded as CF / HM / LP / NLP shows that label in its Snellen column.
# Refractions are written in minus-cylinder notation, as the template
# requires, whatever the analysis data carry: a plus-cylinder row is
# transposed in the tab only (the SEQ is unchanged by the transposition).
make_meyestro_tabs <- function(df, wb, roles, pairing = NULL, target = list(), logger = message,
                               display_names = NULL) {
  r <- function(nm) .role_col(roles, nm)
  need_roles <- c("pre_bcva","post_ucva","post_bcva",
                  "pre_sph","pre_cyl","pre_axis","post_sph","post_cyl","post_axis","post_seq")
  cols <- vapply(need_roles, r, character(1))
  found <- !is.na(cols) & cols %in% names(df)
  miss <- need_roles[!found]
  if (sum(found) < 6) {
    log_once(logger, paste0("mEYEstro tabs skipped - only ", sum(found), " of the ", length(need_roles),
                            " required roles are mapped to a column in the data (at least 6 needed); ",
                            "missing: ", paste(miss, collapse = ", "), "."))
    return(invisible(NULL))
  }
  if (length(miss))
    log_once(logger, paste0("mEYEstro tabs written with blank columns for the unmapped role(s): ",
                            paste(miss, collapse = ", "), " - mEYEstro needs every column filled."))
  if (is.null(get_snellen_table()))
    log_once(logger, paste0("Snellen lookup table (engine/data/snellen_lookup.csv) not found - ",
                            "the Snellen columns of the mEYEstro tabs are left blank."))
  log_once(logger, paste0("mEYEstro tabs: intended refraction - ",
                          meyestro_intended_source(roles, target, display_names), "."))

  gray   <- openxlsx::createStyle(fgFill = "#D3D3D3", halign = "center", textDecoration = "bold")
  yellow <- openxlsx::createStyle(fgFill = "#FFFF99", halign = "center", textDecoration = "bold")
  green  <- openxlsx::createStyle(fgFill = "#CCFFCC", halign = "center", textDecoration = "bold")
  red    <- openxlsx::createStyle(fgFill = "#FF9999", halign = "center", textDecoration = "bold")
  blue   <- openxlsx::createStyle(fgFill = "#99CCFF", halign = "center", textDecoration = "bold")

  for (g in .group_levels(df)) {
    dfg <- df[!is.na(df$GroupLabel) & as.character(df$GroupLabel) == g, , drop = FALSE]
    tab_what <- paste0(g, if (is.null(pairing)) "" else paste0(" (", pairing, ")"))
    n <- nrow(dfg)
    it <- meyestro_intended(dfg, r("target_seq"), target, logger = logger, what = tab_what)
    filled <- data.frame(
      Preop_MR_Vertex_Distance = rep(12, n), Intended_Postop_MR_Sphere = it$sph,
      Intended_Postop_MR_Cylinder = it$cyl, Intended_Postop_MR_Axis = it$axis,
      Intended_Postop_MR_Vertex_Distance = rep(12, n), Postop_MR_Vertex_Distance = rep(12, n))

    pick <- function(role) {
      cc <- r(role)
      if (is.na(cc) || !(cc %in% names(dfg))) return(rep(NA, n))
      dfg[[cc]]
    }
    # acuity: the canonical DECIMAL value stashed by the pipeline, whatever the
    # recorded notation and whether or not the analysis runs on logMAR (the
    # recorded column read as decimal when there is no stash), rounded to 10
    # significant digits so a logMAR round trip shows 0.78, not 0.779999...
    pick_va <- function(role) {
      cc <- r(role)
      if (is.na(cc)) return(rep(NA_real_, n))
      st <- paste0(".pre_logmar__", cc)
      v <- if (st %in% names(dfg)) dfg[[st]] else if (cc %in% names(dfg)) dfg[[cc]] else rep(NA, n)
      signif(as_numeric_safe(v), 10)
    }
    # off-chart label (CF / HM / LP / NLP) parse_va() gave the recorded cell
    pick_off <- function(role) {
      cc <- r(role)
      st <- paste0(".va_offchart__", cc)
      if (!is.na(cc) && st %in% names(dfg)) return(as.character(dfg[[st]]))
      if (!is.na(cc) && cc %in% names(dfg) && (is.character(dfg[[cc]]) || is.factor(dfg[[cc]])) &&
          exists("parse_va", mode = "function"))
        return(parse_va(dfg[[cc]], "decimal")$offchart)
      rep(NA_character_, n)
    }

    # Snellen columns computed from the embedded table (no external workbook)
    snel <- function(role) {
      x <- decimal_to_snellen(pick_va(role), offchart = pick_off(role))
      k <- attr(x, "n_out_of_range")
      if (isTRUE(k > 0))
        logger(sprintf("mEYEstro tab '%s': %d %s acuity value(s) outside logMAR -0.5 to 3.0 have no Snellen line - the Snellen cell is left blank.",
                       tab_what, k, role))
      as.vector(x)
    }
    formulas <- data.frame(
      Preop_CDVA  = snel("pre_bcva"),
      Postop_UDVA = snel("post_ucva"),
      Postop_CDVA = snel("post_bcva"),
      stringsAsFactors = FALSE)
    # template order (block E-S): 1-3 preop sphere/cylinder/axis, 4 vertex,
    # 5 preop CDVA, 6-9 intended sphere/cylinder/axis/vertex, 10-13 postop
    # sphere/cylinder/axis/vertex, 14 postop CDVA, 15 postop UDVA
    out <- data.frame(
      GroupLabel = rep(as.character(g), n),
      Pre_BCVA = pick_va("pre_bcva"), Post_UCVA = pick_va("post_ucva"), Post_BCVA = pick_va("post_bcva"),
      Pre_Sph = pick("pre_sph"), Pre_Cyl = pick("pre_cyl"), Pre_Axis = pick("pre_axis"),
      filled[, "Preop_MR_Vertex_Distance", drop = FALSE],
      formulas[, "Preop_CDVA", drop = FALSE],
      filled[, c("Intended_Postop_MR_Sphere","Intended_Postop_MR_Cylinder",
                 "Intended_Postop_MR_Axis","Intended_Postop_MR_Vertex_Distance"), drop = FALSE],
      Post_Sph = pick("post_sph"), Post_Cyl = pick("post_cyl"), Post_Axis = pick("post_axis"),
      filled[, "Postop_MR_Vertex_Distance", drop = FALSE],
      formulas[, c("Postop_CDVA","Postop_UDVA"), drop = FALSE],
      Post_SEQ = pick("post_seq"),
      check.names = FALSE, stringsAsFactors = FALSE)

    # minus-cylinder notation for the template (export copy only). "plano" /
    # "pl" in a sphere or cylinder cell is 0; any other text cell cannot be
    # written as a number and is logged with the tab it belongs to.
    n_plus <- 0L
    tab_log <- function(m) logger(paste0("mEYEstro tab '", tab_what, "': ", m))
    for (s in c("Pre", "Post")) {
      sc <- paste0(s, c("_Sph", "_Cyl", "_Axis"))
      for (k in sc[1:2]) {
        v <- out[[k]]
        if (is.character(v) || is.factor(v)) {
          v <- as.character(v)
          v[!is.na(v) & tolower(trimws(v)) %in% c("plano", "pl", "pl.", "plan")] <- "0"
          out[[k]] <- v
        }
      }
      cy <- as_numeric_safe(out[[sc[2]]])
      n_plus <- n_plus + sum(is.finite(cy) & cy > 0)
      out <- standardize_minus_cyl(out, sc[1], sc[2], sc[3], logger = tab_log)
    }
    if (n_plus > 0)
      logger(sprintf("mEYEstro tab '%s': %d plus-cylinder refraction(s) written in minus-cylinder notation, as the template requires.",
                     tab_what, n_plus))

    sh <- add_or_replace_worksheet(wb, if (is.null(pairing)) paste0("mEYEstro_", g)
                                       else paste0("mEYEstro_M_", g, "_", pairing))
    openxlsx::writeData(wb, sh, out)
    nc <- ncol(out)
    if (nc >= 1)  openxlsx::addStyle(wb, sh, gray,   1, 1:min(4, nc),   gridExpand = TRUE)
    if (nc >= 5)  openxlsx::addStyle(wb, sh, yellow, 1, 5:min(9, nc),   gridExpand = TRUE)
    if (nc >= 10) openxlsx::addStyle(wb, sh, green,  1, 10:min(13, nc), gridExpand = TRUE)
    if (nc >= 14) openxlsx::addStyle(wb, sh, red,    1, 14:min(19, nc), gridExpand = TRUE)
    if (nc >= 20) openxlsx::addStyle(wb, sh, blue,   1, 20:min(20, nc), gridExpand = TRUE)
  }
}

# AstigMATIC reads a positional five-column sheet: eye (OD/OS), preoperative
# cylinder, preoperative axis, postoperative cylinder, postoperative axis
# (negative cylinder accepted; the tab is written in minus-cylinder notation,
# a plus-cylinder row transposed in the tab only). The group is in the sheet
# name, so columns A-E load as they are. Eye codes R/RE/Right and L/LE/Left become OD/OS; the
# numeric codes 1 / 2 too, unless numeric_ok = FALSE (the column also holds
# a 0, i.e. a 0/1 coding whose meaning cannot be told): then every numeric
# code is left blank.
.eye_num_code <- function(e) ifelse(grepl("^[0-9]+(\\.0+)?$", e), sub("\\.0+$", "", e), NA_character_)
normalize_eye_od_os <- function(x, numeric_ok = TRUE) {
  e <- toupper(trimws(as.character(x)))
  num <- .eye_num_code(e)
  e[!is.na(num)] <- if (numeric_ok) num[!is.na(num)] else NA_character_
  ifelse(e %in% c("OD", "R", "RE", "RIGHT", "1"), "OD",
         ifelse(e %in% c("OS", "L", "LE", "LEFT", "2"), "OS", NA_character_))
}
# TRUE when the eye column's numeric codes include 0 (0/1 coding: which of
# 0 and 1 is the right eye is not recorded anywhere).
eye_codes_ambiguous <- function(x) {
  num <- .eye_num_code(toupper(trimws(as.character(x))))
  "0" %in% num
}

make_alpins_tabs <- function(df, wb, roles, pairing = NULL, logger = message, display_names = NULL) {
  r <- function(nm) .role_col(roles, nm)
  need_roles <- c("eye","pre_cyl","pre_axis","post_cyl","post_axis")
  cols <- vapply(need_roles, r, character(1))
  found <- !is.na(cols) & cols %in% names(df)
  miss <- need_roles[!found]
  if (sum(found) < 4) {
    log_once(logger, paste0("AstigMATIC tabs skipped - only ", sum(found), " of the ", length(need_roles),
                            " required roles are mapped to a column in the data (at least 4 needed); ",
                            "missing: ", paste(miss, collapse = ", "), "."))
    return(invisible(NULL))
  }
  if ("eye" %in% miss)
    log_once(logger, "AstigMATIC tabs: no eye column mapped - the Eye column (OD/OS) is left blank.")
  if (length(setdiff(miss, "eye")))
    log_once(logger, paste0("AstigMATIC tabs written with a blank column for the unmapped role(s): ",
                            paste(setdiff(miss, "eye"), collapse = ", "), "."))
  eye_num_ok <- TRUE
  if (!("eye" %in% miss) && eye_codes_ambiguous(df[[r("eye")]])) {
    eye_num_ok <- FALSE
    log_once(logger, paste0("AstigMATIC tabs: the eye column '", disp_name(r("eye"), display_names),
                            "' holds the numeric code 0 - ",
                            "numeric eye codes are ambiguous (0/1?), so they are left blank in the Eye column; ",
                            "map them to OD/OS (or R/L) in the data."))
  }
  for (g in .group_levels(df)) {
    dfg <- df[!is.na(df$GroupLabel) & as.character(df$GroupLabel) == g, , drop = FALSE]
    n <- nrow(dfg)
    pick <- function(role) {
      cc <- r(role)
      if (is.na(cc)) return(rep(NA, n))
      if (cc %in% names(dfg)) return(dfg[[cc]])
      rep(NA, n)
    }
    eye_raw <- pick("eye")
    eye <- normalize_eye_od_os(eye_raw, numeric_ok = eye_num_ok)
    bad <- is.na(eye) & !is.na(eye_raw) & nzchar(trimws(as.character(eye_raw)))
    # ambiguous numeric codes were logged once for the whole column above
    if (!eye_num_ok) bad <- bad & is.na(.eye_num_code(toupper(trimws(as.character(eye_raw)))))
    if (any(bad))
      logger(sprintf("AstigMATIC tab '%s': %d eye code(s) not recognized as right/left (e.g. '%s') - left blank.",
                     g, sum(bad), paste(utils::head(unique(as.character(eye_raw[bad])), 3), collapse = "', '")))
    # minus-cylinder notation, as the tab has always been read (export copy
    # only): a plus cylinder is transposed (axis + 90), whatever the analysis
    # data carry - with the minus-cylinder switch off, or a row typed in plus
    # form, the tab is still fed the same astigmatism (decision D1)
    minus_of <- function(cyl_role, ax_role) {
      cy <- pick(cyl_role); ax <- pick(ax_role)
      if (is.character(cy) || is.factor(cy)) {
        cy <- as.character(cy)
        cy[!is.na(cy) & tolower(trimws(cy)) %in% c("plano", "pl", "pl.", "plan")] <- "0"
      }
      cyn <- as_numeric_safe(cy); axn <- as_numeric_safe(ax)
      pos <- is.finite(cyn) & cyn > 0
      if (all(is.na(cy))) return(list(cyl = cy, axis = ax, n_plus = 0L))
      cyn[pos] <- -cyn[pos]
      axn[pos & is.finite(axn)] <- wrap_axis_0_180(axn[pos & is.finite(axn)] + 90)
      list(cyl = cyn, axis = if (all(is.na(ax))) ax else axn, n_plus = sum(pos))
    }
    pre <- minus_of("pre_cyl", "pre_axis"); post <- minus_of("post_cyl", "post_axis")
    if (pre$n_plus + post$n_plus > 0)
      logger(sprintf("AstigMATIC tab '%s': %d plus-cylinder refraction(s) written in minus-cylinder notation.",
                     g, pre$n_plus + post$n_plus))
    out <- data.frame(
      Eye = eye,
      `preoperative cylinder` = pre$cyl,
      `preoperative axis` = pre$axis,
      `postoperative cylinder` = post$cyl,
      `postoperative axis` = post$axis,
      check.names = FALSE, stringsAsFactors = FALSE)
    sh <- add_or_replace_worksheet(wb, if (is.null(pairing)) paste0("AstigMATIC_", g)
                                       else paste0("AstigMATIC_M_", g, "_", pairing))
    openxlsx::writeDataTable(wb, sh, out)
  }
}

# ------------------------------------------------------------
# Astigmatism per the JCRS 2025 standard (decisions D4-D9; astig.md S5, S10):
# the "Astigmatism (JCRS 2025)" sheet (a matched pairing: "Astig_M_<pairing>"),
# its CSV copy beside the workbook, and the per-eye "Astigmatism vectors"
# sheet. The statistics are graphs.R astig_vector_stats - the same function,
# inputs and seeds as the double-angle figures, which reuse them from its
# cache - so a sheet and its figure never disagree.
# ------------------------------------------------------------
# The vectors and statistics of one cohort set, or NULL (logged once) when the
# preoperative and postoperative cylinder and axis roles are not all mapped.
# matched: a matched pairing (independence working correlation, D18; the
# matching weights enter only a GEE centroid test, as on the figures).
astig_jcrs_compute <- function(dat, cfg, res, logger = message, matched = FALSE) {
  rc <- vapply(c("pre_cyl", "pre_axis", "post_cyl", "post_axis"),
               function(nm) .role_col(res$roles, nm), character(1))
  if (any(is.na(rc)) || !all(rc %in% names(dat))) {
    log_once(logger, "Astigmatism (JCRS 2025) sheet skipped - map the preoperative and postoperative cylinder and axis columns.")
    return(NULL)
  }
  av <- resolve_astig_vectors(dat, dat[[rc[1]]], dat[[rc[2]]], dat[[rc[3]]], dat[[rc[4]]],
                              vec_cols = res$astig_vector_cols %||% list(),
                              target = res$astig_target %||% list(), logger = logger,
                              display_names = res$display_names)
  ast <- astig_settings(cfg)
  glv <- .group_levels(dat)
  gl <- as.character(dat$GroupLabel)
  id <- astig_cluster_ids(dat, cfg, res)
  # the patients COUNTED (Patients columns, D4): the mapped ID also when the
  # tests do not cluster
  cnt_id <- astig_count_ids(dat, res)
  w <- if (isTRUE(matched) && "weights" %in% names(dat) && weights_nonunit(dat$weights))
    suppressWarnings(as.numeric(dat$weights)) else NULL
  vecs <- astig_jcrs_vectors(av)
  stats <- lapply(vecs, function(v)
    astig_vector_stats(v$mag, v$ax, gl, glv, id = id, method = ast$method, region = ast$region,
                       centroid_region = ast$centroid_region, matched = matched, w = w,
                       key = v$key, logger = logger, count_id = cnt_id))
  # the eye column, for the right / left centroids (D9: only when mapped)
  ec <- .role_col(res$roles, "eye")
  eye <- if (!is.na(ec) && ec %in% names(dat))
    normalize_eye_od_os(dat[[ec]], numeric_ok = !eye_codes_ambiguous(dat[[ec]])) else NULL
  list(av = av, vecs = vecs, stats = stats, gl = gl, glv = glv, id = id, cnt_id = cnt_id, eye = eye,
       settings = ast, clustered = !is.null(id) && !identical(ast$method, "classic"),
       pid_col = res$patient_id_clean)
}

# The sheet's tables (named list, in order) for one cohort set.
astig_jcrs_sections <- function(cp) {
  num <- function(v) suppressWarnings(as.numeric(v))
  pfx <- function(p) { s <- fmt_p(p); ifelse(is.na(s), "", s) }
  vlab <- function(k) switch(k, PRE = "PRE (preoperative)", POST = "POST (postoperative)",
                             SIA = "SIA (surgically induced)",
                             PE = if (isTRUE(cp$av$n_target_nonplano > 0)) "PE (prediction error)"
                                  else "PE (prediction error = POST; target plano)", k)
  keys <- names(cp$stats)
  summ <- do.call(rbind, lapply(keys, function(k) {
    s <- cp$stats[[k]]$summary; rg <- cp$stats[[k]]$regions
    reg_txt <- vapply(s$group, function(g) {
      dr <- rg[[g]]$data
      # not drawn: too few eyes, or a line rather than a polygon (R2-6)
      if (is.null(dr)) return(if (nzchar(rg[[g]]$data_why %||% "")) rg[[g]]$data_why
                              else sprintf("none (n = %d)", s$n[s$group == g]))
      # (decision R3-1: the method as the reference tool names it, and what it holds)
      attr(dr, "note") %||% ""
    }, character(1))
    in_reg <- vapply(s$group, function(g) {
      dr <- rg[[g]]$data
      if (is.null(dr) || is.null(attr(dr, "inside"))) NA_real_ else as.numeric(attr(dr, "inside"))
    }, numeric(1))
    cr_txt <- vapply(s$group, function(g) {
      cr <- rg[[g]]$centroid
      if (is.null(cr)) paste0("not computed: ", rg[[g]]$centroid_why)
      else if (!is.null(attr(cr, "B")))
        sprintf("%s: %d of %d bootstrap centroids (%s, %d %s)", ASTIG_CENTROID_LABEL, attr(cr, "kept"),
                attr(cr, "B"), attr(cr, "basis"), attr(cr, "N"),
                if (grepl("patients", attr(cr, "basis"), fixed = TRUE)) "patients" else "eyes")
      else paste0("Hotelling T² ellipse (", attr(cr, "basis"), ")")
    }, character(1))
    data.frame(Vector = vlab(k), Cohort = s$group, Eyes = s$n, Patients = s$n_pat,
               `Centroid x (D)` = s$cx, `Centroid y (D)` = s$cy,
               `Centroid magnitude (D)` = s$cm, `Centroid plus axis (°)` = s$ca,
               Centroid = mapply(astig_polar_text, s$cm, s$ca),
               `Total SD (D, N denominator)` = s$tsd,
               `Magnitude mean (D)` = s$mmean, `Magnitude SD (D)` = s$msd,
               `Magnitude median (D)` = s$mmed, `Magnitude min (D)` = s$mmin,
               `Magnitude max (D)` = s$mmax, `Magnitude RMS (D)` = s$rms,
               `95% data region` = reg_txt, `Eyes in data region` = in_reg,
               `Centroid 95% confidence region` = cr_txt,
               check.names = FALSE, stringsAsFactors = FALSE)
  }))
  cum <- do.call(rbind, lapply(keys, function(k) {
    s <- cp$stats[[k]]$summary
    out <- data.frame(Vector = vlab(k), Cohort = s$group, Eyes = s$n, check.names = FALSE,
                      stringsAsFactors = FALSE)
    for (cc in ASTIG_CUTS) out[[sprintf("≤ %.2f D (%%)", cc)]] <- s[[sprintf("le%.2f", cc)]]
    out[["> 1.00 D (%)"]] <- s$gt1
    out
  }))
  zero <- do.call(rbind, lapply(keys, function(k) {
    z <- cp$stats[[k]]$zero
    data.frame(Vector = vlab(k), Cohort = z$group, Eyes = z$n, Patients = z$n_pat,
               Test = ifelse(is.finite(z$B), paste0(z$test, sprintf(" (B = %d)", as.integer(z$B))), z$test),
               P = pfx(z$p), `P (Holm, cohorts of this vector)` = pfx(z$p_holm),
               Note = ifelse(is.finite(z$p),
                             ifelse(if (is.null(z$floor)) FALSE else z$floor %in% TRUE,
                                    sprintf("P at the bootstrap floor 1/B (B = %d): no bootstrap centroid lay farther from the centroid than zero",
                                            as.integer(z$B)), ""),
                             paste0("Not estimable - ", z$why)),
               check.names = FALSE, stringsAsFactors = FALSE)
  }))
  btw <- do.call(rbind, lapply(keys, function(k) {
    b <- cp$stats[[k]]$between
    if (is.null(b) || !nrow(b)) return(NULL)
    data.frame(Vector = vlab(k),
               Comparison = ifelse(b$cmp == "omnibus", "all cohorts (omnibus)",
                                   paste(b$group, "vs", b$ref)),
               Measure = sub("^<= ", "≤ ", b$measure),
               Values = b$values,
               Test = ifelse(is.finite(b$B), paste0(b$test, sprintf(" (B = %d)", as.integer(b$B))), b$test),
               P = pfx(b$p), `P (Holm, vs reference)` = pfx(b$p_holm_ref),
               `P (Holm, family)` = pfx(b$p_holm_fam),
               Note = ifelse(is.finite(b$p), "", paste0("Not estimable - ", b$why)),
               check.names = FALSE, stringsAsFactors = FALSE)
  }))
  # McNemar: preoperative vs postoperative refractive astigmatism of the same
  # eyes, each cohort and interval; Holm across the intervals of a cohort.
  # Clustered exactly when the sheet's other tests are (never with the
  # "classic" method, whose sheet says "Eyes treated as independent")
  mcn <- do.call(rbind, lapply(cp$glv, function(g) {
    k <- !is.na(cp$gl) & cp$gl == g
    rows <- lapply(ASTIG_CUTS, function(cc)
      astig_mcnemar(cp$av$pre[k], cp$av$post[k], if (cp$clustered) cp$id[k] else NULL, cc,
                    count_id = if (is.null(cp$cnt_id)) NULL else cp$cnt_id[k]))
    p <- vapply(rows, function(r) r$p, numeric(1))
    ph <- if (sum(is.finite(p)) >= 2) stats::p.adjust(p, "holm") else rep(NA_real_, length(p))
    data.frame(Cohort = g, Interval = sprintf("≤ %.2f D", ASTIG_CUTS),
               Eyes = vapply(rows, function(r) r$n, numeric(1)),
               Patients = vapply(rows, function(r) as.numeric(r$n_pat), numeric(1)),
               `Preop within (%)` = vapply(rows, function(r) r$pre_pct, numeric(1)),
               `Postop within (%)` = vapply(rows, function(r) r$post_pct, numeric(1)),
               `Improved (preop above, postop within)` = vapply(rows, function(r) r$improved, numeric(1)),
               `Worsened (preop within, postop above)` = vapply(rows, function(r) r$worsened, numeric(1)),
               Test = vapply(rows, function(r) r$test, character(1)),
               `Chi-squared` = vapply(rows, function(r) r$stat, numeric(1)),
               P = pfx(p), `P (Holm, intervals of this cohort)` = pfx(ph),
               Note = vapply(rows, function(r) if (nzchar(r$why)) paste0("Not estimable - ", r$why) else "",
                             character(1)),
               check.names = FALSE, stringsAsFactors = FALSE)
  }))
  # right and left eyes apart, only with a mapped eye column (D9)
  lat <- NULL
  if (!is.null(cp$eye) && any(!is.na(cp$eye))) {
    lat <- do.call(rbind, lapply(keys, function(k) {
      v <- cp$vecs[[k]]
      do.call(rbind, lapply(cp$glv, function(g) do.call(rbind, lapply(c("OD", "OS"), function(e) {
        kk <- !is.na(cp$gl) & cp$gl == g & !is.na(cp$eye) & cp$eye == e
        s <- astig_summary1(v$mag[kk], v$ax[kk])
        if (!s$n) return(NULL)
        data.frame(Vector = vlab(k), Cohort = g, Eye = e, Eyes = s$n,
                   `Centroid x (D)` = s$cx, `Centroid y (D)` = s$cy,
                   Centroid = astig_polar_text(s$cm, s$ca), check.names = FALSE, stringsAsFactors = FALSE)
      }))))
    }))
  }
  vert <- do.call(rbind, lapply(keys, function(k) {
    rg <- cp$stats[[k]]$regions
    do.call(rbind, lapply(names(rg), function(g) {
      one <- function(p, what) if (is.null(p) || !nrow(p)) NULL else {
        p <- p[-nrow(p), , drop = FALSE]          # the closing point repeats the first
        data.frame(Vector = vlab(k), Cohort = g, Region = what, Vertex = seq_len(nrow(p)),
                   `x (D)` = p$x, `y (D)` = p$y, check.names = FALSE, stringsAsFactors = FALSE)
      }
      dr <- rg[[g]]$data
      rbind(one(dr, if (identical(attr(dr, "method"), "normal")) "95% data ellipse (path)" else "95% data polygon"),
            one(rg[[g]]$ellipse, "95% data ellipse (path)"),
            one(rg[[g]]$centroid, if (!is.null(attr(rg[[g]]$centroid, "B"))) "centroid 95% confidence polygon"
                                  else "centroid 95% confidence ellipse (path)"))
    }))
  }))
  st1 <- cp$stats[[1]]
  notes <- c(astig_method_text(st1, cp$clustered),
             paste0("Cumulative intervals: magnitude ≤ 0.25, 0.50 ... 2.00 D (a magnitude on an interval edge counts ",
                    "as within it); the > 1.00 D column is the complement of ≤ 1.00 D (the same test)."),
             paste0("Holm: 'P (Holm, family)' adjusts within each vector and each comparison (the omnibus, or one cohort ",
                    "vs the reference) across the family {centroid, precision, mean magnitude, ≤ 0.25 ... ≤ 2.00 D}; ",
                    "'P (Holm, vs reference)' adjusts each measure across the cohorts compared with the reference (as the ",
                    "figures' 'vs' lines); the centroid-against-zero P across the cohorts of a vector; McNemar across the ",
                    "intervals of a cohort."),
             paste0("McNemar: fewer than 25 discordant eyes - exact binomial test; otherwise the chi-square without continuity correction. ",
                    if (cp$clustered) paste0("Clustered by patient when a patient contributes two discordant eyes: below 25 discordant ",
                                             "eyes the exact sign test on the patients' net changes, otherwise cluster-adjusted ",
                                             "(Durkalski 2003); not estimable when the discordant eyes are from 1 patient.")
                    else "Eyes treated as independent."),
             if (is.null(cp$eye)) "Right and left eyes: no eye column mapped - not separated; left eyes are not mirrored."
             else "Right and left eyes are listed apart; left eyes are not mirrored.",
             if (isTRUE(cp$settings$jcrs)) NULL
             else "The figures use separate univariate x / y tests (astigmatism JCRS standard off); this sheet keeps the standard's tests.")
  list(`Summary per vector and cohort` = summ,
       `Cumulative percentage of eyes by magnitude` = cum,
       `Centroid against zero (each cohort)` = zero,
       `Comparisons between cohorts` = btw,
       `Preoperative vs postoperative refractive astigmatism, same eyes (McNemar)` = mcn,
       `Right and left eyes (centroids)` = lat,
       `Region vertices (doubled-angle x, y)` = vert,
       `Method notes` = data.frame(Note = notes, stringsAsFactors = FALSE))
}

# The per-eye vectors (D8): PE first (the standard's key metric), then SIA,
# PRE, POST and the Alpins TIA and DV - magnitude, plus axis and doubled-angle
# x, y - with the cohort, the patient ID and a row label (the mapped eye ID
# column, else the row of the analysed data).
astig_vectors_table <- function(dat, cp, res) {
  av <- cp$av
  n <- nrow(dat)
  pid <- if (!is.null(cp$pid_col) && !is.na(cp$pid_col[1]) && cp$pid_col[1] %in% names(dat))
    id_text(dat[[cp$pid_col[1]]]) else rep(NA_character_, n)
  ec <- res$eye_id_clean
  rl <- if (!is.null(ec) && !is.na(ec[1]) && ec[1] %in% names(dat)) id_text(dat[[ec[1]]])
        else paste0("row ", seq_len(n))
  vv <- list(PE = c("pe", "pe_ax"), SIA = c("sia", "sax"), PRE = c("pre", "pre_ax"),
             POST = c("post", "post_ax"), `TIA (Alpins)` = c("tia", "tax"), `DV (Alpins)` = c("dv", "dax"))
  do.call(rbind, lapply(names(vv), function(nm) {
    m <- av[[vv[[nm]][1]]]; a <- av[[vv[[nm]][2]]]
    ok <- is.finite(m) & is.finite(a) & !is.na(dat$GroupLabel)
    data.frame(Group = as.character(dat$GroupLabel)[ok], `Patient ID` = pid[ok], `Row label` = rl[ok],
               Vector = nm, `Magnitude (D)` = m[ok], `Plus axis (°)` = a[ok],
               `x (D)` = m[ok] * cospi(a[ok] / 90), `y (D)` = m[ok] * sinpi(a[ok] / 90),
               check.names = FALSE, stringsAsFactors = FALSE)
  }))
}

# Stacked tables like write_stacked_tables, numbers kept as numbers (full
# precision) with a display format by unit: "(D)" two decimals as every mean
# and SD of the tables (D24), "(%)" one, "(°)" none.
write_astig_sheet <- function(wb, sheet_name, sections, note = NULL) {
  sn <- add_or_replace_worksheet(wb, sheet_name)
  title_st <- openxlsx::createStyle(textDecoration = "bold", fontSize = 12)
  note_st  <- openxlsx::createStyle(textDecoration = "bold", fontColour = "#C00000")
  fmt <- list(D = openxlsx::createStyle(numFmt = "0.00"), P = openxlsx::createStyle(numFmt = "0.0"),
              A = openxlsx::createStyle(numFmt = "0"))
  row <- 1
  if (!is.null(note) && nzchar(note)) {
    openxlsx::writeData(wb, sn, note, startRow = row, startCol = 1)
    openxlsx::addStyle(wb, sn, note_st, rows = row, cols = 1)
    row <- row + 2
  }
  for (nm in names(sections)) {
    d <- sections[[nm]]
    if (is.null(d) || nrow(as.data.frame(d)) == 0) next
    openxlsx::writeData(wb, sn, nm, startRow = row, startCol = 1)
    openxlsx::addStyle(wb, sn, title_st, rows = row, cols = 1)
    row <- row + 1
    d <- as.data.frame(d, check.names = FALSE)
    openxlsx::writeData(wb, sn, d, startRow = row, startCol = 1)
    for (j in seq_along(d)) {
      if (!is.numeric(d[[j]]) || !nrow(d)) next
      h <- names(d)[j]
      st <- if (grepl("(D)", h, fixed = TRUE) || grepl("(D,", h, fixed = TRUE)) fmt$D
            else if (grepl("(%)", h, fixed = TRUE)) fmt$P
            else if (grepl("(°)", h, fixed = TRUE)) fmt$A
            # the McNemar statistic (T1-12)
            else if (startsWith(h, "Chi-squared")) fmt$D else NULL
      if (!is.null(st)) openxlsx::addStyle(wb, sn, st, rows = row + seq_len(nrow(d)), cols = j,
                                           gridExpand = TRUE, stack = TRUE)
    }
    row <- row + nrow(d) + 2
  }
  invisible(sn)
}

# The heading's statement on the matching weights of a matched cohort's
# astigmatism sheet (finding T1-14): the robust astigmatism statistics and
# tests have no weighted form, so with non-unit weights they describe the
# matched rows unweighted - a different pseudo-population from the weighted
# Out_M_ tables - and the heading says so plainly, naming the one test that is
# weighted (the centroid comparison of the "gee" / "classic" methods).
matched_astig_note <- function(dat, cfg) {
  w <- if ("weights" %in% names(dat)) suppressWarnings(as.numeric(dat$weights)) else NULL
  w <- w[is.finite(w)]
  if (!length(w) || all(abs(w - 1) < 1e-8)) return(" Matched rows (unit matching weights).")
  meth <- if (exists("astig_settings", mode = "function")) astig_settings(cfg)$method else "robust"
  paste0(" Matched rows, unweighted: the matching weights (", format(signif(min(w), 3)), "-",
         format(signif(max(w), 3)), ") are not applied to the astigmatism statistics or tests",
         if (!identical(meth, "robust")) ", except the centroid comparison between cohorts (a weighted GEE)" else "",
         "; the weighted matched tables (Out_M_) can therefore differ.")
}

# Write one cohort set's sheet; returns its tables (for the CSV copy), or NULL.
make_astig_jcrs_sheet <- function(dat, wb, cfg, res, sheet_name, cohort_label, logger = message,
                                  matched = FALSE, vectors_sheet = FALSE) {
  cp <- astig_jcrs_compute(dat, cfg, res, logger, matched = matched)
  if (is.null(cp)) return(NULL)
  secs <- astig_jcrs_sections(cp)
  note <- paste0("Astigmatism per the JCRS 2025 standard (Koch et al., Part II), ", cohort_label,
                 ": plus-cylinder format, with-the-rule astigmatism on the left of the double-angle plots. ",
                 "Cohorts: ", paste(cp$glv, collapse = ", "), " (reference: ", cp$glv[1], "). ",
                 if (cp$clustered) "Tests clustered by patient." else "Eyes treated as independent.",
                 if (isTRUE(matched)) matched_astig_note(dat, cfg) else "")
  write_astig_sheet(wb, sheet_name, secs, note = note)
  if (isTRUE(vectors_sheet)) {
    vt <- astig_vectors_table(dat, cp, res)
    if (!is.null(vt) && nrow(vt)) {
      sv <- add_or_replace_worksheet(wb, "Astigmatism vectors")
      openxlsx::writeDataTable(wb, sv, vt)
      for (j in which(names(vt) %in% c("Magnitude (D)", "x (D)", "y (D)")))
        openxlsx::addStyle(wb, sv, openxlsx::createStyle(numFmt = "0.000"), rows = 1 + seq_len(nrow(vt)),
                           cols = j, gridExpand = TRUE, stack = TRUE)
      # the plus axis to a tenth of a degree (a computed vector's axis is not whole, T1-12)
      for (j in which(names(vt) == "Plus axis (°)"))
        openxlsx::addStyle(wb, sv, openxlsx::createStyle(numFmt = "0.0"), rows = 1 + seq_len(nrow(vt)),
                           cols = j, gridExpand = TRUE, stack = TRUE)
    }
  }
  secs
}

# The CSV copy of the astigmatism sheets of one workbook: every table stacked,
# with the cohort set and the table it comes from leading each row.
write_astig_csv <- function(tables, path, logger = message) {
  rows <- list()
  for (cs in names(tables)) for (sec in names(tables[[cs]])) {
    d <- tables[[cs]][[sec]]
    if (is.null(d) || !nrow(d)) next
    d <- as.data.frame(d, check.names = FALSE, stringsAsFactors = FALSE)
    rows[[length(rows) + 1]] <- data.frame(`Cohort set` = cs, Table = sec, d, check.names = FALSE,
                                           stringsAsFactors = FALSE)
  }
  if (!length(rows)) return(invisible(FALSE))
  out <- as.data.frame(dplyr::bind_rows(rows), check.names = FALSE)
  err <- ""
  tryCatch(if (exists("write_csv_utf8_bom", mode = "function")) write_csv_utf8_bom(out, path)
           else utils::write.csv(out, path, row.names = FALSE, fileEncoding = "UTF-8"),
           error = function(e) err <<- conditionMessage(e))
  if (nzchar(err) || !file.exists(path)) {
    logger(paste0("WARNING: astigmatism CSV NOT written (", if (nzchar(err)) err else "the file was not created",
                  "): ", path))
    return(invisible(FALSE))
  }
  logger(paste0("Astigmatism (JCRS 2025) tables exported: ", path))
  invisible(TRUE)
}

# A cell holding several lines (the per-comparison GEE / adjusted columns
# with 3+ groups) is shown on one run-together line unless it is wrapped:
# wrap exactly those cells of data frame `d` written with its header at
# `header_row` (stacked onto the cell's existing style).
.wrap_multiline_cells <- function(wb, sheet, d, header_row, first_col = 1) {
  d <- as.data.frame(d, check.names = FALSE, stringsAsFactors = FALSE)
  if (!nrow(d) || !ncol(d)) return(invisible(NULL))
  st <- openxlsx::createStyle(wrapText = TRUE, valign = "top")
  for (j in seq_len(ncol(d))) {
    v <- d[[j]]
    if (!(is.character(v) || is.factor(v))) next
    i <- which(grepl("\n", as.character(v), fixed = TRUE))
    if (length(i))
      openxlsx::addStyle(wb, sheet, st, rows = header_row + i, cols = first_col + j - 1,
                         gridExpand = TRUE, stack = TRUE)
  }
  invisible(NULL)
}

# ------------------------------------------------------------
# Write several tables stacked on one sheet, each under a bold
# section title; optional bold red note at the very top.
# sections: named list(title -> data.frame). NULL/empty skipped.
# ------------------------------------------------------------
# model_ref: the reference group of the GEE / regression model tables of the
# sheet (gee_sheet_frame names their terms by it); NULL for other sheets.
write_stacked_tables <- function(wb, sheet_name, sections, note = NULL, model_ref = NULL) {
  sn <- add_or_replace_worksheet(wb, sheet_name)
  num3 <- openxlsx::createStyle(numFmt = "0.000")
  title_st <- openxlsx::createStyle(textDecoration = "bold", fontSize = 12)
  note_st  <- openxlsx::createStyle(textDecoration = "bold", fontColour = "#C00000")
  row <- 1
  if (!is.null(note) && nzchar(note)) {
    openxlsx::writeData(wb, sn, note, startRow = row, startCol = 1)
    openxlsx::addStyle(wb, sn, note_st, rows = row, cols = 1)
    row <- row + 2
  }
  for (nm in names(sections)) {
    d <- sections[[nm]]
    if (is.null(d) || nrow(as.data.frame(d)) == 0) next
    openxlsx::writeData(wb, sn, nm, startRow = row, startCol = 1)
    openxlsx::addStyle(wb, sn, title_st, rows = row, cols = 1)
    row <- row + 1
    # the P columns under the header "P" (D24; helpers.R table_headers); a
    # model table (GEE, regression full models) as gee_sheet_frame shows it
    d <- as.data.frame(d, check.names = FALSE)
    is_model <- all(c("term", "estimate") %in% names(d))
    if (is_model) d <- gee_sheet_frame(d, model_ref)
    names(d) <- table_headers(names(d))
    openxlsx::writeData(wb, sn, d, startRow = row, startCol = 1)
    .wrap_multiline_cells(wb, sn, d, header_row = row)
    # three decimals for the model numbers and the matching numbers (T1-03, T1-08)
    n3 <- if (is_model) names(d) %in% GEE_SHEET_NUM3
          else names(d) %in% c("Standardized difference before matching", "Standardized difference after matching",
                                "Matching weight") | startsWith(names(d), "Propensity score")
    if (nrow(d)) for (j in which(n3 & vapply(d, is.numeric, logical(1))))
      openxlsx::addStyle(wb, sn, num3, rows = row + seq_len(nrow(d)), cols = j, gridExpand = TRUE, stack = TRUE)
    row <- row + nrow(as.data.frame(d)) + 2
  }
  invisible(sn)
}

# The rows of a GEE / regression model table as a sheet shows them (finding
# T1-03): headers in words, the group terms as comparisons ("Cohort A vs
# Reference"), the intercept named by the reference group, the exact P
# ("P (exact)") beside a formatted one ("P", D15) after the effect text, and
# the numbers to three decimals (write_stacked_tables). The models keep their R
# names internally (run_gee_models, extract_gee_tidy); only the sheet changes.
GEE_SHEET_HEADERS <- c(term = "Term", estimate = "Estimate", std.error = "SE", statistic = "Statistic (t or z)",
                       p.value = "P (exact)", Effect = "Effect type", EffectValue = "Effect",
                       CI_low = "95% CI lower", CI_high = "95% CI upper")
GEE_SHEET_NUM3 <- c("Estimate", "SE", "Statistic (t or z)", "Effect", "95% CI lower", "95% CI upper")
gee_sheet_frame <- function(d, ref = NULL) {
  d <- as.data.frame(d, check.names = FALSE, stringsAsFactors = FALSE)
  if (!nrow(d)) return(d)
  if ("term" %in% names(d)) {
    t <- as.character(d$term)
    grp <- !is.na(t) & startsWith(t, "GroupLabel")
    t[grp] <- paste0(substring(t[grp], 11), " vs ", if (length(ref)) ref[1] else "the reference group")
    t[!is.na(t) & t == "(Intercept)"] <- if (length(ref)) paste0("Intercept (", ref[1], ")") else "Intercept"
    d$term <- t
  }
  if ("p.value" %in% names(d)) {
    pn <- suppressWarnings(as.numeric(d$p.value))
    pf <- fmt_p(pn); pf[is.na(pf)] <- ""
    at <- if ("CI" %in% names(d)) match("CI", names(d)) else ncol(d)
    d <- data.frame(d[seq_len(at)], .p_fmt = pf, d[setdiff(seq_len(ncol(d)), seq_len(at))],
                    check.names = FALSE, stringsAsFactors = FALSE)
    names(d)[names(d) == ".p_fmt"] <- "P"
  }
  hit <- names(d) %in% names(GEE_SHEET_HEADERS)
  names(d)[hit] <- unname(GEE_SHEET_HEADERS[names(d)[hit]])
  d
}

# Bold note text when some GEE models could not be fitted. Counts OUTCOMES:
# an outcome skipped as a whole has one row per comparison, and one model can
# have several blanked terms.
# A comparison with a group of few patients carries a caution in the 'Note'
# column (run_gee_models); the bold note points to it.
# Only outcomes that FAILED are counted (contract R4-9): the by-design rows
# "Skipped: ordinal / categorical outcome not modeled in GEE" are not failures
# (their Model cell says so) and are left out of the count.
# corstr: the working correlation of the sheet's models (gee_corstr_label(),
# analysis.R); "independence (weighted)" - matching weights that are not all 1
# - is stated in the note (contract R4-1), and so is "independence (matched
# cohort)" with the statement of decision D18 (matched sets are not clusters).
# NULL / "exchangeable" adds nothing.
gee_skip_note <- function(g, corstr = NULL) {
  m_note <- matched_analysis_note()
  wtxt <- if (length(corstr) && identical(corstr[1], "independence (weighted)"))
    paste0("Models weighted by the matching weights - working correlation: ", corstr[1],
           ", ", gee_se_label(1), ". ", m_note)
    else if (length(corstr) && grepl("independence", corstr[1], fixed = TRUE))
      paste0("Matched cohort - working correlation: ", corstr[1], ". ", m_note)
    else NULL
  if (is.null(g) || nrow(g) == 0) return("NOTE: no GEE models were fitted.")
  if (!("Model" %in% names(g))) return(if (length(wtxt)) paste("NOTE:", wtxt) else NULL)
  by_design <- !is.na(g$Model) & grepl("outcome not modeled in GEE", g$Model, fixed = TRUE)
  hit <- !is.na(g$Model) & grepl("Skipped|failed", g$Model) & !by_design
  bad <- if ("Outcome" %in% names(g)) length(unique(g$Outcome[hit])) else sum(hit)
  cau <- if ("Note" %in% names(g)) as.character(g$Note) else character(0)
  cau <- cau[!is.na(cau) & startsWith(cau, "Caution")]
  few <- length(cau) > 0
  # decision R3-4: per-patient outcomes and independence refits, by outcome
  n_of <- function(txt) {
    k <- !is.na(g$Model) & grepl(txt, g$Model, fixed = TRUE)
    if ("Outcome" %in% names(g)) length(unique(g$Outcome[k])) else sum(k)
  }
  n_pp <- n_of(get0("GEE_PER_PATIENT", ifnotfound = "per patient (one row per patient)"))
  n_ind <- n_of(get0("GEE_INDEP_REFIT", ifnotfound = "independence working correlation (the outcome is almost constant"))
  r34 <- c(if (n_pp > 0) paste0(n_pp, " outcome(s) are per-patient variables: modeled on one row per patient ",
                                "by an ordinary linear or logistic model (model-based SE), never by an eye-level ",
                                "GEE - see the 'Model' column."),
           if (n_ind > 0) paste0(n_ind, " outcome(s) were refitted with an independence working correlation: the ",
                                 "exchangeable fit was not accepted (working correlation 0.95 or more, no ",
                                 "convergence, or not at its solution - the outcome is almost constant within ",
                                 "patients); the robust SE stay clustered by patient - see the 'Model' column."))
  if (bad == 0 && !few && !length(wtxt) && !length(r34)) return(NULL)
  # without a patient ID each eye is its own cluster and the cautions count eyes
  unit <- if (few && all(grepl("with few eyes$", cau))) "eyes" else "patients"
  paste0("NOTE: ", paste(c(
    wtxt, r34,
    if (bad > 0) paste0(bad, " outcome(s) could not be modeled by GEE (too few eyes or patients, ",
                        "a group or outcome without usable data, too few events, or model failure) - ",
                        "see the 'Model' column for the reason."),
    if (few) paste0(if (bad > 0 || length(wtxt) || length(r34)) "Comparisons" else "comparisons", " with a group of fewer than ",
                    get0("GEE_FEW_PATIENTS", ifnotfound = 10L), " ", unit,
                    " carry a caution - see the 'Note' column.")), collapse = " "))
}

# ------------------------------------------------------------
# Within-group distribution: for ONE group's rows, describe each
# parameter (no between-group test). Continuous -> mean±SD, median[IQR],
# range, n; ordinal -> median[IQR]; dichotomous -> event n(%);
# categorical -> one row per level n(%). Returns a tibble.
# ------------------------------------------------------------
# display_names: optional clean key -> original header map (res$display_names,
# contract R4-3): the Parameter labels name the column as typed in the file.
# A variable with no recorded value prints "no data", whatever its type.
# va_cols: the acuity columns analysed in logMAR (va_info()$in_logmar).
describe_within_group <- function(df_g, params_clean, type_map, dicho_pref_clean = list(),
                                  levels_map = list(), patient_id_clean = NA_character_,
                                  display_names = NULL, va_cols = character(0)) {
  dn <- function(x) disp_name(x, display_names)
  rows <- list()
  n_tot <- nrow(df_g)
  has_pid <- !is.na(patient_id_clean) && patient_id_clean %in% names(df_g)
  # a variable marked "patient" is collapsed to ONE row per patient before it is
  # described, so a patient contributing two eyes is counted once. The value kept
  # is the first non-missing one, exactly as collapse_patient_level() does for the
  # between-group tables, so the same variable reads the same in both places.
  unit_of <- function(pn) {
    u <- levels_map[[pn]]
    if (!is.null(u) && identical(as.character(u), "patient") && has_pid) "patient" else "eye"
  }
  any_patient <- any(vapply(params_clean, function(p) unit_of(p) == "patient", logical(1)))
  for (pn in params_clean) {
    if (!(pn %in% names(df_g))) next
    vt <- get_var_type(pn, type_map); if (is.na(vt)) vt <- "continuous"
    unit <- unit_of(pn)
    v <- if (unit == "patient") {
      df_g %>%
        dplyr::select(dplyr::all_of(c(patient_id_clean, pn))) %>%
        dplyr::group_by(.data[[patient_id_clean]]) %>%
        dplyr::summarise(val = dplyr::first(na.omit(.data[[pn]])), .groups = "drop") %>%
        dplyr::pull(val)
    } else df_g[[pn]]
    if (vt == "continuous") {
      x <- as_numeric_safe(v); x <- x[is.finite(x)]
      # fixed decimals (D24); acuity in logMAR reads in Snellen too (D16)
      summ <- if (length(x) == 0) "no data"
        else if (pn %in% va_cols) paste0(
          fmt_va(mean(x), if (length(x) > 1) sd(x) else NA, min(x), max(x)),
          " (median ", fmt_fix(median(x)), " logMAR)")
        # two decimals throughout, one space (D24, T1-11); a single value has no SD
        else paste0(
        fmt_fix(mean(x)), if (length(x) > 1) paste0(" ± ", fmt_fix(sd(x))) else "",
        " (median ", fmt_fix(median(x)), " [", fmt_fix(quantile(x, .25)), "–", fmt_fix(quantile(x, .75)), "]",
        ", range ", fmt_fix(min(x)), " to ", fmt_fix(max(x)), ")")
      rows[[length(rows)+1]] <- tibble::tibble(Parameter = dn(pn), n = length(x), Summary = summ, .unit = unit)
    } else if (vt == "ordinal") {
      x <- as_numeric_safe(v); x <- x[is.finite(x)]
      summ <- if (length(x) == 0) "no data" else
        fmt_quartiles(c(median(x), quantile(x, .25), quantile(x, .75)))
      rows[[length(rows)+1]] <- tibble::tibble(Parameter = dn(pn), n = length(x), Summary = summ, .unit = unit)
    } else if (vt == "dichotomous") {
      nn <- sum(!is.na(v))
      if (nn == 0) {
        # all missing: no level to name and no percentage (contract R4-9)
        rows[[length(rows)+1]] <- tibble::tibble(Parameter = dn(pn), n = 0, Summary = "no data", .unit = unit)
      } else {
        lev <- pick_display_level(pn, v, dicho_pref_clean, display_names = display_names)
        c_ <- sum(as.character(v) == as.character(lev), na.rm = TRUE)
        rows[[length(rows)+1]] <- tibble::tibble(
          Parameter = paste0(dn(pn), " (", lev, ")"), n = nn, Summary = paste0(c_, " (", fmt_pct(c_, nn), ")"),
          .unit = unit)
      }
    } else if (vt == "categorical") {
      vv <- as.character(v); levs <- sort(unique(na.omit(vv)))
      nn <- sum(!is.na(v))
      if (length(levs) == 0) {
        rows[[length(rows)+1]] <- tibble::tibble(Parameter = dn(pn), n = 0, Summary = "no data", .unit = unit)
      } else for (lev in levs) {
        c_ <- sum(vv == lev, na.rm = TRUE)
        rows[[length(rows)+1]] <- tibble::tibble(
          Parameter = paste0(dn(pn), ": ", lev), n = nn, Summary = paste0(c_, " (", fmt_pct(c_, nn), ")"),
          .unit = unit)
      }
    }
  }
  if (length(rows) == 0) return(tibble::tibble(Parameter = character(0), n = integer(0), Summary = character(0)))
  out <- dplyr::bind_rows(rows)
  # the extra column only appears when it carries information - with everything
  # at eye level (the default) the sheet is unchanged from before this option
  if (any_patient) {
    out$`Analyzed by` <- ifelse(out$.unit == "patient", "patient", "eye")
  }
  out$.unit <- NULL
  out
}

# ------------------------------------------------------------
# Publication-style table: the three summary tables stacked into
# one manuscript-ready sheet (bold section headers, no Test /
# Interpretation columns, prettified parameter labels).
# sections: named list(section title -> summary tibble).
# A test that could not be performed prints "NE" (not estimable) with a
# footnote - the Interpretation column that explains it is not in this sheet,
# and a blank would read like a level row that carries no p by design. The
# footnote gives the reason the summary table recorded for each NE row (e.g.
# no weighted test for a categorical variable with more than two levels, a
# constant variable, a group with n = 1), with the rows it applies to.
# The Test column is not in this sheet either, so a second footnote names the
# test behind every P value (decision D12: clustered or classical), with the
# variables it was run on.
# note: optional bold line above the table (e.g. matching weights).
# ------------------------------------------------------------
# "Values: ..." - the footnote that says what the cells of a Publication Table
# hold, from the kinds of cell present (finding T1-10): means, medians, counts,
# acuity cells, the per-cell eye counts, the effects and their reference, the
# Holm tag, the adjusted model's eyes and, in a weighted (matched) table, the
# weighted counts and the effective sample size.
pub_values_note <- function(out) {
  cells <- unlist(lapply(out[, setdiff(names(out), "Parameter"), drop = FALSE], as.character))
  cells <- cells[!is.na(cells) & nzchar(cells)]
  hdr <- names(out)
  has <- function(re, x = cells) any(grepl(re, x, perl = TRUE))
  parts <- c(
    if (has("^-?[0-9.]+ ± ")) "mean ± SD",
    if (has("^-?[0-9.]+ \\[-?[0-9.]+–")) "median [interquartile range]",
    if (has("^[0-9.]+/[0-9.]+ \\(")) "n/N (%) - eyes (or patients) with the level among those with data")
  out_txt <- character(0)
  if (length(parts)) out_txt <- paste0("Values are ", paste(parts, collapse = ", "), ".")
  if (has("logMAR\\); range "))
    out_txt <- c(out_txt, paste0("Visual acuity: the Snellen geometric mean (logMAR mean ± SD), then the range of the ",
                                 "eyes in Snellen."))
  if (has("\\(n = [0-9]+\\)"))
    out_txt <- c(out_txt, "(n = ...) in a cell: the eyes (patients, for a row marked [per patient]) with data, where fewer than the column's n.")
  ref <- sub(" \\(n = .*$", "", hdr[grepl(" \\(n = ", hdr)][1])
  if (has("β = "))
    out_txt <- c(out_txt, paste0("β = difference from ", if (!is.na(ref)) ref else "the reference group",
                                 " in the variable's units (logMAR for acuity), with its 95% CI."))
  if (has("OR = "))
    out_txt <- c(out_txt, paste0("OR = odds ratio of the level named in the row vs ",
                                 if (!is.na(ref)) ref else "the reference group", ", with its 95% CI."))
  if (has("\\[Holm\\]"))
    out_txt <- c(out_txt, "[Holm] = Holm-adjusted across the comparisons with the reference group.")
  if (any(grepl("Adjusted model n", hdr, fixed = TRUE)))
    out_txt <- c(out_txt, "Adjusted model n (eyes): the complete-case eyes of the adjusted model.")
  if (any(grepl("ESS = ", hdr, fixed = TRUE)) || has("^[0-9]+\\.[0-9]+/[0-9]+\\.[0-9]+ \\("))
    out_txt <- c(out_txt, paste0("Weighted by the matching weights: counts are sums of weights (hence decimals), and ",
                                 "ESS = effective sample size, (sum of weights)² / sum of squared weights."))
  if (length(out_txt)) c("Values:", paste0("- ", out_txt)) else character(0)
}

make_publication_table <- function(wb, sheet_name, sections, note = NULL) {
  drop_cols <- c("Test", "Practical Interpretation")
  secs <- list()
  ne_why <- character(0); ne_row <- character(0)
  t_lab <- character(0); t_row <- character(0)
  for (nm in names(sections)) {
    d <- sections[[nm]]
    if (is.null(d) || nrow(d) == 0) next
    # the variable each row belongs to (create_summary_table's "row_var": the
    # display label, without a level), so the NE footnote names the variable,
    # not the first level row of a categorical one (contract R4-9)
    rv <- attr(d, "row_var")
    if (length(rv) != nrow(d)) rv <- NULL
    d <- as.data.frame(d, check.names = FALSE, stringsAsFactors = FALSE)
    interp <- if ("Practical Interpretation" %in% names(d))
      as.character(d[["Practical Interpretation"]]) else rep(NA_character_, nrow(d))
    ne <- grepl("^Not estimable", interp)
    ne[is.na(ne)] <- FALSE
    # the test of every row that prints a P value
    if (all(c("Test", "p_value") %in% names(d))) {
      pv <- as.character(d$p_value); tl <- as.character(d$Test)
      has_p <- !is.na(pv) & nzchar(pv) & !ne & !is.na(tl) & nzchar(tl)
      if (any(has_p)) {
        t_lab <- c(t_lab, tl[has_p])
        t_row <- c(t_row, if (!is.null(rv)) rv[has_p]
                          else if ("Parameter" %in% names(d)) d$Parameter[has_p]
                          else rep(nm, sum(has_p)))
      }
    }
    d <- d[, setdiff(names(d), drop_cols), drop = FALSE]
    d[] <- lapply(d, as.character)
    if ("p_value" %in% names(d)) {
      fill <- ne & (is.na(d$p_value) | !nzchar(d$p_value))
      d$p_value[fill] <- "NE"
      if (any(fill)) {
        # a bare "Not estimable" (no "- reason") falls back to the generic text
        why <- trimws(sub("\\.+$", "", sub("^Not estimable\\s*(-\\s*)?", "", interp[fill])))
        why[!nzchar(why)] <- "the test could not be performed (insufficient or missing data)"
        ne_why <- c(ne_why, why)
        ne_row <- c(ne_row, if (!is.null(rv)) rv[fill]
                            else if ("Parameter" %in% names(d)) d$Parameter[fill]
                            else rep(nm, sum(fill)))
      }
    }
    d[] <- lapply(d, function(x) ifelse(is.na(x), "", x))
    secs[[nm]] <- d
  }
  if (length(secs) == 0) return(invisible(NULL))

  all_cols <- names(secs[[1]])
  for (d in secs) all_cols <- c(all_cols, setdiff(names(d), all_cols))

  rows <- list(); sec_row_idx <- integer(0); r <- 0
  for (nm in names(secs)) {
    d <- secs[[nm]]
    for (mc in setdiff(all_cols, names(d))) d[[mc]] <- ""
    d <- d[, all_cols, drop = FALSE]
    hdr <- as.data.frame(as.list(setNames(rep("", length(all_cols)), all_cols)),
                         check.names = FALSE, stringsAsFactors = FALSE)
    hdr$Parameter <- nm
    rows[[length(rows) + 1]] <- hdr; r <- r + 1; sec_row_idx <- c(sec_row_idx, r)
    rows[[length(rows) + 1]] <- d;   r <- r + nrow(d)
  }
  out <- do.call(rbind, rows)
  # the user's own column headers, as on every other sheet (disp_name)
  names(out) <- table_headers(names(out))

  sn <- add_or_replace_worksheet(wb, sheet_name)
  top <- 1
  if (!is.null(note) && nzchar(note)) {
    openxlsx::writeData(wb, sn, note, startRow = 1, startCol = 1)
    openxlsx::addStyle(wb, sn, openxlsx::createStyle(textDecoration = "bold", fontColour = "#C00000"),
                       rows = 1, cols = 1)
    top <- 3
  }
  openxlsx::writeData(wb, sn, out, startRow = top)
  head_st <- openxlsx::createStyle(textDecoration = "bold", border = "bottom")
  sec_st  <- openxlsx::createStyle(textDecoration = "bold", fgFill = "#F2F2F2")
  openxlsx::addStyle(wb, sn, head_st, rows = top, cols = seq_len(ncol(out)), gridExpand = TRUE)
  for (i in sec_row_idx)
    openxlsx::addStyle(wb, sn, sec_st, rows = top + i, cols = seq_len(ncol(out)), gridExpand = TRUE)
  .wrap_multiline_cells(wb, sn, out, header_row = top)
  foot <- character(0)
  if (length(ne_why)) {
    # one line per distinct reason, in order of first appearance
    reasons <- unique(ne_why)
    foot <- c("NE = not estimable (no P value):",
              vapply(reasons, function(w) {
                rr <- unique(ne_row[ne_why == w])
                paste0("- ", w, ": ", paste(rr, collapse = "; "), ".")
              }, character(1), USE.NAMES = FALSE))
  }
  # what the cells hold (finding T1-10; the JCRS table rules): built from the
  # kinds of cell the table actually has
  vals <- pub_values_note(out)
  if (length(vals)) foot <- c(foot, if (length(foot)) "", vals)
  if (length(t_lab)) {
    # one line per test, in order of first appearance (decision D12)
    foot <- c(foot, if (length(foot)) "", "Tests behind the P values:",
              vapply(unique(t_lab), function(tl) {
                rr <- unique(t_row[t_lab == tl])
                paste0("- ", tl, ": ", paste(rr, collapse = "; "), ".")
              }, character(1), USE.NAMES = FALSE))
  }
  if (length(foot))
    openxlsx::writeData(wb, sn, foot, startRow = top + nrow(out) + 2, startCol = 1)
  openxlsx::setColWidths(wb, sn, cols = 1, widths = 38)
  if (ncol(out) > 1) openxlsx::setColWidths(wb, sn, cols = 2:ncol(out), widths = 18)
  openxlsx::freezePane(wb, sn, firstActiveRow = top + 1)
  invisible(out)
}

# ------------------------------------------------------------
# Build one full workbook for a prepared (filtered + grouped) dataset.
# Returns list(wb=, matched_only=data.frame or NULL, loveplots=list(),
#              match_failed=<pairing -> reason> or NULL).
# run: optional list(label =, values =) of the subset run this workbook
# belongs to (recorded in the Manifest); NULL values = all rows.
# ------------------------------------------------------------

# Visual-acuity notation and which columns are analysed in logMAR, for the
# Overview note and the Manifest. The pipeline sets res$va_format and
# res$logmar_cols_clean; without them (an older engine) the notation is
# cfg$va_format and the converted columns are the ones it stashed.
va_info <- function(dat, cfg, res) {
  fmt <- tolower(as.character(res$va_format %||% cfg$va_format %||% "decimal"))[1]
  va_roles <- vapply(c("pre_ucva", "pre_bcva", "post_ucva", "post_bcva"),
                     function(k) .role_col(res$roles %||% list(), k), character(1))
  va_roles <- unique(va_roles[!is.na(va_roles) & va_roles %in% names(dat)])
  lm_cols <- if (!is.null(res$logmar_cols_clean)) as.character(unlist(res$logmar_cols_clean))
             else if (is.null(res$va_format) && isTRUE(cfg$do_logmar))
               sub("^\\.pre_logmar__", "", grep("^\\.pre_logmar__", names(dat), value = TRUE))
             else character(0)
  lm_cols <- lm_cols[lm_cols %in% names(dat)]
  converted <- if (fmt != "logmar" && isTRUE(cfg$do_logmar)) lm_cols else character(0)
  in_logmar <- if (fmt == "logmar") unique(c(va_roles, lm_cols)) else converted
  as_decimal <- setdiff(unique(c(va_roles, lm_cols)), in_logmar)
  label <- switch(fmt,
    decimal = "decimal (1.0 = 20/20)",
    logmar  = "logMAR (0.0 = 20/20)",
    snellen = "Snellen fraction (20/x or 6/x, letter suffix accepted)",
    etdrs   = "ETDRS letter score",
    fmt)
  list(format = fmt, label = label, converted = converted, in_logmar = in_logmar,
       as_decimal = as_decimal, va_cols = unique(c(va_roles, lm_cols)))
}

# Range of matching weights, or "unit" when every weight is 1.
weights_summary <- function(w) {
  w <- suppressWarnings(as.numeric(w)); w <- w[is.finite(w)]
  if (!length(w)) return(NA_character_)
  if (all(abs(w - 1) < 1e-8)) return("unit (all weights = 1)")
  sprintf("non-unit (%s-%s)", format(signif(min(w), 3)), format(signif(max(w), 3)))
}

# Matching settings as the PSM settings sheet and the Manifest show them: only
# the ones the method uses (exact / cem ignore distance, ratio, replacement,
# caliper and order; subclass all but distance). psm: .psm_settings() output,
# whose $not_used names the ignored ones; derived from the method otherwise.
.psm_not_used <- function(psm) {
  if (!is.null(psm$not_used)) return(as.character(psm$not_used))
  m <- tolower(trimws(as.character(psm$method %||% "nearest")[1]))
  switch(m,
    exact = , cem = c("distance", "ratio", "replace", "caliper", "std_caliper", "m_order"),
    subclass = c("ratio", "replace", "caliper", "std_caliper", "m_order"),
    character(0))
}
.psm_setting_labels <- c(distance = "distance", ratio = "ratio", replace = "replacement",
                         caliper = "caliper", std_caliper = "standardized caliper",
                         m_order = "matching order")

# The method matchit() actually ran. psm$method_used (pairing label -> method,
# from run_reference_pairwise_matching) differs from the requested psm$method
# only when cem met categorical covariates only and ran as exact matching.
# pairing: that pairing's method; NULL: the one method every pairing ran (the
# requested one when they differ).
.psm_method_run <- function(psm, pairing = NULL) {
  req <- tolower(trimws(as.character(psm$method %||% "nearest")[1]))
  used <- psm$method_used
  if (!length(used)) return(req)
  if (!is.null(pairing) && pairing %in% names(used)) return(unname(used[[pairing]]))
  u <- unique(as.character(unname(used)))
  if (length(u) == 1) u else req
}
PSM_CEM_EXACT_WHY <- "every matching covariate is categorical, so coarsened exact matching is exact matching"

# The matching method actually run, in words, for the Manifest's "Matching" row
# (contract R4-5): "nearest-neighbour on the propensity score (MatchIt)",
# "... on Mahalanobis distance", "exact", "coarsened exact (cem)", "coarsened
# exact requested; run as exact (all covariates categorical)",
# "subclassification on the propensity score". psm: .psm_settings() output
# with $method_used (pairing -> method run) when known.
.psm_nn_distance <- function(distance) {
  d <- tolower(trimws(as.character(distance %||% "glm")[1]))
  if (is.na(d) || !nzchar(d)) d <- "glm"
  switch(d,
    mahalanobis        = "nearest-neighbor on Mahalanobis distance",
    robust_mahalanobis = "nearest-neighbor on robust Mahalanobis distance",
    euclidean          = "nearest-neighbor on Euclidean distance",
    scaled_euclidean   = "nearest-neighbor on scaled Euclidean distance",
    "nearest-neighbor on the propensity score")
}
psm_method_label <- function(psm) {
  req <- tolower(trimws(as.character(psm$method %||% "nearest")[1]))
  if (is.na(req) || !nzchar(req)) req <- "nearest"
  used <- as.character(unname(psm$method_used %||% character(0)))
  lab <- switch(req,
    nearest  = .psm_nn_distance(psm$distance),
    exact    = "exact",
    subclass = "subclassification on the propensity score",
    cem      = if (length(used) && all(used == "exact"))
                 "coarsened exact requested; run as exact (all covariates categorical)"
               else "coarsened exact (cem)",
    req)
  out <- paste0(lab, " (MatchIt)")
  # cem with categorical covariates only in SOME pairings: those ran as exact
  if (identical(req, "cem") && length(used) && any(used == "exact") && !all(used == "exact"))
    out <- paste0(out, "; run as exact for ",
                  paste(names(psm$method_used)[used == "exact"], collapse = ", "),
                  " (all covariates categorical)")
  out
}

# caliper_raw: the caliper's width on the score's own scale (MatchIt's
# m.out$caliper for this pairing), printed after the caliper (decision D13).
# The covariate-balance table of a pairing (cobalt's bal.tab) with its columns
# and type codes in words (finding T1-08): "Diff.Un" -> "Standardized difference
# before matching", "Diff.Adj" -> "... after matching", "M.Threshold" ->
# "Balanced (|SMD| < 0.1)", "Contin." -> "Continuous".
PSM_BALANCE_WORDS <- c(Type = "Type", Diff.Un = "Standardized difference before matching",
                       Diff.Adj = "Standardized difference after matching",
                       M.Threshold = "Balanced (|SMD| < 0.1)", M.Threshold.Un = "Balanced before matching (|SMD| < 0.1)")
psm_balance_words <- function(d) {
  if ("Type" %in% names(d))
    d$Type <- ifelse(as.character(d$Type) == "Contin.", "Continuous", as.character(d$Type))
  for (k in c("M.Threshold", "M.Threshold.Un")) if (k %in% names(d))
    d[[k]] <- sub("^Balanced, <0.1$", "yes", sub("^Not Balanced, >0.1$", "no", as.character(d[[k]])))
  hit <- names(d) %in% names(PSM_BALANCE_WORDS)
  names(d)[hit] <- unname(PSM_BALANCE_WORDS[names(d)[hit]])
  d
}

psm_settings_table <- function(psm, reference_group, patient_id_raw = NULL, pairing = NULL,
                               caliper_raw = NULL) {
  # NULL-safe: c() silently drops NULLs (e.g. an empty caliper box = no
  # caliper), which would misalign the two columns
  fmt_setting <- function(x) if (is.null(x) || length(x) == 0 || is.na(x[[1]])) "(none)" else as.character(x)[1]
  nu <- .psm_not_used(psm)
  requested <- tolower(trimws(fmt_setting(psm$method %||% "nearest")))
  method <- .psm_method_run(psm, pairing)
  rows <- list(c("Matching method", method))
  if (!identical(method, requested))
    rows[[length(rows) + 1]] <- c("Requested method", paste0(requested, " (", PSM_CEM_EXACT_WHY, ")"))
  has_scale <- exists("psm_score_scale", mode = "function")
  if (!("distance" %in% nu)) {
    sc <- if (has_scale) psm_score_scale(psm$distance) else NA_character_
    # in words, MatchIt's name kept in brackets (T1-08)
    rows[[length(rows) + 1]] <- c("Distance", if (!is.na(sc)) paste0("propensity score, ", sc, " (",
                                                                      fmt_setting(psm$distance), ")")
                                              else fmt_setting(psm$distance))
    if (!is.na(sc)) rows[[length(rows) + 1]] <- c("Propensity score scale", sc)
  }
  if (!("ratio" %in% nu))    rows[[length(rows) + 1]] <- c("Ratio", fmt_setting(psm$ratio))
  yes_no <- function(x) { v <- fmt_setting(x); if (toupper(v) == "TRUE") "yes" else if (toupper(v) == "FALSE") "no" else v }
  if (!("replace" %in% nu))  rows[[length(rows) + 1]] <- c("Replacement", yes_no(psm$replace))
  if (!("caliper" %in% nu)) {
    rows[[length(rows) + 1]] <- c("Caliper",
      if (has_scale && !is.null(psm$caliper))
        psm_caliper_text(psm$caliper, psm$std_caliper, if ("distance" %in% nu) NULL else psm$distance,
                         raw = caliper_raw)
      else fmt_setting(psm$caliper))
    if (!is.null(psm$caliper) && !("std_caliper" %in% nu))
      rows[[length(rows) + 1]] <- c("Standardized caliper", yes_no(psm$std_caliper))
  }
  if (!("m_order" %in% nu))  rows[[length(rows) + 1]] <- c("Matching order", fmt_setting(psm$m_order))
  if (length(nu))
    rows[[length(rows) + 1]] <- c(paste0("Not used by method '", method, "'"),
                                  paste(.psm_setting_labels[intersect(names(.psm_setting_labels), nu)],
                                        collapse = ", "))
  rows <- c(rows, list(
    c("Random seed (matching)", fmt_setting(psm$seed)),
    c("Reference group", fmt_setting(reference_group)),
    c("Patient ID (clustering for GEE only; not used in matching)", fmt_setting(patient_id_raw)),
    c("Matched analyses", matched_analysis_note())))
  data.frame(Setting = vapply(rows, `[`, character(1), 1),
             Value   = vapply(rows, `[`, character(1), 2),
             stringsAsFactors = FALSE)
}

# One line for the Manifest, with the same rule.
psm_settings_text <- function(psm) {
  one <- function(x, default) if (is.null(x) || length(x) == 0 || is.na(x[[1]])) default else as.character(x[[1]])
  nu <- .psm_not_used(psm)
  requested <- tolower(trimws(one(psm$method, "nearest")))
  method <- .psm_method_run(psm)
  # pairings that ran another method than the one requested (cem as exact)
  other <- names(psm$method_used)[as.character(psm$method_used) != requested]
  method_txt <- paste0("method ", method,
    if (!length(other)) ""
    else if (!identical(method, requested)) paste0(" (", requested, " requested; ", PSM_CEM_EXACT_WHY, ")")
    else paste0(" (exact for ", paste(other, collapse = ", "), ": ", PSM_CEM_EXACT_WHY, " there)"))
  cal <- one(psm$caliper, "none")
  # the caliper names the score's scale (D13): "caliper 0.2 SD of the logit of
  # the propensity score"
  cal_txt <- if (cal != "none" && exists("psm_caliper_text", mode = "function"))
    psm_caliper_text(psm$caliper, isTRUE(as.logical(one(psm$std_caliper, "TRUE"))),
                     if ("distance" %in% nu) NULL else psm$distance)
    else paste0(cal, if (cal != "none" && isTRUE(as.logical(one(psm$std_caliper, "TRUE"))))
                       " (SD of the propensity score)" else "")
  parts <- c(method_txt,
    if (!("distance" %in% nu)) paste("distance", one(psm$distance, "linear.logit")),
    if (!("ratio" %in% nu))    paste("ratio", one(psm$ratio, "1")),
    if (!("replace" %in% nu))  paste("replacement", if (isTRUE(as.logical(one(psm$replace, "FALSE")))) "on" else "off"),
    if (!("caliper" %in% nu))  paste0("caliper ", cal_txt),
    if (!("m_order" %in% nu))  paste("order", one(psm$m_order, "default")),
    if (!is.null(psm$seed))    paste("seed", one(psm$seed, "")))
  txt <- paste(parts, collapse = ", ")
  if (length(nu))
    txt <- paste0(txt, " (", paste(.psm_setting_labels[intersect(names(.psm_setting_labels), nu)], collapse = ", "),
                  if (length(nu) == 1) " does" else " do", " not apply to ", method, ")")
  txt
}

# Stack the matched rows of every pairing. A column whose type differs between
# pairings (e.g. a text covariate read as numbers in one pairing only) is
# stacked as text instead of aborting the run.
stack_matched_pairings <- function(lst) {
  if (length(lst) == 0) return(NULL)
  out <- tryCatch(dplyr::bind_rows(lst), error = function(e) NULL)
  if (!is.null(out)) return(out)
  fam <- function(x) {
    cl <- class(x)[1]
    if (cl %in% c("integer", "numeric", "logical")) "number"
    else if (cl %in% c("factor", "character")) "text" else cl
  }
  nms <- unique(unlist(lapply(lst, names)))
  for (nm in setdiff(nms, "GroupLabel")) {
    cls <- unique(unlist(lapply(lst, function(d) if (nm %in% names(d)) fam(d[[nm]]))))
    if (length(cls) > 1)
      lst <- lapply(lst, function(d) { if (nm %in% names(d)) d[[nm]] <- as.character(d[[nm]]); d })
  }
  dplyr::bind_rows(lst)
}

# The configured covariates that did not resolve to a column of the file, per
# use, as typed (pipeline.R res$*_unresolved; contract R4-5). Only the uses that
# run are listed; matching is listed only when it ran on the covariates that
# resolved (with none, pipeline.R logs the skip and the Manifest records it).
# Returns a named list: matching / GEE adjustment / regression -> names.
unresolved_covariates <- function(cfg, res) {
  pick <- function(x) { x <- as.character(unlist(x)); unique(x[!is.na(x) & nzchar(x)]) }
  gee_adj_on <- isTRUE(cfg$run_adjusted_gee) && as.character(cfg$run_gee_stage %||% "none")[1] != "none"
  out <- list(
    matching = if (isTRUE(cfg$do_matching) && length(res$psm_covariates_clean))
                 pick(res$psm_covariates_unresolved),
    `GEE adjustment` = if (gee_adj_on) pick(res$gee_adjust_unresolved),
    regression = if (isTRUE(cfg$do_regression %||% FALSE)) pick(res$regression_covs_unresolved))
  out[lengths(out) > 0]
}
# "not found in the file (ignored): 'A', 'B'" or "" when there is none.
.not_found_txt <- function(x) {
  x <- as.character(unlist(x))
  if (!length(x)) "" else paste0("not found in the file (ignored): ", paste0("'", x, "'", collapse = ", "))
}

# Love-plot file of one matched pairing (contract R4-4):
#   LovePlot_<prefix>_<subset stem>_<pairing>_<stamp>.png
# with the same prefix, stem and stamp as the run's workbook
# <prefix>_<stem>_<stamp>.xlsx (pipeline.R sets run$prefix / run$stem /
# run$stamp), so two runs into one folder never overwrite each other's plot.
# The name part is sanitised as one (safe_filename: a group name may hold
# / : ? ...); the stamp is appended after it, so a long name that
# safe_filename shortens never loses the stamp the server matches on.
# Without prefix and stamp (an older caller) the earlier name is kept:
# LovePlot_<pairing>.png, led by the run's stem for a subset run.
loveplot_path <- function(dir, pairing, run = NULL) {
  part <- function(x) { x <- as.character(unlist(x)); if (length(x) && !is.na(x[1]) && nzchar(x[1])) x[1] else NULL }
  prefix <- part(run$prefix); stamp <- part(run$stamp)
  if (!is.null(prefix) && !is.null(stamp)) {
    stem <- part(run$stem) %||% part(run$label)
    nm <- paste0("LovePlot_", safe_filename(paste(c(prefix, stem, pairing), collapse = "_")),
                 "_", safe_filename(stamp), ".png")
    return(file.path(dir, nm))
  }
  # a subset run's label leads, so runs sharing one folder never overwrite
  # (run$stem: the pipeline's unique, file-safe run name)
  tag <- if (!is.null(run) && length(run$values)) paste0(run$stem %||% run$label, "_", pairing) else pairing
  file.path(dir, paste0("LovePlot_", safe_filename(tag), ".png"))
}

# cobalt's balance-table row names (a covariate key, or "<key>_<level>" for a
# factor level; "distance" for the propensity score) with each covariate key
# replaced by its header (R4-3). Longest keys first, so "Age_2" is never read
# as "Age" + "_2"; a key that IS its header leaves the name unchanged.
.balance_display_names <- function(vars, keys, map) {
  out <- as.character(vars)
  if (!length(out) || is.null(map) || !length(map)) return(out)
  keys <- unique(as.character(unlist(keys)))
  keys <- keys[!is.na(keys) & nzchar(keys)]
  keys <- keys[order(-nchar(keys))]
  done <- rep(FALSE, length(out))
  for (k in keys) {
    d <- disp_name(k, map)
    hit <- !done & (out == k | startsWith(out, paste0(k, "_")))
    if (any(hit) && !identical(d, k))
      out[hit] <- paste0(d, substring(out[hit], nchar(k) + 1L))
    done <- done | hit
  }
  out
}

build_workbook_for_data <- function(dat, cfg, res, logger = message,
                                    loveplot_dir = tempdir(), run = NULL) {
  # the workbook's author is the app, never the Windows user name (D25)
  wb <- openxlsx::createWorkbook(creator = WORKBOOK_CREATOR)
  glevels <- levels(dat$GroupLabel)
  # clean key -> the header as typed in the file (pipeline.R, contract R4-3):
  # every label the user reads names the column by its header
  dmap <- res$display_names
  dn <- function(x) disp_name(x, dmap)

  # configured covariates that are not in the file: one WARNING line per
  # configuration (log_once; this function runs once per subset run)
  unres <- unresolved_covariates(cfg, res)
  if (length(unres))
    log_once(logger, paste0("WARNING: covariate(s) not found in the file and ignored - ",
                            paste(vapply(names(unres), function(u)
                              paste0(u, ": ", paste0("'", unres[[u]], "'", collapse = ", ")),
                              character(1)), collapse = "; "),
                            ". The models use only the covariates found; the workbook lists both."))

  # --- Overview: initial summary + QC on one sheet ---
  init <- make_initial_summary(dat, res$patient_id_clean, res$age_clean, res$fu_clean, res$sex_clean,
                               logger = logger)
  qc <- dat %>% dplyr::count(!!rlang::sym(res$group_source_clean), GroupLabel, name = "N") %>%
    dplyr::arrange(dplyr::desc(N)) %>%
    dplyr::rename(Group_Source_Value = !!rlang::sym(res$group_source_clean))
  vi <- va_info(dat, cfg, res)
  va_notes <- c(
    if (length(vi$in_logmar))
      paste0("Visual acuity analyzed in logMAR (lower = better): ", paste(dn(vi$in_logmar), collapse = ", "),
             if (vi$format == "logmar") " - as recorded." else " - converted from the recorded notation."),
    if (length(vi$as_decimal))
      paste0("Visual acuity analyzed as decimal acuity (higher = better): ",
             paste(dn(vi$as_decimal), collapse = ", "),
             if (vi$format %in% c("snellen", "etdrs")) paste0(" - converted from ", vi$label, ".") else "."))
  write_stacked_tables(wb, "Overview", list(
    `Notes` = if (length(va_notes)) data.frame(Note = va_notes, stringsAsFactors = FALSE) else NULL,
    `Initial Summary` = init$initial,
    `Sex (per patient)` = init$gender,
    `Groups` = init$groups,
    `QC - group assignment by source value` = qc))

  # the table tests (decision D12): patient-clustered wherever a patient has
  # two analysed eyes, unless the classical tests are chosen
  tbl_tests <- table_tests_of(cfg)
  # acuity analysed in logMAR: Snellen geometric mean, logMAR mean ± SD and the
  # range in Snellen in every cell (D16)
  va_lm <- vi$in_logmar
  summarise_block <- function(params, tmap) create_summary_table(
    dat, params, tmap, glevels, "GroupLabel",
    res$patient_id_clean, res$patient_level_params_clean, res$dicho_pref_clean,
    logger = logger, display_names = dmap, table_tests = tbl_tests, va_cols = va_lm)

  baseline_tbl <- summarise_block(res$baseline_params_clean, res$baseline_type_map)
  preop_tbl    <- summarise_block(res$preop_params_clean,    res$preop_type_map)
  outcome_tbl  <- summarise_block(res$outcome_params_clean,  res$outcome_type_map)

  # GEE results that feed the summary-table columns: the adjusted models when
  # gee_source_for_tables = "adjusted" and they ran, otherwise the main ones.
  gee_stage <- as.character(cfg$run_gee_stage %||% "none")
  gee_tbl_src <- function(main, adj) {
    if (identical(cfg$gee_source_for_tables, "adjusted") && !is.null(adj)) adj
    else if (!is.null(main)) main else adj
  }
  merge_gee3 <- function(tbls, src, terms) {
    if (!isTRUE(cfg$add_gee_columns) || is.null(src)) return(tbls)
    for (k in names(tbls)) tbls[[k]] <- merge_gee_into_summary_table(
      tbls[[k]], build_gee_lookup(src[[k]], terms))
    tbls
  }

  # --- Unmatched GEE (factor group predictor: handles >2 groups). Fitted
  # BEFORE the summary sheets are written so its effect + P can be merged into
  # the unmatched tables, as for the matched ones.
  gee_un <- list()
  if (gee_stage %in% c("unmatched", "both")) {
    gee_run <- function(params, tmap, preds) run_gee_models(
      dat, params, tmap, preds, res$patient_id_clean, res$dicho_pref_clean,
      use_weights = FALSE, logger = logger, display_names = dmap,
      patient_level = res$patient_level_params_clean)
    gee_set <- function(preds) list(
      b = gee_run(res$baseline_params_clean, res$baseline_type_map, preds),
      p = gee_run(res$preop_params_clean,    res$preop_type_map,    preds),
      o = gee_run(res$outcome_params_clean,  res$outcome_type_map,  preds))
    if (isTRUE(cfg$run_main_gee))     gee_un$main <- gee_set("GroupLabel")
    if (isTRUE(cfg$run_adjusted_gee)) gee_un$adj  <- gee_set(unique(c("GroupLabel", res$gee_adjust_clean)))
  }
  tb <- merge_gee3(list(b = baseline_tbl, p = preop_tbl, o = outcome_tbl),
                   gee_tbl_src(gee_un$main, gee_un$adj), paste0("GroupLabel", glevels[-1]))
  baseline_tbl <- tb$b; preop_tbl <- tb$p; outcome_tbl <- tb$o

  # Adjusted regression (unmatched) runs before the tables are written so its
  # effect + P can be embedded into the Outcome Summary / Publication Table
  rr_un <- NULL
  if (isTRUE(cfg$do_regression %||% FALSE) &&
      (cfg$regression_stage %||% "unmatched") %in% c("unmatched", "both")) {
    rr_un <- run_regression_models(dat, res$outcome_params_clean, res$outcome_type_map,
                                   res$regression_covs_clean, res$patient_id_clean,
                                   res$dicho_pref_clean, glevels,
                                   use_weights = FALSE, logger = logger, display_names = dmap,
                                   patient_level = res$patient_level_params_clean)
    outcome_tbl <- merge_regression_into_summary(outcome_tbl, rr_un$pub)
  }

  # one table on a sheet, with an optional bold note row above it
  write_tbl <- function(name, tbl, note = NULL) {
    sn <- add_or_replace_worksheet(wb, name)
    top <- 1
    if (!is.null(note) && nzchar(note)) {
      openxlsx::writeData(wb, sn, note, startRow = 1, startCol = 1)
      openxlsx::addStyle(wb, sn, openxlsx::createStyle(textDecoration = "bold", fontColour = "#C00000"),
                         rows = 1, cols = 1)
      top <- 3
    }
    tbl <- as.data.frame(tbl, check.names = FALSE)
    names(tbl) <- table_headers(names(tbl))         # "P" (D24)
    openxlsx::writeData(wb, sn, tbl, startRow = top)
    .wrap_multiline_cells(wb, sn, tbl, header_row = top)
  }
  # optional tabs never abort the results workbook
  optional_tab <- function(what, expr) tryCatch(expr, error = function(e)
    logger(paste0(what, " skipped: ", conditionMessage(e))))
  # Within-group sections (per group, no between-group test) live on the same
  # sheet as the baseline/preop tables, before AND (below) after matching.
  within_sections <- function(df_src, lvls) {
    out <- list()
    if (length(res$within_params_clean %||% character(0)) == 0) return(out)
    # show the patient count in the heading too as soon as any variable is
    # summarised per patient, so the two denominators are both visible
    any_pat <- any(vapply(res$within_levels_clean %||% list(),
                          function(u) identical(u, "patient"), logical(1))) &&
               !is.na(res$patient_id_clean)
    for (g in lvls) {
      dfg <- df_src[as.character(df_src$GroupLabel) == g, , drop = FALSE]
      tbl <- describe_within_group(dfg, res$within_params_clean, res$within_type_map,
                                   res$dicho_pref_clean, res$within_levels_clean,
                                   res$patient_id_clean, display_names = dmap, va_cols = va_lm)
      denom <- paste0(nrow(dfg), " eyes")
      if (any_pat && res$patient_id_clean %in% names(dfg))
        denom <- paste0(denom, "; ", dplyr::n_distinct(dfg[[res$patient_id_clean]]), " patients")
      out[[paste0("Within-group only - ", g, " (n = ", denom,
                  "; no between-group test)")]] <- tbl
    }
    out
  }

  write_stacked_tables(wb, "Baseline & Preop Summary", c(
    list(`Baseline` = baseline_tbl,
         `Preoperative / intraoperative` = preop_tbl),
    within_sections(dat, glevels)))
  write_tbl("Outcome Summary", outcome_tbl)

  if (isTRUE(cfg$make_pub_table %||% TRUE))
    make_publication_table(wb, "Publication Table",
      list(`Baseline characteristics`      = baseline_tbl,
           `Preoperative / intraoperative` = preop_tbl,
           `Outcomes`                      = outcome_tbl))

  if (isTRUE(cfg$make_meyestro))
    optional_tab("mEYEstro tabs", make_meyestro_tabs(dat, wb, res$roles, pairing = NULL,
                                                     target = res$astig_target %||% list(),
                                                     logger = logger, display_names = dmap))
  if (isTRUE(cfg$make_alpins))
    optional_tab("AstigMATIC tabs", make_alpins_tabs(dat, wb, res$roles, pairing = NULL, logger = logger,
                                                     display_names = dmap))
  # astigmatism per the JCRS 2025 standard (D8): its sheet, the per-eye
  # vectors and - after the matched pairings - a CSV copy of the tables
  astig_tables <- list()
  astig_on <- isTRUE(cfg$astig_sheet %||% TRUE)
  if (astig_on)
    optional_tab("Astigmatism (JCRS 2025) sheet", {
      tb <- make_astig_jcrs_sheet(dat, wb, cfg, res, "Astigmatism (JCRS 2025)", "all analyzed eyes",
                                  logger = logger, vectors_sheet = TRUE)
      if (!is.null(tb)) astig_tables[["All analyzed eyes"]] <- tb
    })

  # --- Unmatched GEE sheets (models fitted above) ---
  gee_sheet <- function(g, name)
    write_stacked_tables(wb, name, list(`GEE results` = g), note = gee_skip_note(g), model_ref = glevels[1])
  if (!is.null(gee_un$main)) {
    gee_sheet(gee_un$main$b, "GEE_Main_Unmatched_Base")
    gee_sheet(gee_un$main$p, "GEE_Main_Unmatched_Preop")
    gee_sheet(gee_un$main$o, "GEE_Main_Unmatched_Out")
  }
  if (!is.null(gee_un$adj)) {
    gee_sheet(gee_un$adj$b, "GEE_Adj_Unmatched_Base")
    gee_sheet(gee_un$adj$p, "GEE_Adj_Unmatched_Preop")
    gee_sheet(gee_un$adj$o, "GEE_Adj_Unmatched_Out")
  }

  # --- Multivariable regression sheets (adjusted; GEE-clustered) ---
  # the title lists the covariates that resolved and were used (by their
  # headers), and the configured ones that are not in the file (R4-5)
  reg_covs_used <- as.character(unlist(res$regression_covs_clean))
  reg_covs_txt <- paste0(if (length(reg_covs_used)) paste(dn(reg_covs_used), collapse = ", ") else "none",
                         if (length(unres$regression)) paste0("; ", .not_found_txt(unres$regression)) else "")
  # the reference group of a regression's models: the first comparison's
  ref_of <- function(rr) {
    cmp <- as.character(rr$pub$Comparison %||% character(0)); cmp <- cmp[!is.na(cmp) & nzchar(cmp)]
    if (length(cmp)) sub("^.* vs ", "", cmp[1]) else glevels[1]
  }
  reg_sheet <- function(rr, sheet, cohort_label) {
    note <- if (rr$n_flagged > 0) paste0(
      "NOTE: ", rr$n_flagged, " model(s) carry a small-sample caution or failed - ",
      "see the Note column. Interpret flagged estimates cautiously.") else NULL
    # the inference of every SE, P and CI (D11), and the crude columns (D17)
    write_stacked_tables(wb, sheet, stats::setNames(list(rr$pub, rr$full),
      c(paste0("Adjusted regression - ", cohort_label, " (covariates: ", reg_covs_txt, "; ",
               gee_se_label(1), "; working correlation: ",
               if (startsWith(cohort_label, "matched")) "independence"
               else "exchangeable unless the Model column names another",
               "; crude columns: the group alone, fitted on the same eyes)"),
        "Full models (every term)")), note = note, model_ref = ref_of(rr))
  }
  reg_write <- function(data, sheet, cohort_label, use_w = FALSE, matched = FALSE) {
    rr <- run_regression_models(data, res$outcome_params_clean, res$outcome_type_map,
                                res$regression_covs_clean, res$patient_id_clean,
                                res$dicho_pref_clean, levels(data$GroupLabel),
                                use_weights = use_w, logger = logger, display_names = dmap,
                                matched = matched, patient_level = res$patient_level_params_clean)
    reg_sheet(rr, sheet, cohort_label)
    invisible(rr)
  }
  if (!is.null(rr_un)) reg_sheet(rr_un, "Regression (adjusted)", "unmatched cohort")

  matched_only_list <- list()
  loveplots <- list()
  match_w <- NULL
  eye_rows <- list()
  match_failed <- NULL
  match_methods <- NULL

  # --- Matching (reference-group pairwise) ---
  if (isTRUE(cfg$do_matching) && length(res$psm_covariates_clean) > 0) {
    reference_group <- if (!is.null(cfg$reference_group) && cfg$reference_group %in% glevels)
      cfg$reference_group else glevels[1]

    psm <- list(method = cfg$psm_method, distance = cfg$psm_distance, ratio = cfg$psm_ratio,
                replace = cfg$psm_replace, caliper = cfg$psm_caliper,
                std_caliper = cfg$psm_std_caliper, m_order = cfg$psm_m_order,
                seed = cfg$psm_seed)

    # .se_eye: the row of `dat` each matched row is, so the matched-only
    # workbook can count a reference eye matched in several pairings once
    dat_m <- dat
    dat_m$.se_eye <- seq_len(nrow(dat_m))
    pairings <- run_reference_pairwise_matching(dat_m, glevels, reference_group,
                                                res$psm_covariates_clean, psm, logger,
                                                display_names = dmap)
    rm(dat_m)
    # the settings matchit() actually received (blank ratio -> 1, default seed ...)
    if (!is.null(attr(pairings, "settings"))) psm <- attr(pairings, "settings")
    match_failed <- attr(pairings, "failed")
    match_methods <- psm$method_used

    # the covariates that resolved and were used, by their headers; the
    # configured ones not in the file in a section of their own (R4-5)
    psm_covs_df <- data.frame(`Matching covariate` = dn(as.character(unlist(res$psm_covariates_clean))),
                              check.names = FALSE, stringsAsFactors = FALSE)
    psm_unres_df <- if (length(unres$matching))
      data.frame(`Matching covariate` = unres$matching, check.names = FALSE, stringsAsFactors = FALSE) else NULL
    # the patient-ID column as the PSM sheet names it: its header
    pid_key <- res$patient_id_clean %||% NA_character_
    pid_shown <- if (!is.na(pid_key[1]) && pid_key[1] %in% names(dat)) dn(pid_key[1]) else cfg$patient_id_raw

    for (pr in pairings) {
      md <- pr$matched
      if (".se_eye" %in% names(md)) {
        eye_rows[[pr$label]] <- md$.se_eye
        md$.se_eye <- NULL
      }
      pair_lvls <- c(pr$reference, pr$treated)
      md$GroupLabel <- droplevels(factor(as.character(md$GroupLabel), levels = pair_lvls))
      lab <- pr$label

      # Balance table + love plot
      bal_df <- NULL
      if (!is.null(pr$bal)) {
        # subclassification: cobalt keeps the table in Balance.Across.Subclass
        bal_tab <- pr$bal$Balance %||% pr$bal$Balance.Across.Subclass
        # cobalt names the covariates by their keys ("Sex_M" for a factor
        # level): shown by their headers (R4-3), in the table and the plot
        bal_vars <- if (!is.null(bal_tab)) rownames(bal_tab) else character(0)
        bal_shown <- .balance_display_names(bal_vars, res$psm_covariates_clean, dmap)
        # cobalt's "distance" row is the propensity score, named with its
        # scale: "Propensity score (logit)" (decision D13)
        if (exists("psm_score_label", mode = "function"))
          bal_shown[bal_vars == "distance"] <- psm_score_label(psm$distance)
        if (!is.null(bal_tab)) {
          bal_df <- as.data.frame(bal_tab); bal_df$Variable <- bal_shown
          bal_df <- bal_df %>% dplyr::relocate(Variable)
          # cobalt's column names and type codes in words (T1-08)
          bal_df <- psm_balance_words(bal_df)
        }
        lp <- loveplot_path(loveplot_dir, lab, run)
        # a publication-ready balance plot (D23): no title inside the image,
        # covariates under their headers, the propensity score named with its
        # scale, grey / black markers - written by the figures' own writer
        # (graphs.R save_graph), so it follows the chosen formats, resolution,
        # width and figure style, and every failed write is logged with its
        # reason (a half-drawn file is never left behind)
        lp_args <- list(pr$bal, threshold = 0.1, stars = "raw",
                        colors = c("grey55", "black"), shapes = c("circle", "triangle"))
        renamed <- bal_shown != bal_vars
        if (any(renamed)) lp_args$var.names <- stats::setNames(bal_shown[renamed], bal_vars[renamed])
        lp_what <- paste0("Love plot for ", lab)
        lp_plot <- tryCatch(do.call(cobalt::love.plot, lp_args) + ggplot2::labs(title = NULL) +
                              ggplot2::theme(plot.background = ggplot2::element_rect(fill = "white", colour = NA)),
                            error = function(e) { logger(paste0(lp_what, " not drawn: ", conditionMessage(e))); NULL })
        # journal style (D20): open L-shaped axes instead of cobalt's panel box
        if (!is.null(lp_plot) && exists("gr_journal_axes", mode = "function") &&
            isTRUE(gr_figure_settings(cfg, logger = NULL)$journal))
          lp_plot <- lp_plot + gr_journal_axes()
        if (!is.null(lp_plot) && exists("save_graph", mode = "function")) {
          fs <- gr_figure_settings(cfg, logger = NULL)
          lw <- if (is.finite(fs$width_in)) fs$width_in else 9
          lh <- lw * 2 / 3
          # a single journal column (narrower than 12 cm, finding F9): the key
          # goes under the plot, so the plotting area keeps the full width
          if (lw < 12 / 2.54) {
            lp_plot <- lp_plot + ggplot2::theme(legend.position = "bottom", legend.title = ggplot2::element_blank())
            lh <- lw * 0.8
          }
          got <- save_graph(lp_plot, sub("\\.png$", "", lp), fs$formats, fs$dpi, w = lw, h = lh,
                            logger = logger, what = lp_what, count = FALSE)
          if (length(got)) {
            loveplots[[lab]] <- got[order(!grepl("\\.png$", got))][1]
            gr_write_legend(sub("\\.png$", "", lp),
                            paste0("Covariate balance before and after matching, ", gsub("_", " ", lab),
                                   # the subset of a subset run (finding F8): figures moved out of
                                   # their folder can still be told apart
                                   if (!is.null(run) && length(run$values)) paste0(", subset: ", run$label) else ""),
                            c(paste0("Standardized mean differences of the matching covariates before ",
                                     "(Unadjusted) and after matching (Adjusted), computed by cobalt; the ",
                                     "dashed lines mark -0.1 and 0.1; grey circles, before matching; black ",
                                     "triangles, after matching."),
                              "* (where shown): the raw (unstandardized) difference in means.",
                              if (!is.na(psm_score_scale(psm$distance)))
                                paste0(psm_score_label(psm$distance), ": the ", psm_score_scale(psm$distance),
                                       ", on which the matching was done.")),
                            texts = c(bal_shown, "Standardized Mean Differences"), cfg = cfg)
          }
        }
      }

      # PS distribution - only when a propensity score exists (R4-5): exact,
      # cem and the non-propensity distances (Mahalanobis ...) estimate none,
      # and their block lists the matching weights only
      has_ps <- "distance" %in% names(md) && any(is.finite(suppressWarnings(as.numeric(md$distance))))
      ps_dist <- md
      if (has_ps) ps_dist$PropensityScore <- md$distance
      ps_dist$MatchingWeight  <- if ("weights"  %in% names(md)) md$weights  else NA_real_
      keepcols <- intersect(c(res$patient_id_clean, "GroupLabel", if (has_ps) "PropensityScore", "MatchingWeight"),
                            names(ps_dist))
      ps_dist <- ps_dist[, keepcols, drop = FALSE]
      # the listing's headers in words (T1-08)
      names(ps_dist)[names(ps_dist) == "GroupLabel"] <- "Group"
      names(ps_dist)[names(ps_dist) == "MatchingWeight"] <- "Matching weight"
      names(ps_dist)[names(ps_dist) == "PropensityScore"] <-
        if (exists("psm_score_label", mode = "function")) psm_score_label(psm$distance) else "Propensity score"
      # the patient-ID column under its header
      j <- which(names(ps_dist) == pid_key[1])
      if (length(j)) names(ps_dist)[j] <- dn(pid_key[1])
      ps_sections <- list(ps_dist)
      # the score's scale in the section title (D13): "... (logit of the
      # propensity score; matched rows)"
      ps_scale <- if (exists("psm_score_scale", mode = "function")) psm_score_scale(psm$distance) else NA
      names(ps_sections) <- if (has_ps) paste0("Propensity-score distribution (",
                                               if (!is.na(ps_scale)) paste0(ps_scale, "; ") else "",
                                               "matched rows)")
                            else "Matching weights (matched rows; no propensity score with this method)"

      # One merged PSM sheet per pairing: settings (with the method this pairing
      # actually ran) + covariates + balance + PS dist
      write_stacked_tables(wb, paste0("PSM_", lab), c(list(
        `PSM settings` = psm_settings_table(psm, reference_group, pid_shown, pairing = lab,
                                            caliper_raw = tryCatch(pr$m.out$caliper, error = function(e) NULL)),
        `Matching covariates` = psm_covs_df,
        `Matching covariates not found in the file (ignored)` = psm_unres_df,
        `Covariate balance (cobalt; standardized differences for every covariate)` = bal_df),
        ps_sections))

      # Matched summary tables (2 groups: reference + treated). MatchIt gives a
      # re-used or k:1-matched row a weight != 1: the tables then use the same
      # weights as the matched design (weighted cells and tests, ESS in the
      # header), and every matched table sheet says so.
      w_md <- if ("weights" %in% names(md)) suppressWarnings(as.numeric(md$weights)) else NULL
      match_w <- c(match_w, w_md)
      w_nonunit <- !is.null(w_md) && any(is.finite(w_md) & abs(w_md - 1) > 1e-8)
      # the merged GEE / regression columns use the weights only when
      # "use matching weights in matched GEE" is on - the note says which
      w_gee_cols <- (gee_stage %in% c("matched", "both") && isTRUE(cfg$add_gee_columns) &&
                       (isTRUE(cfg$run_main_gee) || isTRUE(cfg$run_adjusted_gee))) ||
                    (isTRUE(cfg$do_regression %||% FALSE) &&
                       (cfg$regression_stage %||% "unmatched") %in% c("matched", "both"))
      w_note <- if (w_nonunit) paste0(
        "NOTE: the matched cohort carries non-unit matching weights (",
        format(signif(min(w_md, na.rm = TRUE), 3)), "-", format(signif(max(w_md, na.rm = TRUE), 3)),
        "). Descriptives and the table tests are weighted by the MatchIt weights (weighted Wald test, GEE robust SE); ",
        "n = matched rows, ESS = effective sample size. ",
        if (w_gee_cols) {
          if (isTRUE(cfg$use_matching_weights_in_gee))
            "The GEE / adjusted-regression columns are weighted too. "
          else "The GEE / adjusted-regression columns are UNWEIGHTED (matching weights not used in matched GEE), so their P values can differ from the weighted test. "
        },
        "Within-group sections and the ",
        "mEYEstro / AstigMATIC tabs list the matched rows unweighted. ",
        matched_analysis_note())
        # unit weights: the matched tables' clustered tests and models follow
        # decision D18 too, and the sheet says so
        else paste0("NOTE: matched cohort (unit matching weights). ", matched_analysis_note())
      sub_glevels <- pair_lvls
      msum <- function(params, tmap) create_summary_table(
        md, params, tmap, sub_glevels, "GroupLabel",
        res$patient_id_clean, res$patient_level_params_clean, res$dicho_pref_clean,
        weights = w_md, logger = logger, display_names = dmap,
        table_tests = tbl_tests, matched = TRUE, va_cols = va_lm)
      b_m <- msum(res$baseline_params_clean, res$baseline_type_map)
      p_m <- msum(res$preop_params_clean,    res$preop_type_map)
      o_m <- msum(res$outcome_params_clean,  res$outcome_type_map)

      # Matched GEE
      use_w <- isTRUE(cfg$use_matching_weights_in_gee)
      gee_pair <- function(params, tmap, preds) run_gee_models(
        md, params, tmap, preds, res$patient_id_clean, res$dicho_pref_clean,
        use_weights = use_w, logger = logger, display_names = dmap, matched = TRUE,
        patient_level = res$patient_level_params_clean)
      # a matched cohort: independence working correlation, weighted (R4-1)
      # or not (D18), stated in the GEE sheets' note
      m_corstr <- if (exists("gee_corstr_label", mode = "function")) gee_corstr_label(md, use_w, matched = TRUE) else NULL

      if (gee_stage %in% c("matched","both")) {
        preds_main <- "GroupLabel"
        preds_adj  <- unique(c("GroupLabel", res$gee_adjust_clean))
        g_b <- g_p <- g_o <- NULL
        if (isTRUE(cfg$run_main_gee)) {
          g_b <- gee_pair(res$baseline_params_clean, res$baseline_type_map, preds_main)
          g_p <- gee_pair(res$preop_params_clean,    res$preop_type_map,    preds_main)
          g_o <- gee_pair(res$outcome_params_clean,  res$outcome_type_map,  preds_main)
          gg <- dplyr::bind_rows(g_b, g_p, g_o)
          write_stacked_tables(wb, paste0("GEE_M_", lab), list(
            `GEE - baseline` = g_b, `GEE - preop/intraop` = g_p, `GEE - outcomes` = g_o),
            note = gee_skip_note(gg, m_corstr), model_ref = pr$reference)
        }
        ga_b <- ga_p <- ga_o <- NULL
        if (isTRUE(cfg$run_adjusted_gee)) {
          ga_b <- gee_pair(res$baseline_params_clean, res$baseline_type_map, preds_adj)
          ga_p <- gee_pair(res$preop_params_clean,    res$preop_type_map,    preds_adj)
          ga_o <- gee_pair(res$outcome_params_clean,  res$outcome_type_map,  preds_adj)
          gga <- dplyr::bind_rows(ga_b, ga_p, ga_o)
          write_stacked_tables(wb, paste0("GEE_Adj_M_", lab), list(
            `Adjusted GEE - baseline` = ga_b, `Adjusted GEE - preop/intraop` = ga_p,
            `Adjusted GEE - outcomes` = ga_o),
            note = gee_skip_note(gga, m_corstr), model_ref = pr$reference)
        }

        tb <- merge_gee3(list(b = b_m, p = p_m, o = o_m),
                         gee_tbl_src(if (isTRUE(cfg$run_main_gee)) list(b = g_b, p = g_p, o = g_o),
                                     if (isTRUE(cfg$run_adjusted_gee)) list(b = ga_b, p = ga_p, o = ga_o)),
                         paste0("GroupLabel", pr$treated))
        b_m <- tb$b; p_m <- tb$p; o_m <- tb$o
      }

      # matched regression first, so its columns embed into the matched outcome table
      if (isTRUE(cfg$do_regression %||% FALSE) &&
          (cfg$regression_stage %||% "unmatched") %in% c("matched", "both")) {
        rr_m <- reg_write(md, paste0("Reg_M_", lab), paste0("matched cohort (", lab, ")"),
                          use_w = isTRUE(cfg$use_matching_weights_in_gee), matched = TRUE)
        o_m <- merge_regression_into_summary(o_m, rr_m$pub)
      }

      write_stacked_tables(wb, paste0("BasePreop_M_", lab), c(
        list(`Baseline (matched)` = b_m,
             `Preoperative / intraoperative (matched)` = p_m),
        within_sections(md, pair_lvls)), note = w_note)
      write_tbl(paste0("Out_M_", lab), o_m, note = w_note)

      if (isTRUE(cfg$make_pub_table %||% TRUE))
        make_publication_table(wb, paste0("Pub_M_", lab),
          list(`Baseline characteristics`      = b_m,
               `Preoperative / intraoperative` = p_m,
               `Outcomes`                      = o_m), note = w_note)

      if (isTRUE(cfg$make_meyestro))
        optional_tab("mEYEstro tabs", make_meyestro_tabs(md, wb, res$roles, pairing = lab,
                                                         target = res$astig_target %||% list(),
                                                         logger = logger, display_names = dmap))
      if (isTRUE(cfg$make_alpins))
        optional_tab("AstigMATIC tabs", make_alpins_tabs(md, wb, res$roles, pairing = lab, logger = logger,
                                                         display_names = dmap))
      if (astig_on)
        optional_tab("Astigmatism (JCRS 2025) sheet", {
          tb <- make_astig_jcrs_sheet(md, wb, cfg, res, paste0("Astig_M_", lab),
                                      paste0("matched cohort ", lab), logger = logger, matched = TRUE)
          if (!is.null(tb)) astig_tables[[paste0("Matched ", lab)]] <- tb
        })

      md$Pairing <- lab
      matched_only_list[[lab]] <- md
    }
  }

  # the CSV copy of the astigmatism sheets, named after the workbook
  # (<prefix>_<run stem>_Astigmatism_<stamp>.csv, beside it)
  if (length(astig_tables)) {
    if (!is.null(run) && !is.null(run$stem) && !is.null(run$stamp))
      optional_tab("Astigmatism CSV", {
        fp <- file.path(loveplot_dir, paste0(run$prefix %||% "StandardEyes", "_", run$stem, "_Astigmatism_",
                                             run$stamp, ".csv"))
        if (exists(".pl_os_path", mode = "function")) fp <- .pl_os_path(fp)
        write_astig_csv(astig_tables, fp, logger)
      })
    else log_once(logger, "Astigmatism CSV not written: no run name (direct call) - the workbook sheet holds the tables.")
  }

  matched_only <- stack_matched_pairings(matched_only_list)
  # which row of the cohort each stacked row is (attribute, not a column, so
  # it never reaches an exported sheet): the matched-only Manifest counts eyes
  if (!is.null(matched_only) && length(eye_rows) == length(matched_only_list) &&
      sum(lengths(eye_rows)) == nrow(matched_only))
    attr(matched_only, "eye_row") <- unlist(eye_rows[names(matched_only_list)], use.names = FALSE)
  add_contents_and_manifest(wb, dat, cfg, res, logger, run = run, match_weights = match_w,
                            match_failed = match_failed, match_methods = match_methods)
  # match_failed: the pairings without a matched cohort, and match_methods: the
  # method each matched pairing ran, for the matched-only workbook's Manifest
  # (pipeline.R)
  list(wb = wb, matched_only = matched_only, loveplots = loveplots, match_failed = match_failed,
       match_methods = match_methods)
}

# ------------------------------------------------------------
# Contents + Manifest sheets.
# Contents: a clickable index of every sheet, because a full run produces
# 26-39 sheets and Excel truncates long tab names.
# Manifest: everything needed to reproduce the run - versions, input file and
# its checksum, key settings, group sizes and the fixed bootstrap seed.
# ------------------------------------------------------------
# nm: the sheet's full (untruncated) title. The more specific rules come
# first (GEE_Adj_M_ before GEE_Adj_, the matched export tabs before the
# full-cohort ones).
describe_sheet <- function(nm) {
  mey <- paste0("copy columns E-S (15 columns, template order) into the mEYEstro template - ",
                "yellow E-I preop refraction, vertex (12 mm), preop CDVA; green J-M intended refraction; ",
                "red N-S postop refraction, vertex, postop CDVA, postop UDVA. Gray A-D (group, decimal ",
                "acuities) and blue T (postop SEQ) are for reference only.")
  ast <- paste0("columns A-E (eye OD/OS, preop cylinder, preop axis, postop cylinder, postop axis) ",
                "load directly into AstigMATIC; written in minus-cylinder notation.")
  jcrs <- paste0("plus-cylinder format: per vector (PRE, SIA, PE, and POST with a target that is not plano) and ",
                 "cohort the centroid, total SD, magnitudes and cumulative intervals; the tests (centroid against ",
                 "zero; centroid, precision, mean magnitude and intervals between cohorts; McNemar preop vs postop) ",
                 "with nominal and Holm-adjusted P; the vertices of the depth-based convex polygon and of the ",
                 "bootstrap convex polygon of the centroid (as in the WHWK functions); method notes.")
  rules <- list(
    c("^Contents$",           "This index."),
    c("^Manifest$",           "Versions, input checksum and settings of this run."),
    c("^Overview$",           "Initial data summary, notes and quality checks."),
    c("^Baseline & Preop",    "Baseline and preoperative tables, with within-group sections."),
    c("^Outcome Summary$",    "Outcome comparisons (GEE / adjusted columns merged in when requested)."),
    c("^Publication Table$",  "Manuscript-ready stacked table."),
    c("^mEYEstro_M_",         paste("mEYEstro-format export of one group of a matched pairing (matched rows, unweighted):", mey)),
    c("^mEYEstro_",           paste("mEYEstro-format export of one group (all rows):", mey)),
    c("^AstigMATIC_M_",       paste("AstigMATIC input for one group of a matched pairing (matched rows, unweighted):", ast)),
    c("^AstigMATIC_",         paste("AstigMATIC input for one group (all rows):", ast)),
    c("^Astigmatism \\(JCRS 2025\\)$", paste("Astigmatism per the JCRS 2025 standard (all analyzed eyes; a CSV copy is written beside the workbook),", jcrs)),
    c("^Astigmatism vectors$", paste0("Per eye: the astigmatism vectors PE, SIA, PRE, POST and the Alpins TIA and DV - ",
                                      "magnitude, plus axis and doubled-angle x, y - input for external tools.")),
    c("^Astig_M_",            paste("Astigmatism per the JCRS 2025 standard for the matched cohort of one pairing (matched rows),", jcrs)),
    c("^GEE_Main_",           "Clustered GEE with the group as the only predictor; per-patient variables by an ordinary model on one row per patient."),
    c("^GEE_Adj_M_",          "Adjusted clustered GEE on the matched cohort (covariates listed per row and in the Manifest)."),
    c("^GEE_Adj_",            "Clustered GEE adjusted for the covariates listed per row and in the Manifest (a variable is never adjusted for itself)."),
    c("^GEE_M_",              "Clustered GEE on the matched cohort."),
    c("^Regression",          "Covariate-adjusted models, the crude estimate on the same eyes, and a full-model appendix."),
    c("^Reg_M_",              "Covariate-adjusted models on the matched cohort, with the crude estimate on the same eyes."),
    c("^PSM_",                "Matching (MatchIt): settings, covariates, balance and the matched rows."),
    c("^BasePreop_M_",        "Baseline and preop tables after matching."),
    c("^Out_M_",              "Outcome comparisons after matching."),
    c("^Pub_M_",              "Publication-style table for the matched cohort."),
    c("^Matched_Data$",       paste("Matched rows of every pairing, with MatchIt's own columns: distance = the propensity ",
                                    "score (on the scale in the Manifest), weights = the matching weight, subclass = the ",
                                    "matched set; 'Pairing' names the comparison.", sep = ""))
  )
  for (r in rules) if (grepl(r[1], nm)) return(r[2])
  ""
}

# Input sheet as the Manifest records it: the sheet's name, with its position
# when the setting is a number ("sheet 1 ('Data')"); blank = the first sheet.
# As readxl reads it: a number is a position, text is a sheet name.
manifest_sheet_text <- function(file_path, sheet) {
  s <- unlist(sheet)
  num <- is.numeric(s) && length(s) > 0 && is.finite(s[1])
  s <- if (is.null(s) || length(s) == 0 || is.na(s[1])) "" else trimws(as.character(s[1]))
  if (nzchar(s) && !num) return(s)                            # a sheet name
  idx <- if (num) as.integer(s) else 1L
  nms <- tryCatch(readxl::excel_sheets(file_path), error = function(e) character(0))
  nm <- if (idx >= 1 && idx <= length(nms)) nms[idx] else NA_character_
  paste0(if (nzchar(s)) paste("sheet", idx) else "first sheet",
         if (!is.na(nm)) paste0(" ('", nm, "')") else "")
}

# Distinct eyes of a stacked matched-only table: attr "eye_row" (the cohort
# row of each stacked row, set by build_workbook_for_data); without it, rows
# identical in every recorded column are taken to be the same eye.
.stacked_eye_key <- function(dat) {
  k <- attr(dat, "eye_row")
  if (!is.null(k) && length(k) == nrow(dat)) return(as.character(k))
  rec <- setdiff(names(dat), c("Pairing", "distance", "weights", "subclass"))
  rec <- rec[!startsWith(rec, ".")]
  do.call(paste, c(lapply(dat[rec], as.character), sep = "\r"))
}

# run: list(label =, values =) of the subset run (NULL = not recorded).
# match_weights: the matching weights of every matched pairing in this
# workbook; NULL takes dat$weights when present (the matched-only workbook).
# match_failed: named character, pairing -> why matching produced no cohort.
# match_methods: named character, pairing -> the method matchit() ran (it
# differs from cfg$psm_method only for cem on categorical covariates).
# A `dat` with a "Pairing" column is the matched-only workbook's stack of
# every pairing: a reference eye matched in two pairings is in it twice, so
# the counts are given per pairing and as distinct eyes, and the rows that
# describe the full-cohort workbook (mEYEstro tabs, figures) are left out.
# The filter rules in words, as the Manifest records them (finding T1-01): the
# column as typed, the operation and its values ("Age_Years >= 18;
# FollowUp_Days between 150 and 210; |Preop_Cylinder_D| <= 5"); "none".
filter_rules_text <- function(rules) {
  if (is.null(rules) || !length(rules)) return("none")
  one <- function(r) {
    if (is.null(r$var) || is.null(r$op)) return(NULL)
    v <- as.character(r$var)[1]; op <- tolower(trimws(as.character(r$op)[1]))
    val <- as.character(unlist(r$value))
    q <- function(x) paste0("'", x, "'")
    switch(op,
      between = sprintf("%s between %s and %s", v, val[1], val[2]),
      between_any = paste0(v, " in ", paste(vapply(if (is.list(r$value)) r$value else list(r$value), function(b)
        sprintf("[%s, %s]", as.character(unlist(b))[1], as.character(unlist(b))[2]), character(1)), collapse = " or ")),
      abs_ge = sprintf("|%s| >= %s", v, val[1]), abs_le = sprintf("|%s| <= %s", v, val[1]),
      abs_between = sprintf("|%s| between %s and %s", v, val[1], val[2]),
      `in` = sprintf("%s in {%s}", v, paste(val, collapse = ", ")),
      notin = sprintf("%s not in {%s}", v, paste(val, collapse = ", ")),
      contains = sprintf("%s contains %s", v, paste(q(val), collapse = " or ")),
      not_contains = sprintf("%s does not contain %s", v, paste(q(val), collapse = " or ")),
      isna = sprintf("%s is missing", v), notna = sprintf("%s is not missing", v),
      paste(v, op, paste(val, collapse = ", ")))
  }
  tx <- unlist(lapply(rules, one))
  if (!length(tx)) return("none")
  paste0(paste(tx, collapse = "; "),
         " (a row with a missing value is kept by every rule except 'is missing' / 'is not missing')")
}

# The analysed variables by section and type, the preferred level of each
# dichotomous variable and the per-patient variables, as the Manifest records
# them (finding T1-01); headers as typed.
variables_text <- function(cfg) {
  secs <- c(baseline = "Baseline", preop = "Preoperative", outcome = "Outcome", within_group = "Within-group")
  part <- unlist(lapply(names(secs), function(s) {
    tp <- cfg[[s]]
    if (is.null(tp)) return(NULL)
    kk <- unlist(lapply(c("continuous", "ordinal", "dichotomous", "categorical"), function(k) {
      v <- as.character(unlist(tp[[k]]))
      if (length(v)) paste0(k, ": ", paste(v, collapse = ", "))
    }))
    if (length(kk)) paste0(secs[[s]], " - ", paste(kk, collapse = "; "))
  }))
  pref <- cfg$dichotomous_preferred_level
  pref_txt <- if (length(pref)) paste0("preferred levels: ",
    paste(sprintf("%s = '%s'", names(pref), vapply(pref, function(x) as.character(unlist(x))[1], character(1))),
          collapse = ", "))
  pp <- as.character(unlist(cfg$patient_level_params_raw))
  ex <- as.character(unlist(cfg$exclude_params_raw))
  out <- c(part, pref_txt,
           if (length(pp)) paste0("per-patient variables: ", paste(pp, collapse = ", ")),
           if (length(ex)) paste0("excluded: ", paste(ex, collapse = ", ")))
  if (length(out)) paste(out, collapse = " | ") else "none"
}

# The settings exactly as run, as compact JSON for the Manifest (finding
# T1-01): the input file by its name only and the output folder left out (no
# local path, so never the Windows user name, D25); NA when it does not fit one
# cell.
# The settings file the engine was given is read again unsimplified, so its
# arrays stay arrays (a one-element list is not turned into a scalar); without
# one (a direct call) the configuration in memory is written.
settings_json_text <- function(cfg) {
  fp <- getOption("standardeyes.cfg_path")
  x <- if (is.character(fp) && length(fp) == 1 && file.exists(fp))
         tryCatch(jsonlite::fromJSON(fp, simplifyVector = FALSE), error = function(e) NULL)
  if (is.null(x)) x <- cfg
  if (!is.null(x$file_path)) x$file_path <- basename(as.character(unlist(x$file_path))[1])
  x$output_dir <- NULL
  # any other absolute path a setting holds is reduced to its last part
  scrub <- function(v) {
    if (is.list(v)) return(lapply(v, scrub))
    if (is.character(v)) {
      ab <- !is.na(v) & grepl("^([A-Za-z]:[\\/]|[\\/]{1,2}[^\\/])", v)
      v[ab] <- basename(v[ab])
    }
    v
  }
  x <- scrub(x)
  js <- tryCatch(as.character(jsonlite::toJSON(x, auto_unbox = TRUE, null = "null", na = "null", digits = NA)),
                 error = function(e) NA_character_)
  if (is.na(js) || nchar(js) > 32000) NA_character_ else js
}

add_contents_and_manifest <- function(wb, dat, cfg, res, logger = message,
                                      run = NULL, match_weights = NULL, match_failed = NULL,
                                      match_methods = NULL) {
  tryCatch({
    # ---- Manifest ----------------------------------------------------------
    sn <- add_or_replace_worksheet(wb, "Manifest")
    pkg_v <- function(p) tryCatch(as.character(utils::packageVersion(p)),
                                  error = function(e) "not installed")
    one <- function(x, default = "") {
      x <- unlist(x)
      if (is.null(x) || length(x) == 0 || is.na(x[1])) default else as.character(x[1])
    }
    md5 <- tryCatch(unname(tools::md5sum(cfg$file_path)), error = function(e) NA_character_)
    stacked <- "Pairing" %in% names(dat)
    eye_key <- if (stacked) .stacked_eye_key(dat) else NULL
    first_eye <- if (stacked) !duplicated(eye_key) else rep(TRUE, nrow(dat))
    gtab <- table(dat$GroupLabel[first_eye])
    gtab <- gtab[gtab > 0]
    pair_txt <- NULL
    if (stacked) {
      pl <- unique(as.character(dat$Pairing))
      pair_txt <- paste(vapply(pl, function(p) {
        tp <- table(dat$GroupLabel[as.character(dat$Pairing) == p]); tp <- tp[tp > 0]
        paste0(p, ": ", paste(sprintf("%s = %d", names(tp), as.integer(tp)), collapse = ", "))
      }, character(1)), collapse = "; ")
    }

    # the reference group actually used (first level), not just the requested one
    used_ref <- if (is.factor(dat$GroupLabel)) levels(droplevels(dat$GroupLabel))[1]
                else names(gtab)[1]
    req_ref <- one(cfg$reference_group)
    ref_txt <- if (nzchar(req_ref) && !identical(req_ref, used_ref))
      sprintf("%s (configured '%s' not present in this run; first group used)", used_ref, req_ref)
      else one(used_ref)

    run <- run %||% res$run
    run_txt <- if (is.null(run)) "not recorded"
      else if (is.null(run$values) || length(run$values) == 0) paste0(one(run$label, "All"), " (all rows)")
      else sprintf("%s: %s in {%s}", one(run$label), one(cfg$subset_col_raw),
                   paste(as.character(unlist(run$values)), collapse = ", "))

    # columns named by their headers as typed in the file (R4-3)
    dmap <- res$display_names
    dn <- function(x) disp_name(x, dmap)
    vi <- va_info(dat, cfg, res)
    logmar_txt <- if (vi$format == "logmar")
      paste0("not applied - acuity recorded in logMAR",
             if (length(vi$in_logmar)) paste0(" (analyzed as recorded: ", paste(dn(vi$in_logmar), collapse = ", "), ")") else "")
      else if (isTRUE(cfg$do_logmar))
        paste0("on", if (length(vi$converted)) paste0(": ", paste(dn(vi$converted), collapse = ", "))
                     else " (no column converted)")
      else "off"

    # matching requested but no covariate resolved: skipped (pipeline.R logs
    # the WARNING once; contract R4-5) - no settings, weights or matched tables
    matching_req <- isTRUE(cfg$do_matching)
    matching_on <- matching_req && length(res$psm_covariates_clean) > 0
    match_skip_txt <- if (matching_req && !matching_on)
      paste0("skipped - ", if (length(res$psm_covariates_unresolved)) "no matching covariate found in the file"
                           else "no matching covariate is set")
    unres <- unresolved_covariates(cfg, res)
    psm_txt <- NULL
    if (matching_on) {
      ps <- list(method = cfg$psm_method, distance = cfg$psm_distance, ratio = cfg$psm_ratio,
                 replace = cfg$psm_replace, caliper = cfg$psm_caliper,
                 std_caliper = cfg$psm_std_caliper, m_order = cfg$psm_m_order, seed = cfg$psm_seed)
      if (exists(".psm_settings", mode = "function"))
        ps <- tryCatch(.psm_settings(ps), error = function(e) ps)
      if (length(match_methods)) ps$method_used <- match_methods
      psm_txt <- psm_settings_text(ps)
    }
    # the method actually run, in words (R4-5)
    match_txt <- if (!is.null(match_skip_txt)) match_skip_txt
                 else if (matching_on) psm_method_label(ps) else "off"
    if (is.null(match_weights) && "weights" %in% names(dat)) match_weights <- dat$weights
    # the matched GEE / regression are named only when one of them runs
    m_stage <- one(cfg$run_gee_stage, "none")
    m_models <- (m_stage %in% c("matched", "both") && (isTRUE(cfg$run_main_gee) || isTRUE(cfg$run_adjusted_gee))) ||
                (isTRUE(cfg$do_regression) && one(cfg$regression_stage, "unmatched") %in% c("matched", "both"))
    # weights not all 1 used in the matched GEE / regression: those models use
    # an independence working correlation (R4-1)
    w_corstr <- if (!is.null(match_weights) && weights_nonunit(match_weights))
      " (working correlation: independence (weighted))" else ""
    w_txt <- if (!matching_on && is.null(match_weights)) NULL
             else if (is.null(match_weights)) "no matched cohort"
             else if (is.na(weights_summary(match_weights))) "no matching weights recorded"
             else paste0(weights_summary(match_weights),
                         if (!m_models) "; used in the matched tables (no matched GEE / regression run)"
                         else if (isTRUE(cfg$use_matching_weights_in_gee))
                           paste0("; used in the matched tables and in the matched GEE / regression", w_corstr)
                         else "; used in the matched tables only (matched GEE / regression unweighted)")
    fail_txt <- if (length(match_failed))
      paste(sprintf("%s: %s", names(match_failed), sub("\\.+$", "", as.character(match_failed))), collapse = "; ")
      else NULL

    # patient identifier: blank ids were given "(no ID) row <n>" tokens by the
    # pipeline (each such eye is its own patient); none mapped = row clusters
    pid <- res$patient_id_clean %||% NA_character_
    pid_txt <- if (is.na(pid[1]) || !(pid[1] %in% names(dat)))
      "none mapped - every eye is its own patient (own cluster) in GEE, figure P values and the bootstrap"
      else {
        k <- sum(grepl("^\\(no ID\\) row ", as.character(dat[[pid[1]]][first_eye])))
        paste0("'", dn(pid[1]), "'",
               if (k > 0) sprintf(" (%d %s without an ID, each counted as a separate patient)",
                                  k, if (stacked) "distinct eye(s)" else "row(s)") else "")
      }
    # the figures cluster only when a patient-ID column is actually in the data
    fig_clustered <- isTRUE(cfg$cluster_by_patient %||% TRUE) && !is.na(pid[1]) && pid[1] %in% names(dat)

    # robust astigmatism test constants (graphs.R), when it is the one used;
    # its bootstrap resamples patients only when the figures cluster
    astig_m <- one(cfg$astig_test_method, "robust")
    astig_txt <- if (identical(astig_m, "robust") && exists("ASTIG_BOOT_B") && exists("ASTIG_TRIM"))
      sprintf(paste0("robust - centroid against zero by the bootstrap projection-distance test (as in the WHWK ",
                     "functions; %d bootstrap centroids, %s, seed 20250602; ",
                     "untrimmed; P = .001 is the bootstrap floor 1/B); between cohorts %d%%-trimmed mean vectors ",
                     "with the pooled winsorized covariance, precision and mean magnitude by trimmed means ",
                     "(trimming and winsorizing by order statistics, g = floor(0.2 n), Wilcox), %s B = %d usable ",
                     "resamples, seed 20250601; a trimmed test whose statistic does not exist in the data or in ",
                     "more than 5%% of the resamples is run untrimmed - ordinary means and covariance, same ",
                     "bootstrap - and labeled so on the astigmatism sheet and the figure legends"),
              as.integer(get("ASTIG_BOOT_B")),
              if (fig_clustered) "patients resampled" else "eyes resampled (eyes independent)",
              as.integer(round(get("ASTIG_TRIM") * 100)),
              if (fig_clustered) "bootstrap resampling patients within each cohort"
              else "bootstrap resampling eyes within each cohort (eyes independent)",
              as.integer(get("ASTIG_BOOT_B")))
      else if (identical(astig_m, "classic")) "classic - one-sample Hotelling T² against zero, Hotelling T² / Pillai MANOVA and Welch tests between cohorts; eyes independent"
      else astig_m
    # the astigmatism convention and regions of v1.1.0 (D1; decisions R3-1, R3-2)
    astig_set <- if (exists("astig_settings", mode = "function")) astig_settings(cfg) else NULL
    astig_reg_txt <- if (is.null(astig_set)) NULL else paste0(
      "data: ", switch(astig_set$region, normal = "95% bivariate-normal ellipse",
                       both = paste0("95% ", ASTIG_REGION_LABEL, " and 95% bivariate-normal ellipse"),
                       paste0("95% ", ASTIG_REGION_LABEL, " (eyes of depth at least that of rank ",
                              "floor(0.05 n + 0.5); the convex hull of all eyes below 10 eyes)")),
      "; centroid: ", if (identical(astig_set$centroid_region, "hotelling")) "Hotelling T² ellipse"
                      else paste0(ASTIG_CENTROID_LABEL, " (999 bootstrap centroids, projection distance, ",
                                  "95% to 98% kept by the number of resampling units; seed 20250602)"),
      if (isTRUE(cfg$double_angle_figs %||% TRUE) && isTRUE(cfg$astig_whwk_files %||% TRUE))
        "; input files for the WHWK functions in the 'WHWK input' sub-folder of the run's figure folder (Graphs_...)" else "")
    psm_seed_txt <- if (matching_on) paste0("; matching ", one(ps$seed, as.character(cfg$psm_seed %||% "20250601")))
                    else ""

    gee_stage <- one(cfg$run_gee_stage, "none")
    gee_on <- gee_stage != "none" && (isTRUE(cfg$run_main_gee) || isTRUE(cfg$run_adjusted_gee))
    # the matched summary tables are named only when a matched cohort is built
    gee_tbls <- switch(gee_stage, unmatched = "unmatched", matched = "matched",
                       both = "unmatched and matched", gee_stage)
    if (!matching_on && gee_stage %in% c("matched", "both"))
      gee_tbls <- if (gee_stage == "both") "unmatched" else NA_character_
    gee_cols_txt <- if (!isTRUE(cfg$add_gee_columns) || !gee_on) "off"
      else if (is.na(gee_tbls)) "off (no matched cohort)"
      else paste0(if (identical(cfg$gee_source_for_tables, "adjusted") && isTRUE(cfg$run_adjusted_gee))
                    "adjusted" else if (isTRUE(cfg$run_main_gee)) "main" else "adjusted",
                  " GEE, in the ", gee_tbls, " summary tables")
    # covariates actually used, by their headers, and the ones not in the file
    gee_adj_txt <- if (isTRUE(cfg$run_adjusted_gee) && gee_stage != "none") {
      used <- as.character(unlist(res$gee_adjust_clean))
      paste0(if (length(used)) paste(dn(used), collapse = ", ") else "none",
             if (length(unres$`GEE adjustment`)) paste0("; ", .not_found_txt(unres$`GEE adjustment`)) else "")
    } else "off"

    # version and build from the one version source (D25, standardeyes_version:
    # the app's own app.py); the values the server passed only when that file
    # cannot be read
    ver <- standardeyes_version()
    t0 <- res$run_started %||% NULL
    # replaced by the whole run's duration once the figures are drawn
    # (pipeline.R, MANIFEST_DURATION_RE); kept, labelled, where that fails
    dur_txt <- if (inherits(t0, "POSIXt"))
      sprintf("%.1f (seconds from the start of the run to the writing of this workbook%s)",
              as.numeric(difftime(Sys.time(), t0, units = "secs")),
              if (!stacked && isTRUE(cfg$make_graphs %||% TRUE)) "; the figures are drawn after it" else "")
      else "not recorded"
    rows <- list(
      c("StandardEyes version", if (!is.na(ver$public)) ver$public else one(cfg$public_version, "not recorded")),
      c("Build",                if (!is.na(ver$build)) ver$build else one(cfg$app_version, "not recorded")),
      c("Run duration (s)",     dur_txt),
      c("R version",            R.version.string),
      c("MatchIt",              pkg_v("MatchIt")),
      c("cobalt",               pkg_v("cobalt")),
      c("geepack",              pkg_v("geepack")),
      c("ggplot2",              pkg_v("ggplot2")),
      c("openxlsx",             pkg_v("openxlsx")),
      c("Run time",             format(Sys.time(), "%Y-%m-%d %H:%M:%S")),
      c("Input file",           basename(one(cfg$file_path))),
      c("Sheet",                tryCatch(manifest_sheet_text(cfg$file_path, cfg$sheet_name),
                                         error = function(e) one(cfg$sheet_name, "first sheet"))),
      c("Input MD5 checksum",   ifelse(is.na(md5), "unavailable", md5)),
      c("Run / subset",         run_txt),
      if (stacked) c("Matched rows (stacked over pairings)",
                     sprintf("%d rows; %d distinct eyes (a reference eye matched in several pairings is listed once per pairing)",
                             nrow(dat), sum(first_eye)))
      else c("Rows analyzed",   as.character(nrow(dat))),
      c("Patient ID",           pid_txt),
      if (stacked) c("Pairings (matched rows per group)", pair_txt),
      c(if (stacked) "Groups (distinct eyes)" else "Groups",
        paste(sprintf("%s = %d", names(gtab), as.integer(gtab)), collapse = "; ")),
      c("Reference group",      ref_txt),
      c("Filter rules",         filter_rules_text(cfg$filter_rules)),
      c("Variables",            variables_text(cfg)),
      c("VA notation",          vi$label),
      c("logMAR conversion",    paste0(logmar_txt, if (length(vi$in_logmar))
        "; acuity cells and figure headers read the Snellen geometric mean (logMAR mean ± SD), the table range in Snellen"
        else "")),
      c("Matching",             match_txt),
      if (!is.null(psm_txt)) c("Matching settings", psm_txt),
      # the covariates of every model step that runs, always (T1-01), with any
      # not found in the file
      if (matching_on)
        c("Matching covariates", paste0(paste(dn(as.character(unlist(res$psm_covariates_clean))), collapse = ", "),
                                        if (length(unres$matching)) paste0("; ", .not_found_txt(unres$matching)) else "")),
      if (!is.null(w_txt))   c("Matching weights", w_txt),
      if (!is.null(fail_txt)) c("Pairings not matched", fail_txt),
      # the v1.1.0 statistical settings that change results (D11-D18), with
      # the value this run used
      c("GEE small-sample inference",
        paste0(gee_small_sample(), " - ",
               gee_inference_text(unit = if (!is.na(pid[1]) && pid[1] %in% names(dat)) "patient" else "eye"))),
      c("GEE working correlation",
        paste0("exchangeable, kept only if the fit converged, its working correlation is below ",
               get0("GEE_EXCH_ALPHA_MAX", ifnotfound = 0.95), " and one further scoring step moves no coefficient ",
               "by more than ", get0("GEE_EXCH_STEP_MAX", ifnotfound = 0.01), " SE - otherwise refitted with an ",
               "independence working correlation and labeled so (sheets, tables, figures, run log); independence on ",
               "matched cohorts; variables marked per patient are modeled on one row per patient by an ordinary ",
               "linear or logistic model, never by an eye-level GEE")),
      c("Table tests",          table_tests_text(cfg, has_pid = !is.na(pid[1]) && pid[1] %in% names(dat))),
      c("Multiplicity",         paste0("Holm within each variable across the comparisons of each group with ",
                                       "the reference (summary-table GEE and adjusted columns, tagged [Holm]; ",
                                       "regression 'P (Holm)' and 'Crude P (Holm), same eyes'; figure 'vs ",
                                       "reference' lines, from the same GEE of all groups as the GEE column ",
                                       "when the figure's test is a GEE); omnibus P ",
                                       "values and single comparisons unadjusted")),
      if (matching_on) c("Matched analyses", matched_analysis_note()),
      c("GEE stage",            gee_stage),
      c("GEE adjustment covariates", gee_adj_txt),
      c("GEE columns in summary tables", gee_cols_txt),
      c("Regression",           ifelse(isTRUE(cfg$do_regression), "on", "off")),
      if (isTRUE(cfg$do_regression))
        c("Crude vs adjusted",  "the crude GEE (group alone) is refitted on the adjusted model's complete-case eyes; the outcome tables give the adjusted model's number of eyes"),
      if (isTRUE(cfg$do_regression))
        c("Regression covariates", paste0(if (length(res$regression_covs_clean))
                                            paste(dn(as.character(unlist(res$regression_covs_clean))), collapse = ", ")
                                          else "none",
                                          if (length(unres$regression)) paste0("; ", .not_found_txt(unres$regression)) else "")),
      c("Minus-cyl standardization", ifelse(isTRUE(cfg$do_standardize_minus_cyl), "on", "off")),
      c("Astigmatism vectors",  paste0("plus-cylinder format (JCRS 2025), converted by the sign of each cylinder ",
                                       "whatever the minus-cylinder switch; with-the-rule on the left of the ",
                                       "double-angle plots; SIA = postoperative - preoperative, PE = postoperative - ",
                                       "target; total SD with the N denominator")),
      c("Astigmatism test",     astig_txt),
      if (!is.null(astig_reg_txt)) c("Astigmatism regions", astig_reg_txt),
      if (isTRUE(cfg$make_meyestro) && !stacked)
        c("mEYEstro intended refraction", meyestro_intended_source(res$roles %||% list(),
                                                                   res$astig_target %||% list(), dmap)),
      if (isTRUE(cfg$make_meyestro) && !stacked)
        c("mEYEstro refraction notation", "minus cylinder (plus-cylinder rows transposed in the tab only)"),
      c("Random seeds",         paste0("bootstrap and Monte-Carlo exact tests 20250601; astigmatism centroid ",
                                       "confidence regions and centroid-against-zero tests 20250602", psm_seed_txt,
                                       " (fixed, set locally; identical settings give identical results)")),
      # the v1.1.0 reporting settings (D15, D20, D21), with the value this run used
      c("P value style",        p_style_text()),
      if (!stacked && exists("gr_figure_settings", mode = "function"))
        c("Figures",            gr_figure_settings_text(gr_figure_settings(cfg))),
      if (!stacked)
        c("Figure p-values",    if (!isTRUE(cfg$cluster_by_patient %||% TRUE)) "eyes treated as independent"
                                else if (!fig_clustered)
                                  "eyes treated as independent (clustering requested, but no patient ID column is mapped)"
                                else paste0("clustered by patient (GEE, ",
                                            if (identical(gee_small_sample(), "none")) "uncorrected robust SE, normal reference"
                                            else "robust SE (Mancl-DeRouen), t reference; F for joint tests",
                                            ") wherever a patient has two analyzed eyes; ",
                                            "classical tests for one eye per patient")),
      # every setting exactly as run (the input file by its name, no output folder)
      c("Settings (JSON)", { js <- settings_json_text(cfg)
                             if (is.na(js)) "not recorded (too long for one cell)" else js })
    )
    rows <- Filter(Negate(is.null), rows)
    mf <- data.frame(Setting = vapply(rows, `[`, character(1), 1),
                     Value   = vapply(rows, `[`, character(1), 2),
                     stringsAsFactors = FALSE)
    openxlsx::writeData(wb, sn, mf)
    openxlsx::setColWidths(wb, sn, cols = 1:2, widths = c(30, 80))

    # ---- Contents ----------------------------------------------------------
    # rows in the final tab order (Contents, Manifest, then creation order);
    # "Full name" is the untruncated title of a sheet whose tab name Excel's
    # 31-character limit shortened or de-duplicated
    sn <- add_or_replace_worksheet(wb, "Contents")
    sheets <- setdiff(names(wb), "Contents")
    sheets <- c(intersect("Manifest", sheets), setdiff(sheets, "Manifest"))
    full <- if (exists("sheet_full_name", mode = "function")) sheet_full_name(sheets) else sheets
    df <- data.frame(Sheet = sheets,
                     `Full name` = full,
                     Description = vapply(full, describe_sheet, character(1), USE.NAMES = FALSE),
                     check.names = FALSE, stringsAsFactors = FALSE)
    openxlsx::writeData(wb, sn, df)
    for (i in seq_along(sheets)) {
      # quote rules: ' doubles inside the sheet reference, " inside the strings
      ref <- gsub('"', '""', gsub("'", "''", sheets[i], fixed = TRUE), fixed = TRUE)
      txt <- gsub('"', '""', sheets[i], fixed = TRUE)
      f <- sprintf('HYPERLINK("#\'%s\'!A1", "%s")', ref, txt)
      openxlsx::writeFormula(wb, sn, x = f, startCol = 1, startRow = i + 1)
    }
    openxlsx::setColWidths(wb, sn, cols = 1:3, widths = c(34, 44, 90))

    # Contents first, Manifest second, everything else in original order
    ord <- names(wb)
    want <- c("Contents", "Manifest", setdiff(ord, c("Contents", "Manifest")))
    openxlsx::worksheetOrder(wb) <- match(want, ord)
    invisible(TRUE)
  }, error = function(e) {
    logger(paste0("Contents/Manifest sheets skipped: ", conditionMessage(e)))
    invisible(FALSE)
  })
}
