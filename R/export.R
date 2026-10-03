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
# A blank target cell is read as the figures read it: a blank SEQ target is
# plano, a blank target cylinder is no cylinder, a blank target axis is 0
# (graphs.R does the same); the number of such eyes is logged once per tab.
meyestro_intended <- function(dfg, seq_col, target = list(), logger = NULL, what = "") {
  n <- nrow(dfg)
  has <- function(cc) !is.null(cc) && length(cc) == 1 && !is.na(cc) && cc %in% names(dfg)
  from_seq <- has(seq_col)
  from_ast <- length(target) == 2 && has(target$cyl) && has(target$axis)
  s  <- if (from_seq) as_numeric_safe(dfg[[seq_col]]) else rep(0, n)
  cy <- if (from_ast) as_numeric_safe(dfg[[target$cyl]]) else rep(0, n)
  ax <- if (from_ast) as_numeric_safe(dfg[[target$axis]]) else rep(0, n)
  blank <- !is.finite(s) | !is.finite(cy) | !is.finite(ax)
  if (any(blank) && !is.null(logger))
    log_once(logger, sprintf("mEYEstro tab%s: %d eye(s) with a blank target cell written as plano (blank SEQ target = 0, blank target cylinder = 0, blank target axis = 0), as the figures read them.",
                             if (nzchar(what)) paste0(" '", what, "'") else "", sum(blank)))
  s[!is.finite(s)] <- 0
  cy[!is.finite(cy)] <- 0
  ax[!is.finite(ax)] <- 0
  sp <- s - cy / 2
  pos <- is.finite(cy) & cy > 0
  sp[pos] <- sp[pos] + cy[pos]
  cy[pos] <- -cy[pos]
  ax[pos] <- wrap_axis_0_180(ax[pos] + 90)
  # no intended cylinder: the axis is meaningless and written as 0 (R4-9)
  ax[cy == 0] <- 0
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
# (negative cylinder accepted). The group is in the sheet name, so columns
# A-E load as they are. Eye codes R/RE/Right and L/LE/Left become OD/OS; the
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
      logger(sprintf("AstigMATIC tab '%s': %d eye code(s) not recognised as right/left (e.g. '%s') - left blank.",
                     g, sum(bad), paste(utils::head(unique(as.character(eye_raw[bad])), 3), collapse = "', '")))
    out <- data.frame(
      Eye = eye,
      `preoperative cylinder` = pick("pre_cyl"),
      `preoperative axis` = pick("pre_axis"),
      `postoperative cylinder` = pick("post_cyl"),
      `postoperative axis` = pick("post_axis"),
      check.names = FALSE, stringsAsFactors = FALSE)
    sh <- add_or_replace_worksheet(wb, if (is.null(pairing)) paste0("AstigMATIC_", g)
                                       else paste0("AstigMATIC_M_", g, "_", pairing))
    openxlsx::writeDataTable(wb, sh, out)
  }
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
write_stacked_tables <- function(wb, sheet_name, sections, note = NULL) {
  sn <- add_or_replace_worksheet(wb, sheet_name)
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
    openxlsx::writeData(wb, sn, as.data.frame(d, check.names = FALSE), startRow = row, startCol = 1)
    .wrap_multiline_cells(wb, sn, d, header_row = row)
    row <- row + nrow(as.data.frame(d)) + 2
  }
  invisible(sn)
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
# - is stated in the note (contract R4-1). NULL / "exchangeable" adds nothing.
gee_skip_note <- function(g, corstr = NULL) {
  wtxt <- if (length(corstr) && grepl("independence", corstr[1], fixed = TRUE))
    paste0("Models weighted by the matching weights - working correlation: ", corstr[1],
           ", robust (sandwich) standard errors.") else NULL
  if (is.null(g) || nrow(g) == 0) return("NOTE: no GEE models were fitted.")
  if (!("Model" %in% names(g))) return(if (length(wtxt)) paste("NOTE:", wtxt) else NULL)
  by_design <- !is.na(g$Model) & grepl("outcome not modeled in GEE", g$Model, fixed = TRUE)
  hit <- !is.na(g$Model) & grepl("Skipped|failed", g$Model) & !by_design
  bad <- if ("Outcome" %in% names(g)) length(unique(g$Outcome[hit])) else sum(hit)
  cau <- if ("Note" %in% names(g)) as.character(g$Note) else character(0)
  cau <- cau[!is.na(cau) & startsWith(cau, "Caution")]
  few <- length(cau) > 0
  if (bad == 0 && !few && !length(wtxt)) return(NULL)
  # without a patient ID each eye is its own cluster and the cautions count eyes
  unit <- if (few && all(grepl("with few eyes$", cau))) "eyes" else "patients"
  paste0("NOTE: ", paste(c(
    wtxt,
    if (bad > 0) paste0(bad, " outcome(s) could not be modeled by GEE (too few eyes or patients, ",
                        "a group or outcome without usable data, too few events, or model failure) - ",
                        "see the 'Model' column for the reason."),
    if (few) paste0(if (bad > 0 || length(wtxt)) "Comparisons" else "comparisons", " with a group of fewer than ",
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
describe_within_group <- function(df_g, params_clean, type_map, dicho_pref_clean = list(),
                                  levels_map = list(), patient_id_clean = NA_character_,
                                  display_names = NULL) {
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
      summ <- if (length(x) == 0) "no data" else paste0(
        round(mean(x), 2), " ± ", round(sd(x), 2),
        "  (median ", round(median(x), 2), " [", round(quantile(x, .25), 2), "-",
        round(quantile(x, .75), 2), "], range ", round(min(x), 2), "-", round(max(x), 2), ")")
      rows[[length(rows)+1]] <- tibble::tibble(Parameter = dn(pn), n = length(x), Summary = summ, .unit = unit)
    } else if (vt == "ordinal") {
      x <- as_numeric_safe(v); x <- x[is.finite(x)]
      summ <- if (length(x) == 0) "no data" else paste0(
        round(median(x), 2), " [", round(quantile(x, .25), 2), "-", round(quantile(x, .75), 2), "]")
      rows[[length(rows)+1]] <- tibble::tibble(Parameter = dn(pn), n = length(x), Summary = summ, .unit = unit)
    } else if (vt == "dichotomous") {
      nn <- sum(!is.na(v))
      if (nn == 0) {
        # all missing: no level to name and no percentage (contract R4-9)
        rows[[length(rows)+1]] <- tibble::tibble(Parameter = dn(pn), n = 0, Summary = "no data", .unit = unit)
      } else {
        lev <- pick_display_level(pn, v, dicho_pref_clean, display_names = display_names)
        c_ <- sum(as.character(v) == as.character(lev), na.rm = TRUE)
        p_ <- round(100 * c_ / nn, 1)
        rows[[length(rows)+1]] <- tibble::tibble(
          Parameter = paste0(dn(pn), " (", lev, ")"), n = nn, Summary = paste0(c_, " (", p_, "%)"),
          .unit = unit)
      }
    } else if (vt == "categorical") {
      vv <- as.character(v); levs <- sort(unique(na.omit(vv)))
      nn <- sum(!is.na(v))
      if (length(levs) == 0) {
        rows[[length(rows)+1]] <- tibble::tibble(Parameter = dn(pn), n = 0, Summary = "no data", .unit = unit)
      } else for (lev in levs) {
        c_ <- sum(vv == lev, na.rm = TRUE)
        p_ <- if (nn > 0) round(100 * c_ / nn, 1) else NA
        rows[[length(rows)+1]] <- tibble::tibble(
          Parameter = paste0(dn(pn), ": ", lev), n = nn, Summary = paste0(c_, " (", p_, "%)"),
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
# note: optional bold line above the table (e.g. matching weights).
# ------------------------------------------------------------
make_publication_table <- function(wb, sheet_name, sections, note = NULL) {
  drop_cols <- c("Test", "Practical Interpretation")
  secs <- list()
  ne_why <- character(0); ne_row <- character(0)
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
        ne_row <- c(ne_row, if (!is.null(rv)) gsub("_", " ", rv[fill])
                            else if ("Parameter" %in% names(d)) gsub("_", " ", d$Parameter[fill])
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
  out$Parameter <- gsub("_", " ", out$Parameter)
  names(out)[names(out) == "p_value"] <- "p value"

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
  if (length(ne_why)) {
    # one line per distinct reason, in order of first appearance
    reasons <- unique(ne_why)
    foot <- c("NE = not estimable (no P value):",
              vapply(reasons, function(w) {
                rr <- unique(ne_row[ne_why == w])
                paste0("- ", w, ": ", paste(rr, collapse = "; "), ".")
              }, character(1), USE.NAMES = FALSE))
    openxlsx::writeData(wb, sn, foot, startRow = top + nrow(out) + 2, startCol = 1)
  }
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
    mahalanobis        = "nearest-neighbour on Mahalanobis distance",
    robust_mahalanobis = "nearest-neighbour on robust Mahalanobis distance",
    euclidean          = "nearest-neighbour on Euclidean distance",
    scaled_euclidean   = "nearest-neighbour on scaled Euclidean distance",
    "nearest-neighbour on the propensity score")
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

psm_settings_table <- function(psm, reference_group, patient_id_raw = NULL, pairing = NULL) {
  # NULL-safe: c() silently drops NULLs (e.g. an empty caliper box = no
  # caliper), which would misalign the two columns
  fmt_setting <- function(x) if (is.null(x) || length(x) == 0 || is.na(x[[1]])) "(none)" else as.character(x)[1]
  nu <- .psm_not_used(psm)
  requested <- tolower(trimws(fmt_setting(psm$method %||% "nearest")))
  method <- .psm_method_run(psm, pairing)
  rows <- list(c("Matching method", method))
  if (!identical(method, requested))
    rows[[length(rows) + 1]] <- c("Requested method", paste0(requested, " (", PSM_CEM_EXACT_WHY, ")"))
  if (!("distance" %in% nu)) rows[[length(rows) + 1]] <- c("Distance", fmt_setting(psm$distance))
  if (!("ratio" %in% nu))    rows[[length(rows) + 1]] <- c("Ratio", fmt_setting(psm$ratio))
  if (!("replace" %in% nu))  rows[[length(rows) + 1]] <- c("Replacement", fmt_setting(psm$replace))
  if (!("caliper" %in% nu)) {
    rows[[length(rows) + 1]] <- c("Caliper", fmt_setting(psm$caliper))
    if (!is.null(psm$caliper) && !("std_caliper" %in% nu))
      rows[[length(rows) + 1]] <- c("Standardized caliper", fmt_setting(psm$std_caliper))
  }
  if (!("m_order" %in% nu))  rows[[length(rows) + 1]] <- c("Matching order", fmt_setting(psm$m_order))
  if (length(nu))
    rows[[length(rows) + 1]] <- c(paste0("Not used by method '", method, "'"),
                                  paste(.psm_setting_labels[intersect(names(.psm_setting_labels), nu)],
                                        collapse = ", "))
  rows <- c(rows, list(
    c("Random seed (matching)", fmt_setting(psm$seed)),
    c("Reference group", fmt_setting(reference_group)),
    c("Patient ID (clustering for GEE only; not used in matching)", fmt_setting(patient_id_raw))))
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
  parts <- c(method_txt,
    if (!("distance" %in% nu)) paste("distance", one(psm$distance, "glm")),
    if (!("ratio" %in% nu))    paste("ratio", one(psm$ratio, "1")),
    if (!("replace" %in% nu))  paste("replacement", if (isTRUE(as.logical(one(psm$replace, "FALSE")))) "on" else "off"),
    if (!("caliper" %in% nu))  paste0("caliper ", cal,
                                      if (cal != "none" && isTRUE(as.logical(one(psm$std_caliper, "TRUE"))))
                                        " (SD of the propensity score)" else ""),
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
  wb <- openxlsx::createWorkbook()
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
      paste0("Visual acuity analysed in logMAR (lower = better): ", paste(dn(vi$in_logmar), collapse = ", "),
             if (vi$format == "logmar") " - as recorded." else " - converted from the recorded notation."),
    if (length(vi$as_decimal))
      paste0("Visual acuity analysed as decimal acuity (higher = better): ",
             paste(dn(vi$as_decimal), collapse = ", "),
             if (vi$format %in% c("snellen", "etdrs")) paste0(" - converted from ", vi$label, ".") else "."))
  write_stacked_tables(wb, "Overview", list(
    `Notes` = if (length(va_notes)) data.frame(Note = va_notes, stringsAsFactors = FALSE) else NULL,
    `Initial Summary` = init$initial,
    `Sex (per patient)` = init$gender,
    `Groups` = init$groups,
    `QC - group assignment by source value` = qc))

  summarise_block <- function(params, tmap) create_summary_table(
    dat, params, tmap, glevels, "GroupLabel",
    res$patient_id_clean, res$patient_level_params_clean, res$dicho_pref_clean,
    logger = logger, display_names = dmap)

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
      use_weights = FALSE, logger = logger, display_names = dmap)
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
                                   use_weights = FALSE, logger = logger, display_names = dmap)
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
                                   res$patient_id_clean, display_names = dmap)
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

  # --- Unmatched GEE sheets (models fitted above) ---
  gee_sheet <- function(g, name)
    write_stacked_tables(wb, name, list(`GEE results` = g), note = gee_skip_note(g))
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
  reg_sheet <- function(rr, sheet, cohort_label) {
    note <- if (rr$n_flagged > 0) paste0(
      "NOTE: ", rr$n_flagged, " model(s) carry a small-sample caution or failed - ",
      "see the Note column. Interpret flagged estimates cautiously.") else NULL
    write_stacked_tables(wb, sheet, stats::setNames(list(rr$pub, rr$full),
      c(paste0("Adjusted regression - ", cohort_label, " (covariates: ", reg_covs_txt, ")"),
        "Full models (every term)")), note = note)
  }
  reg_write <- function(data, sheet, cohort_label, use_w = FALSE) {
    rr <- run_regression_models(data, res$outcome_params_clean, res$outcome_type_map,
                                res$regression_covs_clean, res$patient_id_clean,
                                res$dicho_pref_clean, levels(data$GroupLabel),
                                use_weights = use_w, logger = logger, display_names = dmap)
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
    psm_covs_df <- data.frame(PSM_Covariates = dn(as.character(unlist(res$psm_covariates_clean))),
                              stringsAsFactors = FALSE)
    psm_unres_df <- if (length(unres$matching))
      data.frame(PSM_Covariates = unres$matching, stringsAsFactors = FALSE) else NULL
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
        if (!is.null(bal_tab)) {
          bal_df <- as.data.frame(bal_tab); bal_df$Variable <- bal_shown
          bal_df <- bal_df %>% dplyr::relocate(Variable)
        }
        lp <- loveplot_path(loveplot_dir, lab, run)
        lp_args <- list(pr$bal, threshold = 0.1, stars = "raw")
        renamed <- bal_shown != bal_vars
        if (any(renamed)) lp_args$var.names <- stats::setNames(bal_shown[renamed], bal_vars[renamed])
        dev_lp <- NA_integer_
        lp_ok <- FALSE
        lp_why <- ""
        # the device's own "unable to open file" warnings are folded into the
        # one log line (with the path-length hint the figures give), not
        # printed raw at the end of the run
        lp_fail <- function(why) {
          if (exists("gr_write_failed", mode = "function"))
            gr_write_failed(lp, why, logger, what = paste0("Love plot for ", lab), count = FALSE)
          else logger(paste0("Love plot for ", lab, " not written: ", why))
        }
        tryCatch(withCallingHandlers({
          grDevices::png(lp, width = 1800, height = 1200, res = 200)
          dev_lp <- grDevices::dev.cur()
          print(do.call(cobalt::love.plot, lp_args))
          lp_ok <- TRUE
        }, warning = function(w) {
          if (grepl("unable to open|opening device|cannot open", conditionMessage(w))) {
            if (!nzchar(lp_why)) lp_why <<- conditionMessage(w)
            invokeRestart("muffleWarning")
          }
        }), error = function(e) lp_fail(if (nzchar(lp_why)) lp_why else conditionMessage(e)),
        finally = if (!is.na(dev_lp) && dev_lp %in% grDevices::dev.list()) grDevices::dev.off(dev_lp))
        if (lp_ok && file.exists(lp) && isTRUE(file.info(lp)$size > 0)) loveplots[[lab]] <- lp
        else if (lp_ok) lp_fail(if (nzchar(lp_why)) lp_why else "the file was not created")
        else if (file.exists(lp)) unlink(lp)   # a half-drawn file is not left behind
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
      # the patient-ID column under its header
      j <- which(names(ps_dist) == pid_key[1])
      if (length(j)) names(ps_dist)[j] <- dn(pid_key[1])
      ps_sections <- list(ps_dist)
      names(ps_sections) <- if (has_ps) "Propensity-score distribution (matched rows)"
                            else "Matching weights (matched rows; no propensity score with this method)"

      # One merged PSM sheet per pairing: settings (with the method this pairing
      # actually ran) + covariates + balance + PS dist
      write_stacked_tables(wb, paste0("PSM_", lab), c(list(
        `PSM settings` = psm_settings_table(psm, reference_group, pid_shown, pairing = lab),
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
        "mEYEstro / AstigMATIC tabs list the matched rows unweighted.") else NULL
      sub_glevels <- pair_lvls
      msum <- function(params, tmap) create_summary_table(
        md, params, tmap, sub_glevels, "GroupLabel",
        res$patient_id_clean, res$patient_level_params_clean, res$dicho_pref_clean,
        weights = w_md, logger = logger, display_names = dmap)
      b_m <- msum(res$baseline_params_clean, res$baseline_type_map)
      p_m <- msum(res$preop_params_clean,    res$preop_type_map)
      o_m <- msum(res$outcome_params_clean,  res$outcome_type_map)

      # Matched GEE
      use_w <- isTRUE(cfg$use_matching_weights_in_gee)
      gee_pair <- function(params, tmap, preds) run_gee_models(
        md, params, tmap, preds, res$patient_id_clean, res$dicho_pref_clean,
        use_weights = use_w, logger = logger, display_names = dmap)
      # weights not all 1 + the weights switch: independence working
      # correlation (R4-1), stated in the GEE sheets' note
      m_corstr <- if (exists("gee_corstr_label", mode = "function")) gee_corstr_label(md, use_w) else NULL

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
            note = gee_skip_note(gg, m_corstr))
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
            note = gee_skip_note(gga, m_corstr))
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
                          use_w = isTRUE(cfg$use_matching_weights_in_gee))
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

      md$Pairing <- lab
      matched_only_list[[lab]] <- md
    }
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
                "red N-S postop refraction, vertex, postop CDVA, postop UDVA. Grey A-D (group, decimal ",
                "acuities) and blue T (postop SEQ) are for reference only.")
  ast <- paste0("columns A-E (eye OD/OS, preop cylinder, preop axis, postop cylinder, postop axis) ",
                "load directly into AstigMATIC; minus cylinder accepted.")
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
    c("^GEE_Main_",           "Clustered GEE, group as the only predictor."),
    c("^GEE_Adj_M_",          "Adjusted clustered GEE on the matched cohort (covariates listed per row and in the Manifest)."),
    c("^GEE_Adj_",            "Clustered GEE adjusted for the covariates listed per row and in the Manifest (a variable is never adjusted for itself)."),
    c("^GEE_M_",              "Clustered GEE on the matched cohort."),
    c("^Regression",          "Covariate-adjusted models, with full-model appendix."),
    c("^Reg_M_",              "Covariate-adjusted models on the matched cohort."),
    c("^PSM_",                "Matching (MatchIt): settings, covariates, balance and the matched rows."),
    c("^BasePreop_M_",        "Baseline and preop tables after matching."),
    c("^Out_M_",              "Outcome comparisons after matching."),
    c("^Pub_M_",              "Publication-style table for the matched cohort."),
    c("^Matched_Data$",       paste("Matched rows of every pairing (MatchIt distance, weights, subclass; ",
                                    "'Pairing' names the comparison).", sep = ""))
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
             if (length(vi$in_logmar)) paste0(" (analysed as recorded: ", paste(dn(vi$in_logmar), collapse = ", "), ")") else "")
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
      sprintf("robust (%d%%-trimmed mean vectors, pooled winsorized covariance; %s B = %d, seed 20250601)",
              as.integer(round(get("ASTIG_TRIM") * 100)),
              if (fig_clustered) "cluster bootstrap over patients" else "bootstrap over eyes (eyes independent)",
              as.integer(get("ASTIG_BOOT_B")))
      else astig_m
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

    rows <- list(
      c("StandardEyes version", one(cfg$public_version, "not recorded (run from the command line)")),
      c("Build",                one(cfg$app_version, "not recorded (run from the command line)")),
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
      c("Filter rules",         as.character(length(cfg$filter_rules %||% list()))),
      c("VA notation",          vi$label),
      c("LogMAR conversion",    logmar_txt),
      c("Matching",             match_txt),
      if (!is.null(psm_txt)) c("Matching settings", psm_txt),
      if (matching_on && length(unres$matching))
        c("Matching covariates", paste0(paste(dn(as.character(unlist(res$psm_covariates_clean))), collapse = ", "),
                                        "; ", .not_found_txt(unres$matching))),
      if (!is.null(w_txt))   c("Matching weights", w_txt),
      if (!is.null(fail_txt)) c("Pairings not matched", fail_txt),
      c("GEE stage",            gee_stage),
      c("GEE adjustment covariates", gee_adj_txt),
      c("GEE columns in summary tables", gee_cols_txt),
      c("Regression",           ifelse(isTRUE(cfg$do_regression), "on", "off")),
      if (isTRUE(cfg$do_regression) && length(unres$regression))
        c("Regression covariates", paste0(if (length(res$regression_covs_clean))
                                            paste(dn(as.character(unlist(res$regression_covs_clean))), collapse = ", ")
                                          else "none", "; ", .not_found_txt(unres$regression))),
      c("Minus-cyl standardization", ifelse(isTRUE(cfg$do_standardize_minus_cyl), "on", "off")),
      c("Astigmatism test",     astig_txt),
      if (isTRUE(cfg$make_meyestro) && !stacked)
        c("mEYEstro intended refraction", meyestro_intended_source(res$roles %||% list(),
                                                                   res$astig_target %||% list(), dmap)),
      if (isTRUE(cfg$make_meyestro) && !stacked)
        c("mEYEstro refraction notation", "minus cylinder (plus-cylinder rows transposed in the tab only)"),
      c("Random seeds",         paste0("bootstrap and Monte-Carlo exact tests 20250601", psm_seed_txt,
                                       " (fixed, set locally; identical settings give identical results)")),
      if (!stacked)
        c("Figure p-values",    if (!isTRUE(cfg$cluster_by_patient %||% TRUE)) "eyes treated as independent"
                                else if (!fig_clustered)
                                  "eyes treated as independent (clustering requested, but no patient ID column is mapped)"
                                else "clustered by patient (GEE)")
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
