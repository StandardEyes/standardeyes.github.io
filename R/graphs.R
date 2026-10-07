# ============================================================
# graphs.R
# StandardEyes publication figure set: ALL groups superimposed
# on each panel, user colors, stat+P annotations, %-labels,
# indicator lines (blue identity, green +/-0.50 D, magenta
# +/-1.00 D), per-group regression lines + equations on the scatters,
# JCRS-standard double-angle astigmatism plots, grey semicircle
# vector plots, and a sectioned composite figure with captions
# below each panel. Bins follow the published standard-graph
# conventions (Reinstein/JRS).
# ============================================================

suppressPackageStartupMessages({ requireNamespace("ggplot2", quietly = TRUE) })

# positions 1-6 are frozen: changing them would alter every figure ever produced.
# 7-8 exist so 7+ groups no longer silently reuse colour 1 and 2.
DEFAULT_GROUP_COLS <- c("#5B2C86", "#F26522", "#1B9E4B", "#2166AC", "#00838F", "#C2185B",
                        "#8B5A2B", "#3A3A3A")
IND_IDENT <- "#1F4FD8"; IND_HALF <- "#1E8A2E"; IND_ONE <- "#E31FC4"   # blue / green / magenta

gr_num <- function(v) as_numeric_safe(v)
gr_logmar <- function(dec) { d <- gr_num(dec); d[!is.finite(d) | d <= 0] <- NA; -log10(d) }

lighten_col <- function(col, f = 0.55) {
  m <- grDevices::col2rgb(col)
  grDevices::rgb(m[1] + (255 - m[1]) * f, m[2] + (255 - m[2]) * f, m[3] + (255 - m[3]) * f,
                 maxColorValue = 255)
}

group_palette <- function(glv, cfg) {
  gc <- cfg$group_colors %||% list()
  vapply(seq_along(glv), function(i) {
    v <- gc[[glv[i]]]
    if (is.null(v) || !nzchar(v)) DEFAULT_GROUP_COLS[(i - 1) %% length(DEFAULT_GROUP_COLS) + 1]
    else as.character(v)
  }, character(1)) |> stats::setNames(glv)
}

# rendering options (reset at entry to generate_standard_graphs)
.gr_opts <- new.env(parent = emptyenv())
.gr_opts$label_angle <- 90; .gr_opts$box_left <- NULL; .gr_opts$box_right <- NULL

gr_theme <- function() {
  # MATLAB-like: full black panel box, inward ticks, bold text, light y-grid
  th <- ggplot2::theme_bw(base_size = gr_pt(12.5)) +
    ggplot2::theme(
      plot.title = ggplot2::element_text(face = "bold", hjust = .5, size = gr_pt(13)),
      axis.title = ggplot2::element_text(face = "bold", size = gr_pt(11)),
      axis.text = ggplot2::element_text(face = "bold", colour = "black", size = gr_pt(9.5)),
      axis.ticks = ggplot2::element_line(colour = "black", linewidth = .5),
      panel.border = ggplot2::element_rect(colour = "black", linewidth = .8, fill = NA),
      panel.grid.major.x = ggplot2::element_blank(),
      panel.grid.minor = ggplot2::element_blank(),
      panel.grid.major.y = ggplot2::element_line(colour = "grey88", linewidth = .3),
      legend.position = "top", legend.title = ggplot2::element_blank(),
      legend.text = ggplot2::element_text(size = gr_pt(9.5), face = "bold"),
      legend.background = ggplot2::element_rect(colour = "grey20", linewidth = .4),
      plot.background = ggplot2::element_rect(fill = "white", colour = NA),
      plot.margin = ggplot2::margin(gr_sp(6), gr_sp(10), gr_sp(4), gr_sp(6)))
  # journal style (D20): no box around the plotting area, open L-shaped axes
  # as on the bar panels
  if (isTRUE(.gr_opts$journal)) th + gr_journal_axes() else th
}
gr_journal_axes <- function() ggplot2::theme(
  panel.border = ggplot2::element_blank(),
  axis.line = ggplot2::element_line(colour = "black", linewidth = .7))

# ------------------------------------------------------------
# Writing figure files. Every write is checked: an error, or a device that
# "succeeds" without leaving a non-empty file (a path Windows cannot create),
# is logged with the reason and counted in .gr_opts$n_failed, so the run
# summary can say how many of the expected files exist. Only the device a
# write opened is ever closed.
# ------------------------------------------------------------
# Longest full path Windows accepts unless long paths are enabled (MAX_PATH 260
# including the terminating NUL)
GR_MAX_PATH <- 259L
.gr_opts$n_failed <- 0L

gr_write_failed <- function(fn, why, logger = NULL, what = "Figure", count = TRUE) {
  if (count) .gr_opts$n_failed <- (.gr_opts$n_failed %||% 0L) + 1L
  full <- normalizePath(fn, winslash = "\\", mustWork = FALSE)
  why <- trimws(strsplit(as.character(why)[1], "\n", fixed = TRUE)[[1]][1])   # one log line
  hint <- if (nchar(full) > GR_MAX_PATH)
    sprintf(" - the full path is %d characters, beyond the %d Windows allows unless long paths are enabled; choose a shorter output folder or output prefix",
            nchar(full), GR_MAX_PATH) else ""
  lg <- if (is.function(logger)) logger else message
  lg(sprintf("%s NOT written: %s (%s%s)", what, full, why, hint))
}

GR_FORMATS <- c("png", "tiff", "jpeg", "pdf", "eps")

# ------------------------------------------------------------
# Figure style and size of a run (decisions D20, D21), read once from cfg:
#   figure_style "journal" (default): white background everywhere, no frame
#     around a figure or a section, no section title bands or captions, no
#     title line or method paragraph inside the double-angle / semicircle
#     images, no shaded wedges; the method text, the panel titles and the
#     abbreviations go to a legend file <figure>_legend.txt beside each
#     figure (gr_write_legend). Text is never smaller than 6 pt at the width
#     the figure is written at (GR_MIN_TEXT_PT).
#   figure_style "annotated": the v1.0.0 look (frames, bands, in-image text).
#   figure_width_cm: blank = the current sizes; a width (8.76, 12.69 and 17.77
#     cm are the journal's column widths) lays every figure out AT that
#     physical width - the composites with as many panels per row as fit
#     (gr_composite_cols) - so the text keeps its point size; nothing is
#     shrunk after drawing.
#   graph_formats: png, tiff, jpeg, pdf, eps (vector); graph_dpi up to
#     GR_DPI_MAX. Colour stays RGB: R cannot write CMYK files, so a journal's
#     CMYK conversion is done outside the app.
# ------------------------------------------------------------
GR_FIGURE_STYLES <- c("journal", "annotated")
GR_DPI_MAX <- 1200
GR_DPI_MIN <- 50
GR_MIN_TEXT_PT <- 6
GR_WIDTH_CM_MAX <- 60
gr_figure_settings <- function(cfg, logger = NULL) {
  say <- function(m) if (!is.null(logger)) log_once(logger, m)
  st <- tolower(trimws(as.character(unlist(cfg$figure_style %||% "journal"))[1]))
  if (is.na(st) || !nzchar(st)) st <- "journal"
  if (!(st %in% GR_FIGURE_STYLES)) {
    say(sprintf("Figure style '%s' is not known (use journal or annotated) - journal is used.",
                as.character(unlist(cfg$figure_style))[1]))
    st <- "journal"
  }
  wr <- unlist(cfg$figure_width_cm)
  wcm <- if (is.null(wr) || !length(wr) || is.na(wr[1]) || !nzchar(trimws(as.character(wr[1])))) NA_real_
         else suppressWarnings(as.numeric(as.character(wr[1])))
  if (length(wr) && !is.null(wr) && !is.na(wr[1]) && nzchar(trimws(as.character(wr[1]))) &&
      (!is.finite(wcm) || wcm <= 0 || wcm > GR_WIDTH_CM_MAX)) {
    say(sprintf("Figure width '%s' cm is not a width between 0 and %d cm - the figures keep their own sizes.",
                as.character(wr[1]), GR_WIDTH_CM_MAX))
    wcm <- NA_real_
  }
  dpi <- suppressWarnings(as.numeric(unlist(cfg$graph_dpi %||% 300))[1])
  if (!is.finite(dpi)) { say("Graph resolution is not a number - 300 dpi is used."); dpi <- 300 }
  if (dpi > GR_DPI_MAX) {
    say(sprintf("Graph resolution %s dpi is above %d - %d dpi is used.", format(dpi), GR_DPI_MAX, GR_DPI_MAX))
    dpi <- GR_DPI_MAX
  } else if (dpi < GR_DPI_MIN) {
    say(sprintf("Graph resolution %s dpi is below %d - %d dpi is used.", format(dpi), GR_DPI_MIN, GR_DPI_MIN))
    dpi <- GR_DPI_MIN
  }
  list(style = st, journal = identical(st, "journal"), width_cm = wcm,
       width_in = if (is.finite(wcm)) wcm / 2.54 else NA_real_,
       formats = gr_normalise_formats(cfg$graph_formats %||% list("png"), if (is.null(logger)) function(m) NULL
                                      else function(m) log_once(logger, m)),
       dpi = dpi)
}
# The settings in words, for the run log and the Manifest.
gr_figure_settings_text <- function(fs) {
  paste0(if (fs$journal) "journal style (no frames, section bands, captions or shaded wedges; titles, method text and abbreviations in <figure>_legend.txt)"
         else "annotated style (the v1.0.0 look)",
         "; width ", if (is.finite(fs$width_cm))
           paste0(format(fs$width_cm), " cm (laid out at that width; combined figures ",
                  if (fs$width_cm < 12) "one panel" else "two panels", " per row, a combined figure taller than ",
                  GR_PAGE_MAX_H_CM, " cm split into pages _p1, _p2 ... with the panel letters continuing)")
         else "as designed",
         "; ", paste(toupper(fs$formats), collapse = ", "), " at ", format(fs$dpi), " dpi",
         if ("eps" %in% fs$formats)
           paste0(" (EPS vector; its semi-transparent points and regions are embedded as images at ",
                  format(gr_eps_fallback_dpi(fs$dpi)), " dpi)") else "",
         "; RGB color")
}

# ------------------------------------------------------------
# Legend files (decision D20). In the journal style the text that is not drawn
# inside a figure - its title, the method text of the double-angle and
# semicircle plots, the panel titles of a composite, notes - is written to
# <figure>_legend.txt beside it, followed by every abbreviation and symbol the
# figure shows, defined, in alphabetical order and separated by semicolons
# (the journal's legend format). The abbreviations are read from the text
# actually drawn (gr_figure_texts), so the list never names one the figure
# does not use.
# ------------------------------------------------------------
GR_ABBREV <- c(
  "AoE" = "angle of error", "|AoE|" = "absolute angle of error",
  "CDVA" = "corrected distance visual acuity", "CI" = "correction index",
  "Cyl" = "refractive cylinder",
  "d" = "Cohen's d (standardized mean difference)", "DEQ" = "defocus equivalent",
  "N, n" = "number of eyes",
  "R²" = "coefficient of determination of the regression line",
  "y and x (y = ax + b)" = "vertical-axis and horizontal-axis values of a group's least-squares regression line",
  "DV" = "difference vector", "ESS" = "effective sample size",
  "GEE" = "generalized estimating equations",
  "JCRS" = "Journal of Cataract & Refractive Surgery",
  # the refractive procedures, as a subset or cohort name prints them (a subset
  # run names its subset in every legend title, finding F8)
  "KLEx" = "keratorefractive lenticule extraction", "LASIK" = "laser in situ keratomileusis",
  "PRK" = "photorefractive keratectomy",
  "logMAR" = "logarithm of the minimum angle of resolution", "med" = "median",
  "mo" = "months",
  "B" = "number of bootstrap resamples",
  "PE" = "prediction error of refractive astigmatism (postoperative - target)",
  "Postop" = "postoperative", "Pre" = "preoperative visit", "Preop" = "preoperative",
  "RA" = "refractive astigmatism",
  "RMS" = "root mean square", "SD" = "standard deviation", "SE" = "standard error",
  "SEQ" = "spherical equivalent refraction", "SIA" = "surgically induced astigmatism",
  "T²" = "Hotelling's T² statistic (bivariate test of centroids)",
  "TIA" = "target induced astigmatism", "UDVA" = "uncorrected distance visual acuity",
  "VA" = "visual acuity",
  "WHWK" = "Wilcox-Holladay-Wang-Koch R functions (the reference tool of the JCRS 2025 standard)",
  "x, y" = "doubled-angle components of the centroid, M cos 2θ and M sin 2θ (M = magnitude, θ = plus axis)")
GR_ABBREV_RE <- c("d" = "\\(d = |\\bd = -?[0-9]", "B" = "\\bB = [0-9]",
                  "x, y" = "\\(x -?[0-9.]+; y |centroid x -?[0-9.]+, y ",
                  "|AoE|" = "\\|AoE\\|",
                  "N, n" = "(?<![A-Za-z])N( UDVA| CDVA)? ?= ?[0-9]|(?<![A-Za-z])n ?= ?[0-9]|\\bn per group",
                  "mo" = "[0-9] mo\\b",
                  "y and x (y = ax + b)" = "(?<![A-Za-z])y ?= ?-?[0-9.]+x")
GR_SYMBOLS <- c("●" = "eyes of a group (and its color key)", "◆" = "centroid (mean vector)",
                "▼" = "origin (zero astigmatism)",
                "[Holm]" = "P values Holm-adjusted for the comparisons with the reference group")

# The legend line of a bar panel that left some value labels out because they
# would overlap at its width (gr_bars; finding F3)
GR_THIN_NOTE <- paste0("Value labels of some bars are not printed at this width (they would overlap another ",
                       "label or a taller bar beside them); those bars are read on the percentage axis.")

# Method lines a panel's legend carries whatever its text (journal style): the
# reference lines of the two scatters, and the bin rules of the two histograms
# (decisions D20, D27), so an eye on a bin edge can be placed from the legend.
GR_PANEL_NOTES <- list(
  scatter = paste0("Solid blue line, y = x (attempted equals achieved); green lines, ±0.50 D; ",
                   "magenta lines, ±1.00 D; dotted line, the least-squares regression line of each group."),
  "10_CIhist" = paste0("Correction index bins are closed on the right: a bin labeled 0.16 to 0.30 holds ",
                       "0.15 < CI ≤ 0.30, so a ratio of 0.153 is in it."),
  "11_AEhist" = paste0("Angle-of-error bins include the edge nearer zero: 5° and -5° are in -5 to 5, ",
                       "15° is in 5 to 15 and -15° in -15 to -5; the angle is positive when the SIA lies counterclockwise of the TIA."))
# The outcome each panel's header P value compares (finding F7: the legend
# names it, so a P beside two summaries - AoE and |AoE| - is never misread).
GR_PANEL_P <- c(
  "02_Efficacy" = "P (header): comparison of the efficacy index between the groups.",
  "03_Safety" = "P (header): comparison of the safety index between the groups.",
  "04_SEQ" = "P (header): comparison of the postoperative spherical equivalent (SEQ - target when a target is mapped), the quantity on the horizontal axis.",
  "06_DEQ" = "P (header): comparison of the postoperative defocus equivalent between the groups.",
  "08_Cylinder" = "P (header): comparison of the postoperative refractive astigmatism (magnitude) between the groups.",
  "10_CIhist" = "P (header): comparison of the correction index between the groups.",
  "11_AEhist" = "P (header): comparison of the signed angle of error between the groups; |AoE| is descriptive.")
# texts: the panel's drawn text; the P line is added only when it prints a P
gr_panel_notes <- function(nm, texts = NULL) {
  key <- if (nm %in% c("05_SEQregress", "09_CYLregress")) "scatter" else nm
  pl <- if (nm %in% names(GR_PANEL_P) &&
            (is.null(texts) || any(grepl("\\bP ?[=<>]", as.character(unlist(texts)))))) GR_PANEL_P[[nm]]
        else character(0)
  if (key == "scatter" && nm == "09_CYLregress")
    return(sub("attempted equals achieved", "SIA equals TIA", GR_PANEL_NOTES$scatter, fixed = TRUE))
  c(GR_PANEL_NOTES[[key]] %||% character(0), pl)
}

# the sort key of an abbreviation: its letters and digits, lower case
gr_abbrev_key <- function(ab) tolower(gsub("[^A-Za-z0-9]", "", ab))

# "Abbreviations: ...; Symbols: ..." for the texts `txt` (NULL when none occurs)
gr_abbrev_line <- function(txt) {
  txt <- as.character(unlist(txt)); txt <- txt[!is.na(txt) & nzchar(txt)]
  if (!length(txt)) return(NULL)
  all <- paste(txt, collapse = "\n")
  has <- vapply(names(GR_ABBREV), function(a) {
    re <- if (a %in% names(GR_ABBREV_RE)) GR_ABBREV_RE[[a]] else paste0("(?<![A-Za-z])", a, "(?![A-Za-z])")
    grepl(re, all, perl = TRUE)
  }, logical(1))
  ab <- names(GR_ABBREV)[has]
  # alphabetical by letters, case and signs ignored ("|AoE|" beside "AoE")
  ab <- ab[order(gr_abbrev_key(ab))]
  sy <- names(GR_SYMBOLS)[vapply(names(GR_SYMBOLS), function(s) grepl(s, all, fixed = TRUE), logical(1))]
  parts <- c(if (length(ab)) paste0("Abbreviations: ", paste(sprintf("%s = %s", ab, GR_ABBREV[ab]), collapse = "; "), "."),
             if (length(sy)) paste0("Symbols: ", paste(sprintf("%s = %s", sy, GR_SYMBOLS[sy]), collapse = "; "), "."))
  if (length(parts)) parts else NULL
}

# Every text a figure draws (axis titles and labels, header lines, value
# labels, N boxes): the labels of its text grobs, read from the built grob tree.
gr_figure_texts <- function(p) {
  g <- if (inherits(p, "ggplot")) tryCatch(ggplot2::ggplotGrob(p), error = function(e) NULL) else p
  out <- character(0)
  walk <- function(x, depth = 0) {
    if (is.null(x) || depth > 60) return(invisible())
    if (inherits(x, "mheader_fit"))
      out <<- c(out, unlist(lapply(x$spec, function(cl) lapply(cl$lines, function(l)
        paste(vapply(l, function(s) as.character(s$text), character(1)), collapse = "")))))
    # (the stability axis draws its count rows as "N =" and the counts)
    if (inherits(x, "stab_axis")) out <<- c(out, as.character(x$labs),
                                            if (length(unlist(x$nrows))) paste("N =", unlist(x$nrows)))
    if (inherits(x, "stab_labels")) out <<- c(out, as.character(x$lab$lab))
    # the header markers (HDR_MARKS) and the points of a group are drawn as
    # symbols, not text: a filled circle (header marker, or plotted eyes) is
    # "●", a pch-23 diamond "◆", a pch-25 triangle (the semicircle's origin)
    # "▼", so the legend's Symbols line defines them
    if (inherits(x, "circle")) out <<- c(out, "●")
    if (inherits(x, "points")) {
      pch <- suppressWarnings(as.integer(unlist(x$pch)))
      if (any(pch %in% 23L)) out <<- c(out, "◆")
      if (any(pch %in% 25L)) out <<- c(out, "▼")
      if (any(pch %in% c(16L, 19L, 20L, 21L))) out <<- c(out, "●")
    }
    lab <- tryCatch(x$label, error = function(e) NULL)
    if (is.character(lab)) out <<- c(out, lab)
    if (inherits(x, "gtable")) for (k in x$grobs) walk(k, depth + 1)
    kids <- tryCatch(x$children, error = function(e) NULL)
    if (length(kids)) for (k in kids) walk(k, depth + 1)
  }
  tryCatch(walk(g), error = function(e) NULL)
  unique(out[!is.na(out) & nzchar(out)])
}

# Write <base>_legend.txt (journal style only): the title, the method lines and
# the abbreviations / symbols of `texts` (the figure's drawn text, e.g.
# gr_figure_texts()) and of the method lines. cfg: optional, to read the style
# when it is not the current figure pass's. Never stops a run: a failure is
# logged. Returns the file written, or NULL.
gr_write_legend <- function(base, title, lines = character(0), texts = character(0), cfg = NULL,
                            logger = NULL) {
  journal <- if (!is.null(cfg)) gr_figure_settings(cfg)$journal else isTRUE(.gr_opts$journal)
  if (!journal) return(invisible(NULL))
  lines <- as.character(unlist(lines)); lines <- lines[!is.na(lines) & nzchar(lines)]
  # "95% CI" (a confidence interval) in the method text is not the correction
  # index; the title is scanned too ("... DV = -PE")
  scan <- c(texts, gsub("95% CI", "95% interval", c(as.character(title)[1], lines), fixed = TRUE))
  body <- c(as.character(title)[1], lines, gr_abbrev_line(scan))
  fn <- paste0(base, "_legend.txt")
  ok <- tryCatch({
    con <- file(fn, open = "w", encoding = "UTF-8")
    on.exit(close(con), add = TRUE)
    writeLines(body, con)
    TRUE
  }, error = function(e) {
    lg <- if (is.function(logger)) logger else message
    lg(sprintf("Figure legend NOT written: %s (%s)", normalizePath(fn, winslash = "\\", mustWork = FALSE),
               conditionMessage(e)))
    FALSE
  })
  if (isTRUE(ok)) invisible(fn) else invisible(NULL)
}

# The legend lines that name the inference behind a figure's P values (decision
# D11; D18), from its drawn text `texts`: "GEE P values: clustered by patient;
# <working correlation>; <gee_inference_text>." when a GEE P value is printed
# (a label that only says "GEE not fitted" does not count), and on a matched
# pass the matched-analysis note. Without patient ids in scope a (weighted) GEE
# has each eye as its own cluster, and the line says so. The working
# correlation is named (R3-4, finding S4): exchangeable unless a test is
# labeled independence, or independence on a matched pass. polar = FALSE (the
# standard panels): a "(GEE not fitted)" mark is the permutation test over
# patients (p_omnibus; finding S2), said in a line; and when the summary tables
# run the classical tests (table_tests = "classical", .gr_opts$tables_classical)
# a line says that their P values can differ from the figure's clustered ones
# (finding S3). The double-angle and semicircle figures (polar = TRUE) explain
# their own fallbacks in their method lines.
gr_gee_legend_lines <- function(texts, polar = FALSE) {
  t0 <- as.character(unlist(texts))
  t <- gsub("GEE not fitted", "", t0, fixed = TRUE)
  out <- character(0)
  if (any(grepl("GEE", t, fixed = TRUE))) {
    eye <- is.null(.gr_opts$cluster_id)
    wc <- if (isTRUE(.gr_opts$matched)) "independence working correlation (matched cohort)"
          else "exchangeable working correlation unless a test is labeled independence"
    out <- paste0("GEE P values: ", if (eye) "each eye its own cluster (no patient ID)" else "clustered by patient",
                  "; ", wc, "; ", gee_inference_text(unit = if (eye) "eye" else "patient"), ".")
    if (!polar && isTRUE(.gr_opts$tables_classical) && !eye &&
        any(grepl("GEE", t, fixed = TRUE) & !grepl("Weighted GEE", t, fixed = TRUE)))
      out <- c(out, paste0("The summary tables of this run use the classical tests (Summary-table tests = classical: ",
                           "eyes treated as independent), so their P values can differ from the patient-clustered ",
                           "P values of this figure."))
    # an independence refit named in a test label (decision R3-4)
    if (any(grepl("independence working correlation", t, fixed = TRUE)))
      out <- c(out, paste0("Independence working correlation: the exchangeable GEE of that test was not accepted ",
                           "(working correlation ", GEE_EXCH_ALPHA_MAX, " or more, no convergence, or not at its ",
                           "solution - the outcome is almost constant within patients); the robust SE stay ",
                           "clustered by patient."))
  }
  # a standard panel's "(GEE not fitted)": the permutation test over patients
  if (!polar && any(grepl("GEE not fitted", t0, fixed = TRUE)))
    out <- c(out, paste0("GEE not fitted: the patient-clustered GEE of that test could not be fitted (a group with no ",
                         "events or only events, no convergence, or a singular covariance); its P value comes from a ",
                         "permutation test over patients - the group labels permuted across patients, each patient's ",
                         "eyes kept together, ", CLUSTER_PERM_B, " permutations, fixed seed."))
  # "clustered by patient" only when the figure's tests have patient ids
  # (decision R2-7); otherwise each eye is its own cluster
  if (isTRUE(.gr_opts$matched) && exists("matched_analysis_note", mode = "function"))
    out <- c(out, matched_analysis_note(clustered = !is.null(.gr_opts$cluster_id)))
  out
}

# PDF device. grDevices::pdf() is a single-byte Type-1 device: every glyph
# outside Latin-1 (Hebrew group names, ≥ ≤ ±²) is replaced by a dot. cairo_pdf
# embeds real fonts and keeps them; the header markers are plotted symbols
# (HDR_MARKS), since its font has no "◆". This build's cairo has no text
# shaping, so right-to-left text comes out letter-reversed in PDFs only: the run
# log says so when a label needs it (gr_rtl_pdf_note).
gr_pdf_device <- function() {
  if (isTRUE(tryCatch(capabilities("cairo"), error = function(e) FALSE)))
    grDevices::cairo_pdf else grDevices::pdf
}

# EPS (vector, decision D21): cairo_ps embeds real fonts like cairo_pdf (the
# header markers are plotted symbols either way). PostScript has no
# transparency, so cairo embeds each semi-transparent point or region as an
# image: at GR_EPS_MIN_DPI at least (the journals' resolution for line art),
# or the run's resolution when that is higher; the Manifest's Figures row and
# the README say so. Without cairo: postscript(), whose Latin-1 fonts lose
# glyphs such as ≤ (the run log says so once).
GR_EPS_MIN_DPI <- 1200
gr_eps_fallback_dpi <- function(dpi) max(as.numeric(dpi)[1], GR_EPS_MIN_DPI, na.rm = TRUE)
gr_eps_open <- function(fn, w, h, dpi) {
  if (isTRUE(tryCatch(capabilities("cairo"), error = function(e) FALSE)))
    grDevices::cairo_ps(fn, width = w, height = h, onefile = FALSE, bg = "white",
                        fallback_resolution = gr_eps_fallback_dpi(dpi))
  else grDevices::postscript(fn, width = w, height = h, onefile = FALSE, horizontal = FALSE,
                             paper = "special", bg = "white")
}

# open a graphics device for `fm`; returns the device number
gr_open_device <- function(fn, fm, w, h, dpi) {
  switch(fm,
    png  = grDevices::png(fn, width = w, height = h, units = "in", res = dpi, bg = "white"),
    tiff = grDevices::tiff(fn, width = w, height = h, units = "in", res = dpi, compression = "lzw", bg = "white"),
    jpeg = grDevices::jpeg(fn, width = w, height = h, units = "in", res = dpi, quality = 95, bg = "white"),
    pdf  = gr_pdf_device()(fn, width = w, height = h, bg = "white"),
    eps  = gr_eps_open(fn, w, h, dpi),
    stop("unsupported graph format '", fm, "' (use png, tiff, jpeg, pdf or eps)"))
  grDevices::dev.cur()
}

# The formats the figure writers accept, normalised ONCE before any device is
# opened: case-insensitive, "jpg"/"tif" accepted as aliases. Anything else is
# dropped with a log line, so it can neither report a file that was never
# written nor open R's default device (a stray Rplots.pdf in the app folder).
gr_normalise_formats <- function(formats, logger = message) {
  f <- tolower(trimws(as.character(unlist(formats))))
  f <- f[!is.na(f) & nzchar(f)]
  f[f == "jpg"] <- "jpeg"; f[f == "tif"] <- "tiff"; f[f == "ps"] <- "eps"
  bad <- unique(setdiff(f, GR_FORMATS))
  if (length(bad))
    logger(sprintf("Unsupported graph format(s) ignored: %s (use png, tiff, jpeg, pdf or eps).",
                   paste(bad, collapse = ", ")))
  f <- unique(intersect(f, GR_FORMATS))
  if (!length(f)) {
    f <- "tiff"
    if (length(bad)) logger("No supported graph format left - writing TIFF.")
  }
  f
}

# draw() onto a fresh device writing `fn`; the device is closed even on error
gr_draw_to <- function(fn, fm, w, h, dpi, draw) {
  dev <- gr_open_device(fn, fm, w, h, dpi)
  on.exit(if (dev %in% grDevices::dev.list()) grDevices::dev.off(dev), add = TRUE)
  draw()
  invisible(TRUE)
}

# Run write() (which must create `fn`); TRUE when a non-empty file results.
# A device that cannot open its file warns ("unable to open file ...") before it
# fails: that warning becomes part of the one logged reason instead of being
# printed, unexplained, at the end of the run.
gr_write_file <- function(fn, write, logger = NULL, what = "Figure", count = TRUE) {
  if (file.exists(fn)) unlink(fn)          # the existence check below must mean THIS write
  dev_warn <- character(0)
  err <- tryCatch({
    withCallingHandlers(write(), warning = function(cnd) {
      m <- conditionMessage(cnd)
      if (grepl("unable to (open|start)|opening device|cannot open", m)) {
        dev_warn <<- c(dev_warn, m); invokeRestart("muffleWarning")
      }
    })
    ""
  }, error = function(e) paste(unique(c(conditionMessage(e), dev_warn)), collapse = ": "))
  if (nzchar(err)) {
    if (file.exists(fn)) unlink(fn)        # never leave a half-drawn file behind
    gr_write_failed(fn, err, logger, what, count); return(FALSE)
  }
  if (!file.exists(fn) || !isTRUE(file.info(fn)$size > 0)) {
    gr_write_failed(fn, paste(c("the file was not created", dev_warn), collapse = ": "),
                    logger, what, count)
    return(FALSE)
  }
  TRUE
}

save_graph <- function(p, base, formats, dpi, w = 6.5, h = 5.4, logger = NULL,
                       what = "Figure", count = TRUE) {
  out <- character(0)
  if (length(formats) == 0) formats <- "tiff"
  # panels carrying a grid header come through as a gtable/grob, not a ggplot
  is_grob <- inherits(p, "gtable") || inherits(p, "grob")
  for (fm in unique(formats)) {
    fn <- paste0(base, ".", fm)
    write <- if (is_grob) function()
      gr_draw_to(fn, fm, w, h, dpi, function() { grid::grid.newpage(); grid::grid.draw(p) })
    else if (fm == "tiff") function()
      # bg="white": theme_void panels are otherwise transparent, which TIFF
      # viewers render as BLACK
      ggplot2::ggsave(fn, p, width = w, height = h, dpi = dpi, compression = "lzw", bg = "white")
    else if (fm == "pdf") function()
      ggplot2::ggsave(fn, p, device = gr_pdf_device(), width = w, height = h, bg = "white")
    else if (fm == "eps") function()
      gr_draw_to(fn, fm, w, h, dpi, function() print(p))
    else if (fm %in% GR_FORMATS) function()
      ggplot2::ggsave(fn, p, width = w, height = h, dpi = dpi, bg = "white")
    else function() stop("unsupported graph format '", fm, "' (use png, tiff, jpeg, pdf or eps)")
    if (gr_write_file(fn, write, logger, what, count)) out <- c(out, fn)
  }
  out
}

# The line printed in place of a P value when no test could run, so the reader
# can tell "not applicable" from an omission ("" when there is no reason), e.g.
# "P not computed: B has n = 1" - the wording the tables use.
P_NOT_COMPUTED <- "P not computed: "
p_not_tested <- function(reason) {
  reason <- as.character(reason %||% "")[1]
  if (is.na(reason) || !nzchar(reason)) "" else paste0(P_NOT_COMPUTED, reason)
}

# The label of a clustered figure P value whose GEE could not be fitted: the
# tables' permutation test over patients ran instead (p_omnibus; findings S2,
# T1-02). The legend file says what it is (gr_gee_legend_lines).
GR_PERM_TEST <- "Permutation test over patients (GEE not fitted)"

# The test name as printed in a two-group header, short enough for one line
gr_short_test <- function(lab) {
  lab <- as.character(lab)
  # an independence refit (R3-4) keeps its name, shortened
  if (grepl(GEE_INDEP_REFIT, lab, fixed = TRUE))
    return(sub(paste0("clustered by patient; ", GEE_INDEP_REFIT), "clustered; independence working correlation",
               lab, fixed = TRUE))
  switch(lab,
    "GEE (clustered by patient)"          = "GEE (clustered)",
    "GEE on ranks (clustered by patient)" = "GEE on ranks (clustered)",
    "Weighted GEE (clustered by patient)" = "Weighted GEE (clustered)",
    "Weighted GEE (robust SE)"            = "Weighted GEE",
    "Welch t-test"                        = "Welch t",
    "Permutation test over patients (GEE not fitted)" = "Permutation over patients (GEE not fitted)",
    lab)
}

# P between two groups: the same test p_omnibus runs (GEE when clustered, else
# the Welch t-test of the summary tables), NAMED, e.g. "GEE (clustered): P =
# 0.0211" or "Welch t: P = 0.4546", so a clustered, an unclustered and a
# fallen-back P value never look alike; or "P not computed: <reason>".
p_two <- function(v, gl, glv) {
  if (length(glv) != 2) return("")
  om <- p_omnibus(v, gl, glv, "continuous")
  if (!nzchar(om$text)) return(p_not_tested(om$reason))
  gr_with_few(if (nzchar(om$test)) paste0(gr_short_test(om$test), ": ", om$text) else om$text, om$few)
}

# Stat lines carry attr "few": the groups with 2 to GEE_FEW_PATIENTS - 1
# patients in a clustered or weighted GEE P value printed on them (named
# patient counts, helpers.R few_patient_groups), for the caution line.
gr_with_few <- function(txt, few) {
  if (length(few)) attr(txt, "few") <- few
  txt
}
# (the P values of one panel share their clustering, so the first "unit" found
# - patient, or eye without patient ids - is the unit of them all)
gr_few_merge <- function(...) {
  parts <- unname(list(...))      # named arguments would prefix the group names
  m <- unlist(parts)
  if (!length(m)) return(NULL)
  m <- m[!duplicated(names(m))]
  u <- unlist(lapply(parts, function(z) attr(z, "unit")))
  if (length(u)) attr(m, "unit") <- u[1]
  m
}
# The caution line for a panel's stat lines (pp: p_group_text() output, or
# any value with attr "few"); "" when none. Recorded for the run log either way.
gr_caution <- function(pp) few_patients_text(attr(pp, "few"), short = TRUE)

# a value rounded to `d` places with a negative zero turned into 0, so a mean of
# -0.003 prints "0.00" and not "-0.00"
gr_round0 <- function(x, d = 2) round(x, d) + 0

# "mean ± SD" over the finite values: "n/a" when there are none and
# "<value> (n = 1)" for a single eye, whose SD does not exist
msd <- function(x, d = 2) {
  x <- gr_num(x); x <- x[is.finite(x)]
  fm <- paste0("%.", d, "f")
  if (!length(x)) return("n/a")
  if (length(x) == 1) return(sprintf(paste0(fm, " (n = 1)"), gr_round0(x, d)))
  sprintf(paste0(fm, " ± ", fm), gr_round0(mean(x), d), stats::sd(x))
}

# Mean acuity of logMAR values for a header (decision D16): "20/20 (0.00 ±
# 0.05 logMAR)" - the Snellen geometric mean with the logMAR mean ± SD, as in
# the tables (helpers.R fmt_va); "n/a" without a usable value
gr_va_text <- function(lm) {
  lm <- gr_num(lm); lm <- lm[is.finite(lm)]
  if (!length(lm)) return("n/a")
  fmt_va(mean(lm), if (length(lm) > 1) stats::sd(lm) else NA_real_, range = FALSE)
}

# one number for a header; "n/a" when it is undefined (an SD of one eye)
f2 <- function(v, d = 2) if (length(v) == 1 && is.finite(v)) sprintf(paste0("%.", d, "f"), v) else "n/a"

# "84.2%": the share of TRUE among the eyes where `cond` is defined; "n/a" when
# no eye has it defined (a group without a single usable value), never "NaN%".
# One decimal by default: every header percentage of every panel has the same
# precision as the tables (D24), whatever the number of groups
gr_pct <- function(cond, d = 1) {
  cond <- cond[!is.na(cond)]
  if (!length(cond)) "n/a" else sprintf(paste0("%.", d, "f%%"), mean(cond) * 100)
}

# ============================================================
# mEYEstro inline-colored headers (drawn with grid, no ggtext dep)
# A "spec" is a list of columns; each column = list(x=<npc from panel
# left>, lines=list(<line>,...)); each <line> = list(<seg>,...);
# each <seg> = list(text=, col=). Segments are laid out left-to-right,
# their widths measured at draw time so inline colour is exact at any
# panel size. Columns are TOP-aligned; the block is bottom-anchored so
# the last line sits just above the panel.
# ============================================================
MHDR_FS <- 8.5      # header font size (pt)
MHDR_LS <- 11.5     # line skip (pt)

# Statistics lines a panel leaves out of its cell in a combined figure at a
# journal width (decision R2-2): .gr_opts$short_hdr asks the panel being built
# for its short header, gr_moved() records each line (or part) left out, and
# the combined figure's legend file lists them under the panel's letter.
.gr_opts$short_hdr <- FALSE
.gr_opts$moved <- character(0)
gr_moved <- function(txt) {
  txt <- trimws(as.character(unlist(txt)))
  txt <- txt[!is.na(txt) & nzchar(txt)]
  if (length(txt)) .gr_opts$moved <- c(.gr_opts$moved, txt)
  invisible(NULL)
}
# the text of one header line (its segments joined)
gr_line_text <- function(line) paste(vapply(line, function(s) as.character(s$text %||% ""), character(1)), collapse = "")

# ------------------------------------------------------------
# Text scale of the panels being built (decision R2-2). 1 = the designed
# sizes. A combined figure laid out at a journal width (figure_width_cm) builds
# its bar, scatter and stability panels again for their cells with every font
# size times .gr_opts$tscale - never below GR_MIN_TEXT_PT - and every spacing in
# points times the same factor, so a two-panel row at 17.77 cm is about 7.6 cm
# high; the panels' own files keep the designed sizes. A grob that is drawn
# later (a header, the stability axis and labels) keeps the size it was built
# with.
# ------------------------------------------------------------
.gr_opts$tscale <- 1
gr_ts <- function() min(1, .gr_opts$tscale %||% 1)
gr_pt <- function(pt) if (gr_ts() >= 1) pt else max(GR_MIN_TEXT_PT, pt * gr_ts())   # a font size (pt)
GR_PT_PER_MM <- 72.27 / 25.4                                     # ggplot2's .pt
gr_mm <- function(mm) gr_pt(mm * GR_PT_PER_MM) / GR_PT_PER_MM    # a ggplot text size (mm)
gr_sp <- function(pt) pt * gr_ts()                               # a spacing (pt)
# n text lines of the default 12-pt context as a unit, at the text scale - and
# never less than the lines of the header-size text they hold (fonts stop at
# GR_MIN_TEXT_PT while the scale goes on)
gr_lines <- function(n) if (gr_ts() >= 1) grid::unit(n, "lines") else
  grid::unit(n * max(14.4 * gr_ts(), 1.45 * hdr_fs()), "pt")
hdr_fs <- function() gr_pt(MHDR_FS)                              # header font size now
hdr_ls <- function() if (gr_ts() >= 1) MHDR_LS else 1.22 * hdr_fs()   # header line skip now

seg <- function(text, col = "black") list(text = text, col = col)

# "<label> <v1> vs <v2>[<tail>]" with the two group values coloured ("vs"
# without a period throughout, as in the axis titles - AMA style)
vs_line <- function(label, v1, v2, cols, tail = NULL) {
  L <- if (nzchar(label)) list(seg(label)) else list()
  L <- c(L, list(seg(v1, cols[[1]]), seg(" vs "), seg(v2, cols[[2]])))
  if (!is.null(tail) && nzchar(tail)) L <- c(L, list(seg(tail)))
  L
}

# Cohen's d between the two groups: mean difference over the POOLED SD,
# sqrt(((n1-1) var1 + (n2-1) var2) / (n1 + n2 - 2)), so unequal group sizes are
# weighted correctly. Computed per eye and unweighted, whatever the P value is
# (GEE-clustered or weighted on a matched cohort).
d_cohen <- function(x, gl, glv) {
  x <- gr_num(x)
  a <- x[!is.na(gl) & gl == glv[1]]; b <- x[!is.na(gl) & gl == glv[2]]
  a <- a[is.finite(a)]; b <- b[is.finite(b)]
  na <- length(a); nb <- length(b)
  if (na < 2 || nb < 2) return(NA_real_)
  sp <- sqrt(((na - 1) * stats::var(a) + (nb - 1) * stats::var(b)) / (na + nb - 2))
  if (!is.finite(sp) || sp == 0) return(NA_real_)
  (mean(a) - mean(b)) / sp
}
# "GEE (clustered): P = 0.0211 (d = -0.08)" text. pp: the panel's p_two() text
# when it already has it, so the test (and a GEE fallback in the run log's
# count) runs once per panel, not twice
p_d_text <- function(x, gl, glv, pp = NULL) {
  if (is.null(pp)) pp <- p_two(x, gl, glv)
  pp <- as.character(pp)[1]
  if (is.na(pp) || !nzchar(pp)) return("")
  if (startsWith(pp, P_NOT_COMPUTED)) return(pp)
  d <- d_cohen(x, gl, glv)
  # fixed decimals with no negative zero (D24): -0.003 prints "0.00"
  if (is.na(d)) pp else sprintf("%s (d = %s)", pp, fmt_fix(d, 2))
}
# the same as header lines for a NARROW header column: the test name on one
# line, "P = ... (d = ...)" on the next (a single line overflows the second
# column of a composite cell once the test is named)
p_d_lines <- function(x, gl, glv, pp = NULL) {
  txt <- p_d_text(x, gl, glv, pp)
  if (!nzchar(txt)) return(list())
  i <- regexpr(": P ", txt, fixed = TRUE)
  if (i < 1) return(list(list(seg(txt))))
  list(list(seg(substr(txt, 1, i))), list(seg(substr(txt, i + 2, nchar(txt)))))
}

# ============================================================
# N-group statistics for the StandardEyes figures.
#
# Test selection mirrors analysis.R::create_summary_table when patient
# clustering is off: the test is chosen by helpers.R::group_test, the SAME
# selector the summary table uses:
#   continuous  -> two groups: Welch t-test; more: Welch ANOVA (Kruskal-Wallis,
#                  labelled as such, when a group has zero variance)
#   ordinal     -> two groups: wilcox.test; more: kruskal.test
#   proportion  -> chi-squared; Fisher's exact when any expected count < 5
#                  (seeded Monte-Carlo Fisher, labelled, when FEXACT fails)
# With clustering on (the default) and at least one patient with two usable
# eyes, figure P values are GEE (clustered by patient) - the same model the
# summary table's clustered test runs (helpers.R gee_group_p, decision D12), so
# the figure's P equals the table's P column; with three or more groups the
# omnibus line is the joint Wald test of all group terms and each "vs
# reference" line is that pair's own GEE. Every GEE P value takes the run's
# small-sample inference (helpers.R gee_wald: Mancl-DeRouen, F reference, by
# default; decision D11). On a matched cohort the working correlation is
# independence (decision D18); with non-unit matching weights the tests are
# weighted GEE Wald tests, like the matched summary tables.
# Every P printed names its test, the two-group headers included; a GEE that
# was fitted but failed (no convergence, a numerically singular covariance, a
# group of a 0/1 outcome without events) is replaced by the tables' permutation
# test over patients (helpers.R cluster_perm_test), labelled "(GEE not fitted)"
# and counted in the run log (a weighted one has no fallback).
# No P at all - classical, clustered or weighted - when a compared group cannot
# support one (a group with a single usable value, a constant variable): this
# is checked BEFORE any model is fitted, with the reason the tables give
# (p_value_guard, helpers.R), and printed as "P not computed: B has n = 1"; a
# clustered or weighted GEE also needs two patients per group ("P not
# computed: B has 1 patient"; "B has 1 eye" when no patient ids are in scope and
# each eye is its own cluster) and the minimum data of every clustered model
# ("P not computed: too few eyes for this model (n = 5)", model_data_guard). A
# GEE P value involving a group of fewer than GEE_FEW_PATIENTS patients (eyes)
# gets a caution line under the stat lines, wrapped to the panel like them
# (gr_bars; under the N-boxes on the rotated two-group panels), and is named in
# the run log.
# Multiplicity: each "vs reference" line is Holm-adjusted within its panel and
# tagged [Holm] when it holds two or more comparisons, as the tables' columns
# of comparisons with the reference are (decision D14); the omnibus P is not
# adjusted.
# ============================================================
# "P = .03" / "P < .001" / "P > .99" - the run's P style (helpers.R p_eq,
# decision D15); the classic style keeps the v1.0.0 "P = 0.0312" of the panels
p_text <- function(p) {
  if (!is.finite(p)) return("")
  p_eq(p, where = "figure")
}

# group_test's label, shortened for a figure header ("Kruskal-Wallis test" ->
# "Kruskal-Wallis"); the label always names the test that actually ran
gr_test_label <- function(lab) {
  lab <- sub(" test", "", as.character(lab), fixed = TRUE)
  sub("(Welch ANOVA undefined: a group has zero variance)", "(a group has zero variance)",
      lab, fixed = TRUE)
}

# The outcome of each eye as the GEE reads it: the event indicator for a
# proportion (TRUE/FALSE when there is no event level), else the number
gr_test_y <- function(x, kind, event = NULL) {
  if (identical(kind, "proportion"))
    as.numeric(if (is.null(event)) as.logical(x) else as.character(x) == as.character(event))
  else gr_num(x)
}

# The one-patient guard is helpers.R cluster_guard_note(id, g, unit): "" when
# every group that has a usable eye holds at least two patients (distinct ids),
# else "B has 1 patient" for each group that does not ("B has 1 eye" when unit =
# "eye": each eye its own cluster). A GEE with robust SE has nothing to estimate
# a single cluster's variance from: that group's mean is treated as known
# exactly and the P value is meaningless. The tables, the GEE models and the
# figures share that one implementation (contract R4-9), as they share the
# missing-ID tokens of helpers.R fill_missing_ids().

# The test label of a clustered GEE P value whose exchangeable fit was not
# accepted (helpers.R gee_group_p attr "refit", decision R3-4): the
# independence working correlation is named inside the label's parentheses,
# "GEE (clustered by patient; independence working correlation ...)".
gr_refit_label <- function(lab, p) {
  if (!nzchar(as.character(attr(p, "refit") %||% "")[1])) return(lab)
  sub("\\)$", paste0("; ", GEE_INDEP_REFIT, ")"), lab)
}

# out$reason says why no P was computed (empty when one was), see p_not_tested()
p_omnibus <- function(v, gl, glv, kind = c("continuous", "ordinal", "proportion"),
                      event = NULL) {
  kind <- match.arg(kind)
  out <- list(p = NA_real_, test = "", text = "", reason = "")
  keep <- !is.na(gl) & gl %in% glv
  if (!any(keep)) { out$reason <- "no eyes with data"; return(out) }
  g <- factor(as.character(gl[keep]), levels = glv)
  x <- v[keep]
  cid <- gr_cluster_of(gl)
  wts <- gr_weights_of(gl)
  # the eyes a test can use: a usable value and, on a weighted cohort, a
  # positive matching weight (gee_group_p leaves the others out)
  y <- gr_test_y(x, kind, event)
  use <- (if (identical(kind, "proportion")) !is.na(y) else is.finite(y))
  if (!is.null(wts)) {
    wk <- suppressWarnings(as.numeric(wts[keep]))
    use <- use & is.finite(wk) & wk > 0
  }
  # No P value from a group that cannot support one (every compared group needs
  # two usable values; a proportion, an eye in every group and both outcomes):
  # checked HERE, before any GEE - weighted, clustered or none - is fitted, with
  # the tables' reason ("B has n = 1"), so a figure never prints a P its table
  # refuses
  why <- p_value_guard(y[use], g[use], if (identical(kind, "proportion")) "dichotomous" else kind)
  if (nzchar(why)) { out$reason <- why; return(out) }

  # Matched cohort with non-unit matching weights: a weighted GEE Wald test
  # (robust SE; clustered by patient when ids are in scope, else each eye its own
  # cluster), as in the matched summary tables. There is deliberately no
  # unweighted fallback: it would answer a different question under the same
  # "matched cohort" heading.
  if (!is.null(wts)) {
    idw <- if (!is.null(cid)) cid[keep] else paste0("row ", which(keep))
    unit_w <- if (is.null(cid)) "eye" else "patient"
    out$test <- if (!is.null(cid)) "Weighted GEE (clustered by patient)" else "Weighted GEE (robust SE)"
    # a group of one patient has no between-patient variance to estimate
    why_c <- if (!is.null(cid)) cluster_guard_note(idw[use], g[use]) else ""
    if (nzchar(why_c)) { out$reason <- why_c; return(out) }
    pw <- gee_group_p(x, g, idw, kind, event, w = wts[keep], unit = unit_w,
                      ranks = identical(kind, "ordinal"))
    if (is.finite(pw)) {
      out$p <- pw; out$text <- p_text(pw)
      # without patient ids each eye is its own cluster: the caution counts eyes
      out$few <- few_patient_groups(idw[use], g[use], unit_w)
    } else {
      # the matched table's reason when the data cannot support the model ("too
      # few eyes for this model (n = 5)", "no weighted test possible: no events
      # in B" - the guards of .weighted_wald, in its order); otherwise the
      # weighted GEE was tried and failed (no convergence, singular covariance)
      out$reason <- attr(pw, "why") %||% "the weighted test could not be computed"
    }
    return(out)
  }

  # Eyes of the same patient are correlated. When a cluster id is in scope
  # (patient clustering on) and at least one patient has two usable eyes, use a
  # GEE with robust SE instead of a test that assumes independent eyes - the
  # clustered table test runs the same model (helpers.R gee_group_p, decision
  # D12), so a table and a figure of one outcome give one P value. With one
  # eye per patient the classical test below runs, as in the tables (not a
  # fallback). An ordinal variable is tested on its ranks; a matched cohort
  # uses an independence working correlation (decision D18). A group whose
  # eyes all belong to one patient gets no P at all (cluster_guard_note), nor
  # do data below the minimum for any clustered model ("too few eyes for this
  # model (n = 5)", helpers.R model_data_guard), as in the GEE tables and the
  # weighted tests. A GEE that was fitted but failed (no convergence, a
  # numerically singular covariance, a group of a 0/1 outcome without events or
  # with only events) is replaced by the permutation test over patients of the
  # tables (helpers.R cluster_perm_test, findings S2, T1-02) - it keeps each
  # patient's eyes together, never an eye-level test - labelled "(GEE not
  # fitted)" and counted.
  gee_tried <- !is.null(cid) && fellow_eyes_present(cid[keep][use])
  if (gee_tried) {
    why_c <- cluster_guard_note(cid[keep][use], g[use])
    if (nzchar(why_c)) { out$reason <- why_c; return(out) }
    pc <- gee_group_p(x, g, cid[keep], kind, event, ranks = identical(kind, "ordinal"),
                      corstr = if (isTRUE(.gr_opts$matched)) "independence")
    if (is.finite(pc)) {
      out$p <- as.numeric(pc)
      out$test <- gr_refit_label(if (kind == "proportion") "GEE (binomial, clustered by patient)"
                                 else if (kind == "ordinal") "GEE on ranks (clustered by patient)"
                                 else "GEE (clustered by patient)", pc)
      out$text <- p_text(out$p)
      out$few <- few_patient_groups(cid[keep][use], g[use])
      return(out)
    }
    why_g <- as.character(attr(pc, "why") %||% "")[1]
    if (!is.na(why_g) && nzchar(why_g)) { out$reason <- why_g; return(out) }
    pp <- cluster_perm_test(if (identical(kind, "proportion")) y else x, g, cid[keep], kind)
    if (!is.finite(pp)) { out$reason <- "the permutation test over patients could not be computed"; return(out) }
    out$p <- pp
    out$test <- GR_PERM_TEST
    out$text <- p_text(pp)
    out$few <- few_patient_groups(cid[keep][use], g[use])
    .gr_opts$n_gee_fallback <- (.gr_opts$n_gee_fallback %||% 0L) + 1L
    return(out)
  }

  if (kind == "proportion") {
    # no event level: the values are read as TRUE/FALSE, as before
    gt <- group_test(if (is.null(event)) as.logical(x) else x, g, "dichotomous", event = event)
  } else {
    gt <- group_test(x, g, kind)
  }
  out$p <- gt$p
  out$test <- gr_test_label(gt$test)
  out$text <- p_text(out$p)
  # (with one eye per patient no GEE was needed and the classical test is
  # simply the test; a clustered GEE that could not be fitted never gets here:
  # its permutation test over patients ran above)
  if (!nzchar(out$text)) {
    out$reason <- gt$note %||% ""
    if (!nzchar(out$reason)) out$reason <- "the test could not be computed"
  }
  out
}

# Each group against the REFERENCE group (glv[1]). When the omnibus test is a
# clustered or weighted GEE (p_omnibus), each comparison is the Wald test of
# that group's coefficient in the SAME model of all groups (gee_group_p, each =
# TRUE) - the model whose coefficients the tables' GEE column reports, so a
# table and its figure print one number per comparison (decision D14).
# Otherwise - or when that model cannot be fitted, or for a comparison it
# cannot test - the same test as p_omnibus runs on the pair.
# Returns the RAW numeric P per comparison (NA when not testable), named by
# group; the caller adjusts for multiplicity (stat_footer: Holm). attr
# "fallback": the comparisons whose clustered GEE could not be fitted, so the
# permutation test over patients ran instead (stat_footer marks them "GEE not
# fitted").
p_vs_reference <- function(v, gl, glv, kind = "continuous", event = NULL) {
  if (length(glv) < 2) return(numeric(0))
  ref <- glv[1]
  jt <- gr_vs_ref_joint(v, gl, glv, kind, event)
  res <- lapply(glv[-1], function(g) {
    if (!is.null(jt) && is.finite(jt$p[[g]]))
      return(list(p = jt$p[[g]], test = jt$test, few = jt$few[[g]]))
    keep <- !is.na(gl) & gl %in% c(ref, g)
    restore <- gr_narrow_scope(gl, keep)       # cluster ids / weights stay aligned
    on.exit(restore(), add = TRUE)
    p_omnibus(v[keep], gl[keep], c(ref, g), kind, event)
  })
  out <- stats::setNames(vapply(res, function(o) o$p, numeric(1)), glv[-1])
  attr(out, "fallback") <- glv[-1][vapply(res, function(o)
    is.finite(o$p) && grepl("(GEE not fitted)", o$test, fixed = TRUE), logical(1))]
  attr(out, "few") <- do.call(gr_few_merge, lapply(res, function(o) if (is.finite(o$p)) o$few))
  out
}

# The comparisons with the reference from ONE GEE of all groups (three or
# more), when p_omnibus's test of these groups is a GEE - weighted (matching
# weights in scope) or clustered (cluster ids in scope and a patient with two
# usable eyes), with p_omnibus's model, working correlation and unit.
# list(p = nominal P per group, test = p_omnibus's label, few = per group the
# few-patients record of its pair); NULL when p_omnibus would run a classical
# test or the model of all groups cannot be fitted (the pairwise tests then
# run, as before).
gr_vs_ref_joint <- function(v, gl, glv, kind = "continuous", event = NULL) {
  if (length(glv) < 3) return(NULL)
  keep <- !is.na(gl) & gl %in% glv
  if (!any(keep)) return(NULL)
  g <- factor(as.character(gl[keep]), levels = glv)
  x <- v[keep]
  cid <- gr_cluster_of(gl)
  wts <- gr_weights_of(gl)
  y <- gr_test_y(x, kind, event)
  use <- (if (identical(kind, "proportion")) !is.na(y) else is.finite(y))
  ord <- identical(kind, "ordinal")
  if (!is.null(wts)) {
    wk <- suppressWarnings(as.numeric(wts[keep]))
    use <- use & is.finite(wk) & wk > 0
    idk <- if (!is.null(cid)) cid[keep] else paste0("row ", which(keep))
    unit <- if (is.null(cid)) "eye" else "patient"
    p <- gee_group_p(x, g, idk, kind, event, w = wts[keep], unit = unit, ranks = ord, each = TRUE)
    test <- if (!is.null(cid)) "Weighted GEE (clustered by patient)" else "Weighted GEE (robust SE)"
  } else if (!is.null(cid) && fellow_eyes_present(cid[keep][use])) {
    idk <- cid[keep]
    unit <- "patient"
    p <- gee_group_p(x, g, idk, kind, event, ranks = ord, each = TRUE,
                     corstr = if (isTRUE(.gr_opts$matched)) "independence")
    test <- gr_refit_label(if (kind == "proportion") "GEE (binomial, clustered by patient)"
                           else if (ord) "GEE on ranks (clustered by patient)" else "GEE (clustered by patient)", p)
  } else return(NULL)
  if (is.null(names(p))) return(NULL)            # nothing fitted
  few <- lapply(stats::setNames(glv[-1], glv[-1]), function(o) {
    k <- use & as.character(g) %in% c(glv[1], o)
    few_patient_groups(idk[k], droplevels(g[k]), unit)
  })
  list(p = p, test = test, few = few)
}

# One header line per group, tinted with the group colour, plus optional footer
# lines in black. A single column that grows DOWNWARD, so it scales to any group
# count -- unlike the two side-by-side columns the mEYEstro scatters use, which
# run out of panel width past two groups.
hdr_groups <- function(glv, pal, line_fn, foot = NULL, x = 0.01, split_at = 5,
                       title = NULL) {
  mk <- function(g) {
    gc <- unname(pal[g])
    list(seg(paste0("● ", g, ": "), gc), seg(line_fn(g), gc))
  }
  ttl <- if (!is.null(title) && nzchar(title)) list(list(seg(title))) else list()
  # past `split_at` groups a single stacked column eats a third of the figure, so
  # break it into two side-by-side columns; the footer stays under the first
  if (length(glv) > split_at) {
    h <- ceiling(length(glv) / 2)
    left <- c(ttl, lapply(glv[seq_len(h)], mk))
    # blank first line so column 2 starts BELOW the title rather than across it
    right <- c(if (length(ttl)) list(list(seg(""))) else list(),
               lapply(glv[(h + 1):length(glv)], mk))
    for (f in foot) if (nzchar(f)) left <- c(left, list(list(seg(f))))
    # a FIXED start for the second column: "auto" places it after the widest line
    # of the first, which pushes it off the right edge once the lines are long
    return(list(list(x = x, lines = left), list(x = 0.52, lines = right)))
  }
  lines <- c(ttl, lapply(glv, mk))
  for (f in foot) if (nzchar(f)) lines <- c(lines, list(list(seg(f))))
  list(list(x = x, lines = lines))
}

# The "vs reference" line grows linearly with the group count and runs off the
# panel. Break it into chunks of at most `per` comparisons per line.
wrap_vs <- function(prefix, parts, per = 3) {
  if (!length(parts)) return(character(0))
  ch <- split(parts, ceiling(seq_along(parts) / per))
  # a continuation line is indented like a wrapped header line (hdr_wrap_text):
  # spaces as wide as the prefix do not line up in a proportional font
  vapply(seq_along(ch), function(i)
    paste0(if (i == 1) prefix else "   ", paste(ch[[i]], collapse = ", "),
           if (i < length(ch)) "," else ""), character(1))
}

# "Welch ANOVA: P < 0.0001"
# "vs Reference: Cohort A P = .006, Cohort B P = .41  [Holm]"
# An omnibus that cannot run (e.g. one group has a single eye) says why ("P not
# computed: C has n = 1"), and the pairwise lines that CAN be computed are still
# shown. A comparison whose clustered GEE could not be fitted (the permutation
# test over patients ran under a GEE omnibus line) is marked "(GEE not fitted)".
# max_chars: the longest "vs" line the panel has room for; longer lines are
# re-wrapped with fewer comparisons per line (the narrow horizontal panels).
stat_footer <- function(v, gl, glv, kind = "continuous", event = NULL, max_chars = Inf) {
  om <- p_omnibus(v, gl, glv, kind, event)
  out <- if (nzchar(om$text)) paste0(om$test, ": ", om$text) else p_not_tested(om$reason)
  out <- out[nzchar(out)]
  few <- om$few
  if (length(glv) > 2) {
    pv <- p_vs_reference(v, gl, glv, kind, event)
    few <- gr_few_merge(few, attr(pv, "few"))
    fb <- attr(pv, "fallback") %||% character(0)
    ok <- is.finite(pv)
    if (any(ok)) {
      # Holm across this panel's comparisons; a single testable comparison is
      # printed as is and carries no tag
      holm <- sum(ok) >= 2
      if (holm) pv[ok] <- stats::p.adjust(pv[ok], method = "holm")
      # a comparison that could not be tested is shown as such, not dropped
      vs <- ifelse(ok, vapply(pv, p_text, character(1)), "n/a")
      vs <- ifelse(names(pv) %in% fb, paste0(vs, " (GEE not fitted)"), vs)
      # 5+ groups: three comparisons per line overflow the narrow horizontal
      # panels (I/J) in a 4-across composite row, so wrap a line earlier
      per <- if (length(glv) >= 5) 2 else 3
      repeat {
        vl <- wrap_vs(paste0("vs ", glv[1], ": "), paste(names(pv), vs, sep = " "), per = per)
        if (holm) vl[length(vl)] <- paste0(vl[length(vl)], "  [Holm]")
        if (per == 1 || max(nchar(vl)) <= max_chars) break
        per <- per - 1
      }
      out <- c(out, vl)
    }
  }
  gr_with_few(out, few)
}

# Header markers: "●" (a group, as its points) and "◆" (its centroid, as the
# filled diamond on the plot) are DRAWN as plotted symbols wherever they occur
# in a header text, never as font glyphs: the PDF font (Arial through cairo) has
# no "◆", and a symbol looks the same in PNG, TIFF, JPEG and PDF.
HDR_MARKS <- c("●" = 19L, "◆" = 23L)            # glyph -> pch
hdr_mark_w <- function(fs) 0.72 * fs                       # advance (pt) of one marker

# a header text split into runs of plain text and single markers, in order
hdr_pieces <- function(txt) {
  m <- gregexpr("[●◆]", txt)[[1]]
  if (m[1] < 0) return(txt)
  out <- character(0); st <- 1L
  for (i in m) {
    if (i > st) out <- c(out, substr(txt, st, i - 1L))
    out <- c(out, substr(txt, i, i)); st <- i + 1L
  }
  if (st <= nchar(txt)) out <- c(out, substr(txt, st, nchar(txt)))
  out
}

# rendered width (pt) of a header text at font size fs on the current device,
# markers counted at their drawn advance
hdr_text_pt <- function(txt, fs = hdr_fs()) {
  sum(vapply(hdr_pieces(txt), function(p)
    if (!is.na(HDR_MARKS[p])) hdr_mark_w(fs)
    else grid::convertWidth(grid::grobWidth(grid::textGrob(p,
      gp = grid::gpar(fontface = "bold", fontsize = fs))), "pt", valueOnly = TRUE), numeric(1)))
}

# draw one line of segments starting at unit x0; return the grobs and the
# line's right-edge unit (so the next column can be positioned after it).
# A segment with $sw draws a small filled swatch (legend key) before its text.
header_line_grob <- function(segs, y_pt, x0, fs = hdr_fs()) {
  gl <- grid::gList(); xpos <- x0
  sw <- 8 * fs / MHDR_FS                       # the swatch scales with the text
  for (s in segs) {
    if (!is.null(s$sw)) {
      gl <- grid::gList(gl, grid::rectGrob(x = xpos, y = grid::unit(y_pt + sw / 8, "pt"),
        width = grid::unit(sw, "pt"), height = grid::unit(sw, "pt"), just = c("left", "bottom"),
        gp = grid::gpar(fill = s$sw, col = "grey20", lwd = .5)))
      xpos <- xpos + grid::unit(sw * 1.25, "pt")
    }
    for (pc in hdr_pieces(s$text)) {
      pch <- HDR_MARKS[pc]
      if (!is.na(pch)) {
        # centred on the x-height, as the glyph was. The dot is a filled circle
        # (a small pch 19 renders as a diamond at preview resolution); the
        # diamond is outlined in grey like the centroid marker it stands for
        mw <- hdr_mark_w(fs)
        mx <- xpos + grid::unit(mw * 0.45, "pt"); my <- grid::unit(y_pt + 0.34 * fs, "pt")
        gl <- grid::gList(gl, if (pch == 23L)
          grid::pointsGrob(x = mx, y = my, pch = 23L, size = grid::unit(0.6 * fs, "pt"),
                           gp = grid::gpar(col = "grey10", fill = s$col, lwd = .6))
          else grid::circleGrob(x = mx, y = my, r = grid::unit(0.31 * fs, "pt"),
                                gp = grid::gpar(col = NA, fill = s$col)))
        xpos <- xpos + grid::unit(mw, "pt")
        next
      }
      tg <- grid::textGrob(pc, x = xpos, y = grid::unit(y_pt, "pt"),
                           just = c("left", "bottom"),
                           gp = grid::gpar(col = s$col, fontface = "bold", fontsize = fs))
      gl <- grid::gList(gl, tg)
      xpos <- xpos + grid::grobWidth(tg)
    }
  }
  list(grobs = gl, right = xpos)
}

# scales: optional list (one numeric vector per column, one value per line) of
# font-size factors, see hdr_fit_scales
build_header_children <- function(spec, scales = NULL, fs = hdr_fs(), ls = hdr_ls()) {
  maxL <- max(vapply(spec, function(c) length(c$lines), integer(1)))
  ytop <- 3 * ls / MHDR_LS + (maxL - 1) * ls
  gl <- grid::gList()
  prev_right <- grid::unit(0, "npc")
  for (k in seq_along(spec)) {
    col <- spec[[k]]
    # x = "auto" places this column just after the previous column's widest line
    x0 <- if (identical(col$x, "auto")) prev_right + grid::unit(12 * fs / MHDR_FS, "pt")
          else grid::unit(col$x, "npc")
    col_right <- x0
    for (j in seq_along(col$lines)) {
      f <- if (is.null(scales)) 1 else scales[[k]][j]
      lg <- header_line_grob(col$lines[[j]], ytop - (j - 1) * ls, x0, fs = fs * f)
      gl <- grid::gList(gl, lg$grobs)
      col_right <- grid::unit.pmax(col_right, lg$right)
    }
    prev_right <- col_right
  }
  gl
}

# fit = TRUE: the header measures its lines when it is drawn, in the space it
# actually gets, and draws any line that would still run past the frame in a
# smaller font (see hdr_fit_scales). Wrapping happens earlier, at build time
# (hdr_fit_spec), because the extra lines need margin reserved in the layout.
build_header_grob <- function(spec, fit = FALSE, slack = 4) {
  # the font size and line skip of the panel being built, kept for draw time
  if (isTRUE(fit))
    return(grid::gTree(spec = spec, slack = slack, fs = hdr_fs(), ls = hdr_ls(), cl = "mheader_fit"))
  grid::gTree(children = build_header_children(spec))
}

# convert a ggplot to a gtable with the colored header drawn in its top margin;
# span "panel": the header runs across the plotting panel (from the y axis),
# "all": across the whole figure (a horizontal bar panel, whose category
# labels take the left of the figure, or a square scatter narrower than it)
# own_row: the header in a row of its own at the very top, above a key placed
# on top of the plot (a panel built at a reduced text scale, R2-2); otherwise
# it is drawn in the plot's top margin, which the caller reserves for it
attach_header <- function(p, spec, fit = FALSE, span = "panel", own_row = FALSE) {
  gt <- ggplot2::ggplotGrob(p)
  if (isTRUE(own_row))
    gt <- gtable::gtable_add_rows(gt, grid::unit(gr_sp(8) + hdr_ls() * hdr_nlines(spec), "pt"), pos = 0)
  pan <- gt$layout[gt$layout$name == "panel", ]
  whole <- identical(span, "all")
  if (isTRUE(own_row))
    return(gr_widen_legend(gtable::gtable_add_grob(gt, build_header_grob(spec, fit = fit), t = 1, b = 1,
                                                   l = if (whole) 1 else min(pan$l), r = if (whole) ncol(gt) else max(pan$r),
                                                   clip = "off", name = "mheader", z = Inf)))
  gr_widen_legend(gtable::gtable_add_grob(gt, build_header_grob(spec, fit = fit), t = 1, b = min(pan$t) - 1,
                                          l = if (whole) 1 else min(pan$l), r = if (whole) ncol(gt) else max(pan$r),
                                          clip = "off", name = "mheader", z = Inf))
}

# A plot that keeps its aspect ratio (coord_fixed: its gtable is "respected",
# so in a cell wider than the square plot it is drawn narrower than the cell
# and centred) with its header in a row of an OUTER table that spans the
# cell's full width - the width the header lines were wrapped to - rather than
# across the plot's own columns, which are narrower than that (finding F1: the
# statistics lines above the scatter panels of a combined figure ran past their
# cell). h_pt: the header row's height (pt). gr_need_h counts the inner plot's
# fixed rows too.
gr_header_over <- function(p, spec, h_pt) {
  gt <- ggplot2::ggplotGrob(p)
  outer <- gtable::gtable(widths = grid::unit(1, "null"),
                          heights = grid::unit.c(grid::unit(h_pt, "pt"), grid::unit(1, "null")))
  outer <- gtable::gtable_add_grob(outer, gt, t = 2, l = 1, clip = "off", name = "figure")
  gtable::gtable_add_grob(outer, build_header_grob(spec, fit = TRUE), t = 1, l = 1, clip = "off",
                          name = "mheader", z = Inf)
}

# A group key above or below the plot is centred on the WHOLE figure, not on
# the plotting panel: ggplot centres it on the panel, which the axis title and
# the rotated stat columns push to the right, so a key that fits the figure
# (gr_fit_legend) still ran past its right edge (decisions D20, D21).
gr_widen_legend <- function(gt) {
  k <- gt$layout$name %in% c("guide-box-top", "guide-box-bottom")
  if (any(k)) { gt$layout$l[k] <- 1L; gt$layout$r[k] <- ncol(gt) }
  gt
}

# ------------------------------------------------------------
# Header fitting (double-angle and semicircle plots). A header line must end
# inside the frame. Every line is measured as rendered (bold, MHDR_FS); one
# that is too wide is broken at its separators - " · " first, then ", ", then a
# run of spaces - into as many lines as it needs, continuation lines indented
# and in the same colour. A line that cannot be broken (no separator, mixed
# colours, a legend swatch) is drawn in a smaller font instead, at draw time.
# ------------------------------------------------------------
HDR_FIT_SEPS <- c("\\s+\u00b7\\s+", ",\\s+", "(?<=\\S)\\s{2,}(?=\\S)")
# the double-angle / semicircle headers (hdr_fit_spec) break last after a
# colon: "Centroid comparison: P not computed: ..." has no other break, and at
# a journal column width it is wider than the frame
HDR_POLAR_SEPS <- c(HDR_FIT_SEPS, ":\\s+")
HDR_FIT_MIN <- 0.6     # smallest font factor a too-wide line is drawn at
# ... in the journal style never below GR_MIN_TEXT_PT (decision D20)
gr_fit_min <- function(fs = hdr_fs()) {
  # a header at a reduced text scale (R2-2) is already at or near 6 pt: no
  # smaller in any style
  lo <- if (isTRUE(.gr_opts$journal) || fs < MHDR_FS) GR_MIN_TEXT_PT / fs else 0
  min(1, max(HDR_FIT_MIN, lo))
}

# rendered width (pt) of one header line at font size fs, on the current device
hdr_line_pt <- function(segs, fs = hdr_fs()) {
  sum(vapply(segs, function(s) (if (!is.null(s$sw)) 10 * fs / MHDR_FS else 0) + hdr_text_pt(s$text, fs),
             numeric(1)))
}

# break `txt` at the first separator kind that occurs in it, filling each line
# greedily; a piece still too wide is broken at the next kind
hdr_wrap_text <- function(txt, avail, wfun, indent = "   ", seps = HDR_FIT_SEPS) {
  if (wfun(txt) <= avail || !length(seps)) return(txt)
  m <- gregexpr(seps[1], txt, perl = TRUE)[[1]]
  if (m[1] < 0) return(hdr_wrap_text(txt, avail, wfun, indent, seps[-1]))
  ml <- attr(m, "match.length")
  pieces <- substring(txt, c(1, m + ml), c(m - 1, nchar(txt)))
  sepv <- substring(txt, m, m + ml - 1)
  out <- character(0); cur <- pieces[1]
  for (i in seq_along(pieces)[-1]) {
    cand <- paste0(cur, sepv[i - 1], pieces[i])
    if (wfun(cand) <= avail) { cur <- cand; next }
    # a list keeps its comma (a label its colon) at the end of the broken
    # line; a " · " is dropped
    out <- c(out, paste0(cur, if (startsWith(sepv[i - 1], ",")) ","
                              else if (startsWith(sepv[i - 1], ":")) ":" else ""))
    cur <- paste0(indent, pieces[i])
  }
  out <- c(out, cur)
  unlist(lapply(out, function(l) hdr_wrap_text(l, avail, wfun, indent, seps[-1])))
}

# one header line -> one or more lines no wider than `avail` pt where possible
hdr_wrap_line <- function(line, avail, seps = HDR_FIT_SEPS) {
  if (hdr_line_pt(line) <= avail) return(list(line))
  cols <- unique(vapply(line, function(s) as.character(s$col), character(1)))
  has_sw <- any(vapply(line, function(s) !is.null(s$sw), logical(1)))
  # a line in several colours ("Label: <v1> vs <v2>") breaks after its
  # leading label, the coloured values going to an indented line of their own
  if (length(cols) != 1 && !has_sw && length(line) > 2 && identical(line[[1]]$col, "black"))
    return(c(hdr_wrap_line(line[1], avail, seps),
             list(c(list(seg("   ")), line[-1]))))
  if (length(cols) != 1 || has_sw)
    return(list(line))
  txt <- paste(vapply(line, function(s) s$text, character(1)), collapse = "")
  lapply(hdr_wrap_text(txt, avail, function(t) hdr_line_pt(list(seg(t))), seps = seps),
         function(t) list(seg(t, cols)))
}

# room (pt) for line `j` of column `k` in a header `width_pt` wide: a column
# with a fixed start runs to the next fixed column (less a gap) on the rows that
# column occupies, and below them to the right edge plus `slack` (the plot
# margin up to the frame); "auto" columns are not fitted (NA)
hdr_line_avail <- function(spec, k, j, width_pt, slack = 4) {
  x <- spec[[k]]$x
  if (!is.numeric(x)) return(NA_real_)
  if (k < length(spec)) {
    nx <- spec[[k + 1]]$x
    if (is.numeric(nx) && nx > x && j <= length(spec[[k + 1]]$lines))
      return((nx - x) * width_pt - 8)
  }
  (1 - x) * width_pt + slack
}

# wrap every line to its room; right-most column first, so a column to its left
# knows how many rows it has to share with it
hdr_fit_spec <- function(spec, width_pt, slack = 4) {
  for (k in rev(seq_along(spec))) {
    out <- list()
    for (l in spec[[k]]$lines) {
      av <- hdr_line_avail(spec, k, length(out) + 1, width_pt, slack)
      out <- c(out, if (is.na(av)) list(l) else hdr_wrap_line(l, av, HDR_POLAR_SEPS))
    }
    spec[[k]]$lines <- out
  }
  spec
}

# font factor per line so that every line fits its room (1 = as designed);
# fs: the header's font size
hdr_fit_scales <- function(spec, width_pt, slack = 4, fs = hdr_fs()) {
  lapply(seq_along(spec), function(k) vapply(seq_along(spec[[k]]$lines), function(j) {
    av <- hdr_line_avail(spec, k, j, width_pt, slack)
    if (is.na(av)) return(1)
    w <- hdr_line_pt(spec[[k]]$lines[[j]], fs)
    if (w <= av || w <= 0) 1 else max(gr_fit_min(fs), av / w)
  }, numeric(1)))
}

# drawn at the font size and line skip the header was built with
makeContent.mheader_fit <- function(x) {
  w <- grid::convertWidth(grid::unit(1, "npc"), "pt", valueOnly = TRUE)
  fs <- x$fs %||% MHDR_FS; ls <- x$ls %||% MHDR_LS
  grid::setChildren(x, build_header_children(x$spec, hdr_fit_scales(x$spec, w, x$slack, fs), fs, ls))
}
# registered with grid so the method is found however this file was sourced
registerS3method("makeContent", "mheader_fit", makeContent.mheader_fit,
                 envir = asNamespace("grid"))

# Run fun() with a measuring device open: the device family the PNG / TIFF / JPEG
# writers use, `w` x `h` inches, on a temporary file. The previous current
# device is restored afterwards. Measured at GR_MEASURE_DPI: at 100 dpi the
# hinted 6-pt bold text of a figure at a journal width measured up to 2%
# narrower than in the 300-dpi file, and a fitted line ran past the frame.
GR_MEASURE_DPI <- 300
gr_with_measure_device <- function(w, h, fun) {
  old <- grDevices::dev.cur()
  tf <- tempfile(fileext = ".png")
  ok <- tryCatch({ grDevices::png(tf, width = w, height = h, units = "in", res = GR_MEASURE_DPI,
                                  bg = "white"); TRUE },
                 error = function(e) FALSE)
  if (!ok) grDevices::pdf(NULL, width = w, height = h)
  dev <- grDevices::dev.cur()
  on.exit({
    if (dev %in% grDevices::dev.list()) grDevices::dev.off(dev)
    if (old > 1 && old %in% grDevices::dev.list()) grDevices::dev.set(old)
    unlink(tf)
  }, add = TRUE)
  fun()
}

# Finish a framed polar plot (double-angle, semicircle) that is always drawn at
# fig_w x fig_h inches (its own file and its composite cell are that size):
#  1. wrap the header lines to the width the panel will have;
#  2. reserve top margin for the header as wrapped;
#  3. when the panel is height-limited (coord_fixed), widen the x range so the
#     panel - and with it the frame and the header's room - fills the width,
#     instead of the header running past a frame that hugs the circle;
#  4. never let the header squeeze the diagram: at a narrow width (a journal
#     column, figure_width_cm) the header wraps to many more lines, so when
#     less than GR_POLAR_MIN_PLOT_IN (at most 90% of the panel width) is left
#     for the diagram the figure is made taller instead. The height laid out
#     is returned as attr "fig_h" (fig_h, or more): the caller saves the file
#     and sizes the composite row at it.
# Any line still too wide is drawn smaller at draw time (fit = TRUE).
GR_POLAR_MIN_PLOT_IN <- 3.0     # the least width (inches) of the drawn diagram
gr_polar_finish <- function(p, hs, xlim, ylim, fig_w, fig_h) {
  build <- function(spec, xl) p +
    ggplot2::coord_fixed(xlim = xl, ylim = ylim, clip = "off") +
    ggplot2::theme(plot.margin = ggplot2::margin(t = gr_sp(10) + hdr_ls() * hdr_nlines(spec),
                                                 r = gr_sp(8), b = gr_sp(6), l = gr_sp(8)))
  fixed_pt <- function(u, conv) {
    nn <- grid::unitType(u) != "null"
    if (!any(nn)) 0 else sum(conv(u[nn], "pt", valueOnly = TRUE))
  }
  fitted <- tryCatch(gr_with_measure_device(fig_w, fig_h, function() {
    gt <- ggplot2::ggplotGrob(build(hs, xlim))
    pw <- fig_w * 72.27 - fixed_pt(gt$widths, grid::convertWidth)
    hs2 <- hdr_fit_spec(hs, pw)
    gt <- ggplot2::ggplotGrob(build(hs2, xlim))
    fh <- fixed_pt(gt$heights, grid::convertHeight)
    ph <- fig_h * 72.27 - fh
    # the diagram's least width, and the panel height that gives it
    # (at a reduced text scale, R2-2, the diagram may be as much smaller)
    min_ph <- min(0.9 * pw, GR_POLAR_MIN_PLOT_IN * gr_ts() * 72.27) * diff(ylim) / diff(xlim)
    h_out <- fig_h
    if (pw > 0 && ph < min_ph) { ph <- min_ph; h_out <- (fh + min_ph) / 72.27 }
    xl <- xlim
    if (ph > 0 && pw / ph > diff(xlim) / diff(ylim))
      xl <- mean(xlim) + c(-0.5, 0.5) * diff(ylim) * pw / ph
    list(hs = hs2, xlim = xl, fig_h = h_out)
  }), error = function(e) list(hs = hs, xlim = xlim, fig_h = fig_h))
  out <- attach_header(build(fitted$hs, fitted$xlim), fitted$hs, fit = TRUE)
  attr(out, "fig_h") <- fitted$fig_h
  out
}
hdr_nlines <- function(spec) if (is.null(spec)) 0 else
  max(vapply(spec, function(c) length(c$lines), integer(1)))

# rotated (90°) inline-colored stat line, centered in its cell (bottom-to-top)
rot_line_grob <- function(segs) {
  tgs <- lapply(segs, function(s) grid::textGrob(s$text, rot = 90, just = c("centre", "centre"),
    gp = grid::gpar(col = s$col, fontface = "bold", fontsize = hdr_fs())))
  hts <- lapply(tgs, grid::grobHeight)
  totalH <- Reduce(`+`, hts)
  gl <- grid::gList(); yrun <- grid::unit(0.5, "npc") - 0.5 * totalH
  for (i in seq_along(tgs)) {
    gl <- grid::gList(gl, grid::editGrob(tgs[[i]], x = grid::unit(0.5, "npc"),
                                         y = yrun + 0.5 * hts[[i]]))
    yrun <- yrun + hts[[i]]
  }
  grid::gTree(children = gl)
}

# two stacked full-width colored "N = ..." boxes (bottom of horizontal panels)
nbox_stack_grob <- function(nbox) {
  n <- length(nbox); gl <- grid::gList()
  for (i in seq_len(n)) {
    yc <- 1 - (i - 0.5) / n
    gl <- grid::gList(gl,
      grid::rectGrob(y = grid::unit(yc, "npc"), height = grid::unit(0.82 / n, "npc"),
                     width = grid::unit(0.99, "npc"),
                     gp = grid::gpar(fill = "white", col = unname(nbox[i]), lwd = 1)),
      grid::textGrob(names(nbox)[i], y = grid::unit(yc, "npc"),
                     gp = grid::gpar(col = unname(nbox[i]), fontface = "bold", fontsize = hdr_fs())))
  }
  grid::gTree(children = gl)
}

# horizontal (coord_flip) panel: rotated colored stat on the left (replaces the
# category-axis title), rotated P on the right, colored N-boxes at the bottom.
# caution: the few-patients caution of that P value, as lines already wrapped
# to the figure's width (gr_hbar_caution), drawn under the N-boxes across the
# whole figure, left-aligned (contract R4-6: the rotated panels carry it too).
GR_HBAR_CAU_PAD <- 6     # pt between the figure's left edge and the caution
# left_segs: one line (a list of segments) or several lines (a list of such
# lists), each drawn rotated in a column of its own, the first on the left.
attach_hbar <- function(p, left_segs, right_text, nbox, caution = character(0)) {
  gt <- ggplot2::ggplotGrob(p)
  pan <- gt$layout[gt$layout$name == "panel", ]
  left_lines <- if (length(left_segs) && is.null(left_segs[[1]]$text)) left_segs else list(left_segs)
  for (ln in rev(left_lines)) {
    gt <- gtable::gtable_add_cols(gt, gr_lines(1.5), pos = 0)
    gt <- gtable::gtable_add_grob(gt, rot_line_grob(ln), t = pan$t, b = pan$b,
                                  l = 1, r = 1, clip = "off", name = "hstat", z = Inf)
  }
  pan <- gt$layout[gt$layout$name == "panel", ]
  gt <- gtable::gtable_add_cols(gt, gr_lines(1.3), pos = ncol(gt))
  gt <- gtable::gtable_add_grob(gt, grid::textGrob(right_text, rot = 90,
    gp = grid::gpar(fontface = "bold", fontsize = hdr_fs())),
    t = pan$t, b = pan$b, l = ncol(gt), r = ncol(gt), clip = "off", name = "pstat", z = Inf)
  if (!is.null(nbox) && length(nbox)) {
    pan <- gt$layout[gt$layout$name == "panel", ]
    gt <- gtable::gtable_add_rows(gt, gr_lines(2.1), pos = nrow(gt))
    # under the panel; across the whole figure when the panel is narrow (a
    # journal column width), so the "N = ... of ... eyes" text stays in its box
    wide <- gr_narrow()
    gt <- gtable::gtable_add_grob(gt, nbox_stack_grob(nbox), t = nrow(gt), b = nrow(gt),
                                  l = if (wide) 1 else pan$l, r = if (wide) ncol(gt) else pan$r,
                                  clip = "off", name = "nbox", z = Inf)
  }
  caution <- as.character(caution)
  caution <- caution[!is.na(caution) & nzchar(caution)]
  if (length(caution)) {
    nl <- length(caution)
    gt <- gtable::gtable_add_rows(gt, grid::unit(gr_sp(5) + hdr_ls() * nl, "pt"), pos = nrow(gt))
    gt <- gtable::gtable_add_grob(gt, grid::textGrob(caution,
      x = grid::unit(GR_HBAR_CAU_PAD, "pt"),
      y = grid::unit(1, "npc") - grid::unit(gr_sp(3) + hdr_ls() * (seq_len(nl) - 1), "pt"),
      just = c("left", "top"), gp = grid::gpar(fontface = "bold", fontsize = hdr_fs())),
      t = nrow(gt), b = nrow(gt), l = 1, r = ncol(gt), clip = "off", name = "hcaution", z = Inf)
  }
  gr_widen_legend(gt)
}

# The caution of a rotated two-group panel as lines no wider than the figure
# (less GR_HBAR_CAU_PAD on each side) at the narrowest width the panel is drawn
# at (.gr_opts$cell_w), broken like the other stat lines (GR_BAR_SEPS), so it is
# never cut off. Without a measuring device the lines are broken by an estimate
# of the character width instead.
gr_hbar_caution <- function(caution, fig_w = .gr_opts$cell_w %||% GR_BAR_MIN_W) {
  caution <- as.character(caution %||% "")[1]
  if (is.na(caution) || !nzchar(caution)) return(character(0))
  avail <- fig_w * 72.27 - 2 * GR_HBAR_CAU_PAD
  out <- tryCatch(gr_with_measure_device(fig_w, 4.9, function()
           hdr_wrap_text(caution, avail, function(t) hdr_text_pt(t, hdr_fs()), seps = GR_BAR_SEPS)),
         error = function(e) NULL)
  if (is.null(out) || !length(out))
    out <- strwrap(caution, width = max(20L, floor(avail / (0.6 * hdr_fs()))), exdent = 3)
  out
}

# ------------------------------------------------------------
# Grouped bar chart core: %-of-eyes bars per group with labels,
# colored stat-annotation lines at the top (mEYEstro layout).
# dfb: label / pct / series (ordered); ann: list(text=, col=)
# ------------------------------------------------------------
# The narrowest a bar panel is ever drawn (inches): a cell of a four-across row
# of the composite (compose_panels: three 5.6-inch columns shared by four).
GR_BAR_MIN_W <- 5.6 * 3 / 4
# Below this width (inches) a bar panel's side-by-side header columns are
# stacked into one column, and a scatter's per-group header columns into one
# line per group (gr_bars, scatter_ind)
GR_HDR_STACK_W <- 4.0
# TRUE when a panel `w` inches wide is that narrow for its text: the width is
# compared at the panel's text scale (a panel built at a reduced scale, R2-2,
# has smaller text and room for more)
gr_narrow <- function(w = .gr_opts$cell_w %||% GR_BAR_MIN_W) w < GR_HDR_STACK_W * hdr_fs() / MHDR_FS

# fun(<width of the plotting panel in pt>) for ggplot `p` drawn `w` inches wide,
# run on a measuring device like the polar headers (gr_with_measure_device), so
# fun() can measure text (hdr_text_pt); NULL when nothing could be measured.
gr_on_panel <- function(p, w, fun, h = 4.9) {
  tryCatch(gr_with_measure_device(w, h, function() {
    gt <- ggplot2::ggplotGrob(p)
    nn <- grid::unitType(gt$widths) != "null"
    fixed <- if (any(nn)) sum(grid::convertWidth(gt$widths[nn], "pt", valueOnly = TRUE)) else 0
    fun(w * 72.27 - fixed)
  }), error = function(e) NULL)
}

# The plotting panel of figure `g` (a ggplot, or a figure already assembled
# as a gtable with its header, key and N-box rows) drawn `w` x `h` inches:
# list(w =, h =) in pt, measured on a measuring device - the figure's size
# less every fixed row and column, shared out among the "null" ones by their
# weights (a bar panel has one); NULL when it cannot be measured.
gr_panel_size <- function(g, w, h) {
  out <- tryCatch(gr_with_measure_device(w, h, function() {
    gt <- if (inherits(g, "ggplot")) ggplot2::ggplotGrob(g) else g
    if (!inherits(gt, "gtable")) return(NULL)
    pan <- gt$layout[gt$layout$name == "panel", , drop = FALSE]
    if (nrow(pan) != 1) return(NULL)
    share <- function(u, total, conv, i) {
      nn <- grid::unitType(u) == "null"
      fixed <- if (any(!nn)) sum(conv(u[!nn], "pt", valueOnly = TRUE)) else 0
      wt <- ifelse(nn, as.numeric(u), 0)
      if (sum(wt) <= 0) return(NA_real_)
      (total - fixed) * sum(wt[i]) / sum(wt)
    }
    list(w = share(gt$widths, w * 72.27, grid::convertWidth, pan$l:pan$r),
         h = share(gt$heights, h * 72.27, grid::convertHeight, pan$t:pan$b))
  }), error = function(e) NULL)
  if (is.list(out) && is.finite(out$w) && is.finite(out$h) && out$w > 0 && out$h > 0) out else NULL
}

# Where a bar panel's single-colour stat line (a P value, "P not computed:
# <reason>", the caution) may break when it is too wide: the header separators
# first, then after " -", then after a colon, then at any space.
GR_BAR_SEPS <- c(HDR_FIT_SEPS, "(?<=\\s-)\\s+", "(?<=:)\\s+", "\\s+")
GR_BAR_SLACK <- 4      # pt a header line may run into the plot's right margin

# The bar panel's inline-coloured header with every single-colour line wrapped
# to the room it has in a plotting panel `pw` pt wide: a column with a fixed x
# runs to the next fixed column (hdr_line_avail), an "auto" column starts 12 pt
# after the previous column's widest line (as build_header_children draws it)
# and runs to the right edge. Lines in several colours or with a swatch are kept.
gr_fit_bar_header <- function(spec, pw) {
  prev_right <- 0
  for (k in seq_along(spec)) {
    x_pt <- if (is.numeric(spec[[k]]$x)) spec[[k]]$x * pw else prev_right + 12
    out <- list()
    for (l in spec[[k]]$lines) {
      av <- hdr_line_avail(spec, k, length(out) + 1, pw, GR_BAR_SLACK)
      if (is.na(av)) av <- pw - x_pt + GR_BAR_SLACK
      out <- c(out, hdr_wrap_line(l, av, GR_BAR_SEPS))
    }
    spec[[k]]$lines <- out
    prev_right <- x_pt + max(0, vapply(out, hdr_line_pt, numeric(1)))
  }
  spec
}

# The group key of bar panel `p` (ns entries, first laid out in ncol0 columns)
# measured on a measuring device at the width w_in (inches) the panel is drawn
# at: with fewer columns, down to one, until it fits the figure (inside = the
# key over the bars: the plotting panel's width, about three quarters of it); a
# key that had to be narrowed is left-aligned. As built when nothing can be
# measured.
gr_fit_legend <- function(p, ns, w_in, inside = FALSE, ncol0 = ns) {
  if (ns < 1) return(p)
  avail <- w_in * 72.27 * (if (inside) 0.75 else 1) - 8
  key_w <- function(q) gr_with_measure_device(w_in, 4.9, function() {
    gt <- ggplot2::ggplotGrob(q)
    k <- grep("^guide-box", gt$layout$name)
    if (!length(k)) return(0)
    max(0, vapply(gt$grobs[k], function(g) if (inherits(g, "zeroGrob")) 0
                  else grid::convertWidth(grid::grobWidth(g), "pt", valueOnly = TRUE), numeric(1)))
  })
  left <- if (inside) NULL else ggplot2::theme(legend.justification = "left")
  q <- p
  for (k in seq(max(1L, as.integer(ncol0)), 1L)) {
    q <- p + ggplot2::guides(fill = ggplot2::guide_legend(ncol = k, byrow = TRUE))
    w <- tryCatch(key_w(q), error = function(e) NA_real_)
    if (!is.finite(w)) return(p)
    if (w <= avail) return(if (k < ncol0 && !is.null(left)) q + left else q)
  }
  if (is.null(left)) q else q + left
}

# Every stat line of a bar panel ends inside the narrowest width the panel is
# drawn at (.gr_opts$cell_w, set by generate_standard_graphs from the planned
# composite; GR_BAR_MIN_W otherwise), so its composite cell and its own file
# both show it complete: a line that would run past the panel's right edge is
# wrapped onto further lines (GR_BAR_SEPS), never cut off.
# caution: the few-patients caution of the panel's P values (gr_caution), drawn
# as the last stat line, wrapped like the others; on a rotated two-group
# horizontal panel (Correction Index, Angle of Error) under its N-boxes, wrapped
# to the figure (attach_hbar). It is in the run log either way
# (few_patients_flush).
gr_bars <- function(dfb, pal, xlab, ylab, title, ann = list(), horizontal = FALSE,
                    label_size = 2.6, box_left = .gr_opts$box_left,
                    box_right = .gr_opts$box_right,
                    label_angle = .gr_opts$label_angle, x_angle = 0, nbox = NULL,
                    header_spec = NULL, legend_pos = "inside",
                    hbar_left = NULL, hbar_right = NULL, caution = "", short_spec = NULL) {
  # StandardEyes panels leave the corner boxes NULL, which selects the mEYEstro
  # look the builders reproduce: OPEN (no full box) + N-boxes off the right edge.
  is_meyestro <- is.null(box_left) && is.null(box_right)
  key_top <- FALSE                              # the key above the plot (R2-2)
  meyestro_v <- is_meyestro && !horizontal      # vertical mEYEstro bar panel
  # The N-boxes are rotated labels stacked down the right edge; past four groups
  # they overprint each other into an unreadable smear. Drop the boxes and fold
  # the counts into the stat lines instead, which stack and wrap cleanly.
  # (only reachable with >4 superimposed groups)
  if (!horizontal && !is.null(nbox) && length(nbox) > 4) {
    cnt <- if (!is.null(attr(nbox, "n"))) as.character(attr(nbox, "n"))
           else sub("^N = ([0-9]+).*$", "\\1", names(nbox))
    ann <- c(ann, list(list(col = "black", role = "n",
      text = paste0("n per group, in the order above: ", paste(cnt, collapse = ", ")))))
    nbox <- NULL
  }
  # Journal style, the correction-index and angle-of-error histograms (decision
  # R2-3): no title in the panel (the axis title names the measure) and their
  # statistics lines in a grid header above the plot area, like every other
  # panel's - never over the bars; the two-group form's rotated side lines
  # become header lines too. The header runs across the whole figure, since
  # the category labels take its left part.
  jh <- horizontal && is_meyestro && isTRUE(.gr_opts$journal)
  if (jh && !is.null(hbar_left)) {
    hl <- if (length(hbar_left) && is.null(hbar_left[[1]]$text)) hbar_left else list(hbar_left)
    pl <- if (length(hbar_right) && nzchar(hbar_right[1])) list(list(seg(hbar_right[1]))) else list()
    header_spec <- list(list(x = 0.01, lines = c(hl, pl)))
    # short form (R2-2): the first line and the test line
    if (is.null(short_spec) && length(hl) > 1) short_spec <- list(list(x = 0.01, lines = c(hl[1], pl)))
    hbar_left <- NULL
  }
  # A combined figure at a journal width asks for the short form of the stat
  # lines when the full form does not fit its row (decision R2-2): the first
  # line per cohort and the test lines stay, the rest goes to the figure's
  # legend file (gr_moved). With 3+ groups a cohort's line keeps its first
  # measure (up to the first " · "); a two-group header is replaced by its
  # builder's short form (short_spec) when it has one.
  if (isTRUE(.gr_opts$short_hdr)) {
    if (is.null(header_spec) && length(ann)) {
      keep <- list()
      for (a in ann) {
        if (!identical(a$col, "black")) {
          i <- regexpr(" · ", a$text, fixed = TRUE)
          if (i > 0) {
            lead <- sub("^([^:]*: ).*$", "\\1", a$text)
            gr_moved(paste0(if (grepl(": ", a$text, fixed = TRUE)) lead else "",
                            substring(a$text, i + 3)))
            a$text <- substr(a$text, 1, i - 1)
          }
          keep <- c(keep, list(a))
        } else if (identical(a$role, "n")) gr_moved(a$text)
        else keep <- c(keep, list(a))
      }
      ann <- keep
    } else if (!is.null(header_spec) && !is.null(short_spec)) {
      txt <- function(sp) unlist(lapply(sp, function(cl) vapply(cl$lines, gr_line_text, character(1))))
      gr_moved(setdiff(txt(header_spec), txt(short_spec)))
      header_spec <- short_spec
    }
    if (nzchar(as.character(caution %||% "")[1])) { gr_moved(caution); caution <- "" }
  }
  # Three or more groups: the stat lines (`ann`) go through the same grid
  # header as the two-group panels - one line each, spaced in points
  # (MHDR_LS) in the margin reserved above the panel - so they never overprint
  # however short the panel is drawn (decision D20); never at data-unit heights.
  if ((meyestro_v || jh) && is.null(header_spec) && length(ann))
    header_spec <- list(list(x = 0.01, lines = lapply(ann, function(a) list(seg(a$text, a$col)))))
  use_hdr <- (meyestro_v || jh) && !is.null(header_spec)  # grid inline-colored header
  use_hbar <- is_meyestro && horizontal && !is.null(hbar_left)  # rotated side stats
  if (use_hbar) xlab <- ""                       # rotated stat replaces the category title
  cell_w <- .gr_opts$cell_w %||% GR_BAR_MIN_W    # the narrowest width this panel is drawn at
  # a header of side-by-side columns needs a wide panel: in a narrow one (a
  # figure laid out at a journal column width) its columns are stacked into one
  if (use_hdr && gr_narrow(cell_w) && length(header_spec) > 1) {
    lines <- unlist(lapply(header_spec, function(cl) cl$lines), recursive = FALSE)
    header_spec <- list(list(x = 0.01, lines = lines))
  }
  dfb$label <- factor(dfb$label, levels = unique(dfb$label))
  dfb$series <- factor(dfb$series, levels = names(pal))
  ymax <- max(dfb$pct, na.rm = TRUE)
  # mEYEstro puts the stat text ABOVE the panel and the legend INSIDE, so the
  # interior only needs room for the legend (not the annotation lines).
  # (a journal-style histogram has its lines above the plot: room for the
  # value labels right of the bars only)
  # (a key above the plot at a reduced text scale, R2-2: room for the value
  # labels only - a 6-pt "100%" upright is about a quarter of the short plot)
  # (and at any size a key of more than six entries - five cohorts x preop /
  # postop are ten in five rows - which would cover the value labels and the
  # tops of the bars inside the plot, finding F2)
  key_top <- meyestro_v && use_hdr && (gr_ts() < 1 || length(unique(dfb$series)) > 6) &&
    identical(legend_pos, "inside") && !gr_narrow(cell_w)
  headroom <- if (key_top) 1.25 else if (meyestro_v) 1.30 else if (jh) 1.16 else 1 + 0.09 * (length(ann) + 1)
  # The room along the category axis, ESTIMATED before the panel is laid out
  # (for the angle of the category labels and the N boxes' spacing; the value
  # labels use it only when the laid-out panel cannot be measured): the
  # category axis spans the categories plus the room the panel adds to it (the
  # N boxes on the right of a vertical panel; the stat lines above the bars of
  # a horizontal one).
  nlev0 <- length(unique(dfb$label))
  # (the axis room and the header lines above a horizontal plot at the panel's
  # text scale, R2-2)
  n_hdr <- if (use_hdr) sum(vapply(header_spec, function(cl) length(cl$lines), integer(1))) else 0
  span_pt <- if (horizontal) max((.gr_opts$cell_h %||% 4.9) * 72 * 0.3,
                                 (.gr_opts$cell_h %||% 4.9) * 72 * 0.62 - (if (jh) hdr_ls() * n_hdr else 0))
             else (cell_w - 0.75 * gr_ts()) * 72
  n_cat <- nlev0 + if (horizontal) 0.6 + (if (is_meyestro && !use_hbar && !jh) 0.9 + 0.85 * length(ann) else 0)
                   else 0.6 + (if (!is.null(nbox) && length(nbox) && length(nbox) <= 4) 1.7 else 0)
  PT_PER_MM <- 72.27 / 25.4          # ggplot2's .pt, which is not exported
  # The value labels: "<1%" for a bar under 0.5%, none for a bar of 0%. They
  # are sized and chosen once the panel is laid out (at the end of this
  # function), from the plotting area the figure really gets.
  dfb$vlab <- ifelse(!is.finite(dfb$pct) | dfb$pct <= 0, "",
                     ifelse(round(dfb$pct) < 1, "<1%", sprintf("%.0f%%", dfb$pct)))
  show_val <- any(nzchar(dfb$vlab))
  # a long axis title is wrapped in a narrow panel instead of running off it
  if (gr_narrow(cell_w) && nchar(xlab) > 30) xlab <- paste(strwrap(xlab, 30), collapse = "\n")
  # Category labels that would run into each other at this width (a panel laid
  # out at a journal column width) are turned: 45 degrees, on one line each,
  # or upright (90) when even that is too tight for their line height.
  if (!horizontal) {
    cat_pt <- span_pt / max(1, n_cat)
    labw <- max(nchar(unlist(strsplit(levels(dfb$label), "\n", fixed = TRUE)))) * 0.52 * gr_pt(9.5)
    if (x_angle == 0 && labw > cat_pt) {
      lv <- gsub("\n", " ", levels(dfb$label), fixed = TRUE)
      dfb$label <- factor(gsub("\n", " ", as.character(dfb$label), fixed = TRUE), levels = unique(lv))
      x_angle <- 45
    }
    if (x_angle > 0 && x_angle < 90 && 11 * gr_pt(9.5) / 9.5 / sin(x_angle * pi / 180) > cat_pt) x_angle <- 90
  }
  p <- ggplot2::ggplot(dfb, ggplot2::aes(x = label, y = pct, fill = series)) +
    ggplot2::geom_col(position = ggplot2::position_dodge(width = .8), colour = "grey20",
                      width = .72, linewidth = .25)
  p <- p +
    ggplot2::scale_fill_manual(values = pal) +
    ggplot2::labs(x = xlab, y = ylab, title = if (meyestro_v || use_hbar || jh) NULL else title) + gr_theme()
  if (x_angle != 0)
    p <- p + ggplot2::theme(axis.text.x = ggplot2::element_text(
      angle = x_angle, hjust = 1, vjust = if (x_angle == 90) 0.5 else 1, face = "bold", size = gr_pt(8.5)))
  caution <- as.character(caution %||% "")[1]
  has_caution <- !is.na(caution) && nzchar(caution)
  if (is_meyestro && !use_hbar && (use_hdr || length(ann) || has_caution)) {
    # header lines are MHDR_FS pt; the annotate() stat lines 2.9 mm (2.8 mm on
    # the horizontal panels). A header line runs from its column's x to the
    # panel's right edge; an annotated line from the panel's left edge. A
    # journal-style histogram's header runs across the whole figure (jh).
    fit_w <- .gr_opts$cell_w %||% GR_BAR_MIN_W
    on_fig <- function(q, w, fun) tryCatch(gr_with_measure_device(w, 4.9, function()
      fun(w * 72.27 - gr_sp(16))), error = function(e) NULL)
    fitted <- (if (jh) on_fig else gr_on_panel)(if (horizontal) p + ggplot2::coord_flip() else p,
                                                fit_w, function(pw) {
      if (use_hdr) {
        hs <- gr_fit_bar_header(header_spec, pw)
        if (has_caution) {
          # its own column at the first column's x, below every other line: a
          # line in an existing column would push an "auto" column to its right
          x0 <- if (is.numeric(hs[[1]]$x)) hs[[1]]$x else 0
          hs <- c(hs, list(list(x = x0, lines = c(rep(list(list(seg(""))), hdr_nlines(hs)),
            hdr_wrap_line(list(seg(caution)), (1 - x0) * pw + GR_BAR_SLACK, GR_BAR_SEPS)))))
        }
        return(hs)
      }
      fs <- gr_mm(if (horizontal) 2.8 else 2.9) * 72.27 / 25.4
      a2 <- c(ann, if (has_caution) list(list(col = "black", text = caution)))
      unlist(lapply(a2, function(a) lapply(
        hdr_wrap_text(a$text, pw, function(t) hdr_text_pt(t, fs), seps = GR_BAR_SEPS),
        function(t) list(col = a$col, text = t))), recursive = FALSE)
    })
    # nothing measurable (no device): the lines as built, without the caution
    if (!is.null(fitted)) {
      if (use_hdr) header_spec <- fitted else ann <- fitted
    }
  }
  nlev <- length(levels(dfb$label))
  # (more than four N-boxes were folded into the stat lines at the top)
  has_nbox <- !horizontal && !is.null(nbox) && length(nbox)
  # mEYEstro panels are OPEN (no full box): only horizontal grid + L-shaped
  # axes, so the "N = x eyes" boxes can float just off the right of the bars.
  if (is_meyestro) {
    p <- p + ggplot2::theme(
      panel.border = ggplot2::element_blank(),
      axis.line = ggplot2::element_line(colour = "black", linewidth = .7),
      panel.grid.major.x = if (horizontal) ggplot2::element_line(colour = "grey85", linewidth = .35) else ggplot2::element_blank(),
      panel.grid.major.y = if (horizontal) ggplot2::element_blank() else ggplot2::element_line(colour = "grey85", linewidth = .35))
    # widen the plotting range on the right so the N-boxes sit fully inside
    # the canvas (no clipping) yet clearly to the right of the last bar
    # Two or more boxes stand side by side (decision D22: stacked one above
    # the other they ran into each other in a short panel), each a box height
    # (~14 pt) apart, so the room on the right grows with their number.
    nb_sp <- if (has_nbox) 14 * gr_mm(2.7) / 2.7 / max(1, span_pt / max(1, n_cat)) else 0
    if (has_nbox)
      p <- p + ggplot2::scale_x_discrete(expand = ggplot2::expansion(
        add = c(0.6, max(1.7, 0.75 + length(nbox) * nb_sp))))
    if (meyestro_v || jh) {
      # legend INSIDE the plot (upper-left) + tall top margin for the stat text.
      # top margin reserves room for the grid header (or the annotate fallback)
      nlh <- if (use_hdr) hdr_nlines(header_spec) else length(ann)
      # a narrow panel has no room for the key over the bars: below them; a
      # journal-style histogram's key goes below too (its header is above)
      if (identical(legend_pos, "inside") && (gr_narrow(cell_w) || jh)) legend_pos <- "bottom"
      # a panel at a reduced text scale (R2-2) is too short for a key over the
      # bars - it would cover the value labels: the key goes on top of the
      # plot, under the header, which gets a row of its own above it
      key_top <- key_top && identical(legend_pos, "inside")
      if (key_top) legend_pos <- "top"
      p <- p + ggplot2::theme(
        legend.text = ggplot2::element_text(size = gr_pt(8), face = "bold"),
        legend.background = ggplot2::element_rect(fill = "white", colour = "grey45", linewidth = .3),
        legend.key.size = grid::unit(gr_pt(10), "pt"),
        legend.margin = ggplot2::margin(gr_sp(1), gr_sp(4), gr_sp(1), gr_sp(4)),
        legend.box.spacing = if (key_top) grid::unit(gr_sp(4), "pt") else NULL,
        plot.margin = ggplot2::margin(t = if (key_top) gr_sp(2) else gr_sp(8) + hdr_ls() * nlh,
                                      r = gr_sp(if (has_nbox) 8 else 10), b = gr_sp(4), l = gr_sp(6)))
      if (identical(legend_pos, "inside"))
        p <- p + ggplot2::theme(legend.position = "inside",
          legend.position.inside = c(0.005, 0.99), legend.justification = c(0, 1))
      else
        p <- p + ggplot2::theme(legend.position = legend_pos)
    }
  }
  ylim_top <- ymax * headroom + 6
  # a percentage axis ends at 100 (decision D22): its breaks stop at 100 and the
  # axis line is capped at the last break; the room above it holds the value
  # labels and the legend, not a scale running to 120%
  brk <- pretty(c(0, min(100, ylim_top)), n = 5)
  brk <- brk[brk >= 0 & brk <= min(100, ylim_top)]
  p <- p + ggplot2::scale_y_continuous(breaks = brk, guide = ggplot2::guide_axis(cap = "upper"))
  if (horizontal) {
    # the horizontal panels need room (and unclipped drawing) above the top
    # category for the stat lines
    cmp_h <- is_meyestro && !use_hbar && !jh && length(ann) > 0
    if (cmp_h)
      p <- p + ggplot2::scale_x_discrete(
        expand = ggplot2::expansion(add = c(0.6, 0.9 + 0.85 * length(ann))))
    # (a journal-style histogram: its value labels stay inside the headroom of
    # the value axis, so nothing reaches past the panel's right edge)
    p <- p + ggplot2::coord_flip(ylim = c(0, ylim_top),
                                 clip = if (cmp_h) "off" else "on")
  } else {
    p <- p + ggplot2::coord_cartesian(ylim = c(0, ylim_top), clip = "off")
  }
  # mEYEstro rotated "N = x eyes" boxes, fully OUTSIDE the bars on the right,
  # side by side (never clipped, never overlapping)
  if (has_nbox) {
    kk <- length(nbox)
    xs <- if (kk == 1) nlev + 0.98 else nlev + 0.6 + (seq_len(kk) - 0.5) * nb_sp
    for (i in seq_len(kk))
      # colour per group so the reader can tell which N is which (matches ref)
      p <- p + ggplot2::annotate("label", x = xs[i], y = 0.55 * ylim_top,
                                 label = names(nbox)[i], colour = unname(nbox[i]),
                                 fill = "white", angle = 90, size = gr_mm(2.7),
                                 fontface = "bold", linewidth = .5)
  }
  if (length(ann)) {
    n_ann <- length(ann)
    for (i in seq_along(ann)) {
      if (horizontal && (use_hbar || jh)) {
        # rotated stat/P + bottom N-boxes handled by attach_hbar; a journal
        # histogram's lines by its grid header; skip ann
      } else if (horizontal && is_meyestro) {
        # comparative horizontal panel: with one line per group the old inward
        # stack ran over the top categories, so put the lines ABOVE the bars
        p <- p + ggplot2::annotate("text", x = nlev + 0.75 + (n_ann - i) * 0.85, y = 0,
                                   label = ann[[i]]$text, colour = ann[[i]]$col,
                                   hjust = 0, vjust = 0.5, size = gr_mm(2.8), fontface = "bold")
      } else if (horizontal) {
        # after coord_flip: x = category axis (vertical), y = value axis; stack
        # the annotation lines down the category axis, right-aligned
        p <- p + ggplot2::annotate("text", x = nlev + 0.2 - (i - 1) * 0.75, y = ylim_top,
                                   label = ann[[i]]$text, colour = ann[[i]]$col,
                                   hjust = 1, vjust = 1, size = gr_mm(2.8), fontface = "bold")
      } else if (meyestro_v && !use_hdr) {
        # fallback (no header_spec): stat lines above the panel via annotate
        yy <- ylim_top * (1.05 + 0.055 * (n_ann - i))
        p <- p + ggplot2::annotate("text", x = 0.35, y = yy, label = ann[[i]]$text,
                                   colour = ann[[i]]$col, hjust = 0, vjust = 0,
                                   size = gr_mm(2.9), fontface = "bold")
      } else if (meyestro_v) {
        # header handled by the grid overlay; skip the annotate lines
      } else {
        y0 <- if (!is.null(box_left) || !is.null(box_right)) 0.915 else 0.985
        p <- p + ggplot2::annotate("text", x = 0.55, y = ylim_top * (1 - 0.055 * (i - 1)) * y0,
                                   label = ann[[i]]$text, colour = ann[[i]]$col,
                                   hjust = 0, vjust = 1, size = gr_mm(2.9), fontface = "bold")
      }
    }
  }
  # Keep the legend to at most TWO columns. With 3 groups x pre/post that is 6
  # entries; laying them 3-across overflowed the panel in a 4-panel composite row.
  ns <- length(unique(dfb$series))
  if (ns > 2)
    p <- p + ggplot2::guides(fill = ggplot2::guide_legend(
      nrow = if (ns > 4) ceiling(ns / 2) else 2, byrow = TRUE))
  # classic Reinstein info boxes at the top corners
  if (!horizontal && !is.null(box_left))
    p <- p + ggplot2::annotate("label", x = 0.55, y = ylim_top * .999, label = box_left,
                               hjust = 0, vjust = 1, size = 2.8, fontface = "bold")
  if (!horizontal && !is.null(box_right))
    p <- p + ggplot2::annotate("label", x = nlev + 0.45, y = ylim_top * .999, label = box_right,
                               hjust = 1, vjust = 1, size = 2.8, fontface = "bold")
  # the group key must fit the width the panel is drawn at (decisions D20,
  # D21): measured, and laid out in fewer columns - down to one, left-aligned -
  # until it does; on a rotated two-group panel it goes under the plot
  if (is_meyestro && !identical(legend_pos, "none")) {
    if (use_hbar && gr_narrow(cell_w)) p <- p + ggplot2::theme(legend.position = "bottom")
    p <- gr_fit_legend(p, ns, cell_w, inside = meyestro_v && identical(legend_pos, "inside"),
                       ncol0 = if (key_top) ns else if (ns > 2) 2 else ns)
  }
  # (fit = TRUE: a line that would still run past the panel at draw time is
  # drawn smaller, never below GR_MIN_TEXT_PT in the journal style)
  assemble <- function(q) {
    if (use_hdr && jh) gr_add_nbox_rows(attach_header(q, header_spec, fit = TRUE, span = "all"), nbox)
    else if (use_hdr) attach_header(q, header_spec, fit = TRUE, own_row = key_top)
    else if (use_hbar) attach_hbar(q, hbar_left, hbar_right, nbox,
                                   caution = if (has_caution) gr_hbar_caution(caution) else character(0))
    else if (is_meyestro && !identical(legend_pos, "none")) gr_widen_legend(ggplot2::ggplotGrob(q))
    else q
  }
  # Value labels (decision D22, finding F3), placed now that the panel is laid
  # out: its plotting area is measured as the figure draws it at the narrowest
  # width (and the height) it is drawn at - every header row, key and N-box row
  # taken off - so a label's room is the bar's real slot, not an estimate
  # (which stands only when nothing can be measured). Across the category axis
  # a label is its FONT HEIGHT thick (t, about 0.75 of its font size: the
  # height of its digits), set about 0.08 t off its bar's centre (its vjust),
  # so it reaches 0.58 t towards a neighbour; a bar is 0.9 of its slot wide
  # plus its outline (0.55 pt each side). Every non-zero bar is labeled when
  # its slot holds a label of at least GR_MIN_TEXT_PT - a label 1 pt clear of
  # the next label and 0.1 pt clear of the next bar, made smaller to fit and
  # never larger than designed; otherwise labels of GR_MIN_TEXT_PT are chosen
  # BAR BY BAR - the postoperative bars, the outcome, first, then the others;
  # within each, the label that blocks the fewest others first, then the
  # tallest - each kept only where it overlaps no label already kept (two
  # labels collide only when they are close along BOTH axes: less than t + 1 pt
  # apart across the category axis, and their extents along the value axis)
  # and runs into no taller bar beside it. (Physical geometry - raising the DPI
  # cannot fix it.) Panels that left labels out are counted for the run log,
  # and their legend files say so (generate_standard_graphs).
  if (show_val) {
    # (the categories in the order the axis draws them)
    lv_c <- levels(droplevels(dfb$label))
    lv_s <- levels(droplevels(factor(dfb$series, levels = names(pal))))
    m <- max(1, length(lv_s))
    # the category axis in data units: the categories 1 ... n and the room
    # the panel adds on each side (scale expansion; N boxes on the right of a
    # vertical panel, stat lines above the bars of a horizontal one)
    rexp <- if (has_nbox && is_meyestro) max(1.7, 0.75 + length(nbox) * nb_sp)
            else if (horizontal && isTRUE(cmp_h)) 0.9 + 0.85 * length(ann) else 0.6
    ps <- gr_panel_size(assemble(p), cell_w, .gr_opts$cell_h %||% 4.9)
    cat_pt0 <- if (is.null(ps)) span_pt / max(1, n_cat)
               else (if (horizontal) ps$h else ps$w) / (length(lv_c) - 1 + 0.6 + rexp)
    slot <- cat_pt0 * 0.8 / m
    t_max <- min(slot - 1, (0.55 * slot - 0.55 - 0.1) / 0.58)
    val_size <- min(gr_mm(label_size + 0.3), t_max / 0.75 / PT_PER_MM)
    if (val_size * PT_PER_MM < GR_MIN_TEXT_PT) {
      fs <- GR_MIN_TEXT_PT
      val_size <- fs / PT_PER_MM
      need_pt <- 0.75 * fs + 1                                     # label against label
      bar_pt <- 0.58 * 0.75 * fs + 0.1 + 0.45 * slot + 0.55        # label against a bar beside it
      xpt <- (match(as.character(dfb$label), lv_c) +
              (match(as.character(dfb$series), lv_s) - (m + 1) / 2) * 0.8 / m) * cat_pt0
      # where each label lies ALONG the value axis (finding F3): from its
      # bar's end, as long as its text (about 0.62 em a character, and its
      # offset) - in % units of the measured plotting area, whose value axis
      # runs over 1.1 x its limit (5% expansion each side); without a
      # measurement conservatively half the panel's height (60% of its width
      # on a horizontal panel)
      pct_pt <- if (!is.null(ps)) (if (horizontal) ps$w else ps$h) / (1.1 * ylim_top)
                else (if (horizontal) cell_w * 72 * 0.6 else (.gr_opts$cell_h %||% 4.9) * 72 * 0.5) / ylim_top
      ext <- (nchar(dfb$vlab) * 0.62 * fs + 3) / max(1e-6, pct_pt)
      # a label that would run into a TALLER bar beside it is never drawn
      ok <- nzchar(dfb$vlab) & is.finite(xpt) & is.finite(dfb$pct)
      for (i in which(ok)) {
        near <- setdiff(which(abs(xpt - xpt[i]) < bar_pt - 1e-9), i)
        if (any(dfb$pct[near] > dfb$pct[i], na.rm = TRUE)) ok[i] <- FALSE
      }
      # clash[i, j]: the labels of bars i and j would overlap
      nb <- nrow(dfb)
      clash <- outer(seq_len(nb), seq_len(nb), function(i, j)
        abs(xpt[i] - xpt[j]) < need_pt - 1e-9 &
          dfb$pct[j] < dfb$pct[i] + ext[i] & dfb$pct[i] < dfb$pct[j] + ext[j])
      clash[is.na(clash)] <- FALSE
      diag(clash) <- FALSE
      # the postoperative bars first, then the others; within each, the label
      # that blocks the fewest others still possible is kept first (then the
      # tallest), so a panel keeps as many labels as it can - three
      # postoperative bars side by side keep the outer two rather than the
      # tallest alone
      keep <- logical(nb)
      post_s <- grepl("Postop", as.character(dfb$series), fixed = TRUE)
      for (cls in list(post_s, !post_s)) {
        cand <- which(ok & cls)
        cand <- cand[!vapply(cand, function(i) any(clash[i, keep]), logical(1))]
        while (length(cand)) {
          i <- cand[order(rowSums(clash[cand, cand, drop = FALSE]), -dfb$pct[cand])[1]]
          keep[i] <- TRUE
          cand <- setdiff(cand[!clash[i, cand]], i)
        }
      }
      if (any(nzchar(dfb$vlab) & !keep))
        .gr_opts$n_label_thin <- (.gr_opts$n_label_thin %||% 0L) + 1L
      dfb$vlab[!keep] <- ""
    }
    if (any(nzchar(dfb$vlab))) {
      # (the plot's own data, its category labels as drawn, with the labels kept)
      dl <- p$data
      dl$vlab <- dfb$vlab
      p <- p + ggplot2::geom_text(data = dl, ggplot2::aes(label = vlab),
                         position = ggplot2::position_dodge(width = .8),
                         angle = if (horizontal) 0 else label_angle,
                         vjust = if (horizontal || label_angle == 0) -0.45 * (label_angle == 0) + 0.35 * (label_angle != 0 || horizontal) else 0.35,
                         hjust = if (horizontal) -0.15 else if (label_angle == 0) 0.5 else -0.12,
                         size = val_size, fontface = "bold")
    }
  }
  assemble(p)
}

# The colored "N = ..." boxes of a horizontal panel, stacked in rows added
# under figure `gt` (a gtable) across its whole width; gt unchanged without them
gr_add_nbox_rows <- function(gt, nbox) {
  if (is.null(nbox) || !length(nbox) || !inherits(gt, "gtable")) return(gt)
  gt <- gtable::gtable_add_rows(gt, gr_lines(1.05 * length(nbox) + 0.2), pos = nrow(gt))
  gtable::gtable_add_grob(gt, nbox_stack_grob(nbox), t = nrow(gt), b = nrow(gt), l = 1, r = ncol(gt),
                          clip = "off", name = "nbox", z = Inf)
}

# "N = x eyes" box per group, counting the eyes in `gl` (NA = an eye the panel
# does not use). total: optional group vector of ALL the eyes; when a group
# plots fewer eyes than it has, the box says so ("N = 217 of 247 eyes"), so the
# printed N is always the denominator of the panel's percentages. The counts
# are kept in attr(, "n").
make_nbox <- function(gl, glv, pal, suffix = "eyes", total = NULL) {
  n <- vapply(glv, function(g) sum(gl == g, na.rm = TRUE), numeric(1))
  nt <- if (is.null(total)) n else vapply(glv, function(g) sum(total == g, na.rm = TRUE), numeric(1))
  lab <- ifelse(n < nt, sprintf("N = %d of %d %s", n, nt, suffix), sprintf("N = %d %s", n, suffix))
  out <- stats::setNames(unname(pal[glv]), lab)
  attr(out, "n") <- stats::setNames(n, glv)
  out
}

# N box of a panel built from the eyes it actually uses (`used`: logical per
# eye). nb = TRUE builds it, NULL / FALSE draws none, a ready-made box is kept.
panel_nbox <- function(nb, used, gl, glv, pal, suffix = "eyes") {
  if (isTRUE(nb)) return(make_nbox(ifelse(used & !is.na(gl), gl, NA), glv, pal, suffix, total = gl))
  if (is.null(nb) || isFALSE(nb)) return(NULL)
  nb
}

# symmetric = TRUE closes every interval TOWARD ZERO (negative values [a, b),
# positive values (a, b]), so a signed quantity is binned symmetrically:
# -5 and +5 both fall in "-5 to 5", -45 and +45 in the matching 35-45 bins
pct_by_group <- function(v, gl, glv, breaks, labels, symmetric = FALSE) {
  do.call(rbind, lapply(glv, function(g) {
    x <- v[gl == g]
    ct <- if (symmetric)
      table(factor(ifelse(x >= 0,
                          as.character(cut(x, breaks = breaks, labels = labels, right = TRUE)),
                          as.character(cut(x, breaks = breaks, labels = labels, right = FALSE))),
                   levels = labels))
    else table(cut(x, breaks = breaks, labels = labels, right = TRUE))
    data.frame(label = labels, pct = as.numeric(ct) / max(1, sum(is.finite(x))) * 100, series = g)
  }))
}

# ------------------------------------------------------------
# Astigmatism vectors (JCRS 2025 standard, decisions D1-D2), all in PLUS-
# cylinder format: every refraction is converted sign-aware (helpers.R
# astig_plus - a minus cylinder at axis a is the plus cylinder at a + 90), so
# the switch "standardize to minus cylinder", a row typed half plus and half
# minus, or a target typed in either form never changes a vector. In double-
# angle space (x = M cos 2θ, y = M sin 2θ, θ = plus axis):
#   P = preoperative, Q = postoperative, T = target refractive astigmatism
#   PRE  = P                  preoperative refractive astigmatism
#   POST = Q                  postoperative refractive astigmatism
#   SIA  = Q - P              surgically induced astigmatism
#   PE   = Q - T              prediction error (= POST when the target is plano)
# and the Alpins quantities of the standard graphs, unchanged in value:
#   TIA  = T - P              the change that was intended
#   DV   = T - Q = -PE        the difference vector
# Every "axis" returned is the plus axis of that vector, in [0, 180), so the
# double-angle plots put with-the-rule (plus axis 90) on the LEFT.
# The components are built with cospi / sinpi (x = M cospi(θ / 90)), exact at
# multiples of 45°: an eye at plus axis 90 or 180 has y = 0 exactly, not the
# 1.2e-16 M of sin(pi), so a cohort on one meridian stays on it.
# With no target (or a plano one) the plano branch reuses the magnitudes and
# axes directly rather than recomputing them through sqrt/atan2, so it
# introduces no floating-point drift. A blank or zero target cylinder is plano;
# a non-zero target cylinder without an axis leaves PE, TIA, DV and the indices
# of that eye missing (n_target_noaxis). pre_noaxis / post_noaxis mark the
# eyes whose refraction has a non-zero cylinder but no axis (no vector).
# Also returns the scalar indices Alpins defines alongside the vectors, and
# n_target_nonplano: the eyes whose target is not plano (decision D3: only
# then is POST a panel of its own).
# ------------------------------------------------------------
alpins_vectors_target <- function(pre_cyl, pre_ax, post_cyl, post_ax,
                                  tgt_cyl = NULL, tgt_ax = NULL) {
  P <- astig_plus(pre_cyl, pre_ax); Q <- astig_plus(post_cyl, post_ax)
  pm <- P$mag; pa <- P$axis; qm <- Q$mag; qa <- Q$axis
  px <- pm * cospi(pa / 90); py <- pm * sinpi(pa / 90)
  qx <- qm * cospi(qa / 90); qy <- qm * sinpi(qa / 90)

  # a null vector has no meridian; keep the eye's own axis so a zero-cylinder
  # eye reports the axis its data carries rather than an artificial 0
  ax_of <- function(x, y, fallback) {
    a <- (atan2(y, x) * 180 / pi / 2) %% 180
    m <- sqrt(x^2 + y^2)
    bad <- !is.finite(m) | m < 1e-12
    a[bad] <- fallback[bad]
    a
  }
  turn <- function(a) (a + 90) %% 180               # the plus axis of the opposite vector

  has_t <- !is.null(tgt_cyl) && !is.null(tgt_ax)
  n <- max(length(pm), length(qm))
  n_noaxis_t <- 0L
  if (has_t) {
    # a blank or zero target cylinder is plano (as the mEYEstro tab reads it),
    # its axis irrelevant; a non-zero target cylinder WITHOUT an axis is no
    # target - PE, TIA, DV and the indices are missing for that eye, never
    # computed against a plano target (resolve_astig_vectors logs the count)
    tc <- gr_num(tgt_cyl); ta0 <- gr_num(tgt_ax)
    blank <- !is.finite(tc) | tc == 0
    tc[blank] <- 0; ta0[blank] <- 0
    Tt <- astig_plus(tc, ta0)
    tm <- Tt$mag; ta <- Tt$axis
    n_noaxis_t <- sum(Tt$noaxis)
    tx <- tm * cospi(ta / 90); ty <- tm * sinpi(ta / 90)
  } else {
    tm <- rep(0, n); ta <- rep(0, n); tx <- 0; ty <- 0
  }
  plano <- is.finite(tm) & tm == 0
  no_t <- !is.finite(tm)                             # a target without an axis
  n_nonplano <- if (has_t) sum(!plano & !no_t) else 0L

  sx <- qx - px; sy <- qy - py                       # SIA = POST - PRE, target-independent
  sia <- sqrt(sx^2 + sy^2)
  sax <- (atan2(sy, sx) * 180 / pi / 2) %% 180

  # TIA = T - P and PE = Q - T; DV = -PE. Eyes with a plano target take the
  # exact branch (TIA = -P: |P| at the plus axis + 90; PE = Q)
  ix <- tx - px; iy <- ty - py
  ex <- qx - tx; ey <- qy - ty
  tia <- sqrt(ix^2 + iy^2); tax <- ax_of(ix, iy, turn(pa))
  pe <- sqrt(ex^2 + ey^2);  pex <- ax_of(ex, ey, qa)
  tia[plano] <- pm[plano]; tax[plano] <- turn(pa[plano])
  pe[plano] <- qm[plano];  pex[plano] <- qa[plano]
  tia[no_t] <- NA; tax[no_t] <- NA; pe[no_t] <- NA; pex[no_t] <- NA
  dv <- pe; dax <- turn(pex)

  aoe <- (atan2(sy, sx) - atan2(iy, ix)) * 180 / pi / 2
  aoe <- ((aoe + 90) %% 180) - 90
  undef <- !is.finite(tia) | tia == 0                # every index divides by TIA
  aoe[undef] <- NA
  # no surgically induced change: the SIA has no meridian, so there is no
  # angle of error either (decision D27; atan2(0, 0) would give minus the TIA
  # meridian)
  aoe[!is.finite(sia) | sia < 1e-12] <- NA
  ci <- sia / tia; ci[undef] <- NA                   # correction index
  me <- sia - tia                                    # magnitude of error
  ios <- dv / tia; ios[undef] <- NA                  # index of success
  list(pre = pm, pre_ax = pa, post = qm, post_ax = qa, pe = pe, pe_ax = pex,
       tia = tia, tax = tax, sia = sia, sax = sax, dv = dv, dax = dax,
       aoe = aoe, ci = ci, me = me, ios = ios, has_target = has_t,
       n_target_nonplano = n_nonplano, n_target_noaxis = n_noaxis_t,
       pre_noaxis = P$noaxis, post_noaxis = Q$noaxis)
}

# The Alpins TIA threshold of the correction-index and angle-of-error panels
# (0.25 D), with a floating-point guard: against a non-plano target |T - P|
# can fall a few ulps below 0.25 for an eye whose TIA is exactly 0.25 D
# (validation.md V19)
TIA_MIN <- 0.25
tia_ok <- function(tia) is.finite(tia) & tia >= TIA_MIN - 1e-9

# ------------------------------------------------------------
# Resolve the astigmatism vectors for a run, PER VECTOR, logging the provenance
# of each so a reader can always tell whether a number came from the user's
# spreadsheet or from the engine. Precedence, independently for TIA / SIA / DV
# / PE:
#   1. magnitude AND axis columns mapped and present -> use them as given
#   2. else a target cylinder + axis mapped          -> compute against that target
#   3. else                                          -> compute with target = 0
# Supplied columns are read in the plus-cylinder convention of the computed
# vectors (magnitude >= 0, the vector's plus axis; v1.0.0 columns of TIA, SIA
# and DV mean the same points). PE and DV are one vector with opposite signs:
# when only one of them is supplied the other is derived from it (PE = -DV),
# and the log says so. PRE and POST are the refractions themselves.
# When a vector is supplied AND computable, the computed value is produced anyway
# and the largest disagreement is reported, so the two can be reconciled.
# ------------------------------------------------------------
# display_names: optional clean key -> header map (res$display_names): the log
# names the mapped columns by their headers as typed in the file (R4-3).
resolve_astig_vectors <- function(df, pre_cyl, pre_ax, post_cyl, post_ax,
                                  vec_cols = list(), target = list(), logger = NULL,
                                  display_names = NULL) {
  # once per run: the figures of every cohort and the astigmatism sheet
  # resolve the same vectors
  say <- function(...) if (!is.null(logger)) log_once(logger, paste0(...))
  dn <- function(k) disp_name(k, display_names)
  usable <- function(x) !is.null(x) && !is.na(x) && nzchar(x) && x %in% names(df)
  gcol <- function(k) { v <- vec_cols[[k]]; if (usable(v)) v else NULL }

  tc <- if (usable(target$cyl)) target$cyl else NULL
  ta <- if (usable(target$axis)) target$axis else NULL
  has_t <- !is.null(tc) && !is.null(ta)
  av <- alpins_vectors_target(pre_cyl, pre_ax, post_cyl, post_ax,
                              if (has_t) df[[tc]] else NULL,
                              if (has_t) df[[ta]] else NULL)
  base <- if (has_t) sprintf("computed vs mapped target ('%s', '%s')", dn(tc), dn(ta))
          else "computed (target = 0)"
  say("Astigmatism vectors: plus-cylinder format (JCRS 2025; each refraction converted by the sign of ",
      "its cylinder) - with-the-rule astigmatism on the left of the double-angle plots; SIA = ",
      "postoperative - preoperative, PE = postoperative - target.")
  if (has_t)
    say("Astigmatism: a target refraction is mapped, so TIA is the intended CHANGE (target - preoperative) ",
        "and PE is postoperative - target - values will differ from a target-of-zero run",
        if (av$n_target_nonplano > 0) sprintf(" (%d eye(s) with a target that is not plano).", av$n_target_nonplano)
        else " (every target is plano, so PE equals the postoperative astigmatism).")
  if (has_t && isTRUE(av$n_target_noaxis > 0))
    say(sprintf("Astigmatism target: %d eye(s) have a target cylinder without an axis - PE/TIA/DV not computed for them.",
                av$n_target_noaxis))
  # a refraction whose cylinder is not zero but whose axis is blank has no
  # vector: those eyes are left out of the vectors that need it, never silently
  # (a zero cylinder with a blank axis is plano, and kept)
  inrun <- if ("GroupLabel" %in% names(df)) !is.na(df$GroupLabel) else TRUE
  n_pre_na <- sum(av$pre_noaxis & inrun); n_post_na <- sum(av$post_noaxis & inrun)
  if (n_pre_na + n_post_na > 0)
    say(sprintf(paste0("Astigmatism: %d analyzed eye(s) have a preoperative and %d a postoperative cylinder without an axis - ",
                       "no vector for that refraction, so those eyes are left out of PRE, SIA and TIA (preoperative) or of ",
                       "POST, SIA, PE and DV (postoperative) and of their tests."), n_pre_na, n_post_na))

  spec <- list(list("tia", "tax", "tia_mag", "tia_axis", "TIA"),
               list("sia", "sax", "sia_mag", "sia_axis", "SIA"),
               list("dv",  "dax", "dv_mag",  "dv_axis",  "DV"),
               list("pe",  "pe_ax", "pe_mag", "pe_axis", "PE"))
  changed <- FALSE
  got <- character(0)
  for (v in spec) {
    cm <- gcol(v[[3]]); ca <- gcol(v[[4]])
    if (is.null(cm) || is.null(ca)) next
    sm <- abs(gr_num(df[[cm]])); sa <- gr_num(df[[ca]]) %% 180
    d <- abs(sm - av[[v[[1]]]]); d <- d[is.finite(d)]
    say(sprintf("Astigmatism %s: supplied from '%s' / '%s' (%d eyes); max difference vs %s = %s D.",
                v[[5]], dn(cm), dn(ca), sum(is.finite(sm)), base,
                if (length(d)) sprintf("%.4f", max(d)) else "n/a"))
    av[[v[[1]]]] <- sm; av[[v[[2]]]] <- sa
    got <- c(got, v[[5]])
    changed <- TRUE
  }
  # PE and DV are the same vector with opposite signs: one supplied, the
  # other follows it, so the PE plot and the Alpins panels never disagree
  if ("DV" %in% got && !("PE" %in% got)) {
    av$pe <- av$dv; av$pe_ax <- (av$dax + 90) %% 180
    say("Astigmatism PE: derived from the supplied DV columns (PE = -DV).")
    got <- c(got, "PE")
  } else if ("PE" %in% got && !("DV" %in% got)) {
    av$dv <- av$pe; av$dax <- (av$pe_ax + 90) %% 180
    say("Astigmatism DV: derived from the supplied PE columns (DV = -PE).")
    got <- c(got, "DV")
  }
  for (v in spec) if (!(v[[5]] %in% got)) say("Astigmatism ", v[[5]], ": ", base, ".")

  # every index is a function of the RESOLVED vectors, so recompute them only if
  # a vector was actually overridden (otherwise keep the exact original values)
  if (changed) {
    undef <- !is.finite(av$tia) | av$tia == 0
    av$ci <- av$sia / av$tia; av$ci[undef] <- NA
    av$me <- av$sia - av$tia
    av$ios <- av$dv / av$tia; av$ios[undef] <- NA
    sx <- av$sia * cospi(av$sax / 90); sy <- av$sia * sinpi(av$sax / 90)
    tx <- av$tia * cospi(av$tax / 90); ty <- av$tia * sinpi(av$tax / 90)
    aoe <- (atan2(sy, sx) - atan2(ty, tx)) * 180 / pi / 2
    av$aoe <- ((aoe + 90) %% 180) - 90
    av$aoe[undef] <- NA
    av$aoe[!is.finite(av$sia) | av$sia < 1e-12] <- NA      # no SIA, no angle of error (D27)
  }
  av
}

# ============================================================
# Standard comparison panels (all groups on one chart)
# ============================================================

# The stat line printed above each panel. For TWO groups this is the classic
# pairwise line (p_two); for MORE it becomes the omnibus test plus each group
# against the reference, which is what keeps the panels readable with all
# groups superimposed.
p_group_text <- function(v, gl, glv, kind = "continuous", event = NULL, max_chars = Inf) {
  if (length(glv) == 2) return(p_two(v, gl, glv))
  stat_footer(v, gl, glv, kind, event, max_chars)   # omnibus line, then the vs-reference line
}
# the narrow horizontal panels (CI, angle of error) fit about this many
# characters per stat line in a 4-across composite row
HBAR_MAX_CHARS <- 56

# `pc` is the reference CDVA drawn beside postop UDVA and `second_lab` names it:
# preoperative CDVA for the corneal sets, postoperative for the cataract set (the
# caller passes the same reference the efficacy panel uses).
g_cumulative_va <- function(pu, pc, gl, glv, pal, second_col = NULL, nb = TRUE,
                            second_lab = "Preop CDVA") {
  # each series is a percentage of the eyes with a usable acuity in THAT series:
  # the N box counts the postop UDVA eyes, and the CDVA count is printed
  # wherever it differs
  has_u <- is.finite(gr_logmar(pu)); has_c <- is.finite(gr_logmar(pc))
  nu <- function(g) sum(has_u & !is.na(gl) & gl == g); nc <- function(g) sum(has_c & !is.na(gl) & gl == g)
  nb <- panel_nbox(nb, has_u, gl, glv, pal)
  # uniform 0.1-logMAR steps: 20/12.5 (-0.2) ... 20/40 (0.3), 20/50 (0.4), 20/63 (0.5) ...
  denoms <- c(12.5, 16, 20, 25, 32, 40, 50, 63, 80, 100)
  # An eye is "20/d or better" when it reaches the Snellen fraction 20/d OR the
  # nominal 0.1-logMAR line that fraction stands for (20/25 is logMAR 0.097 as a
  # fraction, 0.1 as a chart line; 20/32 is 0.204 or 0.2): decimal and Snellen
  # acuities sit on the fractions, logMAR and ETDRS ones on the nominal lines,
  # and both must count. The 1e-6 absorbs the decimal <-> logMAR round trip.
  lm_cut <- pmax(log10(denoms / 20), round(log10(denoms / 20), 1)) + 1e-6
  cum_pct <- function(dec) {
    lm <- gr_logmar(dec)
    vapply(lm_cut, function(cut) mean(lm <= cut, na.rm = TRUE) * 100, numeric(1))
  }
  rows <- list(); pal2 <- c(); ann <- list()
  for (g in glv) {
    u <- gr_num(pu[gl == g]); c_ <- gr_num(pc[gl == g])
    nm_u <- paste0(g, " Postop UDVA"); nm_c <- paste0(g, " ", second_lab)
    rows[[length(rows) + 1]] <- data.frame(label = paste0("20/", denoms),
      pct = cum_pct(u), series = nm_u)
    rows[[length(rows) + 1]] <- data.frame(label = paste0("20/", denoms),
      pct = cum_pct(c_), series = nm_c)
    pal2[nm_u] <- pal[[g]]; pal2[nm_c] <- second_col %||% lighten_col(pal[[g]])
    ann[[length(ann) + 1]] <- list(col = pal[[g]],
      text = sprintf("%s: UDVA %s · %s %s · N UDVA = %d, CDVA = %d", g,
                     gr_va_text(gr_logmar(pu[gl == g])), second_lab, gr_va_text(gr_logmar(pc[gl == g])),
                     nu(g), nc(g)))
  }
  dfb <- do.call(rbind, rows)
  # mEYEstro-style bin trimming: drop leading all-zero bins and stop at the
  # first bin where every series has reached ~100%
  labs_all <- paste0("20/", denoms)
  mx <- vapply(labs_all, function(l) max(dfb$pct[dfb$label == l], na.rm = TRUE), numeric(1))
  mn <- vapply(labs_all, function(l) min(dfb$pct[dfb$label == l], na.rm = TRUE), numeric(1))
  first_keep <- which(mx > 0.5)[1] %||% 1
  done <- which(mn >= 99.5)
  last_keep <- if (length(done)) done[1] else length(labs_all)
  keep_labs <- labs_all[first_keep:last_keep]
  dfb <- dfb[dfb$label %in% keep_labs, , drop = FALSE]
  # Panel A header (mEYEstro): a 2x2 legend (drawn as swatches) with the postop
  # UDVA / reference CDVA mean beneath - the Snellen geometric mean with the
  # logMAR mean ± SD (decision D16) - one column per group. The text is in the
  # full group colour (a pale tint is illegible in print; D22): the tints are
  # for the bars and swatches only.
  hs <- NULL; lpos <- "inside"
  if (length(glv) == 2) {
    col_of <- function(g, light) if (light) (second_col %||% lighten_col(pal[[g]])) else pal[[g]]
    mkcol <- function(g) list(x = if (g == glv[1]) 0.01 else "auto", lines = list(
      list(seg(paste0(g, " Postop UDVA"), "black")),
      list(seg(paste0(g, " ", second_lab,
                      if (nc(g) != nu(g)) sprintf(" (n = %d)", nc(g)) else ""), "black")),
      list(seg(gr_va_text(gr_logmar(pu[gl == g])), pal[[g]])),
      list(seg(gr_va_text(gr_logmar(pc[gl == g])), pal[[g]]))))
    # seg() has no swatch arg; attach $sw for the two legend rows
    c1 <- mkcol(glv[1]); c1$lines[[1]][[1]]$sw <- col_of(glv[1], FALSE)
    c1$lines[[2]][[1]]$sw <- col_of(glv[1], TRUE)
    c2 <- mkcol(glv[2]); c2$lines[[1]][[1]]$sw <- col_of(glv[2], FALSE)
    c2$lines[[2]][[1]]$sw <- col_of(glv[2], TRUE)
    hs <- list(c1, c2); lpos <- "none"
  }
  gr_bars(dfb, pal2, "Cumulative Snellen VA (20/x or better)",
          "Cumulative % of eyes", "Uncorrected Distance Visual Acuity", ann,
          label_size = 2.3, nbox = nb, header_spec = hs, legend_pos = lpos)
}

# Difference a - b between two DECIMAL acuities in Snellen lines (0.1 logMAR),
# rounded to whole lines half AWAY from zero, as the mEYEstro tabs round
# (R's round() rounds half to even, so the exact half-lines that logMAR and
# ETDRS input can produce would land on alternating sides). Rounding to 1e-6
# first removes the floating noise of the logMAR -> decimal -> logMAR round trip.
gr_line_diff <- function(a_dec, b_dec) {
  d <- round((gr_logmar(a_dec) - gr_logmar(b_dec)) / 0.1, 6)
  trunc(d + 0.5 * sign(d))
}

# Efficacy: postop UDVA against ONE reference CDVA (`pv`), used for both the
# line bins and the Efficacy Index (eff_num / eff_den). The caller passes the
# preoperative CDVA for the corneal sets (Waring/Reinstein 2011 standard
# graph 2) and the postoperative CDVA for the cataract set (Reinstein 2017);
# cdva_lab names it on the axis and in the header.
# An acuity index (efficacy, safety): numerator / denominator for the eyes where
# both are defined. A zero or negative denominator, a negative numerator or a
# non-finite ratio is NA, so the header mean, its P value and d all run on one
# finite set of eyes and never print "NaN ± NaN".
gr_ratio <- function(n, d) {
  n <- gr_num(n); d <- gr_num(d)
  r <- n / d
  r[!is.finite(r) | !is.finite(d) | d <= 0 | !is.finite(n) | n < 0] <- NA_real_
  r
}

g_udva_vs_cdva <- function(pu, pv, gl, glv, pal, eff_num, eff_den, nb = TRUE,
                           cdva_lab = "Preop CDVA") {
  lines <- gr_line_diff(pu, pv)
  nb <- panel_nbox(nb, is.finite(lines), gl, glv, pal)
  labs <- c("3 or more\nworse", "2 worse", "1 worse", "same", "1 or more\nbetter")
  dfb <- pct_by_group(-lines, gl, glv, c(-Inf, -2.5, -1.5, -0.5, 0.5, Inf), labs)
  ei <- gr_ratio(eff_num, eff_den)
  # (" ≥ " spaced like every other sign of the headers, finding F11)
  ann <- lapply(glv, function(g) list(col = pal[[g]], text = sprintf(
    "%s: Efficacy Index %s · UDVA ≥ %s %s", g, msd(ei[gl == g]), cdva_lab,
    gr_pct(lines[gl == g] <= 0))))
  pp <- p_group_text(ei, gl, glv)
  for (.t in pp) if (nzchar(.t)) ann <- c(ann, list(list(col = "black", text = .t)))
  hs <- NULL
  if (length(glv) == 2) {
    cols <- unname(pal[glv])
    same <- vapply(glv, function(g) gr_pct(lines[gl == g] <= 0), character(1))
    w1   <- vapply(glv, function(g) gr_pct(abs(lines[gl == g]) <= 1), character(1))
    eff  <- vapply(glv, function(g) msd(ei[gl == g]), character(1))
    # the named test + P + d gets its own line under the index: on one line with
    # it the header reached the edge of a composite cell
    pdl <- p_d_text(ei, gl, glv, pp)
    hs <- list(list(x = 0.01, lines = c(list(
      vs_line("Efficacy Index: ", eff[1], eff[2], cols)),
      if (nzchar(pdl)) list(list(seg(pdl))) else list(), list(
      vs_line(paste0("UDVA same or better than ", cdva_lab, ": "), same[1], same[2], cols),
      vs_line(paste0("UDVA within 1 line of ", cdva_lab, ": "), w1[1], w1[2], cols)))))
  }
  gr_bars(dfb, pal[glv], sprintf("Postop UDVA vs %s (Snellen lines)", cdva_lab), "% of eyes",
          "Uncorrected vs Corrected Distance Visual Acuity", ann, nbox = nb, header_spec = hs,
          caution = gr_caution(pp), short_spec = gr_short_head(hs))
}

# The short form of a two-group header (decision R2-2): its first line (the
# panel's index, both cohorts) and the test line under it, when there is one
gr_short_head <- function(hs) {
  if (is.null(hs) || !length(hs)) return(NULL)
  ln <- hs[[1]]$lines
  k <- if (length(ln) >= 2 && length(ln[[2]]) == 1 && grepl("P [=<>]|P not computed", ln[[2]][[1]]$text)) 2L else 1L
  list(list(x = 0.01, lines = ln[seq_len(min(k, length(ln)))]))
}

g_cdva_change <- function(pv_pre, pv_post, gl, glv, pal, saf_num, saf_den, nb = TRUE) {
  lines <- gr_line_diff(pv_pre, pv_post)
  nb <- panel_nbox(nb, is.finite(lines), gl, glv, pal)
  # the seven categories of the standard graph (Dupps 2011): a loss of three
  # or more lines is shown apart from a loss of two (decision D22)
  labs <- c("Loss 3\nor more", "Loss 2", "Loss 1", "No change", "Gain 1", "Gain 2", "Gain 3\nor more")
  dfb <- pct_by_group(lines, gl, glv, c(-Inf, -2.5, -1.5, -0.5, 0.5, 1.5, 2.5, Inf), labs)
  si <- gr_ratio(saf_num, saf_den)
  ann <- lapply(glv, function(g) list(col = pal[[g]], text = sprintf(
    "%s: Safety Index %s · 2+ lines lost %s", g, msd(si[gl == g]),
    gr_pct(lines[gl == g] <= -2, 1))))
  pp <- p_group_text(si, gl, glv)
  for (.t in pp) if (nzchar(.t)) ann <- c(ann, list(list(col = "black", text = .t)))
  hs <- NULL
  if (length(glv) == 2) {
    cols <- unname(pal[glv])
    gain <- vapply(glv, function(g) gr_pct(lines[gl == g] >= 0), character(1))
    lost <- vapply(glv, function(g) gr_pct(lines[gl == g] <= -2), character(1))
    saf  <- vapply(glv, function(g) msd(si[gl == g]), character(1))
    pdl <- p_d_text(si, gl, glv, pp)             # own line, as on the efficacy panel
    hs <- list(list(x = 0.01, lines = c(list(
      vs_line("Safety Index: ", saf[1], saf[2], cols)),
      if (nzchar(pdl)) list(list(seg(pdl))) else list(), list(
      vs_line("same or gain lines: ", gain[1], gain[2], cols),
      vs_line("2 or more lines lost: ", lost[1], lost[2], cols)))))
  }
  gr_bars(dfb, pal[glv], "Change in Snellen lines of CDVA", "% of eyes",
          "Change in Corrected Distance Visual Acuity", ann, nbox = nb, header_spec = hs,
          caution = gr_caution(pp), short_spec = gr_short_head(hs))
}

# `poseq` is the SEQ error to bin: postop SEQ minus the intended target (the
# caller subtracts a mapped target_seq; with none the target is plano and this
# is the postop SEQ itself). has_target only changes the header wording.
g_seq_accuracy <- function(poseq, gl, glv, pal, nb = TRUE, has_target = FALSE) {
  err <- gr_num(poseq)
  nb <- panel_nbox(nb, is.finite(err), gl, glv, pal)
  slab <- if (has_target) "SEQ - target" else "SEQ"
  # the standard graph's bin labels, two decimals throughout (decision D22)
  labs <- c("< -2.00", "-2.00 to -1.51", "-1.50 to -1.01", "-1.00 to -0.51", "-0.50 to -0.14",
            "-0.13 to +0.13", "+0.14 to +0.50", "+0.51 to +1.00", "+1.01 to +1.50",
            "+1.51 to +2.00", "> +2.00")
  brk <- c(-Inf, -2.005, -1.505, -1.005, -0.505, -0.135, 0.135, 0.505, 1.005, 1.505, 2.005, Inf)
  dfb <- pct_by_group(err, gl, glv, brk, labs)
  ann <- lapply(glv, function(g) list(col = pal[[g]], text = sprintf(
    "%s: %s %s D · ±0.50 D %s · ±1.00 D %s", g, slab, msd(err[gl == g]),
    gr_pct(abs(err[gl == g]) <= .5 + 1e-9), gr_pct(abs(err[gl == g]) <= 1 + 1e-9))))
  pp <- p_group_text(err, gl, glv)
  for (.t in pp) if (nzchar(.t)) ann <- c(ann, list(list(col = "black", text = .t)))
  hs <- NULL
  if (length(glv) == 2) {
    cols <- unname(pal[glv])
    leftlines <- lapply(c(0.25, 0.5, 0.75, 1.0), function(cc) {
      pc <- vapply(glv, function(g) gr_pct(abs(err[gl == g]) <= cc + 1e-9, 1), character(1))
      vs_line(sprintf("SEQ ± %.2f D: ", cc), pc[1], pc[2], cols)
    })
    sq <- vapply(glv, function(g) msd(err[gl == g]), character(1))
    rightlines <- c(list(list(seg(if (has_target) "Postop SEQ - target:" else "Postop SEQ:")),
      vs_line("", paste0(sq[1], " D"), paste0(sq[2], " D"), cols)),
      p_d_lines(err, gl, glv, pp))
    hs <- list(list(x = 0.01, lines = leftlines), list(x = "auto", lines = rightlines))
    # short form (R2-2): the mean of each cohort and the test
    pdl <- p_d_text(err, gl, glv, pp)
    hs_short <- list(list(x = 0.01, lines = c(list(
      vs_line(if (has_target) "Postop SEQ - target: " else "Postop SEQ: ", paste0(sq[1], " D"), paste0(sq[2], " D"), cols)),
      if (nzchar(pdl)) list(list(seg(pdl))) else list())))
  }
  gr_bars(dfb, pal[glv], "Accuracy of SEQ to intended target (D)", "% of eyes",
          "Spherical Equivalent Refraction Accuracy", ann, label_size = 2.2,
          x_angle = 32, nbox = nb, header_spec = hs, caution = gr_caution(pp),
          short_spec = if (!is.null(hs)) hs_short)
}

# indicator-line scatter core (attempted/achieved + TIA/SIA), styled to match
# mEYEstro: two-column colored stat header ABOVE the panel (eq, R2, Mean/Range
# or TIA/SIA), colored N-boxes inside, two corner boxes (corner_labels[1] at
# the DISPLAYED top-left, corner_labels[2] at the bottom-right, whether or not
# the axes are reversed), axes reversed for an all-myopic SEQ cohort.
scatter_ind <- function(d, pal, xlab, ylab, title, from_zero = FALSE,
                        reverse = FALSE, stat_kind = "meanrange", nbox = NULL,
                        corner_labels = c("Overcorrected", "Undercorrected")) {
  lim <- range(c(d$x, d$y, if (from_zero) 0 else NULL), na.rm = TRUE)
  pad <- diff(lim) * .06; lim <- lim + c(-pad, pad)
  if (from_zero) lim[1] <- 0
  span <- diff(lim)
  # the range is printed in axis order (left to right), so it reads the same
  # way as the reversed myopic axes and the normal hyperopic / mixed ones
  rng_of <- function(x) if (reverse) c(max(x), min(x)) else c(min(x), max(x))
  # per-group header block: name, equation, R2, then Mean/Range or TIA/SIA
  blocks <- lapply(names(pal), function(g) {
    dd <- d[d$Group == g, ]
    if (nrow(dd) < 2) return(NULL)
    f <- stats::lm(y ~ x, data = dd); b <- stats::coef(f)
    rg <- rng_of(dd$x)
    ls <- c(sprintf("● %s", g), sprintf("y = %.2fx %+.2f", b[2], b[1]),
            sprintf("R² = %.2f", summary(f)$r.squared))
    if (identical(stat_kind, "tiasia"))
      ls <- c(ls, sprintf("TIA: %.2f ± %.2f D", mean(dd$x), stats::sd(dd$x)),
                  sprintf("SIA: %.2f ± %.2f D", mean(dd$y), stats::sd(dd$y)))
    else
      ls <- c(ls, sprintf("Mean: %.2f ± %.2f D", mean(dd$x), stats::sd(dd$x)),
                  sprintf("Range: %.2f to %.2f D", rg[1], rg[2]))
    # compact one-liner used when there are too many groups for columns
    stat2 <- if (identical(stat_kind, "tiasia"))
      sprintf("TIA %.2f±%.2f · SIA %.2f±%.2f",
              mean(dd$x), stats::sd(dd$x), mean(dd$y), stats::sd(dd$y))
    else
      sprintf("%.2f±%.2f D · %.2f to %.2f",
              mean(dd$x), stats::sd(dd$x), rg[1], rg[2])
    short <- sprintf("● %s: y=%.2fx%+.2f · R²=%.2f · %s",
                     g, b[2], b[1], summary(f)$r.squared, stat2)
    # the same on two lines, for a narrow panel
    short2 <- c(sprintf("● %s: y=%.2fx%+.2f · R²=%.2f", g, b[2], b[1], summary(f)$r.squared),
                paste0("    ", stat2))
    list(col = pal[[g]], lines = ls, short = short, short2 = short2)
  })
  blocks <- Filter(Negate(is.null), blocks)
  # A panel built at a reduced text scale (a combined figure at a journal
  # width, decision R2-2): one line per group in a grid header spaced in
  # points above the plot - wrapped at " · " to the figure's width - instead of
  # lines placed at data heights, which assume the designed panel size; the
  # short form keeps each group's equation and R² and moves the rest to the
  # legend file.
  if (gr_ts() < 1) {
    lines <- lapply(blocks, function(b) {
      txt <- b$short
      if (isTRUE(.gr_opts$short_hdr)) {
        i <- regexpr(" · ", txt, fixed = TRUE)
        j <- if (i > 0) regexpr(" · ", substring(txt, i + 3), fixed = TRUE) else -1
        if (j > 0) {
          cut <- i + 3 + j - 1
          gr_moved(paste0(sub("^● ([^:]*: ).*$", "\\1", txt), substring(txt, cut + 3)))
          txt <- substr(txt, 1, cut - 1)
        }
      }
      list(seg(txt, b$col))
    })
    hs <- list(list(x = 0.01, lines = lines))
    fig_w <- .gr_opts$cell_w %||% GR_BAR_MIN_W
    hs <- tryCatch(gr_with_measure_device(fig_w, 4.9, function()
      gr_fit_bar_header(hs, fig_w * 72.27 - gr_sp(16))), error = function(e) hs)
    nlines <- hdr_nlines(hs)
    blocks <- list()
  }
  # 1-2 groups: classic side-by-side stat columns. 3+ groups: one FULL-WIDTH
  # line per group instead, because thirds of a composite cell are too narrow
  # and the columns collide. In a panel narrower than GR_HDR_STACK_W (laid out
  # at a journal column width) every group gets two full-width lines.
  narrow <- gr_narrow()
  stacked <- length(blocks) > 2 || (narrow && length(blocks) > 1)
  if (stacked)
    blocks <- if (narrow) unlist(lapply(blocks, function(b)
                lapply(b$short2, function(t) list(col = b$col, lines = t))), recursive = FALSE)
              else lapply(blocks, function(b) list(col = b$col, lines = b$short))
  compact <- gr_ts() < 1
  if (!compact)
    nlines <- if (stacked) length(blocks)
              else if (length(blocks)) length(blocks[[1]]$lines) else 0
  # the per-group least-squares line whose equation the header prints: same
  # n >= 2 rule as the header blocks (and >= 2 distinct x, else no slope), drawn
  # only across the group's own x range so it never extrapolates
  fit_grp <- names(pal)[vapply(names(pal), function(g) {
    xx <- d$x[d$Group == g & is.finite(d$x) & is.finite(d$y)]
    length(xx) >= 2 && length(unique(xx)) >= 2 }, logical(1))]
  d_fit <- d[d$Group %in% fit_grp, , drop = FALSE]
  p <- ggplot2::ggplot(d, ggplot2::aes(x, y, colour = Group)) +
    ggplot2::geom_abline(slope = 1, intercept = 0, colour = IND_IDENT, linewidth = .55) +
    ggplot2::geom_abline(slope = 1, intercept = c(-.5, .5), colour = IND_HALF, linewidth = .45) +
    ggplot2::geom_abline(slope = 1, intercept = c(-1, 1), colour = IND_ONE, linewidth = .45) +
    ggplot2::geom_point(alpha = .7, size = 1.5) +
    (if (nrow(d_fit))
       ggplot2::geom_smooth(data = d_fit, method = "lm", formula = y ~ x, se = FALSE,
                            linewidth = .5, linetype = "22", fullrange = FALSE)
     else NULL) +
    ggplot2::scale_colour_manual(values = pal) +
    ggplot2::labs(x = xlab, y = ylab) + gr_theme() +   # title omitted: header block identifies it
    ggplot2::theme(panel.grid.major = ggplot2::element_line(colour = "grey85", linewidth = .25),
                   legend.position = "none",
                   # (compact: the header has a row of its own, gr_header_over)
                   plot.margin = if (compact) ggplot2::margin(t = gr_sp(4), r = gr_sp(10),
                                                              b = gr_sp(4), l = gr_sp(6))
                                 else ggplot2::margin(t = 26 + 13 * nlines, r = 10, b = 4, l = 6))
  if (reverse) p <- p + ggplot2::scale_x_reverse() + ggplot2::scale_y_reverse()
  # place things by DISPLAY fraction (from left / from top) so axis reversal is
  # handled uniformly; fy < 0 sits above the panel
  dx <- function(fx) if (reverse) lim[2] - fx * span else lim[1] + fx * span
  dy <- function(fy) if (reverse) lim[1] + fy * span else lim[2] - fy * span
  # two-column colored header ABOVE the panel. HDR_BASE must clear the panel's
  # own expansion: coord_fixed adds ~5% beyond `lim`, so a smaller base left the
  # last line sitting on the border, clipped in half.
  HDR_BASE <- 0.115
  # one header line at display fractions (fx, fy). A leading "●" is a plotted
  # point, not a glyph (see HDR_MARKS), followed by the text after a run of
  # spaces; a header line is 0.058 x span apart for ~13 pt, which places the
  # point in pt-sized steps
  pt_f <- 0.058 / 13
  # The side (pt) of the square plot as it is drawn, measured on a measuring
  # device in the room its cell (or the panel's own file) leaves it: the
  # header row of a compact panel and the journal style's N boxes taken off.
  # NA when it cannot be measured.
  journal <- isTRUE(.gr_opts$journal)
  cw <- .gr_opts$cell_w %||% GR_BAR_MIN_W
  nb_pt <- if (journal && !is.null(nbox) && length(nbox))
    tryCatch(gr_with_measure_device(cw, 4.9, function()
      grid::convertHeight(gr_lines(1.05 * length(nbox) + 0.2), "pt", valueOnly = TRUE)), error = function(e) 0)
    else 0
  plot_side <- function(q, hdr_pt = 0) {
    ps <- gr_panel_size(q + ggplot2::coord_fixed(xlim = lim, ylim = lim, clip = "off"), cw,
                        (.gr_opts$cell_h %||% 4.9) - (hdr_pt + nb_pt) / 72.27)
    if (is.null(ps)) NA_real_ else min(ps$w, ps$h)
  }
  # Two groups side by side need half the plot each: where it is drawn smaller
  # than designed (its own file at 12.69 cm), a column's widest line ran into
  # the other column - the groups then get two full-width lines each, as in a
  # narrow panel (the lines measured at their font size)
  if (!compact && !stacked && length(blocks) == 2) {
    wl <- tryCatch(gr_with_measure_device(cw, 4.9, function()
      max(vapply(unlist(lapply(blocks, function(b) b$lines)), function(t) hdr_text_pt(t, 2.85 * GR_PT_PER_MM),
                 numeric(1)))), error = function(e) NA_real_)
    side0 <- plot_side(p)
    if (is.finite(wl) && is.finite(side0) && wl > 0.5 * side0 - 6) {
      blocks <- unlist(lapply(blocks, function(b)
        lapply(b$short2, function(t) list(col = b$col, lines = t))), recursive = FALSE)
      stacked <- TRUE
      nlines <- length(blocks)
      p <- p + ggplot2::theme(plot.margin = ggplot2::margin(t = 26 + 13 * nlines, r = 10, b = 4, l = 6))
    }
  }
  hdr_line <- function(fx, fy, lab, col, size) {
    mk <- startsWith(lab, "● ")
    c(if (mk) list(ggplot2::annotate("point", x = dx(fx + 2.6 * pt_f), y = dy(fy - 2.6 * pt_f),
                                     colour = col, shape = 19, size = size * 0.5)),
      list(ggplot2::annotate("text", x = dx(fx), y = dy(fy),
                             label = if (mk) paste0("    ", substring(lab, 3)) else lab,
                             colour = col, hjust = 0, vjust = 0, size = size, fontface = "bold")))
  }
  for (ci in seq_along(blocks)) {
    if (stacked) {
      p <- p + hdr_line(0, -(HDR_BASE + 0.058 * (nlines - ci)), blocks[[ci]]$lines,
                        blocks[[ci]]$col, 2.6)
      next
    }
    fx <- if (length(blocks) == 1) 0 else (ci - 1) / length(blocks)
    for (li in seq_along(blocks[[ci]]$lines))
      p <- p + hdr_line(fx, -(HDR_BASE + 0.058 * (nlines - li)), blocks[[ci]]$lines[li],
                        blocks[[ci]]$col, 2.85)
  }
  # Over/Undercorrected corner boxes (dx/dy already honour `reverse`). A long
  # corner label is broken into as few lines (up to three) as keep it in its
  # corner of the square plot as it is drawn - its width and height together
  # at most 0.6 of the plot's side, so it stays off the eyes and the reference
  # lines along the diagonal (finding F13: on one line it reached across the
  # small plot of a journal width). The side is the plot's as drawn
  # (plot_side); when it cannot be measured, three lines at a reduced text
  # scale and one at the designed size.
  side <- plot_side(p, if (compact) gr_sp(8) + hdr_ls() * nlines else 0)
  cfs <- gr_pt(2.9 * GR_PT_PER_MM)            # the corner labels' font size (pt)
  corner_labels <- vapply(corner_labels, function(s) {
    if (nchar(s) <= 16) return(s)
    if (!is.finite(side))
      return(if (compact) paste(strwrap(s, width = max(12, ceiling(nchar(s) / 3) + 6)), collapse = "\n") else s)
    for (k in 1:3) {
      t <- if (k == 1) s else paste(strwrap(s, width = ceiling(nchar(s) / k) + 4), collapse = "\n")
      ln <- strsplit(t, "\n", fixed = TRUE)[[1]]
      # width and height (pt): about 0.56 em a bold character, 1.2 em a line,
      # and the plate's padding
      if (length(ln) <= k && (max(nchar(ln)) * 0.56 + length(ln) * 1.2 + 0.6) * cfs <= 0.6 * side) return(t)
    }
    t
  }, character(1))
  # on a semi-transparent plate (finding F13): the eyes and reference lines
  # under a corner label stay visible
  corner_fill <- grDevices::adjustcolor("white", alpha.f = 0.6)
  p <- p +
    ggplot2::annotate("label", x = dx(0.03), y = dy(0.05), label = corner_labels[1],
                      hjust = 0, vjust = 1, size = gr_mm(2.9), fontface = "bold", fill = corner_fill) +
    ggplot2::annotate("label", x = dx(0.97), y = dy(0.95), label = corner_labels[2],
                      hjust = 1, vjust = 0, size = gr_mm(2.9), fontface = "bold", fill = corner_fill)
  # Journal style (D20): the colored N-boxes go UNDER the plot, across the
  # figure (as on the horizontal bar panels, nbox_stack_grob), so they never
  # cover a data point; the annotated style keeps them inside, stacked under
  # the top-left corner box
  if (!journal && !is.null(nbox) && length(nbox))
    for (i in seq_along(nbox))
      p <- p + ggplot2::annotate("label", x = dx(0.03), y = dy(0.12 + 0.085 * i),
                                 label = names(nbox)[i], colour = unname(nbox[i]),
                                 fill = "white", hjust = 0, vjust = 1, size = 2.7,
                                 fontface = "bold", linewidth = .5)
  p <- p + ggplot2::coord_fixed(xlim = lim, ylim = lim, clip = "off")
  # the compact header across the whole cell (the square plot is narrower)
  if (compact) return(gr_add_nbox_rows(gr_header_over(p, hs, gr_sp(8) + hdr_ls() * nlines),
                                       if (journal) nbox else NULL))
  if (!journal || is.null(nbox) || !length(nbox)) return(p)
  gr_add_nbox_rows(ggplot2::ggplotGrob(p), nbox)
}

# attempted = preop SEQ - intended target (tgt: per-eye target SEQ, 0 = plano);
# achieved = preop SEQ - postop SEQ, so the identity line still means "on target".
# achieved - attempted = target - postop SEQ, so which side of the identity is
# "overcorrected" depends on the direction of the correction:
#   all attempted <= 0 (myopic)  : axes reversed (the published myopic layout);
#                                  an undercorrected myope plots below the line
#   all attempted >= 0 (hyperopic): normal axes; an undercorrected hyperope
#                                  (achieved < attempted) plots below the line
#   mixed                         : normal axes and direction-neutral corner
#                                  labels, since "over" means opposite sides for
#                                  myopes and hyperopes
g_attempted_achieved <- function(pseq, poseq, gl, glv, pal, tgt = 0) {
  att <- gr_num(pseq) - tgt; ach <- gr_num(pseq) - gr_num(poseq)
  ok <- is.finite(att) & is.finite(ach) & !is.na(gl)
  att_ok <- att[ok]
  myopic <- all(att_ok <= 0)
  hyperopic <- !myopic && all(att_ok >= 0)
  corners <- if (myopic || hyperopic) c("Overcorrected", "Undercorrected")
             else c("Postop more myopic than target", "Postop more hyperopic than target")
  nb <- make_nbox(gl[ok], glv, pal, total = gl)
  scatter_ind(data.frame(x = att[ok], y = ach[ok], Group = factor(gl[ok], levels = glv)),
              pal[glv], "Attempted SEQ (D)", "Achieved SEQ (D)",
              "Attempted vs Achieved Spherical Equivalent",
              reverse = myopic, stat_kind = "meanrange", nbox = nb,
              corner_labels = corners)
}

# Defocus equivalent = |postop SEQ - target| + |postop cylinder| / 2: the SEQ
# term is the error against the intended SEQ (`tgt`, per eye, from a mapped
# target_seq; 0 = plano, which gives the classic |SEQ| + |cyl| / 2).
# has_target only changes the header wording.
g_deq <- function(poseq, ocyl, gl, glv, pal, nb = TRUE, tgt = 0, has_target = FALSE) {
  # needs both: NA unless SEQ and cylinder exist
  deq <- abs(gr_num(poseq) - tgt) + abs(gr_num(ocyl)) / 2
  dlab <- if (has_target) "Postop DEQ (SEQ vs target)" else "Postop DEQ"
  nb <- panel_nbox(nb, is.finite(deq), gl, glv, pal)
  cuts <- c(0.25, 0.5, 0.75, 1, 2)
  rows <- do.call(rbind, lapply(glv, function(g) {
    x <- deq[gl == g]
    data.frame(label = paste0("≤ ", sprintf("%.2f", cuts)),
               pct = vapply(cuts, function(cc) mean(x <= cc + 1e-9, na.rm = TRUE) * 100, numeric(1)),
               series = g)
  }))
  ann <- lapply(glv, function(g) list(col = pal[[g]],
    text = sprintf("%s: %s %s D", g, dlab, msd(deq[gl == g]))))
  pp <- p_group_text(deq, gl, glv)
  for (.t in pp) if (nzchar(.t)) ann <- c(ann, list(list(col = "black", text = .t)))
  hs <- NULL
  if (length(glv) == 2) {
    cols <- unname(pal[glv]); dq <- vapply(glv, function(g) msd(deq[gl == g]), character(1))
    pdl <- p_d_text(deq, gl, glv, pp)
    hs <- list(list(x = 0.01, lines = c(list(
      list(seg(paste0(dlab, ":"))),
      vs_line("", paste0(dq[1], " D"), paste0(dq[2], " D"), cols)),
      if (nzchar(pdl)) list(list(seg(pdl))) else list())))
  }
  # cumulative bars fill the panel, so the legend goes at the bottom (as in ref)
  gr_bars(rows, pal[glv], "Defocus Equivalent (D)", "Cumulative % of eyes",
          "Defocus Equivalent Accuracy", ann, nbox = nb, header_spec = hs, legend_pos = "bottom",
          caution = gr_caution(pp))
}

g_astigmatism <- function(pcyl, ocyl, gl, glv, pal, second_col = NULL, nb = TRUE) {
  labs <- c("≤ 0.25", "0.26 to 0.50", "0.51 to 0.75", "0.76 to 1.00", "1.01 to 1.25",
            "1.26 to 1.50", "1.51 to 2.00", "2.01 to 3.00", "> 3.00")
  brk <- c(-Inf, .255, .505, .755, 1.005, 1.255, 1.505, 2.005, 3.005, Inf)
  rows <- list(); pal2 <- c(); ann <- list()
  post <- abs(gr_num(ocyl)); pre <- abs(gr_num(pcyl))
  # preop and postop bars describe the SAME eyes (those with both values), so
  # the N box is the denominator of every bar and the two series are comparable
  both <- is.finite(post) & is.finite(pre)
  post[!both] <- NA_real_; pre[!both] <- NA_real_
  nb <- panel_nbox(nb, both, gl, glv, pal)
  for (g in glv) {
    nm_po <- paste0(g, " Postop"); nm_pr <- paste0(g, " Preop")
    bpo <- pct_by_group(post, gl, g, brk, labs); bpo$series <- nm_po
    bpr <- pct_by_group(pre, gl, g, brk, labs); bpr$series <- nm_pr
    rows[[length(rows) + 1]] <- bpo; rows[[length(rows) + 1]] <- bpr
    pal2[nm_po] <- pal[[g]]; pal2[nm_pr] <- second_col %||% lighten_col(pal[[g]])
    ann[[length(ann) + 1]] <- list(col = pal[[g]], text = sprintf(
      "%s: Postop RA %s D · ≤ 0.25 D %s · ≤ 0.50 D %s · ≤ 1.00 D %s", g, msd(post[gl == g]),
      gr_pct(post[gl == g] <= .25 + 1e-9), gr_pct(post[gl == g] <= .5 + 1e-9), gr_pct(post[gl == g] <= 1 + 1e-9)))
  }
  pp <- p_group_text(post, gl, glv)
  for (.t in pp) if (nzchar(.t)) ann <- c(ann, list(list(col = "black", text = .t)))
  hs <- NULL
  if (length(glv) == 2) {
    cols <- unname(pal[glv])
    leftlines <- lapply(c(0.25, 0.5, 0.75, 1.0), function(cc) {   # JCRS 2025 intervals
      pc <- vapply(glv, function(g) gr_pct(post[gl == g] <= cc + 1e-9, 1), character(1))
      vs_line(sprintf("Cyl ≤ %.2f D: ", cc), pc[1], pc[2], cols)
    })
    ra <- vapply(glv, function(g) msd(post[gl == g]), character(1))
    rightlines <- c(list(list(seg("Postop RA:")),
      vs_line("", paste0(ra[1], " D"), paste0(ra[2], " D"), cols)),
      p_d_lines(post, gl, glv, pp))
    hs <- list(list(x = 0.01, lines = leftlines), list(x = "auto", lines = rightlines))
    # short form (R2-2): the mean of each cohort and the test
    pdl <- p_d_text(post, gl, glv, pp)
    hs_short <- list(list(x = 0.01, lines = c(list(
      vs_line("Postop RA: ", paste0(ra[1], " D"), paste0(ra[2], " D"), cols)),
      if (nzchar(pdl)) list(list(seg(pdl))) else list())))
  }
  gr_bars(do.call(rbind, rows), pal2, "Refractive Astigmatism (D)", "% of eyes",
          "Refractive Astigmatism", ann, label_size = 2.2, x_angle = 45, nbox = nb, header_spec = hs,
          caution = gr_caution(pp), short_spec = if (!is.null(hs)) hs_short)
}

g_tia_sia <- function(av, gl, glv, pal) {
  ok <- tia_ok(av$tia) & is.finite(av$sia) & !is.na(gl)
  nb <- make_nbox(gl[ok], glv, pal, suffix = "eyes with TIA ≥ 0.25 D", total = gl)
  scatter_ind(data.frame(x = av$tia[ok], y = av$sia[ok], Group = factor(gl[ok], levels = glv)),
              pal[glv], "TIA Vector (D)", "SIA Vector (D)",
              "Target Induced vs Surgically Induced Astigmatism", from_zero = TRUE,
              reverse = FALSE, stat_kind = "tiasia", nbox = nb)
}

g_ci_hist <- function(av, gl, glv, pal) {
  keep <- tia_ok(av$tia)   # TIA >= 0.25 D (V19 guard)
  ci <- ifelse(keep, av$sia / av$tia, NA)
  # right-closed bins, labelled as such with two decimals (decision D22); a
  # ratio that lands on an edge (0.30, 0.60 ...) up to floating-point noise is
  # snapped first, so it falls in the bin its label names (D27)
  labs <- c("≤ 0.15", "0.16 to 0.30", "0.31 to 0.60", "0.61 to 0.90", "0.91 to 1.10",
            "1.11 to 1.60", "1.61 to 2.10", "2.11 to 2.50", "> 2.50")
  brk <- c(-Inf, .15, .3, .6, .9, 1.1, 1.6, 2.1, 2.5, Inf)
  dfb <- pct_by_group(round(ci, 6), gl, glv, brk, labs)
  ann <- lapply(glv, function(g) list(col = pal[[g]], text = sprintf(
    "%s: CI %s (N = %d, TIA ≥ 0.25 D)", g, msd(ci[gl == g]), sum(is.finite(ci) & gl == g, na.rm = TRUE))))
  pp <- p_group_text(ci, gl, glv, max_chars = HBAR_MAX_CHARS)
  for (.t in pp) if (nzchar(.t)) ann <- c(ann, list(list(col = "black", text = .t)))
  hl <- hr <- nb <- NULL
  if (length(glv) == 2) {
    cols <- unname(pal[glv]); cim <- vapply(glv, function(g) msd(ci[gl == g]), character(1))
    hl <- vs_line("Correction Index: ", cim[1], cim[2], cols)
    hr <- p_d_text(ci, gl, glv, pp)
    nb <- make_nbox(ifelse(is.finite(ci), gl, NA), glv, pal, suffix = "eyes with TIA ≥ 0.25 D",
                    total = gl)
  }
  gr_bars(dfb, pal[glv], "Correction Index", "% of eyes", "Correction Index",
          ann, horizontal = TRUE, nbox = nb, hbar_left = hl, hbar_right = hr,
          caution = gr_caution(pp))
}

g_angle_of_error <- function(av, gl, glv, pal) {
  keep <- tia_ok(av$tia)   # TIA >= 0.25 D (V19 guard)
  # snapped to 1e-6 degree (decision D27): whole-degree axes put many eyes
  # exactly on a bin edge, which must not depend on the last floating-point bit
  aoe <- round(ifelse(keep, av$aoe, NA), 6)
  labs <- c("< -45", "-45 to -35", "-35 to -25", "-25 to -15", "-15 to -5", "-5 to 5",
            "5 to 15", "15 to 25", "25 to 35", "35 to 45", "> 45")
  brk <- c(-Inf, -45, -35, -25, -15, -5, 5, 15, 25, 35, 45, Inf)
  # AoE is signed: bin edges are inclusive toward zero on both sides
  dfb <- pct_by_group(aoe, gl, glv, brk, labs, symmetric = TRUE)
  ann <- lapply(glv, function(g) list(col = pal[[g]], text = sprintf(
    "%s: AoE %s° · |AoE| %s°", g, msd(aoe[gl == g], 1), msd(abs(aoe[gl == g]), 1))))
  pp <- p_group_text(aoe, gl, glv, max_chars = HBAR_MAX_CHARS)
  for (.t in pp) if (nzchar(.t)) ann <- c(ann, list(list(col = "black", text = .t)))
  hl <- hr <- nb <- NULL
  if (length(glv) == 2) {
    # the two-group form of the header: AoE and |AoE|, with the degree sign,
    # as the form with more groups (decision D22)
    cols <- unname(pal[glv])
    am <- vapply(glv, function(g) paste0(msd(aoe[gl == g], 1), "°"), character(1))
    ab <- vapply(glv, function(g) paste0(msd(abs(aoe[gl == g]), 1), "°"), character(1))
    # two rotated lines: one would run past the panel's height
    hl <- list(vs_line("AoE: ", am[1], am[2], cols), vs_line("|AoE|: ", ab[1], ab[2], cols))
    hr <- p_d_text(aoe, gl, glv, pp)
    nb <- make_nbox(ifelse(is.finite(aoe), gl, NA), glv, pal, suffix = "eyes with TIA ≥ 0.25 D",
                    total = gl)
  }
  gr_bars(dfb, pal[glv], "Angle of Error (degrees)", "% of eyes",
          "Refractive Astigmatism Angle of Error", ann, horizontal = TRUE,
          nbox = nb, hbar_left = hl, hbar_right = hr, caution = gr_caution(pp))
}

# ============================================================
# SEQ stability over time
# Reference behaviour (Reinstein Standard-Nine template block "6. Stability"
# and mEYEstro graph 11): mean SEQ +/- SD at each follow-up, n at every point,
# an optional preoperative point, and the percentage of eyes whose SEQ changed
# by more than <thresh> D between two nominated visits (computed PAIRWISE on
# eyes seen at both).
#
# visits: list of list(label=, time=, seq=, sph=, cyl=) with CLEAN column names.
#         SEQ is used when supplied, otherwise derived as sphere + cylinder/2.
# style : "meyestro"  -> groups overlaid, tinted value boxes, N rows beneath
#         "reinstein" -> single cohort, grey value boxes, n inside the tick label
# ============================================================
stability_long <- function(df, visits, gl, glv, pre_seq = NULL, logger = NULL) {
  rows <- list()
  seq_of <- function(v) {
    if (!is.na(v$seq) && v$seq %in% names(df)) {
      list(val = gr_num(df[[v$seq]]), src = "supplied")
    } else if (!is.na(v$sph) && !is.na(v$cyl) &&
               all(c(v$sph, v$cyl) %in% names(df))) {
      list(val = gr_num(df[[v$sph]]) + gr_num(df[[v$cyl]]) / 2, src = "computed")
    } else NULL
  }
  if (!is.null(pre_seq)) {
    pv <- gr_num(pre_seq)
    rows[[length(rows) + 1]] <- data.frame(
      eye = seq_along(pv), months = 0, label = "Pre", ord = 0,
      seq = pv, grp = gl, stringsAsFactors = FALSE)
  }
  for (i in seq_along(visits)) {
    v <- visits[[i]]
    s <- seq_of(v)
    if (is.null(s) || !(v$time %in% names(df))) next
    tt <- gr_num(df[[v$time]])
    ok <- is.finite(s$val) & is.finite(tt)
    if (!any(ok)) next
    # x position = median follow-up of the eyes actually seen at this visit
    mo <- stats::median(tt[ok], na.rm = TRUE) / 30.44
    # the label is display text only (a blank one shows the rounded median
    # month); visits are keyed by `ord`, their order, so a blank or repeated
    # label can neither merge two visits nor stop the figure
    lab <- as.character(v$label %||% "")[1]
    if (is.na(lab) || !nzchar(trimws(lab))) lab <- sprintf("%.0f", mo)
    if (!is.null(logger))
      logger(sprintf("Stability visit '%s': %d eyes, median %.1f mo, SEQ %s.",
                     lab, sum(ok), mo, s$src))
    rows[[length(rows) + 1]] <- data.frame(
      eye = seq_along(s$val), months = mo, label = lab, ord = i,
      seq = ifelse(ok, s$val, NA_real_), grp = gl, stringsAsFactors = FALSE)
  }
  if (!length(rows)) return(NULL)
  d <- do.call(rbind, rows)
  d <- d[!is.na(d$grp) & d$grp %in% glv, , drop = FALSE]
  if (!is.null(logger)) {
    # the x axis is real follow-up time, so two visits with the SAME median day
    # sit on one tick (their labels are joined there); say so rather than let
    # one visit seem to vanish. Visits that are merely close keep their own x.
    vm <- unique(d[d$ord > 0, c("ord", "label", "months")])
    mk <- stab_snap(vm$months)
    for (x in unique(mk[duplicated(mk)]))
      logger(sprintf("WARNING: stability visits %s share the same median follow-up (%.0f days) and are drawn at one x position.",
                     paste(sprintf("'%s'", vm$label[mk == x]), collapse = ", "), x * 30.44))
    # a shared label is only a display matter now, but the change pair is
    # given by label: say which visit it means
    va <- unique(d[, c("ord", "label")]); va <- va[order(va$ord), , drop = FALSE]
    for (lb in unique(va$label[duplicated(va$label)]))
      logger(sprintf("Stability: %d time points are labeled '%s' - each is kept as its own point; a change pair naming '%s' uses the %s.",
                     sum(va$label == lb), lb, lb,
                     if (va$ord[va$label == lb][1] == 0) "preoperative point" else "first of them"))
  }
  d
}

# Every visit is drawn at its own median follow-up. Only visits whose medians
# are the same day (equal up to floating noise, `tol` months) share one x, and
# then one tick naming them all. Visits that are merely close - day 1 and week
# 1 on a five-year axis - are never merged or moved: only their TEXT (tick
# labels, N rows, value labels) is spread apart, see stab_axis_grob.
stab_snap <- function(m, tol = 1e-6) {
  if (length(m) < 2) return(m)
  u <- sort(unique(m))
  cl <- cumsum(c(TRUE, diff(u) > tol))
  first <- tapply(u, cl, function(z) z[1])
  unname(first[cl[match(m, u)]])
}

# the time point (ord) a change-pair label names: the first, in visit order
# (the preoperative point first), whose label matches
stability_ord_of <- function(long, lab) {
  lab <- trimws(as.character(lab %||% "")[1])
  if (is.na(lab) || !nzchar(lab)) return(NA_real_)
  o <- sort(unique(long$ord[trimws(long$label) == lab]))
  if (length(o)) o[1] else NA_real_
}

# paired % of eyes changing by more than `thresh` D between two time points,
# named by their labels and resolved to visit order (stability_ord_of)
stability_change <- function(long, from_lab, to_lab, thresh, g) {
  oa <- stability_ord_of(long, from_lab); ob <- stability_ord_of(long, to_lab)
  if (is.na(oa) || is.na(ob) || oa == ob) return(NULL)
  a <- long[long$grp == g & long$ord == oa, c("eye", "seq")]
  b <- long[long$grp == g & long$ord == ob, c("eye", "seq")]
  if (!nrow(a) || !nrow(b)) return(NULL)
  m <- merge(a, b, by = "eye", suffixes = c(".a", ".b"))
  m <- m[is.finite(m$seq.a) & is.finite(m$seq.b), , drop = FALSE]
  if (!nrow(m)) return(NULL)
  list(pct = 100 * mean(abs(m$seq.b - m$seq.a) > thresh), n = nrow(m))
}

g_stability <- function(df, visits, gl, glv, pal, pre_seq = NULL,
                        change_from = "", change_to = "", thresh = 0.50,
                        style = "meyestro", logger = NULL) {
  long <- stability_long(df, visits, gl, glv, pre_seq, logger)
  if (is.null(long) || !nrow(long)) stop("no usable stability visits")
  # the summary and the paired "% changed" are keyed by visit ORDER (ord; 0 =
  # preoperative), never by label: a blank or repeated label is display text and
  # can neither merge two visits nor stop the figure

  # per group x visit summary; a point needs >= 3 eyes to be meaningful
  key <- interaction(long$grp, long$ord, drop = TRUE)
  sm <- do.call(rbind, lapply(split(long, key), function(d) {
    v <- d$seq[is.finite(d$seq)]
    if (length(v) < 3) return(NULL)
    data.frame(grp = d$grp[1], label = d$label[1], ord = d$ord[1],
               months = d$months[1], mean = mean(v), sd = stats::sd(v),
               n = length(v), stringsAsFactors = FALSE)
  }))
  if (is.null(sm) || !nrow(sm)) stop("no visit with at least 3 eyes in a group")
  sm <- sm[order(sm$ord), , drop = FALSE]
  sm$grp <- factor(sm$grp, levels = glv)

  # the preoperative point sits to the LEFT of the first follow-up; give it real
  # separation or its tick label collides with the 1-month one
  post_mo <- sm$months[sm$ord > 0]
  if (any(sm$ord == 0) && length(post_mo))
    sm$months[sm$ord == 0] <- -max(post_mo, na.rm = TRUE) * 0.075
  # visits with the same median day share one x (and one tick); every other
  # visit stays at its own median follow-up, however close (stab_snap)
  sm$months <- stab_snap(sm$months)

  xs <- sort(unique(sm$months))
  span <- max(diff(range(xs)), 1)
  k <- length(glv)
  # nudge groups apart so overlapping error bars stay readable - by at most a
  # fraction of the room to the neighbouring time point, so a dodged point never
  # crosses towards another visit, and with error-bar caps no wider than that
  # room (day 1 and week 1 on a five-year axis stay two distinct points)
  gaps <- diff(xs)
  room <- vapply(seq_along(xs), function(i)
    min(c(if (i > 1) gaps[i - 1], if (i < length(xs)) gaps[i], span)), numeric(1))
  ix <- match(sm$months, xs)
  dodge <- pmin(span * 0.012, 0.3 * room / max(k - 1, 1))
  sm$xpos <- sm$months + (as.integer(sm$grp) - (k + 1) / 2) * dodge[ix]
  sm$ew <- pmin(span * 0.018, 0.5 * room[ix])

  # one tick per x; visits that share a median day share the tick, which names
  # them all ("M3 / M3-repeat"), one label per visit in visit order, and lists
  # each one's n ("40/38")
  labs <- vapply(xs, function(x) {
    v <- unique(sm[abs(sm$months - x) < 1e-9, c("ord", "label")])
    paste(v$label[order(v$ord)], collapse = " / ")
  }, character(1))
  ns_by_x <- lapply(glv, function(g) {
    s <- sm[sm$grp == g, ]
    vapply(xs, function(x) { i <- which(abs(s$months - x) < 1e-9)
      if (length(i)) paste(s$n[i], collapse = "/") else "" }, character(1))
  })
  names(ns_by_x) <- glv

  rein <- identical(style, "reinstein")
  if (rein) {                       # n lives inside the tick label
    labs <- paste0(labs, "\n(", ns_by_x[[glv[1]]], ")")
  }

  lo <- min(sm$mean - sm$sd, na.rm = TRUE); hi <- max(sm$mean + sm$sd, na.rm = TRUE)
  pad <- max((hi - lo) * 0.14, 0.25)

  # value label position at every point: the highest group at each time sits
  # above its point, every other one below its own point - and never closer than
  # `gap` to the label above it, so with 3+ groups whose means are close the
  # labels stack downward instead of overprinting
  off <- pad * 0.55; gap <- pad * 0.5
  sm$ly0 <- sm$mean - off                      # undisplaced position (below)
  sm$ly <- sm$ly0
  for (x in xs) {
    i <- which(abs(sm$months - x) < 1e-9)
    if (length(i) < 2) next
    o <- i[order(sm$mean[i], decreasing = TRUE)]
    sm$ly[o[1]] <- sm$ly0[o[1]] <- sm$mean[o[1]] + off
    prev <- NA_real_
    for (j in o[-1]) {
      y <- sm$ly0[j]
      if (!is.na(prev) && (prev - y) < gap) y <- prev - gap
      sm$ly[j] <- y; prev <- y
    }
  }

  ylo <- min(lo - pad, min(sm$ly) - pad * 0.45)  # bottom of the data area
  yhi <- max(hi + pad, max(sm$ly) + pad * 0.45)

  # The tick labels and the mEYEstro N rows (one per group, "N =" in the corner
  # under the y axis) are drawn by stab_axis_grob beneath the axis, where they
  # are measured in the panel they land in and spread apart when ticks are close.
  # ggplot keeps the ticks at the true times and reserves the room: its own tick
  # labels are drawn transparent, with margins for the leaders and the N rows.
  n_rows <- if (rein) 0 else k
  p <- ggplot2::ggplot(sm, ggplot2::aes(x = xpos, y = mean, colour = grp, group = grp)) +
    ggplot2::geom_hline(yintercept = 0, colour = "grey55", linewidth = .4) +
    ggplot2::geom_vline(xintercept = xs, colour = "grey88", linewidth = .3,
                        linetype = if (rein) "dashed" else "solid") +
    ggplot2::geom_errorbar(ggplot2::aes(ymin = mean - sd, ymax = mean + sd, width = ew),
                           linewidth = .55) +
    ggplot2::geom_line(linewidth = .7) +
    ggplot2::geom_point(size = 1.9) +
    ggplot2::scale_colour_manual(values = pal[glv]) +
    ggplot2::scale_x_continuous(breaks = xs, labels = labs) +
    ggplot2::scale_y_continuous(expand = ggplot2::expansion(mult = c(0.01, 0.03))) +
    # the standard graph's axis title, in every style (decision D22)
    ggplot2::labs(x = "Time after surgery (months)", y = "Mean ± SD SEQ (D)") +
    gr_theme() +
    ggplot2::theme(legend.position = "none",
                   panel.grid.major.x = ggplot2::element_blank(),
                   panel.grid.minor = ggplot2::element_blank(),
                   axis.text.x = ggplot2::element_text(
                     colour = "transparent", lineheight = STAB_AX_LH,
                     margin = ggplot2::margin(t = stab_sizes()$top, b = gr_sp(2) + stab_sizes()$nrow * n_rows)))
  # Explicit limits, so the value labels and the axis text (drawn by grobs that
  # map data to the panel themselves) land exactly where ggplot would put them:
  # x = the range of everything drawn plus 5% each side, y = c(ylo, yhi)
  # widened by the scale's 1% / 3% expansion.
  x_rng <- range(c(xs, sm$xpos - sm$ew / 2, sm$xpos + sm$ew / 2))
  xl <- x_rng + c(-1, 1) * 0.05 * diff(x_rng)
  yl <- c(ylo - 0.01 * (yhi - ylo), yhi + 0.03 * (yhi - ylo))
  p <- p + ggplot2::coord_cartesian(xlim = xl, ylim = yl, expand = FALSE, clip = "off")
  # value label at every point, tinted per group (mEYEstro) or grey (Reinstein);
  # + 0 turns a rounded negative zero back into 0, so it prints "0.00" not "-0.00"
  lab_df <- data.frame(
    x = sm$xpos, y = sm$mean, ly = sm$ly, ly0 = sm$ly0, t = sm$months,
    lab = sprintf("%.2f", round(sm$mean, 2) + 0),
    col = if (rein) "black" else unname(pal[as.character(sm$grp)]),
    fill = if (rein) "grey92" else vapply(unname(pal[as.character(sm$grp)]), lighten_col,
                                         character(1), f = 0.78),
    lead = vapply(unname(pal[as.character(sm$grp)]), lighten_col, character(1), f = 0.35),
    stringsAsFactors = FALSE)

  # header: the "% changed > x D" statistic, per group
  chg <- lapply(glv, function(g) stability_change(long, change_from, change_to, thresh, g))
  names(chg) <- glv
  # the pair as printed: bare numbers ("6", "24") are months and get the
  # unit; a label that names its own time ("1 month") is printed as is
  num_lab <- function(x) grepl("^\\s*[0-9]+([.,][0-9]+)?\\s*$", as.character(x %||% ""))
  pair_txt <- function(unit) paste0(change_from, "-", change_to,
                                    if (num_lab(change_from) && num_lab(change_to)) paste0(" ", unit) else "")
  hs <- list()
  if (length(glv) > 2) {
    # 3+ groups: one COMPACT line each, stacked. Side-by-side columns would run
    # off the right edge of a narrow composite cell and be clipped.
    lines <- lapply(seq_along(glv), function(j) {
      g <- glv[j]; gcol <- unname(pal[g]); c1 <- chg[[g]]
      list(seg(if (is.null(c1)) paste0("● ", g)
               else sprintf("● %s: %.1f%% changed > %.2f D (%s)",
                            g, c1$pct, thresh, pair_txt("mo")), gcol))
    })
    hs[[1]] <- list(x = 0.01, lines = lines)
  } else for (j in seq_along(glv)) {
    g <- glv[j]; gcol <- unname(pal[g]); c1 <- chg[[g]]
    lines <- list(list(seg(paste0("● ", g), gcol)))
    if (!is.null(c1)) {
      lines <- c(lines, list(
        list(seg(sprintf("%% changed > %.2f D", thresh), gcol)),
        list(seg(sprintf("%s: %.1f%%", pair_txt("months"), c1$pct), gcol))))
    }
    hs[[length(hs) + 1]] <- list(x = if (j == 1) 0.01 else "auto", lines = lines)
  }
  p <- p + ggplot2::theme(
    plot.margin = ggplot2::margin(t = gr_sp(10) + hdr_ls() * hdr_nlines(hs), r = gr_sp(12),
                                  b = gr_sp(if (rein) 4 else 10), l = gr_sp(6)))
  gt <- attach_header(p, hs)
  pan <- gt$layout[grepl("^panel", gt$layout$name), , drop = FALSE]
  gt <- gtable::gtable_add_grob(gt, stab_label_grob(lab_df, xl, yl),
                                t = min(pan$t), l = min(pan$l), b = max(pan$b), r = max(pan$r),
                                clip = "off", name = "stab-labels", z = 50)
  ax <- gt$layout[gt$layout$name == "axis-b", , drop = FALSE]
  if (!nrow(ax)) return(gt)
  gtable::gtable_add_grob(gt, stab_axis_grob(xs, labs, if (rein) list() else unname(ns_by_x[glv]),
                                             unname(pal[glv]), xl),
                          t = min(ax$t), b = max(ax$b), l = min(pan$l), r = max(pan$r),
                          clip = "off", name = "stab-axis", z = 60)
}

# ------------------------------------------------------------
# Stability x axis text: one COLUMN per time point - its tick label and, below
# it, the N of each group (mEYEstro; "N =" sits in the corner under the y axis,
# clear of the axis labels). Drawn in the axis row ggplot reserved (its own tick
# labels are transparent), measured at draw time in the space it really gets
# (a single figure or a narrow composite cell). Columns of ticks that are too
# close to print apart are spread horizontally just enough not to touch, in
# order, and joined to their own tick by a thin leader; the ticks, points and
# error bars never move.
# ------------------------------------------------------------
STAB_AX_FS <- 9.5      # tick label font size, as axis.text in gr_theme
STAB_AX_LH <- 0.9      # its line height (multi-line Reinstein labels)
STAB_TICK_PT <- 2.75   # axis tick length (theme_bw)
STAB_AX_TOP <- 7       # pt from the tick end to its label: room for a leader
STAB_NROW_PT <- 9      # pt per N row
STAB_COL_GAP <- 4      # pt kept between neighbouring columns

# The stability axis and value-label sizes (pt) at text scale `sc` - the
# gr_ts() of the panel being built (R2-2), kept in the grobs for draw time
stab_sizes <- function(sc = gr_ts()) {
  sc <- min(1, sc %||% 1)
  f <- function(pt) if (sc >= 1) pt else max(GR_MIN_TEXT_PT, pt * sc)
  list(ax = f(STAB_AX_FS), lab = f(STAB_LAB_FS), nrow = STAB_NROW_PT * f(STAB_LAB_FS) / STAB_LAB_FS,
       top = STAB_AX_TOP * sc, tick = STAB_TICK_PT * sc, gap = STAB_COL_GAP * sc,
       pad = STAB_LAB_PAD * sc, lgap = STAB_LAB_GAP * sc)
}

stab_axis_grob <- function(xs, labs, nrows, cols, xl)
  grid::gTree(xs = xs, labs = labs, nrows = nrows, cols = cols, xl = xl, sc = gr_ts(), cl = "stab_axis")

# Centre (pt) of each column, columns in tick order: as close to its tick `px`
# as possible with neighbouring columns (widths `w`) at least `gap` apart and
# all inside [lo, hi] - the least-squares placement, by isotonic regression of
# the ticks less the cumulative spacing (a column with room stays on its tick)
stab_col_place <- function(px, w, lo, hi, gap = STAB_COL_GAP) {
  n <- length(px)
  if (!n) return(numeric(0))
  cum <- c(0, cumsum((w[-n] + w[-1]) / 2 + gap))[seq_len(n)]
  y <- px - cum
  yf <- if (n > 1) stats::isoreg(seq_len(n), y)$yf else y
  y_lo <- lo + w[1] / 2; y_hi <- hi - w[n] / 2 - cum[n]
  yf <- if (y_lo > y_hi) rep(y_lo, n) else pmin(pmax(yf, y_lo), y_hi)
  yf + cum
}

makeContent.stab_axis <- function(x) {
  n <- length(x$xs)
  if (!n) return(x)
  vw <- grid::convertWidth(grid::unit(1, "npc"), "pt", valueOnly = TRUE)
  vh <- grid::convertHeight(grid::unit(1, "npc"), "pt", valueOnly = TRUE)
  px <- (x$xs - x$xl[1]) / diff(x$xl) * vw
  ss <- stab_sizes(x$sc %||% 1)
  gp_l <- grid::gpar(fontsize = ss$ax, fontface = "bold", lineheight = STAB_AX_LH)
  tw <- function(s, gp) grid::convertWidth(grid::grobWidth(grid::textGrob(s, gp = gp)),
                                           "pt", valueOnly = TRUE)
  w <- vapply(x$labs, tw, numeric(1), gp = gp_l)
  for (r in x$nrows) {
    gp_n <- grid::gpar(fontsize = ss$lab, fontface = "bold")
    w <- pmax(w, vapply(r, tw, numeric(1), gp = gp_n))
  }
  # the right edge may use part of the plot margin; the left one may not ("N =")
  lx <- stab_col_place(px, w + 2, lo = 0, hi = vw + 8 * min(1, x$sc %||% 1), gap = ss$gap)
  pt <- function(v) grid::unit(v, "pt")
  tick_end <- vh - ss$tick
  lab_top <- tick_end - ss$top
  kids <- grid::gList()
  moved <- which(abs(lx - px) > 0.5)
  if (length(moved))
    kids <- grid::gList(kids, grid::segmentsGrob(pt(px[moved]), pt(tick_end), pt(lx[moved]),
      pt(lab_top + 1), gp = grid::gpar(col = "grey45", lwd = 0.6)))
  kids <- grid::gList(kids, grid::textGrob(x$labs, pt(lx), pt(lab_top), just = c("centre", "top"),
                                           gp = gp_l))
  nr <- length(x$nrows)
  for (j in seq_len(nr)) {
    yj <- 2 * min(1, x$sc %||% 1) + (nr - j + 0.5) * ss$nrow
    gp_j <- grid::gpar(col = x$cols[j], fontsize = ss$lab, fontface = "bold")
    kids <- grid::gList(kids,
      grid::textGrob(x$nrows[[j]], pt(lx), pt(yj), gp = gp_j),
      grid::textGrob("N =", pt(-3), pt(yj), just = c("right", "centre"), gp = gp_j))
  }
  grid::setChildren(x, kids)
}
registerS3method("makeContent", "stab_axis", makeContent.stab_axis,
                 envir = asNamespace("grid"))

# ------------------------------------------------------------
# Stability value labels. Drawn as a grob that measures itself in the panel it
# lands in (a single figure or a narrower composite cell), so labels of
# neighbouring time points that sit close on the x axis (1 and 3 months) are
# pushed apart horizontally until they no longer touch. Labels at ONE time point
# are already stacked vertically by g_stability and are not moved against each
# other. A label moved off its point, or pushed down the stack, keeps a thin
# leader to it.
# lab: data frame x, y (point), ly (label centre), ly0 (undisplaced), t (time
#      key), lab, col, fill, lead (leader colour); xl / yl: the panel limits.
# ------------------------------------------------------------
# ggplot2's .pt (pt per mm), spelled out: nothing at top level may call into
# ggplot2, so this file sources without it and generate_standard_graphs can
# report the missing package instead of the engine failing at start
STAB_PT <- 72.27 / 25.4
STAB_LAB_FS <- 2.5 * STAB_PT        # = annotate(size = 2.5)
STAB_LAB_PAD <- 1.4                 # pt, as label.padding
STAB_LAB_GAP <- 2                   # pt kept between neighbouring labels

stab_label_grob <- function(lab, xl, yl) grid::gTree(lab = lab, xl = xl, yl = yl, sc = gr_ts(), cl = "stab_labels")

# horizontal shift (pt) per label so that no two labels of DIFFERENT time points
# overlap; the left one moves left and the right one right, half each
stab_label_shift <- function(cx, cy, w, h, t, gap = STAB_LAB_GAP) {
  n <- length(cx); dx <- numeric(n)
  if (n < 2) return(dx)
  for (it in seq_len(100)) {
    moved <- FALSE
    for (i in seq_len(n - 1)) for (j in (i + 1):n) {
      if (abs(t[i] - t[j]) < 1e-9) next
      if (abs(cy[i] - cy[j]) >= (h[i] + h[j]) / 2 + 1) next
      a <- if (cx[i] <= cx[j]) i else j; b <- if (a == i) j else i
      need <- (w[a] + w[b]) / 2 + gap - ((cx[b] + dx[b]) - (cx[a] + dx[a]))
      if (need <= 1e-6) next
      dx[a] <- dx[a] - need / 2; dx[b] <- dx[b] + need / 2; moved <- TRUE
    }
    if (!moved) break
  }
  dx
}

makeContent.stab_labels <- function(x) {
  d <- x$lab
  if (!nrow(d)) return(x)
  vw <- grid::convertWidth(grid::unit(1, "npc"), "pt", valueOnly = TRUE)
  vh <- grid::convertHeight(grid::unit(1, "npc"), "pt", valueOnly = TRUE)
  px <- (d$x - x$xl[1]) / diff(x$xl) * vw
  py <- (d$y - x$yl[1]) / diff(x$yl) * vh
  ly <- (d$ly - x$yl[1]) / diff(x$yl) * vh
  ss <- stab_sizes(x$sc %||% 1)
  gp_t <- grid::gpar(fontsize = ss$lab, fontface = "bold")
  tw <- vapply(d$lab, function(s) grid::convertWidth(grid::grobWidth(
    grid::textGrob(s, gp = gp_t)), "pt", valueOnly = TRUE), numeric(1))
  th <- grid::convertHeight(grid::grobHeight(grid::textGrob("0.00", gp = gp_t)), "pt", valueOnly = TRUE) +
    grid::convertHeight(grid::grobDescent(grid::textGrob("0.00", gp = gp_t)), "pt", valueOnly = TRUE)
  w <- tw + 2 * ss$pad; h <- rep(th + 2 * ss$pad, nrow(d))
  dx <- stab_label_shift(px, ly, w, h, d$t, gap = ss$lgap)
  lx <- px + dx
  pt <- function(v) grid::unit(v, "pt")
  kids <- grid::gList()
  # leaders first, so the labels sit on top of them
  lead <- which(d$ly < d$ly0 - 1e-9 | abs(dx) > w / 2)
  for (i in lead)
    kids <- grid::gList(kids, grid::segmentsGrob(pt(px[i]), pt(py[i]), pt(lx[i]), pt(ly[i]),
      gp = grid::gpar(col = d$lead[i], lwd = .3 * STAB_PT)))
  for (i in seq_len(nrow(d))) {
    kids <- grid::gList(kids,
      # a semi-transparent plate (finding F13): a box that lands on an error
      # bar, a marker or a line lets it show through
      grid::roundrectGrob(pt(lx[i]), pt(ly[i]), width = pt(w[i]), height = pt(h[i]),
                          r = grid::unit(0.15, "lines"),
                          gp = grid::gpar(fill = grDevices::adjustcolor(d$fill[i], alpha.f = 0.6), col = NA,
                                          fontsize = ss$lab)),
      grid::textGrob(d$lab[i], pt(lx[i]), pt(ly[i]),
                     gp = grid::gpar(col = d$col[i], fontsize = ss$lab, fontface = "bold")))
  }
  grid::setChildren(x, kids)
}
registerS3method("makeContent", "stab_labels", makeContent.stab_labels,
                 envir = asNamespace("grid"))

# ------------------------------------------------------------
# The magnitude scale of a double-angle plot: one bold label on a white plate
# per ring, laid out when the plot is drawn (finding F4), where the diagram's
# size is known - its viewport is the square of the outer ring
# (annotation_custom over -rmax ... rmax), so a diopter is vw / (2 rmax) pt
# and every size is measured, not estimated. The plates go straight DOWN the
# 135° spoke, on its right; it is drawn under the centroids, so when a
# centroid marker comes within 1 pt of a plate there, the scale goes to the
# first of the spoke's left, the 45° spoke's right (straight UP) or its left
# that no marker touches; when every one is touched (centroids close to zero),
# to the first where only the innermost plate is, which is then left out (the
# labels kept show the step between the rings); else to the one with the
# most room. Each plate is centred on its ring - the outermost moved in just
# enough to stay 1.5 pt clear of the spoke label beyond it, or left out when
# that would put it on the next one (a small diagram at a journal width).
# rings: the ring radii (D); cx, cy: the centroids (D); mark_size: their
# markers' ggplot size (pch 23, stroke 0.7: it reaches 1.25 x 0.375 of the
# symbol's font size from its centre, plus half its outline); spoke_fs,
# spoke_r: the spoke labels' font size (pt) and radius (x rmax).
# ------------------------------------------------------------
RING_SCALE_FS <- 3.0           # the ring labels' text size (ggplot mm)
ring_scale_grob <- function(rings, rmax, cx, cy, mark_size, spoke_fs, spoke_r)
  grid::gTree(rings = rings, rmax = rmax, cx = cx, cy = cy,
              mark_pt = 1.2533 * 0.375 * (mark_size * GR_PT_PER_MM + 0.7 * 96 / 25.4 / 2) + 0.5,
              spoke_fs = spoke_fs, spoke_r = spoke_r, fs = gr_pt(RING_SCALE_FS * GR_PT_PER_MM),
              pad = gr_sp(1.3), cl = "ring_scale")

makeContent.ring_scale <- function(x) {
  vw <- grid::convertWidth(grid::unit(1, "npc"), "pt", valueOnly = TRUE)
  vh <- grid::convertHeight(grid::unit(1, "npc"), "pt", valueOnly = TRUE)
  n <- length(x$rings)
  if (!n || !is.finite(vw) || vw <= 0) return(x)
  s <- vw / (2 * x$rmax)                       # pt per diopter
  gp_t <- grid::gpar(fontsize = x$fs, fontface = "bold")
  pt_w <- function(g) grid::convertWidth(grid::grobWidth(g), "pt", valueOnly = TRUE)
  pt_h <- function(g) grid::convertHeight(grid::grobHeight(g), "pt", valueOnly = TRUE)
  lab <- as.character(x$rings)
  w <- vapply(lab, function(l) pt_w(grid::textGrob(l, gp = gp_t)), numeric(1)) + 2 * x$pad
  h <- pt_h(grid::textGrob("0123456789", gp = gp_t)) + 2 * x$pad
  off <- 0.045 * x$rmax * s                    # the plates start this far beside the spoke
  yc <- x$rings * s
  keep <- rep(TRUE, n)
  # the outermost plate against the spoke label beyond it (centred at spoke_r)
  sh <- pt_h(grid::textGrob("135°", gp = grid::gpar(fontsize = x$spoke_fs, fontface = "bold")))
  lim <- x$spoke_r * x$rmax * s - sh / 2 - 1.5 - h / 2
  if (yc[n] > lim) {
    yc[n] <- lim
    if (yc[n] < h / 2 || (n > 1 && yc[n] - yc[n - 1] < h + 0.5)) keep[n] <- FALSE
  }
  # the room (pt) between the centroid markers and the plates `k`, down
  # (v = -1) or up (1) the vertical spoke, on its right (hz = 1) or left (-1)
  mx <- x$cx * s; my <- x$cy * s
  room <- function(v, hz, k) {
    k <- which(k)
    if (!length(mx) || !length(k)) return(Inf)
    xc <- hz * (off + w[k] / 2); yy <- v * yc[k]
    min(vapply(seq_along(mx), function(j)
      min(sqrt(pmax(0, abs(mx[j] - xc) - w[k] / 2)^2 + pmax(0, abs(my[j] - yy) - h / 2)^2)),
      numeric(1))) - x$mark_pt - 1
  }
  opts <- list(c(-1, 1), c(-1, -1), c(1, 1), c(1, -1))
  r_all <- vapply(opts, function(o) room(o[1], o[2], keep), numeric(1))
  pick <- which(r_all >= 0)
  inner <- keep; inner[1] <- FALSE
  if (!length(pick) && sum(inner) >= 2) {
    r_in <- vapply(opts, function(o) room(o[1], o[2], inner), numeric(1))
    if (any(r_in >= 0)) { pick <- which(r_in >= 0); keep <- inner }
  }
  o <- opts[[if (length(pick)) pick[1] else which.max(r_all)]]
  pt <- function(v) grid::unit(v, "pt")
  kids <- grid::gList()
  for (i in which(keep)) {
    xc <- vw / 2 + o[2] * (off + w[i] / 2); y0 <- vh / 2 + o[1] * yc[i]
    kids <- grid::gList(kids,
      grid::roundrectGrob(pt(xc), pt(y0), width = pt(w[i]), height = pt(h), r = grid::unit(1, "pt"),
                          gp = grid::gpar(fill = "white", col = "black", lwd = .18 * GR_PT_PER_MM)),
      grid::textGrob(lab[i], pt(xc), pt(y0), gp = gp_t))
  }
  grid::setChildren(x, kids)
}
registerS3method("makeContent", "ring_scale", makeContent.ring_scale,
                 envir = asNamespace("grid"))

# ============================================================
# JCRS-standard DOUBLE-ANGLE plot, all groups superimposed.
#
# Every point is placed at (M cos 2θ, M sin 2θ) with θ the PLUS axis (plus-
# cylinder format, decision D1), so the DISPLAYED angle is twice the axis and
# the plot is a full 360 degrees. This is what makes the vector arithmetic
# legitimate and groups with-the-rule / against-the-rule data on opposite
# sides instead of splitting each across the horizontal meridian:
#   plus axis  90 (WTR; minus axis 180) -> displayed 180 -> left
#   plus axis 0/180 (ATR; minus axis 90) -> displayed 0/360 -> right
#   plus axes 45 / 135 (oblique) -> displayed 90 / 270 -> top / bottom
# The spokes are labelled with the plus axis (half the displayed angle).
# Required by JCRS 2025 "Standards for analyzing astigmatic outcomes" (Part II).
# ============================================================

# 95% region of the eyes of one cohort (astig_region, decision R3-1):
#   "depth"  - distribution-free (default): the depth-based convex polygon as
#              the journal's reference tool (the WHWK functions) builds it
#              (see astig_region, astig_depth). Makes no normality assumption
#              (JCRS Part II).
#   "normal" - bivariate-normal ellipse from the 2x2 covariance:
#              {u : (u-c)' S^-1 (u-c) <= qchisq(.95, 2)}, drawn from the
#              eigen-decomposition (semi-axes sqrt(qchisq * lambda_i)).
# The 95% confidence region of the CENTROID is astig_centroid_region (R3-2).
# ============================================================
# Cluster-robust p-values for the figures.
# Two eyes of one patient are NOT independent. `.gr_opts$cluster_id` carries a
# patient id ALIGNED ELEMENTWISE with the group vector; when it is set the figure
# comparisons use a GEE (exchangeable working correlation, robust sandwich SE)
# instead of tests that assume independent eyes. Under matching weights that
# are not all 1 the GEE is weighted with an INDEPENDENCE working correlation,
# like the matched tables (contract R4-1). Cleared again on exit from
# generate_standard_graphs so it can never leak into a later call.
# ============================================================
.gr_opts$cluster_id <- NULL
# Matching weights of a matched pass, aligned elementwise with the group vector
# like the cluster id; NULL unless at least one weight differs from 1.
.gr_opts$weights <- NULL
# TRUE during a matched pass: its clustered GEE uses an independence working
# correlation, weighted or not (decision D18), like the matched tables.
.gr_opts$matched <- FALSE

# TRUE when a usable, correctly-aligned cluster id is available for `gl`
gr_cluster_of <- function(gl) {
  cid <- .gr_opts$cluster_id
  if (is.null(cid) || length(cid) != length(gl)) return(NULL)
  cid
}
gr_weights_of <- function(gl) {
  w <- .gr_opts$weights
  if (is.null(w) || length(w) != length(gl)) return(NULL)
  w
}

# Narrow the cluster ids and matching weights in scope to the rows `keep` of a
# vector aligned with `gl` (a test on a subset must see the subset's ids and
# weights, or gr_cluster_of / gr_weights_of no longer line up and the test
# silently stops clustering / weighting). Returns a function restoring the scope.
gr_narrow_scope <- function(gl, keep) {
  old_c <- .gr_opts$cluster_id; old_w <- .gr_opts$weights
  cid <- gr_cluster_of(gl); w <- gr_weights_of(gl)
  .gr_opts$cluster_id <- if (is.null(cid)) NULL else cid[keep]
  .gr_opts$weights <- if (is.null(w)) NULL else w[keep]
  function() { .gr_opts$cluster_id <- old_c; .gr_opts$weights <- old_w; invisible(NULL) }
}

# Blank / NA cluster ids are helpers.R fill_missing_ids() tokens "(no ID) row
# <n>": an eye without a patient id is its own cluster. Dropping it instead would
# test fewer eyes than the bars and centroids show, and leave it out of every
# bootstrap resample.

# Joint Wald test of the coefficients `idx` of a fitted geeglm, with the
# robust covariance and reference of the run's small-sample setting (helpers.R
# gee_wald: Mancl-DeRouen + F by default, decision D11; the uncorrected
# sandwich + chi-square with gee_small_sample "none"). NA - never a P value -
# when that covariance is numerically singular (contract R4-6; helpers.R
# gee_vcov_singular, the rule the weighted table test shares): a group of two
# patients adds only a rank-1 block, a group without between-patient spread
# adds none, and the statistic would explode into a spurious "P < 0.001". NA
# too when the statistic is not finite, the covariance cannot be computed or
# the model has no more patients than coefficients. The NA carries attr
# "singular" = TRUE for a singular covariance. The caller treats either NA as
# a model that could not be fitted.
# The group test of the figures, gee_group_p(), lives in helpers.R: the
# clustered table tests (analysis.R) run the same function.
gr_wald_p <- function(fit, idx) {
  wt <- gee_wald(fit, idx)
  if (isTRUE(wt$singular)) return(structure(NA_real_, singular = TRUE))
  wt$p
}

# ROBUST bivariate centroid comparison in the spirit of JCRS 2025 Part II (not
# its exact trimmed-Hotelling variant): a Hotelling-type statistic - the sum over
# groups of n_i * d_i' S^-1 d_i, d_i = a group's 20%-trimmed mean vector minus
# the grand mean, S = the pooled 20%-winsorized covariance - so outlying eyes
# cannot drag the centroid, calibrated by B = 999 BOOTSTRAP resamples (each
# group recentred on its trimmed mean; fixed seed) rather than an F reference.
# The bootstrap resamples WITHIN each cohort (statistics.md S12): every resample
# keeps every cohort with its own number of patients (eyes), so none is skipped
# for a lost cohort. When a cluster id is supplied it resamples PATIENTS, so
# the robustness and the clustering hold at the same time.
# Quantized data (decision R2-5): refractive astigmatism measured in quarter-
# dioptre steps often puts more than 60% of a cohort's eyes on one value, where
# the 20%-winsorized variance is zero and the trimmed statistic does not exist.
# Every robust between-cohort test (this centroid comparison, the precision and
# the mean magnitude) therefore runs through astig_boot_test() (the centroid
# against zero is the untrimmed projection-distance test of astig_zero_p since
# decision R3-2):
# the trimmed test is used only when its statistic exists in the data AND in at
# least ASTIG_TRIM_OK (95%) of the resamples; otherwise the same bootstrap test
# runs UNTRIMMED (ordinary means and covariance, patients resampled within the
# cohorts), labelled ASTIG_UNTRIMMED. Resampling continues until exactly B
# usable resamples (at most ASTIG_BOOT_MAX x B draws); short of that the test
# is not computed, with the reason. A P value always rests on exactly B usable
# resamples (attr "B"); attr "variant" is "trimmed" or "untrimmed".
ASTIG_TRIM <- 0.2
ASTIG_BOOT_B <- 999L
ASTIG_SEED <- 20250601L
ASTIG_TRIM_OK <- 0.95
ASTIG_BOOT_MAX <- 3L
ASTIG_UNTRIMMED <- "untrimmed bootstrap test (the trimmed statistic is not defined for these data)"

# Count the resamples whose statistic reaches the observed one, drawing until B
# usable resamples: list(ok, p = (count + 1) / (B + 1), eff, draws). ok is
# FALSE when more than `max_fail` draws were unusable (the statistic undefined)
# before B usable ones were found.
astig_boot_count <- function(obs, stat, draw, B, max_fail) {
  cnt <- 0L; eff <- 0L; fail <- 0L
  while (eff < B) {
    if (fail > max_fail) return(list(ok = FALSE, eff = eff, draws = eff + fail))
    st <- stat(draw())
    if (!is.finite(st)) { fail <- fail + 1L; next }
    eff <- eff + 1L
    if (st >= obs) cnt <- cnt + 1L
  }
  list(ok = TRUE, p = (cnt + 1) / (B + 1), eff = eff, draws = eff + fail)
}

# The trimmed bootstrap test, else the untrimmed one (decision R2-5).
#   prep(trim) -> list(obs = the observed statistic (NA when it does not exist),
#                      stat = function(idx) the statistic of a resample of the
#                      null data, recentred at that trim's centre)
#   draw()     -> the row indices of one resample (astig_resampler)
#   tst        -> the trimmed test's label; why_none: the reason when even the
#                 untrimmed statistic does not exist in the data
# Each attempt starts from the fixed seed ASTIG_SEED. Returns the P value with
# attrs "test", "B" (= B), "variant", or NA with "why" and "test".
astig_boot_test <- function(prep, draw, B, tst, why_none, trim = ASTIG_TRIM) {
  tr <- with_local_seed(ASTIG_SEED, {
    pr <- prep(trim)
    if (!is.finite(pr$obs)) NULL
    else astig_boot_count(pr$obs, pr$stat, draw, B, floor(B * (1 / ASTIG_TRIM_OK - 1) + 1e-9))
  })
  if (!is.null(tr) && isTRUE(tr$ok))
    return(structure(tr$p, test = tst, B = tr$eff, variant = "trimmed"))
  un <- with_local_seed(ASTIG_SEED, {
    pr <- prep(0)
    if (!is.finite(pr$obs)) NULL
    else astig_boot_count(pr$obs, pr$stat, draw, B, (ASTIG_BOOT_MAX - 1L) * B)
  })
  # no statistic at all, trimmed or not: no variant ran, so none is named - the
  # label is the test that was meant
  if (is.null(un)) return(astig_na(why_none, tst))
  if (!isTRUE(un$ok))
    return(astig_na(sprintf("only %d of %d bootstrap resamples were usable (%d needed)", un$eff, un$draws, B),
                    ASTIG_UNTRIMMED))
  structure(un$p, test = ASTIG_UNTRIMMED, B = un$eff, variant = "untrimmed")
}
# A function returning the row indices of one bootstrap resample: whole
# clusters (patients; each eye its own cluster when id is NULL) drawn with
# replacement WITHIN each group, as many as the group has. A patient whose
# eyes are in two groups is one cluster in each.
astig_resampler <- function(g, id = NULL) {
  g <- droplevels(factor(g))
  id <- if (is.null(id)) seq_along(g) else fill_missing_ids(id)
  cl <- split(seq_along(g), paste(as.integer(g), id, sep = "\r"))
  first <- vapply(cl, `[`, integer(1), 1)
  by_g <- split(seq_along(cl), as.integer(g)[first])
  function() unlist(lapply(by_g, function(k)
    cl[k[sample.int(length(k), length(k), replace = TRUE)]]), use.names = FALSE)
}
# Winsorizing in the standard definition of the robust-statistics literature
# (Wilcox; decision R3-5): for n values and trimming proportion `trim`, g =
# floor(trim n); the g smallest values are replaced by the (g + 1)-th smallest
# and the g largest by the (n - g)-th smallest - order statistics, not sample
# quantiles. The winsorized variance and covariance are the usual ones (n - 1
# denominator) of the winsorized values, a bivariate sample winsorized component
# by component. R's mean(x, trim = ) removes the same g values at each end, so
# the trimmed means and the winsorized spread use one g.
winsor <- function(v, trim) {
  if (trim <= 0) return(v)
  z <- sort(v[!is.na(v)])
  n <- length(z)
  if (n < 2) return(v)
  g <- floor(trim * n)
  pmin(pmax(v, z[g + 1]), z[n - g])
}
robust_biv_stat <- function(x, y, g, trim) {
  lv <- levels(g); k <- length(lv)
  mu <- t(vapply(lv, function(l)
    c(mean(x[g == l], trim = trim), mean(y[g == l], trim = trim)), numeric(2)))
  # pooled winsorized covariance
  S <- matrix(0, 2, 2); dfw <- 0
  for (l in lv) {
    xi <- winsor(x[g == l], trim); yi <- winsor(y[g == l], trim)
    ni <- length(xi); if (ni < 3) next
    S <- S + (ni - 1) * stats::cov(cbind(xi, yi)); dfw <- dfw + ni - 1
  }
  if (dfw <= 0) return(NA_real_)
  S <- S / dfw
  Si <- tryCatch(solve(S), error = function(e) NULL)
  if (is.null(Si)) return(NA_real_)
  gm <- colMeans(mu)
  sum(vapply(seq_len(k), function(i) {
    d <- matrix(mu[i, ] - gm, ncol = 1)
    as.numeric(sum(g == lv[i]) * t(d) %*% Si %*% d) }, numeric(1)))
}
# "" when a bivariate centroid test may run on these eyes (g: the group of each
# usable eye), else why not, in the tables' words ("C has n = 1"). id: the
# patient ids when the test is a clustered or weighted GEE, which also needs
# two patients per group (helpers.R cluster_guard_note).
astig_p_guard <- function(g, id = NULL) {
  g <- droplevels(factor(g))
  if (nlevels(g) < 2) return("fewer than two groups have data for this variable")
  why <- small_group_note(g)
  if (nzchar(why) || is.null(id)) return(why)
  cluster_guard_note(id, g)
}
# NA P value carrying the reason (attr "why") and the test meant (attr "test")
astig_na <- function(why, test = "") structure(NA_real_, why = why, test = test)

astig_robust_biv_p <- function(x, y, g, id = NULL, trim = ASTIG_TRIM, B = ASTIG_BOOT_B) {
  g <- droplevels(factor(as.character(g)))
  ok <- is.finite(x) & is.finite(y) & !is.na(g)
  x <- x[ok]; y <- y[ok]; g <- droplevels(g[ok])
  clustered <- !is.null(id)
  # split() below drops NA ids: an id-less eye must be its own patient instead
  id <- if (is.null(id)) seq_along(x) else fill_missing_ids(id[ok])
  # resampling patients: a group of one patient is the same eyes in every
  # resample, so its spread never enters the null distribution - no P
  why <- astig_p_guard(g, if (clustered) id else NULL)
  if (nzchar(why)) return(astig_na(why))
  if (length(x) < 12) return(astig_na("fewer than 12 eyes for the bootstrap test"))
  # Reproducibility: the bootstrap seed is fixed locally (with_local_seed,
  # inside astig_boot_test), restoring the caller's RNG state afterwards.
  # The null: every group recentred on its own (trimmed) mean, so the groups
  # share a centre; resampled within each cohort.
  prep <- function(tr) {
    x0 <- x; y0 <- y
    for (l in levels(g)) {
      k <- g == l
      x0[k] <- x[k] - mean(x[k], trim = tr); y0[k] <- y[k] - mean(y[k], trim = tr)
    }
    list(obs = robust_biv_stat(x, y, g, tr),
         stat = function(idx) robust_biv_stat(x0[idx], y0[idx], g[idx], tr))
  }
  astig_boot_test(prep, astig_resampler(g, if (clustered) id else NULL), B,
                  sprintf("robust %d%%-trimmed T², bootstrap", as.integer(round(trim * 100))),
                  "the eyes have no spread in one direction", trim)
}

# Bivariate comparison of astigmatism centroids on the DOUBLED-angle components.
# JCRS 2025 Part II asks for a bivariate test rather than two separate univariate
# ones. Pillai's trace on a 2-column MANOVA is algebraically Hotelling's T-squared
# when there are exactly 2 groups, and generalises to more. NOT the robust
# (trimmed + bootstrapped) variant the paper mentions - stated as such in the figure.
# w: matching weights of a matched cohort. The test is then the weighted
# stacked GEE below (clustered by `id`, or each eye its own cluster when id is
# NULL); there is no unweighted fallback.
# Nothing is fitted for a group of one eye (or, for a GEE, one patient): the
# sandwich variance of that group is zero and the Wald chi-square explodes; nor
# for data below the minimum for any clustered model (helpers.R
# model_data_guard, contract R4-10: "too few eyes for this model (n = 5)").
# With weights that are not all 1 the GEE uses an independence working
# correlation, like every weighted model (contract R4-1), and so does every
# GEE of a matched cohort (matched = TRUE; decision D18), weighted or not. A GEE whose
# group-term covariance is numerically singular is not reported (gr_wald_p,
# contract R4-6): a weighted test then says it could not be fitted, a clustered
# one falls back to Hotelling / Pillai, labelled "GEE not fitted" and counted
# in the run log's fallback line (.gr_opts$n_gee_fallback), like p_omnibus.
# The result carries attr "test" - the test that actually ran ("weighted GEE",
# "clustered GEE", "Hotelling", "Pillai") - and, when it is NA, attr "why".
astig_bivariate_p <- function(x, y, g, id = NULL, w = NULL, matched = isTRUE(.gr_opts$matched)) {
  g <- droplevels(factor(as.character(g)))
  ok <- is.finite(x) & is.finite(y) & !is.na(g)
  weighted <- !is.null(w)
  # no patient ids: each eye is its own cluster, and the texts count eyes
  unit <- if (is.null(id)) "eye" else "patient"
  if (weighted) {
    w <- suppressWarnings(as.numeric(w))
    ok <- ok & is.finite(w) & w > 0
    if (is.null(id)) id <- paste0("eye ", seq_along(x))
  }
  why <- astig_p_guard(g[ok])
  if (nzchar(why)) return(astig_na(why, if (weighted) "weighted GEE" else ""))
  k <- nlevels(droplevels(g[ok]))
  if (sum(ok) < k + 2) return(astig_na("too few eyes", if (weighted) "weighted GEE" else ""))
  # clustered: stack the two doubled-angle components as 2 records per eye and
  # joint-Wald the group + component:group terms - the clustered analogue of
  # Hotelling T-squared, with both records of an eye inside the patient cluster
  if (!is.null(id) && requireNamespace("geepack", quietly = TRUE)) {
    idk0 <- id[ok]
    idk <- fill_missing_ids(idk0)            # id-less eyes: singleton clusters
    clustered <- weighted || !all(is.na(idk0) | !nzchar(trimws(as.character(idk0))))
    gtest <- if (weighted) "weighted GEE" else "clustered GEE"
    # a group whose eyes all belong to one patient: no GEE and no P, weighted
    # or not (the GEE tables say the same)
    why_c <- cluster_guard_note(idk, g[ok], unit)
    if (clustered && nzchar(why_c)) return(astig_na(why_c, gtest))
    if (clustered) {
      # a 2-D centroid needs more than two clusters in every group: with two
      # patients the group's sandwich covariance has rank 1, the joint Wald
      # statistic explodes and "P < 0.001" is printed from two patients
      ncl <- tapply(as.character(idk), droplevels(g[ok]), function(v) length(unique(v)))
      two <- names(ncl)[!is.na(ncl) & ncl < 3]
      if (length(two))
        return(astig_na(sprintf("%s (a centroid test needs at least 3 %ss per group)",
                                paste(sprintf("%s has %d %ss", two, as.integer(ncl[two]), unit), collapse = "; "),
                                unit), gtest))
      # the minimum data for any clustered model, with the tables' reason (the
      # stacked model has one coefficient per group and component: 2k)
      small <- model_data_guard(sum(ok), length(unique(idk)), 2L * k, unit)
      if (!is.null(small)) return(astig_na(small, gtest))
      wk <- if (weighted) w[ok] else rep(1, sum(ok))
      d2 <- rbind(
        data.frame(v = x[ok], comp = "x", g = droplevels(g[ok]), cid = as.character(idk), .w = wk),
        data.frame(v = y[ok], comp = "y", g = droplevels(g[ok]), cid = as.character(idk), .w = wk))
      d2$comp <- factor(d2$comp); d2$cid <- as.integer(factor(d2$cid))
      d2 <- d2[order(d2$cid), , drop = FALSE]
      cs <- if (isTRUE(matched) || (weighted && weights_nonunit(wk))) "independence" else "exchangeable"
      # an exchangeable fit only when it is accepted, else independence,
      # named in the test label (decision R3-4)
      fc <- gee_fit_checked(function(cs_) quiet_eval(suppressWarnings(
              if (weighted) geepack::geeglm(v ~ comp * g, id = cid, data = d2, weights = .w,
                                            corstr = cs_)
              else geepack::geeglm(v ~ comp * g, id = cid, data = d2, corstr = cs_)))$value, cs)
      f2 <- fc$fit
      if (!is.null(f2)) {
        if (nzchar(fc$refit)) gee_refit_note(fc$refit)
        pg <- gr_wald_p(f2, grep("(^g)|(:g)", names(stats::coef(f2))))
        if (is.finite(pg))
          return(structure(as.numeric(pg), test = if (nzchar(fc$refit)) paste0(gtest, ", ", GEE_INDEP_REFIT) else gtest,
                           few = few_patient_groups(idk, g[ok], unit)))
      }
    }
  }
  if (weighted) return(astig_na("the weighted test could not be computed", "weighted GEE"))
  # unclustered Hotelling / Pillai: the "classic" test, or the fallback when a
  # clustered GEE was asked for but could not be fitted (counted for the run
  # log's fallback line when it gives a P value, as p_omnibus counts its own)
  fb <- !is.null(id)
  tst <- if (k == 2) "Hotelling" else "Pillai"
  gk <- droplevels(g[ok])
  fit <- tryCatch(stats::manova(cbind(x[ok], y[ok]) ~ gk), error = function(e) NULL)
  st <- if (is.null(fit)) NULL else tryCatch(summary(fit, test = "Pillai")$stats, error = function(e) NULL)
  if (is.null(st) || !("Pr(>F)" %in% colnames(st)))
    return(structure(astig_na("the test could not be computed", tst), fallback = fb))
  pf <- as.numeric(st[1, "Pr(>F)"])
  if (fb && is.finite(pf)) .gr_opts$n_gee_fallback <- (.gr_opts$n_gee_fallback %||% 0L) + 1L
  structure(pf, test = tst, fallback = fb)
}

# each group vs the reference (first level), Holm-adjusted across the
# comparisons; attr "fallback": the comparisons whose clustered GEE could not
# be fitted (the unclustered test ran instead)
astig_vs_ref <- function(x, y, g, glv, holm = TRUE, id = NULL, w = NULL,
                         matched = isTRUE(.gr_opts$matched)) {
  if (length(glv) < 2) return(NULL)
  ref <- glv[1]; others <- glv[-1]
  res <- lapply(others, function(o) {
    k <- g %in% c(ref, o)
    astig_bivariate_p(x[k], y[k], g[k], id = if (is.null(id)) NULL else id[k],
                      w = if (is.null(w)) NULL else w[k], matched = matched)
  })
  p <- vapply(res, function(r) as.numeric(r)[1], numeric(1))
  fb <- others[vapply(res, function(r) is.finite(r) && isTRUE(attr(r, "fallback")), logical(1))]
  few <- do.call(gr_few_merge, lapply(res, function(r) if (is.finite(r)) attr(r, "few")))
  if (holm && length(p) > 1) p <- stats::p.adjust(p, method = "holm")
  structure(stats::setNames(p, others), fallback = fb, few = few)
}

# The 95% region of one cohort's doubled-angle cloud, the ONE place its method
# is chosen: astig_region(x, y, level, method).
#   method "depth" (default; decision R3-1) - the depth-based convex polygon of
#          the journal's reference tool (the WHWK functions, JCRS 2025 Part
#          II), built from the description of the project lead:
#          1. for every eye i off the centroid c, the line through c and p_i;
#             each eye j is projected onto it (its position on the line is
#             (p_j - c).(p_i - c) / |p_i - c|; the ranks below are those of
#             the dot product, the common positive factor changes none);
#          2. on each line the depth of eye j is the smaller of the shares of
#             eyes at or beyond its position on either side (ties count on
#             both sides);
#          3. an eye's depth is its smallest depth over the lines - every
#             depth is 0 when an eye lies exactly at the centroid (kept on
#             purpose: the reference tool does the same);
#          4. every vertex of the convex hull of all the eyes takes the
#             smallest depth of step 3;
#          5. depths are rounded to multiples of 1/n;
#          6. threshold t = the depth of rank floor(0.05 n + 0.5) in increasing
#             order - rank 0 for n < 10, where the rule is undefined and the
#             region is the convex hull of all the eyes (the note says so);
#          7. the region is the convex hull of the eyes of depth >= t. Depths
#             tie, so it can hold more than 95% of the eyes; the count is
#             reported.
#   method "normal" - the bivariate-normal ellipse {u : (u-c)' S^-1 (u-c) <=
#          qchisq(level, 2)}.
# ("polygon", the v1.0.0 name of the depth polygon, is read as "depth"; so is
# the round-2 call form astig_region(x, y, "depth").) Returns
#   list(vertices = the closed path (data.frame x, y) or NULL when no region is
#        drawn, kept = the indices of the eyes (in the vectors given) the hull
#        is built from, count = the eyes inside or on the region, n = the eyes
#        with both components, method = "depth" / "normal", rule = "depth" or
#        "all" (n < 10: the hull of every eye), note = what was drawn or why
#        nothing was: fewer than 3 eyes, or a line rather than a polygon
#        (decision R2-6: every eye - or every eye of the hull - on one line,
#        e.g. plus axes 90 and 180 only), or the ellipse's covariance singular).
ASTIG_REGION_LABEL <- "depth-based convex polygon (as in the WHWK functions)"
ASTIG_REGION_MIN_N <- 10L      # below it the depth rule's rank floor(0.05 n + 0.5) is 0
astig_region <- function(x, y, level = 0.95, method = "depth") {
  if (is.character(level)) { method <- level; level <- 0.95 }
  if (identical(method, "polygon")) method <- "depth"
  ok <- is.finite(x) & is.finite(y)
  pos <- which(ok)
  x <- x[ok]; y <- y[ok]
  n <- length(x)
  out <- list(vertices = NULL, kept = integer(0), count = 0L, n = n, method = method, rule = "", note = "")
  none <- function(note) { out$note <- note; out }
  if (n < 3) return(none(sprintf("95%% region not drawn: %d eye%s (3 needed)", n, if (n == 1) "" else "s")))
  if (astig_collinear(x, y))
    return(none(if (astig_identical(x, y)) ASTIG_REGION_SAME
                else if (astig_on_meridian(x, y)) ASTIG_REGION_LINE else ASTIG_REGION_LINE_OFF))
  if (identical(method, "depth")) {
    r <- floor((1 - level) * n + 0.5 + 1e-9)
    if (r < 1) {
      keep <- seq_len(n); out$rule <- "all"
    } else {
      dep <- astig_depth(x, y)
      keep <- which(dep >= sort(dep)[r]); out$rule <- "depth"
    }
    if (astig_collinear(x[keep], y[keep]))
      return(none(sprintf("95%% region not drawn: the %d eyes it is built from lie on one line", length(keep))))
    h <- grDevices::chull(x[keep], y[keep])
    hx <- x[keep][h]; hy <- y[keep][h]
    out$vertices <- data.frame(x = c(hx, hx[1]), y = c(hy, hy[1]))
    out$kept <- pos[keep]
    out$count <- sum(in_convex_polygon(x, y, hx, hy))
    out$note <- if (identical(out$rule, "all"))
      sprintf("convex hull of all %d eyes (fewer than %d eyes: the depth rule is undefined)", n, ASTIG_REGION_MIN_N)
      else sprintf("%s: %d of %d eyes", ASTIG_REGION_LABEL, out$count, n)
    return(out)
  }
  S <- tryCatch(stats::cov(cbind(x, y)), error = function(e) NULL)
  q <- stats::qchisq(level, df = 2)
  e <- ellipse_path(mean(x), mean(y), S, sqrt(q))
  if (is.null(e)) return(none("95% region not drawn: the covariance of the eyes is singular"))
  md <- stats::mahalanobis(cbind(x, y), c(mean(x), mean(y)), S)
  out$vertices <- e
  out$kept <- pos[md <= q + 1e-9]
  out$count <- length(out$kept)
  out$rule <- "normal"
  out$note <- sprintf("95%% bivariate-normal ellipse: %d of %d eyes", out$count, n)
  out
}

# The region of astig_region as the closed path the figures and the sheet use:
# the vertices with attrs "kept" (the number of eyes the hull is built from),
# "inside" (the eyes inside or on it), "n", "method", "rule" and "note"; NULL
# when no region is drawn.
astig_region_path <- function(r) {
  if (is.null(r) || is.null(r$vertices)) return(NULL)
  v <- r$vertices
  attr(v, "kept") <- length(r$kept)
  attr(v, "inside") <- r$count
  attr(v, "n") <- r$n
  attr(v, "method") <- r$method
  attr(v, "rule") <- r$rule
  attr(v, "note") <- r$note
  v
}

# TRUE when the points (x, y) lie on one line - no spread across it, relative
# to the spread along it (all eyes on one meridian, or all identical)
astig_collinear <- function(x, y) {
  if (length(x) < 2) return(TRUE)
  ev <- tryCatch(eigen(stats::cov(cbind(x, y)), symmetric = TRUE, only.values = TRUE)$values,
                 error = function(e) c(0, 0))
  !all(is.finite(ev)) || max(ev) < 1e-24 || min(ev) < 1e-10 * max(ev)
}

# Why astig_region() drew no region for these eyes (the sheet's "95% data
# region" cell and the figure's legend file say it in these words); "" when
# it drew one
ASTIG_REGION_LINE <- "95% region not drawn: all eyes lie on one meridian"
# eyes on one straight line of the doubled-angle plane that does not pass
# through its origin (not one meridian, e.g. every eye with the same y)
ASTIG_REGION_LINE_OFF <- "95% region not drawn: all eyes lie on one line"
# every eye the same vector: one point, not a line
ASTIG_REGION_SAME <- "95% region not drawn: all eyes are identical"
astig_identical <- function(x, y)
  length(x) > 0 && max(abs(c(x - mean(x), y - mean(y)))) <= 1e-12 * max(1, abs(c(x, y)))
# TRUE when collinear eyes (astig_collinear) lie on a line through the origin of
# the doubled-angle plane - one meridian (and its opposite) - or are one point
astig_on_meridian <- function(x, y) {
  dx <- x - mean(x); dy <- y - mean(y)
  if (max(abs(c(dx, dy))) < 1e-12 * max(1, abs(c(x, y)))) return(TRUE)
  e <- eigen(stats::cov(cbind(x, y)), symmetric = TRUE)$vectors[, 1]   # the line's direction
  abs(mean(x) * e[2] - mean(y) * e[1]) <= 1e-9 * max(1, abs(c(x, y)))  # the origin's distance from it
}
astig_region_skip_why <- function(x, y, method = "depth", level = 0.95) {
  r <- astig_region(x, y, level, method)
  if (is.null(r$vertices)) r$note else ""
}

# The depth of every eye (decision R3-1, steps 1-5 of astig_region): a share
# of the n eyes, in multiples of 1/n. One projection of all the eyes per line
# - one line per distinct eye off the centroid, since identical eyes give the
# same line - and the two shares from the ranks of the projections (rank
# "max" = the eyes at or below a position, n + 1 - rank "min" = the eyes at or
# above it), so n = 2000 takes about a second.
astig_depth <- function(x, y) {
  n <- length(x)
  if (!n) return(numeric(0))
  dx <- x - mean(x); dy <- y - mean(y)
  # an eye exactly at the centroid: every depth is 0 (step 3, on purpose)
  if (any(dx == 0 & dy == 0)) return(rep(0, n))
  L <- unique(cbind(dx, dy))
  cnt <- rep(n, n)
  for (i in seq_len(nrow(L))) {
    z <- dx * L[i, 1] + dy * L[i, 2]
    cnt <- pmin(cnt, rank(z, ties.method = "max"), n + 1L - rank(z, ties.method = "min"))
  }
  dep <- cnt / n
  # the vertices of the hull of all the eyes take the smallest depth (step 4;
  # chull names one eye of a set of identical eyes at a vertex - the eyes are
  # kept in the order of the data, the order of the WHWK input files)
  dep[grDevices::chull(x, y)] <- min(dep)
  round(dep * n) / n
}

# TRUE for each point inside or on the convex polygon with vertices (hx, hy)
# in hull order (either orientation); a degenerate hull (a segment or one
# point) contains the points on it.
in_convex_polygon <- function(x, y, hx, hy, eps = 1e-9) {
  k <- length(hx)
  if (k == 0) return(rep(FALSE, length(x)))
  if (k < 3) {
    if (k == 1) return(abs(x - hx) <= eps & abs(y - hy) <= eps)
    cr <- (hx[2] - hx[1]) * (y - hy[1]) - (hy[2] - hy[1]) * (x - hx[1])
    t <- ((x - hx[1]) * (hx[2] - hx[1]) + (y - hy[1]) * (hy[2] - hy[1])) /
         max((hx[2] - hx[1])^2 + (hy[2] - hy[1])^2, 1e-300)
    return(abs(cr) <= eps & t >= -eps & t <= 1 + eps)
  }
  j <- c(seq_len(k)[-1], 1)
  cr <- vapply(seq_len(k), function(i)
    (hx[j[i]] - hx[i]) * (y - hy[i]) - (hy[j[i]] - hy[i]) * (x - hx[i]), numeric(length(x)))
  cr <- matrix(cr, nrow = length(x))
  apply(cr, 1, function(r) all(r <= eps) || all(r >= -eps))
}

# points of the ellipse {u : (u - c)' S^-1 (u - c) <= r^2}; NULL when S is not
# positive definite
ellipse_path <- function(cx, cy, S, r) {
  if (is.null(S) || any(!is.finite(S)) || !is.finite(r)) return(NULL)
  e <- eigen(S, symmetric = TRUE)
  if (any(!is.finite(e$values)) || any(e$values <= 0)) return(NULL)
  th <- seq(0, 2 * pi, length.out = 181)
  pts <- t(e$vectors %*% (t(cbind(cos(th), sin(th))) * (r * sqrt(e$values))))
  data.frame(x = cx + pts[, 1], y = cy + pts[, 2])
}

# 95% confidence region of a group's CENTROID (the mean doubled-angle vector).
#   id NULL - eyes independent: covariance S/n with the exact Hotelling T^2
#             radius r^2 = 2(n-1)/(n-2) F(.95; 2, n-2). The chi-square radius is
#             only its large-n limit and under-covers at small n (74% at n = 5).
#   id set  - eyes clustered in patients (the figure's tests cluster too):
#             sandwich covariance of the mean from the per-patient sums of the
#             residuals, V = G/(G-1) sum_j U_j U_j' / n^2, with the same F radius
#             on G - 2 df (G = patients), so the fellow-eye correlation widens it.
# Needs >= 4 eyes (patients); attr "basis" says which form was drawn.
CENTROID_MIN <- 4L
centroid_region <- function(x, y, id = NULL, level = 0.95) {
  n <- length(x)
  cx <- mean(x); cy <- mean(y)
  if (!is.null(id)) {
    idf <- fill_missing_ids(id)
    G <- length(unique(idf))
    if (G < CENTROID_MIN) return(NULL)
    U <- rowsum(cbind(x - cx, y - cy), idf, reorder = FALSE)
    V <- crossprod(U) / n^2 * G / (G - 1)
    r <- sqrt(2 * (G - 1) / (G - 2) * stats::qf(level, 2, G - 2))
    basis <- "clustered"
  } else {
    if (n < CENTROID_MIN) return(NULL)
    V <- tryCatch(stats::cov(cbind(x, y)) / n, error = function(e) NULL)
    r <- sqrt(2 * (n - 1) / (n - 2) * stats::qf(level, 2, n - 2))
    basis <- "independent"
  }
  out <- ellipse_path(cx, cy, V, r)
  if (!is.null(out)) attr(out, "basis") <- basis
  out
}

# Why astig_centroid_region() drew nothing for these eyes: too few
# eyes or patients, or a cloud with no spread in some direction (all eyes on one
# meridian, or identical), whose covariance is singular
centroid_skip_why <- function(x, y, id = NULL) {
  ok <- is.finite(x) & is.finite(y)
  if (!is.null(id)) {
    G <- length(unique(fill_missing_ids(id[ok])))
    if (G < CENTROID_MIN) return(sprintf("%d patient%s; %d needed", G, if (G == 1) "" else "s",
                                         CENTROID_MIN))
  } else if (sum(ok) < CENTROID_MIN) {
    return(sprintf("%d eye%s; %d needed", sum(ok), if (sum(ok) == 1) "" else "s", CENTROID_MIN))
  }
  if (astig_identical(x[ok], y[ok])) "all eyes are identical" else "no spread in one direction"
}

# The 95% confidence region of a cohort's CENTROID and the test of the
# centroid against zero (decision R3-2: as the journal's reference tool, the
# WHWK functions, builds them; written from the project lead's description).
#
# The B = 999 bootstrap centroids (astig_boot_centroids): patients resampled
# when `id` is given (all eyes of a drawn patient enter together; the
# centroid of a resample is the sum of its patients' sums over their eyes),
# eyes otherwise; fixed seed ASTIG_CENTROID_SEED. `idx`: an optional matrix of
# resample indices (rows = resamples, one column per resampling unit: the eyes
# in the order given, or the patients in the order of their first eye) - a
# validation script feeds the very resamples a reference tool drew, and the
# result then does not depend on the seed. NULL with attr "why" when the
# matrix does not fit the data.
ASTIG_CENTROID_SEED <- 20250602L
astig_boot_centroids <- function(x, y, id = NULL, B = ASTIG_BOOT_B, idx = NULL) {
  supplied <- !is.null(idx)
  unit <- if (is.null(id)) seq_along(x) else fill_missing_ids(id)
  u <- match(unit, unique(unit))                  # 1..N in the order of first appearance
  N <- max(c(u, 0L))
  sx <- rowsum(x, u)[, 1]; sy <- rowsum(y, u)[, 1]
  nn <- rowsum(rep(1, length(x)), u)[, 1]
  if (is.null(idx)) {
    idx <- with_local_seed(ASTIG_CENTROID_SEED, matrix(sample.int(N, N * B, replace = TRUE), nrow = B))
  } else {
    idx <- as.matrix(idx)
    if (!is.numeric(idx) || ncol(idx) != N || nrow(idx) < 1 || any(!is.finite(idx)) ||
        any(idx < 1 | idx > N | idx != round(idx)))
      return(structure(list(), why = sprintf(
        "the resample-index matrix does not fit the data (%d columns of indices 1 to %d needed)", N, N)))
    storage.mode(idx) <- "integer"
  }
  Bn <- nrow(idx)
  den <- rowSums(matrix(nn[idx], nrow = Bn))
  list(x = rowSums(matrix(sx[idx], nrow = Bn)) / den, y = rowSums(matrix(sy[idx], nrow = Bn)) / den,
       B = Bn, N = N, unit = if (is.null(id)) "eyes" else "patients", supplied = supplied)
}

# The "ideal fourths" (lower and upper quartile) of a sample: for m sorted
# values z, j = floor(m/4 + 5/12), g = m/4 - j + 5/12, lower = (1 - g) z_j +
# g z_(j+1), upper = (1 - g) z_(m-j+1) + g z_(m-j). NA for fewer than 3 values.
ideal_fourths <- function(z) {
  z <- sort(z[!is.na(z)])
  m <- length(z)
  if (m < 3) return(c(lower = NA_real_, upper = NA_real_))
  j <- floor(m / 4 + 5 / 12)
  g <- m / 4 - j + 5 / 12
  c(lower = (1 - g) * z[j] + g * z[j + 1], upper = (1 - g) * z[m - j + 1] + g * z[m - j])
}

# Projection distance of each point of the cloud W (a k x 2 matrix) from the
# centre `cen` within W: for every point w of W that differs from the centre,
# the line through the centre and w; every point is projected onto it, and the
# absolute lengths of the projections are divided by the interquartile range
# (ideal fourths) of those lengths over W; a point's distance is the largest
# of its scaled lengths over the lines. A line whose interquartile range is 0
# scales a length above 0 to Inf and a zero length to nothing (0 / 0 is left
# out); a point with no defined scaled length on any line gets 0.
# A point within 1e-9 of the largest distance from the centre counts as the
# centre itself (it defines no line): a resample whose centroid equals the
# cohort's up to rounding would otherwise define a line in an arbitrary
# direction, on which every projection is rounding noise (finding A1).
astig_proj_dist <- function(W, cen) {
  D <- sweep(as.matrix(W), 2, as.numeric(cen))
  s <- sqrt(rowSums(D^2))
  L <- unique(D[s > 1e-9 * max(s), , drop = FALSE])  # identical points: the same line
  best <- rep(-Inf, nrow(D))
  for (i in seq_len(nrow(L))) {
    len <- abs(D %*% (L[i, ] / sqrt(sum(L[i, ]^2))))[, 1]
    f <- ideal_fourths(len)
    best <- pmax(best, len / (f[[2]] - f[[1]]), na.rm = TRUE)
  }
  best[!is.finite(best) & best < 0] <- 0
  best
}

# The share of resampling units in the region (decision R3-2, step 3): 1 - a
# of the bootstrap centroids, a set by the number N of resampling units (eyes,
# or patients when clustered); a = 1 - level when level is not 0.95.
astig_centroid_alpha <- function(N, level = 0.95) {
  if (abs(level - 0.95) > 1e-9) return(1 - level)
  if (N <= 20) 0.02 else if (N <= 30) 0.025 else if (N <= 40) 0.03 else if (N <= 60) 0.035
  else if (N <= 80) 0.04 else if (N <= 120) 0.045 else 0.05
}

# The bootstrap centroids of these eyes when they can carry a region or a
# test, else the reason (attr "why" of an empty list): fewer than
# CENTROID_MIN resampling units, a resample-index matrix that does not fit, or
# bootstrap centroids without spread in some direction (every eye on one
# meridian, or identical: a "polygon" of floating-point noise; the check is
# relative to the spread there is).
astig_boot_cloud <- function(x, y, id = NULL, B = ASTIG_BOOT_B, idx = NULL, need_2d = TRUE) {
  N <- length(unique(if (is.null(id)) seq_along(x) else fill_missing_ids(id)))
  unit <- if (is.null(id)) "eye" else "patient"
  if (N < CENTROID_MIN)
    return(structure(list(), why = sprintf("%d %s%s; %d needed", N, unit, if (N == 1) "" else "s", CENTROID_MIN)))
  bc <- astig_boot_centroids(x, y, id, B, idx)
  if (!length(bc)) return(bc)
  ev <- tryCatch(eigen(stats::cov(cbind(bc$x, bc$y)), symmetric = TRUE, only.values = TRUE)$values,
                 error = function(e) c(0, 0))
  flat <- !all(is.finite(ev)) || max(ev) < 1e-24
  if (flat || (need_2d && min(ev) < 1e-10 * max(ev)))
    return(structure(list(), why = if (flat) "no spread" else "no spread in one direction"))
  bc
}

# The 95% confidence region of a cohort's CENTROID, method:
#   "bootstrap" (default; decision R3-2) - the bootstrap convex polygon of the
#              centroid (as in the WHWK functions): the projection distance of
#              each bootstrap centroid from the cohort's centroid within the
#              cloud of the B bootstrap centroids (astig_proj_dist); the
#              round((1 - a) B) centroids with the smallest distances are kept
#              (a: astig_centroid_alpha) and the region is their convex hull;
#              attr "basis" "bootstrap over patients" / "... eyes", "B",
#              "kept", "alpha", "N";
#   "hotelling" - the Hotelling T² ellipse of centroid_region (clustered
#              sandwich covariance when `id` is given).
# NULL when there are fewer than CENTROID_MIN eyes (patients) or no spread.
astig_centroid_region <- function(x, y, id = NULL, method = "bootstrap", level = 0.95,
                                  B = ASTIG_BOOT_B, idx = NULL) {
  ok <- is.finite(x) & is.finite(y)
  x <- x[ok]; y <- y[ok]
  if (!is.null(id)) id <- id[ok]
  if (!identical(method, "bootstrap")) return(centroid_region(x, y, id, level))
  bc <- astig_boot_cloud(x, y, id, B, idx)
  if (!length(bc)) return(NULL)
  W <- cbind(bc$x, bc$y)
  d <- astig_proj_dist(W, c(mean(x), mean(y)))
  a <- astig_centroid_alpha(bc$N, level)
  k <- round((1 - a) * bc$B)
  keep <- order(d)[seq_len(k)]
  if (astig_collinear(W[keep, 1], W[keep, 2])) return(NULL)
  h <- grDevices::chull(W[keep, 1], W[keep, 2])
  hx <- W[keep, 1][h]; hy <- W[keep, 2][h]
  out <- data.frame(x = c(hx, hx[1]), y = c(hy, hy[1]))
  attr(out, "basis") <- if (is.null(id)) "bootstrap over eyes" else "bootstrap over patients"
  attr(out, "B") <- bc$B
  attr(out, "kept") <- k
  attr(out, "alpha") <- a
  attr(out, "N") <- bc$N
  out
}

# ============================================================
# JCRS 2025 astigmatism statistics (decisions D4, D7): what a double-angle
# panel prints and the "Astigmatism (JCRS 2025)" sheet lists, computed ONCE per
# vector and cohort set (astig_vector_stats) and shared by both, so a figure
# and its sheet never disagree. Every test names itself; a test that cannot
# run gives NA with its reason (attr "why"), never a silent substitute.
# ============================================================
ASTIG_CUTS <- seq(0.25, 2.00, by = 0.25)     # the cumulative intervals (D) of Part II / WHWK
ASTIG_TOL <- 1e-9                            # a magnitude on an interval edge counts as within

# The vectors of the standard (D2-D3) for a run's resolved vectors `av`, in
# panel order: PRE, SIA, PE - and POST only when at least one target is not
# plano (otherwise POST = PE, and the PE title says so). key: file and sheet
# name; title: the figure title.
astig_jcrs_vectors <- function(av) {
  nonplano <- isTRUE(av$n_target_nonplano > 0)
  out <- list(
    PRE = list(key = "PRE", title = "Preoperative refractive astigmatism",
               mag = av$pre, ax = av$pre_ax),
    SIA = list(key = "SIA", title = "Surgically induced astigmatism (postoperative - preoperative)",
               mag = av$sia, ax = av$sax),
    PE = list(key = "PE", title = if (nonplano)
                "Prediction error of refractive astigmatism (postoperative - target)"
              else "Prediction error (= postoperative refractive astigmatism; target plano)",
              mag = av$pe, ax = av$pe_ax))
  if (nonplano)
    out$POST <- list(key = "POST", title = "Postoperative refractive astigmatism",
                     mag = av$post, ax = av$post_ax)
  out
}

# "0.13 D @ 97°": a centroid (or any vector) in polar form, plus axis
astig_polar_text <- function(m, a, d = 2)
  if (is.finite(m) && is.finite(a)) sprintf(paste0("%.", d, "f D @ %.0f°"), m, a) else "n/a"

# The summary of one cohort's vector (D4): n eyes and patients, centroid (x,
# y, magnitude @ plus axis), total SD with the N denominator (Part II
# equations 6-9: sqrt of the population variances of x and y), magnitude mean
# ± SD, median, range and root mean square, and the cumulative percentages of
# eyes with a magnitude <= 0.25 ... 2.00 D and > 1.00 D.
astig_summary1 <- function(m, a, id = NULL) {
  ok <- is.finite(m) & is.finite(a)
  m <- m[ok]; a <- a[ok]
  n <- length(m)
  x <- m * cospi(a / 90); y <- m * sinpi(a / 90)
  cx <- if (n) mean(x) else NA_real_; cy <- if (n) mean(y) else NA_real_
  list(n = n,
       n_pat = if (is.null(id)) NA_integer_ else length(unique(fill_missing_ids(id[ok]))),
       cx = cx, cy = cy, cm = sqrt(cx^2 + cy^2),
       ca = if (n) (atan2(cy, cx) * 180 / pi / 2) %% 180 else NA_real_,
       tsd = if (n) sqrt(mean((x - cx)^2) + mean((y - cy)^2)) else NA_real_,
       mmean = if (n) mean(m) else NA_real_,
       msd = if (n > 1) stats::sd(m) else NA_real_,
       mmed = if (n) stats::median(m) else NA_real_,
       mmin = if (n) min(m) else NA_real_, mmax = if (n) max(m) else NA_real_,
       rms = if (n) sqrt(mean(m^2)) else NA_real_,
       cum = vapply(ASTIG_CUTS, function(cc) if (n) 100 * mean(m <= cc + ASTIG_TOL) else NA_real_,
                    numeric(1)),
       gt1 = if (n) 100 * mean(m > 1 + ASTIG_TOL) else NA_real_)
}

# One-sample test of a cohort's centroid against zero (D7a; WHWK "bivariate
# mean is zero", Eyetemis trueness), by method:
#   "robust"  - (default; decision R3-2) the bootstrap projection-distance test
#               of the WHWK functions: the cloud W = the B bootstrap centroids
#               of astig_boot_centroids (patients resampled when `id` is given,
#               eyes otherwise; seed ASTIG_CENTROID_SEED - the resamples of the
#               centroid's confidence region) and the null point (0, 0); each
#               point's projection distance from the cohort's centroid within W
#               (astig_proj_dist: all B + 1 points define lines and enter the
#               interquartile ranges); P = the share of the B bootstrap
#               centroids farther than the null point. A P of 0 is reported at
#               the bootstrap floor 1/B ("P = .001" with B = 999; attr "floor"),
#               as "< .001" only when B >= 1000. Untrimmed by construction, so
#               the trimmed / untrimmed rule R2-5 does not concern it. `idx`: an
#               optional resample-index matrix (astig_boot_centroids), with
#               which the P value does not depend on the seed;
#   "classic" - one-sample Hotelling T², F = (n - 2) / (2 (n - 1)) T² on (2,
#               n - 2) df, eyes independent;
#   "gee"     - both components stacked, intercepts only, independence working
#               correlation, clustered by patient: joint Wald test of the two
#               intercepts in the run's small-sample inference (gee_wald).
# Returns the P value with attr "test" (and "B", "floor", "count" for the
# bootstrap), or NA with attr "why".
ASTIG_ZERO_TEST <- "bootstrap projection-distance test (as in the WHWK functions)"
astig_zero_p <- function(x, y, id = NULL, method = "robust", trim = ASTIG_TRIM, B = ASTIG_BOOT_B,
                         idx = NULL) {
  ok <- is.finite(x) & is.finite(y)
  x <- x[ok]; y <- y[ok]
  if (!is.null(id)) id <- fill_missing_ids(id[ok])
  n <- length(x)
  unit <- if (is.null(id)) "eye" else "patient"
  if (identical(method, "gee")) {
    tst <- "clustered GEE (intercepts)"
    if (is.null(id)) id <- paste0("eye ", seq_len(n))
    G <- length(unique(id))
    small <- model_data_guard(n, G, 2L, unit)
    if (!is.null(small)) return(astig_na(small, tst))
    if (!requireNamespace("geepack", quietly = TRUE)) return(astig_na("geepack is not installed", tst))
    d2 <- rbind(data.frame(v = x, comp = "x", cid = id), data.frame(v = y, comp = "y", cid = id))
    d2$comp <- factor(d2$comp); d2$cid <- as.integer(factor(d2$cid))
    d2 <- d2[order(d2$cid), , drop = FALSE]
    fit <- quiet_eval(suppressWarnings(
      geepack::geeglm(v ~ comp - 1, id = cid, data = d2, corstr = "independence")))$value
    if (is.null(fit)) return(astig_na("the GEE could not be fitted", tst))
    wt <- gee_wald(fit, 1:2, unit)
    if (!is.finite(wt$p))
      return(astig_na(if (nzchar(wt$why)) wt$why else "the GEE covariance is singular (no spread)", tst))
    return(structure(wt$p, test = tst, few = few_patient_groups(id, rep("cohort", n), unit)))
  }
  if (identical(method, "classic")) {
    tst <- "one-sample Hotelling T²"
    if (n < 3) return(astig_na(sprintf("too few eyes (n = %d)", n), tst))
    S <- stats::cov(cbind(x, y)); mu <- c(mean(x), mean(y))
    Si <- tryCatch(solve(S), error = function(e) NULL)
    if (is.null(Si) || any(!is.finite(Si))) return(astig_na("the eyes have no spread in one direction", tst))
    T2 <- as.numeric(n * t(mu) %*% Si %*% mu)
    Fv <- (n - 2) / (2 * (n - 1)) * T2
    return(structure(stats::pf(Fv, 2, n - 2, lower.tail = FALSE), test = tst, stat = Fv))
  }
  tst <- ASTIG_ZERO_TEST
  N <- length(unique(if (is.null(id)) seq_len(n) else id))
  if (N < CENTROID_MIN)
    return(astig_na(sprintf("%d %s%s (at least %d needed for the bootstrap test)", N, unit,
                            if (N == 1) "" else "s", CENTROID_MIN), tst))
  bc <- astig_boot_cloud(x, y, id, B, idx, need_2d = FALSE)
  if (!length(bc)) return(astig_na(sub("^no spread", "the eyes have no spread", attr(bc, "why")), tst))
  W <- rbind(cbind(bc$x, bc$y), c(0, 0))
  d <- astig_proj_dist(W, c(mean(x), mean(y)))
  cnt <- sum(d[seq_len(bc$B)] > d[bc$B + 1])
  fl <- cnt == 0
  p <- if (!fl) cnt / bc$B else if (bc$B >= 1000) 1 / (bc$B + 1) else 1 / bc$B
  out <- structure(p, test = tst, B = bc$B, floor = fl, count = cnt)
  if (!is.null(id)) attr(out, "few") <- few_patient_groups(id, rep("cohort", n))
  out
}

# Heteroscedastic comparison of the 20%-trimmed means of a scalar between
# cohorts (D7b: the precision - each eye's distance to its cohort centroid -
# and the mean magnitude; the robust t test of Eyetemis): Yuen's statistic for
# two cohorts and its Welch-type form for more,
#   Q = sum_j w_j (t_j - t_w)^2,  w_j = 1 / d_j,  t_w = sum w_j t_j / sum w_j,
#   d_j = (n_j - 1) s_wj^2 / (h_j (h_j - 1)),  h_j = n_j - 2 floor(0.2 n_j),
# (s_wj^2: the 20%-winsorized variance), calibrated by B bootstrap resamples of
# the data recentred on each cohort's trimmed mean, patients (eyes) resampled
# within each cohort; fixed seed. Returns P with attrs "test", "B", "values"
# (the trimmed mean of each cohort), or NA with "why".
trim_q_stat <- function(v, g, trim) {
  lv <- levels(g)
  tm <- dj <- numeric(length(lv))
  for (i in seq_along(lv)) {
    z <- v[g == lv[i]]; nj <- length(z)
    hj <- nj - 2 * floor(trim * nj)
    if (hj < 2) return(NA_real_)
    tm[i] <- mean(z, trim = trim)
    dj[i] <- (nj - 1) * stats::var(winsor(z, trim)) / (hj * (hj - 1))
  }
  if (any(!is.finite(dj)) || any(dj <= 0)) return(NA_real_)
  w <- 1 / dj
  tw <- sum(w * tm) / sum(w)
  sum(w * (tm - tw)^2)
}
astig_trimmed_p <- function(v, g, id = NULL, trim = ASTIG_TRIM, B = ASTIG_BOOT_B) {
  tst <- "robust 20%-trimmed means, bootstrap"
  g <- droplevels(factor(g))
  ok <- is.finite(v) & !is.na(g)
  v <- v[ok]; g <- droplevels(g[ok])
  clustered <- !is.null(id)
  if (clustered) id <- fill_missing_ids(id[ok])
  why <- p_value_guard(v, g, "continuous")
  if (!nzchar(why)) why <- astig_p_guard(g, if (clustered) id else NULL)
  if (nzchar(why)) return(astig_na(why, tst))
  if (length(v) < 12) return(astig_na("fewer than 12 eyes for the bootstrap test", tst))
  # the null: each cohort recentred on its own (trimmed) mean; the trimmed
  # test, else the untrimmed one (Welch-type statistic of the ordinary means,
  # decision R2-5)
  prep <- function(tr) {
    tms <- tapply(v, g, mean, trim = tr)
    v0 <- v - tms[as.character(g)]
    list(obs = trim_q_stat(v, g, tr), stat = function(i) trim_q_stat(v0[i], g[i], tr))
  }
  flat <- levels(g)[vapply(levels(g), function(l) !isTRUE(stats::var(v[g == l]) > 0), logical(1))]
  out <- astig_boot_test(prep, astig_resampler(g, if (clustered) id else NULL), B, tst,
                         paste0(if (length(flat)) paste(flat, collapse = ", ") else "a cohort",
                                if (length(flat) > 1) " have" else " has", " no spread"), trim)
  if (!is.finite(out)) return(out)
  attr(out, "values") <- tapply(v, g, mean, trim = if (identical(attr(out, "variant"), "trimmed")) trim else 0)
  if (clustered) attr(out, "few") <- few_patient_groups(id, g)
  out
}

# The univariate between-cohort test of the "gee" and "classic" methods (and
# of the cumulative intervals, D7c): a GEE clustered by patient when `id` is
# given and a patient contributes two of these eyes (Gaussian; binomial for a
# 0/1 `y` with kind "proportion"; independence working correlation on a
# matched cohort, D18) - helpers.R gee_group_p, the figures' and tables' one
# implementation - else the classical test: Welch t / Welch ANOVA, or Fisher's
# exact test for a proportion. A clustered GEE that was fitted but failed (a
# cohort with 0% or 100%: no finite estimate) is replaced by a permutation test
# over patients (astig_perm_p), which keeps the pairing of fellow eyes - never
# by an eye-level test that would ignore it - labelled "(GEE not fitted)" with
# the reason. Returns P with attr "test", or NA with "why".
astig_univ_p <- function(y, g, id = NULL, kind = "continuous", matched = FALSE) {
  prop <- identical(kind, "proportion")
  g <- droplevels(factor(g))
  ok <- (if (prop) !is.na(y) else is.finite(y)) & !is.na(g)
  y <- y[ok]; g <- droplevels(g[ok])
  if (!is.null(id)) id <- fill_missing_ids(id[ok])
  why <- p_value_guard(y, g, if (prop) "dichotomous" else kind)
  if (nzchar(why)) return(astig_na(why, ""))
  if (!is.null(id) && fellow_eyes_present(id)) {
    tst <- if (prop) "GEE (binomial, clustered by patient)" else "GEE (clustered by patient)"
    why <- cluster_guard_note(id, g)
    if (nzchar(why)) return(astig_na(why, tst))
    pc <- gee_group_p(y, g, id, kind, corstr = if (isTRUE(matched)) "independence")
    if (is.finite(pc)) return(structure(as.numeric(pc), test = gr_refit_label(tst, pc), few = few_patient_groups(id, g)))
    why_g <- as.character(attr(pc, "why") %||% "")[1]
    if (!is.na(why_g) && nzchar(why_g)) return(astig_na(why_g, tst))
    return(astig_perm_p(y, g, id, kind))
  }
  if (prop) {
    tab <- table(factor(y, levels = c(FALSE, TRUE)), g)
    p <- tryCatch(stats::fisher.test(tab)$p.value, error = function(e) NULL)
    tst <- "Fisher's exact test"
    if (is.null(p)) {
      p <- with_local_seed(ASTIG_SEED, tryCatch(
        stats::fisher.test(tab, simulate.p.value = TRUE, B = 20000L)$p.value, error = function(e) NA_real_))
      tst <- "Fisher's exact test (Monte Carlo, 20,000 replicates)"
    }
  } else {
    gt <- group_test(y, g, "continuous")
    p <- gt$p; tst <- gr_test_label(gt$test)
    if (!is.finite(p)) return(astig_na(gt$note %||% "the test could not be computed", tst))
  }
  structure(as.numeric(p), test = tst)
}

# The between-cohort test of astig_univ_p when the patient-clustered GEE could
# not be fitted (findings A5, T1-02; D7c allows an eye-level test only without
# clustering): a permutation test over patients. The clusters - a patient's
# eyes within one cohort (a patient with eyes in two cohorts is one cluster in
# each) - keep their eyes together; the cohort labels are permuted across the
# clusters, B = ASTIG_PERM_B times (seed ASTIG_SEED). Statistic: the between-
# cohort sum of squares of the eye means, sum_g n_g (m_g - m)^2 (for a 0/1
# outcome the Pearson chi-square statistic of the eye counts up to the constant
# p(1 - p); with two cohorts a function of the difference in proportions or
# means); P = (count of permuted statistics >= observed + 1) / (B + 1). The
# one implementation is helpers.R cluster_perm_p, which the summary tables and
# the standard figures use for the same case (finding S2).
ASTIG_PERM_B <- 999L
# the working correlation of the astigmatism GEE tests, for the sheet's method
# notes and the legend files (R3-4, finding S4: every sheet names it)
ASTIG_GEE_WC <- "exchangeable working correlation; independence on a matched cohort or where a test is labeled so"
astig_perm_p <- function(y, g, id, kind = "proportion") {
  g <- droplevels(factor(g))
  yv <- as.numeric(y)
  p <- cluster_perm_p(yv, g, id, "mean", B = ASTIG_PERM_B, seed = ASTIG_SEED)
  prop <- identical(kind, "proportion")
  edge <- prop && any(tapply(yv, g, function(z) all(z == 0) || all(z == 1)))
  structure(p,
            test = sprintf("permutation test over patients, %d permutations (GEE not fitted%s)", ASTIG_PERM_B,
                           if (edge) ": a cohort at 0% or 100%" else ""),
            few = few_patient_groups(id, g))
}

# McNemar's test of the preoperative against the postoperative refractive
# astigmatism of the same eyes at one interval (D7c): the eyes within `cut` D
# before and after. `id` (the clustering of the tests; NULL when the tests do
# not cluster) clusters the test when a patient contributes two discordant
# eyes. Fewer than 25 discordant eyes: the exact binomial test - on the eyes,
# or, clustered, the exact sign test on the patients' net changes d_k = b_k -
# c_k (patients with d_k = 0 drop out); otherwise, clustered, the
# cluster-adjusted statistic of Durkalski et al. 2003,
# (sum_k (b_k - c_k))^2 / sum_k (b_k - c_k)^2 over patients k, else McNemar's
# chi-square (b - c)^2 / (b + c), without continuity correction (the two agree
# with one eye per patient). b: preop above the interval, postop within it
# (improved); c: the reverse (worsened). count_id: the patients counted
# (n_pat; the mapped ID also when the tests do not cluster).
astig_mcnemar <- function(pre, post, id = NULL, cut = 0.5, count_id = id) {
  ok <- is.finite(pre) & is.finite(post)
  a <- pre[ok] <= cut + ASTIG_TOL; b <- post[ok] <= cut + ASTIG_TOL
  imp <- !a & b; wor <- a & !b
  out <- list(n = sum(ok), n_pat = if (is.null(count_id)) NA_integer_ else length(unique(fill_missing_ids(count_id[ok]))),
              pre_pct = if (any(ok)) 100 * mean(a) else NA_real_,
              post_pct = if (any(ok)) 100 * mean(b) else NA_real_,
              improved = sum(imp), worsened = sum(wor), test = "", stat = NA_real_, p = NA_real_, why = "")
  nd <- out$improved + out$worsened
  if (!any(ok)) { out$why <- "no eye has both refractions"; return(out) }
  if (nd == 0) {
    out$why <- "no discordant eyes (every eye on the same side of the interval before and after)"
    return(out)
  }
  idk <- if (is.null(id)) NULL else fill_missing_ids(id[ok])
  # clustered only when a patient has two discordant eyes: otherwise every
  # patient's d_k is -1, 0 or 1 and the eye-level test is the patient-level one
  clus <- !is.null(idk) && fellow_eyes_present(idk[imp | wor])
  # with clustering on, a single patient is no test (as every other test of
  # the cohort says)
  if (!is.null(idk) && length(unique(idk[imp | wor])) < 2) {
    out$test <- if (clus) "McNemar exact, cluster-level (sign test on patients)" else "McNemar exact (binomial)"
    out$why <- "the discordant eyes are from 1 patient"
    return(out)
  }
  if (nd < 25 && clus) {
    out$test <- "McNemar exact, cluster-level (sign test on patients; fewer than 25 discordant eyes)"
    dk <- rowsum(as.numeric(imp) - as.numeric(wor), idk, reorder = FALSE)[, 1]
    kk <- sum(dk != 0)
    if (kk == 0) {
      out$why <- "no patient has a net change (each patient's improved and worsened eyes cancel)"
      return(out)
    }
    out$p <- min(1, 2 * stats::pbinom(min(sum(dk > 0), sum(dk < 0)), kk, 0.5))
    return(out)
  }
  if (nd < 25) {
    out$test <- "McNemar exact (binomial; fewer than 25 discordant eyes)"
    out$p <- min(1, 2 * stats::pbinom(min(out$improved, out$worsened), nd, 0.5))
    return(out)
  }
  if (clus) {
    dk <- rowsum(as.numeric(imp) - as.numeric(wor), idk, reorder = FALSE)[, 1]
    out$test <- "McNemar, cluster-adjusted (Durkalski 2003)"
    if (sum(dk^2) == 0) {
      out$why <- "no patient has a net change (each patient's improved and worsened eyes cancel)"
      return(out)
    }
    out$stat <- sum(dk)^2 / sum(dk^2)
  } else {
    out$stat <- (out$improved - out$worsened)^2 / nd
    out$test <- "McNemar chi-square (no continuity correction)"
  }
  out$p <- stats::pchisq(out$stat, 1, lower.tail = FALSE)
  out
}

# The run-level cache of astig_vector_stats: the astigmatism sheet (built with
# the workbook) and the double-angle figures (drawn after it) ask for the same
# statistics, which include several bootstrap tests; the second ask is served
# from here. The key is a fingerprint of everything the result depends on.
.astig_cache <- new.env(parent = emptyenv())
astig_cache_reset <- function() rm(list = ls(.astig_cache, all.names = TRUE), envir = .astig_cache)
astig_key <- function(...) {
  fp <- function(v) {
    if (is.null(v)) return("-")
    if (is.character(v) || is.factor(v)) v <- as.integer(factor(as.character(v), levels = unique(as.character(v))))
    v <- as.numeric(v); v[!is.finite(v)] <- -7.77
    i <- seq_along(v)
    sprintf("%d:%.15g:%.15g", length(v), sum(v * sqrt(i)), sum(v^2 * (1 + i %% 7)))
  }
  paste(vapply(list(...), function(z) if (is.list(z)) paste(unlist(z), collapse = "|")
                                     else if (length(z) == 1 && is.character(z)) z else fp(z),
               character(1)), collapse = "#")
}

# Everything the standard asks for ONE vector of one cohort set (D4-D7):
#   m, a     : magnitude and plus axis of each eye
#   g, glv   : cohort of each eye and the cohort order (reference first)
#   id       : patient id of each eye when the tests cluster (NULL: eyes
#              independent); the "classic" method never clusters
#   method   : astig_test_method - "robust" (default), "gee" or "classic"
#   region   : the data region drawn ("polygon" = the depth-based convex polygon, "normal",
#              "both"); centroid_region: "bootstrap" (default) or "hotelling"
#   matched  : a matched cohort (independence working correlation, D18)
#   w        : matching weights (the gee / classic centroid test is the
#              weighted GEE then, as in v1.0.0; the other tests are unweighted)
#   count_id : patient id of each eye for the patient COUNTS of the summary
#              (D4: the mapped ID, also when the tests do not cluster);
#              NULL: counted from `id`
# Returns list(summary, regions, zero, between, method) - see the sheet writer
# (export.R astig_jcrs_sections) for how each is printed.
astig_vector_stats <- function(m, a, g, glv, id = NULL, method = "robust", region = "polygon",
                               centroid_region = "bootstrap", matched = FALSE, w = NULL, key = "",
                               logger = NULL, what = key, count_id = NULL) {
  if (is.null(count_id)) count_id <- id
  ck <- astig_key(key, method, region, centroid_region, as.character(isTRUE(matched)),
                  gee_small_sample(), list(glv), m, a, as.character(g), id, w, count_id)
  hit <- .astig_cache[[ck]]
  if (!is.null(hit)) return(hit)
  m <- abs(gr_num(m)); a <- gr_num(a) %% 180
  ok <- is.finite(m) & is.finite(a) & !is.na(g) & g %in% glv
  # the bootstrap tests are the slow step of a run: say what is running
  if (!is.null(logger) && identical(method, "robust") && sum(ok) > 0)
    logger(sprintf("Astigmatism %s: JCRS 2025 statistics, %d cohort(s), %d eyes - robust tests with B = %d bootstrap resamples each ...",
                   what, length(unique(as.character(g[ok]))), sum(ok), ASTIG_BOOT_B))
  m <- m[ok]; a <- a[ok]
  gg <- factor(as.character(g[ok]), levels = glv)
  present <- glv[glv %in% levels(droplevels(gg))]
  gg <- factor(as.character(gg), levels = present)
  idk <- if (is.null(id)) NULL else fill_missing_ids(id[ok])
  nidk <- if (is.null(count_id)) NULL else fill_missing_ids(count_id[ok])
  wk <- if (is.null(w)) NULL else w[ok]
  x <- m * cospi(a / 90); y <- m * sinpi(a / 90)
  cid <- if (identical(method, "classic")) NULL else idk   # the clustering of the tests
  rob <- identical(method, "robust")

  # ---- per cohort: summary, regions, centroid against zero ----
  summ <- list(); regs <- list(); zero <- list()
  for (gn in present) {
    k <- gg == gn
    s <- astig_summary1(m[k], a[k], nidk[k])
    reg_m <- if (identical(region, "normal")) "normal" else "depth"
    rr <- astig_region(x[k], y[k], 0.95, reg_m)
    dr <- astig_region_path(rr)
    er <- if (identical(region, "both")) astig_region_path(astig_region(x[k], y[k], 0.95, "normal")) else NULL
    cr <- astig_centroid_region(x[k], y[k], if (is.null(cid)) NULL else cid[k], centroid_region)
    regs[[gn]] <- list(data = dr, ellipse = er, centroid = cr,
                       data_why = if (is.null(dr)) rr$note else "",
                       centroid_why = if (is.null(cr)) centroid_skip_why(x[k], y[k],
                                        if (is.null(cid)) NULL else cid[k]) else "")
    summ[[gn]] <- data.frame(group = gn, n = s$n, n_pat = s$n_pat, cx = s$cx, cy = s$cy, cm = s$cm,
                             ca = s$ca, tsd = s$tsd, mmean = s$mmean, msd = s$msd, mmed = s$mmed,
                             mmin = s$mmin, mmax = s$mmax, rms = s$rms,
                             t(stats::setNames(s$cum, sprintf("le%.2f", ASTIG_CUTS))),
                             gt1 = s$gt1, check.names = FALSE, stringsAsFactors = FALSE)
    pz <- astig_zero_p(x[k], y[k], if (is.null(cid)) NULL else cid[k], method)
    zero[[gn]] <- data.frame(group = gn, n = s$n, n_pat = s$n_pat, test = attr(pz, "test") %||% "",
                             p = as.numeric(pz), B = attr(pz, "B") %||% NA_integer_,
                             floor = isTRUE(attr(pz, "floor")),
                             why = attr(pz, "why") %||% "", stringsAsFactors = FALSE)
    # the caution names the cohort (astig_zero_p sees one cohort and calls it
    # "cohort")
    f <- attr(pz, "few")
    if (length(f)) names(f) <- gn
    attr(zero[[gn]], "few") <- f
  }
  summary <- do.call(rbind, summ)
  zero_df <- do.call(rbind, zero)
  # Holm across the cohorts of this vector
  zero_df$p_holm <- if (sum(is.finite(zero_df$p)) >= 2) stats::p.adjust(zero_df$p, "holm") else NA_real_
  # unnamed: unlist() of a named list would prefix each name ("Cohort C.Cohort C")
  # and the duplicate check of gr_few_merge would miss the repeats
  zero_few <- do.call(gr_few_merge, unname(lapply(zero, function(z) attr(z, "few"))))

  # ---- between cohorts (D7b-c) ----
  dist <- rep(NA_real_, length(x))      # each eye's distance to its cohort centroid
  for (gn in present) {
    k <- gg == gn
    dist[k] <- sqrt((x[k] - mean(x[k]))^2 + (y[k] - mean(y[k]))^2)
  }
  measures <- c("Centroid", "Precision", "Mean magnitude", sprintf("<= %.2f D", ASTIG_CUTS))
  # the cohort values a comparison is about, as text ("Reference 0.31 D;
  # Cohort A 0.29 D")
  val_txt <- function(v, gk, f, fmt) {
    tv <- tapply(v, gk, f)
    paste(sprintf(paste0("%s ", fmt), names(tv), tv), collapse = "; ")
  }
  one_cmp <- function(keep, lv) {
    gk <- droplevels(factor(as.character(gg[keep]), levels = lv))
    ik <- if (is.null(cid)) NULL else cid[keep]
    res <- list()
    pc <- if (rob) astig_robust_biv_p(x[keep], y[keep], gk, id = ik)
          else astig_bivariate_p(x[keep], y[keep], gk, id = if (identical(method, "gee")) ik else NULL,
                                 w = if (is.null(wk)) NULL else wk[keep], matched = matched)
    # (the test meant, on a centroid comparison that could not run at all)
    if (rob && !nzchar(as.character(attr(pc, "test") %||% "")[1]))
      attr(pc, "test") <- sprintf("robust %d%%-trimmed T², bootstrap", as.integer(round(ASTIG_TRIM * 100)))
    res[[1]] <- pc
    sc <- function(v) if (rob) astig_trimmed_p(v[keep], gk, ik)
                      else astig_univ_p(v[keep], gk, ik, "continuous", matched)
    res[[2]] <- sc(dist)
    res[[3]] <- sc(m)
    for (cc in ASTIG_CUTS) {
      yk <- m[keep] <= cc + ASTIG_TOL
      r <- astig_univ_p(yk, gk, ik, "proportion", matched)
      # the same interval for every eye: no test, said in the interval's words
      if (identical(attr(r, "why"), "variable is constant in this cohort"))
        attr(r, "why") <- if (all(yk)) sprintf("every eye is within %.2f D", cc)
                          else sprintf("no eye is within %.2f D", cc)
      res[[length(res) + 1]] <- r
    }
    # the cohort values the test compared: trimmed means for a trimmed test,
    # plain means for every other (an untrimmed bootstrap test included, R2-5)
    trimmed <- function(r) rob && identical(attr(r, "variant"), "trimmed")
    tmf <- function(r) if (trimmed(r)) function(z) mean(z, trim = ASTIG_TRIM) else mean
    tml <- function(r) if (trimmed(r)) sprintf("%d%%-trimmed mean: ", as.integer(round(ASTIG_TRIM * 100))) else "mean: "
    vals <- c(paste(vapply(lv, function(l) {
                  k <- gk == l
                  paste0(l, " ", astig_polar_text(sqrt(mean(x[keep][k])^2 + mean(y[keep][k])^2),
                                                  (atan2(mean(y[keep][k]), mean(x[keep][k])) * 180 / pi / 2) %% 180))
                }, character(1)), collapse = "; "),
              paste0(tml(res[[2]]), val_txt(dist[keep], gk, tmf(res[[2]]), "%.2f D")),
              paste0(tml(res[[3]]), val_txt(m[keep], gk, tmf(res[[3]]), "%.2f D")),
              vapply(ASTIG_CUTS, function(cc)
                val_txt(m[keep] <= cc + ASTIG_TOL, gk, function(z) 100 * mean(z), "%.1f%%"), character(1)))
    out <- data.frame(measure = measures,
                      test = vapply(res, function(r) as.character(attr(r, "test") %||% "")[1], character(1)),
                      p = vapply(res, function(r) as.numeric(r)[1], numeric(1)),
                      B = vapply(res, function(r) as.numeric(attr(r, "B") %||% NA)[1], numeric(1)),
                      fallback = vapply(res, function(r) isTRUE(attr(r, "fallback")), logical(1)),
                      why = vapply(res, function(r) as.character(attr(r, "why") %||% "")[1], character(1)),
                      values = vals, stringsAsFactors = FALSE)
    attr(out, "few") <- do.call(gr_few_merge, lapply(res, function(r) if (is.finite(r)) attr(r, "few")))
    out
  }
  between <- NULL; bfew <- NULL
  if (length(present) >= 2) {
    blocks <- list()
    if (length(present) > 2) {
      om <- one_cmp(rep(TRUE, length(x)), present)
      om$group <- NA_character_; om$ref <- present[1]; om$cmp <- "omnibus"
      blocks[[1]] <- om
    }
    for (o in present[-1]) {
      vs <- one_cmp(as.character(gg) %in% c(present[1], o), c(present[1], o))
      vs$group <- o; vs$ref <- present[1]; vs$cmp <- "vs"
      blocks[[length(blocks) + 1]] <- vs
    }
    bfew <- do.call(gr_few_merge, lapply(blocks, function(b) attr(b, "few")))
    between <- do.call(rbind, blocks)
    # Holm (D7d): across the cohorts compared with the reference, per measure
    # (the figures' "vs reference" lines, as v1.0.0), and across the family of
    # this vector within ONE comparison - every measure of the omnibus test, or
    # every measure of one cohort against the reference (each comparison is a
    # family of its own; the adjustment across cohorts is the column above)
    between$p_holm_ref <- NA_real_
    if (length(present) > 2)
      for (ms in measures) {
        k <- between$cmp == "vs" & between$measure == ms
        if (sum(is.finite(between$p[k])) >= 2) between$p_holm_ref[k] <- stats::p.adjust(between$p[k], "holm")
      }
    between$p_holm_fam <- NA_real_
    fam <- paste(between$cmp, between$group)
    for (ff in unique(fam)) {
      k <- fam == ff
      if (sum(is.finite(between$p[k])) >= 2) between$p_holm_fam[k] <- stats::p.adjust(between$p[k], "holm")
    }
  }
  # the caution names the cohorts in their order (reference first)
  few <- gr_few_merge(zero_few, bfew)
  if (length(few)) {
    u <- attr(few, "unit")
    few <- few[order(match(names(few), present))]
    attr(few, "unit") <- u
  }
  out <- list(summary = summary, regions = regs, zero = zero_df, between = between,
              method = method, region = region, centroid_region = centroid_region,
              clustered = !is.null(cid), present = present, few = few)
  assign(ck, out, envir = .astig_cache)
  out
}

# The cluster ids of the astigmatism tests of a cohort `df` (the figures' rule:
# clustering on, a patient ID column mapped and in the data, geepack
# installed), the "(no ID) row <n>" tokens filled in; NULL otherwise.
astig_cluster_ids <- function(df, cfg, res) {
  pid <- res$patient_id_clean
  if (!isTRUE(cfg$cluster_by_patient %||% TRUE) || is.null(pid) || is.na(pid[1]) ||
      !(pid[1] %in% names(df)) || !requireNamespace("geepack", quietly = TRUE)) return(NULL)
  fill_missing_ids(df[[pid[1]]])
}
# The patient of each eye of `df` for COUNTING patients (the Patients columns
# and the "n patients" of a double-angle header, D4): the mapped patient ID
# column whenever it is in the data, whether or not the tests cluster
# (clustering off, geepack missing); NULL without one.
astig_count_ids <- function(df, res) {
  pid <- res$patient_id_clean
  if (is.null(pid) || is.na(pid[1]) || !(pid[1] %in% names(df))) return(NULL)
  fill_missing_ids(df[[pid[1]]])
}

# The astigmatism settings of a run, read once (blank / unknown = the default)
astig_settings <- function(cfg) {
  one <- function(v, ok, def) { v <- tolower(trimws(as.character(unlist(v))[1]))
    if (length(v) && !is.na(v) && v %in% ok) v else def }
  list(method = one(cfg$astig_test_method, c("robust", "gee", "classic"), "robust"),
       region = one(cfg$astig_region, c("polygon", "depth", "normal", "both"), "polygon"),
       centroid_region = one(cfg$astig_centroid_region, c("bootstrap", "hotelling"), "bootstrap"),
       jcrs = isTRUE(cfg$astig_jcrs_standard %||% TRUE))
}

# The two regions in words (decisions R3-1, R3-2), one source for the legend
# files, the sheet's notes and the Manifest. unit: what the centroid's
# bootstrap resamples ("patients" or "eyes").
astig_region_text <- function()
  paste0("95% ", ASTIG_REGION_LABEL, " - the convex hull of the eyes whose depth (the smallest share of eyes ",
         "on either side of the eye's projection, over the lines through the centroid and each eye; hull vertices ",
         "take the smallest depth; steps of 1/n) is at least the depth of rank floor(0.05 n + 0.5); depths tie, so ",
         "it can hold more than 95% of the eyes (the count is given); with fewer than ", ASTIG_REGION_MIN_N,
         " eyes the convex hull of all the eyes")
astig_centroid_region_text <- function(unit = "patients")
  sprintf(paste0("%s - the convex hull of the bootstrap centroids nearest the centroid by projection distance ",
                 "(interquartile range by the ideal fourths), %d bootstrap resamples with %s resampled (seed %d), ",
                 "keeping 95%% of them for more than 120 resampling units and 95.5%% to 98%% for 120 or fewer"),
          ASTIG_CENTROID_LABEL, ASTIG_BOOT_B, unit, ASTIG_CENTROID_SEED)
ASTIG_CENTROID_LABEL <- "bootstrap convex polygon of the centroid (as in the WHWK functions)"

# The method text of a run's astigmatism statistics, for the figure legends,
# the sheet's notes and the Manifest
astig_method_text <- function(st, clustered = st$clustered) {
  unit <- if (clustered) "patients" else "eyes"
  c(paste0("Vectors in plus-cylinder format (JCRS 2025): x = M cos 2θ, y = M sin 2θ, θ = plus axis; ",
           "with-the-rule astigmatism to the left, against-the-rule to the right; SIA = postoperative - ",
           "preoperative, PE = postoperative - target."),
    paste0("Total SD = sqrt(σx² + σy²) with the population (N) denominator (Part II)."),
    paste0("Data region: ", if (identical(st$region, "normal")) "95% bivariate-normal ellipse"
           else paste0(astig_region_text(), if (identical(st$region, "both")) "; with the 95% bivariate-normal ellipse" else ""),
           "."),
    paste0("Centroid 95% confidence region: ",
           if (identical(st$centroid_region, "hotelling")) paste0("Hotelling T² ellipse",
             if (clustered) " (sandwich covariance, clustered by patient)" else "")
           else astig_centroid_region_text(unit), "."),
    paste0("Tests: ", switch(st$method,
      robust = sprintf(paste0("robust - centroid against zero by the %s: %d bootstrap centroids (seed ",
                              ASTIG_CENTROID_SEED, ", the resamples of the bootstrap convex polygon of the centroid) ",
                              "and the null point (0, 0) form one cloud, and P is the share of the ",
                              "bootstrap centroids whose projection distance from the centroid is greater than that ",
                              "of the null point (smallest P 1/B: P = .001 with B = %d is the bootstrap floor); ",
                              "between cohorts, centroids by a %d%%-trimmed Hotelling-type T² (pooled winsorized ",
                              "covariance), precision and mean magnitude by %d%%-trimmed means (Yuen / Welch-type), ",
                              "each calibrated by %d bootstrap resamples with %s resampled within each cohort ",
                              "(seed %d). Trimming and winsorizing by order statistics: g = floor(0.2 n) values at ",
                              "each end (Wilcox). A trimmed test is used only when its statistic exists in the data ",
                              "and in at least %d%% of the resamples; otherwise the same bootstrap test runs untrimmed ",
                              "(ordinary means and covariance) and is labeled '%s'. Resampling continues until exactly ",
                              "%d usable resamples (at most %d drawn), else the test is not computed; each test is ",
                              "named with the variant that ran"),
                       ASTIG_ZERO_TEST, ASTIG_BOOT_B, ASTIG_BOOT_B,
                       as.integer(ASTIG_TRIM * 100), as.integer(ASTIG_TRIM * 100), ASTIG_BOOT_B, unit, ASTIG_SEED,
                       as.integer(round(ASTIG_TRIM_OK * 100)), ASTIG_UNTRIMMED, ASTIG_BOOT_B,
                       ASTIG_BOOT_MAX * ASTIG_BOOT_B),
      gee = paste0("clustered GEE - centroids by the joint Wald test of both components (stacked), precision ",
                   "and mean magnitude by a Gaussian GEE, centroid against zero by the intercepts' joint Wald ",
                   "test; ", ASTIG_GEE_WC, "; ", gee_inference_text(unit = if (clustered) "patient" else "eye")),
      classic = "classic - Hotelling T² / Pillai MANOVA, Welch t / ANOVA, one-sample Hotelling T²; eyes independent"),
      "; cumulative intervals between cohorts by ",
      if (clustered) paste0("a binomial GEE clustered by patient (", ASTIG_GEE_WC, "; ", gee_se_label(1),
                            if (length(st$present) > 2 && identical(gee_small_sample(), "md"))
                              "; F reference for the omnibus test",
                            "; Fisher's exact test with one eye per patient; when the GEE cannot be fitted - ",
                            "typically a cohort at 0% or 100% - a permutation test over patients: the cohort ",
                            "labels permuted across patients, the eyes of a patient kept together, ",
                            ASTIG_PERM_B, " permutations, seed ", ASTIG_SEED, ")")
      else "Fisher's exact test", "."),
    "Precision = each eye's distance to its cohort centroid.")
}

# ============================================================
# Input files for the journal's reference tool (decision R3-3): the Wilcox-
# Holladay-Wang-Koch (WHWK) R functions that JCRS 2025 Part II recommends
# (https://osf.io/xhe8u/). When the double-angle figures are drawn, the run
# writes into the sub-folder "WHWK input" of the figures' folder one pair of
# tab-delimited files per plotted vector (PRE, SIA, PE, and POST when plotted):
#   <vector>_vector.txt    - two columns per cohort, <Cohort>.x and <Cohort>.y
#                            (plus-cylinder double-angle form, as plotted);
#   <vector>_magnitude.txt - one column per cohort, the magnitude (D);
# a matched pairing adds its file tag (<vector>_vector_Matched_<pairing>.txt).
# One row per eye in the order of the analyzed data (the order the app's own
# bootstrap indexes, so a validation script can feed the tool's resamples
# back: astig_boot_centroids), shorter cohorts padded with empty cells, cohort
# names reduced to letters and digits; a README.txt says what the files are.
# Setting astig_whwk_files (default on).
# ============================================================
WHWK_DIR <- "WHWK input"
WHWK_URL <- "https://osf.io/xhe8u/"
WHWK_MAX_D <- 6
# the cohort names and the out-of-range magnitudes already written into each
# folder by an earlier pass of the run (unmatched, then each matched pairing)
.whwk_seen <- new.env(parent = emptyenv())

# Column names for the cohorts: letters and digits only (no spaces or dots),
# a leading digit prefixed with "G" (R would otherwise rename the column on
# reading), an empty result named "Cohort<k>", repeats numbered.
whwk_names <- function(g) {
  nm <- gsub("[^A-Za-z0-9]", "", iconv(as.character(g), to = "ASCII//TRANSLIT", sub = ""))
  nm[is.na(nm)] <- ""
  nm[!nzchar(nm)] <- paste0("Cohort", seq_along(g))[!nzchar(nm)]
  nm <- ifelse(grepl("^[0-9]", nm), paste0("G", nm), nm)
  for (i in seq_along(nm)) {
    k <- 1L; b <- nm[i]
    while (nm[i] %in% nm[seq_len(i - 1L)]) { k <- k + 1L; nm[i] <- paste0(b, k) }
  }
  nm
}

# Numbers as written: 12 significant digits (12 decimals at most), no "-0"
whwk_num <- function(v) {
  v <- round(v, 12)
  v[v == 0] <- 0
  sprintf("%.12g", v)
}

# Write one tab-delimited table: `cols` a named list of numeric vectors of any
# lengths, padded with empty cells. TRUE when the file was written.
whwk_write_table <- function(cols, fn, logger) {
  L <- max(c(0L, lengths(cols)))
  cells <- vapply(cols, function(v) { s <- rep("", L); if (length(v)) s[seq_along(v)] <- whwk_num(v); s },
                  character(L))
  if (!is.matrix(cells)) cells <- matrix(cells, nrow = L)
  lines <- c(paste(names(cols), collapse = "\t"),
             if (L) apply(cells, 1, paste, collapse = "\t"))
  ok <- tryCatch({ writeLines(lines, fn, useBytes = TRUE); TRUE },
                 error = function(e) { logger(sprintf("WARNING: WHWK input file NOT written: %s (%s)", fn,
                                                      conditionMessage(e))); FALSE })
  isTRUE(ok) && file.exists(fn)
}

# vecs: the plotted double-angle vectors (astig_jcrs_vectors entries: key,
# mag, ax); gl / glv: each eye's cohort label and the cohort order; tag: the
# figure file tag of the pass ("" for the unmatched cohort); clustered: the
# run's own astigmatism tests resample patients. Returns the files written.
write_whwk_inputs <- function(vecs, gl, glv, out_dir, tag = "", logger = message, clustered = FALSE) {
  dir <- normalizePath(file.path(out_dir, WHWK_DIR), winslash = .Platform$file.sep, mustWork = FALSE)
  if (identical(.Platform$OS.type, "windows")) dir <- gsub("/", "\\", dir, fixed = TRUE)
  if (!dir.exists(dir)) dir.create(dir, recursive = TRUE, showWarnings = FALSE)
  if (!dir.exists(dir)) {
    logger(sprintf("WARNING: WHWK input files NOT written: the folder could not be created (%s)", dir))
    return(character(0))
  }
  written <- character(0); over <- character(0); map <- character(0)
  for (v in vecs) {
    m <- abs(gr_num(v$mag)); a <- gr_num(v$ax) %% 180
    ok <- is.finite(m) & is.finite(a) & !is.na(gl) & gl %in% glv
    present <- glv[glv %in% unique(gl[ok])]
    if (!length(present)) next
    nm <- whwk_names(present)
    map <- unique(c(map, sprintf("%s = %s", present, nm)))
    vc <- list(); mc <- list()
    for (i in seq_along(present)) {
      k <- ok & gl == present[i]
      vc[[paste0(nm[i], ".x")]] <- m[k] * cospi(a[k] / 90)
      vc[[paste0(nm[i], ".y")]] <- m[k] * sinpi(a[k] / 90)
      mc[[nm[i]]] <- m[k]
      nb <- sum(m[k] > WHWK_MAX_D + 1e-9)
      if (nb) over <- c(over, sprintf("%s, %s: %d eye%s", v$key, present[i], nb, if (nb == 1) "" else "s"))
    }
    for (f in list(list(paste0(v$key, "_vector", tag, ".txt"), vc), list(paste0(v$key, "_magnitude", tag, ".txt"), mc))) {
      fn <- file.path(dir, f[[1]])
      if (whwk_write_table(f[[2]], fn, logger)) written <- c(written, fn)
    }
  }
  if (!length(written)) return(written)
  # the README lists every file of the folder (a matched pairing's pass adds its
  # own), so it names the cohorts and the magnitudes beyond the tool's range of
  # every pass of this run that wrote into the folder, not only this one's
  seen <- .whwk_seen[[dir]] %||% list(map = character(0), over = character(0))
  over_tag <- if (length(over) && nzchar(tag)) paste0(over, " (", sub("^_", "", tag), ")") else over
  seen <- list(map = unique(c(seen$map, map)), over = unique(c(seen$over, over_tag)))
  .whwk_seen[[dir]] <- seen
  over_now <- over_tag
  map <- seen$map; over <- seen$over
  files <- sort(list.files(dir, pattern = "_(vector|magnitude).*[.]txt$"))
  rd <- c("StandardEyes - input files for the WHWK functions",
          "",
          paste0("These tab-delimited text files hold the astigmatism vectors of this run's double-angle plots, ",
                 "ready for the Wilcox-Holladay-Wang-Koch (WHWK) R functions - one of the two tools the JCRS 2025 ",
                 "standard for analyzing astigmatic outcomes (Koch et al., Part II) recommends. The functions are ",
                 "obtained from ", WHWK_URL, " (the journal's reference for them)."),
          "",
          "Files - one pair per plotted vector (PRE, SIA, PE, and POST when it is plotted; a matched pairing adds its name):",
          "  <vector>_vector.txt     two columns per cohort, <Cohort>.x and <Cohort>.y (D)",
          "  <vector>_magnitude.txt  one column per cohort: the magnitude (D)",
          "",
          paste0("Form: plus-cylinder double-angle - x = M cos 2θ, y = M sin 2θ, with M the magnitude and θ the ",
                 "plus axis; the coordinates of the double-angle plots (with-the-rule astigmatism, plus axis 90, ",
                 "at negative x). PRE = preoperative, POST = postoperative refractive astigmatism, SIA = ",
                 "postoperative - preoperative, PE = postoperative - target."),
          paste0("Rows: one per eye with both a magnitude and an axis, in the order of the analyzed data; shorter ",
                 "cohorts are padded with empty cells (read as NA). Values are written with 12 significant digits."),
          paste0("Cohort names are reduced to letters and digits (no spaces or dots): ", paste(map, collapse = "; "), "."),
          paste0("Independence: the WHWK functions treat eyes as independent, so when patients contribute both ",
                 "eyes their P values ignore the pairing. This run's own astigmatism tests ",
                 if (isTRUE(clustered)) "resample patients (all eyes of a drawn patient together)."
                 else "treat the eyes as independent too (no patient ID mapped, or clustering off)."),
          paste0("Range: the WHWK wrap-up functions refuse values beyond ", WHWK_MAX_D, " D. ",
                 if (length(over)) paste0("Magnitudes above ", WHWK_MAX_D, " D in these files - ",
                                          paste(over, collapse = "; "), ".")
                 else paste0("No magnitude in these files exceeds ", WHWK_MAX_D, " D.")),
          "",
          "Files in this folder:", paste0("  ", files))
  rf <- file.path(dir, "README.txt")
  tryCatch({
    con <- file(rf, open = "w", encoding = "UTF-8")
    on.exit(close(con), add = TRUE)
    writeLines(rd, con)
  }, error = function(e) logger(sprintf("WARNING: WHWK README NOT written: %s (%s)", rf, conditionMessage(e))))
  logger(sprintf("WHWK input files%s: %d file(s) and README.txt in %s", if (nzchar(tag)) paste0(" (", sub("^_", "", tag), ")") else "",
                 length(written), dir))
  if (length(over_now))
    logger(sprintf("WHWK input files: magnitudes above %g D (the WHWK wrap-up functions refuse them) - %s.",
                   WHWK_MAX_D, paste(over_now, collapse = "; ")))
  written
}

# Size (inches) of a double-angle / semicircle plot: its own file and its cell in
# the composite. The header is fitted to it (gr_polar_finish).
DA_W <- 6.8; DA_H <- 6.6
SC_W <- 7.2; SC_H <- 5.4
POLAR_GAP_IN <- 0.14      # white space between the framed panels of their composites

# The statistics of astig_vector_stats with every cohort name mapped through
# `lab` (original group name -> the label drawn; cfg$group_labels), so the
# figures print their labels while the computation - and its cache entry -
# stays the sheet's.
astig_stats_relabel <- function(st, lab = NULL) {
  if (is.null(lab) || is.null(st)) return(st)
  f <- function(v) { v <- as.character(v); hit <- !is.na(v) & v %in% names(lab)
    v[hit] <- unname(lab[v[hit]]); v }
  st$summary$group <- f(st$summary$group)
  st$zero$group <- f(st$zero$group)
  if (!is.null(st$between)) { st$between$group <- f(st$between$group); st$between$ref <- f(st$between$ref) }
  names(st$regions) <- f(names(st$regions))
  st$present <- f(st$present)
  if (length(st$few)) names(st$few) <- f(names(st$few))
  st
}

# The test label of a figure line: the method's test with the resampling unit
g_astig_tlab <- function(test, B = NA, clustered = FALSE) {
  test <- as.character(test %||% "")[1]
  # the one-sample test of R3-2 resamples within its own cohort
  if (identical(test, ASTIG_ZERO_TEST))
    return(sprintf("%s, B = %d, %s resampled", test, as.integer(if (is.finite(B)) B else ASTIG_BOOT_B),
                   if (clustered) "patients" else "eyes"))
  if (grepl("bootstrap", test, fixed = TRUE))
    paste0(sub(", bootstrap", "", test, fixed = TRUE),
           sprintf(", B = %d, %s resampled within cohorts",
                   as.integer(if (is.finite(B)) B else ASTIG_BOOT_B), if (clustered) "patients" else "eyes"))
  else switch(test, "Hotelling" = "Hotelling T²", "Pillai" = "Pillai MANOVA",
              "weighted GEE" = "weighted bivariate GEE", "clustered GEE" = "clustered bivariate GEE", test)
}

# st: the vector's statistics (astig_vector_stats, cohorts named as `lab`
# maps them; computed here when NULL). ellipse: the data region ("polygon" =
# the depth-based convex polygon, "normal", "both"); centroid_region: "bootstrap" or
# "hotelling" (D6).
g_double_angle_plot <- function(mag, axisdeg, gl, glv, pal, title,
                                unit_lab = "Diopters", ellipse = "polygon",
                                show_centroid_ellipse = TRUE, jcrs = TRUE,
                                test_method = "robust", fig_w = DA_W, fig_h = DA_H,
                                st = NULL, centroid_region = "bootstrap", lab = NULL) {
  m <- abs(gr_num(mag)); a <- gr_num(axisdeg) %% 180
  ok <- is.finite(m) & is.finite(a) & !is.na(gl) & gl %in% glv
  if (sum(ok) < 3) stop("too few eyes")
  cid0 <- gr_cluster_of(gl)
  did <- if (is.null(cid0)) NULL else cid0[ok]     # aligned with d, below
  w0 <- gr_weights_of(gl)
  dw <- if (is.null(w0)) NULL else w0[ok]          # matching weights, aligned with d
  if (is.null(st))
    st <- astig_vector_stats(m, a, gl, glv, id = cid0, method = test_method, region = ellipse,
                             centroid_region = centroid_region, matched = isTRUE(.gr_opts$matched),
                             w = w0, key = title)
  else st <- astig_stats_relabel(st, lab)
  d <- data.frame(x = m[ok] * cospi(a[ok] / 90),
                  y = m[ok] * sinpi(a[ok] / 90),
                  grp = factor(as.character(gl[ok]), levels = glv))
  present <- glv[glv %in% levels(droplevels(d$grp))]
  sm <- st$summary
  srow <- function(g) sm[sm$group == g, , drop = FALSE]

  rmax <- max(sqrt(d$x^2 + d$y^2), 0.5, na.rm = TRUE)
  rings <- pretty(c(0, rmax), 4); rings <- rings[rings > 0]
  rmax <- max(rings)
  th <- seq(0, 2 * pi, length.out = 241)

  circ <- do.call(rbind, lapply(rings, function(rr)
    data.frame(r = rr, x = rr * cos(th), y = rr * sin(th))))
  spk <- do.call(rbind, lapply(seq(0, 330, by = 30), function(s)
    data.frame(a = s, x = c(0, rmax * cos(s * pi / 180)), y = c(0, rmax * sin(s * pi / 180)))))
  # spokes are labelled with the PLUS AXIS (half the displayed angle), so the
  # reader never has to double anything in their head: 90° (with-the-rule) on
  # the left, 0° (against-the-rule) on the right
  labd <- data.frame(a = seq(0, 330, by = 30))
  labd$lab <- paste0(labd$a / 2, "°")
  labd$x <- rmax * 1.14 * cos(labd$a * pi / 180)
  labd$y <- rmax * 1.14 * sin(labd$a * pi / 180)

  # alternating 30-degree "pizza slices" behind everything, so the eye can count
  # meridians without tracing back to a spoke label
  wedge <- function(a1, a2, fill) {
    tt <- seq(a1 * pi / 180, a2 * pi / 180, length.out = 40)
    ggplot2::annotate("polygon", x = c(0, rmax * cos(tt)), y = c(0, rmax * sin(tt)),
                      fill = fill, colour = NA)
  }
  # (none in the journal style: no shaded background, decision D20)
  slices <- if (isTRUE(.gr_opts$journal)) list() else lapply(seq(0, 330, by = 30), function(s)
    wedge(s, s + 30, if ((s / 30) %% 2 == 0) "grey93" else "white"))

  p <- Reduce(`+`, slices, ggplot2::ggplot()) +
    ggplot2::geom_path(data = circ, ggplot2::aes(x, y, group = r),
                       colour = "grey30", linewidth = .38) +
    ggplot2::geom_line(data = spk, ggplot2::aes(x, y, group = a),
                       colour = "grey62", linewidth = .28) +
    ggplot2::geom_text(data = labd, ggplot2::aes(x, y, label = lab),
                       size = gr_mm(2.9), colour = "grey15", fontface = "bold") +
    ggplot2::geom_point(data = d, ggplot2::aes(x, y, colour = grp),
                        size = 1.5, alpha = .5)
  # The magnitude scale (ring_scale_grob): straight DOWN the 135° spoke,
  # clear of every meridian label, on white plates over the eyes and the data
  # regions, so it stays legible however dense they are, but UNDER the
  # centroids and their confidence regions, which it must never hide - laid
  # out when it is drawn, where the diagram's size is known, so it moves off a
  # centroid that would hide a label (finding F4). cd_size: the centroid
  # markers' size (below).
  cd_size <- 3.0 * max(0.7, gr_ts())
  cxy <- sm[sm$group %in% present & is.finite(sm$cx) & is.finite(sm$cy), c("cx", "cy"), drop = FALSE]
  ring_labels <- ggplot2::annotation_custom(
    ring_scale_grob(rings, rmax, cxy$cx, cxy$cy, mark_size = cd_size, spoke_fs = gr_pt(2.9 * GR_PT_PER_MM),
                    spoke_r = 1.14),
    xmin = -rmax, xmax = rmax, ymin = -rmax, ymax = rmax)

  # the 95% region of each cohort's eyes, as computed for the sheet (D5)
  inside <- character(0)       # "inside / n" per group, read back from the region drawn
  reg_skip <- character(0)     # a region not drawn, with its reason (R2-6: a line, not a polygon)
  for (g in present) {
    rg <- st$regions[[g]]
    gc <- unname(pal[g])
    dr <- rg$data
    if (!is.null(dr) && !is.null(attr(dr, "inside")))
      inside[g] <- if (identical(attr(dr, "rule"), "all"))
                     sprintf("all %d eyes (fewer than %d: the hull of every eye)", attr(dr, "n"), ASTIG_REGION_MIN_N)
                   else sprintf("%d of %d eyes", attr(dr, "inside"), attr(dr, "n"))
    else if (is.null(dr))
      reg_skip[g] <- if (nzchar(rg$data_why %||% "")) rg$data_why else sprintf("95%% region not drawn: n = %d", srow(g)$n)
    for (e1 in list(dr, rg$ellipse)) {
      if (is.null(e1)) next
      dashed <- identical(ellipse, "both") && identical(attr(e1, "method"), "depth")
      p <- p + ggplot2::annotate("path", x = e1$x, y = e1$y, colour = gc, linewidth = .55,
                                 linetype = if (dashed) "22" else "solid")
    }
  }
  cd <- data.frame(x = sm$cx, y = sm$cy, grp = sm$group, stringsAsFactors = FALSE)
  cd <- cd[cd$grp %in% present, , drop = FALSE]
  p <- p + ring_labels +
    ggplot2::geom_point(data = cd, ggplot2::aes(x, y, fill = grp), shape = 23,
                        size = cd_size, stroke = .7, colour = "grey10")
  # the 95% confidence region OF THE CENTROID goes on last: at large n it is
  # smaller than the marker (the centroid is precisely estimated), and it must
  # not be buried. Clustered by patient exactly when the printed tests are, so
  # the region and the P beside it answer the same question; a region that
  # would spill past the plotted rings (a handful of patients) is left out and
  # named in the legend instead of drawn off the plot.
  ci_basis <- character(0)
  ci_skip <- character(0)      # why a group's region is not drawn, named by group
  if (isTRUE(show_centroid_ellipse)) {
    for (g in present) {
      e2 <- st$regions[[g]]$centroid
      if (is.null(e2)) { ci_skip[g] <- st$regions[[g]]$centroid_why; next }
      if (max(sqrt(e2$x^2 + e2$y^2)) > rmax * 1.2) { ci_skip[g] <- "wider than the plot"; next }
      ci_basis <- c(ci_basis, attr(e2, "basis"))
      p <- p + ggplot2::annotate("path", x = e2$x, y = e2$y,
                                 colour = unname(pal[g]), linewidth = .8, linetype = "42")
    }
  }
  p <- p +
    ggplot2::scale_colour_manual(values = pal[present], drop = FALSE) +
    ggplot2::scale_fill_manual(values = pal[present], drop = FALSE) +
    ggplot2::labs(title = NULL, x = unit_lab, y = NULL) +
    ggplot2::theme_void(base_size = gr_pt(12)) +
    ggplot2::theme(legend.position = "none",
                   axis.title.x = ggplot2::element_text(face = "bold", size = gr_pt(10.5)),
                   # no frame around the figure in the journal style (D20)
                   plot.background = ggplot2::element_rect(fill = "white",
                                                           colour = if (isTRUE(.gr_opts$journal)) NA else "grey25",
                                                           linewidth = .8))
  # ---- statistics -----------------------------------------------------------
  # "P = .26" / "P < .001" in the run's P style (helpers.R p_eq, D15; the
  # classic style keeps "P = 0.262"); an untestable comparison prints
  # "P = n/a", never "P = NA" (nor "P = <.001")
  fp <- function(p) if (!is.finite(p)) "P = n/a" else p_eq(p, where = "table")
  rob <- identical(test_method, "robust")
  clus <- isTRUE(st$clustered)
  bt <- st$between
  brow <- function(cmp, ms) if (is.null(bt)) bt else bt[bt$cmp == cmp & bt$measure == ms, , drop = FALSE]
  # one line per measure: the omnibus (or the single) comparison, then each
  # cohort against the reference with 3+ cohorts ([Holm] across them)
  cmp_lines <- function(ms, lead) {
    if (is.null(bt)) return(NULL)
    vs <- brow("vs", ms)
    main <- if (length(present) > 2) brow("omnibus", ms) else vs[1, , drop = FALSE]
    tl <- g_astig_tlab(main$test, main$B, clus)
    if (isTRUE(main$fallback) || grepl("(GEE not fitted)", tl, fixed = TRUE))
      tl <- paste0(sub(" (GEE not fitted)", "", tl, fixed = TRUE), ", GEE not fitted")
    no_main <- !is.finite(main$p) && nzchar(main$why)
    first <- if (no_main) paste0(lead, p_not_tested(main$why))
             else paste0(lead, fp(main$p), "  [", tl, "]")
    if (length(present) <= 2) return(first)
    pv <- ifelse(is.finite(vs$p_holm_ref), vs$p_holm_ref, vs$p)
    # the test is named on the "vs" line when the omnibus line has no P
    tv <- vs$test[is.finite(vs$p)][1]
    tv_lab <- if (no_main && !is.na(tv))
      paste0("  [", sub(", GEE not fitted", "", g_astig_tlab(tv, vs$B[is.finite(vs$p)][1], clus), fixed = TRUE), "]")
      else ""
    c(first, paste0("vs ", present[1], ": ",
                    paste(sprintf("%s %s%s", vs$group, vapply(pv, fp, character(1)),
                                  ifelse(vs$fallback | grepl("GEE not fitted", vs$test, fixed = TRUE),
                                         " (GEE not fitted)", "")), collapse = "  ·  "),
                    tv_lab, if (sum(is.finite(vs$p)) >= 2) "  [Holm]" else ""))
  }
  # Journal style (decision R2-4): each test is ONE short line in the image -
  #   "Centroid: P = .24 · vs Reference: Cohort A P = .24, Cohort B P = .24 [Holm]"
  #   "Centroid vs 0: Reference P = .001, Cohort A P = .001, Cohort B P = .001"
  #   "Mean magnitude: P = .32 · vs Reference: Cohort A P = .25, Cohort B P = .76 [Holm]"
  # and the test that ran for each comparison (trimming or the untrimmed
  # variant, B, the resampling unit), complete, goes to the legend file
  # (test_desc). The annotated style keeps the bracketed test names in the image.
  gnf <- function(r) isTRUE(r$fallback) || grepl("GEE not fitted", as.character(r$test %||% ""), fixed = TRUE)
  short_cmp <- function(ms, lead) {
    if (is.null(bt)) return(NULL)
    vs <- brow("vs", ms)
    main <- if (length(present) > 2) brow("omnibus", ms) else vs[1, , drop = FALSE]
    first <- if (!is.finite(main$p)) {
               if (nzchar(main$why %||% "")) p_not_tested(main$why) else "P = n/a"
             } else paste0(fp(main$p), if (gnf(main)) " (GEE not fitted)" else "")
    if (length(present) <= 2) return(paste0(lead, first))
    pv <- ifelse(is.finite(vs$p_holm_ref), vs$p_holm_ref, vs$p)
    parts <- vapply(seq_len(nrow(vs)), function(i)
      paste0(vs$group[i], " ", if (is.finite(vs$p[i])) fp(pv[i]) else "n/a",
             if (is.finite(vs$p[i]) && gnf(vs[i, , drop = FALSE])) " (GEE not fitted)" else ""), character(1))
    paste0(lead, first, " · vs ", present[1], ": ", paste(parts, collapse = ", "),
           if (sum(is.finite(vs$p)) >= 2) " [Holm]" else "")
  }
  # the legend's account of one test line: each comparison with the test that
  # ran on it (grouped when several ran the same), or why it was not computed
  test_desc <- function(rows, labs, what) {
    if (is.null(rows) || !nrow(rows)) return(character(0))
    tl <- vapply(seq_len(nrow(rows)), function(i)
      if (is.finite(rows$p[i])) {
        t <- g_astig_tlab(rows$test[i], rows$B[i], clus)
        if (gnf(rows[i, , drop = FALSE])) paste0(sub(" (GEE not fitted)", "", t, fixed = TRUE),
                                                 " - the clustered GEE could not be fitted") else t
      } else paste0("not computed (", rows$why[i] %||% "", ")"), character(1))
    u <- unique(tl)
    paste0(what, " (test that ran): ", paste(vapply(u, function(t) paste0(paste(labs[tl == t], collapse = ", "), " - ", t),
                                    character(1)), collapse = "; "), ".")
  }
  cmp_desc <- function(ms, what) {
    if (is.null(bt)) return(character(0))
    rows <- rbind(if (length(present) > 2) brow("omnibus", ms), brow("vs", ms))
    test_desc(rows, ifelse(rows$cmp == "omnibus", "all cohorts", paste(rows$group, "vs", rows$ref)), what)
  }
  if (isTRUE(jcrs)) {
    journal_lines <- isTRUE(.gr_opts$journal)
    # JCRS 2025 Part II asks for a BIVARIATE comparison of the centroids; say
    # "P =" explicitly - a bare "Centroids: 0.26" reads as a distance in D
    stat_lines <- if (journal_lines) short_cmp("Centroid", "Centroid: ")
                  else cmp_lines("Centroid", "Centroid comparison: ")
    # each cohort's centroid against zero (D7a), and the mean magnitude - for
    # PE the standard's main outcome (D7b)
    z <- st$zero[st$zero$group %in% present, , drop = FALSE]
    if (nrow(z)) {
      zt <- z$test[is.finite(z$p)][1]
      stat_lines <- c(stat_lines, if (journal_lines) paste0("Centroid vs 0: ",
        paste(sprintf("%s %s", z$group, ifelse(is.finite(z$p), vapply(z$p, fp, character(1)), "n/a")),
              collapse = ", "))
        else paste0("Centroid vs 0: ",
        paste(ifelse(is.finite(z$p), sprintf("%s %s", z$group, vapply(z$p, fp, character(1))),
                     sprintf("%s %s", z$group, "P = n/a")), collapse = "  ·  "),
        if (!is.na(zt)) paste0("  [", g_astig_tlab(zt, z$B[is.finite(z$p)][1], clus), "]") else ""))
    }
    stat_lines <- c(stat_lines, if (journal_lines) short_cmp("Mean magnitude", "Mean magnitude: ")
                                else cmp_lines("Mean magnitude", "Mean magnitude: "))
    # the tests that ran, complete, for the legend file (R2-4, R2-5); a P of
    # the centroid against zero at the bootstrap floor 1/B is named (R3-2)
    zfl <- if (nrow(z) && "floor" %in% names(z)) z[is.finite(z$p) & z$floor %in% TRUE, , drop = FALSE] else z[0, ]
    floor_line <- if (nrow(zfl))
      sprintf("Centroid vs 0: %s is the bootstrap floor 1/B (B = %d) for %s: no bootstrap centroid lay farther from the centroid than zero.",
              fp(zfl$p[1]), as.integer(zfl$B[1]), paste(zfl$group, collapse = ", ")) else character(0)
    test_lines <- c(cmp_desc("Centroid", "Centroid comparison"),
                    if (nrow(z)) test_desc(z, z$group, "Centroid vs 0"), floor_line,
                    cmp_desc("Mean magnitude", "Mean magnitude comparison"))
    # a P value from a group of 2-9 patients: the caution line
    cau <- few_patients_text(st$few, short = !rob, bootstrap = rob)
    if (nzchar(cau)) {
      # (the short header of a narrow combined cell moves it to the legend, R2-2)
      if (isTRUE(.gr_opts$short_hdr)) gr_moved(cau) else stat_lines <- c(stat_lines, cau)
    }
    meth <- paste0(if (rob) paste0("JCRS 2025 Part II plot  ·  centroid vs 0 as in the WHWK functions  ·  ",
                                   "between cohorts robust trimmed tests (not the exact Part II variant), bootstrap over ")
                   else "Per JCRS 2025 Part II  ·  ",
                   if (rob) (if (clus) "patients within cohorts" else "eyes within cohorts")
                   else if (!is.null(dw)) paste0("matching weights for the centroid test (weighted GEE",
                                                 if (identical(test_method, "gee") && clus) ", clustered by patient" else "", ")")
                   else if (identical(test_method, "gee") && clus) "clustered by patient (GEE)"
                   else "eyes treated as independent")
    # the trimmed-mean bootstrap has no weighted form: say so on a matched cohort
    if (!is.null(dw))
      meth <- c(meth, "Matched cohort: the astigmatism tests other than a GEE centroid test do not use the matching weights")
    # (the annotated style writes no legend file: the floor is named in the image)
    if (!isTRUE(.gr_opts$journal)) meth <- c(meth, sub("[.]$", "", floor_line))
  } else {
    test_lines <- character(0)
    # p_omnibus looks the cluster ids / weights up by position: narrow them to
    # the eyes in `d`, or a single dropped eye would silently switch the test
    # to independent, unweighted eyes
    restore <- gr_narrow_scope(gl, ok)
    on.exit(restore(), add = TRUE)
    omx <- p_omnibus(d$x, as.character(d$grp), present, "continuous")
    omy <- p_omnibus(d$y, as.character(d$grp), present, "continuous")
    restore()
    om_txt <- function(om) if (nzchar(om$text)) om$text else p_not_tested(om$reason)
    stat_lines <- paste0("Centroid x: ", om_txt(omx), "   y: ", om_txt(omy))
    cau <- few_patients_text(gr_few_merge(omx$few, omy$few), short = TRUE)
    if (nzchar(cau)) stat_lines <- c(stat_lines, cau)
    # the method line names the test p_omnibus actually RAN for each P printed,
    # never the one asked for: a clustered GEE that could not be fitted fell back
    # to the unclustered test, and a weighted GEE is also clustered when patient
    # ids are in scope. x and y are named apart only when their tests differ.
    uni_meth <- function(t) {
      base <- sub(" (GEE not fitted)", "", t, fixed = TRUE)
      switch(base,
        "Weighted GEE (clustered by patient)" = "weighted GEE (matching weights), clustered by patient",
        "Weighted GEE (robust SE)" = "weighted GEE (matching weights)",
        "GEE (clustered by patient)" = "clustered by patient (GEE)",
        paste0(base, ", eyes treated as independent",
               if (!identical(base, t)) " (the clustered GEE could not be fitted)" else ""))
    }
    tx <- if (nzchar(omx$text)) omx$test else ""
    ty <- if (nzchar(omy$text)) omy$test else ""
    ran <- unique(c(tx, ty)[nzchar(c(tx, ty))])
    meth <- paste0("Separate univariate x/y tests - NOT the JCRS standard",
                   if (length(ran) == 1) paste0("  ·  ", uni_meth(ran))
                   else if (length(ran) == 2)
                     paste0("  ·  x: ", uni_meth(tx), "  ·  y: ", uni_meth(ty))
                   else "")
  }
  # (one style of line key: "Solid line, ...; dashed line, ...")
  reg <- if (identical(ellipse, "normal")) "Solid line, 95% bivariate-normal ellipse of the eyes (assumes Gaussian)"
         else if (identical(ellipse, "both"))
           paste0("Solid line, 95% bivariate-normal ellipse of the eyes; dotted line, 95% ", ASTIG_REGION_LABEL,
                  " of the eyes")
         else paste0("Solid line, 95% ", ASTIG_REGION_LABEL, " of the eyes")
  # the eyes inside or on each region, read from the regions drawn, so the
  # legend can never disagree with the drawing; each entry is named by its
  # group (a group too small for a region is named by skip_lines below)
  inside_lines <- if (length(inside)) {
    gi <- present[present %in% names(inside)]
    paste0("Eyes inside or on the 95% ", if (identical(ellipse, "normal")) "ellipse" else "polygon", ": ",
           paste(sprintf("%s %s", gi, inside[gi]), collapse = ", "))
  } else character(0)
  # a region not drawn, with its reason, once per reason (R2-6: "95% region not
  # drawn: all eyes lie on one meridian (Cohort D)")
  skip_lines <- if (length(reg_skip))
    vapply(unique(reg_skip), function(w) paste0(w, " (", paste(names(reg_skip)[reg_skip == w], collapse = ", "), ")."),
           character(1)) else character(0)
  # the dashed region's basis, as drawn: the region of the plain MEAN centroid
  # (the diamond)
  ci_txt <- if (!isTRUE(show_centroid_ellipse)) ""
            else paste0("; dashed line, 95% confidence region of the centroid: ",
                        if (identical(st$centroid_region, "hotelling")) "Hotelling T² ellipse" else
                          sprintf("%s, %d resamples", ASTIG_CENTROID_LABEL, ASTIG_BOOT_B),
                        if (!length(ci_basis)) ""
                        else if (all(ci_basis %in% c("clustered", "bootstrap over patients"))) ", clustered by patient"
                        else ", eyes independent")
  # each group left out, with its own reason
  ci_note <- if (isTRUE(show_centroid_ellipse) && length(ci_skip))
    paste0("Centroid confidence region not drawn: ",
           paste(sprintf("%s (%s)", names(ci_skip), ci_skip), collapse = ", "))
    else character(0)
  orient <- paste0("Plus-cylinder format: each spoke is labeled with the plus axis; with-the-rule ",
                   "astigmatism to the left, against-the-rule to the right; ◆ = centroid (mean vector).")
  # the journal style keeps only the short statistics lines (the P values with
  # their tests, and any caution) inside the image; the title and the method
  # text go to the figure's legend file (attr "legend", decision D20)
  # the tests behind the P values printed (the sheet's method notes say the same)
  mt <- astig_method_text(st)
  tests_txt <- if (isTRUE(jcrs)) grep("^(Tests|Precision)", mt, value = TRUE) else character(0)
  # the two regions' algorithms in full (R3-1, R3-2), the centroid's only when it is drawn
  region_txt <- grep(if (isTRUE(show_centroid_ellipse)) "^(Data region|Centroid 95%)" else "^Data region", mt,
                     value = TRUE)
  journal <- isTRUE(.gr_opts$journal)
  # the method line in the legend file as a sentence: its " · " separators
  # (the image's style) as semicolons, a full stop at its end (finding F11)
  meth_leg <- sub("([^.])$", "\\1.", gsub("\\s+\u00b7\\s+", "; ", meth))
  method <- c(orient, paste0(reg, ci_txt, "."), ci_note, inside_lines, skip_lines, region_txt, meth_leg, tests_txt,
              if (journal && length(test_lines)) test_lines,
              "Total SD: population (N) denominator, JCRS 2025 Part II.")
  foot <- if (journal) stat_lines else c(paste0(reg, ci_txt), ci_note, inside_lines, skip_lines, stat_lines, meth)
  terse <- length(present) > 5      # two narrow columns: the long form will not fit
  # the short header of a narrow combined cell (R2-2): each cohort's centroid,
  # total SD and n; the rest of its line goes to the legend file. The terse
  # form of six or more cohorts leaves out the same statistics (D4): they go
  # to the combined figure's legend through gr_moved in a narrow cell, else to
  # this plot's own legend lines (dropped_line below)
  short <- isTRUE(.gr_opts$short_hdr)
  pts_of <- function(r) if (is.finite(r$n_pat)) sprintf(", %d %s", as.integer(r$n_pat),
                                                        cluster_unit_word("patient", as.integer(r$n_pat))) else ""
  dropped_of <- function(g) {
    r <- srow(g); pts <- pts_of(r)
    paste0(g, ": centroid x ", fmt_fix(r$cx, 2), ", y ", fmt_fix(r$cy, 2),
           " · magnitude ", f2(r$mmean), " ± ", f2(r$msd), ", median ", f2(r$mmed),
           ", range ", f2(r$mmin), " to ", f2(r$mmax), ", RMS ", f2(r$rms), " D",
           if (nzchar(pts)) paste0(" · ", sub("^, ", "", pts)) else "")
  }
  dropped_line <- if (terse && !short)
    paste0("Statistics not printed in the panel (six or more cohorts): ",
           paste(vapply(present, dropped_of, character(1)), collapse = "; "), ".") else character(0)
  # the annotated style writes no legend file (D20), so there the terse form's
  # left-out statistics are printed under the header, one line per cohort
  if (!journal && terse)
    foot <- c(foot, "Statistics not printed in the cohort lines (six or more cohorts):",
              vapply(present, dropped_of, character(1)))
  hs <- hdr_groups(present, pal, function(g) {
    r <- srow(g)
    pts <- pts_of(r)
    if (short) gr_moved(dropped_of(g))
    if (terse) sprintf("%s, total SD %s, n = %d", astig_polar_text(r$cm, r$ca), f2(r$tsd), r$n)
    else if (short) sprintf("centroid %s · total SD %s D · n = %d", astig_polar_text(r$cm, r$ca), f2(r$tsd), r$n)
    # single-space separators: the header is wrapped at " · " and ", "
    else paste0("centroid ", astig_polar_text(r$cm, r$ca),
                sprintf(" (x %s; y %s)", fmt_fix(r$cx, 2), fmt_fix(r$cy, 2)),
                " · total SD ", f2(r$tsd), " D",
                " · magnitude ", f2(r$mmean), " ± ", f2(r$msd), ", median ", f2(r$mmed),
                ", range ", f2(r$mmin), " to ", f2(r$mmax), ", RMS ", f2(r$rms), " D",
                sprintf(" · n = %d eyes%s", r$n, pts))
  }, foot = foot, title = if (journal) NULL else title)
  # top margin sized from the header actually built (title + groups + footer
  # lines, as wrapped to the frame), so the bottom-anchored block never pushes
  # the title off the canvas; fig_w / fig_h are the size the plot is saved at
  out <- gr_polar_finish(p, hs, xlim = c(-rmax * 1.24, rmax * 1.24),
                         ylim = c(-rmax * 1.24, rmax * 1.24), fig_w = fig_w, fig_h = fig_h)
  method <- c(method, dropped_line)
  attr(out, "legend") <- list(title = title, lines = method[!is.na(method) & nzchar(method)])
  out
}

# ============================================================
# AstigMATIC-style semicircle, restyled: background in GREY shades so colour
# carries group identity only, and every group superimposed on one diagram.
# Single-angle (each vector drawn at its PLUS axis, decision D3) - kept for
# familiarity, for the Alpins vectors TIA, SIA and DV; the double-angle plot
# above is the one that satisfies the JCRS standard.
# ============================================================
g_astig_semicircle_multi <- function(mag, axisdeg, gl, glv, pal, title,
                                     unit_lab = "Diopters", arc_at = NULL,
                                     fig_w = SC_W, fig_h = SC_H) {
  m <- abs(gr_num(mag)); a <- gr_num(axisdeg) %% 180
  ok <- is.finite(m) & is.finite(a) & !is.na(gl) & gl %in% glv
  if (sum(ok) < 3) stop("too few eyes")
  d <- data.frame(x = m[ok] * cos(a[ok] * pi / 180), y = m[ok] * sin(a[ok] * pi / 180),
                  m = m[ok], a = a[ok],
                  grp = factor(as.character(gl[ok]), levels = glv))
  present <- glv[glv %in% levels(droplevels(d$grp))]

  # rings and their scale labels come from ONE vector (as on the double-angle
  # plot): the outer ring is the plot radius, >= every point and >= 1 so the
  # CI arc at 1 always has room
  rings <- pretty(c(0, max(d$m, 1, na.rm = TRUE)), 4); rings <- rings[rings > 0]
  rmax <- max(rings)
  th_s <- seq(0, pi, length.out = 181)
  wedge <- function(a1, a2, fill) {
    tt <- seq(a1 * pi / 180, a2 * pi / 180, length.out = 60)
    ggplot2::annotate("polygon", x = c(0, rmax * cos(tt)), y = c(0, rmax * sin(tt)),
                      fill = fill, colour = NA)
  }
  # alternating 30-degree slices; the middle pair (plus axes 60-120: the
  # with-the-rule zone) is a step darker so it still reads as a band, as it did
  # in the original diagram (white in the journal style: no shaded background,
  # decision D20)
  SEMI_FILL <- if (isTRUE(.gr_opts$journal)) rep("white", 6)
               else c("grey93", "white", "grey86", "grey86", "white", "grey93")
  circ <- do.call(rbind, lapply(rings, function(rr)
    data.frame(r = rr, x = rr * cos(th_s), y = rr * sin(th_s))))
  spokes <- do.call(rbind, lapply(seq(0, 180, by = 15), function(sp)
    data.frame(a = sp, x = c(0, rmax * cos(sp * pi / 180)), y = c(0, rmax * sin(sp * pi / 180)))))
  labd <- data.frame(a = seq(0, 180, by = 15))
  labd$x <- rmax * 1.08 * cos(labd$a * pi / 180); labd$y <- rmax * 1.08 * sin(labd$a * pi / 180)

  # greys only, so hue means "group"
  slices <- lapply(seq_along(SEMI_FILL), function(i)
    wedge((i - 1) * 30, i * 30, SEMI_FILL[i]))
  p <- Reduce(`+`, slices, ggplot2::ggplot()) +
    ggplot2::geom_path(data = circ, ggplot2::aes(x, y, group = r),
                       colour = "grey30", linewidth = .38) +
    ggplot2::geom_line(data = spokes, ggplot2::aes(x, y, group = a),
                       colour = "grey62", linewidth = .28) +
    ggplot2::geom_point(data = d, ggplot2::aes(x, y, colour = grp), size = 1.6, alpha = .55)

  cent <- list()
  for (g in present) {
    dg <- d[d$grp == g, , drop = FALSE]
    # vector mean in DOUBLE-angle space, displayed single-angle (as in the original)
    dx <- dg$m * cospi(dg$a / 90); dy <- dg$m * sinpi(dg$a / 90)
    cm <- sqrt(mean(dx)^2 + mean(dy)^2)
    ca <- (atan2(mean(dy), mean(dx)) * 180 / pi / 2) %% 180
    cent[[g]] <- data.frame(x = cm * cos(ca * pi / 180), y = cm * sin(ca * pi / 180),
                            grp = g, cm = cm, ca = ca, n = nrow(dg),
                            sdx = stats::sd(dx), sdy = stats::sd(dy),
                            am = mean(dg$m), asd = stats::sd(dg$m),
                            stringsAsFactors = FALSE)
  }
  cd <- do.call(rbind, cent)
  # the ring scale sits under the diameter, far enough below the 0° and 180°
  # spoke labels that they never touch at a narrow width (an offset of at least
  # 1.1 lines of its text, in data units of this figure's width)
  ring_y <- -rmax * max(0.055, 1.1 * gr_pt(8.5) / 72 * 2.32 / max(fig_w, 1))
  p <- p +
    # the origin (▼, explained in the legend file) under the centroids, which
    # it must never hide
    ggplot2::annotate("point", x = 0, y = 0, shape = 25, fill = "black",
                      colour = "black", size = 2.6 * max(0.7, gr_ts())) +
    ggplot2::geom_point(data = cd, ggplot2::aes(x, y, fill = grp), shape = 23,
                        size = 3.4 * max(0.7, gr_ts()), stroke = .7, colour = "grey10") +
    ggplot2::scale_colour_manual(values = pal[present], drop = FALSE) +
    ggplot2::scale_fill_manual(values = pal[present], drop = FALSE) +
    ggplot2::geom_text(data = labd, ggplot2::aes(x, y, label = paste0(a, "°")),
                       size = gr_mm(2.9), colour = "grey15", fontface = "bold") +
    ggplot2::annotate("text", x = c(-rings, rings), y = ring_y,
                      label = c(rings, rings), size = gr_mm(3.0), colour = "black",
                      fontface = "bold") +
    ggplot2::labs(title = NULL, x = unit_lab, y = NULL) +
    ggplot2::theme_void(base_size = gr_pt(12)) +
    ggplot2::theme(legend.position = "none",
                   axis.title.x = ggplot2::element_text(face = "bold", size = gr_pt(10.5)),
                   plot.background = ggplot2::element_rect(fill = "white",
                                                           colour = if (isTRUE(.gr_opts$journal)) NA else "grey25",
                                                           linewidth = .8))
  if (!is.null(arc_at) && arc_at <= rmax)
    p <- p + ggplot2::annotate("path", x = arc_at * cos(th_s), y = arc_at * sin(th_s),
                               colour = "grey25", linewidth = .3)
  terse <- length(present) > 5      # two narrow columns: the long form will not fit
  # the short header of a narrow combined cell (R2-2): the mean magnitude goes
  # to the legend file - the combined figure's (gr_moved), or this plot's own
  # for the terse form of six or more cohorts
  short <- isTRUE(.gr_opts$short_hdr)
  mag_of <- function(g) { r <- cd[cd$grp == g, ]; sprintf("%s: magnitude mean %.2f ± %s D", g, r$am, f2(r$asd)) }
  dropped_line <- if (terse && !short)
    paste0("Statistics not printed in the panel (six or more cohorts): ",
           paste(vapply(present, mag_of, character(1)), collapse = "; "), ".") else character(0)
  hs <- hdr_groups(present, pal, function(g) {
    r <- cd[cd$grp == g, ]
    if (short) gr_moved(mag_of(g))
    if (terse || short) sprintf("◆ %s, n = %d", astig_polar_text(r$cm, r$ca), r$n)
    else sprintf("◆ %s  ·  mean %.2f ± %s D  ·  n = %d",
                 astig_polar_text(r$cm, r$ca), r$am, f2(r$asd), r$n)
  }, foot = if (isTRUE(.gr_opts$journal)) NULL
            else c("◆ vector mean, computed in double-angle space; angles are plus axes",
                   # no legend file in the annotated style: the terse form's
                   # left-out magnitudes go under the header
                   if (terse) c("Statistics not printed in the cohort lines (six or more cohorts):",
                                vapply(present, mag_of, character(1)))),
     title = if (isTRUE(.gr_opts$journal)) NULL else title)
  # header wrapped to the frame and top margin sized from it (see g_double_angle_plot)
  out <- gr_polar_finish(p, hs, xlim = c(-rmax * 1.16, rmax * 1.16),
                         ylim = c(-rmax * 0.17, rmax * 1.12), fig_w = fig_w, fig_h = fig_h)
  # the title and the method note go to the legend file in the journal style
  attr(out, "legend") <- list(title = title, lines = c(
    "◆ vector mean, computed in double-angle space.",
    "Each vector is drawn at its plus axis (plus-cylinder format); the spokes are labeled with the axis.",
    dropped_line))
  out
}

# ============================================================
# Composite figure: panels lettered, captions BELOW (Reinstein)
# Pure grid, no extra packages.
# ============================================================
# ------------------------------------------------------------
# Derive the composite layout from the section tag of each panel that is
# actually present, so the section headers survive whatever combination of
# figures the user switched on. Previously the header block was built only
# when there were exactly 10 panels, so turning any panel off silently
# dropped every header and fell back to captions.
#
# Greedy packing, max `max_per_row` panels per row, never splitting a section
# unless it is itself longer than a row. For the classic ten-panel set this
# reproduces the historical c(3,3,4) and its three section bands exactly.
# ------------------------------------------------------------
build_sections <- function(tags, max_per_row = 4) {
  if (!length(tags) || any(!nzchar(tags)))
    return(list(rows_spec = NULL, sections = NULL))
  # A 4-panel set laid out 4-across gives each panel a quarter of the page, which
  # is too narrow for the two-column stat headers and the wrapped x-axis category
  # labels - they overflow the panel and get clipped. 2x2 is both legible and the
  # conventional way the Standard Four is shown.
  if (length(tags) == 4) max_per_row <- 2
  r <- rle(as.character(tags))
  runs <- list()
  for (i in seq_along(r$values)) {          # split a section too long for one row
    n <- r$lengths[i]
    while (n > 0) {
      k <- min(n, max_per_row)
      runs[[length(runs) + 1]] <- list(label = r$values[i], len = k)
      n <- n - k
    }
  }
  rows <- list(); cur <- list(); cur_n <- 0
  for (rr in runs) {
    if (cur_n > 0 && cur_n + rr$len > max_per_row) {
      rows[[length(rows) + 1]] <- cur; cur <- list(); cur_n <- 0
    }
    cur[[length(cur) + 1]] <- rr; cur_n <- cur_n + rr$len
  }
  if (cur_n > 0) rows[[length(rows) + 1]] <- cur
  list(
    rows_spec = vapply(rows, function(rw)
      sum(vapply(rw, function(x) x$len, numeric(1))), numeric(1)),
    sections = lapply(rows, function(rw)
      lapply(rw, function(x) list(label = x$label, span = x$len))))
}

# note: optional line printed in a band under the panels (e.g. the matched-
# cohort weighting statement). preview_dir: when set, a 100-dpi PNG copy is
# also written there (same name less the "StandardEyes_" prefix), whatever
# `formats` holds - the app's figure gallery shows these (browsers cannot
# display TIFF). gap_in: white space (inches) around every panel, so framed
# panels (double-angle, semicircle) are drawn apart instead of their frames
# touching; each panel keeps its pw x ph size and the page grows by the gaps.
# Every cell also reserves a PANEL_LETTER_IN strip above and left of its panel
# for the panel letter (A, B, ...), so the letter never lies on the panel or
# its frame. The page grows by the strips: no panel is narrower or shorter
# than without them, at any panel count.
PANEL_LETTER_IN <- 0.36
# The width (inches) save_graph() writes a single panel's own file at.
GR_PANEL_FILE_W <- 6.5

# Panels per row of a composite of n panels: rows_spec when it adds up to n,
# else the default layout. Every row spans the FULL page width, so the
# composite is always a filled rectangle (mEYEstro layout: 10 -> 3+3+4).
compose_rows <- function(n, ncol = 3, rows_spec = NULL) {
  if (is.null(rows_spec)) {
    rows_spec <- if (n == 10) c(3, 3, 4) else if (n == 9) c(3, 3, 3)
      else if (n == 8) c(3, 3, 2) else if (n == 7) c(3, 4)
      # 4 -> 2x2, not 3+1: a lone full-width panel on its own row reads badly and
      # a quarter-width panel clips its header. This is the Standard Four layout.
      else if (n == 4) c(2, 2)
      else {
        nr <- ceiling(n / ncol)
        v <- rep(ncol, nr); v[nr] <- n - ncol * (nr - 1); v
      }
  }
  if (sum(rows_spec) != n) {
    nr0 <- ceiling(n / ncol)
    rows_spec <- rep(ncol, nr0); rows_spec[nr0] <- n - ncol * (nr0 - 1)
  }
  rows_spec
}

# The width (inches) of each of the n panels in compose_panels(), in order.
compose_panel_w <- function(n, ncol = 3, pw = 5.6, rows_spec = NULL, gap_in = 0) {
  rs <- compose_rows(n, ncol, rows_spec)
  W <- ncol * (pw + gap_in) + max(rs) * PANEL_LETTER_IN
  rep(W / rs - gap_in - PANEL_LETTER_IN, rs)
}

# A composite laid out AT a chosen page width (figure_width_cm, decision D21):
# as many panels per row as keep each at least min_pw inches wide (one at
# least), every row full, the panels as wide as the page allows and their
# height scaled with the width (not below ph_min). Nothing is shrunk after
# drawing, so the text keeps its point size. Returns list(ncol, rows_spec, pw, ph).
GR_COMPOSITE_MIN_PW <- 3.1      # the bar and scatter panels
GR_POLAR_MIN_PW <- 5.0          # the double-angle and semicircle plots
GR_COMPOSITE_MIN_PH <- 4.9      # the lowest a bar or scatter panel is laid out (its header needs it)
gr_width_layout <- function(n, width_in, pw, ph, gap_in = 0, min_pw = GR_COMPOSITE_MIN_PW,
                            ph_min = 0) {
  k <- max(1L, min(n, floor((width_in + 1e-9) / (min_pw + gap_in + PANEL_LETTER_IN))))
  rs <- rep(k, ceiling(n / k)); rs[length(rs)] <- n - k * (length(rs) - 1L)
  pw2 <- (width_in - k * PANEL_LETTER_IN) / k - gap_in
  list(ncol = k, rows_spec = rs, pw = pw2, ph = max(ph_min, ph * pw2 / pw))
}

# The highest a composite laid out at a chosen width (figure_width_cm) may be:
# a journal page. A taller one is split into pages, each a complete figure of
# its own (<base>_p1, <base>_p2 ..., its own legend file), the panel letters
# continuing from page to page (A-F, G-K; decision R2-2).
GR_PAGE_MAX_H_CM <- 23
# The plotting area of a panel is never laid out lower than this (inches): a
# panel whose stat lines, axes and key take more of its height grows downward
# (its own file and its composite row) instead of squashing the bars. A panel
# built at a reduced text scale (R2-2) needs proportionally less.
GR_MIN_PANEL_IN <- 2.3
GR_MIN_PANEL_COMPACT_IN <- 1.1     # ... in a combined figure at a journal width (R2-2)

# The height (inches) panel `p` needs when drawn `w` inches wide: its fixed
# rows (header, margins, axis text and titles, key) plus `min_plot` inches for
# the plotting area; 0 when it cannot be measured.
gr_need_h <- function(p, w, min_plot = GR_MIN_PANEL_IN) {
  out <- tryCatch(gr_with_measure_device(w, 6, function() {
    gt <- if (inherits(p, "ggplot")) ggplot2::ggplotGrob(p) else p
    if (!inherits(gt, "gtable")) return(0)
    nn <- grid::unitType(gt$heights) != "null"
    fixed <- if (any(nn)) sum(grid::convertHeight(gt$heights[nn], "in", valueOnly = TRUE)) else 0
    # a plot nested under its own header row (gr_header_over): its fixed rows
    k <- which(gt$layout$name == "figure")
    if (length(k) && inherits(gt$grobs[[k[1]]], "gtable")) {
      ih <- gt$grobs[[k[1]]]$heights
      ni <- grid::unitType(ih) != "null"
      if (any(ni)) fixed <- fixed + sum(grid::convertHeight(ih[ni], "in", valueOnly = TRUE))
    }
    fixed + min_plot
  }), error = function(e) 0)
  if (length(out) == 1 && is.finite(out)) out else 0
}

# ------------------------------------------------------------
# Combined figures at a journal width (decision R2-2). With figure_width_cm
# set, a combined figure (the standard graphs, the double-angle and the
# semicircle sets) has two panels per row from 12 cm (17.77 and 12.69 cm) and
# one below (8.76 cm); a row is about 7.6 cm high at 17.77 cm - the same
# proportion of a panel's width at every width (GR_JOURNAL_ROW_RATIO) - not
# the free-size layout's 4.9-inch floor; a page is at most 23 cm high, so a
# six-panel set is one page at 17.77 cm and the ten- or eleven-panel set two
# (_p1, _p2), the panel letters continuing. The panels of such a figure are
# built again for their cells at a reduced text scale (gr_ts: every font size
# times the cell's share of the designed panel width, never below 6 pt); a
# panel whose statistics lines do not fit its row at that size keeps the
# first line per cohort and its test lines, and the rest goes to the legend
# file (gr_moved). A row higher than that grows to what its panels need.
# Returns list(ncol, pw, ph, row_in, ts, letter_in).
# ------------------------------------------------------------
GR_JOURNAL_ROW_RATIO <- 7.6 / (17.77 / 2)   # row height / cell width (7.6 cm at 17.77 cm, two per row)
GR_JOURNAL_LETTER_IN <- 0.2                 # the strip above a panel for its letter
GR_JOURNAL_LETTER_PT <- 10                  # the panel letters (bold)
GR_JOURNAL_DESIGN_W <- 5.6                  # the designed width (in) of a combined figure's panel
gr_journal_layout <- function(n, width_in, gap_in = 0) {
  wcm <- width_in * 2.54
  k <- if (wcm < 12) 1L else if (wcm <= 26) 2L else max(2L, as.integer(floor(width_in / 4.2)))
  k <- max(1L, min(k, n))
  cell <- width_in / max(k, if (wcm >= 12 && n == 1) 2L else 1L)
  row_in <- GR_JOURNAL_ROW_RATIO * cell
  pw <- cell - gap_in
  ph <- row_in - GR_JOURNAL_LETTER_IN - 0.06 - gap_in      # less the letter strip and the bottom band
  list(ncol = k, pw = pw, ph = ph, row_in = row_in, letter_in = GR_JOURNAL_LETTER_IN,
       ts = min(1, pw / GR_JOURNAL_DESIGN_W))
}

# A panel built for its cell of a combined figure at a journal width (R2-2):
# build() runs at the cell's size (.gr_opts$cell_w / cell_h = lay$pw / lay$ph)
# and text scale (lay$ts); when the panel needs more height than its row
# gives (need(p) > lay$ph) and short_ok, it is built again with its short
# statistics lines (.gr_opts$short_hdr), the lines left out recorded. The run
# log's counters are restored, so a panel built twice is counted once.
# Returns list(p (NULL when it could not be built), need, moved).
gr_build_cell <- function(build, lay, short_ok = TRUE, need = function(p) 0) {
  keep <- list(ts = .gr_opts$tscale, cw = .gr_opts$cell_w, ch = .gr_opts$cell_h,
               sh = .gr_opts$short_hdr, mv = .gr_opts$moved,
               fb = .gr_opts$n_gee_fallback, lt = .gr_opts$n_label_thin)
  on.exit({
    .gr_opts$tscale <- keep$ts; .gr_opts$cell_w <- keep$cw; .gr_opts$cell_h <- keep$ch
    .gr_opts$short_hdr <- keep$sh; .gr_opts$moved <- keep$mv
    .gr_opts$n_gee_fallback <- keep$fb; .gr_opts$n_label_thin <- keep$lt
  }, add = TRUE)
  .gr_opts$tscale <- lay$ts; .gr_opts$cell_w <- lay$pw; .gr_opts$cell_h <- lay$ph
  .gr_opts$short_hdr <- FALSE; .gr_opts$moved <- character(0)
  why <- ""
  p <- tryCatch(build(), error = function(e) { why <<- conditionMessage(e); NULL })
  if (is.null(p)) return(list(p = NULL, need = 0, moved = character(0), why = why))
  nd <- need(p); moved <- character(0)
  # value labels left out in the version kept (its legend line, F3)
  lt_now <- function() (.gr_opts$n_label_thin %||% 0L)
  lt1 <- lt_now(); thin <- lt1 > (keep$lt %||% 0L)
  if (isTRUE(short_ok) && nd > lay$ph + 0.02) {
    .gr_opts$short_hdr <- TRUE; .gr_opts$moved <- character(0)
    p2 <- tryCatch(build(), error = function(e) NULL)
    if (!is.null(p2)) { p <- p2; moved <- unique(.gr_opts$moved); nd <- need(p2); thin <- lt_now() > lt1 }
  }
  list(p = p, need = nd, moved = moved, thin = thin)
}

# The legend lines that list, under each panel's letter, the statistics lines
# that panel leaves out of its cell at a journal width (R2-2)
gr_moved_legend <- function(letters, moved) {
  out <- character(0)
  for (k in seq_along(letters)) {
    mv <- moved[[k]]
    if (length(mv)) out <- c(out, paste0(letters[k], ": statistics not printed in the panel at this width (",
                                        "too long for its row at 6 pt): ", paste(mv, collapse = "; "), "."))
  }
  out
}

# width_in: the page width the composite is laid out at (NA = its designed
# size, pw x ph panels); min_pw / ph_min: see gr_width_layout.
# need_h: optional, per panel the height it needs (gr_need_h): a row is laid
# out as high as its tallest need, never lower than ph.
# max_h_in: at a chosen width, a composite taller than this is written as pages
# (GR_PAGE_MAX_H_CM). The files written carry attr "pages": per page its base
# and the indices of its panels, for the caller's legend files.
# journal: a gr_journal_layout() list - a combined figure at a journal width
# (decision R2-2): ncol panels of pw x ph per row, every panel the same width
# (a last row of fewer panels is not stretched), a letter strip above each
# panel only (letters of GR_JOURNAL_LETTER_PT), pages <base>_p1, _p2 ... with
# the letters continuing; width_in is then not used.
# letter0: the panels before these (a later page: its first letter follows).
compose_panels <- function(plots, captions, base, formats, dpi, ncol = 3,
                           pw = 5.6, ph = 4.9, cap_h = 0.42, rows_spec = NULL,
                           sections = NULL, note = NULL, preview_dir = NULL,
                           logger = NULL, gap_in = 0, width_in = NA_real_,
                           min_pw = GR_COMPOSITE_MIN_PW, ph_min = 0, need_h = NULL,
                           max_h_in = if (is.finite(width_in)) GR_PAGE_MAX_H_CM / 2.54 else NA_real_,
                           row_ph = NULL, n_lab = NULL, journal = NULL, letter0 = 0L) {
  n <- length(plots); if (n == 0) return(character(0))
  if (!is.null(journal)) {
    ncol <- journal$ncol; pw <- journal$pw; ph <- journal$ph
    rows_spec <- rep(ncol, ceiling(n / ncol)); rows_spec[length(rows_spec)] <- n - ncol * (length(rows_spec) - 1L)
    if (!is.finite(max_h_in)) max_h_in <- GR_PAGE_MAX_H_CM / 2.54
  } else if (is.finite(width_in)) {
    lay <- gr_width_layout(n, width_in, pw, ph, max(as.numeric(gap_in %||% 0)[1], 0, na.rm = TRUE),
                           min_pw, ph_min)
    ncol <- lay$ncol; rows_spec <- lay$rows_spec; pw <- lay$pw; ph <- lay$ph
  }
  rows_spec <- compose_rows(n, ncol, rows_spec)
  nr <- length(rows_spec)
  hh_in  <- if (!is.null(sections)) 0.55 else 0            # section header band
  cap_in <- if (is.null(captions)) 0.06 else cap_h         # caption band
  gap_in <- max(as.numeric(gap_in %||% 0)[1], 0, na.rm = TRUE)
  lab_in <- if (is.null(journal)) PANEL_LETTER_IN else journal$letter_in   # panel-letter strips
  lab_w <- if (is.null(journal)) lab_in else 0             # ... beside a panel (none in the journal layout)
  fill_rows <- is.null(journal)                            # a short last row is stretched to the full width
  # each row as high as ph, or as its tallest panel needs (need_h)
  row_of <- rep(seq_len(nr), rows_spec)
  if (is.null(row_ph)) {
    nh <- if (length(need_h) == n) as.numeric(need_h) else rep(0, n)
    nh[!is.finite(nh)] <- 0
    row_ph <- vapply(seq_len(nr), function(r) max(ph, nh[row_of == r]), numeric(1))
  }
  band_r <- hh_in + lab_in + row_ph + gap_in + cap_in      # inches, per row
  note <- if (is.null(note)) character(0) else note[nzchar(note)]
  note_in <- if (length(note)) 0.30 + 0.22 * length(note) else 0   # note band
  # a row of k panels gives each a 1/k share of W: k letter strips are added
  # for the fullest row, so its panels keep the width they had without them
  # (n_lab: the letter strips of the whole composite, so each of its pages has
  # the page width of the composite)
  W <- ncol * (pw + gap_in) + (n_lab %||% max(rows_spec)) * lab_w; H <- sum(band_r) + note_in
  # Too high for a page: split into pages of whole rows (a row higher than a
  # page is a page of its own), each written by this function as a figure
  if (is.finite(max_h_in) && nr > 1 && H > max_h_in + 1e-6) {
    pages <- list(); cur <- integer(0); h <- note_in
    for (r in seq_len(nr)) {
      if (length(cur) && h + band_r[r] > max_h_in + 1e-6) { pages[[length(pages) + 1]] <- cur; cur <- integer(0); h <- note_in }
      cur <- c(cur, r); h <- h + band_r[r]
    }
    pages[[length(pages) + 1]] <- cur
    if (!is.null(logger))
      logger(sprintf("%s: %.1f cm high at this width - written as %d pages of at most %.0f cm (%s_p1 ... _p%d), each with its own legend file; the panel letters continue from page to page.",
                     basename(base), H * 2.54, length(pages), max_h_in * 2.54, basename(base), length(pages)))
    written <- character(0); pinfo <- list()
    for (pg in seq_along(pages)) {
      rr <- pages[[pg]]
      idx <- which(row_of %in% rr)
      b <- paste0(base, "_p", pg)
      got <- compose_panels(plots[idx], if (is.null(captions)) NULL else captions[idx], b, formats, dpi,
                            ncol = ncol, pw = pw, ph = ph, cap_h = cap_h, rows_spec = rows_spec[rr],
                            sections = if (is.null(sections)) NULL else sections[rr],
                            note = note, preview_dir = preview_dir, logger = logger, gap_in = gap_in,
                            width_in = NA_real_, max_h_in = NA_real_, row_ph = row_ph[rr],
                            n_lab = n_lab %||% max(rows_spec), journal = journal,
                            letter0 = letter0 + idx[1] - 1L)
      written <- c(written, got)
      pinfo[[pg]] <- list(base = b, idx = idx, ok = length(got) > 0)
    }
    attr(written, "pages") <- pinfo
    return(written)
  }
  Hr <- sum(band_r)                                         # the rows' area (inches)
  row_bot <- 1 - cumsum(band_r) / Hr                        # npc of that area
  written <- character(0)
  if (length(formats) == 0) formats <- "tiff"
  draw <- function() {
    grid::grid.newpage()
    # the panel rows fill the area above the note band; every position below
    # is a fraction of that area
    grid::pushViewport(grid::viewport(y = 1, height = (H - note_in) / H, just = "top"))
    i <- 0
    for (r in seq_len(nr)) {
      # (the journal layout: every panel a 1/ncol share, a short row not stretched)
      k <- if (fill_rows) rows_spec[r] else ncol
      # this row's band and its parts, as fractions of the rows' area
      hr <- band_r[r] / Hr
      fh <- hh_in / band_r[r]; fp <- (row_ph[r] + gap_in) / band_r[r]; fc <- cap_in / band_r[r]
      band_bot <- row_bot[r]; band_top <- band_bot + hr
      for (j in seq_len(rows_spec[r])) {
        i <- i + 1; if (i > n) break
        xc <- (j - 0.5) / k
        # the panel's slot less the gap and the letter strip on its left
        # (pw x row height when the row is full); the strip above it is in the band
        p_left <- grid::unit((j - 1) / k, "npc") + grid::unit(gap_in / 2 + lab_w, "in")
        p_top <- grid::unit(band_bot + hr * (fc + fp), "npc") - grid::unit(gap_in / 2, "in")
        vp_p <- grid::viewport(x = p_left, y = band_bot + hr * (fc + fp / 2), just = c("left", "centre"),
                               width = grid::unit(1 / k, "npc") - grid::unit(gap_in + lab_w, "in"),
                               height = grid::unit(hr * fp, "npc") - grid::unit(gap_in, "in"))
        if (inherits(plots[[i]], "gtable") || inherits(plots[[i]], "grob")) {
          grid::pushViewport(vp_p); grid::grid.draw(plots[[i]]); grid::popViewport()
        } else print(plots[[i]], vp = vp_p)
        # the letter above-left of the panel's top-left corner, in its strips;
        # the letters continue across the pages of a figure (letter0)
        lt <- LETTERS[letter0 + i]
        if (is.null(journal))
          grid::grid.text(lt, x = p_left - grid::unit(lab_in - 0.03, "in"),
                          y = p_top + grid::unit(0.03, "in"),
                          just = c("left", "bottom"), gp = grid::gpar(fontface = "bold", cex = 1.9))
        else
          grid::grid.text(lt, x = p_left + grid::unit(0.02, "in"), y = p_top + grid::unit(0.03, "in"),
                          just = c("left", "bottom"),
                          gp = grid::gpar(fontface = "bold", fontsize = GR_JOURNAL_LETTER_PT))
        if (!is.null(captions)) {
          vp_c <- grid::viewport(x = xc, y = band_bot + hr * fc / 2,
                                 width = 1 / k, height = hr * fc)
          grid::pushViewport(vp_c)
          grid::grid.text(captions[i], gp = grid::gpar(fontface = "bold",
                                                       cex = min(0.95, 2.6 / k)))
          grid::popViewport()
        }
      }
      # mEYEstro section super-headers + thick section frames
      if (!is.null(sections) && r <= length(sections)) {
        xs <- 0
        for (sec in sections[[r]]) {
          wsec <- sec$span / k
          grid::grid.rect(x = xs + wsec / 2, y = (band_bot + band_top) / 2,
                          width = wsec, height = hr,
                          gp = grid::gpar(fill = NA, col = "black", lwd = 2.6))
          grid::grid.text(sec$label, x = xs + 0.007,
                          y = band_top - hr * fh * 0.45,
                          just = c("left", "centre"),
                          gp = grid::gpar(fontface = "bold", cex = 2.1))
          xs <- xs + wsec
        }
      }
    }
    grid::popViewport()
    if (length(note)) {
      grid::pushViewport(grid::viewport(y = 0, height = note_in / H, just = "bottom"))
      grid::grid.text(note, x = grid::unit(0.006, "npc"),
                      y = grid::unit(1, "npc") - grid::unit(0.15 + 0.22 * (seq_along(note) - 1), "in"),
                      just = c("left", "top"),
                      gp = grid::gpar(fontface = "bold.italic", fontsize = 15))
      grid::popViewport()
    }
  }
  for (fm in unique(formats)) {
    fn <- paste0(base, ".", fm)
    if (gr_write_file(fn, function() gr_draw_to(fn, fm, W, H, dpi, draw), logger))
      written <- c(written, fn)
  }
  # low-resolution PNG copy for the app's figure gallery (not counted as output);
  # named without the "StandardEyes_" prefix so the path is no longer than the
  # figure's own despite the extra "_preview" folder level
  if (!is.null(preview_dir)) {
    pv <- file.path(preview_dir, paste0(sub("^StandardEyes_", "", basename(base)), ".png"))
    gr_write_file(pv, function() gr_draw_to(pv, "png", W, H, 100, draw), logger,
                  what = "Preview", count = FALSE)
  }
  written
}

# ------------------------------------------------------------
# File-name tag for one generate_standard_graphs call (e.g. "_Matched_<pairing>").
# Unicode letters are kept: only characters Windows forbids in file names,
# control characters and white space become "_". When two DIFFERENT tags reach
# the same output folder and reduce to the same file tag (compared
# case-insensitively, as NTFS does) - e.g. pairings that differ only in "<"/">"
# - the later one falls back to an index (_Matched_P2, ...) and the mapping is
# logged, so one pairing's figures can never overwrite another's.
# ------------------------------------------------------------
.gr_opts$file_tags <- new.env(parent = emptyenv())
gr_file_tag <- function(tag, out_dir, logger = message) {
  tag <- as.character(tag %||% "")[1]
  if (is.na(tag)) tag <- ""
  ft <- gsub('[<>:"/\\\\|?*[:cntrl:][:space:]]+', "_", tag)
  if (nchar(ft) > 60) ft <- substr(ft, 1, 60)          # keeps full paths short
  key <- normalizePath(out_dir, winslash = "/", mustWork = FALSE)
  reg <- .gr_opts$file_tags[[key]] %||% character(0)   # names: raw tags, values: file tags
  if (tag %in% names(reg)) return(unname(reg[names(reg) == tag][1]))
  if (tolower(ft) %in% tolower(reg)) {
    matched <- startsWith(tag, "_Matched_")
    stem <- if (matched) "_Matched_P" else "_Set"
    k <- 1L + (if (matched) sum(startsWith(names(reg), "_Matched_")) else length(reg))
    while (tolower(paste0(stem, k)) %in% tolower(reg)) k <- k + 1L
    clash <- names(reg)[tolower(reg) == tolower(ft)][1]
    new_ft <- paste0(stem, k)
    logger(sprintf(paste0("StandardEyes figures for '%s' are tagged '%s': the file tag '%s' ",
                          "is already used by '%s' in this folder."),
                   sub("^_", "", tag), sub("^_", "", new_ft), sub("^_", "", ft),
                   sub("^_", "", clash)))
    ft <- new_ft
  }
  assign(key, c(reg, stats::setNames(ft, tag)), envir = .gr_opts$file_tags)
  ft
}

# Right-to-left text in a PDF: cairo_pdf here has no text shaping (no Pango),
# so a Hebrew or Arabic label is drawn with its letters in reverse order, while
# the PNG / TIFF / JPEG devices show it correctly. When PDF is requested and a
# label holds right-to-left characters, the run log says so - once per run.
GR_RTL_RE <- "[\u0590-\u08FF\uFB1D-\uFDFF\uFE70-\uFEFC]"   # Hebrew, Arabic and the other RTL scripts
.gr_opts$rtl_noted <- FALSE
gr_rtl_pdf_note <- function(labels, formats, logger = message) {
  if (isTRUE(.gr_opts$rtl_noted) || !("pdf" %in% formats)) return(invisible(FALSE))
  lab <- as.character(unlist(labels))
  lab <- lab[!is.na(lab) & grepl(GR_RTL_RE, lab, perl = TRUE)]
  if (!length(lab)) return(invisible(FALSE))
  .gr_opts$rtl_noted <- TRUE
  logger(sprintf("WARNING: PDF figures cannot show right-to-left text: labels such as '%s' would be drawn with their letters reversed by this R build's PDF device. Choose PNG or TIFF for this run - they show right-to-left text correctly.",
                 lab[1]))
  invisible(TRUE)
}

# ============================================================
# Entry point
# ============================================================
generate_standard_graphs <- function(df, cfg, res, out_dir, tag = "", logger = message, subset_label = "") {
  # the figures need ggplot2 and gtable (grid ships with R); say which is missing
  miss_pk <- c("ggplot2", "gtable")[!vapply(c("ggplot2", "gtable"), requireNamespace,
                                            logical(1), quietly = TRUE)]
  if (length(miss_pk)) {
    logger(sprintf("Graphs skipped: R package(s) not installed: %s.", paste(miss_pk, collapse = ", ")))
    return(character(0))
  }
  r <- res$roles
  # a column named in the log is named by its header as typed in the file
  # (res$display_names, contract R4-3); the role holds the clean key
  col_name <- function(role) disp_name(r[[role]], res$display_names)
  # Visual acuity is ALWAYS used as DECIMAL acuity: the pipeline parses every VA
  # column in its declared notation (cfg$va_format) and keeps the canonical
  # decimal in the .pre_logmar__<col> stash, which is read first. Without a
  # stash (a direct call) a VA column is parsed here, so logMAR or Snellen
  # values are never read as if they were decimal.
  va_roles <- c("pre_ucva", "pre_bcva", "post_ucva", "post_bcva")
  va_fmt <- tolower(trimws(as.character(unlist(res$va_format %||% cfg$va_format %||% "decimal"))[1]))
  if (is.na(va_fmt) || !(va_fmt %in% c("decimal", "logmar", "snellen", "etdrs"))) va_fmt <- "decimal"
  pick <- function(role) {
    cc <- r[[role]]
    if (is.null(cc) || is.na(cc)) return(NULL)
    orig <- paste0(".pre_logmar__", cc)
    if (orig %in% names(df)) return(df[[orig]])
    if (!(cc %in% names(df))) return(NULL)
    if (role %in% va_roles && exists("parse_va", mode = "function"))
      return(parse_va(df[[cc]], va_fmt)$decimal)
    df[[cc]]
  }
  .gr_opts$label_angle <- 90; .gr_opts$box_left <- NULL; .gr_opts$box_right <- NULL
  # the test scope (cluster ids, matching weights) never outlives this call,
  # even when it stops with an error
  .gr_opts$cluster_id <- NULL; .gr_opts$weights <- NULL; .gr_opts$n_failed <- 0L
  .gr_opts$n_gee_fallback <- 0L; .gr_opts$n_label_thin <- 0L
  invisible(gee_refit_take())             # the figure pass's own independence refits (R3-4)
  on.exit({ .gr_opts$cluster_id <- NULL; .gr_opts$weights <- NULL; .gr_opts$journal <- FALSE
            .gr_opts$tables_classical <- FALSE }, add = TRUE)
  # figure style, width, formats and resolution of the run (D20, D21)
  fs <- gr_figure_settings(cfg, logger)
  .gr_opts$journal <- fs$journal
  formats <- fs$formats
  dpi <- fs$dpi
  width_in <- fs$width_in
  log_once(logger, paste0("StandardEyes figures: ", gr_figure_settings_text(fs), "."))
  if ("eps" %in% formats && !isTRUE(tryCatch(capabilities("cairo"), error = function(e) FALSE)))
    log_once(logger, "EPS figures are written with R's postscript device (no cairo): symbols such as ≤ and ± can be lost - check them, or use TIFF.")
  # the size of a single panel's own file: as designed, or laid out at the
  # chosen width (its height scaled with it)
  file_w <- if (is.finite(width_in)) width_in else GR_PANEL_FILE_W
  file_h <- if (is.finite(width_in)) max(4.0, 5.4 * width_in / GR_PANEL_FILE_W) else 5.4
  dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
  if (!dir.exists(out_dir))
    stop("the graphs folder could not be created: ", out_dir,
         " (check that the output folder exists and is writable)")
  written <- character(0)
  # low-resolution PNG copies of the composites for the app's figure gallery,
  # whatever the chosen formats (the folder is created on first use)
  prev_dir <- file.path(out_dir, "_preview")
  preview_dir <- function() {
    if (!dir.exists(prev_dir)) dir.create(prev_dir, showWarnings = FALSE)
    if (dir.exists(prev_dir)) prev_dir else NULL
  }
  # a family with a single figure has no composite: its preview is that figure
  # (named like the composite previews, without the "StandardEyes_" prefix)
  preview_single <- function(p, file_stem, w, h) {
    pd <- preview_dir()
    if (!is.null(pd))
      save_graph(p, file.path(pd, sub("^StandardEyes_", "", file_stem)), "png", 100, w = w, h = h,
                 logger = logger, what = "Preview", count = FALSE)
    invisible(NULL)
  }
  run_lab <- if (nzchar(tag)) paste0(" (", sub("^_", "", tag), ")") else ""
  # the same in a legend file's title, in words: "..., matched cohort:
  # Cohort B vs Reference" rather than the run tag
  leg_lab <- if (!nzchar(tag)) ""
             else if (startsWith(tag, "_Matched_")) paste0(", matched cohort: ", gsub("_", " ", sub("^_Matched_", "", tag)))
             else paste0(", ", gsub("_", " ", sub("^_", "", tag)))
  # a subset run names its subset in every legend title (finding F8): the PRK and
  # LASIK figure sets share their file names
  sl <- as.character(subset_label %||% "")[1]
  if (!is.na(sl) && nzchar(sl)) leg_lab <- paste0(leg_lab, ", subset: ", sl)

  # group display labels for the figures (cfg$group_labels: name -> label);
  # colors are looked up by the ORIGINAL group name, then re-keyed
  orig_glv <- levels(droplevels(factor(df$GroupLabel)))
  pal0 <- group_palette(orig_glv, cfg)
  lab_map <- vapply(orig_glv, function(g) {
    v <- (cfg$group_labels %||% list())[[g]]
    if (is.null(v) || !nzchar(as.character(v))) g else as.character(v)
  }, character(1))
  glv <- unname(lab_map)
  pal <- stats::setNames(unname(pal0), glv)
  gl <- unname(lab_map[as.character(df$GroupLabel)])
  tg <- gr_file_tag(tag, out_dir, logger)

  # a VA column without a single usable acuity (blank, or every cell off-chart /
  # unreadable) would draw 0% bars and NaN headers: its panels are left out
  va_pick <- function(role) {
    v <- pick(role)
    if (is.null(v)) return(NULL)
    vn <- gr_num(v)
    if (!any(is.finite(vn) & vn > 0)) {
      logger(sprintf("StandardEyes figures%s: VA column '%s' has no usable acuity values - the panels that need it are skipped.",
                     run_lab, col_name(role)))
      return(NULL)
    }
    v
  }
  pu <- va_pick("post_ucva"); pcv <- va_pick("pre_bcva"); pov <- va_pick("post_bcva")
  pseq <- pick("pre_seq"); poseq <- pick("post_seq")
  # intended postoperative SEQ per eye (optional target_seq role; absent = plano).
  # SEQ accuracy = postop SEQ - target; attempted = preop SEQ - target;
  # achieved = preop SEQ - postop SEQ. A blank target cell is read as plano.
  seq_tgt <- 0; has_tgt <- FALSE
  tgt_raw <- pick("target_seq")
  # the cataract set (four) has no attempted-vs-achieved panel: the log names
  # only the panels the target actually enters
  no_att <- identical(as.character(cfg$graph_set %||% "nine")[1], "four")
  if (!is.null(tgt_raw)) {
    seq_tgt <- gr_num(tgt_raw)
    n_blank <- sum(!is.finite(seq_tgt))
    seq_tgt[!is.finite(seq_tgt)] <- 0
    has_tgt <- TRUE
    if (!nzchar(tag))
      logger(sprintf("SEQ target: column '%s'%s - SEQ accuracy is postop SEQ minus the target%s.",
                     col_name("target_seq"),
                     if (n_blank) sprintf(" (%d eye(s) with a blank target taken as plano)", n_blank) else "",
                     if (no_att) "" else ", attempted = preop SEQ minus the target"))
  } else if (!nzchar(tag) && (!is.null(poseq) || !is.null(pseq))) {
    logger(sprintf("SEQ target: none mapped - SEQ accuracy%s assume%s every eye was aimed at plano.",
                   if (no_att) "" else " and attempted-vs-achieved", if (no_att) "s" else ""))
  }
  pcyl <- pick("pre_cyl"); pax <- pick("pre_axis")
  ocyl <- pick("post_cyl"); oax <- pick("post_axis")
  # ---- astigmatism vectors for the figures ----
  # Honours supplied vector columns and a mapped target refraction; anything
  # not mapped is computed from sphere/cylinder/axis with target zero.
  cav <- NULL
  if (!is.null(pcyl) && !is.null(pax) && !is.null(ocyl) && !is.null(oax))
    cav <- resolve_astig_vectors(df, pcyl, pax, ocyl, oax,
                                 vec_cols = res$astig_vector_cols %||% list(),
                                 target   = res$astig_target %||% list(),
                                 logger   = logger,
                                 display_names = res$display_names)

  # ---- stability over time (optional; needs mapped follow-up visits) ----
  stab_visits <- res$stability_visits %||% list()
  stab_on     <- isTRUE(cfg$stability_figs) && length(stab_visits) > 0
  stab_preop  <- isTRUE(cfg$stability_include_preop %||% TRUE)
  stab_from   <- as.character(cfg$stability_change_from %||% "")
  stab_to     <- as.character(cfg$stability_change_to %||% "")
  stab_thresh <- as.numeric(cfg$stability_change_threshold %||% 0.50)
  if (isTRUE(cfg$stability_figs) && length(stab_visits) == 0)
    logger("Stability figure requested but no usable follow-up visits are mapped - skipped.")
  # preop SEQ for the leading point: the pre_seq role, else pre sphere + cyl/2
  stab_pre <- NULL
  if (stab_preop) {
    stab_pre <- pseq
    if (is.null(stab_pre) && !is.null(pcyl)) {
      psph <- pick("pre_sph")
      if (!is.null(psph)) stab_pre <- gr_num(psph) + gr_num(pcyl) / 2
    }
  }
  # report each visit's n / median time / SEQ provenance ONCE per run, using the
  # same resolution the figures use. Doing it here rather than inside each
  # g_stability call keeps the record independent of which figure families are
  # enabled, and stops it repeating per group; logging it on the run's first
  # call only (the unmatched cohort, tag "" - pipeline.R makes that call first
  # and once per run) stops it repeating per matched pairing.
  if (stab_on && !nzchar(tag))
    invisible(stability_long(df, stab_visits, gl, glv, stab_pre, logger))
  # user text drawn on the figures: group labels and visit labels
  gr_rtl_pdf_note(c(glv, if (stab_on) vapply(stab_visits, function(v)
                      as.character(v$label %||% "")[1], character(1))),
                  formats, logger)

  # Panel set selector, mirroring the published Reinstein standard sets:
  #   nine - everything
  #   six  - drops stability, DEQ and the three vector panels
  #   four - cataract; additionally drops Safety and attempted-vs-achieved
  set_flags <- function(key) {
    v <- as.character(cfg[[key]] %||% "nine")
    if (!v %in% c("nine", "six", "four")) v <- "nine"
    list(four = identical(v, "four"), six = identical(v, "six"), name = v)
  }
  cset <- set_flags("graph_set")
  # astigmatism PANELS (bar + vector panels); off = a purely refractive set
  c_astig <- isTRUE(cfg$astig_panels %||% TRUE)

  # ============================================================
  # The StandardEyes figures: all groups superimposed on one chart, any group
  # count, the published Reinstein/JRS metrics on every panel, and the stat
  # line as omnibus + each group against the reference (see p_group_text).
  # ============================================================
  # Cluster the figure p-values by patient (GEE, robust SE) so both eyes of
  # one person never count as independent evidence.
  cluster_by_pt <- isTRUE(cfg$cluster_by_patient %||% TRUE)
  pid_col <- res$patient_id_clean
  if (cluster_by_pt && !is.null(pid_col) && !is.na(pid_col) && pid_col %in% names(df) &&
      requireNamespace("geepack", quietly = TRUE)) {
    # every eye in the bars must also be in the clustered test and the patient
    # bootstrap: an eye without a patient id becomes its own cluster
    # (fill_missing_ids, helpers.R - the tokens the tables use)
    cid <- trimws(id_text(df[[pid_col]]))
    n_noid <- sum(is.na(cid) | !nzchar(cid))
    .gr_opts$cluster_id <- fill_missing_ids(df[[pid_col]])
    if (n_noid > 0)
      logger(sprintf("StandardEyes figures%s: %d of %d eyes have no patient ID - each is treated as its own patient in the clustered p-values.",
                     if (nzchar(tag)) paste0(" (", sub("^_", "", tag), ")") else "", n_noid, length(cid)))
    logger(paste0("StandardEyes figures: p-values clustered by patient (GEE; exchangeable working correlation, ",
                  "independence on a matched cohort or where a test is labeled so; ", gee_inference_text(),
                  ") wherever a patient has two analyzed eyes - both eyes of a patient count once; ",
                  "with one eye per patient the classical tests run, as in the tables."))
  } else {
    .gr_opts$cluster_id <- NULL
    if (cluster_by_pt)
      logger("StandardEyes figures: eyes treated as independent - set the patient/cluster ID column to cluster the p-values.")
  }
  # the summary tables run the classical tests although the figures cluster
  # (table_tests = "classical", the v1.0.0 tables): the legend files say that
  # a table and its figure can then give different P values (finding S3)
  .gr_opts$tables_classical <- !is.null(.gr_opts$cluster_id) && exists("table_tests_of", mode = "function") &&
    identical(table_tests_of(cfg), "classical")
  # Matched pass: MatchIt's matching weights. When any differs from 1 (k:1
  # matching, replacement, a caliper on a limited pool) every figure P value is
  # a weighted GEE Wald test, as in the matched summary tables; bars, means and
  # N boxes show the matched eyes unweighted, and the composite says so. A
  # column that merely happens to be called "weights" in an unmatched cohort is
  # never read as matching weights.
  w_note <- NULL
  is_matched <- isTRUE(startsWith(as.character(tag)[1], "_Matched_")) || "Pairing" %in% names(df)
  # a matched pass's clustered GEE: independence working correlation (D18)
  .gr_opts$matched <- is_matched
  if (is_matched && "weights" %in% names(df)) {
    mw <- suppressWarnings(as.numeric(df$weights))
    if (any(is.finite(mw) & abs(mw - 1) > 1e-8)) {
      .gr_opts$weights <- mw
      w_note <- "Matched cohort: P values use the matching weights; bars and means show the matched eyes unweighted."
      logger(sprintf("StandardEyes figures%s: matching weights are non-unit (%s to %s) - figure P values are weighted GEE Wald tests; bars, means and N show the matched eyes unweighted.",
                     run_lab, format(signif(min(mw, na.rm = TRUE), 3)),
                     format(signif(max(mw, na.rm = TRUE), 3))))
    }
  }
  # every panel builds its own "N = x eyes" boxes from the eyes it plots (the
  # denominators of its percentages), not from the group sizes
  cp <- list(); cc_ <- character(0); cs <- character(0); cn <- character(0)
  # The panels are planned first: a pass over draw_panels() with `planning` on
  # only records each panel's section (no builder runs), so each panel is then
  # built knowing the narrowest width it is drawn at - its composite cell or
  # its own file - which decides whether its caution line fits (gr_bars).
  plan <- character(0); cell_w <- numeric(0); cell_h <- 4.9; planning <- TRUE
  fail_why <- character(0)       # why a panel could not be drawn, by panel name
  ptxt <- list()                 # the text each panel draws (journal legend files)
  cneed <- numeric(0)            # the height each panel needs in its composite cell (gr_need_h)
  # at a journal width (R2-2): each panel's version built for its cell of the
  # combined figure, and the statistics lines it left out of that cell
  jl <- NULL; ccell <- list(); cmoved <- list()
  cthin <- list()                # panels whose combined-figure version left value labels out
  addC <- function(nm, cap, expr, section = "") {
    if (planning) { plan[[nm]] <<- section; return(invisible(NULL)) }
    ex <- substitute(expr); env <- parent.frame()
    .gr_opts$cell_w <- if (nm %in% names(cell_w)) cell_w[[nm]] else GR_BAR_MIN_W
    .gr_opts$cell_h <- cell_h
    on.exit({ .gr_opts$cell_w <- NULL; .gr_opts$cell_h <- NULL }, add = TRUE)
    lt0 <- .gr_opts$n_label_thin %||% 0L
    p <- tryCatch(eval(ex, env), error = function(e) {
      fail_why[[nm]] <<- conditionMessage(e)
      logger(paste0("StandardEyes figure '", nm, "' failed", run_lab, ": ", conditionMessage(e))); NULL })
    # this panel left value labels out (gr_bars): its legend says so (F3)
    thin_own <- (.gr_opts$n_label_thin %||% 0L) > lt0
    if (!is.null(p)) {
      if (inherits(p, "ggplot")) p <- p + ggplot2::labs(title = NULL)
      # the version for the combined figure's cell at a journal width: built
      # again at the cell's size and text scale; its short statistics lines
      # when the full ones do not fit the row (journal style only - the
      # annotated style writes no legend file, so its rows grow instead)
      if (!is.null(jl)) {
        cb <- gr_build_cell(function() {
          q <- eval(ex, env)
          if (inherits(q, "ggplot")) q + ggplot2::labs(title = NULL) else q
        }, jl, short_ok = fs$journal, need = function(q) gr_need_h(q, jl$pw, GR_MIN_PANEL_COMPACT_IN))
        if (!is.null(cb$p)) {
          ccell[[nm]] <<- cb$p; cmoved[[nm]] <<- cb$moved; cneed[[nm]] <<- cb$need; cthin[[nm]] <<- isTRUE(cb$thin)
        } else logger(sprintf("StandardEyes figure '%s'%s: its cell of the combined figure could not be built at the journal width (%s) - the combined figure shows its own file's layout.",
                              nm, run_lab, cb$why %||% "unknown error"))
      }
      base <- file.path(out_dir, paste0("StandardEyes_", nm, tg))
      # its own file grows downward when its stat lines, axes and key leave the
      # bars less than GR_MIN_PANEL_IN (never squashed); its composite row too
      fh_p <- max(file_h, gr_need_h(p, file_w))
      if (is.null(jl)) cneed[[nm]] <<- gr_need_h(p, .gr_opts$cell_w)
      got <- save_graph(p, base, formats, dpi, w = file_w, h = fh_p, logger = logger)
      written <<- c(written, got)
      # journal style: the panel title and its abbreviations in <figure>_legend.txt
      if (fs$journal) {
        tx <- gr_figure_texts(p)
        # the stability panel's points are group means, not eyes: not "●"
        if (identical(nm, "07_Stability")) tx <- setdiff(tx, "●")
        ptxt[[nm]] <<- tx
        if (is.null(jl)) cthin[[nm]] <<- thin_own
        if (length(got)) gr_write_legend(base, paste0(cap, leg_lab), c(gr_panel_notes(nm, tx),
                                                                      if (thin_own) GR_THIN_NOTE, gr_gee_legend_lines(tx)),
                                         texts = tx, logger = logger)
      }
      cp[[length(cp) + 1]] <<- p; cc_ <<- c(cc_, cap); cs <<- c(cs, section); cn <<- c(cn, nm)
    }
  }
  draw_panels <- function() {
    # One reference CDVA for the cumulative-VA panel's second series and for the
    # efficacy bins AND index - preoperative for the corneal sets (nine, six;
    # Waring/Reinstein 2011 standard graphs 1-2), postoperative for the cataract
    # set (four; Reinstein 2017)
    cdva_ref <- if (cset$four) pov else pcv
    cdva_lab <- if (cset$four) "Postop CDVA" else "Preop CDVA"
    if (!is.null(pu) && !is.null(cdva_ref))
      addC("01_Vision", "Uncorrected Distance Visual Acuity",
           g_cumulative_va(pu, cdva_ref, gl, glv, pal, second_lab = cdva_lab), "Efficacy")
    if (!is.null(pu) && !is.null(cdva_ref))
      addC("02_Efficacy", "Uncorrected vs Corrected Distance Visual Acuity",
           g_udva_vs_cdva(pu, cdva_ref, gl, glv, pal, pu, cdva_ref, cdva_lab = cdva_lab),
           "Efficacy")
    if (!cset$four && !is.null(pcv) && !is.null(pov))
      addC("03_Safety", "Change in Corrected Distance Visual Acuity",
           g_cdva_change(pcv, pov, gl, glv, pal, pov, pcv), "Safety")
    if (!is.null(poseq))
      addC("04_SEQ", "Spherical Equivalent Refraction Accuracy",
           g_seq_accuracy(gr_num(poseq) - seq_tgt, gl, glv, pal, has_target = has_tgt),
           "SEQ Accuracy")
    if (!cset$four && !is.null(pseq) && !is.null(poseq))
      addC("05_SEQregress", "Spherical Equivalent Attempted vs Achieved",
           g_attempted_achieved(pseq, poseq, gl, glv, pal, tgt = seq_tgt), "SEQ Accuracy")
    # DEQ is never traded for stability: both are kept, so switching stability
    # on never costs the reader a panel. Files sort in composite order.
    if (!cset$six && !cset$four && !is.null(poseq) && !is.null(ocyl)) {
      if (!planning && !nzchar(tag))
        logger(if (has_tgt)
          sprintf("Defocus equivalent: |postop SEQ - target| + |postop cylinder| / 2, target from column '%s'.",
                  col_name("target_seq"))
        else "Defocus equivalent: |postop SEQ| + |postop cylinder| / 2 (no SEQ target mapped: plano).")
      addC("06_DEQ", "Defocus Equivalent Accuracy",
           g_deq(poseq, ocyl, gl, glv, pal, tgt = seq_tgt, has_target = has_tgt), "DEQ Accuracy")
    }
    if (stab_on && !cset$six && !cset$four)
      addC("07_Stability", "Spherical Equivalent Stability",
           g_stability(df, stab_visits, gl, glv, pal, pre_seq = stab_pre,
                       change_from = stab_from, change_to = stab_to,
                       thresh = stab_thresh, style = "meyestro", logger = NULL),
           "Stability")
    if (c_astig && !is.null(pcyl) && !is.null(ocyl))
      addC("08_Cylinder", "Refractive Astigmatism",
           g_astigmatism(pcyl, ocyl, gl, glv, pal), "Astigmatism Accuracy")
    if (c_astig && !cset$six && !cset$four && !is.null(cav)) {
      addC("09_CYLregress", "Target Induced vs Surgically Induced Astigmatism",
           g_tia_sia(cav, gl, glv, pal), "Astigmatism Accuracy")
      addC("10_CIhist", "Correction Index", g_ci_hist(cav, gl, glv, pal),
           "Astigmatism Accuracy")
      addC("11_AEhist", "Refractive Astigmatism Angle of Error",
           g_angle_of_error(cav, gl, glv, pal), "Astigmatism Accuracy")
    }
  }
  draw_panels()
  # each panel's width in the composite (laid out as below, from the planned
  # sections) or in its own file, whichever is narrower. A panel that fails to
  # draw (logged) re-packs the composite rows the others were planned for.
  # At a chosen width the composite is laid out at that width (gr_width_layout).
  if (length(plan) >= 2 && is.finite(width_in)) {
    # a journal width (R2-2): each panel's own file is laid out at the full
    # width, and the combined figure's cells are built apart (addC, jl)
    jl <- gr_journal_layout(length(plan), width_in)
    comp_w <- rep(file_w, length(plan))
    cell_h <- file_h
  } else {
    comp_w <- if (length(plan) >= 2)
      compose_panel_w(length(plan), rows_spec = build_sections(unname(plan))$rows_spec)
      else rep(file_w, length(plan))
    cell_h <- min(file_h, 4.9)
  }
  cell_w <- stats::setNames(pmin(file_w, comp_w), names(plan))
  planning <- FALSE
  draw_panels()
  if (length(cp) >= 2) {
    lay <- build_sections(cs)
    base_c <- file.path(out_dir, paste0("StandardEyes_AllGroups", tg))
    # at a journal width the cells' own versions (R2-2), in panel order (a
    # panel whose cell version could not be built keeps its file's)
    if (!is.null(jl)) {
      jl <- gr_journal_layout(length(cp), width_in)
      cpc <- lapply(seq_along(cp), function(i) ccell[[cn[i]]] %||% cp[[i]])
    } else cpc <- cp
    # journal style (D20): no section bands, frames or captions in the image -
    # the panel titles and the matched-cohort note go to the legend file; at a
    # journal width the two-per-row layout has no section bands either
    got <- compose_panels(cpc,
      captions = if (fs$journal || !is.null(lay$sections) || !is.null(jl)) NULL else cc_,
      base = base_c, formats = formats, dpi = dpi,
      rows_spec = lay$rows_spec, sections = if (fs$journal || !is.null(jl)) NULL else lay$sections,
      note = if (fs$journal) NULL else w_note, preview_dir = preview_dir(), logger = logger,
      width_in = width_in, ph_min = GR_COMPOSITE_MIN_PH,
      need_h = unname(cneed[cn]), journal = jl)
    written <- c(written, got)
    # one legend file per figure: the composite, or each of its pages; the
    # letters continue from page to page (R2-2)
    pages <- attr(got, "pages") %||% list(list(base = base_c, idx = seq_along(cp), ok = length(got) > 0))
    if (fs$journal) for (pg in pages) if (isTRUE(pg$ok)) {
      ii <- pg$idx
      gr_write_legend(pg$base, paste0("Standard graphs", leg_lab,
                                      if (length(pages) > 1) sprintf(" (page %d of %d)", match(pg$base, vapply(pages, `[[`, "", "base")), length(pages)) else ""),
                      c(paste0(paste0(LETTERS[ii], ", ", cc_[ii], collapse = "; "), "."), w_note,
                        unlist(lapply(seq_along(ii), function(k) {
                          nt <- c(gr_panel_notes(cn[ii[k]], ptxt[[cn[ii[k]]]]),
                                  if (isTRUE(cthin[[cn[ii[k]]]])) GR_THIN_NOTE)
                          if (length(nt)) paste0(LETTERS[ii[k]], ": ", nt) })),
                        gr_moved_legend(LETTERS[ii], lapply(cn[ii], function(x) cmoved[[x]])),
                        gr_gee_legend_lines(ptxt[cn[ii]])),
                      texts = unlist(ptxt[cn[ii]]), logger = logger)
    }
  } else if (length(cp) == 1) {
    preview_single(cp[[1]], paste0("StandardEyes_", cn[1], tg), file_w, file_h)
    logger(sprintf("StandardEyes figures%s: only one panel could be drawn (%s) - no composite figure, which needs two or more.",
                   run_lab, cn[1]))
  }

  # ---- astigmatism: JCRS double-angle + the restyled grey semicircle ----
  # Gated only by their own switches: "astig_panels" governs the composite's
  # astigmatism panels, not these standalone figures.
  da_on <- isTRUE(cfg$double_angle_figs %||% TRUE)
  sc_on <- isTRUE(cfg$semicircle_figs %||% TRUE)
  if (!is.null(cav) && (da_on || sc_on)) {
    ast <- astig_settings(cfg)
    ell <- ast$region
    jcrs_astig <- ast$jcrs
    astig_meth <- ast$method
    # The double-angle set is the standard's (D3): PRE, SIA, PE - and POST when
    # a target is not plano; the correction index, a ratio without a meridian,
    # is drawn only as its histogram. The semicircle set keeps the Alpins
    # vectors TIA, SIA and DV, at their plus axes.
    da_vecs <- astig_jcrs_vectors(cav)
    sc_vecs <- list(
      list(key = "TIA", title = "Target induced astigmatism (Alpins; target - preoperative)",
           mag = cav$tia, ax = cav$tax),
      list(key = "SIA", title = "Surgically induced astigmatism (postoperative - preoperative)",
           mag = cav$sia, ax = cav$sax),
      list(key = "DV", title = "Difference vector (Alpins; target - postoperative = -PE)",
           mag = cav$dv, ax = cav$dax))
    # the statistics of each double-angle panel, computed with the cohorts'
    # own names so the astigmatism sheet's (export.R) are reused from the cache
    gl_orig <- as.character(df$GroupLabel)
    dap <- list(); dac <- character(0); scp <- list(); scc <- character(0)
    # their own size, or laid out at the chosen width (aspect kept)
    da_w <- if (is.finite(width_in)) width_in else DA_W
    da_h <- DA_H * da_w / DA_W
    sc_w <- if (is.finite(width_in)) width_in else SC_W
    sc_h <- SC_H * sc_w / SC_W
    # journal style: the plot's title and method text in <figure>_legend.txt
    polar_legend <- function(p, base) {
      if (!fs$journal) return(invisible(NULL))
      lg <- attr(p, "legend")
      tx <- gr_figure_texts(p)
      gr_write_legend(base, paste0(lg$title %||% basename(base), leg_lab),
                      c(lg$lines, gr_gee_legend_lines(tx, polar = TRUE)),
                      texts = c(tx, "◆"), logger = logger)
    }
    da_leg <- list(); sc_leg <- list()
    # each plot is built by a function of its size (w x h inches), kept so a
    # composite laid out at a chosen width can build its panels again at the
    # width of its cells (polar_cells); the height a plot was laid out at is
    # its attr "fig_h" (gr_polar_finish: taller than asked when a header that
    # wraps to many lines would otherwise squeeze the diagram)
    da_mk <- list(); sc_mk <- list()
    fig_h_of <- function(p, h) { v <- suppressWarnings(as.numeric(attr(p, "fig_h"))[1])
      if (length(v) && is.finite(v)) max(v, h) else h }
    # the patients counted in each header: the mapped patient ID, whether or
    # not the tests cluster (D4)
    cnt_id <- astig_count_ids(df, res)
    if (da_on) for (v in da_vecs) {
      st <- tryCatch(astig_vector_stats(v$mag, v$ax, gl_orig, orig_glv, id = .gr_opts$cluster_id,
                                        method = astig_meth, region = ell,
                                        centroid_region = ast$centroid_region,
                                        matched = isTRUE(.gr_opts$matched), w = .gr_opts$weights,
                                        key = v$key, logger = logger, count_id = cnt_id,
                                        what = paste0(v$key, if (nzchar(tag)) paste0(" (", sub("^_", "", tag), ")") else "")),
                     error = function(e) { logger(paste0("Astigmatism statistics '", v$key, "' failed", run_lab,
                                                         ": ", conditionMessage(e))); NULL })
      if (is.null(st)) next
      mk <- local({ v <- v; st <- st; function(w, h) g_double_angle_plot(v$mag, v$ax, gl, glv, pal,
                      paste0(v$title, " - double-angle"), unit_lab = "Diopters",
                      ellipse = ell, jcrs = jcrs_astig,
                      test_method = astig_meth, fig_w = w, fig_h = h,
                      st = st, centroid_region = ast$centroid_region, lab = lab_map) })
      p <- tryCatch(mk(da_w, da_h),
                    error = function(e) { logger(paste0("StandardEyes double-angle '",
                      v$key, "' failed", run_lab, ": ", conditionMessage(e))); NULL })
      if (!is.null(p)) {
        base <- file.path(out_dir, paste0("StandardEyes_DoubleAngle_", v$key, tg))
        got <- save_graph(p, base, formats, dpi, w = da_w, h = fig_h_of(p, da_h), logger = logger)
        written <- c(written, got)
        if (length(got)) polar_legend(p, base)
        dap[[length(dap) + 1]] <- p; dac <- c(dac, v$key); da_leg[[v$key]] <- attr(p, "legend")
        da_mk[[length(da_mk) + 1]] <- mk
      }
    }
    # the reference tool's input files for the vectors plotted (decision R3-3)
    if (length(dac) && isTRUE(cfg$astig_whwk_files %||% TRUE))
      tryCatch(write_whwk_inputs(Filter(function(v) v$key %in% dac, da_vecs), gl, glv, out_dir, tg, logger,
                                 clustered = !is.null(.gr_opts$cluster_id) && !identical(astig_meth, "classic")),
               error = function(e) logger(paste0("WARNING: WHWK input files NOT written", run_lab, ": ",
                                                 conditionMessage(e))))
    if (sc_on) for (v in sc_vecs) {
      mk <- local({ v <- v; function(w, h) g_astig_semicircle_multi(v$mag, v$ax, gl, glv, pal,
                      paste0(v$title, " - semicircle"), unit_lab = "Diopters",
                      fig_w = w, fig_h = h) })
      p <- tryCatch(mk(sc_w, sc_h),
                    error = function(e) { logger(paste0("StandardEyes semicircle '",
                      v$key, "' failed", run_lab, ": ", conditionMessage(e))); NULL })
      if (!is.null(p)) {
        base <- file.path(out_dir, paste0("StandardEyes_Semicircle_", v$key, tg))
        got <- save_graph(p, base, formats, dpi, w = sc_w, h = fig_h_of(p, sc_h), logger = logger)
        written <- c(written, got)
        if (length(got)) polar_legend(p, base)
        scp[[length(scp) + 1]] <- p; scc <- c(scc, v$key); sc_leg[[v$key]] <- attr(p, "legend")
        sc_mk[[length(sc_mk) + 1]] <- mk
      }
    }
    # The panels of a polar composite and the height each needs. At a chosen
    # width a cell is narrower than the figure's own file (panel-letter strip,
    # gap), so each panel is built again at its cell's size - its header wrapped
    # to that width - rather than drawn squeezed; at the designed size the
    # file's plot is the cell's. A panel that cannot be rebuilt keeps its file's.
    # At a journal width (R2-2) the combined figure follows the standard
    # graphs' layout - two panels per row at 17.77 and 12.69 cm, one at 8.76
    # cm, rows of the same proportion, pages of at most 23 cm - and each panel
    # is built again for its cell at the reduced text scale, with its short
    # statistics lines when the full ones do not fit the row (journal style).
    polar_cells <- function(plots, mks) {
      lay <- NULL; moved <- rep(list(character(0)), length(plots))
      if (is.finite(width_in)) {
        lay <- gr_journal_layout(length(plots), width_in, POLAR_GAP_IN)
        plots <- lapply(seq_along(plots), function(i) {
          cb <- gr_build_cell(function() mks[[i]](lay$pw, lay$ph), lay, short_ok = fs$journal,
                              need = function(q) fig_h_of(q, 0))
          if (is.null(cb$p)) {
            logger(sprintf("StandardEyes polar plot %d%s: its cell of the combined figure could not be built at the journal width (%s) - the combined figure shows its own file's layout.",
                           i, run_lab, cb$why %||% "unknown error"))
            return(plots[[i]])
          }
          moved[[i]] <<- cb$moved
          cb$p
        })
      }
      list(plots = plots, need_h = vapply(plots, function(p) fig_h_of(p, 0), numeric(1)),
           lay = lay, moved = moved)
    }
    # a composite's legend: its panels by letter, then the method text of each;
    # a composite written as pages (compose_panels attr "pages") gets one per
    # page, the letters continuing from page to page (R2-2)
    composite_legend <- function(plots, legs, base, what, got = NULL, moved = NULL) {
      if (!fs$journal) return(invisible(NULL))
      pages <- attr(got, "pages") %||% list(list(base = base, idx = seq_along(plots), ok = TRUE))
      for (pg in seq_along(pages)) {
        if (!isTRUE(pages[[pg]]$ok)) next
        ii <- pages[[pg]]$idx
        ttl <- vapply(legs[ii], function(l) as.character(l$title %||% "")[1], character(1))
        # a method line every panel shares is written once, the others under
        # their panel's letter
        common <- if (length(ii) > 1) Reduce(intersect, lapply(legs[ii], function(l) as.character(l$lines)))
                  else character(0)
        lines <- c(common, unlist(lapply(seq_along(ii), function(k) {
          lk <- setdiff(as.character(legs[[ii[k]]]$lines), common)
          if (length(lk)) paste0(LETTERS[ii[k]], ": ", lk) })))
        tx <- unlist(lapply(plots[ii], gr_figure_texts))
        gr_write_legend(pages[[pg]]$base,
                        paste0(what, leg_lab, if (length(pages) > 1) sprintf(" (page %d of %d)", pg, length(pages)) else ""),
                        c(paste0(paste0(LETTERS[ii], ", ", ttl, collapse = "; "), "."), lines,
                          if (length(moved)) gr_moved_legend(LETTERS[ii], moved[ii]),
                          gr_gee_legend_lines(tx, polar = TRUE)),
                        texts = c(tx, "◆"), logger = logger)
      }
    }
    if (length(dap) >= 2) {
      base <- file.path(out_dir, paste0("StandardEyes_DoubleAngle", tg))
      cells <- polar_cells(dap, da_mk)
      got <- compose_panels(cells$plots, captions = NULL, base = base,
        formats = formats, dpi = dpi, ncol = 2, pw = DA_W, ph = DA_H, gap_in = POLAR_GAP_IN,
        preview_dir = preview_dir(), logger = logger, width_in = width_in, min_pw = GR_POLAR_MIN_PW,
        need_h = cells$need_h, journal = cells$lay)
      written <- c(written, got)
      if (length(got)) composite_legend(dap, da_leg, base, "Double-angle plots", got, cells$moved)
    } else if (length(dap) == 1) {
      preview_single(dap[[1]], paste0("StandardEyes_DoubleAngle_", dac[1], tg), da_w, fig_h_of(dap[[1]], da_h))
    }
    if (length(scp) >= 2) {
      base <- file.path(out_dir, paste0("StandardEyes_Semicircle", tg))
      cells <- polar_cells(scp, sc_mk)
      got <- compose_panels(cells$plots, captions = NULL, base = base,
        formats = formats, dpi = dpi, ncol = 2, pw = SC_W, ph = SC_H, gap_in = POLAR_GAP_IN,
        preview_dir = preview_dir(), logger = logger, width_in = width_in, min_pw = GR_POLAR_MIN_PW,
        need_h = cells$need_h, journal = cells$lay)
      written <- c(written, got)
      if (length(got)) composite_legend(scp, sc_leg, base, "Semicircle (single-angle) plots", got, cells$moved)
    } else if (length(scp) == 1) {
      preview_single(scp[[1]], paste0("StandardEyes_Semicircle_", scc[1], tg), sc_w, fig_h_of(scp[[1]], sc_h))
    }
  }
  if (length(glv) > 5)
    logger(sprintf("StandardEyes figures: %d groups superimposed - readable up to about 5; consider subsets.",
                   length(glv)))
  .gr_opts$cluster_id <- NULL; .gr_opts$weights <- NULL     # reset on exit
  .gr_opts$matched <- FALSE
  # every test whose clustered GEE was fitted but failed - the univariate panel
  # tests (p_omnibus) and the double-angle centroid tests (astig_bivariate_p) -
  # and whose P therefore comes from the unclustered test
  n_refit <- length(gee_refit_take())
  if (n_refit > 0) logger(gee_refit_log_text(n_refit, paste0("StandardEyes figures", run_lab)))
  if ((.gr_opts$n_gee_fallback %||% 0L) > 0)
    logger(sprintf("StandardEyes figures%s: in %d test(s) the clustered GEE could not be fitted (no convergence, a numerically singular covariance, or a group of a 0/1 outcome without events); those P values come from a permutation test over patients (%d permutations, each patient's eyes kept together; a double-angle centroid comparison: the unclustered Hotelling / Pillai test) and are labeled \"GEE not fitted\" on the figure.",
                   run_lab, .gr_opts$n_gee_fallback, CLUSTER_PERM_B))
  # bar panels whose bars are too narrow for a label each: the postoperative
  # bars kept theirs first, as many as fit (gr_bars); the panels are counted here
  if ((.gr_opts$n_label_thin %||% 0L) > 0)
    logger(sprintf("StandardEyes figures%s: in %d bar panel(s) the bars are too narrow at this width for a %g-pt value label on each - the postoperative bars are labeled first, as many as fit side by side, then the others; a label that would overlap one already placed, or a taller bar beside it, is left out (each such panel says so in its legend file).",
                   run_lab, .gr_opts$n_label_thin, GR_MIN_TEXT_PT))
  if (dir.exists(prev_dir) && !length(list.files(prev_dir, all.files = TRUE, no.. = TRUE)))
    unlink(prev_dir, recursive = TRUE)

  # visits can resolve cleanly yet no panel be drawn - say so rather than
  # leaving the visit log looking like a figure was produced. The Six/Four sets
  # have no stability slot (said once per run); in the nine set the panel was
  # tried and the line says why it is missing, without advising a set that is
  # already chosen
  if (stab_on && !any(grepl("Stability", basename(written), fixed = TRUE))) {
    if (cset$six || cset$four) {
      if (!nzchar(tag))
        logger("Stability visits were mapped but no stability graph was produced - the stability panel belongs to the nine-panel set; choose the Full Standard (nine) set to include it.")
    } else {
      why_st <- as.character(fail_why["07_Stability"])
      if (is.na(why_st) || !nzchar(why_st))
        why_st <- "its file could not be written (see the 'Figure NOT written' messages above)"
      logger(sprintf("Stability visits were mapped but no stability graph was produced%s - %s.",
                     run_lab, sub("[.]+$", "", why_st)))
    }
  }
  # every failed write was logged where it happened; the summary compares the
  # files that exist with the files attempted, so a partial set is never
  # reported as a clean success
  n_fail <- .gr_opts$n_failed %||% 0L
  if (length(written) || n_fail)
    logger(sprintf("Graphs written%s: %d of %d file(s) in %s", run_lab, length(written),
                   length(written) + n_fail, out_dir))
  if (n_fail)
    logger(sprintf("WARNING: %d figure file(s)%s could not be written - see the 'Figure NOT written' messages above.",
                   n_fail, run_lab))
  if (!isTRUE(cfg$stability_figs))
    logger("Note: the 'Stability over time' graph is available - enable it and map one follow-up visit per time point.")
  written
}
