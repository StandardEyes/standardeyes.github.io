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
  ggplot2::theme_bw(base_size = 12.5) +
    ggplot2::theme(
      plot.title = ggplot2::element_text(face = "bold", hjust = .5, size = 13),
      axis.title = ggplot2::element_text(face = "bold", size = 11),
      axis.text = ggplot2::element_text(face = "bold", colour = "black", size = 9.5),
      axis.ticks = ggplot2::element_line(colour = "black", linewidth = .5),
      panel.border = ggplot2::element_rect(colour = "black", linewidth = .8, fill = NA),
      panel.grid.major.x = ggplot2::element_blank(),
      panel.grid.minor = ggplot2::element_blank(),
      panel.grid.major.y = ggplot2::element_line(colour = "grey88", linewidth = .3),
      legend.position = "top", legend.title = ggplot2::element_blank(),
      legend.text = ggplot2::element_text(size = 9.5, face = "bold"),
      legend.background = ggplot2::element_rect(colour = "grey20", linewidth = .4),
      plot.background = ggplot2::element_rect(fill = "white", colour = NA),
      plot.margin = ggplot2::margin(6, 10, 4, 6))
}

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

GR_FORMATS <- c("png", "tiff", "jpeg", "pdf")

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

# open a graphics device for `fm`; returns the device number
gr_open_device <- function(fn, fm, w, h, dpi) {
  switch(fm,
    png  = grDevices::png(fn, width = w, height = h, units = "in", res = dpi, bg = "white"),
    tiff = grDevices::tiff(fn, width = w, height = h, units = "in", res = dpi, compression = "lzw", bg = "white"),
    jpeg = grDevices::jpeg(fn, width = w, height = h, units = "in", res = dpi, quality = 95, bg = "white"),
    pdf  = gr_pdf_device()(fn, width = w, height = h, bg = "white"),
    stop("unsupported graph format '", fm, "' (use png, tiff, jpeg or pdf)"))
  grDevices::dev.cur()
}

# The formats the figure writers accept, normalised ONCE before any device is
# opened: case-insensitive, "jpg"/"tif" accepted as aliases. Anything else is
# dropped with a log line, so it can neither report a file that was never
# written nor open R's default device (a stray Rplots.pdf in the app folder).
gr_normalise_formats <- function(formats, logger = message) {
  f <- tolower(trimws(as.character(unlist(formats))))
  f <- f[!is.na(f) & nzchar(f)]
  f[f == "jpg"] <- "jpeg"; f[f == "tif"] <- "tiff"
  bad <- unique(setdiff(f, GR_FORMATS))
  if (length(bad))
    logger(sprintf("Unsupported graph format(s) ignored: %s (use png, tiff, jpeg or pdf).",
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
    else if (fm %in% GR_FORMATS) function()
      ggplot2::ggsave(fn, p, width = w, height = h, dpi = dpi, bg = "white")
    else function() stop("unsupported graph format '", fm, "' (use png, tiff, jpeg or pdf)")
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

# The test name as printed in a two-group header, short enough for one line
gr_short_test <- function(lab) {
  lab <- as.character(lab)
  switch(lab,
    "GEE (clustered by patient)"          = "GEE (clustered)",
    "Weighted GEE (clustered by patient)" = "Weighted GEE (clustered)",
    "Weighted GEE (robust SE)"            = "Weighted GEE",
    "Welch t-test"                        = "Welch t",
    "Welch t-test (GEE not fitted)"       = "Welch t (GEE not fitted)",
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
  parts <- list(...)
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

# one number for a header; "n/a" when it is undefined (an SD of one eye)
f2 <- function(v, d = 2) if (length(v) == 1 && is.finite(v)) sprintf(paste0("%.", d, "f"), v) else "n/a"

# "84%": the share of TRUE among the eyes where `cond` is defined; "n/a" when no
# eye has it defined (a group without a single usable value), never "NaN%"
gr_pct <- function(cond, d = 0) {
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

seg <- function(text, col = "black") list(text = text, col = col)

# "<label> <v1> vs. <v2>[<tail>]" with the two group values coloured
vs_line <- function(label, v1, v2, cols, tail = NULL) {
  L <- if (nzchar(label)) list(seg(label)) else list()
  L <- c(L, list(seg(v1, cols[[1]]), seg(" vs. "), seg(v2, cols[[2]])))
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
  if (is.na(d)) pp else sprintf("%s (d = %.2f)", pp, d)
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
# With clustering on (the default) figure P values are GEE (clustered by
# patient), not the eye-level table test: with two groups the P equals the
# table's GEE column; with three or more the omnibus line is the joint Wald
# test of all group terms and each "vs reference" line is that pair's own GEE.
# On a matched cohort with non-unit matching weights they are weighted GEE Wald
# tests (independence working correlation), like the matched summary tables.
# Every P printed names its test, the two-group headers included; a GEE that
# was fitted but failed (no convergence, a numerically singular covariance)
# falls back to the classical test, labelled "(GEE not fitted)" and counted in
# the run log (a weighted one has no fallback).
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
# tagged [Holm] when it holds two or more comparisons; the omnibus P and the
# tables are not adjusted.
# ============================================================
p_text <- function(p) {
  if (!is.finite(p)) return("")
  if (p < 0.0001) "P < 0.0001" else sprintf("P = %.4f", p)
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
    pw <- gee_group_p(x, g, idw, kind, event, w = wts[keep], unit = unit_w)
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
  # (patient clustering on) use a GEE with robust SE instead of a test that assumes
  # independent eyes. A group whose eyes all belong to one patient gets no P at
  # all (cluster_guard_note), nor do data below the minimum for any clustered
  # model ("too few eyes for this model (n = 5)", helpers.R model_data_guard),
  # as in the GEE tables and the weighted tests. A GEE that was fitted but failed
  # (no convergence, a numerically singular covariance) falls through to the
  # classical test, labelled "(GEE not fitted)" and counted.
  if (!is.null(cid)) {
    why_c <- cluster_guard_note(cid[keep][use], g[use])
    if (nzchar(why_c)) { out$reason <- why_c; return(out) }
    pc <- gee_group_p(x, g, cid[keep], kind, event)
    if (is.finite(pc)) {
      out$p <- pc
      out$test <- if (kind == "proportion") "GEE (binomial, clustered by patient)"
                  else "GEE (clustered by patient)"
      out$text <- p_text(pc)
      out$few <- few_patient_groups(cid[keep][use], g[use])
      return(out)
    }
    why_g <- as.character(attr(pc, "why") %||% "")[1]
    if (!is.na(why_g) && nzchar(why_g)) { out$reason <- why_g; return(out) }
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
  # clustering was requested but the GEE could not be fitted (no convergence, a
  # numerically singular covariance, a group of a 0/1 outcome without events):
  # the classical test that ran instead says so in its label, and the run log
  # counts these (see generate_standard_graphs)
  if (!is.null(cid) && nzchar(out$text)) {
    out$test <- paste0(out$test, " (GEE not fitted)")
    .gr_opts$n_gee_fallback <- (.gr_opts$n_gee_fallback %||% 0L) + 1L
  }
  if (!nzchar(out$text)) {
    out$reason <- gt$note %||% ""
    if (!nzchar(out$reason)) out$reason <- "the test could not be computed"
  }
  out
}

# Each group against the REFERENCE group (glv[1]), same test on the pair.
# Returns the RAW numeric P per comparison (NA when not testable), named by
# group; the caller adjusts for multiplicity (stat_footer: Holm). attr
# "fallback": the comparisons whose clustered GEE could not be fitted, so the
# classical test ran instead (stat_footer marks them "GEE not fitted").
p_vs_reference <- function(v, gl, glv, kind = "continuous", event = NULL) {
  if (length(glv) < 2) return(numeric(0))
  ref <- glv[1]
  res <- lapply(glv[-1], function(g) {
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
  vapply(seq_along(ch), function(i)
    paste0(if (i == 1) prefix else strrep(" ", nchar(prefix)),
           paste(ch[[i]], collapse = ", ")), character(1))
}

# "Welch ANOVA: P < 0.0001"
# "vs Control: Diabetes P = 0.0064, Autoimmune P = 0.4100  [Holm]"
# An omnibus that cannot run (e.g. one group has a single eye) says why ("P not
# computed: C has n = 1"), and the pairwise lines that CAN be computed are still
# shown. A comparison whose clustered GEE could not be fitted (the classical
# test ran under a GEE omnibus line) is marked "(GEE not fitted)".
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
hdr_text_pt <- function(txt, fs = MHDR_FS) {
  sum(vapply(hdr_pieces(txt), function(p)
    if (!is.na(HDR_MARKS[p])) hdr_mark_w(fs)
    else grid::convertWidth(grid::grobWidth(grid::textGrob(p,
      gp = grid::gpar(fontface = "bold", fontsize = fs))), "pt", valueOnly = TRUE), numeric(1)))
}

# draw one line of segments starting at unit x0; return the grobs and the
# line's right-edge unit (so the next column can be positioned after it).
# A segment with $sw draws a small filled swatch (legend key) before its text.
header_line_grob <- function(segs, y_pt, x0, fs = MHDR_FS) {
  gl <- grid::gList(); xpos <- x0
  for (s in segs) {
    if (!is.null(s$sw)) {
      gl <- grid::gList(gl, grid::rectGrob(x = xpos, y = grid::unit(y_pt + 1, "pt"),
        width = grid::unit(8, "pt"), height = grid::unit(8, "pt"), just = c("left", "bottom"),
        gp = grid::gpar(fill = s$sw, col = "grey20", lwd = .5)))
      xpos <- xpos + grid::unit(10, "pt")
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
build_header_children <- function(spec, scales = NULL) {
  maxL <- max(vapply(spec, function(c) length(c$lines), integer(1)))
  ytop <- 3 + (maxL - 1) * MHDR_LS
  gl <- grid::gList()
  prev_right <- grid::unit(0, "npc")
  for (k in seq_along(spec)) {
    col <- spec[[k]]
    # x = "auto" places this column just after the previous column's widest line
    x0 <- if (identical(col$x, "auto")) prev_right + grid::unit(12, "pt")
          else grid::unit(col$x, "npc")
    col_right <- x0
    for (j in seq_along(col$lines)) {
      f <- if (is.null(scales)) 1 else scales[[k]][j]
      lg <- header_line_grob(col$lines[[j]], ytop - (j - 1) * MHDR_LS, x0, fs = MHDR_FS * f)
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
  if (isTRUE(fit))
    return(grid::gTree(spec = spec, slack = slack, cl = "mheader_fit"))
  grid::gTree(children = build_header_children(spec))
}

# convert a ggplot to a gtable with the colored header drawn in its top margin
attach_header <- function(p, spec, fit = FALSE) {
  gt <- ggplot2::ggplotGrob(p)
  pan <- gt$layout[gt$layout$name == "panel", ]
  gtable::gtable_add_grob(gt, build_header_grob(spec, fit = fit), t = 1, b = min(pan$t) - 1,
                          l = min(pan$l), r = max(pan$r), clip = "off",
                          name = "mheader", z = Inf)
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
HDR_FIT_MIN <- 0.6     # smallest font factor a too-wide line is drawn at

# rendered width (pt) of one header line at font size fs, on the current device
hdr_line_pt <- function(segs, fs = MHDR_FS) {
  sum(vapply(segs, function(s) (if (!is.null(s$sw)) 10 else 0) + hdr_text_pt(s$text, fs),
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
    # a list keeps its comma at the end of the broken line; a " · " is dropped
    out <- c(out, paste0(cur, if (startsWith(sepv[i - 1], ",")) "," else ""))
    cur <- paste0(indent, pieces[i])
  }
  out <- c(out, cur)
  unlist(lapply(out, function(l) hdr_wrap_text(l, avail, wfun, indent, seps[-1])))
}

# one header line -> one or more lines no wider than `avail` pt where possible
hdr_wrap_line <- function(line, avail, seps = HDR_FIT_SEPS) {
  if (hdr_line_pt(line) <= avail) return(list(line))
  cols <- unique(vapply(line, function(s) as.character(s$col), character(1)))
  if (length(cols) != 1 || any(vapply(line, function(s) !is.null(s$sw), logical(1))))
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
      out <- c(out, if (is.na(av)) list(l) else hdr_wrap_line(l, av))
    }
    spec[[k]]$lines <- out
  }
  spec
}

# font factor per line so that every line fits its room (1 = as designed)
hdr_fit_scales <- function(spec, width_pt, slack = 4) {
  lapply(seq_along(spec), function(k) vapply(seq_along(spec[[k]]$lines), function(j) {
    av <- hdr_line_avail(spec, k, j, width_pt, slack)
    if (is.na(av)) return(1)
    w <- hdr_line_pt(spec[[k]]$lines[[j]])
    if (w <= av || w <= 0) 1 else max(HDR_FIT_MIN, av / w)
  }, numeric(1)))
}

makeContent.mheader_fit <- function(x) {
  w <- grid::convertWidth(grid::unit(1, "npc"), "pt", valueOnly = TRUE)
  grid::setChildren(x, build_header_children(x$spec, hdr_fit_scales(x$spec, w, x$slack)))
}
# registered with grid so the method is found however this file was sourced
registerS3method("makeContent", "mheader_fit", makeContent.mheader_fit,
                 envir = asNamespace("grid"))

# Run fun() with a measuring device open: the device family the PNG / TIFF / JPEG
# writers use, `w` x `h` inches, on a temporary file. The previous current
# device is restored afterwards.
gr_with_measure_device <- function(w, h, fun) {
  old <- grDevices::dev.cur()
  tf <- tempfile(fileext = ".png")
  ok <- tryCatch({ grDevices::png(tf, width = w, height = h, units = "in", res = 100,
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
#     instead of the header running past a frame that hugs the circle.
# Any line still too wide is drawn smaller at draw time (fit = TRUE).
gr_polar_finish <- function(p, hs, xlim, ylim, fig_w, fig_h) {
  build <- function(spec, xl) p +
    ggplot2::coord_fixed(xlim = xl, ylim = ylim, clip = "off") +
    ggplot2::theme(plot.margin = ggplot2::margin(t = 10 + MHDR_LS * hdr_nlines(spec),
                                                 r = 8, b = 6, l = 8))
  fixed_pt <- function(u, conv) {
    nn <- grid::unitType(u) != "null"
    if (!any(nn)) 0 else sum(conv(u[nn], "pt", valueOnly = TRUE))
  }
  fitted <- tryCatch(gr_with_measure_device(fig_w, fig_h, function() {
    gt <- ggplot2::ggplotGrob(build(hs, xlim))
    pw <- fig_w * 72.27 - fixed_pt(gt$widths, grid::convertWidth)
    hs2 <- hdr_fit_spec(hs, pw)
    gt <- ggplot2::ggplotGrob(build(hs2, xlim))
    ph <- fig_h * 72.27 - fixed_pt(gt$heights, grid::convertHeight)
    xl <- xlim
    if (ph > 0 && pw / ph > diff(xlim) / diff(ylim))
      xl <- mean(xlim) + c(-0.5, 0.5) * diff(ylim) * pw / ph
    list(hs = hs2, xlim = xl)
  }), error = function(e) list(hs = hs, xlim = xlim))
  attach_header(build(fitted$hs, fitted$xlim), fitted$hs, fit = TRUE)
}
hdr_nlines <- function(spec) if (is.null(spec)) 0 else
  max(vapply(spec, function(c) length(c$lines), integer(1)))

# rotated (90°) inline-colored stat line, centered in its cell (bottom-to-top)
rot_line_grob <- function(segs) {
  tgs <- lapply(segs, function(s) grid::textGrob(s$text, rot = 90, just = c("centre", "centre"),
    gp = grid::gpar(col = s$col, fontface = "bold", fontsize = MHDR_FS)))
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
                     gp = grid::gpar(col = unname(nbox[i]), fontface = "bold", fontsize = MHDR_FS)))
  }
  grid::gTree(children = gl)
}

# horizontal (coord_flip) panel: rotated colored stat on the left (replaces the
# category-axis title), rotated P on the right, colored N-boxes at the bottom.
# caution: the few-patients caution of that P value, as lines already wrapped
# to the figure's width (gr_hbar_caution), drawn under the N-boxes across the
# whole figure, left-aligned (contract R4-6: the rotated panels carry it too).
GR_HBAR_CAU_PAD <- 6     # pt between the figure's left edge and the caution
attach_hbar <- function(p, left_segs, right_text, nbox, caution = character(0)) {
  gt <- ggplot2::ggplotGrob(p)
  pan <- gt$layout[gt$layout$name == "panel", ]
  gt <- gtable::gtable_add_cols(gt, grid::unit(1.5, "lines"), pos = 0)
  gt <- gtable::gtable_add_grob(gt, rot_line_grob(left_segs), t = pan$t, b = pan$b,
                                l = 1, r = 1, clip = "off", name = "hstat", z = Inf)
  pan <- gt$layout[gt$layout$name == "panel", ]
  gt <- gtable::gtable_add_cols(gt, grid::unit(1.3, "lines"), pos = ncol(gt))
  gt <- gtable::gtable_add_grob(gt, grid::textGrob(right_text, rot = 90,
    gp = grid::gpar(fontface = "bold", fontsize = MHDR_FS)),
    t = pan$t, b = pan$b, l = ncol(gt), r = ncol(gt), clip = "off", name = "pstat", z = Inf)
  if (!is.null(nbox) && length(nbox)) {
    pan <- gt$layout[gt$layout$name == "panel", ]
    gt <- gtable::gtable_add_rows(gt, grid::unit(2.1, "lines"), pos = nrow(gt))
    gt <- gtable::gtable_add_grob(gt, nbox_stack_grob(nbox), t = nrow(gt), b = nrow(gt),
                                  l = pan$l, r = pan$r, clip = "off", name = "nbox", z = Inf)
  }
  caution <- as.character(caution)
  caution <- caution[!is.na(caution) & nzchar(caution)]
  if (length(caution)) {
    nl <- length(caution)
    gt <- gtable::gtable_add_rows(gt, grid::unit(5 + MHDR_LS * nl, "pt"), pos = nrow(gt))
    gt <- gtable::gtable_add_grob(gt, grid::textGrob(caution,
      x = grid::unit(GR_HBAR_CAU_PAD, "pt"),
      y = grid::unit(1, "npc") - grid::unit(3 + MHDR_LS * (seq_len(nl) - 1), "pt"),
      just = c("left", "top"), gp = grid::gpar(fontface = "bold", fontsize = MHDR_FS)),
      t = nrow(gt), b = nrow(gt), l = 1, r = ncol(gt), clip = "off", name = "hcaution", z = Inf)
  }
  gt
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
           hdr_wrap_text(caution, avail, function(t) hdr_text_pt(t, MHDR_FS), seps = GR_BAR_SEPS)),
         error = function(e) NULL)
  if (is.null(out) || !length(out))
    out <- strwrap(caution, width = max(20L, floor(avail / (0.6 * MHDR_FS))), exdent = 3)
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
                    hbar_left = NULL, hbar_right = NULL, caution = "") {
  # StandardEyes panels leave the corner boxes NULL, which selects the mEYEstro
  # look the builders reproduce: OPEN (no full box) + N-boxes off the right edge.
  is_meyestro <- is.null(box_left) && is.null(box_right)
  meyestro_v <- is_meyestro && !horizontal      # vertical mEYEstro bar panel
  use_hdr <- meyestro_v && !is.null(header_spec)  # grid inline-colored header
  use_hbar <- is_meyestro && horizontal && !is.null(hbar_left)  # rotated side stats
  if (use_hbar) xlab <- ""                       # rotated stat replaces the category title
  dfb$label <- factor(dfb$label, levels = unique(dfb$label))
  dfb$series <- factor(dfb$series, levels = names(pal))
  ymax <- max(dfb$pct, na.rm = TRUE)
  # mEYEstro puts the stat text ABOVE the panel and the legend INSIDE, so the
  # interior only needs room for the legend (not the annotation lines).
  headroom <- if (meyestro_v) 1.30 else 1 + 0.09 * (length(ann) + 1)
  # Each bar's value label is rotated 90 deg, so it consumes its FONT HEIGHT of
  # horizontal slot. Slot width shrinks as 1/(categories x series): past a point
  # the labels overprint into an unreadable smear. Drop them rather than lie.
  # (Physical geometry - raising the DPI cannot fix it.)
  slot_pt <- (5.6 * 72 * 0.82) / max(1, length(unique(dfb$label)) * length(unique(dfb$series)))
  PT_PER_MM <- 72.27 / 25.4          # ggplot2's .pt, which is not exported
  show_val <- slot_pt >= (label_size + 0.3) * PT_PER_MM + 1.5
  p <- ggplot2::ggplot(dfb, ggplot2::aes(x = label, y = pct, fill = series)) +
    ggplot2::geom_col(position = ggplot2::position_dodge(width = .8), colour = "grey20",
                      width = .72, linewidth = .25)
  if (show_val)
    p <- p + ggplot2::geom_text(ggplot2::aes(label = sprintf("%.0f%%", pct)),
                       position = ggplot2::position_dodge(width = .8),
                       angle = if (horizontal) 0 else label_angle,
                       vjust = if (horizontal || label_angle == 0) -0.45 * (label_angle == 0) + 0.35 * (label_angle != 0 || horizontal) else 0.35,
                       hjust = if (horizontal) -0.15 else if (label_angle == 0) 0.5 else -0.12,
                       size = label_size + 0.3, fontface = "bold")
  p <- p +
    ggplot2::scale_fill_manual(values = pal) +
    ggplot2::labs(x = xlab, y = ylab, title = if (meyestro_v || use_hbar) NULL else title) + gr_theme()
  if (x_angle != 0)
    p <- p + ggplot2::theme(axis.text.x = ggplot2::element_text(
      angle = x_angle, hjust = 1, vjust = 1, face = "bold", size = 8.5))
  caution <- as.character(caution %||% "")[1]
  has_caution <- !is.na(caution) && nzchar(caution)
  if (is_meyestro && !use_hbar && (use_hdr || length(ann) || has_caution)) {
    # header lines are MHDR_FS pt; the annotate() stat lines 2.9 mm (2.8 mm on
    # the horizontal panels). A header line runs from its column's x to the
    # panel's right edge; an annotated line from the panel's left edge.
    fitted <- gr_on_panel(if (horizontal) p + ggplot2::coord_flip() else p,
                          .gr_opts$cell_w %||% GR_BAR_MIN_W, function(pw) {
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
      fs <- (if (horizontal) 2.8 else 2.9) * 72.27 / 25.4
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
  has_nbox <- !horizontal && !is.null(nbox) && length(nbox)
  # The N-boxes are rotated labels stacked down the right edge; past four groups
  # they overprint each other into an unreadable smear. Drop the boxes and fold
  # the counts into the stat lines instead, which stack and wrap cleanly.
  # (only reachable with >4 superimposed groups)
  if (has_nbox && length(nbox) > 4) {
    cnt <- if (!is.null(attr(nbox, "n"))) as.character(attr(nbox, "n"))
           else sub("^N = ([0-9]+).*$", "\\1", names(nbox))
    ann <- c(ann, list(list(col = "black",
      text = paste0("n per group, in the order above: ", paste(cnt, collapse = ", ")))))
    has_nbox <- FALSE
  }
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
    if (has_nbox)
      p <- p + ggplot2::scale_x_discrete(expand = ggplot2::expansion(add = c(0.6, 1.7)))
    if (meyestro_v) {
      # legend INSIDE the plot (upper-left) + tall top margin for the stat text.
      # top margin reserves room for the grid header (or the annotate fallback)
      nlh <- if (use_hdr) hdr_nlines(header_spec) else length(ann)
      p <- p + ggplot2::theme(
        legend.text = ggplot2::element_text(size = 8, face = "bold"),
        legend.background = ggplot2::element_rect(fill = "white", colour = "grey45", linewidth = .3),
        legend.key.size = grid::unit(10, "pt"),
        legend.margin = ggplot2::margin(1, 4, 1, 4),
        plot.margin = ggplot2::margin(t = 8 + MHDR_LS * nlh, r = if (has_nbox) 8 else 10, b = 4, l = 6))
      if (identical(legend_pos, "inside"))
        p <- p + ggplot2::theme(legend.position = "inside",
          legend.position.inside = c(0.005, 0.99), legend.justification = c(0, 1))
      else
        p <- p + ggplot2::theme(legend.position = legend_pos)
    }
  }
  ylim_top <- ymax * headroom + 6
  if (horizontal) {
    # the horizontal panels need room (and unclipped drawing) above the top
    # category for the stat lines
    cmp_h <- is_meyestro && !use_hbar && length(ann) > 0
    if (cmp_h)
      p <- p + ggplot2::scale_x_discrete(
        expand = ggplot2::expansion(add = c(0.6, 0.9 + 0.85 * length(ann))))
    p <- p + ggplot2::coord_flip(ylim = c(0, ylim_top),
                                 clip = if (cmp_h) "off" else "on")
  } else {
    p <- p + ggplot2::coord_cartesian(ylim = c(0, ylim_top), clip = "off")
  }
  # mEYEstro rotated "N = x eyes" boxes, fully OUTSIDE the bars on the right,
  # black + stacked at two levels (matches the reference; never clipped)
  if (has_nbox) {
    kk <- length(nbox)
    ys <- if (kk == 1) 0.55 * ylim_top else ylim_top * seq(0.76, 0.30, length.out = kk)
    for (i in seq_len(kk))
      # colour per group so the reader can tell which N is which (matches ref)
      p <- p + ggplot2::annotate("label", x = nlev + 0.98, y = ys[i],
                                 label = names(nbox)[i], colour = unname(nbox[i]),
                                 fill = "white", angle = 90, size = 2.7,
                                 fontface = "bold", linewidth = .5)
  }
  if (length(ann)) {
    n_ann <- length(ann)
    for (i in seq_along(ann)) {
      if (horizontal && use_hbar) {
        # rotated stat/P + bottom N-boxes handled by attach_hbar; skip ann
      } else if (horizontal && is_meyestro) {
        # comparative horizontal panel: with one line per group the old inward
        # stack ran over the top categories, so put the lines ABOVE the bars
        p <- p + ggplot2::annotate("text", x = nlev + 0.75 + (n_ann - i) * 0.85, y = 0,
                                   label = ann[[i]]$text, colour = ann[[i]]$col,
                                   hjust = 0, vjust = 0.5, size = 2.8, fontface = "bold")
      } else if (horizontal) {
        # after coord_flip: x = category axis (vertical), y = value axis; stack
        # the annotation lines down the category axis, right-aligned
        p <- p + ggplot2::annotate("text", x = nlev + 0.2 - (i - 1) * 0.75, y = ylim_top,
                                   label = ann[[i]]$text, colour = ann[[i]]$col,
                                   hjust = 1, vjust = 1, size = 2.8, fontface = "bold")
      } else if (meyestro_v && !use_hdr) {
        # fallback (no header_spec): stat lines above the panel via annotate
        yy <- ylim_top * (1.05 + 0.055 * (n_ann - i))
        p <- p + ggplot2::annotate("text", x = 0.35, y = yy, label = ann[[i]]$text,
                                   colour = ann[[i]]$col, hjust = 0, vjust = 0,
                                   size = 2.9, fontface = "bold")
      } else if (meyestro_v) {
        # header handled by the grid overlay; skip the annotate lines
      } else {
        y0 <- if (!is.null(box_left) || !is.null(box_right)) 0.915 else 0.985
        p <- p + ggplot2::annotate("text", x = 0.55, y = ylim_top * (1 - 0.055 * (i - 1)) * y0,
                                   label = ann[[i]]$text, colour = ann[[i]]$col,
                                   hjust = 0, vjust = 1, size = 2.9, fontface = "bold")
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
  if (use_hdr) attach_header(p, header_spec)
  else if (use_hbar) attach_hbar(p, hbar_left, hbar_right, nbox,
                                 caution = if (has_caution) gr_hbar_caution(caution) else character(0))
  else p
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
# Alpins vectors against an EXPLICIT target (intended postop astigmatism).
# All arithmetic happens in double-angle space:
#   P = preop vector, Q = postop vector, T = target vector
#   TIA = P - T   the change that was intended
#   SIA = P - Q   the change that was achieved
#   DV  = Q - T   what is left relative to the intention
# With no target supplied this reduces to the classic target-of-zero case
# (TIA = preop, DV = postop); that branch reuses pm/qm/pa/qa directly rather
# than recomputing them through sqrt/atan2, so it introduces no floating-point
# drift.
# Also returns the scalar indices Alpins defines alongside the vectors.
# ------------------------------------------------------------
alpins_vectors_target <- function(pre_cyl, pre_ax, post_cyl, post_ax,
                                  tgt_cyl = NULL, tgt_ax = NULL) {
  pm <- abs(gr_num(pre_cyl)); pa <- gr_num(pre_ax) %% 180
  qm <- abs(gr_num(post_cyl)); qa <- gr_num(post_ax) %% 180
  px <- pm * cos(2 * pa * pi / 180); py <- pm * sin(2 * pa * pi / 180)
  qx <- qm * cos(2 * qa * pi / 180); qy <- qm * sin(2 * qa * pi / 180)

  # a null vector has no meridian; keep the eye's own axis so a zero-cylinder
  # eye reports the axis its data carries rather than an artificial 0
  ax_of <- function(x, y, fallback) {
    a <- (atan2(y, x) * 180 / pi / 2) %% 180
    m <- sqrt(x^2 + y^2)
    bad <- !is.finite(m) | m < 1e-12
    a[bad] <- fallback[bad]
    a
  }

  has_t <- !is.null(tgt_cyl) && !is.null(tgt_ax)
  if (has_t) {
    tm <- abs(gr_num(tgt_cyl)); ta <- gr_num(tgt_ax) %% 180
    tm[!is.finite(tm)] <- 0; ta[!is.finite(ta)] <- 0
    tx <- tm * cos(2 * ta * pi / 180); ty <- tm * sin(2 * ta * pi / 180)
  } else {
    tx <- 0; ty <- 0
  }

  sx <- px - qx; sy <- py - qy                       # SIA, target-independent
  sia <- sqrt(sx^2 + sy^2)
  sax <- (atan2(sy, sx) * 180 / pi / 2) %% 180

  if (has_t) {
    ix <- px - tx; iy <- py - ty                     # TIA
    dx <- qx - tx; dy <- qy - ty                     # DV
    tia <- sqrt(ix^2 + iy^2); dv <- sqrt(dx^2 + dy^2)
    tax <- ax_of(ix, iy, pa);  dax <- ax_of(dx, dy, qa)
  } else {
    tia <- pm; dv <- qm; tax <- pa; dax <- qa
    ix <- px; iy <- py
  }

  aoe <- (atan2(sy, sx) - atan2(iy, ix)) * 180 / pi / 2
  aoe <- ((aoe + 90) %% 180) - 90
  undef <- !is.finite(tia) | tia == 0                # every index divides by TIA
  aoe[undef] <- NA
  ci <- sia / tia; ci[undef] <- NA                   # correction index
  me <- sia - tia                                    # magnitude of error
  ios <- dv / tia; ios[undef] <- NA                  # index of success
  list(tia = tia, tax = tax, sia = sia, sax = sax, dv = dv, dax = dax,
       aoe = aoe, ci = ci, me = me, ios = ios, has_target = has_t)
}

# ------------------------------------------------------------
# Resolve the Alpins vectors for a run, PER VECTOR, logging the provenance of
# each so a reader can always tell whether a number came from the user's
# spreadsheet or from the engine. Precedence, independently for TIA / SIA / DV:
#   1. magnitude AND axis columns mapped and present -> use them as given
#   2. else a target cylinder + axis mapped          -> compute against that target
#   3. else                                          -> compute with target = 0
# When a vector is supplied AND computable, the computed value is produced anyway
# and the largest disagreement is reported, so the two can be reconciled.
# ------------------------------------------------------------
# display_names: optional clean key -> header map (res$display_names): the log
# names the mapped columns by their headers as typed in the file (R4-3).
resolve_astig_vectors <- function(df, pre_cyl, pre_ax, post_cyl, post_ax,
                                  vec_cols = list(), target = list(), logger = NULL,
                                  display_names = NULL) {
  say <- function(...) if (!is.null(logger)) logger(paste0(...))
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
  if (has_t)
    say("Astigmatism: a target refraction is mapped, so TIA is the intended CHANGE, ",
        "not the preoperative astigmatism - values will differ from a target-of-zero run.")

  spec <- list(list("tia", "tax", "tia_mag", "tia_axis", "TIA"),
               list("sia", "sax", "sia_mag", "sia_axis", "SIA"),
               list("dv",  "dax", "dv_mag",  "dv_axis",  "DV"))
  changed <- FALSE
  for (v in spec) {
    cm <- gcol(v[[3]]); ca <- gcol(v[[4]])
    if (is.null(cm) || is.null(ca)) { say("Astigmatism ", v[[5]], ": ", base, "."); next }
    sm <- abs(gr_num(df[[cm]])); sa <- gr_num(df[[ca]]) %% 180
    d <- abs(sm - av[[v[[1]]]]); d <- d[is.finite(d)]
    say(sprintf("Astigmatism %s: supplied from '%s' / '%s' (%d eyes); max difference vs %s = %s D.",
                v[[5]], dn(cm), dn(ca), sum(is.finite(sm)), base,
                if (length(d)) sprintf("%.4f", max(d)) else "n/a"))
    av[[v[[1]]]] <- sm; av[[v[[2]]]] <- sa
    changed <- TRUE
  }

  # every index is a function of the RESOLVED vectors, so recompute them only if
  # a vector was actually overridden (otherwise keep the exact original values)
  if (changed) {
    undef <- !is.finite(av$tia) | av$tia == 0
    av$ci <- av$sia / av$tia; av$ci[undef] <- NA
    av$me <- av$sia - av$tia
    av$ios <- av$dv / av$tia; av$ios[undef] <- NA
    sx <- av$sia * cos(2 * av$sax * pi / 180); sy <- av$sia * sin(2 * av$sax * pi / 180)
    tx <- av$tia * cos(2 * av$tax * pi / 180); ty <- av$tia * sin(2 * av$tax * pi / 180)
    aoe <- (atan2(sy, sx) - atan2(ty, tx)) * 180 / pi / 2
    av$aoe <- ((aoe + 90) %% 180) - 90
    av$aoe[undef] <- NA
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
      text = sprintf("%s: UDVA %s / %s %s LogMAR  (N UDVA = %d, CDVA = %d)", g,
                     msd(gr_logmar(pu[gl == g])), second_lab, msd(gr_logmar(pc[gl == g])),
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
  # UDVA / reference CDVA LogMAR mean±SD beneath, one column per group.
  hs <- NULL; lpos <- "inside"
  if (length(glv) == 2) {
    col_of <- function(g, light) if (light) (second_col %||% lighten_col(pal[[g]])) else pal[[g]]
    mkcol <- function(g) list(x = if (g == glv[1]) 0.01 else "auto", lines = list(
      list(seg(paste0(g, " Postop UDVA"), "black")),
      list(seg(paste0(g, " ", second_lab,
                      if (nc(g) != nu(g)) sprintf(" (n = %d)", nc(g)) else ""), "black")),
      list(seg(paste0(msd(gr_logmar(pu[gl == g])), " LogMAR"), col_of(g, FALSE))),
      list(seg(paste0(msd(gr_logmar(pc[gl == g])), " LogMAR"), col_of(g, TRUE)))))
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
  ann <- lapply(glv, function(g) list(col = pal[[g]], text = sprintf(
    "%s: Efficacy Index %s · UDVA≥%s %s", g, msd(ei[gl == g]), cdva_lab,
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
          caution = gr_caution(pp))
}

g_cdva_change <- function(pv_pre, pv_post, gl, glv, pal, saf_num, saf_den, nb = TRUE) {
  lines <- gr_line_diff(pv_pre, pv_post)
  nb <- panel_nbox(nb, is.finite(lines), gl, glv, pal)
  labs <- c("Loss 2 or\nmore", "Loss 1", "No change", "Gain 1", "Gain 2 or\nmore")
  dfb <- pct_by_group(lines, gl, glv, c(-Inf, -1.5, -0.5, 0.5, 1.5, Inf), labs)
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
  gr_bars(dfb, pal[glv], "Change in lines of CDVA", "% of eyes",
          "Change in Corrected Distance Visual Acuity", ann, nbox = nb, header_spec = hs,
          caution = gr_caution(pp))
}

# `poseq` is the SEQ error to bin: postop SEQ minus the intended target (the
# caller subtracts a mapped target_seq; with none the target is plano and this
# is the postop SEQ itself). has_target only changes the header wording.
g_seq_accuracy <- function(poseq, gl, glv, pal, nb = TRUE, has_target = FALSE) {
  err <- gr_num(poseq)
  nb <- panel_nbox(nb, is.finite(err), gl, glv, pal)
  slab <- if (has_target) "SEQ - target" else "SEQ"
  labs <- c("<-2", "-2 to -1.51", "-1.5 to -1.01", "-1 to -0.51", "-0.5 to -0.14",
            "-0.13 to +0.13", "+0.14 to +0.5", "+0.51 to +1", "+1.01 to +1.5",
            "+1.51 to +2", ">+2")
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
  }
  gr_bars(dfb, pal[glv], "Accuracy of SEQ to intended target (D)", "% of eyes",
          "Spherical Equivalent Refraction Accuracy", ann, label_size = 2.2,
          x_angle = 32, nbox = nb, header_spec = hs, caution = gr_caution(pp))
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
    list(col = pal[[g]], lines = ls, short = short)
  })
  blocks <- Filter(Negate(is.null), blocks)
  # 1-2 groups: classic side-by-side stat columns. 3+ groups: one FULL-WIDTH
  # line per group instead, because thirds of a composite cell are too narrow
  # and the columns collide.
  stacked <- length(blocks) > 2
  if (stacked)
    blocks <- lapply(blocks, function(b) list(col = b$col, lines = b$short))
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
                   plot.margin = ggplot2::margin(t = 26 + 13 * nlines, r = 10, b = 4, l = 6))
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
  # Over/Undercorrected corner boxes (dx/dy already honour `reverse`)
  p <- p +
    ggplot2::annotate("label", x = dx(0.03), y = dy(0.05), label = corner_labels[1],
                      hjust = 0, vjust = 1, size = 2.9, fontface = "bold") +
    ggplot2::annotate("label", x = dx(0.97), y = dy(0.95), label = corner_labels[2],
                      hjust = 1, vjust = 0, size = 2.9, fontface = "bold")
  # colored N-boxes inside, stacked under the top-left corner box
  if (!is.null(nbox) && length(nbox))
    for (i in seq_along(nbox))
      p <- p + ggplot2::annotate("label", x = dx(0.03), y = dy(0.12 + 0.085 * i),
                                 label = names(nbox)[i], colour = unname(nbox[i]),
                                 fill = "white", hjust = 0, vjust = 1, size = 2.7,
                                 fontface = "bold", linewidth = .5)
  p + ggplot2::coord_fixed(xlim = lim, ylim = lim, clip = "off")
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
  labs <- c("<= 0.25", "0.26 to 0.5", "0.51 to 0.75", "0.76 to 1.00", "1.01 to 1.25",
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
      "%s: Postop RA %s D · ≤0.50 %s · ≤1.00 %s", g, msd(post[gl == g]),
      gr_pct(post[gl == g] <= .5 + 1e-9), gr_pct(post[gl == g] <= 1 + 1e-9)))
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
  }
  gr_bars(do.call(rbind, rows), pal2, "Refractive Astigmatism (D)", "% of eyes",
          "Refractive Astigmatism", ann, label_size = 2.2, x_angle = 45, nbox = nb, header_spec = hs,
          caution = gr_caution(pp))
}

g_tia_sia <- function(av, gl, glv, pal) {
  ok <- is.finite(av$tia) & is.finite(av$sia) & av$tia >= 0.25 & !is.na(gl)
  nb <- make_nbox(gl[ok], glv, pal, suffix = "eyes with TIA ≥ 0.25D", total = gl)
  scatter_ind(data.frame(x = av$tia[ok], y = av$sia[ok], Group = factor(gl[ok], levels = glv)),
              pal[glv], "TIA Vector (D)", "SIA Vector (D)",
              "Target Induced vs Surgically Induced Astigmatism", from_zero = TRUE,
              reverse = FALSE, stat_kind = "tiasia", nbox = nb)
}

g_ci_hist <- function(av, gl, glv, pal) {
  keep <- is.finite(av$tia) & av$tia >= 0.25
  ci <- ifelse(keep, av$sia / av$tia, NA)
  labs <- c("0 to 0.15", "0.15 to 0.3", "0.3 to 0.6", "0.6 to 0.9", "0.9 to 1.1",
            "1.1 to 1.6", "1.6 to 2.1", "2.1 to 2.5", "> 2.5")
  brk <- c(-Inf, .15, .3, .6, .9, 1.1, 1.6, 2.1, 2.5, Inf)
  dfb <- pct_by_group(ci, gl, glv, brk, labs)
  ann <- lapply(glv, function(g) list(col = pal[[g]], text = sprintf(
    "%s: CI %s (N=%d, TIA≥0.25D)", g, msd(ci[gl == g]), sum(is.finite(ci) & gl == g, na.rm = TRUE))))
  pp <- p_group_text(ci, gl, glv, max_chars = HBAR_MAX_CHARS)
  for (.t in pp) if (nzchar(.t)) ann <- c(ann, list(list(col = "black", text = .t)))
  hl <- hr <- nb <- NULL
  if (length(glv) == 2) {
    cols <- unname(pal[glv]); cim <- vapply(glv, function(g) msd(ci[gl == g]), character(1))
    hl <- vs_line("Correction Index: ", cim[1], cim[2], cols)
    hr <- p_d_text(ci, gl, glv, pp)
    nb <- make_nbox(ifelse(is.finite(ci), gl, NA), glv, pal, suffix = "eyes with TIA ≥ 0.25D",
                    total = gl)
  }
  gr_bars(dfb, pal[glv], "Correction Index", "% of eyes", "Correction Index",
          ann, horizontal = TRUE, nbox = nb, hbar_left = hl, hbar_right = hr,
          caution = gr_caution(pp))
}

g_angle_of_error <- function(av, gl, glv, pal) {
  keep <- is.finite(av$tia) & av$tia >= 0.25
  aoe <- ifelse(keep, av$aoe, NA)
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
    cols <- unname(pal[glv]); am <- vapply(glv, function(g) msd(aoe[gl == g]), character(1))
    hl <- vs_line("Angle of Error: ", am[1], am[2], cols)
    hr <- p_d_text(aoe, gl, glv, pp)
    nb <- make_nbox(ifelse(is.finite(aoe), gl, NA), glv, pal, suffix = "eyes with TIA ≥ 0.25D",
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
      logger(sprintf("Stability: %d time points are labelled '%s' - each is kept as its own point; a change pair naming '%s' uses the %s.",
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
    ggplot2::labs(x = "Time after surgery (months)",
                  y = if (rein) "Mean ± SD SEQ (D)" else "SEQ (D)") +
    gr_theme() +
    ggplot2::theme(legend.position = "none",
                   panel.grid.major.x = ggplot2::element_blank(),
                   panel.grid.minor = ggplot2::element_blank(),
                   axis.text.x = ggplot2::element_text(
                     colour = "transparent", lineheight = STAB_AX_LH,
                     margin = ggplot2::margin(t = STAB_AX_TOP, b = 2 + STAB_NROW_PT * n_rows)))
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
               else sprintf("● %s: %.2f%% changed > %.2f D (%s)",
                            g, c1$pct, thresh, pair_txt("mo")), gcol))
    })
    hs[[1]] <- list(x = 0.01, lines = lines)
  } else for (j in seq_along(glv)) {
    g <- glv[j]; gcol <- unname(pal[g]); c1 <- chg[[g]]
    lines <- list(list(seg(paste0("● ", g), gcol)))
    if (!is.null(c1)) {
      lines <- c(lines, list(
        list(seg(sprintf("%% changed > %.2f D", thresh), gcol)),
        list(seg(sprintf("%s: %.2f%%", pair_txt("months"), c1$pct), gcol))))
    }
    hs[[length(hs) + 1]] <- list(x = if (j == 1) 0.01 else "auto", lines = lines)
  }
  p <- p + ggplot2::theme(
    plot.margin = ggplot2::margin(t = 10 + MHDR_LS * hdr_nlines(hs), r = 12,
                                  b = if (rein) 4 else 10, l = 6))
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

stab_axis_grob <- function(xs, labs, nrows, cols, xl)
  grid::gTree(xs = xs, labs = labs, nrows = nrows, cols = cols, xl = xl, cl = "stab_axis")

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
  gp_l <- grid::gpar(fontsize = STAB_AX_FS, fontface = "bold", lineheight = STAB_AX_LH)
  tw <- function(s, gp) grid::convertWidth(grid::grobWidth(grid::textGrob(s, gp = gp)),
                                           "pt", valueOnly = TRUE)
  w <- vapply(x$labs, tw, numeric(1), gp = gp_l)
  for (r in x$nrows) {
    gp_n <- grid::gpar(fontsize = STAB_LAB_FS, fontface = "bold")
    w <- pmax(w, vapply(r, tw, numeric(1), gp = gp_n))
  }
  # the right edge may use part of the plot margin; the left one may not ("N =")
  lx <- stab_col_place(px, w + 2, lo = 0, hi = vw + 8)
  pt <- function(v) grid::unit(v, "pt")
  tick_end <- vh - STAB_TICK_PT
  lab_top <- tick_end - STAB_AX_TOP
  kids <- grid::gList()
  moved <- which(abs(lx - px) > 0.5)
  if (length(moved))
    kids <- grid::gList(kids, grid::segmentsGrob(pt(px[moved]), pt(tick_end), pt(lx[moved]),
      pt(lab_top + 1), gp = grid::gpar(col = "grey45", lwd = 0.6)))
  kids <- grid::gList(kids, grid::textGrob(x$labs, pt(lx), pt(lab_top), just = c("centre", "top"),
                                           gp = gp_l))
  nr <- length(x$nrows)
  for (j in seq_len(nr)) {
    yj <- 2 + (nr - j + 0.5) * STAB_NROW_PT
    gp_j <- grid::gpar(col = x$cols[j], fontsize = STAB_LAB_FS, fontface = "bold")
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

stab_label_grob <- function(lab, xl, yl) grid::gTree(lab = lab, xl = xl, yl = yl, cl = "stab_labels")

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
  gp_t <- grid::gpar(fontsize = STAB_LAB_FS, fontface = "bold")
  tw <- vapply(d$lab, function(s) grid::convertWidth(grid::grobWidth(
    grid::textGrob(s, gp = gp_t)), "pt", valueOnly = TRUE), numeric(1))
  th <- grid::convertHeight(grid::grobHeight(grid::textGrob("0.00", gp = gp_t)), "pt", valueOnly = TRUE) +
    grid::convertHeight(grid::grobDescent(grid::textGrob("0.00", gp = gp_t)), "pt", valueOnly = TRUE)
  w <- tw + 2 * STAB_LAB_PAD; h <- rep(th + 2 * STAB_LAB_PAD, nrow(d))
  dx <- stab_label_shift(px, ly, w, h, d$t)
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
      grid::roundrectGrob(pt(lx[i]), pt(ly[i]), width = pt(w[i]), height = pt(h[i]),
                          r = grid::unit(0.15, "lines"),
                          gp = grid::gpar(fill = d$fill[i], col = NA, fontsize = STAB_LAB_FS)),
      grid::textGrob(d$lab[i], pt(lx[i]), pt(ly[i]),
                     gp = grid::gpar(col = d$col[i], fontsize = STAB_LAB_FS, fontface = "bold")))
  }
  grid::setChildren(x, kids)
}
registerS3method("makeContent", "stab_labels", makeContent.stab_labels,
                 envir = asNamespace("grid"))

# ============================================================
# JCRS-standard DOUBLE-ANGLE plot, all groups superimposed.
#
# Every point is placed at (M cos 2t, M sin 2t), so the DISPLAYED angle is twice
# the meridian and the plot is a full 360 degrees. This is what makes the vector
# arithmetic legitimate and groups with-the-rule / against-the-rule data on
# opposite sides instead of splitting each across the horizontal meridian:
#   meridian  90 (WTR) -> displayed 180 -> left
#   meridian 0/180 (ATR) -> displayed 0/360 -> right
#   meridians 45 / 135 (oblique) -> displayed 90 / 270 -> top / bottom
# Required by JCRS 2025 "Standards for analyzing astigmatic outcomes" (Part II).
# ============================================================

# 95% region for one group's doubled-angle cloud.
#   "normal"  - bivariate-normal ellipse from the 2x2 covariance:
#               {u : (u-c)' S^-1 (u-c) <= qchisq(.95, 2)}, drawn from the
#               eigen-decomposition (semi-axes sqrt(qchisq * lambda_i)).
#               centroid = TRUE gives the 95% CI of the CENTROID instead
#               (Hotelling F radius, patient-clustered when `id` is given;
#               see centroid_region).
#   "polygon" - distribution-free: convex hull of the floor(95% n) deepest eyes,
#               found by hull peeling (see astig_region). Makes no normality
#               assumption (see JCRS Part II).
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

# Joint Wald test of the coefficients `idx` of a fitted geeglm, from their
# robust (sandwich) covariance. NA - never a P value - when that covariance is
# numerically singular (contract R4-6; helpers.R gee_vcov_singular, the rule the
# weighted table test shares): a group of two patients adds only a rank-1 block,
# a group without between-patient spread adds none, and the statistic would
# explode into a spurious "P < 0.001". NA too when the statistic is not finite.
# The NA carries attr "singular" = TRUE for a singular covariance. The caller
# treats either NA as a model that could not be fitted.
gr_wald_p <- function(fit, idx) {
  b <- stats::coef(fit)
  V <- tryCatch(fit$geese$vbeta, error = function(e) NULL)
  if (is.null(V) || !length(idx) || any(!is.finite(b[idx]))) return(NA_real_)
  Vg <- V[idx, idx, drop = FALSE]
  if (any(!is.finite(Vg))) return(NA_real_)
  if (gee_vcov_singular(fit, idx)) return(structure(NA_real_, singular = TRUE))
  W <- tryCatch(as.numeric(t(b[idx]) %*% solve(Vg) %*% b[idx]), error = function(e) NA_real_)
  if (!is.finite(W)) return(NA_real_)
  stats::pchisq(W, df = length(idx), lower.tail = FALSE)
}

# Wald p for the group term. kind "proportion" -> binomial, else gaussian.
# w: optional matching weights (one per element of x); eyes with a missing or
# non-positive weight are left out, as in analysis.R's weighted tables. With
# weights that are not all 1 the model is analysis.R's weighted model: an
# INDEPENDENCE working correlation with the robust SE clustered by `id`, whose
# estimate is the difference of the weighted means / proportions (contract
# R4-1); otherwise exchangeable. unit: "eye" when `id` is the row (no patient
# ids: each eye its own cluster), so the reasons count eyes.
# Returns the P value, or NA:
#  - with attr "why" when the data cannot support the model - nothing is fitted
#    and the reason is the tables' one, checked in .weighted_wald's order:
#    fewer than two groups / "B has n = 1" (p_value_guard, small_group_note),
#    "B has 1 patient" (cluster_guard_note), "too few eyes for this model (n =
#    5)" (model_data_guard: the one minimum-data rule of every clustered model,
#    contract R4-10) and, on a weighted 0/1 outcome, "no weighted test possible:
#    no events in B";
#  - without "why" when a model was fitted but failed (no convergence, a
#    numerically singular covariance - gr_wald_p) or, unweighted, a group of a
#    0/1 outcome has no events or only events (no finite estimate): the caller
#    falls back (unweighted) or says the weighted test could not be computed.
gee_group_p <- function(x, g, id, kind = "continuous", event = NULL, w = NULL,
                        unit = "patient") {
  na_why <- function(why) structure(NA_real_, why = why)
  if (!requireNamespace("geepack", quietly = TRUE)) return(NA_real_)
  # the groups in their given order, for the reasons; the model's factor stays
  # alphabetical (the joint Wald test does not depend on the reference level)
  glv <- if (is.factor(g)) levels(g) else unique(as.character(g[!is.na(g)]))
  g <- factor(as.character(g))
  y <- if (identical(kind, "proportion")) {
    as.numeric(if (is.null(event)) as.logical(x) else as.character(x) == as.character(event))
  } else gr_num(x)
  id <- fill_missing_ids(id)
  weighted <- !is.null(w)                       # a matched cohort's weighted test
  cs <- if (weighted && weights_nonunit(w)) "independence" else "exchangeable"
  wt <- if (is.null(w)) rep(1, length(y)) else suppressWarnings(as.numeric(w))
  ok <- is.finite(y) & !is.na(g) & is.finite(wt) & wt > 0
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
  # protocol): quiet_eval, helpers.R
  fit <- quiet_eval(suppressWarnings(
           if (is.null(w)) geepack::geeglm(y ~ g, id = cid, data = d, family = fam, corstr = cs)
           else geepack::geeglm(y ~ g, id = cid, data = d, weights = .w, family = fam,
                                corstr = cs)))$value
  if (is.null(fit)) return(NA_real_)
  gr_wald_p(fit, grep("^g", names(stats::coef(fit))))
}

# ROBUST bivariate centroid comparison in the spirit of JCRS 2025 Part II (not
# its exact trimmed-Hotelling variant): a Hotelling-type statistic - the sum over
# groups of n_i * d_i' S^-1 d_i, d_i = a group's 20%-trimmed mean vector minus
# the grand mean, S = the pooled 20%-winsorized covariance - so outlying eyes
# cannot drag the centroid, calibrated by B = 999 BOOTSTRAP resamples (each
# group recentred on its trimmed mean; fixed seed) rather than an F reference.
# When a cluster id is supplied the bootstrap resamples PATIENTS, so the
# robustness and the clustering hold at the same time.
ASTIG_TRIM <- 0.2
ASTIG_BOOT_B <- 999L
winsor <- function(v, trim) {
  if (trim <= 0) return(v)
  q <- stats::quantile(v, c(trim, 1 - trim), names = FALSE, na.rm = TRUE)
  pmin(pmax(v, q[1]), q[2])
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
  # Reproducibility: fix the bootstrap seed locally, restoring the caller's RNG
  # state afterwards. Without this the same config + data printed a slightly
  # different p on every run, which contradicts the preset-reproducibility claim.
  if (exists(".Random.seed", envir = globalenv())) {
    old_seed <- get(".Random.seed", envir = globalenv())
    on.exit(assign(".Random.seed", old_seed, envir = globalenv()), add = TRUE)
  } else {
    on.exit(rm(".Random.seed", envir = globalenv()), add = TRUE)
  }
  set.seed(20250601L)
  obs <- robust_biv_stat(x, y, g, trim)
  if (!is.finite(obs)) return(astig_na("the robust statistic could not be computed"))
  # null: recentre every group on its own trimmed mean, so groups share a centre
  for (l in levels(g)) {
    k <- g == l
    x[k] <- x[k] - mean(x[k], trim = trim); y[k] <- y[k] - mean(y[k], trim = trim)
  }
  cl <- split(seq_along(x), id)                 # resample whole patients
  nc <- length(cl)
  cnt <- 0L; eff <- 0L
  for (b in seq_len(B)) {
    idx <- unlist(cl[sample.int(nc, nc, replace = TRUE)], use.names = FALSE)
    gb <- droplevels(g[idx])
    if (nlevels(gb) < nlevels(g)) next
    st <- robust_biv_stat(x[idx], y[idx], gb, trim)
    if (!is.finite(st)) next
    eff <- eff + 1L; if (st >= obs) cnt <- cnt + 1L
  }
  if (eff < 50) return(astig_na("too few usable bootstrap resamples"))
  (cnt + 1) / (eff + 1)
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
# correlation, like every weighted model (contract R4-1). A GEE whose
# group-term covariance is numerically singular is not reported (gr_wald_p,
# contract R4-6): a weighted test then says it could not be fitted, a clustered
# one falls back to Hotelling / Pillai, labelled "GEE not fitted" and counted
# in the run log's fallback line (.gr_opts$n_gee_fallback), like p_omnibus.
# The result carries attr "test" - the test that actually ran ("weighted GEE",
# "clustered GEE", "Hotelling", "Pillai") - and, when it is NA, attr "why".
astig_bivariate_p <- function(x, y, g, id = NULL, w = NULL) {
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
      cs <- if (weighted && weights_nonunit(wk)) "independence" else "exchangeable"
      f2 <- quiet_eval(suppressWarnings(
              if (weighted) geepack::geeglm(v ~ comp * g, id = cid, data = d2, weights = .w,
                                            corstr = cs)
              else geepack::geeglm(v ~ comp * g, id = cid, data = d2, corstr = cs)))$value
      if (!is.null(f2)) {
        pg <- gr_wald_p(f2, grep("(^g)|(:g)", names(stats::coef(f2))))
        if (is.finite(pg))
          return(structure(as.numeric(pg), test = gtest,
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
astig_vs_ref <- function(x, y, g, glv, holm = TRUE, id = NULL, w = NULL) {
  if (length(glv) < 2) return(NULL)
  ref <- glv[1]; others <- glv[-1]
  res <- lapply(others, function(o) {
    k <- g %in% c(ref, o)
    astig_bivariate_p(x[k], y[k], g[k], id = if (is.null(id)) NULL else id[k],
                      w = if (is.null(w)) NULL else w[k])
  })
  p <- vapply(res, function(r) as.numeric(r)[1], numeric(1))
  fb <- others[vapply(res, function(r) is.finite(r) && isTRUE(attr(r, "fallback")), logical(1))]
  few <- do.call(gr_few_merge, lapply(res, function(r) if (is.finite(r)) attr(r, "few")))
  if (holm && length(p) > 1) p <- stats::p.adjust(p, method = "holm")
  structure(stats::setNames(p, others), fallback = fb, few = few)
}

astig_region <- function(x, y, kind = "normal", scale = 1, level = 0.95,
                         centroid = FALSE, id = NULL) {
  ok <- is.finite(x) & is.finite(y)
  if (!is.null(id)) id <- id[ok]
  x <- x[ok]; y <- y[ok]
  n <- length(x)
  if (n < 3) return(NULL)
  cx <- mean(x); cy <- mean(y)
  if (isTRUE(centroid)) return(centroid_region(x, y, id, level))
  if (identical(kind, "polygon")) {
    # JCRS 2025 Part II: a NONPARAMETRIC 95% boundary based on the depth of each
    # point in the cloud - peel whole convex hulls while a full layer still
    # leaves >= floor(95% n) eyes, then finish the last layer vertex by vertex
    # (most extreme first, by Mahalanobis distance so the elongated WTR/ATR
    # clouds are trimmed along their real shape) until EXACTLY floor(95% n)
    # remain, and hull those. Stopping at whole layers kept every eye for any
    # cohort under ~200, because one layer already holds more than 5% of it.
    idx <- seq_len(n)
    target <- max(floor(level * n), 3)
    while (length(idx) > target) {
      h <- grDevices::chull(x[idx], y[idx])
      if (length(idx) - length(h) >= target) { idx <- idx[-h]; next }   # a whole layer
      xi <- x[idx]; yi <- y[idx]
      md <- tryCatch(stats::mahalanobis(cbind(xi[h], yi[h]), c(mean(xi), mean(yi)),
                                        stats::cov(cbind(xi, yi))),
                     error = function(e) NULL)
      if (is.null(md) || any(!is.finite(md)))
        md <- (xi[h] - mean(xi))^2 + (yi[h] - mean(yi))^2
      idx <- idx[-h[which.max(md)]]
    }
    if (length(idx) < 3) return(NULL)
    xs <- x[idx]; ys <- y[idx]
    h <- grDevices::chull(xs, ys)
    out <- data.frame(x = xs[c(h, h[1])], y = ys[c(h, h[1])])
    attr(out, "kept") <- length(idx); attr(out, "n") <- n
    return(out)
  }
  S <- tryCatch(stats::cov(cbind(x, y)) * scale, error = function(e) NULL)
  ellipse_path(cx, cy, S, sqrt(stats::qchisq(level, df = 2)))
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

# Why centroid_region() (via astig_region) drew nothing for these eyes: too few
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
  "no spread in one direction"
}

# Size (inches) of a double-angle / semicircle plot: its own file and its cell in
# the composite. The header is fitted to it (gr_polar_finish).
DA_W <- 6.8; DA_H <- 6.6
SC_W <- 7.2; SC_H <- 5.4
POLAR_GAP_IN <- 0.14      # white space between the framed panels of their composites

g_double_angle_plot <- function(mag, axisdeg, gl, glv, pal, title,
                                unit_lab = "Diopters", ellipse = "normal",
                                show_centroid_ellipse = TRUE, jcrs = TRUE,
                                test_method = "robust", fig_w = DA_W, fig_h = DA_H) {
  m <- abs(gr_num(mag)); a <- gr_num(axisdeg) %% 180
  ok <- is.finite(m) & is.finite(a) & !is.na(gl) & gl %in% glv
  if (sum(ok) < 3) stop("too few eyes")
  cid0 <- gr_cluster_of(gl)
  did <- if (is.null(cid0)) NULL else cid0[ok]     # aligned with d, below
  w0 <- gr_weights_of(gl)
  dw <- if (is.null(w0)) NULL else w0[ok]          # matching weights, aligned with d
  d <- data.frame(x = m[ok] * cos(2 * a[ok] * pi / 180),
                  y = m[ok] * sin(2 * a[ok] * pi / 180),
                  grp = factor(as.character(gl[ok]), levels = glv))
  present <- glv[glv %in% levels(droplevels(d$grp))]

  rmax <- max(sqrt(d$x^2 + d$y^2), 0.5, na.rm = TRUE)
  rings <- pretty(c(0, rmax), 4); rings <- rings[rings > 0]
  rmax <- max(rings)
  th <- seq(0, 2 * pi, length.out = 241)

  circ <- do.call(rbind, lapply(rings, function(rr)
    data.frame(r = rr, x = rr * cos(th), y = rr * sin(th))))
  spk <- do.call(rbind, lapply(seq(0, 330, by = 30), function(s)
    data.frame(a = s, x = c(0, rmax * cos(s * pi / 180)), y = c(0, rmax * sin(s * pi / 180)))))
  # spokes are labelled with the MERIDIAN (half the displayed angle), so the
  # reader never has to double anything in their head
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
  slices <- lapply(seq(0, 330, by = 30), function(s)
    wedge(s, s + 30, if ((s / 30) %% 2 == 0) "grey93" else "white"))

  p <- Reduce(`+`, slices, ggplot2::ggplot()) +
    ggplot2::geom_path(data = circ, ggplot2::aes(x, y, group = r),
                       colour = "grey30", linewidth = .38) +
    ggplot2::geom_line(data = spk, ggplot2::aes(x, y, group = a),
                       colour = "grey62", linewidth = .28) +
    ggplot2::geom_text(data = labd, ggplot2::aes(x, y, label = lab),
                       size = 2.9, colour = "grey15", fontface = "bold") +
    # magnitude scale runs straight DOWN, clear of every meridian label; drawn on
    # a white plate so it stays legible over whichever slice it crosses
    ggplot2::annotate("label", x = rmax * 0.045, y = -rings, label = rings,
                      size = 3.0, colour = "black", fontface = "bold", hjust = 0,
                      fill = "white", linewidth = .18, label.r = grid::unit(1, "pt"),
                      label.padding = grid::unit(1.3, "pt")) +
    ggplot2::geom_point(data = d, ggplot2::aes(x, y, colour = grp),
                        size = 1.5, alpha = .5)

  cent <- list()
  inside <- character(0)       # "kept/n" per group, read back from the polygon drawn
  for (g in present) {
    dg <- d[d$grp == g, , drop = FALSE]
    gc <- unname(pal[g])
    for (kind in if (identical(ellipse, "both")) c("normal", "polygon") else ellipse) {
      e1 <- astig_region(dg$x, dg$y, kind, scale = 1)
      if (!is.null(e1) && !is.null(attr(e1, "kept")))
        inside[g] <- sprintf("%d/%d", attr(e1, "kept"), attr(e1, "n"))
      else if (kind == "polygon")             # a hull needs three eyes
        inside[g] <- sprintf("none (n = %d)", nrow(dg))
      if (!is.null(e1))
        p <- p + ggplot2::annotate("path", x = e1$x, y = e1$y, colour = gc,
                                   linewidth = .55,
                                   linetype = if (identical(ellipse, "both") &&
                                                  kind == "polygon") "22" else "solid")
    }
    cent[[g]] <- data.frame(x = mean(dg$x), y = mean(dg$y), grp = g,
                            n = nrow(dg), stringsAsFactors = FALSE)
  }
  cd <- do.call(rbind, cent)
  p <- p +
    ggplot2::geom_point(data = cd, ggplot2::aes(x, y, fill = grp), shape = 23,
                        size = 3.0, stroke = .7, colour = "grey10")
  # the 95% ellipse OF THE CENTROID goes on last: at large n it is smaller than
  # the marker (the centroid is precisely estimated), and it must not be buried.
  # Clustered by patient exactly when the printed centroid test is (a cluster id
  # is in scope and the test is not the unclustered "classic" one), so the
  # region and the P beside it answer the same question; a region that would
  # spill past the plotted rings (a handful of patients) is left out and named
  # in the footer instead of drawn off the plot.
  ci_id <- if (isTRUE(jcrs) && identical(test_method, "classic")) NULL else did
  ci_basis <- character(0)
  ci_skip <- character(0)      # why a group's region is not drawn, named by group
  if (isTRUE(show_centroid_ellipse)) {
    for (g in present) {
      k <- d$grp == g
      dg <- d[k, , drop = FALSE]
      idg <- if (is.null(ci_id)) NULL else ci_id[k]
      e2 <- astig_region(dg$x, dg$y, "normal", centroid = TRUE, id = idg)
      if (is.null(e2)) { ci_skip[g] <- centroid_skip_why(dg$x, dg$y, idg); next }
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
    ggplot2::theme_void(base_size = 12) +
    ggplot2::theme(legend.position = "none",
                   axis.title.x = ggplot2::element_text(face = "bold", size = 10.5),
                   plot.background = ggplot2::element_rect(fill = "white", colour = "grey25",
                                                           linewidth = .8))
  # header: title, then per-group centroid + spread, then the omnibus test on BOTH
  # doubled-angle components (a centroid can differ in x, in y, or in both).
  # The title lives INSIDE the header block; a ggplot title would be drawn in the
  # same margin rows and overprint it.
  # ---- statistics -----------------------------------------------------------
  # JCRS 2025 Part II asks for a BIVARIATE comparison of the centroids; without
  # it we fall back to the historical pair of separate univariate tests on x and y.
  if (isTRUE(jcrs)) {
    rob <- identical(test_method, "robust")
    if (rob) {
      pj <- astig_robust_biv_p(d$x, d$y, d$grp, id = did)
      # with two cohorts the single "vs reference" test IS the omnibus test (same
      # statistic, same seed): it is neither recomputed nor printed twice
      vs <- NULL
      if (length(present) > 2) {
        pv <- vapply(present[-1], function(o) {
          k <- as.character(d$grp) %in% c(present[1], o)
          astig_robust_biv_p(d$x[k], d$y[k], d$grp[k],
                             id = if (is.null(did)) NULL else did[k]) }, numeric(1))
        vs <- stats::setNames(stats::p.adjust(pv, "holm"), present[-1])
      }
    } else {
      # the patient id enters only with the "gee" method; "classic" is the
      # unclustered Hotelling / Pillai test (weighted GEE on a weighted cohort)
      use_id <- if (identical(test_method, "gee")) did else NULL
      pj <- astig_bivariate_p(d$x, d$y, d$grp, id = use_id, w = dw)
      vs <- if (length(present) > 2)
        astig_vs_ref(d$x, d$y, as.character(d$grp), present, holm = TRUE,
                     id = use_id, w = dw) else NULL
    }
    # "P = 0.262" / "P < 0.001"; an untestable comparison prints "P = n/a",
    # never "P = NA" (nor "P = <0.001")
    fp <- function(p) if (!is.finite(p)) "P = n/a" else if (p < 0.001) "P < 0.001"
                      else paste0("P = ", fmt_p(p))
    # the label names the test that RAN: a clustered GEE that could not be
    # fitted fell back to the unclustered Hotelling / Pillai test
    fell_back <- !rob && isTRUE(attr(pj, "fallback"))
    tlab <- if (rob) sprintf("robust %d%%-trimmed T², B = %d, %s",
                             as.integer(round(ASTIG_TRIM * 100)), ASTIG_BOOT_B,
                             if (!is.null(did)) "cluster bootstrap" else "bootstrap")
            else switch(attr(pj, "test") %||% "",
                        "weighted GEE" = "weighted bivariate GEE",
                        "clustered GEE" = "clustered bivariate GEE",
                        "Hotelling" = "Hotelling T²", "Pillai" = "Pillai MANOVA",
                        if (!is.null(dw)) "weighted bivariate GEE"
                        else if (length(present) == 2) "Hotelling T²" else "Pillai MANOVA")
    if (fell_back) tlab <- paste0(tlab, ", GEE not fitted")
    why_pj <- attr(pj, "why") %||% ""
    fb_vs <- attr(vs, "fallback") %||% character(0)
    stat_lines <- c(
      # say "P =" explicitly - a bare "Centroids: 0.26" reads as a distance in D;
      # no P from a group that cannot support one, and the line says which
      if (!is.finite(pj) && nzchar(why_pj)) paste0("Centroid comparison: ", p_not_tested(why_pj))
      else paste0("Centroid comparison: ", fp(pj), "  [", tlab, "]"),
      # 3+ cohorts only (with two, the omnibus line above is that comparison);
      # [Holm] only when two or more comparisons were actually adjusted
      if (!is.null(vs) && length(vs))
        paste0("vs ", present[1], ": ",
               paste(sprintf("%s %s%s", names(vs), vapply(vs, fp, character(1)),
                             ifelse(names(vs) %in% fb_vs, " (GEE not fitted)", "")),
                     collapse = "  ·  "),
               if (sum(is.finite(vs)) >= 2) "  [Holm]" else "") else NULL)
    # a GEE P value (clustered or weighted) with a group of few patients: the
    # caution line (the header is fitted to the frame, so it always fits)
    if (!rob) {
      cau <- few_patients_text(gr_few_merge(if (is.finite(pj)) attr(pj, "few"), attr(vs, "few")),
                               short = TRUE)
      if (nzchar(cau)) stat_lines <- c(stat_lines, cau)
    } else if (!is.null(did)) {
      # the patient bootstrap is a clustered comparison too: a compared group of
      # 2 to 9 patients gets its own caution (contract R4-6), naming only the
      # groups behind a P value actually printed - every group in the omnibus
      # test, the reference and that group in a "vs" comparison
      cmp <- unique(c(if (is.finite(pj)) present,
                      if (!is.null(vs) && any(is.finite(vs))) c(present[1], names(vs)[is.finite(vs)])))
      if (length(cmp)) {
        kk <- as.character(d$grp) %in% cmp
        cau <- few_patients_text(few_patient_groups(did[kk], droplevels(d$grp[kk])),
                                 bootstrap = TRUE)
        if (nzchar(cau)) stat_lines <- c(stat_lines, cau)
      }
    }
    # the robust test is cluster-aware but is not the exact Part II statistic:
    # the line says so rather than "Per JCRS 2025 Part II"
    meth <- paste0(if (rob) "JCRS 2025 Part II plot  ·  " else "Per JCRS 2025 Part II  ·  ",
                   if (rob && !is.null(did))
                     "robust trimmed test (not the exact Part II variant), bootstrap over patients"
                   else if (rob) "robust trimmed test (not the exact Part II variant), bootstrap (eyes independent)"
                   else if (!is.null(dw))
                     paste0("matching weights (weighted GEE",
                            if (!is.null(use_id)) ", clustered by patient" else "", ")")
                   else if (!is.null(use_id) && fell_back)
                     "eyes treated as independent (the clustered GEE could not be fitted)"
                   else if (!is.null(use_id)) "clustered by patient (GEE)"
                   else "eyes treated as independent")
    # the trimmed-mean bootstrap has no weighted form: say so on a matched cohort
    if (rob && !is.null(dw))
      meth <- c(meth, "Matched cohort: the robust test does not use the matching weights")
  } else {
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
  reg <- if (identical(ellipse, "polygon")) "95% convex polygon (JCRS)"
         else if (identical(ellipse, "both")) "95% ellipse + polygon"
         else "95% normal ellipse (assumes Gaussian)"
  # the eyes each polygon is the hull of, read from the polygons drawn, so the
  # label can never disagree with the drawing; each entry is named by its group,
  # and a group too small for a hull says so ("C none (n = 1)") rather than drop
  # out of the list (an excluded eye that shares the coordinates of a kept one
  # lies on the edge). One line: gr_polar_finish breaks it at its commas to fit.
  inside_lines <- if (length(inside)) {
    gi <- present[present %in% names(inside)]
    paste0("Polygon = hull of the deepest eyes (hull peeling): ",
           paste(sprintf("%s %s", gi, inside[gi]), collapse = ", "))
  } else character(0)
  # the dashed region's basis, as drawn (Hotelling F radius either way). It is
  # the region of the plain MEAN centroid (the one in the group lines), which
  # the robust test - comparing 20%-trimmed centroids - does not use
  ci_txt <- if (!isTRUE(show_centroid_ellipse)) ""
            else paste0("  ·  dashed = 95% CI of ",
                        if (isTRUE(jcrs) && identical(test_method, "robust"))
                          "the untrimmed centroid" else "centroid",
                        if (!length(ci_basis)) ""
                        else if (all(ci_basis == "clustered")) " (clustered by patient)"
                        else " (eyes independent)")
  # each group left out, with its own reason
  ci_note <- if (isTRUE(show_centroid_ellipse) && length(ci_skip))
    paste0("Centroid CI not drawn: ",
           paste(sprintf("%s (%s)", names(ci_skip), ci_skip), collapse = ", "))
    else character(0)
  foot <- c(paste0(reg, ci_txt), ci_note, inside_lines, stat_lines, meth)
  terse <- length(present) > 5      # two narrow columns: the long form will not fit
  hs <- hdr_groups(present, pal, function(g) {
    dg <- d[d$grp == g, , drop = FALSE]
    cm <- sqrt(mean(dg$x)^2 + mean(dg$y)^2)
    ca <- (atan2(mean(dg$y), mean(dg$x)) * 180 / pi / 2) %% 180
    mm <- sqrt(dg$x^2 + dg$y^2)
    # total SD = sqrt(var x + var y): invariant to rotation/translation of the
    # coordinate frame, and a required JCRS 2025 summary statistic
    # (an SD needs two eyes: a one-eye group prints "n/a", never "NA")
    tsd <- if (nrow(dg) > 1) sqrt(stats::var(dg$x) + stats::var(dg$y)) else NA_real_
    msd_ <- if (nrow(dg) > 1) stats::sd(mm) else NA_real_
    if (terse) sprintf("%.2f D @ %.0f°, SD %s, n = %d", cm, ca, f2(tsd), nrow(dg))
    # single-space separators: the double-spaced version ran 1-2 characters past
    # the right edge with 3 groups, clipping the last digit of n
    else sprintf(paste0("centroid %.2f D @ %.0f° · SD %s · mean %.2f ± %s · ",
                        "med %.2f · RMS %.2f D · n = %d"),
                 cm, ca, f2(tsd), mean(mm), f2(msd_),
                 stats::median(mm), sqrt(mean(mm^2)), nrow(dg))
  }, foot = foot, title = title)
  # top margin sized from the header actually built (title + groups + footer
  # lines, as wrapped to the frame), so the bottom-anchored block never pushes
  # the title off the canvas; fig_w / fig_h are the size the plot is saved at
  gr_polar_finish(p, hs, xlim = c(-rmax * 1.24, rmax * 1.24),
                  ylim = c(-rmax * 1.24, rmax * 1.24), fig_w = fig_w, fig_h = fig_h)
}

# ============================================================
# AstigMATIC-style semicircle, restyled: background in GREY shades so colour
# carries group identity only, and every group superimposed on one diagram.
# Single-angle (meridian as drawn) - kept for familiarity; the double-angle plot
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
  # alternating 30-degree slices; the middle pair (60-120) is a step darker so the
  # with-the-rule zone still reads as a band, as it did in the original diagram
  SEMI_FILL <- c("grey93", "white", "grey86", "grey86", "white", "grey93")
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
    dx <- dg$m * cos(2 * dg$a * pi / 180); dy <- dg$m * sin(2 * dg$a * pi / 180)
    cm <- sqrt(mean(dx)^2 + mean(dy)^2)
    ca <- (atan2(mean(dy), mean(dx)) * 180 / pi / 2) %% 180
    cent[[g]] <- data.frame(x = cm * cos(ca * pi / 180), y = cm * sin(ca * pi / 180),
                            grp = g, cm = cm, ca = ca, n = nrow(dg),
                            sdx = stats::sd(dx), sdy = stats::sd(dy),
                            am = mean(dg$m), asd = stats::sd(dg$m),
                            stringsAsFactors = FALSE)
  }
  cd <- do.call(rbind, cent)
  p <- p +
    ggplot2::geom_point(data = cd, ggplot2::aes(x, y, fill = grp), shape = 23,
                        size = 3.4, stroke = .7, colour = "grey10") +
    ggplot2::scale_colour_manual(values = pal[present], drop = FALSE) +
    ggplot2::scale_fill_manual(values = pal[present], drop = FALSE) +
    ggplot2::geom_text(data = labd, ggplot2::aes(x, y, label = paste0(a, "°")),
                       size = 2.9, colour = "grey15", fontface = "bold") +
    ggplot2::annotate("text", x = c(-rings, rings), y = -rmax * 0.055,
                      label = c(rings, rings), size = 3.0, colour = "black",
                      fontface = "bold") +
    ggplot2::annotate("point", x = 0, y = 0, shape = 25, fill = "black",
                      colour = "black", size = 2.6) +
    ggplot2::labs(title = NULL, x = unit_lab, y = NULL) +
    ggplot2::theme_void(base_size = 12) +
    ggplot2::theme(legend.position = "none",
                   axis.title.x = ggplot2::element_text(face = "bold", size = 10.5),
                   plot.background = ggplot2::element_rect(fill = "white", colour = "grey25",
                                                           linewidth = .8))
  if (!is.null(arc_at) && arc_at <= rmax)
    p <- p + ggplot2::annotate("path", x = arc_at * cos(th_s), y = arc_at * sin(th_s),
                               colour = "grey25", linewidth = .3)
  terse <- length(present) > 5      # two narrow columns: the long form will not fit
  hs <- hdr_groups(present, pal, function(g) {
    r <- cd[cd$grp == g, ]
    if (terse) sprintf("◆ %.2f D Ax %.0f°, n = %d", r$cm, r$ca, r$n)
    else sprintf("◆ %.2f D Ax %.0f°  ·  mean %.2f ± %s  ·  n = %d",
                 r$cm, r$ca, r$am, f2(r$asd), r$n)
  }, foot = "◆ vector mean, computed in double-angle space", title = title)
  # header wrapped to the frame and top margin sized from it (see g_double_angle_plot)
  gr_polar_finish(p, hs, xlim = c(-rmax * 1.16, rmax * 1.16),
                  ylim = c(-rmax * 0.17, rmax * 1.12), fig_w = fig_w, fig_h = fig_h)
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

compose_panels <- function(plots, captions, base, formats, dpi, ncol = 3,
                           pw = 5.6, ph = 4.9, cap_h = 0.42, rows_spec = NULL,
                           sections = NULL, note = NULL, preview_dir = NULL,
                           logger = NULL, gap_in = 0) {
  n <- length(plots); if (n == 0) return(character(0))
  rows_spec <- compose_rows(n, ncol, rows_spec)
  nr <- length(rows_spec)
  hh_in  <- if (!is.null(sections)) 0.55 else 0            # section header band
  cap_in <- if (is.null(captions)) 0.06 else cap_h         # caption band
  gap_in <- max(as.numeric(gap_in %||% 0)[1], 0, na.rm = TRUE)
  lab_in <- PANEL_LETTER_IN                                # panel-letter strips
  band_in <- hh_in + lab_in + ph + gap_in + cap_in
  note <- if (is.null(note)) character(0) else note[nzchar(note)]
  note_in <- if (length(note)) 0.30 + 0.22 * length(note) else 0   # note band
  # a row of k panels gives each a 1/k share of W: k letter strips are added
  # for the fullest row, so its panels keep the width they had without them
  W <- ncol * (pw + gap_in) + max(rows_spec) * lab_in; H <- nr * band_in + note_in
  hr <- 1 / nr
  fh <- hh_in / band_in; fp <- (ph + gap_in) / band_in; fc <- cap_in / band_in
  written <- character(0)
  if (length(formats) == 0) formats <- "tiff"
  draw <- function() {
    grid::grid.newpage()
    # the panel rows fill the area above the note band; every position below
    # is a fraction of that area
    grid::pushViewport(grid::viewport(y = 1, height = (H - note_in) / H, just = "top"))
    i <- 0
    for (r in seq_len(nr)) {
      k <- rows_spec[r]
      band_bot <- 1 - r * hr; band_top <- 1 - (r - 1) * hr
      for (j in seq_len(k)) {
        i <- i + 1; if (i > n) break
        xc <- (j - 0.5) / k
        # the panel's slot less the gap and the letter strip on its left
        # (pw x ph when the row is full); the strip above it is in the band
        p_left <- grid::unit((j - 1) / k, "npc") + grid::unit(gap_in / 2 + lab_in, "in")
        p_top <- grid::unit(band_bot + hr * (fc + fp), "npc") - grid::unit(gap_in / 2, "in")
        vp_p <- grid::viewport(x = p_left, y = band_bot + hr * (fc + fp / 2), just = c("left", "centre"),
                               width = grid::unit(1 / k, "npc") - grid::unit(gap_in + lab_in, "in"),
                               height = grid::unit(hr * fp, "npc") - grid::unit(gap_in, "in"))
        if (inherits(plots[[i]], "gtable") || inherits(plots[[i]], "grob")) {
          grid::pushViewport(vp_p); grid::grid.draw(plots[[i]]); grid::popViewport()
        } else print(plots[[i]], vp = vp_p)
        # the letter above-left of the panel's top-left corner, in its strips
        grid::grid.text(LETTERS[i], x = p_left - grid::unit(lab_in - 0.03, "in"),
                        y = p_top + grid::unit(0.03, "in"),
                        just = c("left", "bottom"), gp = grid::gpar(fontface = "bold", cex = 1.9))
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
generate_standard_graphs <- function(df, cfg, res, out_dir, tag = "", logger = message) {
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
  .gr_opts$n_gee_fallback <- 0L
  on.exit({ .gr_opts$cluster_id <- NULL; .gr_opts$weights <- NULL }, add = TRUE)
  formats <- gr_normalise_formats(cfg$graph_formats %||% list("png"), logger)
  dpi <- as.numeric(cfg$graph_dpi %||% 300)
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
    logger("StandardEyes figures: p-values clustered by patient (GEE, robust SE) - both eyes of a patient count once.")
  } else {
    .gr_opts$cluster_id <- NULL
    if (cluster_by_pt)
      logger("StandardEyes figures: eyes treated as independent - set the patient/cluster ID column to cluster the p-values.")
  }
  # Matched pass: MatchIt's matching weights. When any differs from 1 (k:1
  # matching, replacement, a caliper on a limited pool) every figure P value is
  # a weighted GEE Wald test, as in the matched summary tables; bars, means and
  # N boxes show the matched eyes unweighted, and the composite says so. A
  # column that merely happens to be called "weights" in an unmatched cohort is
  # never read as matching weights.
  w_note <- NULL
  is_matched <- isTRUE(startsWith(as.character(tag)[1], "_Matched_")) || "Pairing" %in% names(df)
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
  plan <- character(0); cell_w <- numeric(0); planning <- TRUE
  fail_why <- character(0)       # why a panel could not be drawn, by panel name
  addC <- function(nm, cap, expr, section = "") {
    if (planning) { plan[[nm]] <<- section; return(invisible(NULL)) }
    .gr_opts$cell_w <- if (nm %in% names(cell_w)) cell_w[[nm]] else GR_BAR_MIN_W
    on.exit(.gr_opts$cell_w <- NULL, add = TRUE)
    p <- tryCatch(expr, error = function(e) {
      fail_why[[nm]] <<- conditionMessage(e)
      logger(paste0("StandardEyes figure '", nm, "' failed", run_lab, ": ", conditionMessage(e))); NULL })
    if (!is.null(p)) {
      if (inherits(p, "ggplot")) p <- p + ggplot2::labs(title = NULL)
      written <<- c(written, save_graph(p, file.path(out_dir, paste0("StandardEyes_", nm, tg)),
                                        formats, dpi, logger = logger))
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
      addC("02_Efficacy", "Uncorrected vs. Corrected Distance Visual Acuity",
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
  cell_w <- stats::setNames(pmin(GR_PANEL_FILE_W, if (length(plan) >= 2)
    compose_panel_w(length(plan), rows_spec = build_sections(unname(plan))$rows_spec)
    else rep(GR_PANEL_FILE_W, length(plan))), names(plan))
  planning <- FALSE
  draw_panels()
  if (length(cp) >= 2) {
    lay <- build_sections(cs)
    written <- c(written, compose_panels(cp,
      captions = if (is.null(lay$sections)) cc_ else NULL,
      base = file.path(out_dir, paste0("StandardEyes_AllGroups", tg)),
      formats = formats, dpi = dpi,
      rows_spec = lay$rows_spec, sections = lay$sections,
      note = w_note, preview_dir = preview_dir(), logger = logger))
  } else if (length(cp) == 1) {
    preview_single(cp[[1]], paste0("StandardEyes_", cn[1], tg), 6.5, 5.4)
    logger(sprintf("StandardEyes figures%s: only one panel could be drawn (%s) - no composite figure, which needs two or more.",
                   run_lab, cn[1]))
  }

  # ---- astigmatism: JCRS double-angle + the restyled grey semicircle ----
  # Gated only by their own switches: "astig_panels" governs the composite's
  # astigmatism panels, not these standalone figures.
  da_on <- isTRUE(cfg$double_angle_figs %||% TRUE)
  sc_on <- isTRUE(cfg$semicircle_figs %||% TRUE)
  if (!is.null(cav) && (da_on || sc_on)) {
    ell <- as.character(cfg$astig_region %||% "polygon")
    jcrs_astig <- isTRUE(cfg$astig_jcrs_standard %||% TRUE)
    astig_meth <- as.character(cfg$astig_test_method %||% "robust")
    if (!astig_meth %in% c("robust", "gee", "classic")) astig_meth <- "robust"
    keep <- is.finite(cav$tia) & cav$tia >= 0.25       # CI is meaningless below this
    vecs <- list(
      list("TIA", "Target Induced Astigmatism", cav$tia, cav$tax, "Diopters", NULL),
      list("SIA", "Surgically Induced Astigmatism", cav$sia, cav$sax, "Diopters", NULL),
      list("DV",  "Difference Vector", cav$dv, cav$dax, "Diopters", NULL),
      list("CI",  "Correction Index", ifelse(keep, cav$sia / cav$tia, NA_real_),
           cav$sax, "Index", 1))
    dap <- list(); dac <- character(0); scp <- list(); scc <- character(0)
    for (v in vecs) {
      if (da_on) {
        if (jcrs_astig && identical(astig_meth, "robust")) {
          # the patient bootstrap is the slow step of a run: say what is running
          okv <- is.finite(abs(gr_num(v[[3]]))) & is.finite(gr_num(v[[4]])) &
                 !is.na(gl) & gl %in% glv
          kv <- length(unique(gl[okv]))
          if (kv >= 2)
            logger(sprintf("Astigmatism %s%s: robust centroid test, %d groups, %d eyes - B = %d bootstrap resamples per test ...",
                           v[[1]], if (nzchar(tag)) paste0(" (", sub("^_", "", tag), ")") else "",
                           kv, sum(okv), ASTIG_BOOT_B))
        }
        p <- tryCatch(g_double_angle_plot(v[[3]], v[[4]], gl, glv, pal,
                        paste0(v[[2]], " - double-angle"), unit_lab = v[[5]],
                        ellipse = ell, jcrs = jcrs_astig,
                        test_method = astig_meth, fig_w = DA_W, fig_h = DA_H),
                      error = function(e) { logger(paste0("StandardEyes double-angle '",
                        v[[1]], "' failed", run_lab, ": ", conditionMessage(e))); NULL })
        if (!is.null(p)) {
          written <- c(written, save_graph(p, file.path(out_dir,
            paste0("StandardEyes_DoubleAngle_", v[[1]], tg)), formats, dpi, w = DA_W, h = DA_H,
            logger = logger))
          dap[[length(dap) + 1]] <- p; dac <- c(dac, v[[1]])
        }
      }
      if (sc_on) {
        p <- tryCatch(g_astig_semicircle_multi(v[[3]], v[[4]], gl, glv, pal,
                        paste0(v[[2]], " - semicircle"), unit_lab = v[[5]], arc_at = v[[6]],
                        fig_w = SC_W, fig_h = SC_H),
                      error = function(e) { logger(paste0("StandardEyes semicircle '",
                        v[[1]], "' failed", run_lab, ": ", conditionMessage(e))); NULL })
        if (!is.null(p)) {
          written <- c(written, save_graph(p, file.path(out_dir,
            paste0("StandardEyes_Semicircle_", v[[1]], tg)), formats, dpi, w = SC_W, h = SC_H,
            logger = logger))
          scp[[length(scp) + 1]] <- p; scc <- c(scc, v[[1]])
        }
      }
    }
    if (length(dap) >= 2) {
      written <- c(written, compose_panels(dap, captions = NULL,
        base = file.path(out_dir, paste0("StandardEyes_DoubleAngle", tg)),
        formats = formats, dpi = dpi, ncol = 2, pw = DA_W, ph = DA_H, gap_in = POLAR_GAP_IN,
        preview_dir = preview_dir(), logger = logger))
    } else if (length(dap) == 1) {
      preview_single(dap[[1]], paste0("StandardEyes_DoubleAngle_", dac[1], tg), DA_W, DA_H)
    }
    if (length(scp) >= 2) {
      written <- c(written, compose_panels(scp, captions = NULL,
        base = file.path(out_dir, paste0("StandardEyes_Semicircle", tg)),
        formats = formats, dpi = dpi, ncol = 2, pw = SC_W, ph = SC_H, gap_in = POLAR_GAP_IN,
        preview_dir = preview_dir(), logger = logger))
    } else if (length(scp) == 1) {
      preview_single(scp[[1]], paste0("StandardEyes_Semicircle_", scc[1], tg), SC_W, SC_H)
    }
  }
  if (length(glv) > 5)
    logger(sprintf("StandardEyes figures: %d groups superimposed - readable up to about 5; consider subsets.",
                   length(glv)))
  .gr_opts$cluster_id <- NULL; .gr_opts$weights <- NULL     # reset on exit
  # every test whose clustered GEE was fitted but failed - the univariate panel
  # tests (p_omnibus) and the double-angle centroid tests (astig_bivariate_p) -
  # and whose P therefore comes from the unclustered test
  if ((.gr_opts$n_gee_fallback %||% 0L) > 0)
    logger(sprintf("StandardEyes figures%s: in %d test(s) the clustered GEE could not be fitted (no convergence, a numerically singular covariance, or a group of a 0/1 outcome without events); those P values come from the unclustered test and are labelled \"GEE not fitted\" on the figure.",
                   run_lab, .gr_opts$n_gee_fallback))
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
