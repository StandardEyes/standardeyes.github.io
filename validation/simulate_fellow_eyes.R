# Type I error of an eye-level test and of the app's patient-clustered test when both eyes of a
# patient are analysed and there is NO true difference between the groups.
#
#   Rscript simulate_fellow_eyes.R [replicates]      (default 5000; writes fellow_eye_simulation.csv beside it)
#
# Design. Two groups, the same number of patients in each, both eyes of every patient. The outcome is
# normal with unit variance; the two eyes of a patient share a patient effect so that their correlation
# is rho. The group is a property of the patient, so there is no group effect at all.
# Tests, at the 5% level:
#   eye-level  = Welch t-test on the eyes (stats::t.test), the test of the summary tables;
#   clustered  = the app's own figure test, gee_group_p() of R/graphs.R: a GEE with an exchangeable
#                working correlation and robust standard errors, clustered on the patient.
# The seed is fixed, so the same call gives the same table.
grDevices::pdf(NULL)
args <- commandArgs(trailingOnly = TRUE)
B <- if (length(args)) as.integer(args[1]) else 5000L
here <- dirname(normalizePath(sub("^--file=", "", grep("^--file=", commandArgs(), value = TRUE)[1])))
# the app folder: the parent of this script's folder (validation\ inside the source code), or the
# sibling "App - StandardEyes" when the script is run from the authors' tools folder
app <- if (file.exists(file.path(dirname(here), "R", "helpers.R"))) dirname(here) else file.path(dirname(here), "App - StandardEyes")
if (dir.exists(file.path(app, "runtime", "Rlib"))) .libPaths(c(file.path(app, "runtime", "Rlib"), .libPaths()))
setwd(app)
suppressPackageStartupMessages({ library(ggplot2); source("R/helpers.R"); source("R/analysis.R"); source("R/graphs.R") })

one <- function(n_pat, rho) {
  id <- rep(seq_len(2 * n_pat), each = 2)                       # two eyes per patient
  g <- rep(c("A", "B"), each = 2 * n_pat)                        # the first n_pat patients are group A
  y <- sqrt(rho) * rep(stats::rnorm(2 * n_pat), each = 2) + sqrt(1 - rho) * stats::rnorm(4 * n_pat)
  c(eye = stats::t.test(y ~ g)$p.value,
    clustered = as.numeric(gee_group_p(y, g, paste0("P", id), kind = "continuous")))
}

set.seed(20261003)
grid <- expand.grid(patients_per_group = c(30L, 60L, 120L), rho = c(0.5, 0.8))
out <- do.call(rbind, lapply(seq_len(nrow(grid)), function(i) {
  n <- grid$patients_per_group[i]; rho <- grid$rho[i]
  p <- t(replicate(B, one(n, rho)))
  ok <- is.finite(p[, "clustered"])
  data.frame(patients_per_group = n, eyes_per_group = 2L * n, inter_eye_correlation = rho, replicates = B,
             clustered_fitted = sum(ok),
             eye_level_rejection_pct = round(100 * mean(p[, "eye"] < 0.05), 1),
             clustered_rejection_pct = round(100 * mean(p[ok, "clustered"] < 0.05), 1))
}))
print(out, row.names = FALSE)
utils::write.csv(out, file.path(here, "fellow_eye_simulation.csv"), row.names = FALSE)
cat("written:", file.path(here, "fellow_eye_simulation.csv"), "\n")
