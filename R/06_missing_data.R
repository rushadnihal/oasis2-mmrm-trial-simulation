# ------------------------------------------------------------------------------
# 06_missing_data.R
#
# Purpose : Missing-data analyses of SAP v1.0 Section 9.8 for the primary
#           endpoint in the primary analysis population (PAP):
#             - missingness pattern and baseline characteristics by whether
#               analysis visit 3 was observed (aggregate)
#             - multiple imputation under MAR, then delta adjustment: every
#               imputed change score is shifted by delta, delta = 0 to -1.0
#               percentage points in steps of 0.1 (D-007), and the results are
#               combined by Rubin's rules
#             - the smallest delta at which the estimate at each visit moves
#               by more than one standard error of the MAR (delta = 0)
#               estimate, or a statement that it does not within the grid
#           The primary MAR analysis is the MMRM (03_mmrm_primary.R); it is
#           refitted here only to show it next to the imputation results.
# Methods : Imputation by mice, Bayesian linear regression ("norm"), on one
#           row per participant: change at visit 2, change at visit 3,
#           centred baseline nWBV, age at baseline and education. m = 100
#           imputations, 20 iterations, seed 20261117. Each completed dataset
#           is analysed per visit by linear regression of change on the same
#           centred covariates, so the intercept is the mean change at the PAP
#           covariate means. Pooled by Rubin's rules with Barnard-Rubin
#           degrees of freedom.
# Inputs  : derived/analysis_long.rds in the data directory (OASIS2_DATA_DIR if
#           set, otherwise the working directory).
# Outputs : In the report directory (OASIS2_REPORT_DIR if set, otherwise
#           reports/ under the working directory), aggregate only:
#             06_missing_data_report.md, 06_delta_estimates.csv,
#             06_sessioninfo.txt
# Run     : from the repository root, Rscript R/06_missing_data.R
#           (needs mice, mmrm and emmeans)
# ------------------------------------------------------------------------------

suppressPackageStartupMessages({
  library(mice)
  library(mmrm)
  library(emmeans)
})
source("R/utils_mmrm.R")

M_IMP  <- 100
MAXIT  <- 20
SEED   <- 20261117
DELTAS <- round(seq(0, -1.0, by = -0.1), 1)

data_dir   <- Sys.getenv("OASIS2_DATA_DIR", unset = getwd())
report_dir <- Sys.getenv("OASIS2_REPORT_DIR", unset = file.path(getwd(), "reports"))
long_file  <- file.path(data_dir, "derived", "analysis_long.rds")
stopifnot("Run 02_build_analysis_data.R first" = file.exists(long_file))
dir.create(report_dir, recursive = TRUE, showWarnings = FALSE)

long <- readRDS(long_file)
sc <- function(n) ifelse(n == 0, "0", ifelse(n < 5, "<5", as.character(n)))
covs <- c("base_nwbv_c", "age_bl_c", "educ_c")

# Wide dataset: one row per PAP participant ----------------------------------------
pap  <- long[long$pap_fl, ]
base <- pap[pap$visit_label == 1, c("id", covs, "age_bl", "educ", "sex", "base_nwbv", "base_mmse")]
mm   <- pap[pap$mmrm_fl, c("id", "avisit", "chg_nwbv")]
wide <- base
wide$chg2 <- mm$chg_nwbv[match(paste(wide$id, 2), paste(mm$id, mm$avisit))]
wide$chg3 <- mm$chg_nwbv[match(paste(wide$id, 3), paste(mm$id, mm$avisit))]
stopifnot(nrow(wide) == length(unique(pap$id)), !any(is.na(wide[covs])))
n_pap <- nrow(wide)
miss2 <- is.na(wide$chg2); miss3 <- is.na(wide$chg3)

# Pattern and baseline by visit 3 status ---------------------------------------------
pattern_df <- data.frame(
  Pattern = c("Visit 2 and visit 3 observed", "Visit 2 only", "Visit 3 only", "Neither"),
  Participants = sc(c(sum(!miss2 & !miss3), sum(!miss2 & miss3), sum(miss2 & !miss3),
                      sum(miss2 & miss3))),
  stringsAsFactors = FALSE)

msd <- function(x) if (length(x) < 10) "-" else sprintf("%.2f (%.2f)", mean(x), sd(x))
by_v3 <- function(g) {
  x <- wide[g, ]
  c(sc(nrow(x)), msd(x$age_bl), msd(x$educ), sc(sum(x$sex == "F")),
    msd(x$base_nwbv),
    if (nrow(x) < 10) "-" else sprintf("%.0f (%.0f to %.0f)", median(x$base_mmse),
                                       quantile(x$base_mmse, 0.25), quantile(x$base_mmse, 0.75)))
}
bl_df <- data.frame(
  Characteristic = c("Participants", "Age at baseline, mean (SD)", "Education, years, mean (SD)",
                     "Female", "Baseline nWBV, pp, mean (SD)", "Baseline MMSE, median (IQR)"),
  `Visit 3 observed` = by_v3(!miss3), `Visit 3 missing` = by_v3(miss3),
  check.names = FALSE, stringsAsFactors = FALSE)

sd_obs <- c(sd(wide$chg2, na.rm = TRUE), sd(wide$chg3, na.rm = TRUE))

# Primary MMRM, for reference ----------------------------------------------------------
ad <- long[long$pap_fl & long$mmrm_fl, c("id", "avisit", "chg_nwbv", covs)]
ad$avisit <- factor(ad$avisit, levels = c(2, 3), labels = c("Visit 2", "Visit 3"))
ad$id <- factor(ad$id)
ad <- ad[order(ad$id, ad$avisit), ]
prim <- fit_with_fallback("avisit * base_nwbv_c + age_bl_c + educ_c", ad)
ls_prim <- if (!is.null(prim$fit)) ls_means(prim$fit) else NULL

# Multiple imputation under MAR -----------------------------------------------------------
imp_data <- wide[, c("chg2", "chg3", covs)]
meth <- make.method(imp_data)
meth[c("chg2", "chg3")] <- "norm"
meth[covs] <- ""
imp <- mice(imp_data, m = M_IMP, maxit = MAXIT, method = meth, seed = SEED, printFlag = FALSE)
completed <- lapply(seq_len(M_IMP), function(i) complete(imp, i))

pool_rubin <- function(est, var, dfcom) {
  m <- length(est); qbar <- mean(est); w <- mean(var); b <- var(est)
  t <- w + (1 + 1 / m) * b
  lambda <- (1 + 1 / m) * b / t
  df_old <- (m - 1) / lambda^2
  df_obs <- (dfcom + 1) / (dfcom + 3) * dfcom * (1 - lambda)
  df <- df_old * df_obs / (df_old + df_obs)
  se <- sqrt(t)
  c(estimate = qbar, se = se, df = df, lower = qbar - qt(0.975, df) * se,
    upper = qbar + qt(0.975, df) * se, fmi = lambda)
}

analyse_visit <- function(dat, y) {
  f <- lm(as.formula(paste(y, "~ base_nwbv_c + age_bl_c + educ_c")), data = dat)
  c(coef(f)[1], vcov(f)[1, 1])
}

res <- do.call(rbind, lapply(DELTAS, function(delta) {
  do.call(rbind, lapply(1:2, function(k) {
    y <- c("chg2", "chg3")[k]; miss <- list(miss2, miss3)[[k]]
    ev <- t(vapply(completed, function(d) {
      d[[y]][miss] <- d[[y]][miss] + delta
      analyse_visit(d, y)
    }, numeric(2)))
    p <- pool_rubin(ev[, 1], ev[, 2], dfcom = n_pap - 4)
    data.frame(delta = delta, visit = paste("Visit", k + 1), t(p), stringsAsFactors = FALSE)
  }))
}))
rownames(res) <- NULL
res$delta_sd <- res$delta / sd_obs[match(res$visit, c("Visit 2", "Visit 3"))]
write.csv(res, file.path(report_dir, "06_delta_estimates.csv"), row.names = FALSE)

# Tipping: smallest |delta| where the estimate moves by more than one MAR SE --------------
tip_lines <- vapply(c("Visit 2", "Visit 3"), function(v) {
  r  <- res[res$visit == v, ]
  r0 <- r[r$delta == 0, ]
  moved <- abs(r$estimate - r0$estimate) > r0$se
  if (any(moved)) {
    d <- r$delta[which(moved)[1]]
    sprintf("- %s: the estimate moves by more than one MAR SE (%.3f) at delta = %.1f (%.2f SD of observed change).",
            v, r0$se, d, r$delta_sd[which(moved)[1]])
  } else {
    sprintf("- %s: the estimate does not move by more than one MAR SE (%.3f) within the grid (largest shift moves it by %.3f).",
            v, r0$se, max(abs(r$estimate - r0$estimate)))
  }
}, character(1))

grid_df <- do.call(rbind, lapply(DELTAS, function(d) {
  r <- res[res$delta == d, ]
  data.frame(`Delta (pp)` = sprintf("%.1f", d),
             `Visit 2: change (95% CI)` = fmt_est_ci(r$estimate[1], r$lower[1], r$upper[1]),
             `Visit 2: delta in SD` = sprintf("%.2f", r$delta_sd[1]),
             `Visit 3: change (95% CI)` = fmt_est_ci(r$estimate[2], r$lower[2], r$upper[2]),
             `Visit 3: delta in SD` = sprintf("%.2f", r$delta_sd[2]),
             check.names = FALSE, stringsAsFactors = FALSE)
}))

r0 <- res[res$delta == 0, ]
compare_df <- data.frame(
  Analysis = c("Primary MMRM (MAR, likelihood)", "Multiple imputation (MAR, delta = 0)"),
  `Visit 2: change (95% CI)` = c(
    if (is.null(ls_prim)) "-" else fmt_est_ci(ls_prim$emmean[1], ls_prim$lower.CL[1], ls_prim$upper.CL[1]),
    fmt_est_ci(r0$estimate[1], r0$lower[1], r0$upper[1])),
  `Visit 3: change (95% CI)` = c(
    if (is.null(ls_prim)) "-" else fmt_est_ci(ls_prim$emmean[2], ls_prim$lower.CL[2], ls_prim$upper.CL[2]),
    fmt_est_ci(r0$estimate[2], r0$lower[2], r0$upper[2])),
  check.names = FALSE, stringsAsFactors = FALSE)

# Report --------------------------------------------------------------------------------
out <- c(
  "# Report 06: Missing data (SAP v1.0, Section 9.8)",
  "",
  paste0("- **Generated:** ", format(Sys.time(), "%Y-%m-%d %H:%M %Z"), " by `06_missing_data.R`"),
  "- **Population:** primary analysis population; response is change from baseline in nWBV (percentage points)",
  paste0("- **Imputation:** mice, Bayesian linear regression; m = ", M_IMP, ", ", MAXIT,
         " iterations, seed ", SEED, "; analysis by visit-wise regression on centred covariates; Rubin's rules with Barnard-Rubin df"),
  paste0("- **mice** ", packageVersion("mice"), ", **mmrm** ", packageVersion("mmrm"),
         ", **R** ", getRversion()),
  "",
  "## Missingness pattern", "",
  md_table(pattern_df), "",
  paste0("Missing at analysis visit 2: ", sc(sum(miss2)), " of ", n_pap,
         "; at analysis visit 3: ", sc(sum(miss3)), " of ", n_pap,
         ". The data do not distinguish withdrawal from the end of a participant's follow-up."), "",
  "## Baseline characteristics by whether analysis visit 3 was observed", "",
  md_table(bl_df), "",
  "## MAR: likelihood against multiple imputation", "",
  md_table(compare_df), "",
  paste0("Fraction of missing information at delta = 0: visit 2 ", sprintf("%.2f", r0$fmi[1]),
         ", visit 3 ", sprintf("%.2f", r0$fmi[2]), "."), "",
  "## Delta adjustment", "",
  paste0("Every imputed change score is shifted by delta before analysis. Observed SD of change: visit 2 ",
         sprintf("%.3f", sd_obs[1]), ", visit 3 ", sprintf("%.3f", sd_obs[2]),
         " percentage points; 'delta in SD' is delta divided by these."), "",
  md_table(grid_df), "",
  "### Where the estimate moves materially", "",
  tip_lines, "",
  "Material movement is defined in the SAP as a shift of more than one standard error of the MAR estimate. Negative delta means participants with missing values are assumed to have declined more than MAR predicts.", ""
)
writeLines(out, file.path(report_dir, "06_missing_data_report.md"))
writeLines(capture.output(sessionInfo()), file.path(report_dir, "06_sessioninfo.txt"))

cat(out, sep = "\n")
cat("\nReport written to", file.path(report_dir, "06_missing_data_report.md"), "\n")
