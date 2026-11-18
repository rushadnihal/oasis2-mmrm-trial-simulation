# ------------------------------------------------------------------------------
# 11_results_export.R
#
# Purpose : Assemble the Part A results as publication-ready aggregate tables
#           (SAP v1.0 Section 13: T1 to T8) for the figures, the report and
#           the README. Also computes the supplementary SAS-equivalent 95% CI
#           for the primary estimate (D-014: vcov = "Kenward-Roger-Linear").
#           Small-cell rule applied for publication: no count below 5, and no
#           count from which one below 5 can be recovered by subtraction.
# Inputs  : derived/analysis_long.rds in the data directory (OASIS2_DATA_DIR)
#           04_robustness_estimates.csv, 05_secondary_estimates.csv and
#           06_delta_estimates.csv in the report directory (OASIS2_REPORT_DIR)
# Outputs : CSV tables in <OASIS2_REPORT_DIR>/partA_results/ (aggregate only):
#             t1_flow, t2_baseline, t3_visits, t3b_patterns, t4_primary,
#             t5_covariance, t6_robustness, t6b_continuous_slope, t7_delta,
#             t8_mmse, t8b_cdr
# Run     : from the repository root, Rscript R/11_results_export.R
# ------------------------------------------------------------------------------

suppressPackageStartupMessages({
  library(mmrm)
  library(emmeans)
  library(nlme)
})
source("R/utils_mmrm.R")

data_dir   <- Sys.getenv("OASIS2_DATA_DIR", unset = getwd())
report_dir <- Sys.getenv("OASIS2_REPORT_DIR", unset = file.path(getwd(), "reports"))
out_dir    <- file.path(report_dir, "partA_results")
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
long <- readRDS(file.path(data_dir, "derived", "analysis_long.rds"))
need <- file.path(report_dir, c("04_robustness_estimates.csv", "05_secondary_estimates.csv",
                                "06_delta_estimates.csv"))
stopifnot("Run scripts 04, 05 and 06 first" = all(file.exists(need)))
save_csv <- function(x, name) write.csv(x, file.path(out_dir, paste0(name, ".csv")), row.names = FALSE)
check_cells <- function(x) {
  n <- unlist(x[vapply(x, is.numeric, logical(1)) & grepl("^n", names(x))])
  if (any(n > 0 & n < 5, na.rm = TRUE)) stop("Small cell found in a published count")
  x
}

pap  <- long[long$pap_fl, ]
base <- pap[pap$visit_label == 1, ]
n_pap <- nrow(base)
mm   <- pap[pap$mmrm_fl, ]

# T1 participant flow ---------------------------------------------------------------
n_all  <- length(unique(long$id))
n_sup  <- length(unique(long$id[long$supp_fl]))
excluded_sup <- n_all - n_sup
t1 <- data.frame(
  step = c("All participants", "CDR 0 at baseline (primary analysis population)",
           "Primary population with a session in analysis window 2",
           "Primary population with a session in analysis window 3",
           "Supportive population (all baseline CDR, a session in either window)"),
  n = c(n_all, n_pap, sum(mm$avisit == 2), sum(mm$avisit == 3),
        if (excluded_sup == 0 || excluded_sup >= 5) n_sup else NA),
  note = c("", "", "", "",
           if (excluded_sup > 0 && excluded_sup < 5) "fewer than 5 participants excluded (count suppressed)" else ""),
  stringsAsFactors = FALSE)
save_csv(check_cells(t1), "t1_flow")

# T2 baseline characteristics (primary population) -------------------------------------
q <- function(x) quantile(x, c(0.25, 0.5, 0.75), na.rm = TRUE)
t2 <- data.frame(
  characteristic = c("Participants", "Age, years", "Education, years", "Female",
                     "nWBV, percentage points", "MMSE", "MMSE at 30"),
  summary = c("n", "mean (SD)", "mean (SD)", "n (%)", "mean (SD)", "median (IQR)", "n (%)"),
  value = c(
    n_pap,
    sprintf("%.1f (%.1f)", mean(base$age_bl), sd(base$age_bl)),
    sprintf("%.1f (%.1f)", mean(base$educ), sd(base$educ)),
    sprintf("%d (%.0f%%)", sum(base$sex == "F"), 100 * mean(base$sex == "F")),
    sprintf("%.2f (%.2f)", mean(base$base_nwbv), sd(base$base_nwbv)),
    sprintf("%.0f (%.0f to %.0f)", q(base$base_mmse)[2], q(base$base_mmse)[1], q(base$base_mmse)[3]),
    sprintf("%d (%.0f%%)", sum(base$base_mmse == 30, na.rm = TRUE),
            100 * mean(base$base_mmse == 30, na.rm = TRUE))),
  stringsAsFactors = FALSE)
stopifnot(sum(base$sex == "F") >= 5, sum(base$sex == "M") >= 5)
save_csv(t2, "t2_baseline")

# T3 analysis visits and patterns ---------------------------------------------------------
t3 <- do.call(rbind, lapply(2:3, function(k) {
  y <- mm$years_since_bl[mm$avisit == k]
  data.frame(visit = k, window_years = c("1 to under 3", "3 to under 5")[k - 1],
             n = length(y), percent_of_pap = round(100 * length(y) / n_pap),
             median_years = round(median(y), 2), q1_years = round(q(y)[[1]], 2),
             q3_years = round(q(y)[[3]], 2))
}))
save_csv(check_cells(t3), "t3_visits")
ids2 <- unique(mm$id[mm$avisit == 2]); ids3 <- unique(mm$id[mm$avisit == 3])
t3b <- data.frame(pattern = c("Both analysis visits", "Visit 2 only", "Visit 3 only"),
                  n = c(length(intersect(ids2, ids3)), length(setdiff(ids2, ids3)),
                        length(setdiff(ids3, ids2))))
save_csv(check_cells(t3b), "t3b_patterns")

# T4 primary MMRM, with the SAS-equivalent CI (D-014) ---------------------------------------
ad <- mm[, c("id", "avisit", "chg_nwbv", "base_nwbv_c", "age_bl_c", "educ_c")]
ad$avisit <- factor(ad$avisit, levels = c(2, 3), labels = c("Visit 2", "Visit 3"))
ad$id <- factor(ad$id)
ad <- ad[order(ad$id, ad$avisit), ]
f_us <- chg_nwbv ~ avisit * base_nwbv_c + age_bl_c + educ_c + us(avisit | id)
f_cs <- chg_nwbv ~ avisit * base_nwbv_c + age_bl_c + educ_c + cs(avisit | id)
fit_a <- mmrm(f_us, data = ad, reml = TRUE, method = "Kenward-Roger")
fit_b <- mmrm(f_us, data = ad, reml = TRUE, method = "Kenward-Roger", vcov = "Kenward-Roger-Linear")
stopifnot(component(fit_a, "convergence") == 0, component(fit_b, "convergence") == 0)
la <- ls_means(fit_a); lb <- ls_means(fit_b)
t4 <- data.frame(visit = c(2, 3), n = as.integer(table(ad$avisit)),
                 estimate = la$emmean, se = la$SE, df = la$df, lower = la$lower.CL, upper = la$upper.CL,
                 se_sas_equivalent = lb$SE, df_sas_equivalent = lb$df,
                 lower_sas_equivalent = lb$lower.CL, upper_sas_equivalent = lb$upper.CL)
save_csv(check_cells(t4), "t4_primary")

# T5 covariance -------------------------------------------------------------------------------
fit_cs <- mmrm(f_cs, data = ad, reml = TRUE, method = "Kenward-Roger")
vc <- component(fit_a, "varcor")
lc <- ls_means(fit_cs)
t5 <- data.frame(
  structure = c("Unstructured", "Compound symmetry"),
  parameters = c(component(fit_a, "n_theta"), component(fit_cs, "n_theta")),
  aic = c(AIC(fit_a), AIC(fit_cs)), bic = c(BIC(fit_a), BIC(fit_cs)),
  estimate_v2 = c(la$emmean[1], lc$emmean[1]), estimate_v3 = c(la$emmean[2], lc$emmean[2]),
  var_v2 = c(vc[1, 1], NA), var_v3 = c(vc[2, 2], NA), cov_v2_v3 = c(vc[1, 2], NA),
  corr_v2_v3 = c(cov2cor(vc)[1, 2], NA))
save_csv(t5, "t5_covariance")

# T6 robustness (from Report 04) and continuous-time slope ---------------------------------------
t6 <- read.csv(need[1], stringsAsFactors = FALSE)
save_csv(t6, "t6_robustness")
ct <- pap[, c("id", "years_since_bl", "nwbv_pct", "age_bl_c", "educ_c")]
ct$id <- factor(ct$id)
fit_ct <- lme(nwbv_pct ~ years_since_bl * age_bl_c + educ_c, random = ~ years_since_bl | id,
              data = ct, method = "REML")
tt <- summary(fit_ct)$tTable["years_since_bl", ]
t6b <- data.frame(slope = tt[["Value"]], se = tt[["Std.Error"]], df = tt[["DF"]],
                  lower = tt[["Value"]] - qt(0.975, tt[["DF"]]) * tt[["Std.Error"]],
                  upper = tt[["Value"]] + qt(0.975, tt[["DF"]]) * tt[["Std.Error"]],
                  participants = length(unique(ct$id)), sessions = nrow(ct))
save_csv(t6b, "t6b_continuous_slope")

# T7 delta adjustment (from Report 06) ---------------------------------------------------------
save_csv(read.csv(need[3], stringsAsFactors = FALSE), "t7_delta")

# T8 secondary: MMSE and CDR progression --------------------------------------------------------
sec <- read.csv(need[2], stringsAsFactors = FALSE)
mmse <- sec[sec$analysis == "MMSE change", c("visit", "estimate", "se", "df", "lower", "upper")]
mmse$n <- as.integer(table(factor(mm$avisit[!is.na(mm$chg_mmse)], levels = 2:3)))
mmse$n_at_ceiling <- c(sum(mm$mmse[mm$avisit == 2] == 30, na.rm = TRUE),
                       sum(mm$mmse[mm$avisit == 3] == 30, na.rm = TRUE))
save_csv(check_cells(mmse), "t8_mmse")
wilson <- function(x, n, z = qnorm(0.975)) {
  p <- x / n; den <- 1 + z^2 / n; ctr <- (p + z^2 / (2 * n)) / den
  hw <- z * sqrt(p * (1 - p) / n + z^2 / (4 * n^2)) / den
  c(ctr - hw, ctr + hw)
}
t8b <- do.call(rbind, lapply(2:3, function(k) {
  x <- mm$cdr[mm$avisit == k]; n <- sum(!is.na(x)); e <- sum(x >= 0.5, na.rm = TRUE)
  ci <- wilson(e, n)
  data.frame(visit = k, n_observed = n, n_cdr_05_or_above = e,
             proportion = e / n, lower = ci[1], upper = ci[2])
}))
if (any(t8b$n_cdr_05_or_above < 5 | t8b$n_observed - t8b$n_cdr_05_or_above < 5)) {
  t8b[, c("n_cdr_05_or_above", "proportion", "lower", "upper")] <- NA
}
save_csv(t8b, "t8b_cdr")

cat("Tables written to", out_dir, ":\n")
print(list.files(out_dir))
