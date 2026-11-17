# ------------------------------------------------------------------------------
# 05_secondary_and_contrast.R
#
# Purpose : Secondary endpoints and the demonstration contrast, as
#           pre-specified in SAP v1.0:
#             9.6  MMSE: primary MMRM structure with change in MMSE as the
#                  response and baseline MMSE in place of baseline nWBV;
#                  counts at the ceiling (30) at baseline and each visit.
#                  CDR progression: proportion with CDR 0.5 or above at
#                  analysis visits 2 and 3 among those observed, Wilson 95% CI.
#             9.7  Demonstration contrast: primary MMRM with sex as a
#                  pseudo-arm and sex x visit. An OBSERVATIONAL ASSOCIATION,
#                  fitted only to exercise the between-group contrast for the
#                  SAS/R cross-check.
# Inputs  : derived/analysis_long.rds in the data directory (OASIS2_DATA_DIR if
#           set, otherwise the working directory).
# Outputs : In the report directory (OASIS2_REPORT_DIR if set, otherwise
#           reports/ under the working directory), aggregate only:
#             05_secondary_report.md, 05_secondary_estimates.csv,
#             05_sessioninfo.txt
# Privacy : Counts of 1 to 4 are shown as "<5", and no proportion or CI is
#           given for a numerator or its complement below 5.
# Run     : from the repository root, Rscript R/05_secondary_and_contrast.R
# ------------------------------------------------------------------------------

suppressPackageStartupMessages({
  library(mmrm)
  library(emmeans)
})
source("R/utils_mmrm.R")

data_dir   <- Sys.getenv("OASIS2_DATA_DIR", unset = getwd())
report_dir <- Sys.getenv("OASIS2_REPORT_DIR", unset = file.path(getwd(), "reports"))
long_file  <- file.path(data_dir, "derived", "analysis_long.rds")
stopifnot("Run 02_build_analysis_data.R first" = file.exists(long_file))
dir.create(report_dir, recursive = TRUE, showWarnings = FALSE)

long <- readRDS(long_file)
sc <- function(n) ifelse(n == 0, "0", ifelse(n < 5, "<5", as.character(n)))

prep <- function(rows, response, extra = character()) {
  ad <- long[rows, c("id", "avisit", response, "base_nwbv_c", "base_mmse_c",
                     "age_bl_c", "educ_c", extra)]
  ad <- ad[!is.na(ad[[response]]), ]
  ad$avisit <- factor(ad$avisit, levels = c(2, 3), labels = c("Visit 2", "Visit 3"))
  ad$id     <- factor(ad$id)
  ad <- ad[order(ad$id, ad$avisit), ]
  stopifnot(!any(is.na(ad$avisit)), !any(duplicated(ad[c("id", "avisit")])))
  ad
}
rows_pap <- long$pap_fl & long$mmrm_fl
est_rows <- list()
add_est <- function(analysis, group, ls) {
  est_rows[[length(est_rows) + 1]] <<- data.frame(
    analysis = analysis, group = group, visit = as.character(ls$avisit),
    estimate = ls[[intersect(c("emmean", "estimate"), names(ls))[1]]],
    se = ls$SE, df = ls$df, lower = ls$lower.CL, upper = ls$upper.CL,
    stringsAsFactors = FALSE)
}

# 9.6 MMSE MMRM ---------------------------------------------------------------------
ad_mmse <- prep(rows_pap, "chg_mmse")
mmse_res <- fit_with_fallback("avisit * base_mmse_c + age_bl_c + educ_c", ad_mmse,
                              response = "chg_mmse")
if (!is.null(mmse_res$fit)) {
  ls_mmse <- ls_means(mmse_res$fit, at = list(base_mmse_c = 0, age_bl_c = 0, educ_c = 0))
  add_est("MMSE change", "PAP", ls_mmse)
  n_mmse <- as.integer(table(ad_mmse$avisit))
  mmse_df <- data.frame(
    `Analysis visit` = as.character(ls_mmse$avisit),
    Participants = n_mmse,
    `LS mean change in MMSE (points)` = sprintf("%.2f", ls_mmse$emmean),
    SE = sprintf("%.2f", ls_mmse$SE),
    `95% CI` = sprintf("%.2f to %.2f", ls_mmse$lower.CL, ls_mmse$upper.CL),
    df = sprintf("%.1f", ls_mmse$df),
    check.names = FALSE, stringsAsFactors = FALSE)
  mmse_lines <- md_table(mmse_df)
} else {
  mmse_lines <- paste("Model could not be fitted:", mmse_res$note)
}

# MMSE ceiling counts (PAP)
pap_long <- long[long$pap_fl, ]
bl_mmse  <- pap_long$mmse[pap_long$visit_label == 1]
v_mmse   <- function(k) pap_long$mmse[rows_pap[long$pap_fl] & pap_long$avisit %in% k]
ceil_df <- data.frame(
  Timepoint = c("Baseline", "Analysis visit 2", "Analysis visit 3"),
  `With MMSE` = c(sum(!is.na(bl_mmse)), sum(!is.na(v_mmse(2))), sum(!is.na(v_mmse(3)))),
  `MMSE = 30` = sc(c(sum(bl_mmse == 30, na.rm = TRUE), sum(v_mmse(2) == 30, na.rm = TRUE),
                     sum(v_mmse(3) == 30, na.rm = TRUE))),
  check.names = FALSE, stringsAsFactors = FALSE)

# 9.6 CDR progression ----------------------------------------------------------------
wilson <- function(x, n, z = qnorm(0.975)) {
  p <- x / n; den <- 1 + z^2 / n
  ctr <- (p + z^2 / (2 * n)) / den
  hw  <- z * sqrt(p * (1 - p) / n + z^2 / (4 * n^2)) / den
  c(ctr - hw, ctr + hw)
}
cdr_df <- do.call(rbind, lapply(2:3, function(k) {
  x <- pap_long$cdr[rows_pap[long$pap_fl] & pap_long$avisit %in% k]
  n <- sum(!is.na(x)); e <- sum(x >= 0.5, na.rm = TRUE)
  show <- e >= 5 && (n - e) >= 5
  ci <- wilson(e, n)
  data.frame(`Analysis visit` = k, `Observed` = n, `CDR 0.5 or above` = sc(e),
             `Proportion (Wilson 95% CI)` = if (show)
               sprintf("%.1f%% (%.1f%% to %.1f%%)", 100 * e / n, 100 * ci[1], 100 * ci[2])
             else "suppressed (cell below 5)",
             check.names = FALSE, stringsAsFactors = FALSE)
}))

# 9.7 Demonstration contrast: sex as pseudo-arm ---------------------------------------
ad_sex <- prep(rows_pap, "chg_nwbv", extra = "sex")
ad_sex$sex <- factor(ad_sex$sex, levels = c("F", "M"))
sex_res <- fit_with_fallback("avisit * base_nwbv_c + age_bl_c + educ_c + sex + sex:avisit",
                             ad_sex)
if (!is.null(sex_res$fit)) {
  emm_sex <- emmeans(sex_res$fit, ~ sex | avisit, at = PAP_MEAN_COVARIATES)
  ls_sex  <- as.data.frame(summary(emm_sex, level = 0.95))
  con_sex <- as.data.frame(summary(contrast(emm_sex, method = "revpairwise"),
                                   infer = c(TRUE, FALSE), level = 0.95))
  for (g in levels(ad_sex$sex)) add_est("Sex contrast: LS mean", g, ls_sex[ls_sex$sex == g, ])
  add_est("Sex contrast: M - F", "M - F", con_sex)
  n_sex <- table(ad_sex$sex, ad_sex$avisit)
  sex_df <- data.frame(
    `Analysis visit` = as.character(ls_sex$avisit),
    Sex = as.character(ls_sex$sex),
    Participants = sc(n_sex[cbind(as.character(ls_sex$sex), as.character(ls_sex$avisit))]),
    `LS mean change (pp, 95% CI)` = fmt_est_ci(ls_sex$emmean, ls_sex$lower.CL, ls_sex$upper.CL),
    check.names = FALSE, stringsAsFactors = FALSE)
  con_df <- data.frame(
    `Analysis visit` = as.character(con_sex$avisit),
    `Difference M - F (pp)` = sprintf("%.3f", con_sex$estimate),
    SE = sprintf("%.3f", con_sex$SE),
    `95% CI` = sprintf("%.3f to %.3f", con_sex$lower.CL, con_sex$upper.CL),
    df = sprintf("%.1f", con_sex$df),
    check.names = FALSE, stringsAsFactors = FALSE)
  sex_lines <- c(md_table(sex_df), "", md_table(con_df))
} else {
  sex_lines <- paste("Model could not be fitted:", sex_res$note)
}

# Report ---------------------------------------------------------------------------------
est <- do.call(rbind, est_rows)
write.csv(est, file.path(report_dir, "05_secondary_estimates.csv"), row.names = FALSE)

out <- c(
  "# Report 05: Secondary endpoints and demonstration contrast (SAP v1.0, 9.6 and 9.7)",
  "",
  paste0("- **Generated:** ", format(Sys.time(), "%Y-%m-%d %H:%M %Z"), " by `05_secondary_and_contrast.R`"),
  "- **Population:** primary analysis population; analysis visits by time window",
  paste0("- **mmrm** ", packageVersion("mmrm"), ", **emmeans** ", packageVersion("emmeans"),
         ", **R** ", getRversion()),
  "- **Small cells:** counts of 1 to 4 shown as `<5`.",
  "",
  "## 9.6 MMSE: change from baseline (MMRM)", "",
  paste0("Covariance used: ", mmse_res$covariance,
         if (nzchar(mmse_res$note)) paste0(" (", mmse_res$note, ")") else "",
         ". Estimates at the PAP mean of baseline MMSE, age and education."), "",
  mmse_lines, "",
  "### MMSE at the ceiling", "",
  md_table(ceil_df), "",
  "Participants at 30 can only stay level or fall, which limits what change in MMSE can show in this population.", "",
  "## 9.6 CDR progression", "",
  "Proportion with CDR 0.5 or above among participants observed at each analysis visit (all had CDR 0 at baseline). Descriptive only.", "",
  md_table(cdr_df), "",
  "## 9.7 Demonstration contrast: sex as pseudo-arm", "",
  "**Observational association, not a treatment effect.** Fitted only to exercise the between-group contrast for the SAS/R cross-check (SAP 10). Not interpreted as a finding.", "",
  paste0("Covariance used: ", sex_res$covariance,
         if (nzchar(sex_res$note)) paste0(" (", sex_res$note, ")") else "", "."), "",
  sex_lines, ""
)
writeLines(out, file.path(report_dir, "05_secondary_report.md"))
writeLines(capture.output(sessionInfo()), file.path(report_dir, "05_sessioninfo.txt"))

cat(out, sep = "\n")
cat("\nReport written to", file.path(report_dir, "05_secondary_report.md"), "\n")
