# ------------------------------------------------------------------------------
# 04_robustness.R
#
# Purpose : Robustness of the primary result to covariance structure, time
#           model, visit scheme and population, as pre-specified in SAP v1.0:
#             9.2  Covariance comparison: unstructured against compound
#                  symmetry (AIC, BIC), identical fixed effects, REML
#             9.3  Continuous-time model: random intercept and slope in years
#                  since baseline, all sessions of the PAP; implied mean change
#                  at 1.90 and 3.97 years (PAP median times of analysis visits
#                  2 and 3, fixed in the SAP)
#             9.4  Label-based visit scheme: primary MMRM with the file's Visit
#                  label in place of the time windows
#             9.5  Supportive population: all participants, adding baseline
#                  CDR (0, 0.5 or above) and its interaction with visit
#           Disagreement with the primary analysis is reported, not resolved.
# Inputs  : derived/analysis_long.rds in the data directory (OASIS2_DATA_DIR if
#           set, otherwise the working directory).
# Outputs : In the report directory (OASIS2_REPORT_DIR if set, otherwise
#           reports/ under the working directory), aggregate only:
#             04_robustness_report.md, 04_robustness_estimates.csv,
#             04_sessioninfo.txt
# Run     : from the repository root, Rscript R/04_robustness.R
#           (needs mmrm and emmeans; nlme ships with R)
# ------------------------------------------------------------------------------

suppressPackageStartupMessages({
  library(mmrm)
  library(emmeans)
  library(nlme)
})
source("R/utils_mmrm.R")

data_dir   <- Sys.getenv("OASIS2_DATA_DIR", unset = getwd())
report_dir <- Sys.getenv("OASIS2_REPORT_DIR", unset = file.path(getwd(), "reports"))
long_file  <- file.path(data_dir, "derived", "analysis_long.rds")
stopifnot("Run 02_build_analysis_data.R first" = file.exists(long_file))
dir.create(report_dir, recursive = TRUE, showWarnings = FALSE)

long <- readRDS(long_file)
covs <- c("base_nwbv_c", "age_bl_c", "educ_c")
rhs  <- "avisit * base_nwbv_c + age_bl_c + educ_c"
SAP_TIMES <- c(1.90, 3.97)   # SAP 9.3: PAP median times of analysis visits 2 and 3

prep <- function(rows, visit_col) {
  ad <- long[rows, c("id", visit_col, "chg_nwbv", covs, "base_cdr_cat2")]
  names(ad)[2] <- "avisit"
  ad$avisit <- factor(ad$avisit, levels = c(2, 3), labels = c("Visit 2", "Visit 3"))
  ad$id     <- factor(ad$id)
  ad$base_cdr_cat2 <- factor(ad$base_cdr_cat2, levels = c("0", "0.5 or above"))
  ad <- ad[order(ad$id, ad$avisit), ]
  stopifnot(!any(is.na(ad$avisit)), !any(is.na(ad$chg_nwbv)),
            !any(duplicated(ad[c("id", "avisit")])))
  ad
}

est_rows <- list()
add_est <- function(analysis, population, visit, n, est, se, lo, hi, df, covariance) {
  est_rows[[length(est_rows) + 1]] <<- data.frame(
    analysis = analysis, population = population, visit = visit, n = n,
    estimate = est, se = se, lower = lo, upper = hi, df = df, covariance = covariance,
    stringsAsFactors = FALSE)
}
add_ls <- function(analysis, population, fit_res, ad, ls = NULL) {
  if (is.null(fit_res$fit)) return(invisible())
  if (is.null(ls)) ls <- ls_means(fit_res$fit)
  n <- as.integer(table(ad$avisit))
  for (k in 1:2) {
    add_est(analysis, population, as.character(ls$avisit[k]), n[k], ls$emmean[k], ls$SE[k],
            ls$lower.CL[k], ls$upper.CL[k], ls$df[k], fit_res$covariance)
  }
}

# Primary (refitted here so every comparison is from the same run) --------------------
ad_pap <- prep(long$pap_fl & long$mmrm_fl, "avisit")
primary <- fit_with_fallback(rhs, ad_pap)
stopifnot("Primary MMRM could not be fitted" = !is.null(primary$fit))
add_ls("Primary MMRM", "PAP", primary, ad_pap)

# 9.2 Covariance comparison ---------------------------------------------------------
f_us <- as.formula(paste("chg_nwbv ~", rhs, "+ us(avisit | id)"))
f_cs <- as.formula(paste("chg_nwbv ~", rhs, "+ cs(avisit | id)"))
cov_fits <- list(Unstructured = fit_mmrm(f_us, ad_pap), `Compound symmetry` = fit_mmrm(f_cs, ad_pap))
cov_df <- do.call(rbind, lapply(names(cov_fits), function(nm) {
  f <- cov_fits[[nm]]
  if (!usable_fit(f)) {
    return(data.frame(Structure = nm, `Covariance parameters` = "-", AIC = "-", BIC = "-",
                      `Visit 2 estimate (95% CI)` = paste("not usable:", fit_problem(f)),
                      `Visit 3 estimate (95% CI)` = "-",
                      check.names = FALSE, stringsAsFactors = FALSE))
  }
  ls <- ls_means(f)
  data.frame(Structure = nm,
             `Covariance parameters` = component(f, "n_theta"),
             AIC = sprintf("%.2f", AIC(f)), BIC = sprintf("%.2f", BIC(f)),
             `Visit 2 estimate (95% CI)` = fmt_est_ci(ls$emmean[1], ls$lower.CL[1], ls$upper.CL[1]),
             `Visit 3 estimate (95% CI)` = fmt_est_ci(ls$emmean[2], ls$lower.CL[2], ls$upper.CL[2]),
             check.names = FALSE, stringsAsFactors = FALSE)
}))
if (usable_fit(cov_fits[["Compound symmetry"]])) {
  add_ls("Compound symmetry", "PAP",
         list(fit = cov_fits[["Compound symmetry"]], covariance = "Compound symmetry"), ad_pap)
}

# 9.3 Continuous-time model ----------------------------------------------------------
ct <- long[long$pap_fl, c("id", "years_since_bl", "nwbv_pct", "age_bl_c", "educ_c")]
ct$id <- factor(ct$id)
fit_ct <- tryCatch(
  lme(nwbv_pct ~ years_since_bl * age_bl_c + educ_c, random = ~ years_since_bl | id,
      data = ct, method = "REML"),
  error = function(e) e)
ct_ok <- inherits(fit_ct, "lme")
if (ct_ok) {
  tt    <- summary(fit_ct)$tTable
  b     <- tt["years_since_bl", "Value"]
  b_se  <- tt["years_since_bl", "Std.Error"]
  b_df  <- tt["years_since_bl", "DF"]
  qq    <- qt(0.975, b_df)
  for (k in 1:2) {
    t <- SAP_TIMES[k]
    add_est("Continuous-time", "PAP (all sessions)", paste0("Visit ", k + 1, " (", t, " years)"),
            length(unique(ct$id)), b * t, b_se * t, (b - qq * b_se) * t, (b + qq * b_se) * t,
            b_df, "Random intercept and slope")
  }
  vcm <- VarCorr(fit_ct)
  sds <- suppressWarnings(as.numeric(vcm[, "StdDev"]))
  ct_lines <- c(
    paste0("- Participants: ", length(unique(ct$id)), "; sessions: ", nrow(ct)),
    paste0("- Mean slope at PAP mean age and education: ",
           fmt_est_ci(b, b - qq * b_se, b + qq * b_se), " percentage points per year (df ",
           sprintf("%.0f", b_df), ")"),
    paste0("- Age at baseline x time: ", sprintf("%.4f (SE %.4f)",
           tt["years_since_bl:age_bl_c", "Value"], tt["years_since_bl:age_bl_c", "Std.Error"]),
           " percentage points per year per year of age"),
    paste0("- Between-participant SD of slopes: ", sprintf("%.3f", sds[2]),
           "; residual SD: ", sprintf("%.3f", sds[3]))
  )
} else {
  ct_lines <- paste("- Model could not be fitted:", conditionMessage(fit_ct))
}

# 9.4 Label-based visit scheme --------------------------------------------------------
ad_lab <- prep(long$pap_fl & long$mmrm_lab_fl, "visit_label")
label_res <- fit_with_fallback(rhs, ad_lab)
add_ls("Label-based visits", "PAP", label_res, ad_lab)

# 9.5 Supportive population -----------------------------------------------------------
ad_sup <- prep(long$supp_fl & long$mmrm_fl, "avisit")
sup_res <- fit_with_fallback(paste(rhs, "+ base_cdr_cat2 + base_cdr_cat2:avisit"), ad_sup)
if (!is.null(sup_res$fit)) {
  ls_sup <- ls_means(sup_res$fit, ~ avisit | base_cdr_cat2)
  for (g in levels(ad_sup$base_cdr_cat2)) {
    sub <- ad_sup[ad_sup$base_cdr_cat2 == g, ]
    add_ls("Supportive population", paste("Baseline CDR", g), sup_res, sub,
           ls = ls_sup[ls_sup$base_cdr_cat2 == g, ])
  }
}

# Assemble ------------------------------------------------------------------------------
est <- do.call(rbind, est_rows)
write.csv(est, file.path(report_dir, "04_robustness_estimates.csv"), row.names = FALSE)

summary_df <- do.call(rbind, lapply(split(est, factor(paste(est$analysis, "|", est$population),
                                                      levels = unique(paste(est$analysis, "|", est$population)))),
  function(x) data.frame(
    Analysis   = x$analysis[1],
    Population = x$population[1],
    Covariance = x$covariance[1],
    `n at visit 2` = x$n[1],
    `Visit 2: change (95% CI)` = fmt_est_ci(x$estimate[1], x$lower[1], x$upper[1]),
    `n at visit 3` = x$n[2],
    `Visit 3: change (95% CI)` = fmt_est_ci(x$estimate[2], x$lower[2], x$upper[2]),
    check.names = FALSE, stringsAsFactors = FALSE)))
rownames(summary_df) <- NULL
summary_df$`n at visit 2`[summary_df$Analysis == "Continuous-time"] <- "all sessions"
summary_df$`n at visit 3`[summary_df$Analysis == "Continuous-time"] <- "all sessions"

p2 <- est[est$analysis == "Primary MMRM", ]
diff_df <- do.call(rbind, lapply(setdiff(unique(est$analysis), "Primary MMRM"), function(a) {
  x <- est[est$analysis == a & est$population %in% c("PAP", "PAP (all sessions)", "Baseline CDR 0"), ]
  if (nrow(x) < 2) return(NULL)
  data.frame(Analysis = a,
             `Visit 2: difference from primary` = sprintf("%+.3f", x$estimate[1] - p2$estimate[1]),
             `Visit 2: inside primary 95% CI` = ifelse(x$estimate[1] >= p2$lower[1] & x$estimate[1] <= p2$upper[1], "yes", "no"),
             `Visit 3: difference from primary` = sprintf("%+.3f", x$estimate[2] - p2$estimate[2]),
             `Visit 3: inside primary 95% CI` = ifelse(x$estimate[2] >= p2$lower[2] & x$estimate[2] <= p2$upper[2], "yes", "no"),
             check.names = FALSE, stringsAsFactors = FALSE)
}))

fit_notes <- c(
  paste0("- Primary: ", primary$covariance, if (nzchar(primary$note)) paste0(" (", primary$note, ")")),
  paste0("- Label-based: ", label_res$covariance, if (nzchar(label_res$note)) paste0(" (", label_res$note, ")")),
  paste0("- Supportive: ", sup_res$covariance, if (nzchar(sup_res$note)) paste0(" (", sup_res$note, ")")),
  paste0("- Continuous-time: ", if (ct_ok) "fitted" else "not fitted")
)

out <- c(
  "# Report 04: Robustness of the primary result (SAP v1.0, Sections 9.2 to 9.5)",
  "",
  paste0("- **Generated:** ", format(Sys.time(), "%Y-%m-%d %H:%M %Z"), " by `04_robustness.R`"),
  "- **Response:** change from baseline in nWBV, percentage points. Estimates at the PAP mean of baseline nWBV, age and education.",
  paste0("- **mmrm** ", packageVersion("mmrm"), ", **emmeans** ", packageVersion("emmeans"),
         ", **nlme** ", packageVersion("nlme"), ", **R** ", getRversion()),
  "",
  "## Summary of estimates", "",
  md_table(summary_df), "",
  "The supportive-population row for baseline CDR 0.5 or above describes a different population and is not a check on the primary result. Continuous-time estimates are at 1.90 and 3.97 years, as fixed in the SAP.", "",
  "## Agreement with the primary analysis", "",
  if (is.null(diff_df)) "No comparable estimates." else md_table(diff_df), "",
  "Descriptive only. A sensitivity estimate outside the primary 95% CI is reported and discussed, not used to change the primary analysis.", "",
  "## 9.2 Covariance comparison (PAP, REML, identical fixed effects)", "",
  md_table(cov_df), "",
  "Lower AIC or BIC indicates better fit penalised for the number of covariance parameters. The primary analysis remains unstructured unless the fallback is triggered (SAP 9.1).", "",
  "## 9.3 Continuous-time model", "",
  ct_lines, "",
  "## Fit status", "",
  fit_notes, ""
)
writeLines(out, file.path(report_dir, "04_robustness_report.md"))
writeLines(capture.output(sessionInfo()), file.path(report_dir, "04_sessioninfo.txt"))

cat(out, sep = "\n")
cat("\nReport written to", file.path(report_dir, "04_robustness_report.md"), "\n")
