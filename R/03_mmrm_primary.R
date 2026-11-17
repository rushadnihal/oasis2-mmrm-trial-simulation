# ------------------------------------------------------------------------------
# 03_mmrm_primary.R
#
# Purpose : Primary analysis of the frozen SAP (v1.0, Section 9.1). MMRM for
#           change from baseline in nWBV (percentage points) at analysis
#           visits 2 and 3 in the primary analysis population (PAP).
#             Fixed effects : visit, baseline nWBV, baseline nWBV x visit,
#                             age at baseline, education (all centred at the
#                             PAP mean)
#             Covariance    : unstructured within participant, REML,
#                             Kenward-Roger degrees of freedom
#             Fallback      : compound symmetry if the unstructured model does
#                             not converge or its estimate is not positive
#                             definite (SAP 9.1, D-006)
#           Primary result: least-squares mean change at analysis visit 2,
#           with 95% CI, at the PAP mean of the covariates.
# Inputs  : derived/analysis_long.rds in the data directory (OASIS2_DATA_DIR if
#           set, otherwise the working directory), from
#           02_build_analysis_data.R.
# Outputs : In the report directory (OASIS2_REPORT_DIR if set, otherwise
#           reports/ under the working directory), aggregate only:
#             03_mmrm_primary_report.md   report
#             03_mmrm_primary_lsmeans.csv least-squares means
#             03_sessioninfo.txt          R and package versions
# Privacy : Model estimates and counts only. No participant-level values.
# Run     : Rscript R/03_mmrm_primary.R  (needs packages mmrm and emmeans)
# ------------------------------------------------------------------------------

suppressPackageStartupMessages({
  library(mmrm)
  library(emmeans)
})

data_dir   <- Sys.getenv("OASIS2_DATA_DIR", unset = getwd())
report_dir <- Sys.getenv("OASIS2_REPORT_DIR", unset = file.path(getwd(), "reports"))
long_file  <- file.path(data_dir, "derived", "analysis_long.rds")
stopifnot("Run 02_build_analysis_data.R first" = file.exists(long_file))
dir.create(report_dir, recursive = TRUE, showWarnings = FALSE)

# Analysis dataset ----------------------------------------------------------------
long <- readRDS(long_file)
ad <- long[long$pap_fl & long$mmrm_fl,
           c("id", "avisit", "chg_nwbv", "base_nwbv_c", "age_bl_c", "educ_c")]
ad$avisit <- factor(ad$avisit, levels = c(2, 3), labels = c("Visit 2", "Visit 3"))
ad$id     <- factor(ad$id)
ad        <- ad[order(ad$id, ad$avisit), ]
rownames(ad) <- NULL

stopifnot(
  "Unexpected analysis visit"        = !any(is.na(ad$avisit)),
  "Missing response in MMRM rows"    = !any(is.na(ad$chg_nwbv)),
  "Missing covariate in MMRM rows"   = !any(is.na(ad[c("base_nwbv_c", "age_bl_c", "educ_c")])),
  "More than one row per id x visit" = !any(duplicated(ad[c("id", "avisit")]))
)

# Fit -----------------------------------------------------------------------------
f_us <- chg_nwbv ~ avisit * base_nwbv_c + age_bl_c + educ_c + us(avisit | id)
f_cs <- chg_nwbv ~ avisit * base_nwbv_c + age_bl_c + educ_c + cs(avisit | id)

fit_or_error <- function(f) {
  tryCatch(mmrm(f, data = ad, reml = TRUE, method = "Kenward-Roger"),
           error = function(e) e)
}
usable <- function(fit) {
  inherits(fit, "mmrm_tmb") &&
    component(fit, "convergence") == 0 &&
    all(eigen(component(fit, "varcor"), symmetric = TRUE, only.values = TRUE)$values > 0)
}
describe_failure <- function(fit) {
  if (inherits(fit, "error")) return(paste("error:", conditionMessage(fit)))
  if (component(fit, "convergence") != 0) return("did not converge")
  "covariance estimate not positive definite"
}

fit_us <- fit_or_error(f_us)
if (usable(fit_us)) {
  fit <- fit_us
  cov_used <- "Unstructured (primary)"
  fallback_note <- "Not triggered."
} else {
  us_problem <- describe_failure(fit_us)
  fit_cs <- fit_or_error(f_cs)
  if (!usable(fit_cs)) {
    stop("Both the unstructured and the compound-symmetry models failed. ",
         "Unstructured: ", us_problem, ". Compound symmetry: ", describe_failure(fit_cs), ".")
  }
  fit <- fit_cs
  cov_used <- "Compound symmetry (fallback)"
  fallback_note <- paste0("Triggered. Unstructured model: ", us_problem,
                          ". Log this in the decision log (SAP 9.1).")
}

# Least-squares means at the PAP mean of the covariates (centred covariates = 0) ---
emm <- emmeans(fit, ~ avisit, at = list(base_nwbv_c = 0, age_bl_c = 0, educ_c = 0))
ls  <- as.data.frame(summary(emm, level = 0.95))
n_at_visit <- as.integer(table(ad$avisit))
ls_out <- data.frame(
  `Analysis visit`          = as.character(ls$avisit),
  `Participants`            = n_at_visit,
  `LS mean change (pp)`     = sprintf("%.3f", ls$emmean),
  `SE`                      = sprintf("%.3f", ls$SE),
  `95% CI`                  = sprintf("%.3f to %.3f", ls$lower.CL, ls$upper.CL),
  `df (Kenward-Roger)`      = sprintf("%.1f", ls$df),
  check.names = FALSE, stringsAsFactors = FALSE
)
write.csv(data.frame(visit = as.character(ls$avisit), n = n_at_visit,
                     estimate = ls$emmean, se = ls$SE, df = ls$df,
                     lower = ls$lower.CL, upper = ls$upper.CL,
                     covariance = cov_used),
          file.path(report_dir, "03_mmrm_primary_lsmeans.csv"), row.names = FALSE)

# Fixed effects ----------------------------------------------------------------------
cf <- summary(fit)$coefficients
fe_out <- data.frame(
  Term     = rownames(cf),
  Estimate = sprintf("%.4f", cf[, "Estimate"]),
  SE       = sprintf("%.4f", cf[, "Std. Error"]),
  df       = sprintf("%.1f", cf[, "df"]),
  check.names = FALSE, stringsAsFactors = FALSE
)

# Covariance ------------------------------------------------------------------------
vc  <- component(fit, "varcor")
cor <- cov2cor(vc)
vc_out <- data.frame(
  Quantity = c("Variance at visit 2", "Variance at visit 3",
               "Covariance (visit 2, visit 3)", "Correlation (visit 2, visit 3)"),
  Estimate = sprintf("%.4f", c(vc[1, 1], vc[2, 2], vc[1, 2], cor[1, 2])),
  stringsAsFactors = FALSE
)

# Report ------------------------------------------------------------------------------
md_table <- function(df) {
  df[] <- lapply(df, as.character)
  c(paste0("| ", paste(names(df), collapse = " | "), " |"),
    paste0("|", paste(rep("---", ncol(df)), collapse = "|"), "|"),
    apply(df, 1, function(r) paste0("| ", paste(r, collapse = " | "), " |")))
}
pattern <- table(factor(tapply(as.character(ad$avisit), ad$id, paste, collapse = " + "),
                        levels = c("Visit 2 + Visit 3", "Visit 2", "Visit 3")))

out <- c(
  "# Report 03: Primary MMRM (SAP v1.0, Section 9.1)",
  "",
  paste0("- **Generated:** ", format(Sys.time(), "%Y-%m-%d %H:%M %Z"), " by `03_mmrm_primary.R`"),
  "- **Population:** primary analysis population (CDR 0 at visit 1)",
  "- **Response:** change from baseline in nWBV, percentage points (pp)",
  "- **Model:** visit, baseline nWBV, baseline nWBV x visit, age at baseline, education; REML; Kenward-Roger df",
  paste0("- **Covariance used:** ", cov_used),
  paste0("- **Fallback:** ", fallback_note),
  paste0("- **mmrm** ", packageVersion("mmrm"), ", **emmeans** ", packageVersion("emmeans"),
         ", **R** ", getRversion()),
  "",
  "## Data used",
  "",
  paste0("- Participants: ", nlevels(droplevels(ad$id)), "; observations: ", nrow(ad)),
  paste0("- Both visits: ", pattern[["Visit 2 + Visit 3"]], "; visit 2 only: ",
         pattern[["Visit 2"]], "; visit 3 only: ", pattern[["Visit 3"]]),
  "",
  "## Least-squares mean change from baseline",
  "",
  "At the primary analysis population mean of baseline nWBV, age and education. **Visit 2 is the primary timepoint.**",
  "",
  md_table(ls_out),
  "",
  "## Fixed effects",
  "",
  md_table(fe_out),
  "",
  "## Covariance estimate",
  "",
  md_table(vc_out),
  "",
  "## Fit details",
  "",
  paste0("- Covariance parameters: ", component(fit, "n_theta")),
  paste0("- Optimizer: ", component(fit, "optimizer"),
         "; convergence code: ", component(fit, "convergence")),
  paste0("- -2 REML log-likelihood: ", sprintf("%.2f", -2 * as.numeric(logLik(fit)))),
  ""
)
writeLines(out, file.path(report_dir, "03_mmrm_primary_report.md"))
writeLines(capture.output(sessionInfo()), file.path(report_dir, "03_sessioninfo.txt"))

cat(out, sep = "\n")
cat("\nReport written to", file.path(report_dir, "03_mmrm_primary_report.md"), "\n")
