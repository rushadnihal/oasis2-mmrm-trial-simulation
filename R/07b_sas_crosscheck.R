# ------------------------------------------------------------------------------
# 07b_sas_crosscheck.R
#
# Purpose : R side and comparison of the SAS/R cross-check (SAP v1.0,
#           Section 10), on the SYNTHETIC cross-check dataset.
#           Fits in R the two models of sas/mmrm_crosscheck.sas and compares
#           least-squares means, contrasts, standard errors, degrees of
#           freedom and covariance parameters with the SAS output.
#           Two R variants are compared with SAS:
#             A. vcov = "Kenward-Roger" (mmrm default; as in the primary
#                analysis, R/03_mmrm_primary.R)
#             B. vcov = "Kenward-Roger-Linear", which the mmrm documentation
#                gives as the setting that matches SAS DDFM=KR for an
#                unstructured covariance
#           Point estimates and covariance parameters do not depend on the
#           variant; standard errors and df can.
# Tolerances (SAP 10): absolute difference in LS means and contrasts below
#           1e-4; relative difference in SEs below 1e-3. Differences in df and
#           covariance parameters are reported.
# Inputs  : sas/crosscheck/mmrm_input.csv (synthetic, committed)
#           sas/crosscheck/sas_output/sas_*.csv (from SAS OnDemand, committed)
# Outputs : sas/crosscheck/crosscheck_report.md
# Run     : from the repository root, Rscript R/07b_sas_crosscheck.R
# ------------------------------------------------------------------------------

suppressPackageStartupMessages({
  library(mmrm)
  library(emmeans)
})
source("R/utils_mmrm.R")

cc_dir   <- file.path("sas", "crosscheck")
in_file  <- file.path(cc_dir, "mmrm_input.csv")
sas_dir  <- file.path(cc_dir, "sas_output")
out_file <- file.path(cc_dir, "crosscheck_report.md")
TOL_EST  <- 1e-4
TOL_SE   <- 1e-3

if (!file.exists(in_file)) {
  cat("No cross-check input at", in_file, "- nothing to do.\n")
  quit(save = "no", status = 0)
}

ad <- read.csv(in_file, stringsAsFactors = FALSE)
ad$avisit <- factor(ad$avisit, levels = c(2, 3), labels = c("2", "3"))
ad$id     <- factor(ad$id)
ad$sex    <- factor(ad$sex, levels = c("F", "M"))
ad <- ad[order(ad$id, ad$avisit), ]

rhs1 <- "avisit * base_nwbv_c + age_bl_c + educ_c"
rhs2 <- paste(rhs1, "+ sex + sex:avisit")
fit_r <- function(rhs, vcov) {
  mmrm(as.formula(paste("chg_nwbv ~", rhs, "+ us(avisit | id)")), data = ad,
       reml = TRUE, method = "Kenward-Roger", vcov = vcov)
}
variants <- c(A = "Kenward-Roger", B = "Kenward-Roger-Linear")

r_results <- lapply(variants, function(v) {
  f1 <- fit_r(rhs1, v)
  f2 <- fit_r(rhs2, v)
  stopifnot(component(f1, "convergence") == 0, component(f2, "convergence") == 0)
  ls1 <- ls_means(f1)
  emm2 <- emmeans(f2, ~ sex | avisit, at = PAP_MEAN_COVARIATES)
  ls2 <- as.data.frame(summary(emm2, level = 0.95))
  con <- as.data.frame(summary(contrast(emm2, method = "revpairwise"),
                               infer = c(TRUE, FALSE), level = 0.95))
  vc1 <- component(f1, "varcor"); vc2 <- component(f2, "varcor")
  list(
    est = rbind(
      data.frame(quantity = paste("Model 1 LS mean, visit", ls1$avisit),
                 r_est = ls1$emmean, r_se = ls1$SE, r_df = ls1$df),
      data.frame(quantity = paste("Model 2 LS mean,", ls2$sex, "visit", ls2$avisit),
                 r_est = ls2$emmean, r_se = ls2$SE, r_df = ls2$df),
      data.frame(quantity = paste("Model 2 contrast M - F, visit", con$avisit),
                 r_est = con$estimate, r_se = con$SE, r_df = con$df)),
    cov = data.frame(
      quantity = c(paste("Model 1", c("UN(1,1)", "UN(2,1)", "UN(2,2)")),
                   paste("Model 2", c("UN(1,1)", "UN(2,1)", "UN(2,2)"))),
      r_est = c(vc1[1, 1], vc1[2, 1], vc1[2, 2], vc2[1, 1], vc2[2, 1], vc2[2, 2]))
  )
})

# SAS output ------------------------------------------------------------------------
sas_files <- c("ls1", "cp1", "cs1", "ls2", "df2", "cp2", "cs2", "version")
sas_paths <- file.path(sas_dir, paste0("sas_", sas_files, ".csv"))
have_sas  <- all(file.exists(sas_paths))

header <- c(
  "# SAS/R cross-check (SAP v1.0, Section 10)",
  "",
  "- **Data:** synthetic cross-check dataset `sas/crosscheck/mmrm_input.csv` (not OASIS-2; D-009)",
  paste0("- **Rows:** ", nrow(ad), " from ", nlevels(ad$id), " synthetic participants"),
  paste0("- **R:** ", getRversion(), ", mmrm ", packageVersion("mmrm"),
         ", emmeans ", packageVersion("emmeans")),
  "- **Models:** (1) primary MMRM; (2) primary MMRM plus sex and sex x visit. Unstructured covariance, REML, Kenward-Roger.",
  "- **R variants:** A = `vcov = \"Kenward-Roger\"` (as in the primary analysis); B = `vcov = \"Kenward-Roger-Linear\"` (the mmrm-documented match to SAS DDFM=KR for unstructured covariance)",
  paste0("- **Tolerances (SAP 10):** |difference| in LS means and contrasts < ", TOL_EST,
         "; relative difference in SEs < ", TOL_SE),
  ""
)

if (!have_sas) {
  r_tab <- merge(r_results$A$est, r_results$B$est, by = "quantity", suffixes = c("_A", "_B"),
                 sort = FALSE)
  out <- c(header, "**Status: SAS output pending.** R results below; the comparison is added once `sas/crosscheck/sas_output/` holds the SAS files.", "",
           md_table(data.frame(Quantity = r_tab$quantity,
                               `R estimate` = sprintf("%.6f", r_tab$r_est_A),
                               `SE (A)` = sprintf("%.6f", r_tab$r_se_A),
                               `SE (B)` = sprintf("%.6f", r_tab$r_se_B),
                               `df (A)` = sprintf("%.2f", r_tab$r_df_A),
                               `df (B)` = sprintf("%.2f", r_tab$r_df_B),
                               check.names = FALSE)), "")
  writeLines(out, out_file); cat(out, sep = "\n")
  quit(save = "no", status = 0)
}

rd <- function(n) read.csv(file.path(sas_dir, paste0("sas_", n, ".csv")),
                            stringsAsFactors = FALSE, check.names = FALSE)
names_lower <- function(d) { names(d) <- tolower(names(d)); d }
ls1 <- names_lower(rd("ls1")); ls2 <- names_lower(rd("ls2")); df2 <- names_lower(rd("df2"))
cp1 <- names_lower(rd("cp1")); cp2 <- names_lower(rd("cp2"))
cs1 <- names_lower(rd("cs1")); cs2 <- names_lower(rd("cs2"))
sas_version <- names_lower(rd("version"))$sysvlong[1]
trim <- function(x) trimws(as.character(x))

sas_est <- rbind(
  data.frame(quantity = paste("Model 1 LS mean, visit", trim(ls1$avisit)),
             s_est = ls1$estimate, s_se = ls1$stderr, s_df = ls1$df),
  data.frame(quantity = paste("Model 2 LS mean,", trim(ls2$sex), "visit", trim(ls2$avisit)),
             s_est = ls2$estimate, s_se = ls2$stderr, s_df = ls2$df)
)
same_visit <- trim(df2$avisit) == trim(df2$`_avisit`) & trim(df2$sex) != trim(df2$`_sex`)
d <- df2[same_visit, ]
sign <- ifelse(trim(d$sex) == "M", 1, -1)        # SAS gives sex - _sex; convert to M - F
sas_est <- rbind(sas_est, data.frame(
  quantity = paste("Model 2 contrast M - F, visit", trim(d$avisit)),
  s_est = sign * d$estimate, s_se = d$stderr, s_df = d$df))
sas_cov <- data.frame(
  quantity = c(paste("Model 1", trim(cp1$covparm)), paste("Model 2", trim(cp2$covparm))),
  s_est = c(cp1$estimate, cp2$estimate))

compare <- function(v) {
  e <- merge(r_results[[v]]$est, sas_est, by = "quantity", sort = FALSE)
  e$d_est <- abs(e$r_est - e$s_est)
  e$rel_se <- abs(e$r_se - e$s_se) / e$s_se
  e$d_df <- e$r_df - e$s_df
  cv <- merge(r_results[[v]]$cov, sas_cov, by = "quantity", sort = FALSE)
  cv$rel <- abs(cv$r_est - cv$s_est) / abs(cv$s_est)
  list(est = e, cov = cv,
       pass_est = all(e$d_est < TOL_EST), pass_se = all(e$rel_se < TOL_SE),
       n = nrow(e), n_cov = nrow(cv))
}
cmp <- lapply(c(A = "A", B = "B"), compare)
stopifnot("SAS and R quantities did not line up" =
            cmp$A$n == nrow(r_results$A$est) && cmp$A$n_cov == 6)

est_table <- function(e) md_table(data.frame(
  Quantity = e$quantity,
  `R estimate` = sprintf("%.6f", e$r_est), `SAS estimate` = sprintf("%.6f", e$s_est),
  `abs diff` = sprintf("%.1e", e$d_est),
  `R SE` = sprintf("%.6f", e$r_se), `SAS SE` = sprintf("%.6f", e$s_se),
  `rel diff SE` = sprintf("%.1e", e$rel_se),
  `R df` = sprintf("%.2f", e$r_df), `SAS df` = sprintf("%.2f", e$s_df),
  check.names = FALSE))
verdict <- function(c) paste0("LS means and contrasts: **", if (c$pass_est) "PASS" else "FAIL",
                              "** (max abs diff ", sprintf("%.1e", max(c$est$d_est)),
                              "); SEs: **", if (c$pass_se) "PASS" else "FAIL",
                              "** (max rel diff ", sprintf("%.1e", max(c$est$rel_se)),
                              "); max |df difference| ", sprintf("%.2f", max(abs(c$est$d_df))),
                              "; max rel diff in covariance parameters ",
                              sprintf("%.1e", max(c$cov$rel)), ".")

conv <- paste0("SAS convergence status: model 1 ", paste(cs1$status, collapse = " "),
               ", model 2 ", paste(cs2$status, collapse = " "), " (0 = converged).")

out <- c(
  header,
  paste0("- **SAS:** ", sas_version), "",
  "## Summary", "",
  paste0("- **Variant A (primary-analysis setting):** ", verdict(cmp$A)),
  paste0("- **Variant B (Kenward-Roger-Linear):** ", verdict(cmp$B)),
  paste0("- ", conv), "",
  "## Variant A against SAS", "", est_table(cmp$A$est), "",
  "## Variant B against SAS", "", est_table(cmp$B$est), "",
  "## Covariance parameters", "",
  md_table(data.frame(Quantity = cmp$A$cov$quantity,
                      R = sprintf("%.6f", cmp$A$cov$r_est),
                      SAS = sprintf("%.6f", cmp$A$cov$s_est),
                      `rel diff` = sprintf("%.1e", cmp$A$cov$rel), check.names = FALSE)), ""
)
writeLines(out, out_file)
cat(out, sep = "\n")
cat("\nReport written to", out_file, "\n")
