# ------------------------------------------------------------------------------
# utils_mmrm.R
#
# Purpose : Shared helpers for the Part A analysis scripts: fitting an MMRM
#           with the pre-specified covariance fallback (SAP 9.1, D-006),
#           least-squares means at the PAP covariate means, and markdown
#           tables. Sourced by scripts run from the repository root.
# ------------------------------------------------------------------------------

# Covariates are centred at the PAP mean, so 0 is the PAP mean.
PAP_MEAN_COVARIATES <- list(base_nwbv_c = 0, age_bl_c = 0, educ_c = 0)

fit_mmrm <- function(formula, data) {
  tryCatch(mmrm::mmrm(formula, data = data, reml = TRUE, method = "Kenward-Roger"),
           error = function(e) e)
}

usable_fit <- function(fit) {
  inherits(fit, "mmrm_tmb") &&
    mmrm::component(fit, "convergence") == 0 &&
    all(eigen(mmrm::component(fit, "varcor"), symmetric = TRUE,
              only.values = TRUE)$values > 0)
}

fit_problem <- function(fit) {
  if (inherits(fit, "error")) return(paste("error:", conditionMessage(fit)))
  if (mmrm::component(fit, "convergence") != 0) return("did not converge")
  "covariance estimate not positive definite"
}

# Unstructured first; compound symmetry if unstructured is not usable.
# Returns list(fit, covariance, note); fit is NULL if both fail.
fit_with_fallback <- function(rhs, data, response = "chg_nwbv") {
  f_us <- stats::as.formula(paste(response, "~", rhs, "+ us(avisit | id)"))
  f_cs <- stats::as.formula(paste(response, "~", rhs, "+ cs(avisit | id)"))
  us <- fit_mmrm(f_us, data)
  if (usable_fit(us)) return(list(fit = us, covariance = "Unstructured", note = ""))
  cs <- fit_mmrm(f_cs, data)
  if (usable_fit(cs)) {
    return(list(fit = cs, covariance = "Compound symmetry (fallback)",
                note = paste("Unstructured:", fit_problem(us))))
  }
  list(fit = NULL, covariance = "None",
       note = paste("Unstructured:", fit_problem(us), "; compound symmetry:", fit_problem(cs)))
}

ls_means <- function(fit, spec = ~ avisit, at = PAP_MEAN_COVARIATES) {
  as.data.frame(summary(emmeans::emmeans(fit, spec, at = at), level = 0.95))
}

fmt_est_ci <- function(est, lo, hi, digits = 3) {
  sprintf(paste0("%.", digits, "f (%.", digits, "f to %.", digits, "f)"), est, lo, hi)
}

md_table <- function(df) {
  df[] <- lapply(df, as.character)
  c(paste0("| ", paste(names(df), collapse = " | "), " |"),
    paste0("|", paste(rep("---", ncol(df)), collapse = "|"), "|"),
    apply(df, 1, function(r) paste0("| ", paste(r, collapse = " | "), " |")))
}
