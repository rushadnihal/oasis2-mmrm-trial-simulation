# ------------------------------------------------------------------------------
# sim_functions.R
#
# Purpose : Data-generating mechanism, dropout mechanisms, analysis methods
#           and performance measures for the Part B simulation study
#           (SAP v1.0, Section 11; ADEMP structure, Morris, White and
#           Crowther 2019).
#
# DGM     : Two-arm trial, two post-baseline visits (V2 about 2 years, V3
#           about 4 years). Parameters are rounded aggregates from the primary
#           MMRM on OASIS-2 (Report 03); no participant data are used.
#             change_k = mu_k * (1 - theta * arm) + b_k * base_c + e_k
#             (e_V2, e_V3) ~ N(0, Sigma)
#           theta is the proportional slowing of mean decline in the active
#           arm. The true treatment effect at V3 is -theta * mu_V3.
#           Age and education are omitted (small coefficients in Report 03).
#
# Dropout : Monotone. A fraction early_fraction of all dropouts occur before
#           V2 (both visits missing), the rest between V2 and V3 (V3 missing).
#             MCAR  random
#             MAR   missing V3 depends on the observed change at V2
#             MNAR  missing V3 depends on the unobserved change at V3; the
#                   active arm drops out at mnar_active_ratio x the control
#                   rate
#           Logistic dependence with odds ratio `or` per SD of more decline;
#           the intercept is calibrated within each simulated trial and arm
#           so the expected missingness equals the target.
# ------------------------------------------------------------------------------

SIM_PARAMS <- list(
  mu        = c(V2 = -0.75, V3 = -1.55),
  sigma     = matrix(c(1.10, 0.49,
                       0.49, 0.82), 2, 2, dimnames = list(c("V2", "V3"), c("V2", "V3"))),
  base_sd   = 3.8,                      # baseline nWBV SD (pp); centred at 0
  b_base    = c(V2 = -0.07, V3 = -0.02),
  early_fraction    = 1 / 3,
  mar_or            = 2,
  mnar_or           = 2,
  mnar_active_ratio = 1.5
)

true_effect <- function(theta, params = SIM_PARAMS) -theta * params$mu[["V3"]]

calibrated_prob <- function(z, target, or) {
  if (target <= 0) return(rep(0, length(z)))
  beta <- log(or)
  a <- uniroot(function(a) mean(plogis(a + beta * z)) - target, c(-30, 30))$root
  plogis(a + beta * z)
}

simulate_trial <- function(n, theta, mech, p, params = SIM_PARAMS) {
  N      <- 2 * n
  arm    <- rep(0:1, each = n)
  base_c <- rnorm(N, 0, params$base_sd)
  e      <- matrix(rnorm(2 * N), N, 2) %*% chol(params$sigma)
  y2 <- params$mu[["V2"]] * (1 - theta * arm) + params$b_base[["V2"]] * base_c + e[, 1]
  y3 <- params$mu[["V3"]] * (1 - theta * arm) + params$b_base[["V3"]] * base_c + e[, 2]

  miss2 <- miss3 <- rep(FALSE, N)
  if (mech != "none" && p > 0) {
    ef    <- params$early_fraction
    p_arm <- if (mech == "MNAR") ifelse(arm == 1, pmin(params$mnar_active_ratio * p, 0.9), p) else rep(p, N)
    early <- runif(N) < p_arm * ef
    late_target <- p_arm * (1 - ef) / (1 - p_arm * ef)     # P(miss V3 | present at V2)
    late <- rep(FALSE, N)
    for (g in 0:1) {
      idx <- which(arm == g & !early)
      tgt <- late_target[arm == g][1]
      prob <- switch(mech,
        MCAR = rep(tgt, length(idx)),
        MAR  = calibrated_prob(-as.numeric(scale(y2[idx])), tgt, params$mar_or),
        MNAR = calibrated_prob(-as.numeric(scale(y3[idx])), tgt, params$mnar_or))
      late[idx] <- runif(length(idx)) < prob
    }
    miss2 <- early
    miss3 <- early | late
  }

  data.frame(
    id     = factor(rep(seq_len(N), each = 2)),
    arm    = rep(arm, each = 2),
    visit  = factor(rep(c("V2", "V3"), N), levels = c("V2", "V3")),
    base_c = rep(base_c, each = 2),
    chg    = as.vector(rbind(ifelse(miss2, NA, y2), ifelse(miss3, NA, y3))),
    stringsAsFactors = FALSE
  )
}

# MMRM (MAR): treatment effect at V3. Unstructured, REML, Kenward-Roger
# (as in the primary analysis); compound symmetry if unstructured fails.
analyse_mmrm <- function(d) {
  dd <- d[!is.na(d$chg), ]
  dd$id <- droplevels(dd$id)
  fit_cov <- function(cov) {
    f <- as.formula(paste("chg ~ 0 + visit + visit:arm + visit:base_c +", cov, "(visit | id)"))
    tryCatch(mmrm::mmrm(f, data = dd, reml = TRUE, method = "Kenward-Roger"),
             error = function(e) NULL)
  }
  cov_used <- "us"
  fit <- fit_cov("us")
  if (is.null(fit) || mmrm::component(fit, "convergence") != 0) {
    cov_used <- "cs"
    fit <- fit_cov("cs")
  }
  if (is.null(fit) || mmrm::component(fit, "convergence") != 0) {
    return(data.frame(method = "MMRM", est = NA, se = NA, lci = NA, uci = NA,
                      failed = TRUE, cov = "none"))
  }
  cf <- summary(fit)$coefficients["visitV3:arm", ]
  q <- qt(0.975, cf[["df"]])
  data.frame(method = "MMRM", est = cf[["Estimate"]], se = cf[["Std. Error"]],
             lci = cf[["Estimate"]] - q * cf[["Std. Error"]],
             uci = cf[["Estimate"]] + q * cf[["Std. Error"]],
             failed = FALSE, cov = cov_used)
}

# Reference-based multiple imputation (rbmi): jump to reference and copy
# reference, from one set of approximate-Bayesian draws. ANCOVA at each visit
# on the imputed data; Rubin's rules.
analyse_rbmi <- function(d, n_samples) {
  empty <- data.frame(method = c("MI J2R", "MI CR"), est = NA, se = NA, lci = NA, uci = NA,
                      failed = TRUE, cov = "us")
  dat <- d
  dat$group <- factor(ifelse(dat$arm == 1, "Active", "Control"), levels = c("Control", "Active"))
  miss <- tapply(is.na(dat$chg), dat$id, function(x) if (x[1]) "V2" else if (x[2]) "V3" else NA)
  miss <- miss[!is.na(miss)]
  if (length(miss) == 0) return(empty[0, ])
  ice <- data.frame(id = factor(names(miss), levels = levels(dat$id)),
                    visit = factor(miss, levels = c("V2", "V3")),
                    strategy = "JR", stringsAsFactors = FALSE)
  ice_cr <- ice; ice_cr$strategy <- "CR"
  vars <- rbmi::set_vars(subjid = "id", visit = "visit", outcome = "chg", group = "group",
                         covariates = c("base_c*visit", "group*visit"))
  vars_an <- rbmi::set_vars(subjid = "id", visit = "visit", outcome = "chg", group = "group",
                            covariates = "base_c")
  out <- tryCatch({
    dr <- rbmi::draws(data = dat, data_ice = ice, vars = vars,
                      method = rbmi::method_approxbayes(n_samples = n_samples), quiet = TRUE)
    refs <- c(Control = "Control", Active = "Control")
    res <- lapply(list(`MI J2R` = NULL, `MI CR` = ice_cr), function(upd) {
      imp <- if (is.null(upd)) rbmi::impute(dr, references = refs)
             else rbmi::impute(dr, references = refs, update_strategy = upd)
      po <- as.data.frame(rbmi::pool(rbmi::analyse(imp, rbmi::ancova, vars = vars_an)))
      po[po$parameter == "trt_V3", ]
    })
    data.frame(method = names(res),
               est = vapply(res, function(r) r$est, 0), se = vapply(res, function(r) r$se, 0),
               lci = vapply(res, function(r) r$lci, 0), uci = vapply(res, function(r) r$uci, 0),
               failed = FALSE, cov = "us")
  }, error = function(e) empty)
  rownames(out) <- NULL
  out
}

# Performance measures with Monte Carlo SEs (Morris, White and Crowther 2019)
performance <- function(est, se, lci, uci, failed, truth) {
  ok <- !failed & !is.na(est)
  k  <- sum(ok); e <- est[ok]
  cover <- lci[ok] <= truth & truth <= uci[ok]
  rej   <- lci[ok] > 0 | uci[ok] < 0
  emp   <- sd(e)
  data.frame(
    n_rep = length(est), n_failed = sum(!ok),
    bias = mean(e) - truth, bias_mcse = emp / sqrt(k),
    emp_se = emp, emp_se_mcse = emp / sqrt(2 * (k - 1)),
    mod_se = sqrt(mean(se[ok]^2)),
    coverage = mean(cover), coverage_mcse = sqrt(mean(cover) * (1 - mean(cover)) / k),
    rejection = mean(rej), rejection_mcse = sqrt(mean(rej) * (1 - mean(rej)) / k),
    rmse = sqrt(mean((e - truth)^2))
  )
}
