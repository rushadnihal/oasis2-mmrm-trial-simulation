# ------------------------------------------------------------------------------
# 10_simulation_summary.R
#
# Purpose : Performance measures for the Part B simulation (SAP v1.0,
#           Section 11) from the per-scenario files of 09_simulation_full.R:
#           bias, empirical and model SE, coverage, rejection rate (type I
#           error at theta = 0, power otherwise) and RMSE, each with its Monte
#           Carlo SE; fitting failures; and the sample-size table (n per arm
#           for 80% and 90% power, no dropout and MAR 30%).
# Truth   : treatment effect at V3 = theta x 1.55 pp (treatment policy). MI
#           with reference-based imputation targets a different estimand, and
#           MI MAR with delta deliberately assumes worse outcomes in active-arm
#           dropouts; their bias against this truth is expected.
# Inputs  : <OASIS2_SIM_DIR>/raw/*.rds (default results/simulation/raw)
# Outputs : <OASIS2_SIM_DIR>/performance.csv  all scenarios and methods
#           <OASIS2_SIM_DIR>/simulation_summary.md
#           Both aggregate; no participant data.
# Run     : from the repository root, Rscript R/10_simulation_summary.R
# ------------------------------------------------------------------------------

source("R/sim/sim_functions.R")
source("R/utils_mmrm.R")   # md_table

sim_dir <- Sys.getenv("OASIS2_SIM_DIR", file.path("results", "simulation"))
files   <- list.files(file.path(sim_dir, "raw"), pattern = "\\.rds$", full.names = TRUE)
stopifnot("No simulation results found; run R/09_simulation_full.R first" = length(files) > 0)
res <- do.call(rbind, lapply(files, readRDS))

grid <- full_grid()
n_expected <- c(mmrm = sum(grid$part == "mmrm"), mi = sum(grid$part == "mi"))
n_found    <- tapply(unique(res[, c("key", "part")])$part, unique(res[, c("key", "part")])$part, length)

keys <- unique(res[, c("key", "part", "n", "theta", "dropout", "method")])
perf <- do.call(rbind, lapply(seq_len(nrow(keys)), function(j) {
  r <- res[res$key == keys$key[j] & res$method == keys$method[j], ]
  cbind(keys[j, ], performance(r$est, r$se, r$lci, r$uci, r$failed, true_effect(keys$theta[j])),
        n_cs_fallback = sum(r$cov == "cs", na.rm = TRUE),
        mean_seconds = mean(r$seconds))
}))
rownames(perf) <- NULL
write.csv(perf, file.path(sim_dir, "performance.csv"), row.names = FALSE)

pc <- function(x, m) sprintf("%.1f (%.1f)", 100 * x, 100 * m)
f3 <- function(x) sprintf("%.3f", x)
drop_order <- DROPOUT_SCENARIOS$dropout
n_levels <- c(30, 50, 80, 120, 200)

wide <- function(d, value_fun, rows = drop_order) {
  out <- data.frame(Dropout = rows, stringsAsFactors = FALSE)
  for (n in n_levels) {
    out[[paste("n =", n)]] <- vapply(rows, function(dr) {
      x <- d[d$dropout == dr & d$n == n, ]
      if (nrow(x) == 0) "-" else value_fun(x)
    }, character(1))
  }
  out[out$Dropout %in% unique(d$dropout), , drop = FALSE]
}

mm <- perf[perf$part == "mmrm", ]

# Type I error (theta = 0)
t1 <- wide(mm[mm$theta == 0, ], function(x) pc(x$rejection, x$rejection_mcse))

# Power by theta, no dropout and MAR 30%
pow <- do.call(rbind, lapply(c(0.1, 0.2, 0.3, 0.4), function(th) {
  w <- wide(mm[mm$theta == th, ], function(x) sprintf("%.1f", 100 * x$rejection),
            rows = c("none", "MAR 30%", "MNAR 30%", "MAR 50%"))
  cbind(theta = th, w)
}))

# Bias and coverage, theta = 0.2
bc <- wide(mm[mm$theta == 0.2, ], function(x) paste0(f3(x$bias), " / ", sprintf("%.1f", 100 * x$coverage)))

# Sample size for 80% and 90% power
n_for_power <- function(th, dr, target) {
  x <- mm[mm$theta == th & mm$dropout == dr, ]
  x <- x[order(x$n), ]
  if (nrow(x) < 2) return("-")
  pw <- cummax(x$rejection)
  if (pw[1] >= target) return(paste("30 or fewer"))
  if (max(pw) < target) return("more than 200")
  i <- which(pw >= target)[1]
  n <- x$n[i - 1] + (target - pw[i - 1]) / (pw[i] - pw[i - 1]) * (x$n[i] - x$n[i - 1])
  as.character(ceiling(n))
}
ss <- do.call(rbind, lapply(c(0.1, 0.2, 0.3, 0.4), function(th) {
  data.frame(theta = th, `Effect at V3 (pp)` = f3(true_effect(th)),
             `80% power, no dropout` = n_for_power(th, "none", 0.8),
             `90% power, no dropout` = n_for_power(th, "none", 0.9),
             `80% power, MAR 30%` = n_for_power(th, "MAR 30%", 0.8),
             `90% power, MAR 30%` = n_for_power(th, "MAR 30%", 0.9),
             check.names = FALSE)
}))

# MI comparison, with MMRM on the same scenarios
mi_keys <- unique(perf[perf$part == "mi", c("n", "theta", "dropout")])
cmp <- do.call(rbind, lapply(seq_len(nrow(mi_keys)), function(j) {
  k <- mi_keys[j, ]
  x <- perf[perf$n == k$n & perf$theta == k$theta & perf$dropout == k$dropout, ]
  x <- x[order(match(x$method, c("MMRM", "MI J2R", "MI CR", "MI MAR delta"))), ]
  data.frame(n = x$n, theta = x$theta, Dropout = x$dropout, Method = x$method,
             `Bias (MCSE)` = paste0(f3(x$bias), " (", f3(x$bias_mcse), ")"),
             `Emp SE` = f3(x$emp_se), `Model SE` = f3(x$mod_se),
             `Coverage %` = sprintf("%.1f", 100 * x$coverage),
             `Rejection % (MCSE)` = pc(x$rejection, x$rejection_mcse),
             check.names = FALSE)
}))
cmp <- cmp[order(cmp$theta, cmp$Dropout, cmp$n), ]

fail <- aggregate(cbind(n_rep, n_failed, n_cs_fallback) ~ part + method, data = perf, FUN = sum)

out <- c(
  "# Part B simulation: summary (SAP v1.0, Section 11)",
  "",
  paste0("- **Generated:** ", format(Sys.time(), "%Y-%m-%d %H:%M %Z"), " by `10_simulation_summary.R`"),
  paste0("- **Scenarios found:** MMRM ", if ("mmrm" %in% names(n_found)) n_found[["mmrm"]] else 0,
         " of ", n_expected[["mmrm"]],
         "; MI ", if ("mi" %in% names(n_found)) n_found[["mi"]] else 0, " of ", n_expected[["mi"]]),
  "- **Data-generating mechanism:** rounded estimates from the OASIS-2 primary MMRM (control mean change -0.75 and -1.55 pp at about 2 and 4 years; variances 1.10 and 0.82, covariance 0.49). Treatment effect: proportional slowing theta of mean decline. No participant data used.",
  "- **Truth:** effect at V3 = theta x 1.55 pp. Reference-based MI targets a different estimand; MI MAR with delta assumes active-arm dropouts do 0.3 pp worse than MAR predicts.",
  "- Monte Carlo SEs in brackets. Rejection at theta = 0 is the type I error; otherwise power.",
  "",
  "## Fitting failures", "", md_table(fail), "",
  "## MMRM: type I error, % (MCSE), theta = 0", "", md_table(t1), "",
  "## MMRM: power, %", "", md_table(pow), "",
  "## MMRM: bias / coverage %, theta = 0.2", "", md_table(bc), "",
  "## Sample size per arm (MMRM, interpolated between simulated n)", "", md_table(ss), "",
  "## MI methods against MMRM on the same scenarios", "",
  if (is.null(cmp) || nrow(cmp) == 0) "MI part not run yet." else md_table(cmp), ""
)
writeLines(out, file.path(sim_dir, "simulation_summary.md"))
cat(out, sep = "\n")
cat("\nWritten:", file.path(sim_dir, "simulation_summary.md"), "and performance.csv\n")
