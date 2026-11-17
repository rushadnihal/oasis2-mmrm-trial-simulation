# ------------------------------------------------------------------------------
# 08_simulation_pilot.R
#
# Purpose : Pilot of the Part B simulation (SAP v1.0, Section 11). Runs a
#           small set of scenarios to (1) check that the code behaves
#           (type I error near 5% under no dropout, bias near zero under MAR),
#           and (2) time each analysis method, so the full grid can be sized
#           before it is run. Any reduction of the grid is decided from the
#           timings here and logged (D-008).
# Scenarios: n per arm in {80, 200} x theta in {0, 0.3} x dropout in
#           {none, MAR 30%, MNAR 30%} = 12 scenarios.
# Methods : MMRM (all scenarios); MI jump-to-reference and copy-reference
#           with rbmi (scenarios with dropout).
# Settings (environment variables, defaults in brackets):
#           SIM_REPS [200], SIM_NSAMPLES [20] rbmi imputations,
#           SIM_CORES [all cores minus one], SIM_SEED [20261117]
# Inputs  : none (no participant data; parameters in R/sim/sim_functions.R)
# Outputs : In OASIS2_REPORT_DIR (default reports/):
#             08_simulation_pilot_report.md, 08_pilot_reps.rds,
#             08_sessioninfo.txt
# Run     : from the repository root, Rscript R/08_simulation_pilot.R
#           (needs mmrm and rbmi)
# ------------------------------------------------------------------------------

suppressPackageStartupMessages({
  library(parallel)
  library(mmrm)
  library(rbmi)
})
source("R/sim/sim_functions.R")
source("R/utils_mmrm.R")   # md_table

reps      <- as.integer(Sys.getenv("SIM_REPS", "200"))
n_samples <- as.integer(Sys.getenv("SIM_NSAMPLES", "20"))
cores     <- as.integer(Sys.getenv("SIM_CORES", max(1, detectCores() - 1)))
seed      <- as.integer(Sys.getenv("SIM_SEED", "20261117"))
report_dir <- Sys.getenv("OASIS2_REPORT_DIR", unset = file.path(getwd(), "reports"))
dir.create(report_dir, recursive = TRUE, showWarnings = FALSE)

grid <- expand.grid(n = c(80, 200), theta = c(0, 0.3),
                    dropout = c("none", "MAR 30%", "MNAR 30%"), stringsAsFactors = FALSE)
grid$mech <- c(none = "none", `MAR 30%` = "MAR", `MNAR 30%` = "MNAR")[grid$dropout]
grid$p    <- ifelse(grid$mech == "none", 0, 0.3)
grid$scenario <- seq_len(nrow(grid))

# One independent L'Ecuyer-CMRG stream per replicate, independent of core count
RNGkind("L'Ecuyer-CMRG")
set.seed(seed)
n_total <- nrow(grid) * reps
streams <- vector("list", n_total)
s <- .Random.seed
for (i in seq_len(n_total)) { streams[[i]] <- s; s <- nextRNGStream(s) }

one_rep <- function(i) {
  sc <- grid[(i - 1) %/% reps + 1, ]
  assign(".Random.seed", streams[[i]], envir = .GlobalEnv)
  d <- simulate_trial(sc$n, sc$theta, sc$mech, sc$p)
  t1 <- system.time(r1 <- analyse_mmrm(d))[["elapsed"]]
  r1$seconds <- t1
  out <- r1
  if (sc$mech != "none") {
    t2 <- system.time(r2 <- analyse_rbmi(d, n_samples))[["elapsed"]]
    if (nrow(r2) > 0) { r2$seconds <- t2; out <- rbind(out, r2) }
  }
  out$scenario <- sc$scenario
  out$rep <- (i - 1) %% reps + 1
  out
}

t_start <- Sys.time()
res <- mclapply(seq_len(n_total), one_rep, mc.cores = cores, mc.preschedule = FALSE)
wall <- as.numeric(difftime(Sys.time(), t_start, units = "mins"))
bad <- vapply(res, inherits, logical(1), "try-error")
if (any(bad)) stop("Replicates crashed: ", sum(bad), ". First error: ", res[[which(bad)[1]]])
res <- do.call(rbind, res)
res <- merge(res, grid, by = "scenario")
saveRDS(res, file.path(report_dir, "08_pilot_reps.rds"))

# Performance --------------------------------------------------------------------------
keys <- unique(res[, c("scenario", "method")])
perf <- do.call(rbind, lapply(seq_len(nrow(keys)), function(j) {
  r <- res[res$scenario == keys$scenario[j] & res$method == keys$method[j], ]
  cbind(grid[grid$scenario == keys$scenario[j], c("n", "theta", "dropout")],
        method = keys$method[j],
        performance(r$est, r$se, r$lci, r$uci, r$failed, true_effect(r$theta[1])),
        n_cs_fallback = sum(r$cov == "cs"))
}))
perf <- perf[order(perf$theta, perf$dropout, perf$n, perf$method), ]
f3 <- function(x) sprintf("%.3f", x)
pct <- function(x, m) sprintf("%.1f (%.1f)", 100 * x, 100 * m)
perf_tab <- data.frame(
  n = perf$n, theta = perf$theta, Dropout = perf$dropout, Method = perf$method,
  Reps = perf$n_rep, Failed = perf$n_failed, `CS fallback` = perf$n_cs_fallback,
  `Bias (MCSE)` = paste0(f3(perf$bias), " (", f3(perf$bias_mcse), ")"),
  `Emp SE` = f3(perf$emp_se), `Model SE` = f3(perf$mod_se),
  `Coverage % (MCSE)` = pct(perf$coverage, perf$coverage_mcse),
  `Rejection % (MCSE)` = pct(perf$rejection, perf$rejection_mcse),
  check.names = FALSE)

# Timing and projection for the full grid -------------------------------------------
tm <- aggregate(seconds ~ method + n, data = res[!res$failed, ], FUN = mean)
time_at <- function(method, n) {
  x <- tm[tm$method == method, ]
  if (nrow(x) < 2) return(NA)
  approx(x$n, x$seconds, xout = n, rule = 2)$y   # linear in n, flat outside 80 to 200
}
full_n <- c(30, 50, 80, 120, 200)
reps_per_n <- 5000 + 4 * 2000          # theta = 0 at 5000, four other thetas at 2000
n_drop_all <- 12                       # dropout scenarios in the full grid
n_drop_mi  <- 11                       # those with missing data (MI applies)
cpu_hours <- function(method, n_drop) sum(vapply(full_n, function(n) time_at(method, n), 0)) *
  reps_per_n * n_drop / 3600
proj <- data.frame(
  Method = c("MMRM", "MI J2R + MI CR (one rbmi call)"),
  `Seconds per trial at n = 80` = c(time_at("MMRM", 80), time_at("MI J2R", 80)),
  `Seconds per trial at n = 200` = c(time_at("MMRM", 200), time_at("MI J2R", 200)),
  `Full grid, CPU hours` = c(cpu_hours("MMRM", n_drop_all), cpu_hours("MI J2R", n_drop_mi)),
  check.names = FALSE)
proj$`Full grid, hours on this machine` <- proj$`Full grid, CPU hours` / cores
proj[, -1] <- lapply(proj[, -1], function(x) sprintf("%.2f", x))

out <- c(
  "# Report 08: Simulation pilot (SAP v1.0, Section 11)",
  "",
  paste0("- **Generated:** ", format(Sys.time(), "%Y-%m-%d %H:%M %Z"), " by `08_simulation_pilot.R`"),
  paste0("- **Replicates per scenario:** ", reps, "; rbmi imputations: ", n_samples,
         "; cores: ", cores, "; seed: ", seed, " (L'Ecuyer-CMRG, one stream per replicate)"),
  paste0("- **Wall time:** ", sprintf("%.1f", wall), " minutes"),
  paste0("- **mmrm** ", packageVersion("mmrm"), ", **rbmi** ", packageVersion("rbmi"),
         ", **R** ", getRversion()),
  "- **Truth:** treatment effect at V3 = theta x 1.55 percentage points (less decline in the active arm). Reference-based methods target a different estimand, so their bias against this truth is expected under dropout.",
  "",
  "## Performance", "",
  md_table(perf_tab), "",
  paste0("With ", reps, " replicates, the Monte Carlo SE of a 5% rejection rate is about ",
         sprintf("%.1f", 100 * sqrt(0.05 * 0.95 / reps)), " percentage points."), "",
  "## Timing and projection for the full grid", "",
  md_table(proj), "",
  "Full grid: n per arm in {30, 50, 80, 120, 200}, theta in {0, 0.1, 0.2, 0.3, 0.4} (5000 replicates at theta = 0, 2000 otherwise), 12 dropout scenarios (11 with missing data). Times for n = 30, 50 and 120 are interpolated or held at the nearest measured value.", ""
)
writeLines(out, file.path(report_dir, "08_simulation_pilot_report.md"))
writeLines(capture.output(sessionInfo()), file.path(report_dir, "08_sessioninfo.txt"))
cat(out, sep = "\n")
cat("\nReport written to", file.path(report_dir, "08_simulation_pilot_report.md"), "\n")
