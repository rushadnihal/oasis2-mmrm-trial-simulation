# ------------------------------------------------------------------------------
# 09_simulation_full.R
#
# Purpose : Full Part B simulation (SAP v1.0, Section 11; MI grid reduced
#           after the pilot, D-015).
#             Part "mmrm": MMRM on the full grid, 5 n x 5 theta x 12 dropout
#                          scenarios = 300 scenarios; 5000 replicates at
#                          theta = 0, 2000 otherwise.
#             Part "mi":   MI J2R, MI CR and MI MAR with delta = -0.3 in the
#                          active arm, on n in {80, 200}, theta in {0, 0.2,
#                          0.4}, dropout in {MAR 30%, MNAR 30%, MAR 50%}
#                          = 18 scenarios; 2000 replicates.
# Resume  : each scenario is saved to its own file when it finishes. Running
#           the script again skips finished scenarios, so an interrupted run
#           continues where it stopped. Delete the raw folder to start over.
# RNG     : L'Ecuyer-CMRG. Each scenario has its own stream (derived from the
#           seed and the scenario's position in the grid) and each replicate
#           its own substream, so results do not depend on the number of cores
#           or on interruptions.
# Settings (environment variables, defaults in brackets):
#           SIM_PART [all] = mmrm, mi or all
#           SIM_REPS_SCALE [1] multiplies replicate counts (CI uses a tiny value)
#           SIM_NSAMPLES [20] rbmi imputations
#           SIM_CORES [all cores minus one]
#           SIM_SEED [20261117]
#           OASIS2_SIM_DIR [results/simulation]
# Inputs  : none (no participant data; parameters in R/sim/sim_functions.R)
# Outputs : <OASIS2_SIM_DIR>/raw/<scenario>.rds, one per scenario (replicate-
#           level estimates; simulated, not participant data)
#           <OASIS2_SIM_DIR>/progress.log
# Run     : from the repository root, ideally overnight:
#             caffeinate -i Rscript R/09_simulation_full.R
#           then Rscript R/10_simulation_summary.R
# ------------------------------------------------------------------------------

suppressPackageStartupMessages({
  library(parallel)
  library(mmrm)
  library(rbmi)
})
source("R/sim/sim_functions.R")

part      <- Sys.getenv("SIM_PART", "all")
scale     <- as.numeric(Sys.getenv("SIM_REPS_SCALE", "1"))
n_samples <- as.integer(Sys.getenv("SIM_NSAMPLES", "20"))
cores     <- as.integer(Sys.getenv("SIM_CORES", max(1, detectCores() - 1)))
seed      <- as.integer(Sys.getenv("SIM_SEED", "20261117"))
DELTA     <- -0.3
sim_dir   <- Sys.getenv("OASIS2_SIM_DIR", file.path("results", "simulation"))
raw_dir   <- file.path(sim_dir, "raw")
dir.create(raw_dir, recursive = TRUE, showWarnings = FALSE)
log_file  <- file.path(sim_dir, "progress.log")
stopifnot(part %in% c("all", "mmrm", "mi"))

grid <- full_grid()
grid$reps <- pmax(2, round(grid$reps * scale))

# Scenario streams: fixed by position in the full grid, whatever is run
RNGkind("L'Ecuyer-CMRG")
set.seed(seed)
s <- .Random.seed
grid_streams <- vector("list", nrow(grid))
for (k in seq_len(nrow(grid))) { grid_streams[[k]] <- s; s <- nextRNGStream(s) }

todo <- which((part == "all" | grid$part == part) &
              !file.exists(file.path(raw_dir, paste0(grid$key, ".rds"))))

logmsg <- function(...) {
  line <- paste0(format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "  ", ...)
  cat(line, "\n")
  cat(line, "\n", file = log_file, append = TRUE)
}
logmsg("Start: part = ", part, "; ", length(todo), " scenarios to run (",
       sum(grid$part[todo] == "mmrm"), " MMRM, ", sum(grid$part[todo] == "mi"), " MI); ",
       cores, " cores; rbmi imputations ", n_samples, "; replicate scale ", scale)

run_scenario <- function(k) {
  sc <- grid[k, ]
  subs <- vector("list", sc$reps)
  ss <- grid_streams[[k]]
  for (r in seq_len(sc$reps)) { subs[[r]] <- ss; ss <- nextRNGSubStream(ss) }
  one <- function(r) {
    assign(".Random.seed", subs[[r]], envir = .GlobalEnv)
    d <- simulate_trial(sc$n, sc$theta, sc$mech, sc$p)
    t0 <- proc.time()[["elapsed"]]
    out <- if (sc$part == "mmrm") analyse_mmrm(d) else analyse_rbmi(d, n_samples, delta = DELTA)
    out$seconds <- proc.time()[["elapsed"]] - t0
    out$rep <- r
    out
  }
  res <- mclapply(seq_len(sc$reps), one, mc.cores = cores, mc.preschedule = TRUE)
  bad <- vapply(res, inherits, logical(1), "try-error")
  if (any(bad)) stop("Scenario ", sc$key, ": ", sum(bad), " replicates crashed. First: ",
                     as.character(res[[which(bad)[1]]]))
  res <- do.call(rbind, res)
  cbind(sc[rep(1, nrow(res)), c("key", "part", "n", "theta", "dropout", "mech", "p")], res,
        row.names = NULL)
}

t_start <- Sys.time()
for (i in seq_along(todo)) {
  k <- todo[i]
  t0 <- Sys.time()
  res <- run_scenario(k)
  saveRDS(res, file.path(raw_dir, paste0(grid$key[k], ".rds")))
  done_min <- as.numeric(difftime(Sys.time(), t_start, units = "mins"))
  logmsg(sprintf("[%d/%d] %s: %d reps, %d failed, %.1f min. Elapsed %.0f min.",
                 i, length(todo), grid$key[k], grid$reps[k], sum(res$failed),
                 as.numeric(difftime(Sys.time(), t0, units = "mins")), done_min))
}
logmsg("Finished. Total ", sprintf("%.0f", as.numeric(difftime(Sys.time(), t_start, units = "mins"))),
       " minutes. Next: Rscript R/10_simulation_summary.R")
