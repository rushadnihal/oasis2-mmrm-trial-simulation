# ------------------------------------------------------------------------------
# 12_results_simulation.R
#
# Purpose : Publication tables and figures for Part B (SAP v1.0, Section 13)
#           from the simulation performance file.
#             T10 MI methods against MMRM (n = 200 per arm)
#             T11 sample size per arm for 80% and 90% power (MMRM)
#             F3  power by n per arm, effect size and dropout (MMRM)
#             F4  type I error by n per arm and dropout (MMRM)
#             F5  rejection rate by method, effect size and dropout
# Inputs  : <OASIS2_SIM_DIR>/performance.csv (default results/simulation)
# Outputs : OASIS2_RESULTS_DIR (default <OASIS2_REPORT_DIR>/publish/simulation):
#             tables_simulation.md, T10.csv, T11.csv, F3 to F5 as PNG
# Privacy : simulated data only.
# Run     : from the repository root, Rscript R/12_results_simulation.R
#           (needs ggplot2)
# ------------------------------------------------------------------------------

suppressPackageStartupMessages(library(ggplot2))
source("R/utils_mmrm.R")   # md_table
source("R/utils_plot.R")

sim_dir    <- Sys.getenv("OASIS2_SIM_DIR", file.path("results", "simulation"))
report_dir <- Sys.getenv("OASIS2_REPORT_DIR", unset = file.path(getwd(), "reports"))
out_dir    <- Sys.getenv("OASIS2_RESULTS_DIR", unset = file.path(report_dir, "publish", "simulation"))
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
perf <- read.csv(file.path(sim_dir, "performance.csv"), stringsAsFactors = FALSE)
mm <- perf[perf$method == "MMRM" & perf$part == "mmrm", ]
f3 <- function(x) sprintf("%.3f", x)
pc <- function(x) sprintf("%.1f", 100 * x)
tables_md <- character()
add_table <- function(id, title, df, note = NULL) {
  write.csv(df, file.path(out_dir, paste0(id, ".csv")), row.names = FALSE)
  tables_md <<- c(tables_md, paste0("## ", id, ": ", title), "", md_table(df), "",
                  if (!is.null(note)) c(note, ""))
}

# T10 MI methods against MMRM, n = 200 per arm ------------------------------------------
mi_n <- 200
x <- perf[perf$n == mi_n & perf$theta %in% c(0, 0.2) & perf$dropout %in% c("MAR 30%", "MNAR 30%"), ]
x <- x[x$part == "mi" | (x$part == "mmrm" & x$method == "MMRM"), ]
meth_order <- c("MMRM", "MI J2R", "MI CR", "MI MAR delta")
x <- x[order(x$theta, x$dropout, match(x$method, meth_order)), ]
t10 <- data.frame(
  `Slowing (theta)` = x$theta, Dropout = x$dropout,
  Method = c(MMRM = "MMRM (MAR)", `MI J2R` = "MI, jump to reference", `MI CR` = "MI, copy reference",
             `MI MAR delta` = "MI, MAR with delta -0.3 (active arm)")[x$method],
  Bias = f3(x$bias), `Empirical SE` = f3(x$emp_se), `Model SE` = f3(x$mod_se),
  `Coverage %` = pc(x$coverage), `Rejection %` = pc(x$rejection),
  check.names = FALSE)
add_table("T10", sprintf("Analysis methods compared, %d per arm", mi_n), t10,
          paste("Bias against the treatment-policy effect at the final visit (theta x 1.55 pp).",
                "Rejection % is type I error at theta = 0 and power otherwise. 2000 replicates per MI scenario;",
                "Monte Carlo SE of a 5% rate about 0.5 points. Reference-based MI targets a different estimand."))

# T11 Sample size --------------------------------------------------------------------------
n_for_power <- function(th, dr, target) {
  z <- mm[mm$theta == th & mm$dropout == dr, ]
  z <- z[order(z$n), ]
  pw <- cummax(z$rejection)
  if (pw[1] >= target) return("30 or fewer")
  if (max(pw) < target) return("more than 200")
  i <- which(pw >= target)[1]
  as.character(ceiling(z$n[i - 1] + (target - pw[i - 1]) / (pw[i] - pw[i - 1]) * (z$n[i] - z$n[i - 1])))
}
t11 <- do.call(rbind, lapply(c(0.1, 0.2, 0.3, 0.4), function(th) data.frame(
  `Slowing of decline` = sprintf("%.0f%%", 100 * th),
  `Effect at final visit, pp` = sprintf("%.3f", 1.55 * th),
  `80% power` = n_for_power(th, "none", 0.8), `90% power` = n_for_power(th, "none", 0.9),
  `80% power, 30% MAR dropout` = n_for_power(th, "MAR 30%", 0.8),
  `90% power, 30% MAR dropout` = n_for_power(th, "MAR 30%", 0.9),
  check.names = FALSE)))
add_table("T11", "Participants per arm needed (MMRM, two-sided 5%)", t11,
          "Linear interpolation between simulated sample sizes of 30, 50, 80, 120 and 200 per arm.")

# F3 Power ---------------------------------------------------------------------------------
drop4 <- c("none", "MAR 30%", "MAR 50%", "MNAR 30%")
lab4  <- c("No dropout", "MAR 30%", "MAR 50%", "MNAR 30% (biased)")
p3 <- mm[mm$theta > 0 & mm$dropout %in% drop4, ]
p3$dropout <- factor(p3$dropout, drop4, lab4)
p3$facet <- factor(sprintf("%.0f%% slowing (%.2f pp)", 100 * p3$theta, 1.55 * p3$theta))
f3p <- ggplot(p3, aes(n, 100 * rejection, colour = dropout, linetype = dropout, shape = dropout)) +
  geom_hline(yintercept = 80, colour = PAL$muted, linewidth = 0.3) +
  geom_line(linewidth = 0.7) + geom_point(size = 2) +
  facet_wrap(~ facet, nrow = 1) +
  scale_colour_manual(values = setNames(PAL$series, lab4), name = NULL) +
  scale_linetype_manual(values = setNames(c("solid", "solid", "solid", "dashed"), lab4), name = NULL) +
  scale_shape_manual(values = setNames(c(16, 17, 15, 1), lab4), name = NULL) +
  scale_x_continuous("Participants per arm", breaks = c(30, 80, 120, 200)) +
  scale_y_continuous("Power, %", limits = c(0, 100), breaks = seq(0, 100, 20)) +
  labs(title = "Power of the MMRM by sample size, effect size and dropout",
       subtitle = "Effect: proportional slowing of mean decline at about 4 years; grey line at 80%",
       caption = "2000 replicates per point. Under MNAR the MMRM is biased in favour of treatment, so its higher rejection rate is not real power.") +
  theme_report()
save_fig(f3p, file.path(out_dir, "F3_power.png"), width = 7, height = 3.4)

# F4 Type I error ---------------------------------------------------------------------------
p4 <- mm[mm$theta == 0 & mm$dropout != "none", ]
p4$mech <- factor(sub(" .*", "", p4$dropout), c("MCAR", "MAR", "MNAR"))
p4$rate <- factor(sub(".* ", "", p4$dropout), c("10%", "20%", "30%", "50%"))
none <- mm[mm$theta == 0 & mm$dropout == "none", ]
none <- do.call(rbind, lapply(levels(p4$mech), function(m) transform(none, mech = m)))
none$mech <- factor(none$mech, levels(p4$mech))
lim <- 100 * (0.05 + c(-1, 1) * qnorm(0.975) * sqrt(0.05 * 0.95 / 5000))
f4p <- ggplot(p4, aes(n, 100 * rejection)) +
  annotate("rect", xmin = -Inf, xmax = Inf, ymin = lim[1], ymax = lim[2], fill = PAL$grid, alpha = 0.6) +
  geom_hline(yintercept = 5, colour = PAL$muted, linewidth = 0.3) +
  geom_line(data = none, aes(linetype = "No dropout"), colour = PAL$muted, linewidth = 0.6) +
  geom_line(aes(colour = rate), linewidth = 0.7) + geom_point(aes(colour = rate), size = 1.8) +
  facet_wrap(~ mech, nrow = 1) +
  scale_colour_manual(values = setNames(PAL$seq_blue, c("10%", "20%", "30%", "50%")),
                      name = "Missing at final visit") +
  scale_linetype_manual(values = c(`No dropout` = "dashed"), name = NULL) +
  scale_x_continuous("Participants per arm", breaks = c(30, 80, 120, 200)) +
  scale_y_continuous("Type I error, %", limits = c(2, 10), breaks = seq(2, 10, 2)) +
  labs(title = "Type I error of the MMRM by dropout mechanism",
       subtitle = sprintf("Shaded band: Monte Carlo 95%% range around 5%% for 5000 replicates (%.1f%% to %.1f%%)", lim[1], lim[2]),
       caption = "MNAR: dropout depends on the unobserved final value and is 1.5 times as frequent in the active arm. 50% scenarios exist for MCAR and MAR only.") +
  theme_report()
save_fig(f4p, file.path(out_dir, "F4_type1_error.png"), width = 7, height = 3.4)

# F5 Methods compared -----------------------------------------------------------------------
p5 <- perf[perf$n == mi_n & (perf$part == "mi" | perf$method == "MMRM") &
           perf$dropout %in% c("MAR 30%", "MNAR 30%", "MAR 50%") & perf$theta %in% c(0, 0.2, 0.4), ]
p5 <- p5[p5$part == "mi" | p5$part == "mmrm", ]
p5$method <- factor(p5$method, meth_order, c("MMRM (MAR)", "MI jump to reference", "MI copy reference",
                                             "MI MAR, delta -0.3"))
p5$dropout <- factor(p5$dropout, c("MAR 30%", "MAR 50%", "MNAR 30%"))
f5p <- ggplot(p5, aes(theta, 100 * rejection, colour = method, shape = method, linetype = method)) +
  geom_hline(yintercept = 5, colour = PAL$muted, linewidth = 0.3) +
  geom_line(linewidth = 0.7) + geom_point(size = 2.2) +
  facet_wrap(~ dropout, nrow = 1) +
  scale_colour_manual(values = setNames(PAL$series, levels(p5$method)), name = NULL) +
  scale_shape_manual(values = setNames(c(16, 17, 15, 18), levels(p5$method)), name = NULL) +
  scale_linetype_manual(values = setNames(c("solid", "dashed", "dotted", "dotdash"), levels(p5$method)), name = NULL) +
  scale_x_continuous("Proportional slowing of decline (theta)", breaks = c(0, 0.2, 0.4)) +
  scale_y_continuous("Rejection rate, %", limits = c(0, 100), breaks = seq(0, 100, 20)) +
  labs(title = sprintf("Rejection rate by analysis method, %d per arm", mi_n),
       subtitle = "Type I error at theta = 0, power otherwise; grey line at 5%",
       caption = "Reference-based MI targets a different estimand (dropouts lose the treatment benefit). 2000 replicates per point.") +
  guides(colour = guide_legend(nrow = 2), shape = guide_legend(nrow = 2), linetype = guide_legend(nrow = 2)) +
  theme_report()
save_fig(f5p, file.path(out_dir, "F5_methods.png"), width = 7, height = 3.6)

writeLines(c("# Part B tables (simulated data)", "",
             paste0("Generated ", format(Sys.time(), "%Y-%m-%d %H:%M %Z"), " by `12_results_simulation.R`."), "",
             tables_md), file.path(out_dir, "tables_simulation.md"))
cat(tables_md, sep = "\n")
cat("\nWritten to", out_dir, "\n")
