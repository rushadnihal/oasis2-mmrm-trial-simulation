# ------------------------------------------------------------------------------
# 12_figures.R
#
# Purpose : Figures for the report and README (SAP v1.0 Section 13), drawn
#           from aggregate results only; no participant-level data are read.
#             F1 mean change in nWBV over time: MMRM at the analysis visits
#                and the continuous-time mean slope
#             F2 robustness of the primary result (sensitivity analyses)
#             F3 delta adjustment (missing-data sensitivity)
#             F4 simulated power of MMRM by sample size, effect and dropout
#             F5 simulated type I error of MMRM across dropout scenarios
#             F6 MI methods against MMRM: rejection rate by effect size
#           (The SAP's covariance heatmap is replaced by a table: with two
#           post-baseline visits it is a 2 x 2 matrix.)
# Inputs  : Part A tables from 11_results_export.R, in OASIS2_PARTA_DIR
#           (default: <OASIS2_REPORT_DIR>/partA_results if it exists, else
#           results/partA); results/simulation/performance.csv
# Outputs : PNG (300 dpi) in OASIS2_FIG_DIR (default results/figures)
# Colour  : light surface #fcfcfb; categorical slots blue #2a78d6, orange
#           #eb6834, aqua #1baf7a, yellow #eda100; ordinal blue ramp; palettes
#           checked with the dataviz validator. Every figure with two or more
#           series has a legend, and series also differ by shape or label.
# Run     : from the repository root, Rscript R/12_figures.R (needs ggplot2)
# ------------------------------------------------------------------------------

suppressPackageStartupMessages(library(ggplot2))

report_dir <- Sys.getenv("OASIS2_REPORT_DIR", unset = file.path(getwd(), "reports"))
parta_dir  <- Sys.getenv("OASIS2_PARTA_DIR", unset = "")
if (!nzchar(parta_dir)) {
  parta_dir <- if (dir.exists(file.path(report_dir, "partA_results")))
    file.path(report_dir, "partA_results") else file.path("results", "partA")
}
fig_dir  <- Sys.getenv("OASIS2_FIG_DIR", unset = file.path("results", "figures"))
sim_file <- Sys.getenv("OASIS2_SIM_PERF", unset = file.path("results", "simulation", "performance.csv"))
dir.create(fig_dir, recursive = TRUE, showWarnings = FALSE)
rd <- function(name) read.csv(file.path(parta_dir, paste0(name, ".csv")), stringsAsFactors = FALSE)

INK <- "#0b0b0b"; INK2 <- "#52514e"; MUTED <- "#898781"; GRID <- "#e1e0d9"; AXIS <- "#c3c2b7"
SURFACE <- "#fcfcfb"
CAT <- c("#2a78d6", "#eb6834", "#1baf7a", "#eda100")
RAMP4 <- c("#86b6ef", "#3987e5", "#1c5cab", "#0d366b")
RAMP5 <- c("#86b6ef", "#5598e7", "#256abf", "#184f95", "#0d366b")

theme_report <- function() {
  theme_minimal(base_size = 10) +
    theme(plot.background = element_rect(fill = SURFACE, colour = NA),
          panel.background = element_rect(fill = SURFACE, colour = NA),
          panel.grid.major = element_line(colour = GRID, linewidth = 0.3),
          panel.grid.minor = element_blank(),
          axis.line = element_line(colour = AXIS, linewidth = 0.4),
          axis.text = element_text(colour = INK2), axis.title = element_text(colour = INK2),
          plot.title = element_text(colour = INK, face = "bold", size = 11),
          plot.subtitle = element_text(colour = INK2, size = 9),
          plot.caption = element_text(colour = MUTED, size = 7.5, hjust = 0),
          strip.text = element_text(colour = INK, face = "bold"),
          legend.position = "top", legend.justification = "left",
          legend.text = element_text(colour = INK2), legend.title = element_text(colour = INK2))
}
save_fig <- function(p, name, w = 7, h = 4.2) {
  ggsave(file.path(fig_dir, paste0(name, ".png")), p, width = w, height = h, dpi = 300, bg = SURFACE)
}

# F1 trajectory ----------------------------------------------------------------------------
t3 <- rd("t3_visits"); t4 <- rd("t4_primary"); sl <- rd("t6b_continuous_slope")
pts <- data.frame(years = c(0, t3$median_years), est = c(0, t4$estimate),
                  lo = c(0, t4$lower), hi = c(0, t4$upper), n = c(NA, t4$n))
tt <- seq(0, 5, by = 0.05)
line <- data.frame(years = tt, est = sl$slope * tt, lo = sl$lower * tt, hi = sl$upper * tt)
f1 <- ggplot() +
  geom_ribbon(data = line, aes(years, ymin = lo, ymax = hi), fill = CAT[2], alpha = 0.15) +
  geom_line(data = line, aes(years, est, colour = "Continuous-time model (mean slope)"), linewidth = 0.7) +
  geom_errorbar(data = pts[-1, ], aes(years, ymin = lo, ymax = hi, colour = "MMRM, analysis visits"),
                width = 0.08, linewidth = 0.6) +
  geom_point(data = pts, aes(years, est, colour = "MMRM, analysis visits"), size = 2.6,
             stroke = 0.6, shape = 21, fill = CAT[1]) +
  geom_text(data = pts[-1, ], aes(years, hi, label = paste0("n = ", n)), vjust = -0.8,
            colour = INK2, size = 3) +
  scale_colour_manual(NULL, values = c("MMRM, analysis visits" = CAT[1],
                                       "Continuous-time model (mean slope)" = CAT[2])) +
  scale_x_continuous("Years from baseline", breaks = 0:5) +
  scale_y_continuous("Change in nWBV from baseline (percentage points)") +
  labs(title = "Normalised whole-brain volume declines steadily over four years",
       subtitle = "Participants with CDR 0 at baseline; mean change with 95% CIs",
       caption = "MMRM points at the median time of each analysis window. Continuous-time line: mean slope with 95% band, at mean age and education.") +
  theme_report()
save_fig(f1, "F1_trajectory")

# F2 robustness ----------------------------------------------------------------------------
t6 <- rd("t6_robustness")
t6 <- t6[!(t6$analysis == "Supportive population" & t6$population != "Baseline CDR 0"), ]
t6$label <- ifelse(t6$analysis == "Supportive population", "Supportive model (CDR 0 group)", t6$analysis)
t6$label <- factor(t6$label, levels = rev(c("Primary MMRM", "Compound symmetry", "Continuous-time",
                                             "Label-based visits", "Supportive model (CDR 0 group)")))
t6$vis <- ifelse(grepl("^Visit 2", t6$visit), "Analysis visit 2 (about 2 years)", "Analysis visit 3 (about 4 years)")
ref <- t6[t6$analysis == "Primary MMRM", ]
f2 <- ggplot(t6, aes(estimate, label)) +
  geom_rect(data = ref, aes(xmin = lower, xmax = upper, ymin = -Inf, ymax = Inf),
            inherit.aes = FALSE, fill = CAT[1], alpha = 0.08) +
  geom_vline(data = ref, aes(xintercept = estimate), colour = CAT[1], linewidth = 0.4, linetype = "dashed") +
  geom_errorbarh(aes(xmin = lower, xmax = upper), height = 0.2, colour = INK2, linewidth = 0.5) +
  geom_point(aes(shape = analysis == "Primary MMRM"), size = 2.4, colour = CAT[1], fill = CAT[1]) +
  scale_shape_manual(values = c(`TRUE` = 23, `FALSE` = 21), guide = "none") +
  facet_wrap(~ vis, scales = "free_x") +
  labs(x = "Change in nWBV from baseline (percentage points), 95% CI", y = NULL,
       title = "Every sensitivity analysis lies inside the primary 95% CI",
       subtitle = "Shaded band and dashed line: primary MMRM estimate and 95% CI",
       caption = "Continuous-time estimates at 1.90 and 3.97 years. Supportive model: all participants, estimate for the CDR 0 group.") +
  theme_report()
save_fig(f2, "F2_robustness", h = 3.8)

# F3 delta adjustment ------------------------------------------------------------------------
t7 <- rd("t7_delta")
t7$vis <- ifelse(t7$visit == "Visit 2", "Analysis visit 2 (about 2 years)", "Analysis visit 3 (about 4 years)")
mar <- t7[t7$delta == 0, ]
f3 <- ggplot(t7, aes(delta, estimate)) +
  geom_rect(data = mar, aes(xmin = -Inf, xmax = Inf, ymin = estimate - se, ymax = estimate + se),
            inherit.aes = FALSE, fill = MUTED, alpha = 0.15) +
  geom_ribbon(aes(ymin = lower, ymax = upper), fill = CAT[1], alpha = 0.15) +
  geom_line(colour = CAT[1], linewidth = 0.7) +
  geom_point(colour = CAT[1], size = 1.6) +
  geom_hline(yintercept = 0, colour = AXIS, linewidth = 0.4) +
  facet_wrap(~ vis) +
  scale_x_reverse("Delta: extra decline assumed for missing values (percentage points)") +
  labs(y = "Change in nWBV (percentage points), 95% CI",
       title = "The two-year estimate barely moves; the four-year estimate shifts but stays negative",
       subtitle = "Multiple imputation with every imputed value shifted by delta",
       caption = "Grey band: MAR estimate plus or minus one standard error (the SAP's threshold for a material shift).") +
  theme_report()
save_fig(f3, "F3_delta")

# Simulation figures --------------------------------------------------------------------------
perf <- read.csv(sim_file, stringsAsFactors = FALSE)
mm <- perf[perf$method == "MMRM", ]

# F4 power
pw <- mm[mm$theta > 0 & mm$dropout %in% c("none", "MAR 30%", "MNAR 30%", "MAR 50%"), ]
pw$dropout <- factor(pw$dropout, levels = c("none", "MAR 30%", "MAR 50%", "MNAR 30%"),
                     labels = c("No dropout", "MAR 30%", "MAR 50%", "MNAR 30% (power inflated by bias)"))
pw$theta_lab <- factor(paste0(100 * pw$theta, "%"), levels = paste0(c(10, 20, 30, 40), "%"))
f4 <- ggplot(pw, aes(n, 100 * rejection, colour = theta_lab, shape = theta_lab)) +
  geom_hline(yintercept = c(80, 90), colour = MUTED, linewidth = 0.3, linetype = "dotted") +
  geom_line(linewidth = 0.7) + geom_point(size = 2) +
  scale_colour_manual("Slowing of decline", values = RAMP4) +
  scale_shape_manual("Slowing of decline", values = c(16, 17, 15, 18)) +
  scale_x_continuous("Participants per arm", breaks = c(30, 50, 80, 120, 200)) +
  scale_y_continuous("Power (%)", limits = c(0, 100), breaks = seq(0, 100, 20)) +
  facet_wrap(~ dropout, nrow = 1) +
  labs(title = "Simulated power of MMRM for a two-arm trial with a four-year endpoint",
       subtitle = "Effect: proportional slowing of mean decline in nWBV; dotted lines at 80% and 90%",
       caption = "2000 simulated trials per point. Data-generating mechanism from rounded OASIS-2 estimates.") +
  theme_report()
save_fig(f4, "F4_power", w = 8, h = 3.8)

# F5 type I error
t1e <- mm[mm$theta == 0, ]
t1e$dropout <- factor(t1e$dropout, levels = rev(c("none", paste("MCAR", c("10%", "20%", "30%", "50%")),
                                                   paste("MAR", c("10%", "20%", "30%", "50%")),
                                                   paste("MNAR", c("10%", "20%", "30%")))))
t1e$n_lab <- factor(t1e$n, levels = c(30, 50, 80, 120, 200))
lim <- 100 * (0.05 + c(-1, 1) * qnorm(0.975) * sqrt(0.05 * 0.95 / 5000))
f5 <- ggplot(t1e, aes(dropout, 100 * rejection, colour = n_lab, shape = n_lab)) +
  annotate("rect", ymin = lim[1], ymax = lim[2], xmin = -Inf, xmax = Inf, fill = MUTED, alpha = 0.15) +
  geom_hline(yintercept = 5, colour = MUTED, linewidth = 0.4) +
  geom_point(size = 2, position = position_dodge(width = 0.6)) +
  coord_flip() +
  scale_colour_manual("Participants per arm", values = RAMP5) +
  scale_shape_manual("Participants per arm", values = c(16, 17, 15, 18, 8)) +
  labs(y = "Type I error (%)", x = NULL,
       title = "MMRM holds 5% under ignorable dropout but not under differential MNAR",
       subtitle = "No treatment effect; 5000 simulated trials per point",
       caption = sprintf("Grey band: %.1f%% to %.1f%%, the Monte Carlo 95%% range for a true 5%% rate.", lim[1], lim[2])) +
  theme_report()
save_fig(f5, "F5_type1", h = 4.6)

# F6 MI methods against MMRM (n = 200)
mi_scen <- unique(perf[perf$part == "mi", c("n", "theta", "dropout")])
cmp <- merge(perf, mi_scen, by = c("n", "theta", "dropout"))
cmp <- cmp[cmp$n == 200, ]
cmp$method <- factor(cmp$method, levels = c("MMRM", "MI J2R", "MI CR", "MI MAR delta"),
                     labels = c("MMRM (MAR)", "MI jump to reference", "MI copy reference",
                                "MI MAR, delta -0.3 in active arm"))
cmp$dropout <- factor(cmp$dropout, levels = c("MAR 30%", "MAR 50%", "MNAR 30%"))
f6 <- ggplot(cmp, aes(100 * theta, 100 * rejection, colour = method, shape = method)) +
  geom_hline(yintercept = 5, colour = MUTED, linewidth = 0.3, linetype = "dotted") +
  geom_line(linewidth = 0.7) + geom_point(size = 2.2) +
  scale_colour_manual(NULL, values = CAT) +
  scale_shape_manual(NULL, values = c(16, 17, 15, 18)) +
  scale_x_continuous("Slowing of decline (%)", breaks = c(0, 20, 40)) +
  scale_y_continuous("Rejection rate (%)", limits = c(0, 100)) +
  facet_wrap(~ dropout, nrow = 1) +
  guides(colour = guide_legend(nrow = 2), shape = guide_legend(nrow = 2)) +
  labs(title = "Reference-based imputation is conservative and less powerful",
       subtitle = "200 participants per arm; at 0% the rejection rate is the type I error (dotted line at 5%)",
       caption = "Reference-based MI and MAR MMRM target different estimands; power is not a like-for-like comparison. 2000 trials per point.") +
  theme_report()
save_fig(f6, "F6_mi_methods", w = 8, h = 4)

cat("Figures written to", fig_dir, ":\n")
print(list.files(fig_dir))
