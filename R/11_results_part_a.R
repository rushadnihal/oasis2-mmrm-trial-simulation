# ------------------------------------------------------------------------------
# 11_results_part_a.R
#
# Purpose : Publication tables and figures for Part A (SAP v1.0, Section 13),
#           aggregate only, with the small-cell rule applied.
#             T1 participant flow          T2 baseline characteristics
#             T3 analysis visits           T4 primary MMRM (with the SAS-
#                                             equivalent CI, D-014)
#             T5 covariance comparison     T6 robustness
#             T7 delta adjustment          T8 secondary endpoints
#             F1 mean change over time     F2 delta-adjustment curve
#           T4 and T5 are refitted here; T6 to T8 and F2 are read from the
#           estimates files written by scripts 04 to 06.
# Inputs  : derived/analysis_long.rds and analysis_subj.rds (OASIS2_DATA_DIR);
#           04_robustness_estimates.csv, 05_secondary_estimates.csv,
#           06_delta_estimates.csv (OASIS2_REPORT_DIR)
# Outputs : OASIS2_RESULTS_DIR (default <OASIS2_REPORT_DIR>/publish/part_a):
#             tables_part_a.md, T1..T8 as CSV, F1 and F2 as PNG
# Privacy : Every published count is 5 or more. Cells below 5 are merged or
#           withheld, including counts that a reader could recover by
#           subtraction from a total. Figures show group means only.
# Run     : from the repository root, Rscript R/11_results_part_a.R
#           (needs mmrm, emmeans, ggplot2)
# ------------------------------------------------------------------------------

suppressPackageStartupMessages({
  library(mmrm)
  library(emmeans)
  library(ggplot2)
})
source("R/utils_mmrm.R")
source("R/utils_plot.R")

data_dir   <- Sys.getenv("OASIS2_DATA_DIR", unset = getwd())
report_dir <- Sys.getenv("OASIS2_REPORT_DIR", unset = file.path(getwd(), "reports"))
out_dir    <- Sys.getenv("OASIS2_RESULTS_DIR", unset = file.path(report_dir, "publish", "part_a"))
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
need <- c(file.path(data_dir, "derived", c("analysis_long.rds", "analysis_subj.rds")),
          file.path(report_dir, c("04_robustness_estimates.csv", "05_secondary_estimates.csv",
                                  "06_delta_estimates.csv")))
missing <- need[!file.exists(need)]
if (length(missing)) stop("Missing inputs (run scripts 02 to 06 first): ", paste(missing, collapse = ", "))

long <- readRDS(need[1]); subj <- readRDS(need[2])
rob  <- read.csv(need[3], stringsAsFactors = FALSE)
sec  <- read.csv(need[4], stringsAsFactors = FALSE)
dlt  <- read.csv(need[5], stringsAsFactors = FALSE)

# Small-cell guard: stop rather than publish a count below 5
chk <- function(n) { if (any(n > 0 & n < 5)) stop("Small cell (<5) in a published table; merge first."); invisible(n) }
f2 <- function(x) sprintf("%.2f", x)
f3 <- function(x) sprintf("%.3f", x)
ci <- function(e, l, u, d = 3) sprintf(paste0("%.", d, "f (%.", d, "f to %.", d, "f)"), e, l, u)
tables_md <- character()
add_table <- function(id, title, df, note = NULL) {
  write.csv(df, file.path(out_dir, paste0(id, ".csv")), row.names = FALSE)
  tables_md <<- c(tables_md, paste0("## ", id, ": ", title), "", md_table(df), "",
                  if (!is.null(note)) c(note, ""))
}

pap  <- subj[subj$pap_fl, ]
supp <- subj[subj$supp_fl, ]

# T1 Participant flow ------------------------------------------------------------------
n_all <- nrow(subj); n_pap <- nrow(pap); n_cdr0 <- sum(subj$base_cdr == 0)
n_excl_pap <- n_cdr0 - n_pap
t1 <- data.frame(
  Step = c("Participants in OASIS-2",
           "CDR 0 at the first visit",
           "Primary analysis population (CDR 0, nWBV at baseline and at analysis visit 2 or 3)",
           "CDR 0.5 or above at the first visit"),
  Participants = chk(c(n_all, n_cdr0, n_pap, n_all - n_cdr0)))
note_t1 <- if (n_excl_pap == 0) "No participant with CDR 0 at the first visit was excluded from the primary analysis population." else
  "Participants with CDR 0 at the first visit but no session in either analysis window are excluded from the primary analysis population."
n_no_window <- n_all - nrow(supp)
note_t1 <- c(note_t1, paste0("The supportive population is all participants with a session in an analysis window; ",
  if (n_no_window == 0) "every participant had one." else if (n_no_window < 5)
    "fewer than 5 participants had none (count withheld)." else paste(n_no_window, "participants had none.")))
add_table("T1", "Participant flow", t1, note_t1)

# T2 Baseline characteristics -----------------------------------------------------------
desc <- function(x) {
  c(Participants = as.character(chk(nrow(x))),
    `Age, years, mean (SD)` = sprintf("%.1f (%.1f)", mean(x$age_bl), sd(x$age_bl)),
    `Women, n (%)` = { chk(sum(x$sex == "M")); sprintf("%d (%.0f%%)", chk(sum(x$sex == "F")), 100 * mean(x$sex == "F")) },
    `Education, years, mean (SD)` = sprintf("%.1f (%.1f)", mean(x$educ), sd(x$educ)),
    `nWBV, percentage points, mean (SD)` = sprintf("%.2f (%.2f)", mean(x$base_nwbv), sd(x$base_nwbv)),
    `MMSE, median (IQR)` = sprintf("%.0f (%.0f to %.0f)", median(x$base_mmse, na.rm = TRUE),
                                   quantile(x$base_mmse, 0.25, na.rm = TRUE),
                                   quantile(x$base_mmse, 0.75, na.rm = TRUE)))
}
cdr05 <- subj[subj$base_cdr > 0, ]   # all, so no count differs from T1 by fewer than 5
t2 <- data.frame(Characteristic = names(desc(pap)),
                 `CDR 0 (primary population)` = unname(desc(pap)),
                 `CDR 0.5 or above` = unname(desc(cdr05)),
                 check.names = FALSE)
add_table("T2", "Baseline characteristics", t2,
          "CDR 0.5 or above: all participants with CDR 0.5 or above at the first visit (descriptive only).")

# T3 Analysis visits ------------------------------------------------------------------------
lp <- long[long$pap_fl & long$mmrm_fl, ]
t3 <- do.call(rbind, lapply(2:3, function(k) {
  x <- lp$years_since_bl[lp$avisit == k]
  q <- quantile(x, c(0.5, 0.25, 0.75))
  data.frame(`Analysis visit` = k,
             Window = c("1 to under 3 years", "3 to under 5 years")[k - 1],
             Participants = chk(length(x)),
             `Percent of population` = sprintf("%.0f%%", 100 * length(x) / n_pap),
             `Years from baseline, median (IQR)` = sprintf("%.2f (%.2f to %.2f)", q[1], q[2], q[3]),
             check.names = FALSE)
}))
add_table("T3", "Analysis visits, primary analysis population", t3)

# T4 Primary MMRM and T5 covariance comparison (refitted) -------------------------------------
ad <- lp[, c("id", "avisit", "chg_nwbv", "base_nwbv_c", "age_bl_c", "educ_c")]
ad$avisit <- factor(ad$avisit, levels = c(2, 3), labels = c("Visit 2", "Visit 3"))
ad$id <- factor(ad$id)
ad <- ad[order(ad$id, ad$avisit), ]
rhs <- "chg_nwbv ~ avisit * base_nwbv_c + age_bl_c + educ_c"
fit_v <- function(cov, vcov) mmrm(as.formula(paste(rhs, "+", cov, "(avisit | id)")), data = ad,
                                  reml = TRUE, method = "Kenward-Roger", vcov = vcov)
fit_a  <- fit_v("us", "Kenward-Roger")
fit_b  <- fit_v("us", "Kenward-Roger-Linear")
fit_cs <- fit_v("cs", "Kenward-Roger")
stopifnot(component(fit_a, "convergence") == 0, component(fit_b, "convergence") == 0)
la <- ls_means(fit_a); lb <- ls_means(fit_b)
t4 <- data.frame(
  `Analysis visit` = c("2 (primary)", "3"),
  Participants = chk(as.integer(table(ad$avisit))),
  `Mean change, pp (95% CI)` = ci(la$emmean, la$lower.CL, la$upper.CL),
  SE = f3(la$SE), df = sprintf("%.1f", la$df),
  `95% CI, SAS DDFM=KR equivalent` = sprintf("%.3f to %.3f", lb$lower.CL, lb$upper.CL),
  check.names = FALSE)
add_table("T4", "Primary analysis: MMRM, change from baseline in nWBV", t4,
          c("Unstructured covariance, REML, Kenward-Roger. Adjusted for baseline nWBV (with its interaction with visit), age and education, at the population means. pp: percentage points of intracranial volume.",
            "Last column: supplementary interval with the linear Kenward-Roger covariance adjustment, which reproduces SAS PROC MIXED DDFM=KR (SAS/R cross-check, D-014)."))
vc <- component(fit_a, "varcor")
t5 <- data.frame(
  Structure = c("Unstructured (primary)", "Compound symmetry"),
  `Covariance parameters` = c(component(fit_a, "n_theta"), component(fit_cs, "n_theta")),
  AIC = f2(c(AIC(fit_a), AIC(fit_cs))), BIC = f2(c(BIC(fit_a), BIC(fit_cs))),
  check.names = FALSE)
add_table("T5", "Covariance structure comparison", t5,
          sprintf("Unstructured estimate: variance %.3f at visit 2 and %.3f at visit 3, correlation %.2f.",
                  vc[1, 1], vc[2, 2], cov2cor(vc)[1, 2]))

# T6 Robustness -----------------------------------------------------------------------------------
rob_rows <- list(c("Primary MMRM", "PAP"), c("Compound symmetry", "PAP"),
                 c("Continuous-time", "PAP (all sessions)"), c("Label-based visits", "PAP"),
                 c("Supportive population", "Baseline CDR 0"))
t6 <- do.call(rbind, lapply(rob_rows, function(r) {
  x <- rob[rob$analysis == r[1] & rob$population == r[2], ]
  data.frame(Analysis = c(`Primary MMRM` = "Primary MMRM",
                          `Compound symmetry` = "Compound symmetry covariance",
                          `Continuous-time` = "Continuous-time mixed model (at 1.90 and 3.97 years)",
                          `Label-based visits` = "Visit label instead of time windows",
                          `Supportive population` = "All participants, CDR 0 stratum")[[r[1]]],
             `Visit 2 (95% CI)` = ci(x$estimate[1], x$lower[1], x$upper[1]),
             `Visit 3 (95% CI)` = ci(x$estimate[2], x$lower[2], x$upper[2]),
             check.names = FALSE)
}))
add_table("T6", "Robustness of the primary result (change in nWBV, pp)", t6)

# T7 Delta adjustment --------------------------------------------------------------------------------
dsel <- dlt[dlt$delta %in% c(0, -0.2, -0.4, -0.6, -0.8, -1.0), ]
t7 <- do.call(rbind, lapply(sort(unique(dsel$delta), decreasing = TRUE), function(d) {
  x <- dsel[dsel$delta == d, ]
  data.frame(`Delta (pp)` = sprintf("%.1f", d),
             `Visit 2 (95% CI)` = ci(x$estimate[x$visit == "Visit 2"], x$lower[x$visit == "Visit 2"], x$upper[x$visit == "Visit 2"]),
             `Visit 3 (95% CI)` = ci(x$estimate[x$visit == "Visit 3"], x$lower[x$visit == "Visit 3"], x$upper[x$visit == "Visit 3"]),
             check.names = FALSE)
}))
add_table("T7", "Delta-adjusted multiple imputation (change in nWBV, pp)", t7,
          "Multiple imputation under MAR (delta = 0), then every imputed value shifted by delta. Full grid in F2.")

# T8 Secondary endpoints -------------------------------------------------------------------------------
mm <- sec[sec$analysis == "MMSE change", ]
cdr_n <- function(k) { x <- lp$cdr[lp$avisit == k]; c(sum(!is.na(x)), sum(x >= 0.5, na.rm = TRUE)) }
c2 <- cdr_n(2); c3 <- cdr_n(3)
wilson <- function(x, n, z = qnorm(0.975)) {
  p <- x / n; den <- 1 + z^2 / n; ctr <- (p + z^2 / (2 * n)) / den
  hw <- z * sqrt(p * (1 - p) / n + z^2 / (4 * n^2)) / den; c(ctr - hw, ctr + hw)
}
prop_txt <- function(v) { chk(c(v[2], v[1] - v[2])); w <- wilson(v[2], v[1])
  sprintf("%d of %d, %.1f%% (%.1f%% to %.1f%%)", v[2], v[1], 100 * v[2] / v[1], 100 * w[1], 100 * w[2]) }
t8 <- data.frame(
  Endpoint = c("MMSE change, points (95% CI)", "CDR 0.5 or above, n of observed, % (Wilson 95% CI)"),
  `Analysis visit 2` = c(ci(mm$estimate[1], mm$lower[1], mm$upper[1], 2), prop_txt(c2)),
  `Analysis visit 3` = c(ci(mm$estimate[2], mm$lower[2], mm$upper[2], 2), prop_txt(c3)),
  check.names = FALSE)
mmse30 <- c(sum(pap$base_mmse == 30, na.rm = TRUE), sum(lp$mmse[lp$avisit == 2] == 30, na.rm = TRUE),
            sum(lp$mmse[lp$avisit == 3] == 30, na.rm = TRUE))
chk(mmse30)
add_table("T8", "Secondary endpoints, primary analysis population", t8,
          sprintf("MMSE ceiling: %d of %d score 30 at baseline, %d at analysis visit 2 and %d at analysis visit 3. Descriptive; no hypothesis tests.",
                  mmse30[1], n_pap, mmse30[2], mmse30[3]))

# F1 Mean change over time ----------------------------------------------------------------------------
t_med <- vapply(2:3, function(k) median(lp$years_since_bl[lp$avisit == k]), 0)
ct <- rob[rob$analysis == "Continuous-time", ]
slope <- ct$estimate[1] / 1.90; s_lo <- ct$lower[1] / 1.90; s_hi <- ct$upper[1] / 1.90
tt <- seq(0, 4.5, by = 0.05)
band <- data.frame(t = tt, est = slope * tt, lo = s_hi * tt, hi = s_lo * tt,
                   series = "Continuous-time mixed model")
pts <- data.frame(t = c(0, t_med), est = c(0, la$emmean), lo = c(0, la$lower.CL), hi = c(0, la$upper.CL),
                  series = "MMRM, analysis visits")
f1 <- ggplot() +
  geom_hline(yintercept = 0, colour = PAL$axis, linewidth = 0.3) +
  geom_ribbon(data = band, aes(t, ymin = pmin(lo, hi), ymax = pmax(lo, hi)), fill = PAL$series[2], alpha = 0.15) +
  geom_line(data = band, aes(t, est, colour = series, linetype = series), linewidth = 0.7) +
  geom_errorbar(data = pts[-1, ], aes(t, ymin = lo, ymax = hi, colour = series), width = 0.08, linewidth = 0.6) +
  geom_point(data = pts, aes(t, est, colour = series), size = 2.6, shape = 21, fill = PAL$series[1],
             stroke = 0.8) +
  annotate("text", x = t_med, y = la$upper.CL + 0.12, label = sprintf("%.2f", la$emmean),
           colour = PAL$ink, size = 3) +
  scale_colour_manual(values = c(`MMRM, analysis visits` = PAL$series[1],
                                 `Continuous-time mixed model` = PAL$series[2]), name = NULL) +
  scale_linetype_manual(values = c(`Continuous-time mixed model` = "dashed"), guide = "none") +
  scale_x_continuous("Years since baseline", breaks = 0:4, limits = c(0, 4.5)) +
  scale_y_continuous("Change in nWBV, percentage points") +
  labs(title = "Change in normalised whole-brain volume over time",
       subtitle = sprintf("Mean change from baseline with 95%% CI; primary analysis population (n = %d)", n_pap),
       caption = "MMRM points at the median time of each analysis visit. Observational natural history; not a treatment effect.") +
  theme_report()
save_fig(f1, file.path(out_dir, "F1_mean_change.png"))

# F2 Delta-adjustment curve ----------------------------------------------------------------------------
f2d <- dlt
f2d$visit <- factor(f2d$visit, levels = c("Visit 2", "Visit 3"),
                    labels = c("Analysis visit 2 (primary)", "Analysis visit 3"))
cols2 <- setNames(PAL$series[1:2], levels(f2d$visit))
f2p <- ggplot(f2d, aes(delta, estimate, colour = visit, fill = visit)) +
  geom_hline(yintercept = 0, colour = PAL$axis, linewidth = 0.3) +
  geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.15, colour = NA) +
  geom_line(aes(linetype = visit), linewidth = 0.7) +
  geom_point(size = 1.8) +
  scale_colour_manual(values = cols2, name = NULL) +
  scale_fill_manual(values = cols2, name = NULL) +
  scale_linetype_manual(values = c("solid", "dashed"), name = NULL) +
  scale_x_reverse("Delta: extra decline assumed for missing values, pp", breaks = seq(0, -1, by = -0.2)) +
  scale_y_continuous("Change in nWBV, percentage points") +
  labs(title = "Sensitivity of mean change to departures from MAR",
       subtitle = "Multiple imputation under MAR (delta = 0), then imputed values shifted by delta; 95% CI",
       caption = if (all(dlt$upper < 0)) "Every 95% CI excludes zero across the grid." else NULL) +
  theme_report()
save_fig(f2p, file.path(out_dir, "F2_delta_adjustment.png"))

writeLines(c("# Part A tables (aggregate; small-cell rule applied)", "",
             paste0("Generated ", format(Sys.time(), "%Y-%m-%d %H:%M %Z"), " by `11_results_part_a.R`."), "",
             tables_md),
           file.path(out_dir, "tables_part_a.md"))
writeLines(capture.output(sessionInfo()), file.path(out_dir, "11_sessioninfo.txt"))
cat(tables_md, sep = "\n")
cat("\nWritten to", out_dir, "\n")
