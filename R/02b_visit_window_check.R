# ------------------------------------------------------------------------------
# 02b_visit_window_check.R
#
# Purpose : The skipped-visit timing check in 02_build_analysis_data.R failed:
#           the Visit label in the file does not always match session timing.
#           This script compares the label-based visit scheme with a
#           time-window scheme, using counts and timing only, so the visit
#           scheme can be settled before the SAP is frozen.
#           No outcome is read, summarised or modelled.
# Windows : Targets are 2 and 4 years from baseline (PAP medians of visits 2
#           and 3 are 1.99 and 3.91 years). Window 2: 1 to under 3 years.
#           Window 3: 3 to under 5 years. If a participant has more than one
#           session in a window, the one closest to the target is used.
# Inputs  : derived/analysis_long.rds in the data directory (OASIS2_DATA_DIR if
#           set, otherwise the working directory).
# Outputs : 02b_visit_window_check.md and 02b_sessioninfo.txt in the report
#           directory (OASIS2_REPORT_DIR if set, otherwise reports/ under the
#           working directory). Aggregate only.
# Privacy : Counts of 1 to 4 are shown as "<5"; timing summaries only for
#           groups of 10 or more; no minima or maxima.
# Run     : set OASIS2_DATA_DIR and OASIS2_REPORT_DIR (for example in
#           ~/.Renviron), then source("R/02b_visit_window_check.R")
# ------------------------------------------------------------------------------

data_dir   <- Sys.getenv("OASIS2_DATA_DIR", unset = getwd())
report_dir <- Sys.getenv("OASIS2_REPORT_DIR", unset = file.path(getwd(), "reports"))
long_file  <- file.path(data_dir, "derived", "analysis_long.rds")
stopifnot("Run 02_build_analysis_data.R first" = file.exists(long_file))
dir.create(report_dir, recursive = TRUE, showWarnings = FALSE)

long <- readRDS(long_file)
# The comparison is on the file's Visit label (visit_label), not the window-based
# analysis visit that 02_build_analysis_data.R now stores in avisit.
post <- long[long$visit_label > 1, c("id", "visit_label", "years_since_bl", "pap_fl")]
names(post)[names(post) == "visit_label"] <- "avisit"

win_levels <- c("Under 1 year", "Window 2 (1 to under 3 years)",
                "Window 3 (3 to under 5 years)", "5 years or later")
post$window <- factor(win_levels[findInterval(post$years_since_bl, c(1, 3, 5)) + 1],
                      levels = win_levels)
post$awin   <- c(NA, 2L, 3L, NA)[as.integer(post$window)]
post$target <- c(NA, 2, 4, NA)[as.integer(post$window)]

# Helpers ----------------------------------------------------------------------
sc <- function(n) {
  n <- as.integer(n)
  ifelse(n == 0L, "0", ifelse(n < 5L, "<5", as.character(n)))
}
md_table <- function(df) {
  df[] <- lapply(df, as.character)
  c(paste0("| ", paste(names(df), collapse = " | "), " |"),
    paste0("|", paste(rep("---", ncol(df)), collapse = "|"), "|"),
    apply(df, 1, function(r) paste0("| ", paste(r, collapse = " | "), " |")))
}
med_iqr <- function(x) {
  if (length(x) < 10) return("-")
  q <- quantile(x, c(0.5, 0.25, 0.75))
  sprintf("%.2f (%.2f to %.2f)", q[1], q[2], q[3])
}
n_pct <- function(n, den) if (n < 5) sc(n) else sprintf("%d (%.0f%%)", n, 100 * n / den)

one_population <- function(p, n_pop, label) {
  # 1. Visit label by time window (sessions)
  tab <- table(factor(p$avisit, 2:5), p$window)
  cross_df <- data.frame(`Visit label` = rownames(tab),
                         matrix(sc(tab), nrow = nrow(tab)),
                         check.names = FALSE, stringsAsFactors = FALSE)
  names(cross_df)[-1] <- colnames(tab)

  # 2. Sessions where the two schemes give a different MMRM visit
  lab_visit <- ifelse(p$avisit %in% 2:3, p$avisit, NA)
  differ <- !(is.na(lab_visit) & is.na(p$awin)) &
            (is.na(lab_visit) | is.na(p$awin) | lab_visit != p$awin)

  # 3. Window scheme: one session per participant per window, closest to target
  w <- p[!is.na(p$awin), ]
  w <- w[order(w$id, w$awin, abs(w$years_since_bl - w$target)), ]
  n_multi <- length(unique(w$id[duplicated(w[c("id", "awin")])]))
  w <- w[!duplicated(w[c("id", "awin")]), ]
  in2 <- unique(w$id[w$awin == 2]); in3 <- unique(w$id[w$awin == 3])

  # Label scheme, same summaries
  l2 <- unique(p$id[p$avisit == 2]); l3 <- unique(p$id[p$avisit == 3])

  pattern_df <- data.frame(
    Pattern = c("Visit 2", "Visit 3", "Both", "Visit 2 only", "Visit 3 only", "Neither"),
    `Label scheme` = c(n_pct(length(l2), n_pop), n_pct(length(l3), n_pop),
                       sc(length(intersect(l2, l3))), sc(length(setdiff(l2, l3))),
                       sc(length(setdiff(l3, l2))),
                       sc(n_pop - length(union(l2, l3)))),
    `Window scheme` = c(n_pct(length(in2), n_pop), n_pct(length(in3), n_pop),
                        sc(length(intersect(in2, in3))), sc(length(setdiff(in2, in3))),
                        sc(length(setdiff(in3, in2))),
                        sc(n_pop - length(union(in2, in3)))),
    check.names = FALSE, stringsAsFactors = FALSE
  )
  timing_df <- data.frame(
    Visit = c(2, 3),
    `Label scheme: median years (25th to 75th percentile)` =
      c(med_iqr(p$years_since_bl[p$avisit == 2]), med_iqr(p$years_since_bl[p$avisit == 3])),
    `Window scheme: median years (25th to 75th percentile)` =
      c(med_iqr(w$years_since_bl[w$awin == 2]), med_iqr(w$years_since_bl[w$awin == 3])),
    check.names = FALSE, stringsAsFactors = FALSE
  )

  c(paste0("## ", label, " (", n_pop, " participants)"), "",
    "### Post-baseline sessions: visit label by time window", "",
    md_table(cross_df), "",
    paste0("- Sessions assigned to a different MMRM visit (or in or out of the MMRM) by the two schemes: ",
           sc(sum(differ)), " of ", nrow(p)),
    paste0("- Participants with more than one session in the same window: ", sc(n_multi)), "",
    "### Participants contributing to each MMRM visit", "",
    md_table(pattern_df), "",
    "### Timing of the sessions used", "",
    md_table(timing_df), "")
}

n_all <- length(unique(long$id))
n_pap <- length(unique(long$id[long$pap_fl]))

out <- c(
  "# Report 02b: Visit label against time windows",
  "",
  paste0("- **Generated:** ", format(Sys.time(), "%Y-%m-%d %H:%M %Z"),
         " by `02b_visit_window_check.R`"),
  "- **Windows:** window 2 is 1 to under 3 years from baseline (target 2); window 3 is 3 to under 5 years (target 4). With more than one session in a window, the one closest to the target is used.",
  "- **Content:** aggregate only. Counts of 1 to 4 are shown as `<5`; timing summaries only for groups of 10 or more. No outcome is used.",
  "- **Small-cell note:** a `<5` cell next to known totals can sometimes be recovered by subtraction. Do not publish these tables as they are.",
  "",
  one_population(post[post$pap_fl, ], n_pap, "Primary analysis population"),
  one_population(post, n_all, "All participants")
)
writeLines(out, file.path(report_dir, "02b_visit_window_check.md"))
writeLines(capture.output(sessionInfo()), file.path(report_dir, "02b_sessioninfo.txt"))

cat(out, sep = "\n")
cat("\nReport written to", file.path(report_dir, "02b_visit_window_check.md"), "\n")
