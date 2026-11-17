# ------------------------------------------------------------------------------
# 02_build_analysis_data.R
#
# Purpose : Build the analysis datasets defined in the SAP (Sections 4, 5, 8)
#           and report the counts and baseline quantities the SAP relies on:
#           population sizes, analysis-visit counts and timing in the primary
#           analysis population (PAP), baseline characteristics, and the
#           baseline SD used to check the delta grid.
#           Analysis visits are assigned by time window (SAP Section 5):
#           visit 2 = 1 to under 3 years from baseline (target 2 years),
#           visit 3 = 3 to under 5 years (target 4 years). With more than one
#           session in a window, the one closest to the target is used (the
#           earlier one if tied). The file's Visit label is kept for the
#           label-based sensitivity analysis.
#           No outcome is summarised: change scores are derived and saved, but
#           never printed, tabulated or modelled here.
# Inputs  : longitudinaldemo.xlsx in the data directory (OASIS2_DATA_DIR if
#           set, otherwise the working directory).
# Outputs : In <data directory>/derived/ (participant-level, never committed):
#             analysis_subj.rds  one row per participant
#             analysis_long.rds  one row per participant per visit
#           In the report directory (OASIS2_REPORT_DIR if set, otherwise
#           reports/ under the working directory), aggregate only:
#             02_build_report.md, 02_build_sessioninfo.txt
# Privacy : The derived files carry an internal integer id; OASIS subject and
#           session identifiers are dropped. The report shows counts of 1 to 4
#           as "<5", summarises only groups of 10 or more, and gives no minima
#           or maxima.
# Run     : set OASIS2_DATA_DIR and OASIS2_REPORT_DIR (for example in
#           ~/.Renviron), then source("R/02_build_analysis_data.R")
# ------------------------------------------------------------------------------

library(readxl)

data_dir   <- Sys.getenv("OASIS2_DATA_DIR", unset = getwd())
report_dir <- Sys.getenv("OASIS2_REPORT_DIR", unset = file.path(getwd(), "reports"))
data_file  <- file.path(data_dir, "longitudinaldemo.xlsx")
stopifnot("Data file not found in data directory" = file.exists(data_file))
derived_dir <- file.path(data_dir, "derived")
dir.create(derived_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(report_dir, recursive = TRUE, showWarnings = FALSE)

raw <- as.data.frame(read_excel(data_file))

d <- data.frame(
  oasis_id = raw[["Subject ID"]],
  visit    = as.integer(as.numeric(as.character(raw[["Visit"]]))),
  delay    = as.numeric(raw[["MR Delay"]]),
  sex      = raw[["M/F"]],
  age      = as.numeric(raw[["Age"]]),
  educ     = as.numeric(raw[["EDUC"]]),
  ses      = as.numeric(raw[["SES"]]),
  mmse     = as.numeric(raw[["MMSE"]]),
  cdr      = as.numeric(raw[["CDR"]]),
  nwbv     = as.numeric(raw[["nWBV"]]),
  stringsAsFactors = FALSE
)
d <- d[order(d$oasis_id, d$visit), ]
d$id <- match(d$oasis_id, sort(unique(d$oasis_id)))   # internal integer id
d$oasis_id <- NULL
rownames(d) <- NULL

# Analysis visit by time window (SAP Section 5) -----------------------------------
d$years  <- d$delay / 365.25
win_idx  <- findInterval(d$years, c(1, 3, 5))          # 0: <1, 1: [1,3), 2: [3,5), 3: >=5
d$awin   <- ifelse(d$visit > 1, c(NA, 2L, 3L, NA)[win_idx + 1], NA)
d$target <- c(NA, 2, 4)[match(d$awin, c(NA, 2L, 3L))]
cand <- which(!is.na(d$awin) & !is.na(d$nwbv))
cand <- cand[order(d$id[cand], d$awin[cand], abs(d$years[cand] - d$target[cand]), d$years[cand])]
sel  <- cand[!duplicated(d[cand, c("id", "awin")])]
d$mmrm_fl <- seq_len(nrow(d)) %in% sel                  # session used by the primary MMRM
d$avisit  <- ifelse(d$visit == 1, 1L, ifelse(d$mmrm_fl, d$awin, NA))
d$mmrm_lab_fl <- d$visit %in% 2:3 & !is.na(d$nwbv)     # session used by the label-based MMRM

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
mean_sd <- function(x) {
  x <- x[!is.na(x)]
  if (length(x) < 10) "-" else sprintf("%.2f (%.2f)", mean(x), sd(x))
}
med_iqr <- function(x, digits = 2) {
  x <- x[!is.na(x)]
  if (length(x) < 10) return("-")
  q <- quantile(x, c(0.5, 0.25, 0.75))
  sprintf(paste0("%.", digits, "f (%.", digits, "f to %.", digits, "f)"), q[1], q[2], q[3])
}

# Participant-level dataset ------------------------------------------------------
bl <- d[d$visit == 1, ]
stopifnot("Every participant must have exactly one visit 1 row" =
            nrow(bl) == length(unique(d$id)) && !any(duplicated(bl$id)))

has_win <- function(k) bl$id %in% d$id[d$mmrm_fl & d$awin == k]
has_lab <- function(k) bl$id %in% d$id[d$mmrm_lab_fl & d$visit == k]

subj <- data.frame(
  id        = bl$id,
  sex       = bl$sex,
  educ      = bl$educ,
  ses       = bl$ses,
  age_bl    = bl$age,
  base_nwbv = 100 * bl$nwbv,
  base_mmse = bl$mmse,
  base_cdr  = bl$cdr,
  has_v2    = has_win(2),
  has_v3    = has_win(3),
  has_lab_v2 = has_lab(2),
  has_lab_v3 = has_lab(3),
  stringsAsFactors = FALSE
)
subj$base_cdr_cat <- as.character(cut(subj$base_cdr, c(-Inf, 0, 0.5, Inf),
                                      labels = c("0", "0.5", "1 or above")))
subj$base_cdr_cat2 <- ifelse(subj$base_cdr == 0, "0", "0.5 or above")
subj$supp_fl <- !is.na(subj$base_nwbv) & (subj$has_v2 | subj$has_v3)
subj$pap_fl  <- subj$supp_fl & subj$base_cdr == 0

last_visit <- tapply(d$visit, d$id, max)
subj$last_avisit <- as.integer(last_visit[match(subj$id, as.integer(names(last_visit)))])
prog <- tapply(d$cdr >= 0.5 & d$visit > 1, d$id, any)
subj$prog_flag <- as.logical(prog[match(subj$id, as.integer(names(prog)))])

# Covariates centred at the PAP mean (SAP Section 8)
pap <- subj$pap_fl
subj$age_bl_c    <- subj$age_bl    - mean(subj$age_bl[pap])
subj$educ_c      <- subj$educ      - mean(subj$educ[pap])
subj$base_nwbv_c <- subj$base_nwbv - mean(subj$base_nwbv[pap])
subj$base_mmse_c <- subj$base_mmse - mean(subj$base_mmse[pap], na.rm = TRUE)

# Long dataset -------------------------------------------------------------------
m <- match(d$id, subj$id)
long <- data.frame(
  id             = d$id,
  avisit         = d$avisit,
  visit_label    = d$visit,
  mmrm_fl        = d$mmrm_fl,
  mmrm_lab_fl    = d$mmrm_lab_fl,
  years_since_bl = d$years,
  age_visit      = d$age,
  nwbv_pct       = 100 * d$nwbv,
  chg_nwbv       = 100 * d$nwbv - subj$base_nwbv[m],
  mmse           = d$mmse,
  chg_mmse       = d$mmse - subj$base_mmse[m],
  cdr            = d$cdr,
  stringsAsFactors = FALSE
)
keep <- c("sex", "educ", "age_bl", "base_nwbv", "base_mmse", "base_cdr", "base_cdr_cat2",
          "age_bl_c", "educ_c", "base_nwbv_c", "base_mmse_c", "pap_fl", "supp_fl")
long <- cbind(long, subj[m, keep])
rownames(long) <- NULL

saveRDS(subj, file.path(derived_dir, "analysis_subj.rds"))
saveRDS(long, file.path(derived_dir, "analysis_long.rds"))

# Report: participant flow -------------------------------------------------------
n_all  <- nrow(subj)
n_cdr0 <- sum(subj$base_cdr == 0)
n_pap  <- sum(subj$pap_fl)
n_supp <- sum(subj$supp_fl)
flow_df <- data.frame(
  Step = c("All participants",
           "Baseline nWBV and nWBV at analysis visit 2 or 3 (supportive population)",
           "Excluded from supportive population (no session in either window)",
           "CDR 0 at visit 1",
           "CDR 0 at visit 1, baseline nWBV and nWBV at analysis visit 2 or 3 (PAP)",
           "CDR 0 at visit 1 but excluded from PAP"),
  Participants = sc(c(n_all, n_supp, n_all - n_supp, n_cdr0, n_pap, n_cdr0 - n_pap)),
  stringsAsFactors = FALSE
)

# Report: PAP by analysis visit and pattern --------------------------------------
ps <- subj[pap, ]
lp <- long[long$pap_fl, ]
n_pct <- function(n, den) if (n < 5) sc(n) else sprintf("%d (%.0f%%)", n, 100 * n / den)
visit_df <- data.frame(
  `Analysis visit` = c("2 (1 to under 3 years)", "3 (3 to under 5 years)"),
  `Window scheme (primary)` = c(n_pct(sum(ps$has_v2), n_pap), n_pct(sum(ps$has_v3), n_pap)),
  `Label scheme (sensitivity)` = c(n_pct(sum(ps$has_lab_v2), n_pap),
                                   n_pct(sum(ps$has_lab_v3), n_pap)),
  check.names = FALSE, stringsAsFactors = FALSE
)
pattern_df <- data.frame(
  Pattern = c("Visit 2 and visit 3", "Visit 2 only", "Visit 3 only"),
  `PAP participants (window scheme)` = sc(c(sum(ps$has_v2 & ps$has_v3),
                                            sum(ps$has_v2 & !ps$has_v3),
                                            sum(!ps$has_v2 & ps$has_v3))),
  check.names = FALSE, stringsAsFactors = FALSE
)
lpp <- lp[lp$visit_label > 1, ]
session_df <- data.frame(
  `PAP post-baseline sessions` = c("Total", "Used in the primary MMRM",
                                   "Not used in the primary MMRM (continuous-time model only)"),
  Sessions = sc(c(nrow(lpp), sum(lpp$mmrm_fl), sum(!lpp$mmrm_fl))),
  check.names = FALSE, stringsAsFactors = FALSE
)

# Report: timing of the sessions used, PAP ----------------------------------------
timing_df <- do.call(rbind, lapply(2:3, function(k) {
  x <- lp$years_since_bl[lp$mmrm_fl & lp$avisit == k]
  data.frame(`Analysis visit` = k, n = sc(length(x)),
             `Median years from baseline (25th to 75th percentile)` = med_iqr(x),
             check.names = FALSE, stringsAsFactors = FALSE)
}))

# Report: baseline characteristics of the PAP -------------------------------------
base_df <- data.frame(
  Characteristic = c("Participants", "Age at baseline, mean (SD)",
                     "Education in years, mean (SD)", "Sex: female", "Sex: male",
                     "nWBV at baseline, percentage points, mean (SD)",
                     "MMSE at baseline, median (25th to 75th percentile)",
                     "MMSE at baseline equal to 30", "MMSE at baseline missing",
                     "SES missing"),
  Value = c(sc(n_pap), mean_sd(ps$age_bl), mean_sd(ps$educ),
            sc(sum(ps$sex == "F")), sc(sum(ps$sex == "M")),
            mean_sd(ps$base_nwbv), med_iqr(ps$base_mmse, 0),
            sc(sum(ps$base_mmse == 30, na.rm = TRUE)), sc(sum(is.na(ps$base_mmse))),
            sc(sum(is.na(ps$ses)))),
  stringsAsFactors = FALSE
)

# Report: delta grid check (D-007) -------------------------------------------------
sd_base <- sd(ps$base_nwbv)
delta_lines <- c(
  sprintf("- Baseline SD of nWBV in the PAP: %.2f percentage points", sd_base),
  sprintf("- Proposed grid: 0 to -1.0 in steps of 0.1. The largest shift is %.2f baseline SDs; one step is %.2f baseline SDs.",
          1.0 / sd_base, 0.1 / sd_base)
)

# Report: supportive population ----------------------------------------------------
ss <- subj[subj$supp_fl, ]
cdr2 <- factor(ss$base_cdr_cat2, c("0", "0.5 or above"))
supp_tab <- table(cdr2)
supp_df <- data.frame(`Baseline CDR` = names(supp_tab),
                      Participants = sc(supp_tab),
                      `With nWBV at analysis visit 2` = sc(tapply(ss$has_v2, cdr2, sum)),
                      `With nWBV at analysis visit 3` = sc(tapply(ss$has_v3, cdr2, sum)),
                      check.names = FALSE, stringsAsFactors = FALSE)

# Write report ----------------------------------------------------------------------
out <- c(
  "# Report 02: Analysis data build and pre-freeze counts",
  "",
  paste0("- **Generated:** ", format(Sys.time(), "%Y-%m-%d %H:%M %Z"),
         " by `02_build_analysis_data.R`"),
  "- **Visit scheme:** analysis visit 2 is 1 to under 3 years from baseline (target 2); analysis visit 3 is 3 to under 5 years (target 4); closest session to the target if more than one.",
  "- **Content:** aggregate only. Counts of 1 to 4 are shown as `<5`; summaries only for groups of 10 or more; no minima or maxima. No outcome (change score, progression) is summarised.",
  "- **Small-cell note:** a `<5` cell next to known totals can sometimes be recovered by subtraction. Merge categories before publishing any of these tables.",
  "",
  "## Participant flow", "", md_table(flow_df), "",
  "## PAP: participants with nWBV at each analysis visit", "", md_table(visit_df), "",
  "## PAP: pattern across the two analysis visits", "", md_table(pattern_df), "",
  "## PAP: sessions", "", md_table(session_df), "",
  "## PAP: timing of the sessions used in the primary MMRM", "", md_table(timing_df), "",
  "## PAP: baseline characteristics", "", md_table(base_df), "",
  "## Delta grid check (D-007)", "", delta_lines, "",
  "## Supportive population by baseline CDR (two categories)", "", md_table(supp_df), "",
  "## Datasets written (local only, never committed)", "",
  paste0("- `derived/analysis_subj.rds`: ", nrow(subj), " rows, ", ncol(subj), " columns: ",
         paste(names(subj), collapse = ", ")),
  paste0("- `derived/analysis_long.rds`: ", nrow(long), " rows, ", ncol(long), " columns: ",
         paste(names(long), collapse = ", ")),
  ""
)
writeLines(out, file.path(report_dir, "02_build_report.md"))
writeLines(capture.output(sessionInfo()), file.path(report_dir, "02_build_sessioninfo.txt"))

cat(out, sep = "\n")
cat("\nReport written to", file.path(report_dir, "02_build_report.md"), "\n")
