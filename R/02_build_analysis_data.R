# ------------------------------------------------------------------------------
# 02_build_analysis_data.R
#
# Purpose : Build the analysis datasets defined in the SAP (Sections 4, 5, 8)
#           and report the counts and baseline quantities the SAP needs before
#           it is frozen: final population sizes, visit timing in the primary
#           analysis population (PAP), the skipped-visit timing check, baseline
#           characteristics, and the baseline SD used to check the delta grid.
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

has_nwbv_at <- function(k) bl$id %in% d$id[d$visit %in% k & !is.na(d$nwbv)]

subj <- data.frame(
  id        = bl$id,
  sex       = bl$sex,
  educ      = bl$educ,
  ses       = bl$ses,
  age_bl    = bl$age,
  base_nwbv = 100 * bl$nwbv,
  base_mmse = bl$mmse,
  base_cdr  = bl$cdr,
  has_v2    = has_nwbv_at(2),
  has_v3    = has_nwbv_at(3),
  has_v4    = has_nwbv_at(4),
  has_v5    = has_nwbv_at(5),
  stringsAsFactors = FALSE
)
subj$base_cdr_cat <- as.character(cut(subj$base_cdr, c(-Inf, 0, 0.5, Inf),
                                      labels = c("0", "0.5", "1 or above")))
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
  avisit         = d$visit,
  years_since_bl = d$delay / 365.25,
  age_visit      = d$age,
  nwbv_pct       = 100 * d$nwbv,
  chg_nwbv       = 100 * d$nwbv - subj$base_nwbv[m],
  mmse           = d$mmse,
  chg_mmse       = d$mmse - subj$base_mmse[m],
  cdr            = d$cdr,
  stringsAsFactors = FALSE
)
keep <- c("sex", "educ", "age_bl", "base_nwbv", "base_mmse", "base_cdr", "base_cdr_cat",
          "age_bl_c", "educ_c", "base_nwbv_c", "base_mmse_c", "pap_fl", "supp_fl")
long <- cbind(long, subj[m, keep])
rownames(long) <- NULL
long$mmrm_fl <- long$avisit %in% 2:3      # rows used by the MMRM

saveRDS(subj, file.path(derived_dir, "analysis_subj.rds"))
saveRDS(long, file.path(derived_dir, "analysis_long.rds"))

# Report: participant flow -------------------------------------------------------
n_all  <- nrow(subj)
n_cdr0 <- sum(subj$base_cdr == 0)
n_pap  <- sum(subj$pap_fl)
n_supp <- sum(subj$supp_fl)
flow_df <- data.frame(
  Step = c("All participants",
           "Baseline nWBV and nWBV at visit 2 or 3 (supportive population)",
           "Excluded from supportive population (no nWBV at visit 2 or 3)",
           "CDR 0 at visit 1",
           "CDR 0 at visit 1, baseline nWBV and nWBV at visit 2 or 3 (PAP)",
           "CDR 0 at visit 1 but excluded from PAP"),
  Participants = sc(c(n_all, n_supp, n_all - n_supp, n_cdr0, n_pap, n_cdr0 - n_pap)),
  stringsAsFactors = FALSE
)

# Report: PAP by visit and pattern -----------------------------------------------
ps <- subj[pap, ]
visit_df <- data.frame(
  Visit = 2:5,
  `Used in` = c("MMRM and continuous-time", "MMRM and continuous-time",
                "continuous-time only", "continuous-time only"),
  `PAP participants with nWBV` = sc(c(sum(ps$has_v2), sum(ps$has_v3),
                                      sum(ps$has_v4), sum(ps$has_v5))),
  `Percent of PAP` = sprintf("%.0f%%", 100 * c(mean(ps$has_v2), mean(ps$has_v3),
                                               mean(ps$has_v4), mean(ps$has_v5))),
  check.names = FALSE, stringsAsFactors = FALSE
)
visit_df$`Percent of PAP`[c(sum(ps$has_v2), sum(ps$has_v3),
                            sum(ps$has_v4), sum(ps$has_v5)) < 5] <- "-"
pattern_df <- data.frame(
  Pattern = c("Visit 2 and visit 3", "Visit 2 only", "Visit 3 only"),
  `PAP participants` = sc(c(sum(ps$has_v2 & ps$has_v3), sum(ps$has_v2 & !ps$has_v3),
                            sum(!ps$has_v2 & ps$has_v3))),
  check.names = FALSE, stringsAsFactors = FALSE
)

# Report: visit timing in the PAP -------------------------------------------------
lp <- long[long$pap_fl, ]
timing_df <- do.call(rbind, lapply(2:5, function(k) {
  x <- lp$years_since_bl[lp$avisit == k]
  data.frame(Visit = k, n = sc(length(x)),
             `Median years from baseline (25th to 75th percentile)` = med_iqr(x),
             check.names = FALSE, stringsAsFactors = FALSE)
}))

# Report: skipped-visit timing check (SAP Section 5), pass/fail only --------------
skip_ids  <- setdiff(d$id, d$id[d$visit == 2])
q75_v2    <- quantile(d$delay[d$visit == 2], 0.75)
next_days <- vapply(skip_ids, function(i) min(d$delay[d$id == i & d$visit > 1]), numeric(1))
skip_pass <- length(skip_ids) == 0 || all(next_days > q75_v2)

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
supp_tab <- table(factor(ss$base_cdr_cat, c("0", "0.5", "1 or above")))
supp_df <- data.frame(`Baseline CDR` = names(supp_tab),
                      Participants = sc(supp_tab),
                      `With nWBV at visit 2` = sc(tapply(ss$has_v2, factor(ss$base_cdr_cat,
                                                 c("0", "0.5", "1 or above")), sum)),
                      `With nWBV at visit 3` = sc(tapply(ss$has_v3, factor(ss$base_cdr_cat,
                                                 c("0", "0.5", "1 or above")), sum)),
                      check.names = FALSE, stringsAsFactors = FALSE)

# Write report ----------------------------------------------------------------------
out <- c(
  "# Report 02: Analysis data build and pre-freeze counts",
  "",
  paste0("- **Generated:** ", format(Sys.time(), "%Y-%m-%d %H:%M %Z"),
         " by `02_build_analysis_data.R`"),
  "- **Content:** aggregate only. Counts of 1 to 4 are shown as `<5`; summaries only for groups of 10 or more; no minima or maxima. No outcome (change score, progression) is summarised.",
  "- **Small-cell note:** a `<5` cell next to known totals can sometimes be recovered by subtraction. Merge categories before publishing any of these tables.",
  "",
  "## Participant flow", "", md_table(flow_df), "",
  "## PAP: participants with nWBV at each visit", "", md_table(visit_df), "",
  "## PAP: pattern across the two MMRM visits", "", md_table(pattern_df), "",
  "## PAP: visit timing", "", md_table(timing_df), "",
  "## Skipped-visit timing check (SAP Section 5)", "",
  paste0("- Participants with no visit 2: ", sc(length(skip_ids))),
  paste0("- Their next session is later than the 75th percentile of visit 2 timing for all of them: ",
         if (skip_pass) "PASS" else "FAIL"), "",
  "## PAP: baseline characteristics", "", md_table(base_df), "",
  "## Delta grid check (D-007)", "", delta_lines, "",
  "## Supportive population by baseline CDR", "", md_table(supp_df), "",
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
