# ------------------------------------------------------------------------------
# 01_qc.R
#
# Purpose : Quality control of the OASIS-2 longitudinal demographics file
#           (checks 1 to 7 of the project QC specification), plus
#           the count-only tables needed to close D-001, D-002 and D-005
#           (time axis, visit timing, numbers at each visit by population).
#           No outcome analysis: no change scores, no outcome summaries.
# Inputs  : the data file (OASIS2_DATA_FILE, default longitudinaldemo.xlsx)
#           in the data directory. The data directory is
#           the environment variable OASIS2_DATA_DIR if set, otherwise the
#           current working directory.
# Outputs : 01_qc_report.md and 01_qc_sessioninfo.txt in the report directory.
#           The report directory is OASIS2_REPORT_DIR if set, otherwise
#           reports/ under the working directory. The report is also printed.
# Privacy : Aggregate output only. No IDs, rows, per-person values, minima or
#           maxima are written. Counts of 1 to 4 are shown as "<5". Summaries
#           are given only for groups of at least 10.
# Run     : set OASIS2_DATA_DIR and OASIS2_REPORT_DIR (for example in
#           ~/.Renviron), then source("R/01_qc.R")
# ------------------------------------------------------------------------------

data_dir   <- Sys.getenv("OASIS2_DATA_DIR", unset = getwd())
report_dir <- Sys.getenv("OASIS2_REPORT_DIR", unset = file.path(getwd(), "reports"))
data_file  <- file.path(data_dir, Sys.getenv("OASIS2_DATA_FILE",
                                             unset = "longitudinaldemo.xlsx"))
stopifnot("Data file not found in data directory" = file.exists(data_file))
dir.create(report_dir, recursive = TRUE, showWarnings = FALSE)

read_raw <- function(path) {   # .xlsx (real data) or .csv (synthetic data)
  if (grepl("\\.csv$", path, ignore.case = TRUE)) {
    read.csv(path, check.names = FALSE, stringsAsFactors = FALSE)
  } else {
    as.data.frame(readxl::read_excel(path))
  }
}
raw <- read_raw(data_file)

d <- data.frame(
  id    = raw[["Subject ID"]],
  group = raw[["Group"]],
  visit = as.integer(as.numeric(as.character(raw[["Visit"]]))),
  delay = as.numeric(raw[["MR Delay"]]),
  sex   = raw[["M/F"]],
  hand  = raw[["Hand"]],
  age   = as.numeric(raw[["Age"]]),
  educ  = as.numeric(raw[["EDUC"]]),
  ses   = as.numeric(raw[["SES"]]),
  mmse  = as.numeric(raw[["MMSE"]]),
  cdr   = as.numeric(raw[["CDR"]]),
  etiv  = as.numeric(raw[["eTIV"]]),
  nwbv  = as.numeric(raw[["nWBV"]]),
  asf   = as.numeric(raw[["ASF"]]),
  stringsAsFactors = FALSE
)
d <- d[order(d$id, d$visit), ]
rownames(d) <- NULL

# Helpers ----------------------------------------------------------------------
sc <- function(n) {                      # small-cell suppression for counts
  n <- as.integer(n)
  ifelse(n == 0L, "0", ifelse(n < 5L, "<5", as.character(n)))
}
pct <- function(num, den) ifelse(num < 5 | den < 5, "-", sprintf("%.0f%%", 100 * num / den))
md_table <- function(df) {
  df[] <- lapply(df, as.character)
  c(paste0("| ", paste(names(df), collapse = " | "), " |"),
    paste0("|", paste(rep("---", ncol(df)), collapse = "|"), "|"),
    apply(df, 1, function(r) paste0("| ", paste(r, collapse = " | "), " |")))
}
per_id <- function(x, f) vapply(split(x, d$id), f, logical(1))

checks <- data.frame(Check = character(), Description = character(),
                     Result = character(), Detail = character(),
                     stringsAsFactors = FALSE)
add_check <- function(check, description, pass, detail = "") {
  checks[nrow(checks) + 1, ] <<- list(check, description,
                                      if (isTRUE(pass)) "PASS" else "FAIL", detail)
}
n_bad <- function(ok) paste("participants failing:", sc(sum(!ok)))

n_id <- length(unique(d$id))

# Check 1: structure -----------------------------------------------------------
add_check("1a", "One row per participant per visit",
          !any(duplicated(d[c("id", "visit")])))
ok <- vapply(split(d$visit, d$id), function(v) identical(v, seq_along(v)), logical(1))
add_check("1b", "Visit index contiguous from 1", all(ok), n_bad(ok))
ok <- per_id(d$delay, function(x) all(diff(x) > 0))
add_check("1c", "MR Delay strictly increasing within participant", all(ok), n_bad(ok))
add_check("1d", "MR Delay is 0 at visit 1", all(d$delay[d$visit == 1] == 0))

# Check 2: constant-within-person fields ----------------------------------------
for (v in c("sex", "hand", "educ", "ses")) {
  ok <- per_id(d[[v]], function(x) length(unique(x)) == 1)
  add_check(paste0("2-", v), paste(v, "constant within participant"), all(ok), n_bad(ok))
}
add_check("2-hand-all", "Hand takes a single value in the whole file (drop if so)",
          length(unique(d$hand)) == 1,
          paste("distinct values:", length(unique(d$hand))))

# Check 3: value ranges (pass/fail only) ----------------------------------------
in_rng <- function(x, lo, hi) all(x >= lo & x <= hi, na.rm = TRUE)
add_check("3a", "nWBV within 0.60 to 0.90", in_rng(d$nwbv, 0.60, 0.90))
add_check("3b", "MMSE within 0 to 30", in_rng(d$mmse, 0, 30))
add_check("3c", "CDR in {0, 0.5, 1, 2}", all(d$cdr %in% c(0, 0.5, 1, 2)))
add_check("3d", "Age within 60 to 98", in_rng(d$age, 60, 98))
add_check("3e", "Sex in {M, F}", all(d$sex %in% c("M", "F")))
add_check("3f", "SES in 1 to 5 where present", all(d$ses %in% c(1:5, NA)))
add_check("3g", "EDUC within 0 to 30", in_rng(d$educ, 0, 30))
add_check("3h", "Group in {Nondemented, Demented, Converted}",
          all(d$group %in% c("Nondemented", "Demented", "Converted")))

# Check 4: Group against CDR trajectory -----------------------------------------
p <- data.frame(
  id       = names(split(d$id, d$id)),
  group    = vapply(split(d$group, d$id), function(x) x[1], character(1)),
  n_visits = vapply(split(d$visit, d$id), length, integer(1)),
  base_cdr = vapply(split(d$cdr, d$id), function(x) x[1], numeric(1)),
  max_cdr  = vapply(split(d$cdr, d$id), max, numeric(1)),
  max_post = vapply(split(d$cdr, d$id), function(x) if (length(x) > 1) max(x[-1]) else NA_real_,
                    numeric(1)),
  stringsAsFactors = FALSE
)
ok <- per_id(d$group, function(x) length(unique(x)) == 1)
add_check("4a", "Group constant within participant", all(ok), n_bad(ok))
nd <- p$group == "Nondemented"; dm <- p$group == "Demented"; cv <- p$group == "Converted"
add_check("4b", "Nondemented: CDR never 1 or above", all(p$max_cdr[nd] < 1),
          paste("participants failing:", sc(sum(p$max_cdr[nd] >= 1))))
add_check("4c", "Nondemented: CDR 0 at every visit", all(p$max_cdr[nd] == 0),
          paste("participants failing:", sc(sum(p$max_cdr[nd] > 0))))
add_check("4d", "Demented: CDR 0.5 or above at visit 1", all(p$base_cdr[dm] >= 0.5),
          paste("participants failing:", sc(sum(p$base_cdr[dm] < 0.5))))
add_check("4e", "Converted: CDR 0 at visit 1", all(p$base_cdr[cv] == 0),
          paste("participants failing:", sc(sum(p$base_cdr[cv] != 0))))
add_check("4f", "Converted: CDR 0.5 or above at a later visit",
          all(p$max_post[cv] >= 0.5),
          paste("participants failing:", sc(sum(p$max_post[cv] < 0.5))))

cdr_cat <- cut(p$base_cdr, c(-Inf, 0, 0.5, Inf), labels = c("0", "0.5", "1 or above"))
tab_cdr <- table(factor(p$group, c("Nondemented", "Demented", "Converted")), cdr_cat)
tab_cdr_df <- data.frame(Group = rownames(tab_cdr),
                         matrix(sc(tab_cdr), nrow = nrow(tab_cdr)),
                         stringsAsFactors = FALSE)
names(tab_cdr_df) <- c("Group", paste("Baseline CDR", colnames(tab_cdr)))

# Check 5: ASF x eTIV -----------------------------------------------------------
prod_ae <- d$asf * d$etiv
med_ae  <- median(prod_ae)
add_check("5", "ASF x eTIV constant (all within 1% of the median)",
          all(abs(prod_ae / med_ae - 1) < 0.01),
          sprintf("median %.0f; 5th to 95th percentile %.0f to %.0f", med_ae,
                  quantile(prod_ae, 0.05), quantile(prod_ae, 0.95)))

# Check 6: counts against the dataset confirmation ---------------------------------------------
add_check("6a", "373 sessions and 150 participants", nrow(d) == 373 && n_id == 150,
          paste(nrow(d), "sessions,", n_id, "participants"))
add_check("6b", "Group counts 72 / 64 / 14 (Nondemented / Demented / Converted)",
          sum(nd) == 72 && sum(dm) == 64 && sum(cv) == 14,
          paste(sum(nd), sum(dm), sum(cv), sep = " / "))

# Check 7: missingness -----------------------------------------------------------
miss_vars <- c("delay", "age", "educ", "ses", "mmse", "cdr", "etiv", "nwbv", "asf")
miss_df <- data.frame(
  Variable = miss_vars,
  `Sessions missing` = sc(vapply(miss_vars, function(v) sum(is.na(d[[v]])), integer(1))),
  `Participants with any missing` = sc(vapply(miss_vars, function(v)
    sum(vapply(split(is.na(d[[v]]), d$id), any, logical(1))), integer(1))),
  `Participants missing at visit 1` = sc(vapply(miss_vars, function(v)
    sum(is.na(d[[v]][d$visit == 1])), integer(1))),
  check.names = FALSE, stringsAsFactors = FALSE
)

# Time axis: is Age per visit, and is MR Delay consistent with it? ---------------
age_bl   <- ave(d$age, d$id, FUN = function(x) x[1])
yrs      <- d$delay / 365.25
post     <- d$visit > 1
age_gain <- d$age[post] - age_bl[post]
age_varies <- !per_id(d$age, function(x) length(unique(x)) == 1)
time_lines <- c(
  paste0("- Participants whose Age changes across visits: ", sc(sum(age_varies)),
         " of ", n_id, " (if most, Age is age at each visit)"),
  sprintf("- Post-baseline sessions where (Age - baseline Age) is within 1 year of MR Delay / 365.25: %.0f%%",
          100 * mean(abs(age_gain - yrs[post]) <= 1)),
  sprintf("- Median of (Age - baseline Age) minus MR Delay in years, post-baseline sessions: %.2f",
          median(age_gain - yrs[post])),
  "- MR Delay strictly increasing within participant: see check 1c (supports cumulative days from baseline)"
)

# MR Delay by visit (summaries only where n >= 10) -------------------------------
delay_df <- do.call(rbind, lapply(sort(unique(d$visit)), function(k) {
  x <- d$delay[d$visit == k]
  if (length(x) >= 10) {
    q <- quantile(x, c(0.25, 0.5, 0.75))
    data.frame(Visit = k, n = sc(length(x)),
               `25th pct (days)` = sprintf("%.0f", q[1]),
               `Median (days)`   = sprintf("%.0f", q[2]),
               `75th pct (days)` = sprintf("%.0f", q[3]),
               `Median (years)`  = sprintf("%.2f", q[2] / 365.25),
               check.names = FALSE, stringsAsFactors = FALSE)
  } else {
    data.frame(Visit = k, n = sc(length(x)), `25th pct (days)` = "-",
               `Median (days)` = "-", `75th pct (days)` = "-", `Median (years)` = "-",
               check.names = FALSE, stringsAsFactors = FALSE)
  }
}))

# Numbers with nWBV at each visit, by candidate population ------------------------
# Denominator: participants with nWBV at visit 1 and at least one later visit.
has_nwbv <- !is.na(d$nwbv)
elig_id  <- intersect(d$id[d$visit == 1 & has_nwbv], d$id[d$visit > 1 & has_nwbv])
pops <- list(
  "All participants"                              = p$id,
  "Non-demented at baseline by Group (ND + Conv)" = p$id[nd | cv],
  "Non-demented at baseline by CDR (CDR 0)"       = p$id[p$base_cdr == 0],
  "CDR 0 or 0.5 at baseline"                      = p$id[p$base_cdr <= 0.5]
)
risk_df <- do.call(rbind, lapply(names(pops), function(nm) {
  ids <- intersect(pops[[nm]], elig_id)
  den <- length(ids)
  cells <- vapply(1:5, function(k) {
    n <- sum(d$id %in% ids & d$visit == k & has_nwbv)
    if (n < 5) sc(n) else paste0(n, " (", pct(n, den), ")")
  }, character(1))
  out <- data.frame(Population = nm, `In population` = sc(length(pops[[nm]])),
                    `Eligible (baseline + 1 post)` = sc(den),
                    t(cells), check.names = FALSE, stringsAsFactors = FALSE)
  names(out)[4:8] <- paste("Visit", 1:5)
  out
}))

agree <- table(Group = ifelse(nd | cv, "ND or Converted", "Demented"),
               CDR   = ifelse(p$base_cdr == 0, "Baseline CDR 0", "Baseline CDR above 0"))
agree_df <- data.frame(`By Group` = rownames(agree),
                       matrix(sc(agree), nrow = nrow(agree)),
                       check.names = FALSE, stringsAsFactors = FALSE)
names(agree_df)[-1] <- colnames(agree)

# Write report -------------------------------------------------------------------
n_fail <- sum(checks$Result == "FAIL")
out <- c(
  "# Report 01: Quality control",
  "",
  paste0("- **Generated:** ", format(Sys.time(), "%Y-%m-%d %H:%M %Z"), " by `01_qc.R`"),
  paste0("- **Input:** `", basename(data_file), "` (local data directory, never committed)"),
  "- **Content:** aggregate only. Counts of 1 to 4 are shown as `<5`; summaries only for groups of 10 or more; no minima or maxima.",
  "- **Small-cell note:** a `<5` cell next to known row totals can sometimes be recovered by subtraction. Merge categories before publishing any of these tables.",
  "",
  paste0("**Overall: ", nrow(checks) - n_fail, " of ", nrow(checks), " checks passed, ",
         n_fail, " failed.** Failures must be explained and logged in the decision log."),
  "",
  "## Checks (structure, constants, ranges, Group against CDR, ASF x eTIV, counts)", "",
  md_table(checks), "",
  "## Baseline CDR by Group (participants)", "",
  md_table(tab_cdr_df), "",
  "## Missingness", "",
  md_table(miss_df), "",
  "## Time axis", "",
  time_lines, "",
  "## MR Delay by visit", "",
  md_table(delay_df), "",
  "## Participants with nWBV at each visit, by candidate population", "",
  "Percentages are of the eligible participants (nWBV at baseline and at least one later visit). The 60% rule (D-005) reads along each row.", "",
  md_table(risk_df), "",
  "## Agreement of the two baseline non-demented definitions (participants)", "",
  md_table(agree_df), ""
)
writeLines(out, file.path(report_dir, "01_qc_report.md"))
writeLines(capture.output(sessionInfo()), file.path(report_dir, "01_qc_sessioninfo.txt"))

cat(out, sep = "\n")
cat("\nReport written to", file.path(report_dir, "01_qc_report.md"), "\n")
