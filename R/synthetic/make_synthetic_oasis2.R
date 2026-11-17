# ------------------------------------------------------------------------------
# make_synthetic_oasis2.R
#
# Purpose : Generate a synthetic dataset with the same columns and general
#           shape as the OASIS-2 longitudinal demographics file, so that the
#           whole pipeline can be run and tested without the real data (CI,
#           reviewers, and the SAS/R cross-check of SAP Section 10).
# Method  : Every value is drawn from the parametric distributions written out
#           below. The parameters are hand-chosen round numbers. Structural
#           ones (number of participants, group sizes, typical visit spacing,
#           baseline age and education) are rounded from published aggregates.
#           Outcome-related ones (rates of change, their spread, residual
#           noise) are NOT estimated from OASIS-2: they were set by hand before
#           any OASIS-2 outcome was examined. No real row is read, resampled or
#           perturbed, so the file cannot reveal anything about a participant.
#           Results computed on this file say nothing about OASIS-2.
# Inputs  : none.
# Outputs : synthetic_oasis2.csv in OASIS2_SYNTH_DIR (default synthetic_data/).
#           Identifiers start with SYN2_ so they cannot be mistaken for real ones.
# Run     : Rscript R/synthetic/make_synthetic_oasis2.R
# ------------------------------------------------------------------------------

set.seed(20261117)
out_dir <- Sys.getenv("OASIS2_SYNTH_DIR", unset = "synthetic_data")
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

clip <- function(x, lo, hi) pmin(pmax(x, lo), hi)

# Participants -------------------------------------------------------------------
n        <- 150
group    <- rep(c("Nondemented", "Converted", "Demented"), c(72, 14, 64))
base_cdr <- rep(c(0, 0.5, 1), c(86, 51, 13))      # baseline CDR; 0 for the first 86
ci       <- match(base_cdr, c(0, 0.5, 1))         # index into per-CDR parameter vectors

sex    <- sample(c("F", "M"), n, replace = TRUE, prob = c(0.58, 0.42))
age_bl <- clip(rnorm(n, 76, 8), 60, 86)           # exact age; reported as whole years
educ   <- clip(round(rnorm(n, 15, 2.7)), 6, 23)
ses    <- sample(1:5, n, replace = TRUE, prob = c(0.25, 0.30, 0.25, 0.15, 0.05))
ses[sample(which(group == "Demented"), 8)] <- NA  # a few missing, as in the real file
etiv   <- round(clip(rnorm(n, 1490, 175), 1100, 2000))
asf    <- round(1755 / etiv, 3)

# Baseline outcomes
nwbv0 <- clip(0.745 - 0.0035 * (age_bl - 76) - c(0, 0.015, 0.030)[ci] + rnorm(n, 0, 0.025),
              0.64, 0.86)
mmse0 <- ifelse(base_cdr == 0,
                sample(30:26, n, replace = TRUE, prob = c(0.43, 0.30, 0.15, 0.08, 0.04)),
                clip(round(rnorm(n, c(29, 26, 21)[ci], c(1, 3, 4)[ci])), 10, 30))

# Sessions -----------------------------------------------------------------------
n_sess  <- sample(2:5, n, replace = TRUE, prob = c(0.63, 0.28, 0.06, 0.03))
skip_v2 <- seq_len(n) %in% sample(which(n_sess <= 4), 6)   # visit label skips 2

# Hand-chosen outcome parameters, by baseline CDR (0, 0.5, 1)
slope_mean <- c(-0.0045, -0.0065, -0.0080)   # nWBV change per year
slope_sd   <- 0.0015
resid_sd   <- 0.004
mmse_rate  <- c(-0.1, -1.0, -1.5)            # MMSE change per year
mmse_sd    <- c(1, 2, 2.5)

rows <- vector("list", n)
for (i in seq_len(n)) {
  k <- n_sess[i]
  gaps  <- c(clip(rnorm(1, 1.9, 0.6), 0.5, 3.6), clip(rnorm(k - 2, 1.9, 0.5), 1.0, 3.0))
  years <- c(0, cumsum(gaps))
  days  <- round(years * 365.25)
  label <- if (skip_v2[i]) c(1, 3:(k + 1)) else seq_len(k)

  slope <- rnorm(1, slope_mean[ci[i]], slope_sd)
  nwbv  <- clip(nwbv0[i] + slope * years + c(0, rnorm(k - 1, 0, resid_sd)), 0.61, 0.89)

  mmse <- c(mmse0[i],
            clip(round(mmse0[i] + rnorm(k - 1, mmse_rate[ci[i]] * years[-1], mmse_sd[ci[i]])),
                 0, 30))

  cdr <- if (group[i] == "Nondemented") {
    rep(0, k)
  } else if (group[i] == "Converted") {
    onset <- if (k == 2) 2 else sample(2:k, 1)
    c(rep(0, onset - 1), rep(0.5, k - onset + 1))
  } else {
    steps <- cumsum(c(0, rbinom(k - 1, 1, 0.15)))
    c(0, 0.5, 1, 2)[pmin(match(base_cdr[i], c(0, 0.5, 1, 2)) + steps, 4)]
  }

  id <- sprintf("SYN2_%04d", i)
  rows[[i]] <- data.frame(
    `Subject ID` = id,
    `MRI ID`     = sprintf("%s_MR%d", id, label),
    Group        = group[i],
    Visit        = label,
    `MR Delay`   = days,
    `M/F`        = sex[i],
    Hand         = "R",
    Age          = floor(age_bl[i] + years),
    EDUC         = educ[i],
    SES          = ses[i],
    MMSE         = mmse,
    CDR          = cdr,
    eTIV         = etiv[i],
    nWBV         = round(nwbv, 3),
    ASF          = asf[i],
    check.names = FALSE, stringsAsFactors = FALSE
  )
}
syn <- do.call(rbind, rows)

# A couple of missing follow-up MMSE values, as in the real file
syn$MMSE[sample(which(syn$Visit > 1), 2)] <- NA

out_file <- file.path(out_dir, "synthetic_oasis2.csv")
write.csv(syn, out_file, row.names = FALSE)
cat("Synthetic data written to", out_file, ":", nrow(syn), "sessions,",
    length(unique(syn[["Subject ID"]])), "participants\n")
