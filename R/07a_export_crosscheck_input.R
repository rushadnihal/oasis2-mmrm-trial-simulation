# ------------------------------------------------------------------------------
# 07a_export_crosscheck_input.R
#
# Purpose : Export the MMRM analysis rows of the SYNTHETIC dataset as a CSV
#           for the SAS/R cross-check (SAP v1.0, Section 10). The SAS run uses
#           SAS OnDemand for Academics, a third-party service, so real OASIS-2
#           data must never reach this file (D-009). The script refuses to run
#           unless the data file is the synthetic one.
# Inputs  : derived/analysis_long.rds built from synthetic_oasis2.csv
#           (OASIS2_DATA_DIR, OASIS2_DATA_FILE as set by `make synth-ci`).
# Outputs : mmrm_input.csv in OASIS2_CROSSCHECK_DIR (default sas/crosscheck).
#           Columns: id, avisit (2 or 3), chg_nwbv, base_nwbv_c, age_bl_c,
#           educ_c, sex. Primary analysis population, primary MMRM rows.
# Run     : make synth-ci (runs this after the synthetic build)
# ------------------------------------------------------------------------------

data_file <- Sys.getenv("OASIS2_DATA_FILE", unset = "")
if (!grepl("^synthetic", basename(data_file))) {
  stop("Refusing to export: OASIS2_DATA_FILE is not the synthetic dataset. ",
       "The cross-check input must never contain real OASIS-2 data.")
}
data_dir <- Sys.getenv("OASIS2_DATA_DIR", unset = getwd())
out_dir  <- Sys.getenv("OASIS2_CROSSCHECK_DIR", unset = file.path("sas", "crosscheck"))
long     <- readRDS(file.path(data_dir, "derived", "analysis_long.rds"))
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

x <- long[long$pap_fl & long$mmrm_fl,
          c("id", "avisit", "chg_nwbv", "base_nwbv_c", "age_bl_c", "educ_c", "sex")]
x <- x[order(x$id, x$avisit), ]
stopifnot(all(x$avisit %in% 2:3), !anyNA(x))
write.csv(x, file.path(out_dir, "mmrm_input.csv"), row.names = FALSE)
cat("Cross-check input written:", nrow(x), "rows,", length(unique(x$id)), "participants\n")
