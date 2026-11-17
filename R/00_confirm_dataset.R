# ------------------------------------------------------------------------------
# 00_confirm_dataset.R
#
# Purpose : Confirm that the local file is the OASIS-2 longitudinal demographics
#           spreadsheet expected by this project.
#           Prints schema checks and aggregate counts only. No participant rows,
#           IDs or per-person values are printed or written.
# Inputs  : longitudinaldemo.xlsx in the data directory. The data directory is
#           the environment variable OASIS2_DATA_DIR if set, otherwise the
#           current working directory.
# Outputs : Console output (aggregate only)
#           00_confirm_dataset_sessioninfo.txt in the data directory.
# Run     : set OASIS2_DATA_DIR (for example in ~/.Renviron), then
#           source("R/00_confirm_dataset.R")
# Note    : Cells with fewer than 5 participants are shown as "<5"
#           (small-cell rule, docs/data-use-statement.md).
# ------------------------------------------------------------------------------

library(readxl)

data_dir  <- Sys.getenv("OASIS2_DATA_DIR", unset = getwd())
data_file <- file.path(data_dir, "longitudinaldemo.xlsx")
stopifnot("Data file not found in data directory" = file.exists(data_file))

d <- as.data.frame(read_excel(data_file))

suppress_small <- function(tab, min_n = 5) {
  out <- as.character(tab)
  out[tab > 0 & tab < min_n] <- paste0("<", min_n)
  names(out) <- names(tab)
  noquote(out)
}

# 1. Schema ---------------------------------------------------------------------
expected <- c("Subject ID", "MRI ID", "Group", "Visit", "MR Delay", "M/F",
              "Hand", "Age", "EDUC", "SES", "MMSE", "CDR", "eTIV", "nWBV", "ASF")

cat("\n== 1. Schema ==\n")
cat("Columns found (", ncol(d), "):\n", sep = "")
print(names(d))
cat("Expected but missing :", paste(setdiff(expected, names(d)), collapse = ", "), "\n")
cat("Found but unexpected :", paste(setdiff(names(d), expected), collapse = ", "), "\n")
cat("Column classes:\n")
print(vapply(d, function(x) class(x)[1], character(1)))

stopifnot("Core columns missing; cannot continue" =
            all(c("Subject ID", "Group", "Visit", "MR Delay") %in% names(d)))

# 2. Size -----------------------------------------------------------------------
cat("\n== 2. Size ==\n")
cat("Sessions (rows)      :", nrow(d), "\n")
cat("Participants (unique):", length(unique(d[["Subject ID"]])), "\n")
cat("Subject ID pattern OAS2_dddd for all rows:",
    all(grepl("^OAS2_[0-9]{4}$", d[["Subject ID"]])), "\n")

# 3. Group counts (participant level, taken at first visit) ---------------------
cat("\n== 3. Group counts at visit 1 (participants) ==\n")
v1 <- d[d$Visit == 1, ]
cat("Participants with a visit 1 row:", nrow(v1), "\n")
print(suppress_small(table(v1$Group, useNA = "ifany")))

# 4. Visit structure ------------------------------------------------------------
cat("\n== 4. Visit structure ==\n")
cat("Visit numbers present:", paste(sort(unique(d$Visit)), collapse = ", "), "\n")
cat("MR Delay is 0 at every visit 1:", all(v1[["MR Delay"]] == 0), "\n")
cat("Participants by number of visits:\n")
print(suppress_small(table(table(d[["Subject ID"]]))))

# 5. Missing values per column (counts) -----------------------------------------
cat("\n== 5. Missing values per column ==\n")
print(colSums(is.na(d)))

# Session info ------------------------------------------------------------------
writeLines(capture.output(sessionInfo()),
           file.path(data_dir, "00_confirm_dataset_sessioninfo.txt"))
cat("\nSession info written to 00_confirm_dataset_sessioninfo.txt\n")
