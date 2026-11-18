#!/usr/bin/env bash
# Fails if any tracked file contains an OASIS-2 participant identifier, or if a
# data file is tracked. Run from the repository root.
set -euo pipefail
status=0

if git grep -n -I -E 'OAS2_[0-9]{4}' -- . ; then
  echo "ERROR: OASIS-2 participant identifiers found in tracked files." >&2
  status=1
fi

# Allow-list: synthetic SAS/R cross-check files (R/07a_export_crosscheck_input.R) and
# aggregate results tables (R/10, R/11, R/12); none holds participant-level data
data_files=$(git ls-files | grep -i -E '\.(xlsx|xls|csv|rds|rdata|rda|sas7bdat|parquet|feather)$' \
  | grep -v -E '^(sas/crosscheck/|results/(simulation|part_a)/[^/]+\.csv$)' || true)
if [ -n "$data_files" ]; then
  echo "ERROR: data files are tracked:" >&2
  echo "$data_files" >&2
  status=1
fi

if [ "$status" -eq 0 ]; then
  echo "OK: no participant identifiers and no data files in tracked files."
fi
exit $status
