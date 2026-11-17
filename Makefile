# Targets
#   synth-data   generate the synthetic dataset
#   synth-ci     run the pipeline on the synthetic dataset (no real data needed)
#   data-check   run the dataset confirmation and QC on the real data
#   build        build the analysis datasets from the real data
#   analysis     Part A analyses on the real data
#   check-data   verify that no participant data are tracked by git
#   clean        remove synthetic data and synthetic reports
#
# Real-data targets need OASIS2_DATA_DIR (and optionally OASIS2_REPORT_DIR) set.

SYNTH_ENV = OASIS2_DATA_DIR=synthetic_data OASIS2_DATA_FILE=synthetic_oasis2.csv OASIS2_REPORT_DIR=synthetic_reports

.PHONY: synth-data synth-ci data-check build analysis check-data clean

synth-data:
	Rscript R/synthetic/make_synthetic_oasis2.R

synth-ci: synth-data
	$(SYNTH_ENV) Rscript R/00_confirm_dataset.R
	$(SYNTH_ENV) Rscript R/01_qc.R
	$(SYNTH_ENV) Rscript R/02_build_analysis_data.R
	$(SYNTH_ENV) Rscript R/02b_visit_window_check.R
	$(SYNTH_ENV) Rscript R/03_mmrm_primary.R
	$(SYNTH_ENV) Rscript R/04_robustness.R
	$(SYNTH_ENV) Rscript R/05_secondary_and_contrast.R
	$(SYNTH_ENV) Rscript R/06_missing_data.R

data-check:
	Rscript R/00_confirm_dataset.R
	Rscript R/01_qc.R

build:
	Rscript R/02_build_analysis_data.R

analysis:
	Rscript R/03_mmrm_primary.R
	Rscript R/04_robustness.R
	Rscript R/05_secondary_and_contrast.R
	Rscript R/06_missing_data.R

check-data:
	bash scripts/check_no_participant_data.sh

clean:
	rm -rf synthetic_data synthetic_reports
