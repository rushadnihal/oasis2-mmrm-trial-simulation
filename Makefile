# Targets
#   synth-data   generate the synthetic dataset
#   synth-ci     run the pipeline on the synthetic dataset (no real data needed)
#   data-check   run the dataset confirmation and QC on the real data
#   build        build the analysis datasets from the real data
#   analysis     Part A analyses on the real data
#   results      publication tables and figures (Part A real data; Part B)
#   report       render report/report.pdf from the committed results (Quarto)
#   sas-crosscheck  compare committed SAS output with R (synthetic data only)
#   sim-pilot    simulation pilot (Part B); no data needed
#   sim-smoke    tiny simulation run used by CI
#   sim          full simulation (hours; resumable)
#   sim-summary  performance tables from the full simulation
#   check-data   verify that no participant data are tracked by git
#   clean        remove synthetic data and synthetic reports
#
# Real-data targets need OASIS2_DATA_DIR (and optionally OASIS2_REPORT_DIR) set.

SYNTH_ENV = OASIS2_DATA_DIR=synthetic_data OASIS2_DATA_FILE=synthetic_oasis2.csv OASIS2_REPORT_DIR=synthetic_reports

.PHONY: synth-data synth-ci data-check build analysis results report sas-crosscheck sim-pilot sim-smoke sim sim-summary check-data clean

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
	$(SYNTH_ENV) OASIS2_CROSSCHECK_DIR=synthetic_reports/crosscheck Rscript R/07a_export_crosscheck_input.R
	Rscript R/07b_sas_crosscheck.R
	$(SYNTH_ENV) OASIS2_RESULTS_DIR=synthetic_reports/publish/part_a Rscript R/11_results_part_a.R
	OASIS2_RESULTS_DIR=synthetic_reports/publish/simulation Rscript R/12_results_simulation.R

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

results:
	Rscript R/11_results_part_a.R
	Rscript R/12_results_simulation.R

report:
	cd report && quarto render report.qmd

sas-crosscheck:
	Rscript R/07b_sas_crosscheck.R

sim-pilot:
	Rscript R/08_simulation_pilot.R

sim-smoke:
	SIM_REPS=4 SIM_NSAMPLES=3 SIM_CORES=2 OASIS2_REPORT_DIR=synthetic_reports Rscript R/08_simulation_pilot.R
	SIM_REPS_SCALE=0.0005 SIM_NSAMPLES=3 SIM_CORES=2 OASIS2_SIM_DIR=synthetic_reports/sim Rscript R/09_simulation_full.R
	OASIS2_SIM_DIR=synthetic_reports/sim Rscript R/10_simulation_summary.R

sim:
	caffeinate -i Rscript R/09_simulation_full.R

sim-summary:
	Rscript R/10_simulation_summary.R

check-data:
	bash scripts/check_no_participant_data.sh

clean:
	rm -rf synthetic_data synthetic_reports
