# Natural-history MMRM and trial simulation in OASIS-2

**Status: work in progress. The statistical analysis plan is in draft and no outcome has been analysed.**

This repository applies trial-style longitudinal methods to OASIS-2, an observational MRI cohort of older adults. It has two parts:

- **Part A, natural history.** Change from baseline in normalised whole-brain volume (primary) and MMSE (secondary), analysed as a trial statistician would analyse a control arm: a pre-specified MMRM, a continuous-time mixed model, and MAR and delta-adjusted MNAR sensitivity analyses.
- **Part B, simulation.** The fitted model is used to generate virtual two-arm trials with a hypothetical treatment effect, to report power, type I error, bias and coverage under different dropout mechanisms and analysis methods.

## What this is not

- It is not a treatment-effect analysis. OASIS-2 has no treatment arm, and no real treatment effect is estimated anywhere.
- Any between-group contrast fitted to exercise the MMRM code is an observational association and is labelled as such.
- It makes no clinical claims about Alzheimer's disease.
- It does not combine OASIS-2 with any other dataset.

## Contents so far

| Path | Content |
|---|---|
| `docs/SAP_v1.0_draft.md` | Draft statistical analysis plan, written from counts and visit timing only |
| `docs/data-use-statement.md` | Rules for handling the data and what may be published |
| `R/00_confirm_dataset.R` | Confirms the local file has the expected schema and size |
| `R/01_qc.R` | Quality-control checks and the count tables used to set the visit scheme |

The plan will be frozen as `docs/SAP_v1.0.md` and tagged `sap-v1.0` before any outcome analysis. Later changes are recorded as deviations.

## Data

No data are included. OASIS-2 is available from https://www.oasis-brains.org under its data use agreement. Scripts read the data location from the environment variable `OASIS2_DATA_DIR` and write aggregate reports to `OASIS2_REPORT_DIR`.

## Acknowledgement

Data were provided [in part] by OASIS-2: Longitudinal: Principal Investigators: D. Marcus, R, Buckner, J. Csernansky, J. Morris; P50 AG05681, P01 AG03991, P01 AG026276, R01 AG021910, P20 MH071616, U24 RR021382

## Licence

Code: MIT. Results are summary outputs subject to the OASIS data terms.
