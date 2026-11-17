# SAS/R cross-check (SAP v1.0, Section 10)

- **Data:** synthetic cross-check dataset `sas/crosscheck/mmrm_input.csv` (not OASIS-2; D-009)
- **Rows:** 111 from 83 synthetic participants
- **R:** 4.6.2, mmrm 0.3.19, emmeans 2.0.4
- **Models:** (1) primary MMRM; (2) primary MMRM plus sex and sex x visit. Unstructured covariance, REML, Kenward-Roger.
- **R variants:** A = `vcov = "Kenward-Roger"` (as in the primary analysis); B = `vcov = "Kenward-Roger-Linear"` (the mmrm-documented match to SAS DDFM=KR for unstructured covariance)
- **Tolerances (SAP 10):** |difference| in LS means and contrasts < 1e-04; relative difference in SEs < 0.001

- **SAS:** 9.04.01M8P022223

## Summary

- **Variant A (primary-analysis setting):** LS means and contrasts: **PASS** (max abs diff 2.7e-08); SEs: **FAIL** (max rel diff 3.0e-02); max |df difference| 0.00; max rel diff in covariance parameters 6.5e-07.
- **Variant B (Kenward-Roger-Linear):** LS means and contrasts: **PASS** (max abs diff 2.7e-08); SEs: **PASS** (max rel diff 4.2e-07); max |df difference| 0.00; max rel diff in covariance parameters 6.5e-07.
- SAS convergence status: model 1 0, model 2 0 (0 = converged).

## Variant A against SAS

| Quantity | R estimate | SAS estimate | abs diff | R SE | SAS SE | rel diff SE | R df | SAS df |
|---|---|---|---|---|---|---|---|---|
| Model 1 LS mean, visit 2 | -0.970301 | -0.970301 | 2.4e-09 | 0.063242 | 0.063694 | 7.1e-03 | 76.31 | 76.31 |
| Model 1 LS mean, visit 3 | -1.806568 | -1.806568 | 6.4e-09 | 0.103790 | 0.106888 | 2.9e-02 | 33.57 | 33.57 |
| Model 2 LS mean, F visit 2 | -0.938317 | -0.938317 | 1.5e-08 | 0.081133 | 0.081724 | 7.2e-03 | 75.69 | 75.69 |
| Model 2 LS mean, M visit 2 | -1.023641 | -1.023641 | 1.2e-08 | 0.104680 | 0.105436 | 7.2e-03 | 75.74 | 75.74 |
| Model 2 LS mean, F visit 3 | -1.767564 | -1.767564 | 1.1e-08 | 0.129052 | 0.132936 | 2.9e-02 | 32.68 | 32.68 |
| Model 2 LS mean, M visit 3 | -1.878605 | -1.878605 | 3.0e-09 | 0.177069 | 0.182520 | 3.0e-02 | 32.36 | 32.36 |
| Model 2 contrast M - F, visit 2 | -0.085323 | -0.085323 | 2.7e-08 | 0.133690 | 0.134664 | 7.2e-03 | 76.01 | 76.01 |
| Model 2 contrast M - F, visit 3 | -0.111041 | -0.111041 | 8.5e-09 | 0.217249 | 0.223863 | 3.0e-02 | 32.62 | 32.62 |

## Variant B against SAS

| Quantity | R estimate | SAS estimate | abs diff | R SE | SAS SE | rel diff SE | R df | SAS df |
|---|---|---|---|---|---|---|---|---|
| Model 1 LS mean, visit 2 | -0.970301 | -0.970301 | 2.4e-09 | 0.063694 | 0.063694 | 7.6e-08 | 76.31 | 76.31 |
| Model 1 LS mean, visit 3 | -1.806568 | -1.806568 | 6.4e-09 | 0.106888 | 0.106888 | 5.3e-08 | 33.57 | 33.57 |
| Model 2 LS mean, F visit 2 | -0.938317 | -0.938317 | 1.5e-08 | 0.081724 | 0.081724 | 5.3e-08 | 75.69 | 75.69 |
| Model 2 LS mean, M visit 2 | -1.023641 | -1.023641 | 1.2e-08 | 0.105436 | 0.105436 | 5.0e-08 | 75.74 | 75.74 |
| Model 2 LS mean, F visit 3 | -1.767564 | -1.767564 | 1.1e-08 | 0.132936 | 0.132936 | 4.0e-07 | 32.68 | 32.68 |
| Model 2 LS mean, M visit 3 | -1.878605 | -1.878605 | 3.0e-09 | 0.182521 | 0.182520 | 4.2e-07 | 32.36 | 32.36 |
| Model 2 contrast M - F, visit 2 | -0.085323 | -0.085323 | 2.7e-08 | 0.134664 | 0.134664 | 5.2e-08 | 76.01 | 76.01 |
| Model 2 contrast M - F, visit 3 | -0.111041 | -0.111041 | 8.5e-09 | 0.223863 | 0.223863 | 4.1e-07 | 32.62 | 32.62 |

## Covariance parameters

| Quantity | R | SAS | rel diff |
|---|---|---|---|
| Model 1 UN(1,1) | 0.323317 | 0.323317 | 1.4e-07 |
| Model 1 UN(2,1) | 0.180249 | 0.180249 | 6.5e-07 |
| Model 1 UN(2,2) | 0.397361 | 0.397361 | 1.4e-07 |
| Model 2 UN(1,1) | 0.326068 | 0.326068 | 7.6e-08 |
| Model 2 UN(2,1) | 0.181767 | 0.181767 | 6.4e-07 |
| Model 2 UN(2,2) | 0.407233 | 0.407233 | 4.6e-07 |
