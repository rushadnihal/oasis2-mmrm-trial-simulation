# Part A tables (aggregate; small-cell rule applied)

Generated 2026-11-18 09:29 IST by `11_results_part_a.R`.

## T1: Participant flow

| Step | Participants |
|---|---|
| Participants in OASIS-2 | 150 |
| CDR 0 at the first visit | 85 |
| Primary analysis population (CDR 0, nWBV at baseline and at analysis visit 2 or 3) | 85 |
| CDR 0.5 or above at the first visit | 65 |

No participant with CDR 0 at the first visit was excluded from the primary analysis population.
The supportive population is all participants with a session in an analysis window; fewer than 5 participants had none (count withheld).

## T2: Baseline characteristics

| Characteristic | CDR 0 (primary population) | CDR 0.5 or above |
|---|---|---|
| Participants | 85 | 65 |
| Age, years, mean (SD) | 75.8 (8.1) | 75.0 (6.8) |
| Women, n (%) | 59 (69%) | 29 (45%) |
| Education, years, mean (SD) | 15.2 (2.7) | 13.7 (2.9) |
| nWBV, percentage points, mean (SD) | 74.46 (3.79) | 72.48 (3.14) |
| MMSE, median (IQR) | 29 (29 to 30) | 26 (23 to 28) |

CDR 0.5 or above: all participants with CDR 0.5 or above at the first visit (descriptive only).

## T3: Analysis visits, primary analysis population

| Analysis visit | Window | Participants | Percent of population | Years from baseline, median (IQR) |
|---|---|---|---|---|
| 2 | 1 to under 3 years | 75 | 88% | 1.90 (1.66 to 2.16) |
| 3 | 3 to under 5 years | 42 | 49% | 3.97 (3.67 to 4.39) |

## T4: Primary analysis: MMRM, change from baseline in nWBV

| Analysis visit | Participants | Mean change, pp (95% CI) | SE | df | 95% CI, SAS DDFM=KR equivalent |
|---|---|---|---|---|---|
| 2 (primary) | 75 | -0.744 (-0.981 to -0.507) | 0.119 | 71.5 | -0.983 to -0.505 |
| 3 | 42 | -1.568 (-1.831 to -1.305) | 0.130 | 38.5 | -1.841 to -1.295 |

Unstructured covariance, REML, Kenward-Roger. Adjusted for baseline nWBV (with its interaction with visit), age and education, at the population means. pp: percentage points of intracranial volume.
Last column: supplementary interval with the linear Kenward-Roger covariance adjustment, which reproduces SAS PROC MIXED DDFM=KR (SAS/R cross-check, D-014).

## T5: Covariance structure comparison

| Structure | Covariance parameters | AIC | BIC |
|---|---|---|---|
| Unstructured (primary) | 3 | 346.58 | 353.90 |
| Compound symmetry | 2 | 345.53 | 350.41 |

Unstructured estimate: variance 1.102 at visit 2 and 0.817 at visit 3, correlation 0.52.

## T6: Robustness of the primary result (change in nWBV, pp)

| Analysis | Visit 2 (95% CI) | Visit 3 (95% CI) |
|---|---|---|
| Primary MMRM | -0.744 (-0.981 to -0.507) | -1.568 (-1.831 to -1.305) |
| Compound symmetry covariance | -0.742 (-0.969 to -0.515) | -1.586 (-1.874 to -1.299) |
| Continuous-time mixed model (at 1.90 and 3.97 years) | -0.769 (-0.894 to -0.644) | -1.606 (-1.868 to -1.345) |
| Visit label instead of time windows | -0.843 (-1.068 to -0.617) | -1.407 (-1.713 to -1.100) |
| All participants, CDR 0 stratum | -0.744 (-1.005 to -0.482) | -1.633 (-2.003 to -1.262) |

## T7: Delta-adjusted multiple imputation (change in nWBV, pp)

| Delta (pp) | Visit 2 (95% CI) | Visit 3 (95% CI) |
|---|---|---|
| 0.0 | -0.748 (-0.990 to -0.507) | -1.612 (-1.909 to -1.315) |
| -0.2 | -0.772 (-1.015 to -0.529) | -1.714 (-2.013 to -1.414) |
| -0.4 | -0.796 (-1.040 to -0.551) | -1.815 (-2.118 to -1.512) |
| -0.6 | -0.819 (-1.067 to -0.571) | -1.916 (-2.224 to -1.608) |
| -0.8 | -0.843 (-1.094 to -0.591) | -2.017 (-2.332 to -1.703) |
| -1.0 | -0.866 (-1.122 to -0.610) | -2.118 (-2.441 to -1.796) |

Multiple imputation under MAR (delta = 0), then every imputed value shifted by delta. Full grid in F2.

## T8: Secondary endpoints, primary analysis population

| Endpoint | Analysis visit 2 | Analysis visit 3 |
|---|---|---|
| MMSE change, points (95% CI) | -0.21 (-0.45 to 0.03) | -0.18 (-0.51 to 0.16) |
| CDR 0.5 or above, n of observed, % (Wilson 95% CI) | 8 of 75, 10.7% (5.5% to 19.7%) | 7 of 42, 16.7% (8.3% to 30.6%) |

MMSE ceiling: 37 of 85 score 30 at baseline, 32 at analysis visit 2 and 21 at analysis visit 3. Descriptive; no hypothesis tests.

