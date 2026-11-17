# Statistical Analysis Plan

**Natural-history MMRM and trial simulation in OASIS-2**

| | |
|---|---|
| Version | 1.0 DRAFT (not frozen) |
| Draft date | 2026-11-17 |
| Owner | Rushad Nihal |
| Status | Complete; awaiting owner approval. Becomes `docs/SAP_v1.0.md`, tagged `sap-v1.0`, on approval |
| Information used to write this plan | Schema, counts, visit timing and missingness only (internal QC reports 00, 01, 01a). No outcome values, change scores or model fits have been inspected |

Every count in this plan comes from count-only scripts (`R/00` to `R/02b`) run before any outcome was examined.

---

## 1. Background and scope

OASIS-2 is an observational longitudinal MRI cohort of 150 adults aged 60 and over with 373 imaging sessions. There is no treatment. This project has two parts:

- **Part A (natural history):** estimate change from baseline in normalised whole-brain volume (nWBV) and MMSE, using the methods a trial statistician would apply to a control arm.
- **Part B (simulation):** use the fitted Part A model to generate virtual two-arm trials with a hypothetical treatment effect, and report the operating characteristics of candidate analyses.

This plan does not estimate any real treatment effect and makes no clinical claims. OASIS-2 is not combined with any other dataset.

## 2. Objectives

- **A1 (primary):** estimate mean change from baseline in nWBV at analysis visits 2 and 3 (about 2 and 4 years from baseline) in the primary analysis population, using an MMRM. The primary timepoint is analysis visit 2.
- **A2:** describe change in MMSE and progression of CDR.
- **A3:** assess robustness of A1 to the covariance structure, the time model and missing-data assumptions.
- **B1:** quantify power, type I error, bias and coverage of candidate analyses of a hypothetical two-arm trial with this endpoint structure, under varied dropout.

## 3. Data source and structure

One spreadsheet with one row per imaging session and 15 columns. Facts established by QC:

- `Visit` (1 to 5) is neither session order nor a reliable guide to timing. Six participants have no visit 2 but have a later visit, and in the primary analysis population 24 of 140 post-baseline sessions (17%) fall in a different analysis window from the one their label suggests.
- `MR Delay` is cumulative days from the first visit. `Age` is age at each visit.
- nWBV has no missing values. MMSE is missing in fewer than 5 sessions, none at baseline. SES is missing for 8 participants.
- `Group` (Nondemented / Demented / Converted) is assigned using follow-up information and does not follow the CDR trajectory exactly. It is never used as a baseline covariate or to define a population.

Timing by the file's `Visit` label (all participants):

| Visit | n | Median years from baseline | Interquartile range (days) |
|---|---|---|---|
| 2 | 144 | 1.84 | 557 to 795 |
| 3 | 58 | 3.75 | 1128 to 1602 |
| 4 | 15 | 5.16 | 1792 to 2130 |
| 5 | 6 | not summarised | |

## 4. Analysis populations

- **Primary analysis population (PAP):** participants with CDR 0 at visit 1, a baseline nWBV, and nWBV at analysis visit 2 or 3 (Section 5). 85 participants; none of the 85 with CDR 0 at visit 1 is excluded.
- **Supportive population:** all participants with a baseline nWBV and nWBV at analysis visit 2 or 3, with baseline CDR (0, or 0.5 or above) as a covariate. Fewer than 5 of the 150 participants are excluded for having no session in either window. Baseline CDR is collapsed to two categories because fewer than 5 participants with CDR 1 or above have a visit 3.
- **Continuous-time population:** the PAP participants, using nWBV from all of their sessions.

A participant-flow table reports exclusions and counts. (Decision D-001.)

## 5. Analysis visits

Analysis visits are assigned by time from baseline, not by the file's `Visit` label.

| Analysis visit | Window (years from baseline) | Target | PAP participants | Median years (25th to 75th percentile) |
|---|---|---|---|---|
| 2 | 1 to under 3 | 2 | 75 (88%) | 1.90 (1.66 to 2.16) |
| 3 | 3 to under 5 | 4 | 42 (49%) | 3.97 (3.67 to 4.39) |

- If a participant has more than one session in a window, the session closest to the target is used; if tied, the earlier one.
- Sessions earlier than 1 year or at 5 years or later, and second sessions within a window, are not used in the MMRM. They are used in the continuous-time model (Section 9.3).
- In the PAP, 32 participants contribute to both analysis visits, 43 to visit 2 only and 10 to visit 3 only.
- Analysis visits are described by their window, never as a month.

**How this scheme was chosen.** The first choice was the `Visit` label. A pre-planned count-only check of that choice failed, and a count-only comparison (`R/02b_visit_window_check.R`) showed the label and the timing disagree for 17% of post-baseline sessions in the PAP. The windows were set from the median label-based visit times (1.99 and 3.91 years) before the comparison was run; no alternative windows were tried. No outcome was examined at any point. The label-based scheme is kept as a sensitivity analysis (Section 9.4). (Decision D-002, revised.)

## 6. Endpoints

- **Primary:** change from baseline in nWBV, expressed in percentage points (100 x nWBV), at analysis visit 2.
- **Primary timepoint rule:** the latest analysis visit at which at least 60% of the PAP has data. Applied to counts only: analysis visit 2 has 88%, analysis visit 3 has 49%, so the primary timepoint is analysis visit 2. (Decision D-005.)
- **Secondary:** change in nWBV at analysis visit 3; change from baseline in MMSE at analysis visits 2 and 3; CDR progression.

## 7. Estimand (ICH E9(R1))

| Attribute | Part A | Part B (simulated trial) |
|---|---|---|
| Population | Adults aged 60 and over with CDR 0 at baseline (PAP) | Virtual adults with the PAP baseline distribution |
| Variable | Change from baseline in nWBV (percentage points) at analysis visit 2 | Same, at the final visit |
| Intercurrent event: progression to CDR 0.5 or above | Treatment policy: measurements used regardless | Same |
| Dropout | Not an intercurrent event; missing data, MAR in the primary analysis, MNAR in sensitivity | Same |
| Summary measure | Mean change | Difference in mean change between arms |

The report describes, in one paragraph, how hypothetical, composite and while-on-condition strategies for progression would change the question. None is estimated.

## 8. Derived variables

`id` (internal integer), `avisit` (analysis visit by window, Section 5), `visit_label` (= `Visit`), `mmrm_fl` (session used in the primary MMRM), `years_since_bl` (= `MR Delay` / 365.25), `age_bl` (age at visit 1), `nwbv_pct` (= 100 x nWBV), `base_nwbv`, `chg_nwbv`, `base_mmse`, `chg_mmse`, `base_cdr`, `educ`, `sex`, `prog_flag` (CDR 0.5 or above at a post-baseline visit). Continuous covariates are centred at the PAP mean. `Hand` is dropped (constant).

## 9. Part A methods

### 9.1 Primary analysis: MMRM

- **Response:** `chg_nwbv` at analysis visits 2 and 3.
- **Fixed effects:** visit (factor), baseline `nwbv_pct`, baseline x visit, age at baseline, education. Sex is not included.
- **Covariance:** unstructured within participant (2 x 2, 3 parameters). REML. Kenward-Roger degrees of freedom.
- **Fallback:** if the unstructured model does not converge or its estimate is not positive definite, compound symmetry (2 parameters). With two post-baseline visits, heterogeneous Toeplitz, heterogeneous AR(1) and heterogeneous compound symmetry are the same model as unstructured, and Toeplitz and AR(1) are the same model as compound symmetry, so the hierarchy has two steps. Any fallback is logged. (Decision D-006, revised.)
- **Output:** least-squares mean change at each visit with 95% CI, at the mean of the covariates; covariance estimate; convergence status; number of participants contributing at each visit.
- **Software:** R `{mmrm}` (primary), SAS `PROC MIXED` (cross-check).

### 9.2 Covariance comparison (descriptive)

Unstructured against compound symmetry, by AIC and BIC from the REML fits with identical fixed effects. The primary analysis stays unstructured unless the fallback is triggered. The report states plainly that two post-baseline visits leave little covariance structure to compare.

### 9.3 Continuous-time sensitivity analysis

- Linear mixed model for `nwbv_pct` at all visits (1 to 5) on years since baseline, with a random intercept and random slope per participant (unstructured 2 x 2), REML.
- **Fixed effects:** time, age at baseline, education, age at baseline x time.
- **Reported quantity:** the implied mean change at 1.90 and 3.97 years (the PAP median times of analysis visits 2 and 3), with 95% CI, next to the MMRM estimates.
- This analysis is also the timing sensitivity analysis: it uses actual session times and every session, including the 23 PAP sessions the MMRM leaves out. Disagreement with the MMRM is reported and discussed.

### 9.4 Label-based sensitivity analysis

The primary MMRM refitted with the file's `Visit` label (2 and 3) as the visit in place of the analysis windows. In the PAP, 81 participants contribute at label visit 2 and 42 at label visit 3. Reported next to the primary estimates.

### 9.5 Supportive analysis

The primary MMRM in the supportive population, adding baseline CDR (0, or 0.5 or above) and its interaction with visit.

### 9.6 Secondary endpoints

- **MMSE:** the primary MMRM structure with `chg_mmse` as the response and baseline MMSE in place of baseline nWBV. 37 of the 85 PAP participants score 30 at baseline, so MMSE can only stay level or fall for them; counts at the ceiling at each visit are reported.
- **CDR progression:** in the PAP everyone starts at CDR 0, so progression is summarised as the proportion with CDR 0.5 or above at analysis visit 2 and at analysis visit 3, with Wilson 95% CIs, among those observed. Descriptive only. No ordinal model is fitted.

### 9.7 Demonstration contrast (machinery check only)

The primary MMRM with a baseline-fixed binary factor as a pseudo-arm and its interaction with visit. Factor: sex (D-004); the PAP has 59 women and 26 men. Reported as an observational association and labelled as such wherever it appears. Its only purpose is to exercise the between-group contrast for the SAS/R cross-check.

### 9.8 Missing data

- **Pattern:** of the 85 PAP participants, 75 have analysis visit 2 and 42 have analysis visit 3. Absence of visit 3 may reflect the end of the study's follow-up rather than participant withdrawal; the data do not distinguish the two. This is stated wherever dropout is discussed.
- **Summary:** missingness pattern table; baseline characteristics by whether visit 3 was observed (aggregate, small-cell rule).
- **Primary:** MAR, through the MMRM likelihood.
- **Sensitivity:** multiple imputation under MAR (same variables as the primary model), then delta adjustment: every imputed value is shifted by delta before analysis. Rubin's rules. Delta grid: 0 to -1.0 percentage points in steps of 0.1 (D-007). The baseline SD of `nwbv_pct` in the PAP is 3.79, so the largest shift is 0.26 baseline SDs. Between-person spread at baseline is not the natural scale for within-person change, and the SD of change is an outcome that has not been examined; results are therefore also reported with delta expressed in units of the observed SD of change.
- **Reported:** the mean change at visits 2 and 3 across the grid, and the smallest delta at which the estimate at each visit moves by more than one MAR standard error, or a statement that it does not within the grid. Because about 12% of the PAP is missing at analysis visit 2, the primary estimate is expected to move little with delta; this is a property of the data and is reported as such. The analysis is more informative at visit 3.
- Reference-based imputation needs arms and is applied in Part B and to the demonstration contrast only.

## 10. SAS/R cross-check

- The data terms bar giving participant data to any third party, and no local SAS installation is available. The cross-check is therefore run on the **synthetic** analysis dataset: the primary MMRM and the demonstration-contrast MMRM are fitted in R locally and in SAS OnDemand for Academics from the same synthetic file. Real OASIS-2 data are never uploaded to SAS OnDemand (D-009).
- The synthetic dataset has the same structure as the real analysis dataset (same variables, visits and missingness pattern), so the comparison tests the agreement of the two implementations of the identical model. It does not verify the real-data estimates in SAS; this is stated in the report.
- Compared: least-squares means, standard errors, contrast estimates, degrees of freedom, covariance parameters.
- **Acceptance:** maximum absolute difference in least-squares means and contrasts below 1e-4 percentage points; relative difference in standard errors below 1e-3. Larger differences are reported with their cause.
- Output: `crosscheck_report.md`.

## 11. Part B: simulation study (ADEMP)

**Aims.** Power and type I error of a two-arm trial with this endpoint structure, and how dropout mechanism and analysis method change them.

**Data-generating mechanism.**
- Two post-baseline visits (labelled visit 2 and visit 3). Control-arm mean changes and the 2 x 2 covariance come from the fitted primary MMRM, rounded, with positive definiteness checked.
- Baseline covariates drawn from fitted parametric distributions. No resampling of rows.
- Treatment effect: proportional slowing of mean decline, theta in {0, 0.1, 0.2, 0.3, 0.4}, applied at both visits. Theta = 0 gives type I error.
- Participants per arm: {30, 50, 80, 120, 200}.
- Cumulative missingness at the final visit: {0, 10, 20, 30}%. Mechanisms: M0 none; M1 MCAR; M2 MAR (depends on observed change at visit 2); M3 MNAR (depends on the unobserved change at the final visit, with different rates by arm).
- An additional scenario uses the observed missingness at visit 3 (about 50%) under M1 and M2.

**Estimand and primary timepoint.** Difference in mean change at the final visit (visit 3), treatment policy.

**Methods compared.** MMRM (MAR); multiple imputation with jump-to-reference; multiple imputation with copy-reference; delta-adjusted analysis on selected scenarios. Reference-based methods target a different estimand from the MAR MMRM, and this is stated wherever they are compared.

**Performance measures.** Bias, empirical SE, model SE, coverage of 95% CIs, power or type I error, RMSE, each with its Monte Carlo SE. Convergence failures are counted per scenario and never silently dropped.

**Replications.** 5000 per scenario for theta = 0; at least 2000 for theta above 0. Parallel random-number streams (L'Ecuyer-CMRG) with a recorded seed. Reporting follows Morris, White and Crowther (2019).

**Outputs.** Aggregate tables and figures and a sample-size table. No simulated participant-level data are committed. The grid may be reduced after a pilot run; any change is logged. (Decision D-008.)

## 12. Inference conventions

One primary endpoint at one timepoint; no multiplicity adjustment. Secondary and sensitivity analyses are labelled as such and carry no hypothesis-testing claims. Two-sided 95% CIs throughout. P-values are reported sparingly.

## 13. Planned tables and figures (aggregate only)

T1 participant flow. T2 baseline characteristics. T3 participants and timing at each visit. T4 primary MMRM. T5 covariance comparison. T6 continuous-time against MMRM. T7 delta-adjusted sensitivity. T8 secondary endpoints. T9 SAS/R cross-check. T10 simulation operating characteristics. T11 sample-size table.
F1 mean change by visit with CIs. F2 delta-adjustment curve. F3 power curves. F4 type I error across scenarios.

Any published cell with fewer than 5 participants is suppressed or merged, including cells recoverable by subtraction. No participant-level plots.

## 14. Limitations stated in advance

- Observational data; no treatment.
- 85 participants in the primary population: 75 at the primary timepoint and 42 at visit 3.
- Two post-baseline visits in the MMRM, so little covariance structure to study.
- Sessions are irregularly timed. Analysis windows are two years wide, and 23 of 140 post-baseline sessions in the primary population fall outside the MMRM.
- Missing visit 3 cannot be separated into withdrawal and end of follow-up.
- Few participants progress; progression questions are underpowered.
- MMSE has a ceiling in this population.
- Simulation results depend on the fitted model and on a visit 3 estimate from 42 participants.
- The SAS/R cross-check is run on synthetic data, not on the real data.
- One cohort, volunteer sample, no external validation.

## 15. Pre-specification record

All decisions below were made on 2026-11-17 from counts, visit timing and baseline quantities only.

| Item | Decision |
|---|---|
| Primary population (D-001) | CDR 0 at visit 1 |
| Visit scheme (D-002) | Time windows; the label-based scheme, chosen first, was replaced after a count-only check and is kept as a sensitivity analysis |
| Primary timepoint (D-005) | Analysis visit 2, by the 60% rule |
| Covariance (D-006) | Unstructured, then compound symmetry |
| Sex as a covariate in the primary MMRM | Not included |
| CDR progression | Proportions with Wilson CIs; no ordinal model |
| Baseline CDR in the supportive model | Two categories: 0, and 0.5 or above |
| Pseudo-arm for the demonstration contrast (D-004) | Sex |
| Delta grid (D-007) | 0 to -1.0 percentage points in steps of 0.1 |
| SAS route (D-009) | SAS OnDemand on synthetic data only |
| Primary timepoint of the simulated trial (D-008) | Final visit (visit 3) |

## 16. Deviations

After the `sap-v1.0` tag, any change is recorded in the decision log with the date, the section, the reason, and whether it was made before or after seeing outcomes. Analyses not in this plan are labelled exploratory in every table and figure caption.
