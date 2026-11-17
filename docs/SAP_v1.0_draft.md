# Statistical Analysis Plan

**Natural-history MMRM and trial simulation in OASIS-2**

| | |
|---|---|
| Version | 1.0 DRAFT (not frozen) |
| Draft date | 2026-11-17 |
| Owner | Rushad Nihal |
| Status | Design decisions closed 2026-11-17. Becomes `docs/SAP_v1.0.md`, tagged `sap-v1.0`, once the pending counts are filled |
| Information used to write this plan | Schema, counts, visit timing and missingness only (internal QC reports 00, 01, 01a). No outcome values, change scores or model fits have been inspected |

Items marked **[COUNT PENDING]** are counts or baseline quantities, not outcomes, to be filled from the analysis-data build script before the freeze.

---

## 1. Background and scope

OASIS-2 is an observational longitudinal MRI cohort of 150 adults aged 60 and over with 373 imaging sessions. There is no treatment. This project has two parts:

- **Part A (natural history):** estimate change from baseline in normalised whole-brain volume (nWBV) and MMSE, using the methods a trial statistician would apply to a control arm.
- **Part B (simulation):** use the fitted Part A model to generate virtual two-arm trials with a hypothetical treatment effect, and report the operating characteristics of candidate analyses.

This plan does not estimate any real treatment effect and makes no clinical claims. OASIS-2 is not combined with any other dataset.

## 2. Objectives

- **A1 (primary):** estimate mean change from baseline in nWBV at analysis visits 2 and 3 in the primary analysis population, using an MMRM. The primary timepoint is visit 2.
- **A2:** describe change in MMSE and progression of CDR.
- **A3:** assess robustness of A1 to the covariance structure, the time model and missing-data assumptions.
- **B1:** quantify power, type I error, bias and coverage of candidate analyses of a hypothetical two-arm trial with this endpoint structure, under varied dropout.

## 3. Data source and structure

One spreadsheet with one row per imaging session and 15 columns. Facts established by QC:

- `Visit` is a wave label (1 to 5), not session order. Six participants have no visit 2 but have a later visit.
- `MR Delay` is cumulative days from the first visit. `Age` is age at each visit.
- nWBV has no missing values. MMSE is missing in fewer than 5 sessions, none at baseline. SES is missing for 8 participants.
- `Group` (Nondemented / Demented / Converted) is assigned using follow-up information and does not follow the CDR trajectory exactly. It is never used as a baseline covariate or to define a population.

Visit timing (all participants):

| Visit | n | Median years from baseline | Interquartile range (days) |
|---|---|---|---|
| 2 | 144 | 1.84 | 557 to 795 |
| 3 | 58 | 3.75 | 1128 to 1602 |
| 4 | 15 | 5.16 | 1792 to 2130 |
| 5 | 6 | not summarised | |

## 4. Analysis populations

- **Primary analysis population (PAP):** participants with CDR 0 at visit 1, a baseline nWBV, and nWBV at analysis visit 2 or 3. 85 participants have CDR 0 at visit 1; 81 have nWBV at visit 2 and 42 at visit 3. Final PAP size: **[COUNT PENDING]**.
- **Supportive population:** all participants with a baseline nWBV and nWBV at visit 2 or 3, with baseline CDR (0, 0.5, 1 or above) as a covariate.
- **Continuous-time population:** the PAP definition, but using nWBV from all visits (1 to 5).

A participant-flow table reports exclusions and counts. (Decision D-001.)

## 5. Analysis visits

- The analysis visit is the `Visit` label in the file. A skipped visit is treated as missing; later sessions are not re-indexed.
- The MMRM uses visits 2 and 3. Visits 4 and 5 are excluded from the MMRM because 12 and 5 PAP participants cannot support covariance estimation. They are used only in the continuous-time model (Section 9.3).
- Visits are not described in months. They are described by label, with the observed median time from baseline.
- QC condition, pass/fail: for participants who skip visit 2, the next session is later than the 75th percentile of visit 2 timing. If this fails, it is logged and the visit scheme is reconsidered before the freeze.

(Decision D-002.)

## 6. Endpoints

- **Primary:** change from baseline in nWBV, expressed in percentage points (100 x nWBV), at visit 2.
- **Primary timepoint rule:** the latest analysis visit at which at least 60% of the PAP has data. Applied to counts only: visit 2 has 95%, visit 3 has 49%, so the primary timepoint is visit 2. (Decision D-005.)
- **Secondary:** change in nWBV at visit 3; change from baseline in MMSE at visits 2 and 3; CDR progression.

## 7. Estimand (ICH E9(R1))

| Attribute | Part A | Part B (simulated trial) |
|---|---|---|
| Population | Adults aged 60 and over with CDR 0 at baseline (PAP) | Virtual adults with the PAP baseline distribution |
| Variable | Change from baseline in nWBV (percentage points) at visit 2 | Same, at the final visit |
| Intercurrent event: progression to CDR 0.5 or above | Treatment policy: measurements used regardless | Same |
| Dropout | Not an intercurrent event; missing data, MAR in the primary analysis, MNAR in sensitivity | Same |
| Summary measure | Mean change | Difference in mean change between arms |

The report describes, in one paragraph, how hypothetical, composite and while-on-condition strategies for progression would change the question. None is estimated.

## 8. Derived variables

`id` (internal integer), `avisit` (= `Visit`), `years_since_bl` (= `MR Delay` / 365.25), `age_bl` (age at visit 1), `nwbv_pct` (= 100 x nWBV), `base_nwbv`, `chg_nwbv`, `base_mmse`, `chg_mmse`, `base_cdr`, `educ`, `sex`, `last_avisit`, `prog_flag` (CDR 0.5 or above at a post-baseline visit). Continuous covariates are centred at the PAP mean. `Hand` is dropped (constant).

## 9. Part A methods

### 9.1 Primary analysis: MMRM

- **Response:** `chg_nwbv` at visits 2 and 3.
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
- **Reported quantity:** the implied mean change at the PAP median time of visit 2 and of visit 3 **[COUNT PENDING: PAP medians]**, with 95% CI, next to the MMRM estimates.
- This analysis is also the timing sensitivity analysis: it uses actual session times and the data from visits 4 and 5. Disagreement with the MMRM is reported and discussed.
- A separate windowed-visit analysis is not planned.

### 9.4 Supportive analysis

The primary MMRM in the supportive population, adding baseline CDR category and baseline CDR x visit.

### 9.5 Secondary endpoints

- **MMSE:** the primary MMRM structure with `chg_mmse` as the response and baseline MMSE in place of baseline nWBV. Counts at the ceiling (30) at baseline and at each visit are reported.
- **CDR progression:** in the PAP everyone starts at CDR 0, so progression is summarised as the proportion with CDR 0.5 or above at visit 2 and at visit 3, with Wilson 95% CIs, among those observed. Descriptive only. No ordinal model is fitted.

### 9.6 Demonstration contrast (machinery check only)

The primary MMRM with a baseline-fixed binary factor as a pseudo-arm and its interaction with visit. Factor: sex (D-004). Reported as an observational association and labelled as such wherever it appears. Its only purpose is to exercise the between-group contrast for the SAS/R cross-check.

### 9.7 Missing data

- **Pattern:** among 85 participants with CDR 0 at baseline, 81 have visit 2 and 42 have visit 3. Absence of visit 3 may reflect the end of the study's follow-up rather than participant withdrawal; the data do not distinguish the two. This is stated wherever dropout is discussed.
- **Summary:** missingness pattern table; baseline characteristics by whether visit 3 was observed (aggregate, small-cell rule).
- **Primary:** MAR, through the MMRM likelihood.
- **Sensitivity:** multiple imputation under MAR (same variables as the primary model), then delta adjustment: every imputed value is shifted by delta before analysis. Rubin's rules. Delta grid: 0 to -1.0 percentage points in steps of 0.1 (D-007), to be confirmed against the baseline SD of `nwbv_pct` in the PAP **[COUNT PENDING]** before any estimate is seen.
- **Reported:** the mean change at visits 2 and 3 across the grid, and the smallest delta at which the estimate at each visit moves by more than one MAR standard error, or a statement that it does not within the grid. Because about 5% of the PAP is missing at visit 2, the primary estimate is expected to be insensitive to delta; this is a property of the data and is reported as such. The analysis is more informative at visit 3.
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
- 85 participants in the primary population, 42 at visit 3.
- Two post-baseline visits in the MMRM, so little covariance structure to study.
- Visits about two years apart with wide spread in timing.
- Missing visit 3 cannot be separated into withdrawal and end of follow-up.
- Few participants progress; progression questions are underpowered.
- MMSE has a ceiling in this population.
- Simulation results depend on the fitted model and on a visit 3 estimate from 42 participants.
- The SAS/R cross-check is run on synthetic data, not on the real data.
- One cohort, volunteer sample, no external validation.

## 15. Decisions closed on 2026-11-17 and counts still pending

| Item | Decision |
|---|---|
| Sex as a covariate in the primary MMRM | Not included |
| Windowed-visit sensitivity analysis | Dropped; the continuous-time model is the timing sensitivity analysis |
| CDR progression | Proportions with Wilson CIs; no ordinal model |
| Pseudo-arm for the demonstration contrast (D-004) | Sex |
| Delta grid (D-007) | 0 to -1.0 percentage points in steps of 0.1, subject to the baseline SD check |
| SAS route (D-009) | SAS OnDemand on synthetic data only |
| Primary timepoint of the simulated trial | Final visit (visit 3) |

Counts to fill before the freeze (none is an outcome): final PAP size; PAP median time of visits 2 and 3; baseline SD of `nwbv_pct` in the PAP; result of the skipped-visit timing check (Section 5).

## 16. Deviations

After the `sap-v1.0` tag, any change is recorded in the decision log with the date, the section, the reason, and whether it was made before or after seeing outcomes. Analyses not in this plan are labelled exploratory in every table and figure caption.
