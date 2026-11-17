/*------------------------------------------------------------------------------
  mmrm_crosscheck.sas

  Purpose : SAS side of the SAS/R cross-check (SAP v1.0, Section 10). Fits the
            primary MMRM and the demonstration-contrast MMRM with PROC MIXED on
            the SYNTHETIC cross-check dataset and exports the results as CSV
            for comparison with R (R/07b_sas_crosscheck.R).
  Data    : mmrm_input.csv from sas/crosscheck/ in the repository. Synthetic
            only: real OASIS-2 data must never be uploaded to SAS OnDemand.
  Models  : 1. chg_nwbv = visit, baseline nWBV, baseline nWBV x visit, age,
               education; unstructured covariance; REML; DDFM=KR
            2. model 1 + sex + sex x visit (observational pseudo-arm)
            LS means at centred covariates = 0 (the population means).
  Outputs : sas_ls1.csv, sas_cp1.csv, sas_cs1.csv   (model 1)
            sas_ls2.csv, sas_df2.csv, sas_cp2.csv, sas_cs2.csv (model 2)
            sas_version.csv
  Run     : in SAS Studio (SAS OnDemand for Academics):
            1. Create a folder, e.g. oasis2_crosscheck, under Files (Home).
            2. Upload mmrm_input.csv and this program into it.
            3. Edit ROOT below to that folder's path (right-click the folder,
               Properties, Location), then run the whole program.
            4. Download the eight sas_*.csv files from the same folder.
------------------------------------------------------------------------------*/

%let root = /home/u00000000/oasis2_crosscheck;   /* EDIT: your folder path */

proc import datafile="&root/mmrm_input.csv" out=adnw dbms=csv replace;
  guessingrows=max;
run;

proc sort data=adnw;
  by id avisit;
run;

/* Model 1: primary MMRM ------------------------------------------------------*/
proc mixed data=adnw method=reml;
  class id avisit;
  model chg_nwbv = avisit base_nwbv_c avisit*base_nwbv_c age_bl_c educ_c
        / ddfm=kr solution;
  repeated avisit / subject=id type=un;
  lsmeans avisit / at (base_nwbv_c age_bl_c educ_c) = (0 0 0) cl;
  ods output LSMeans=ls1 CovParms=cp1 ConvergenceStatus=cs1;
run;

/* Model 2: demonstration contrast, sex as pseudo-arm --------------------------*/
proc mixed data=adnw method=reml;
  class id avisit sex;
  model chg_nwbv = avisit base_nwbv_c avisit*base_nwbv_c age_bl_c educ_c
                   sex sex*avisit / ddfm=kr solution;
  repeated avisit / subject=id type=un;
  lsmeans sex*avisit / at (base_nwbv_c age_bl_c educ_c) = (0 0 0) cl diff;
  ods output LSMeans=ls2 Diffs=df2 CovParms=cp2 ConvergenceStatus=cs2;
run;

/* SAS version -----------------------------------------------------------------*/
data version;
  length sysvlong $60;
  sysvlong = "&sysvlong";
run;

/* Export ----------------------------------------------------------------------*/
/* ODS tables carry display formats (for example 4 decimals). Replace them
   with BEST32. so the CSV files hold full precision for the comparison. */
%macro export(ds, name);
  data _x_&name;
    set &ds;
    format _numeric_ best32.;
  run;
  proc export data=_x_&name outfile="&root/sas_&name..csv" dbms=csv replace;
  run;
%mend export;

%export(ls1, ls1)
%export(cp1, cp1)
%export(cs1, cs1)
%export(ls2, ls2)
%export(df2, df2)
%export(cp2, cp2)
%export(cs2, cs2)
%export(version, version)
