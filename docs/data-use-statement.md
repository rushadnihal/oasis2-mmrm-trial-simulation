# Data-use statement

This project uses OASIS-2 under the OASIS data use agreement. The rules below apply to everything in this repository.

1. **No participant-level data are committed.** That covers raw files, derived analysis datasets, per-person predictions, and logs that echo data. `.gitignore` blocks the common data file types.
2. **Aggregate outputs only.** Tables of estimates, group-level summaries and simulation results. No spaghetti plots, per-person scatterplots or anything that would let a reader reconstruct individuals.
3. **Small cells.** Any published cell with fewer than 5 participants is suppressed or merged with a neighbouring category, including cells that could be recovered by subtraction from a total.
4. **No third parties.** Participant data are analysed on the owner's machine only. They are not uploaded to cloud services, hosted notebooks, SAS OnDemand or AI assistants. The SAS/R cross-check is run on synthetic data for this reason.
5. **Synthetic data** are generated from hand-chosen or rounded aggregate parameters, never by resampling rows.
6. **One dataset.** OASIS-2 is not combined or linked with any other dataset, and no attempt is made to identify participants.
7. **Acknowledgement.** The OASIS acknowledgement in `DATA_ACKNOWLEDGEMENT.md` accompanies every public presentation of results.

To reproduce the analyses, obtain OASIS-2 from https://www.oasis-brains.org under your own agreement and set `OASIS2_DATA_DIR` to its location.
