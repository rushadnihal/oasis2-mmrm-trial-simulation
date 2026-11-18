# Part B tables (simulated data)

Generated 2026-11-18 09:29 IST by `12_results_simulation.R`.

## T10: Analysis methods compared, 200 per arm

| Slowing (theta) | Dropout | Method | Bias | Empirical SE | Model SE | Coverage % | Rejection % |
|---|---|---|---|---|---|---|---|
| 0 | MAR 30% | MMRM (MAR) | -0.000 | 0.106 | 0.105 | 94.7 | 5.3 |
| 0 | MAR 30% | MI, jump to reference | 0.001 | 0.074 | 0.102 | 99.1 | 0.9 |
| 0 | MAR 30% | MI, copy reference | 0.002 | 0.079 | 0.102 | 98.6 | 1.5 |
| 0 | MAR 30% | MI, MAR with delta -0.3 (active arm) | -0.088 | 0.105 | 0.107 | 87.9 | 12.0 |
| 0 | MNAR 30% | MMRM (MAR) | 0.057 | 0.111 | 0.107 | 91.0 | 9.0 |
| 0 | MNAR 30% | MI, jump to reference | 0.031 | 0.061 | 0.102 | 99.6 | 0.4 |
| 0 | MNAR 30% | MI, copy reference | 0.031 | 0.068 | 0.101 | 99.4 | 0.6 |
| 0 | MNAR 30% | MI, MAR with delta -0.3 (active arm) | -0.080 | 0.108 | 0.109 | 88.8 | 11.2 |
| 0.2 | MAR 30% | MMRM (MAR) | 0.001 | 0.109 | 0.105 | 93.9 | 83.2 |
| 0.2 | MAR 30% | MI, jump to reference | -0.093 | 0.075 | 0.103 | 92.3 | 57.7 |
| 0.2 | MAR 30% | MI, copy reference | -0.080 | 0.081 | 0.103 | 93.2 | 64.0 |
| 0.2 | MAR 30% | MI, MAR with delta -0.3 (active arm) | -0.090 | 0.107 | 0.107 | 87.0 | 53.5 |
| 0.2 | MNAR 30% | MMRM (MAR) | 0.051 | 0.109 | 0.107 | 91.9 | 91.0 |
| 0.2 | MNAR 30% | MI, jump to reference | -0.107 | 0.063 | 0.103 | 93.1 | 50.0 |
| 0.2 | MNAR 30% | MI, copy reference | -0.087 | 0.070 | 0.102 | 94.2 | 62.0 |
| 0.2 | MNAR 30% | MI, MAR with delta -0.3 (active arm) | -0.076 | 0.113 | 0.109 | 88.3 | 56.9 |

Bias against the treatment-policy effect at the final visit (theta x 1.55 pp). Rejection % is type I error at theta = 0 and power otherwise. 2000 replicates per MI scenario; Monte Carlo SE of a 5% rate about 0.5 points. Reference-based MI targets a different estimand.

## T11: Participants per arm needed (MMRM, two-sided 5%)

| Slowing of decline | Effect at final visit, pp | 80% power | 90% power | 80% power, 30% MAR dropout | 90% power, 30% MAR dropout |
|---|---|---|---|---|---|
| 10% | 0.155 | more than 200 | more than 200 | more than 200 | more than 200 |
| 20% | 0.310 | 143 | 187 | 188 | more than 200 |
| 30% | 0.465 | 64 | 82 | 86 | 114 |
| 40% | 0.620 | 36 | 47 | 47 | 65 |

Linear interpolation between simulated sample sizes of 30, 50, 80, 120 and 200 per arm.

