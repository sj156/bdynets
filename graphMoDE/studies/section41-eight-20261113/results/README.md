# Eight-method results on D01 and D02

All eight methods were fitted anew on the two synthetic road panels. All 168 registered tasks finished successfully. Each of the six Bayesian methods used four chains per dataset, with 4,000 sweeps and 2,000 warmup sweeps per chain. STGNN used four final restarts per dataset. PNAR returns forecasts only.

ARI uses the Dahl representative partition and increases with agreement with the five generating groups. RMSE measures count predictions across 121 sites and 48 held-out times and is lower for more accurate forecasts.

| Method | D01 ARI | D02 ARI | D01 RMSE | D02 RMSE |
|---|---:|---:|---:|---:|
| Exp-W | 0.837 | 0.865 | 4.9864 | 5.0248 |
| SE-W | 0.893 | 0.890 | 4.9867 | 5.0241 |
| C | 0.863 | 0.828 | 4.9871 | 5.0271 |
| EucMoDE | 0.587 | 0.493 | 4.9906 | 5.0348 |
| MoDE | 0.542 | 0.412 | 4.9970 | 5.0390 |
| PottsMoDE | 0.625 | 0.877 | 4.9859 | 5.0254 |
| STGNN | 0.032 | 0.075 | 5.0572 | 5.0978 |
| PNAR | — | — | 5.0403 | 5.0830 |

SE-W had the highest representative-partition ARI on both datasets and its representative contained seven groups on each. PottsMoDE had the lowest observed predictive RMSE on D01 and SE-W on D02. Their RMSE differences were about 0.0008 and 0.0013 on D01 and D02, respectively.

All 12 Bayesian fits failed the registered cross-chain screen. In every one of the 48 chains, the occupied-group count stayed at its initial value throughout all 4,000 recorded sweeps. The reported average of 5.25 equals the arithmetic mean of the four starting values (1, 3, 7, 10). These results describe finite runs on two panels. The posterior ranking remains uncertain.

The [full table](comparison.csv) includes representative group counts and the maximum pairwise posterior similarity discrepancy. See the [chain summaries](chains.csv), [diagnostics](diagnostics.csv) and [run manifest](run-manifest.json) for the execution and verification details.
