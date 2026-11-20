# graphMoDE simulation results

On November 14, 2026 (Shanghai time), all eight methods completed fresh fits on the two synthetic road networks D01 and D02. **SE-W had the highest partition agreement on both datasets. None of the 12 Bayesian fits passed the registered convergence checks.** The leading 48-step forecasts were close, and the method with the lowest forecast error differed between datasets.

## Partition recovery and 48-step forecasts

Each dataset has 121 sites, 120 direct road edges and five generating groups. The five seasonal Poisson profiles differ in phase and have the same mean count of 25 over a 24-step cycle. All methods trained on times 1–168 and forecast the same 48 held-out times, 169–216. These are two independent realizations of one design.

ARI is the adjusted Rand index against the generating groups (higher is better). Groups is the number of groups in the representative partition. For each Bayesian fit, the Dahl representative is the sampled partition closest to the pooled pairwise co-membership frequencies across 8,000 retained draws. STGNN uses four fitted restart partitions. PNAR only predicts counts. RMSE is the root mean squared count error over all 121 sites and 48 horizons (lower is better).

| Method | D01 ARI | D01 groups | D01 RMSE | D02 ARI | D02 groups | D02 RMSE |
|:--|--:|--:|--:|--:|--:|--:|
| Exp-W | 0.837 | 7 | 4.9864 | 0.865 | 7 | 5.0248 |
| SE-W | 0.893 | 7 | 4.9867 | 0.890 | 7 | 5.0241 |
| C | 0.863 | 7 | 4.9871 | 0.828 | 7 | 5.0271 |
| EucMoDE | 0.587 | 3 | 4.9906 | 0.493 | 3 | 5.0348 |
| MoDE | 0.542 | 3 | 4.9970 | 0.412 | 3 | 5.0390 |
| PottsMoDE | 0.625 | 3 | 4.9859 | 0.877 | 7 | 5.0254 |
| STGNN | 0.032 | 5 | 5.0572 | 0.075 | 3 | 5.0978 |
| PNAR | — | — | 5.0403 | — | — | 5.0830 |

**Partition recovery.** SE-W attained ARIs of 0.893 and 0.890, although its representative partition contained seven groups on each dataset. Binary connectivity (C) scored higher than Exp-W on D01 (0.863 versus 0.837) and lower on D02 (0.828 versus 0.865). PottsMoDE recovered three groups on D01 and seven on D02, with ARIs of 0.625 and 0.877.

**Forecast accuracy.** PottsMoDE had the lowest RMSE on D01 (4.9859), and SE-W had the lowest on D02 (5.0241). Exp-W ranked second on both, trailing by about 0.0005 and 0.0007 counts in RMSE, respectively. STGNN and PNAR had larger forecast errors than all six Bayesian methods on both panels. The test period measures point-forecast error. Predictive interval coverage was not evaluated.

## What the chains explored

All 48 final Bayesian chains completed 4,000 sweeps each, including 2,000 warmup sweeps. The four chains for each fit started with 1, 3, 7 and 10 occupied groups. **Every chain kept its own starting group count for all 4,000 sweeps.** Individual sites could move between occupied groups. The pooled mean of 5.25 occupied groups is exactly (1 + 3 + 7 + 10) / 4. Its proximity to the five generating groups tells us nothing about recovery of the group count.

The table gives rank-normalized split R̂ and bulk and tail effective sample sizes (ESS) for the mean baseline discount across occupied experts in each draw. ΔPSM is the largest pairwise root mean squared difference between the chains’ posterior similarity matrices, over distinct site pairs. These matrices record how often each pair of sites shares a group, so the diagnostic is invariant to class labels. The registered limits were R̂ < 1.01, both ESS values ≥ 400, and ΔPSM ≤ 0.10.

| Dataset | Bayesian method | R̂ | Bulk ESS | Tail ESS | ΔPSM |
|:--|:--|--:|--:|--:|--:|
| D01 | Exp-W | 1.821 | 5.97 | 25.74 | 0.916 |
| D01 | SE-W | 1.949 | 5.64 | 22.40 | 0.915 |
| D01 | C | 1.882 | 5.69 | 10.62 | 0.921 |
| D01 | EucMoDE | 2.746 | 4.68 | 18.97 | 0.912 |
| D01 | MoDE | 2.930 | 4.58 | 11.65 | 0.885 |
| D01 | PottsMoDE | 1.951 | 5.50 | 25.17 | 0.913 |
| D02 | Exp-W | 2.171 | 5.14 | 11.31 | 0.915 |
| D02 | SE-W | 1.431 | 8.33 | 30.95 | 0.913 |
| D02 | C | 2.063 | 5.34 | 18.28 | 0.922 |
| D02 | EucMoDE | 1.846 | 5.96 | 30.84 | 0.904 |
| D02 | MoDE | 2.054 | 5.47 | 20.30 | 0.872 |
| D02 | PottsMoDE | 1.852 | 5.87 | 15.70 | 0.919 |

ΔPSM ranged from 0.872 to 0.922. The chains therefore disagreed substantially about which sites belonged together as well as about how many groups were occupied. The ARIs and forecast errors describe these runs. With this dependence on initialization and only two datasets, the comparison does not establish a general ranking of the methods.

## Models, fitting and validation

- **Exp-W, SE-W, C and EucMoDE** used independent class-score Gaussian processes with a softmax gate, smoothness 3 and covariance multiplier 1. Exp-W and SE-W learned the bandwidth of the raw weights exp(−d/h) and exp(−(d/h)²) on the same direct edges. C used binary adjacency. EucMoDE used a dense planar Matérn covariance. All four range priors were calibrated from geometry alone.
- **MoDE and PottsMoDE** shared the same dynamic Poisson experts as the four GP methods. MoDE used a global symmetric Dirichlet prior with concentration 0.1 per class. PottsMoDE used raw exponential edge weights at fixed bandwidth 1. Training-prefix forecasts selected interaction strength 4 on both datasets from {0.25, 1, 4}.
- **All six Bayesian methods** had fitted capacity K = 10, independent Beta(49, 1) baseline discounts, Poisson adjustment c = log(2), initial mean (log(25), 0, 0) and initial covariance diag(1, 0.5, 0.5). The Metropolis-corrected Pólya–Gamma state proposals used ρ = 1 and 24-time-point blocks. Each retained draw contributed four simulated future histories, updating the guide with simulated counts and averaging conditional one-step means at each horizon.
- **STGNN and PNAR** used the fixed exponential reference graph and their respective author implementations. Selection used 48-step forecasts from training origins 96 and 120. PNAR selected lag 24 on both panels. STGNN selected five slots and 25 epochs on D01, three slots and 150 epochs on D02, and pooling-loss weight 1 on both. The generating labels and final test counts were reserved for evaluation.

All 168 registered tasks completed, including training-prefix validation, final fits and integration checks. The 48 Bayesian chains are included in that total. The following files contain the reported values at full precision and the source and input identifiers.

- [Comparison metrics](studies/section41-eight-20261113/results/comparison.csv) and [run manifest](studies/section41-eight-20261113/results/run-manifest.json)
- [Chain summaries](studies/section41-eight-20261113/results/chains.csv), [scalar diagnostics](studies/section41-eight-20261113/results/diagnostics.csv) and [pairwise PSM differences](studies/section41-eight-20261113/results/pairs.csv)
- [Status of all 168 tasks](studies/section41-eight-20261113/results/task-status.csv)

## Package functions and study scripts

| Package source | Role in this study |
|:--|:--|
| [gmde-discount.R](../bdynets/R/gmde-discount.R) | `gmde_discount_guide()` replays the Poisson discount guide. `gmde_transition_log_density()` evaluates Gaussian state transitions. |
| [gmde-dynamic.R](../bdynets/R/gmde-dynamic.R) | `gmde_dynamic_fit()` fits one Bayesian chain using the selected allocation model. |
| [gmde-dynamic-comparison.R](../bdynets/R/gmde-dynamic-comparison.R) | `gmde_dynamic_predict()` averages forecasts over simulated future histories. This file also contains the Euclidean and Potts helpers. |
| [gmde-dynamic-diagnostics.R](../bdynets/R/gmde-dynamic-diagnostics.R) | `gmde_dynamic_diagnostics()` summarizes the retained chains and compares their posterior similarity matrices. |
| [gmde-pnar-benchmark.R](../bdynets/R/gmde-pnar-benchmark.R) | Internal helpers fit the external PNAR model and compute recursive count forecasts. |

The package calls the [discount kernels](../bdynets/src/gmde-discount.cpp) and [dynamic-model kernels](../bdynets/src/gmde-dynamic.cpp) in C++. The [study driver](scripts/run-section41-eight.py) orchestrates [input preparation](scripts/prepare-section41-eight.R), [registered fits](scripts/run-section41-eight.R) and [summaries](scripts/summarize-section41-eight.R). Reproducing the run requires the archived synthetic inputs, matching registrations and pinned external benchmark dependencies. Input hashes and the selected tuning values are in the run manifest above.

## Earlier implementation and frozen input provenance

D01 and D02 were originally generated with the earlier road simulator. The November study reads the archived inputs and verifies their hashes before fitting. The earlier shared-innovation sampler, example, and run instructions are available together at the revision immediately before cleanup. [Input checksums](preparation/2026-11-13/data-verification.json) · [Original data generator](https://github.com/sj156/bdynets/blob/079e42ec58efe6c62cfab5a8b2d90cf5cb2bf58e/graphMoDE/R/api.R) · [Earlier code and instructions](https://github.com/sj156/bdynets/tree/079e42ec58efe6c62cfab5a8b2d90cf5cb2bf58e/graphMoDE#earlier-examples-and-custom-inputs)
