Update after user approval: the baseline is Beta(49,1), c = log(2), and the explicitly design-informed m0 = (log(25),0,0). The discussion and pending labels below document the earlier preparation stage. The subsequent run protocol is in `../../studies/section41-discount-20261113/plan.json`. No manuscript setting is changed.

# Section 4.1 simulation preparation

Status: data and deterministic kernel preparation, not a runnable new MCMC study.
The user selected the current Section 4.1 design and the revised Sections 2-3
and appendix as the implementation specification. Unknown scientific constants
remain unset. This work neither changes the manuscript nor starts a simulation.

## Preserved design

Reuse the saved D01 and D02 observations and geometry. There are 121 sites,
120 direct edges, five generating regions of sizes 30/20/21/20/30, and boundary
gap factors 3/4/3/2. The sinusoidal Poisson profiles have amplitude 0.05, period
24 and exact cycle mean 25. Training is times 1-168 and testing is 169-216.
The fitted capacity is **K = 10**, with **p = 3** and identity state evolution.
No nearest-neighbour pruning or new data generation is introduced.

`data-verification.json` records the existing input-file hashes, row alignment,
physical support, arc lengths, seasonal design and truth/profile checks. Truth
is used only in this data verification, never as a fitting input. Old saved
`fits` and `starts` inside the archive are deliberately excluded from the new
specification. No chain or checkpoint is resumed.

The input audit is reproducible without loading a sampler:

```sh
Rscript graphMoDE/scripts/check-section41-inputs.R ARCHIVE_ROOT NEW_REPORT.json
```

`ARCHIVE_ROOT` is the original `core-pilot-20261015` input archive. It is an
external local directory, not part of this repository. Dataset filenames and
hashes identify the exact source independently of the local absolute path.

## Source and implementation

The upstream base is `sj156/bdynets` at
`3f2df3907cd3898f0d657ace862a34ea98e31f9d`. The current manuscript input hashes
are in `configuration.json`. Older local September/October status notes do not
override the user's November 13 model and development decisions.

Core functions belong in `bdynets/R` and `bdynets/src`; study scripts and
reports belong in `graphMoDE`. New repository content is English. Existing
collaborator files and historical scientific results are preserved.

Added package functions:

- `gmde_discount_guide()`: C++ implementation of the multivariate Poisson
  Gamma log-moment guide, with undiscounted information proxies, effective
  discounts, time-varying innovation covariances and linear-Bayes feedback.
- `gmde_transition_log_density()`: C++ evaluation of the transition term D_k,
  including determinants and quadratic forms, with the time-zero state included
  in the path input. The initial-state prior and fixed normalizing constant
  are intentionally excluded from D_k.

The inverse-trigamma calculation reuses the package's safeguarded scalar root
solver. Wrappers reuse the existing gmde input validators. No Laplace guide is
substituted for Gamma log-moment matching. No numerical covariance repair,
clipping or implicit scientific default is introduced.

`kernel-verification.json` records compilation of the complete native library,
preservation of all existing registered native entries, eight focused fixed-input
test groups with 26 assertions, independent R comparisons and an existing
scalar-reference smoke check. Tests cover p=1/p=3, nonidentity transitions,
zero counts, empty classes, causal replay, transition densities, invalid inputs
and RNG preservation. No random draws, MCMC or package installation occurred.
These checks establish local numerical behavior, not sampler convergence or
measured end-to-end speed.

The complete new sampler remains to be implemented. In particular, the shared
C++ FFBS supports one latent scalar, not the correlated p=3 state. The packaged
`gmde_fit()` remains a static Gaussian reference implementation. See
`implementation-map.csv` for reuse, adaptation and missing components. Future
implementation needs multivariate FFBS, corrected NB-PG proposals, baseline
and allocation MH with guide replay, independent class-field ESS, centered raw
graph-parameter updates and valid forecasting. The new primitives do not yet
provide those complete transitions.

## Prior and tuning assessment

The numeric proposals below are candidates, not selected defaults. The
configuration keeps unresolved values as `null` and sets `launch_enabled=false`.

### Baseline discount prior

A common proper Beta family on (0,1) is a transparent starting point. The
continuous full support is compatible with the stated logit MH update and
avoids reviving the retired hidden-state/grid construction. Candidate summaries:

| Prior | Mean | Median | Central 95% interval | Baseline covariance inflation at median |
|---|---:|---:|---|---:|
| Beta(19,1) | 0.95 | 0.96418 | [0.82353, 0.99867] | 3.72% |
| Beta(49,1) | 0.98 | 0.98595 | [0.92748, 0.99948] | 1.42% |
| Beta(99,1) | 0.99 | 0.99302 | [0.96342, 0.99974] | 0.70% |

Inflation is `(1/d)-1`, before the additional Poisson protection. These are
not state-memory estimates, effective sample sizes or observed mixing results.
Beta(49,1) and Beta(99,1) provide useful moderate/stronger smoothing candidates
for hourly data. The static seasonal truth motivates examining smoothing, but
does not justify selecting a prior by achieved ARI or test prediction error.
No choice between these priors has been made. Quantiles are in
`discount-prior-candidates.csv` and are exact deterministic Beta calculations.

### Poisson adjustment c

Use the interpretable protection fraction `exp(-c*iota)`. If protection halves
at `iota_star`, then `c=log(2)/iota_star`:

| iota_star | c | Protection at initial predictor variance 1.5 |
|---:|---:|---:|
| 0.5 | 1.38629 | 0.22825 |
| 1 | 0.69315 | 0.47776 |
| 2 | 0.34657 | 0.69120 |
| 5 | 0.13863 | 0.86266 |

Under the retained initial covariance and seasonal design, the initial
information proxy is approximately 1.065656. Thus `iota_star=1` is an
interpretable candidate for review, not an automatically calibrated constant.
Larger c means less protection and effective discounts closer to the baseline;
smaller c means more protection and smaller innovations. Since d and c both
affect smoothing, assess them jointly and keep c fixed during a registered fit.
Do not learn c without revising the agreed model. These summaries use no
observed outcomes, labels or simulated trajectories.

### Initial state

Keep the current covariance `diag(1,0.5,0.5)` as recorded. Its initial log-rate
variance is 1.5; the already verified central 95% intensity interval is about
0.091 to 11.03 times its prior median. This is broad and deserves prior-scale
review, but it has not been narrowed to match the known small DGP amplitude.

Specify `m0=(log(lambda_ref),0,0)` from declared initial information. The old
whole-training-sample mean is not automatically compatible with the current
pre-observation H0 convention. A reference level 25 would be a **design-informed
prior**, and must be explicitly agreed and disclosed if used; it must not be
presented as externally learned information. The reference level remains unset.

### Graph priors and fixed geometry calibration

Retain nu=3, tau=1, full rank, raw Exp/SE weights, Gamma(0.5, rate=0.5) for h
in the unchanged synthetic arc-length unit, and log-range standard deviation 1.
Recompute u0 from the current raw reference graph; never inherit the historical
nu=1, mean-one/trace-normalized calibration. No rescaling of saved distances is
performed. Both W families use the Exp reference at h=1 and the same u0.

| Dataset | W reference kappa0 | W u0 | Connectivity kappa0 | Connectivity u0 |
|---|---:|---:|---:|---:|
| D01 | 1.479200 | 0.391501 | 1.282868 | 0.249099 |
| D02 | 1.478037 | 0.390715 | 1.282868 | 0.249099 |

Each achieves median direct-edge reference correlation 0.5, using geometry
only. See `geometry-calibration.csv`. The centres remain fixed after h moves.
These calculations evaluate the manuscript's rule; they are not MCMC fits.
The arithmetic and geometry tables can be regenerated with the following
study script. It compiles the deterministic package primitive into a temporary
cache, performs no package installation, and requires a new output directory:

```sh
Rscript graphMoDE/scripts/evaluate-section41-priors.R ARCHIVE_ROOT REPOSITORY_ROOT NEW_OUTPUT
```

Correlation calibration alone does not calibrate softmax probabilities: median
score standard deviations at these reference centres are approximately 0.141
for W and 0.091 for C. The unnormalized covariance couples score variance to
range. Assess induced probabilities before interpreting the prior as equally
informative across graph methods; nu, tau and the stated prior families are
not changed here.

### Computational tuning

- Keep the recorded log-kappa and log-h proposal scales 0.25 and 0.15.
- The baseline logit proposal scale remains unset. A later declared tuning
  comparison can consider 0.2/0.4/0.6, rather than selecting a scale from a
  desired acceptance-rate claim alone.
- For the reused exact integer-shape Devroye PG backend, an integer rho grid
  such as 1/2/4/8 is compatible with `r=rho*max(S,1)`. These are prospective
  candidates, not a pilot authorization or a selected rho. Account for the PG
  cost and corrected trajectory movement per unit time. Historical rho=4 does
  not establish efficiency for the new target.
- Declare numerical handling of very small raw SE weights and covariance conditioning;
  the old mean-one-weight numerical policy has not been validated for this target.
- Choose full-path versus conditional-block state proposals explicitly before
  a run. A historical 42-time block is not silently inherited.
- The old four-chain 4000/2000 schedule is a historical budget reference, not
  proof of an adequate new run length. New schedules, seeds, diagnostics and a
  fresh external destination must be registered before execution.

No Gaussian observation-variance or static-coefficient hyperparameters are
needed for this dynamic Poisson core preparation. Do not add those unresolved
inputs to the current fitting scope merely because the manuscript discusses
other response families.
