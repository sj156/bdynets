# Fresh eight-method comparison under discount evolution

The user explicitly requested new results for all eight methods on the saved
D01/D02 data, using the current model. This decision supersedes the older
preparation-only flags for this specifically registered batch. No old fit or
checkpoint is resumed. The manuscript and historical tables remain unchanged.

All six Bayesian methods share K=10, the current Poisson discount expert,
independent Beta(49,1) baselines, c=log(2), m0=(log(25),0,0),
C0=diag(1,.5,.5), rho=1, temporal blocks of 24 and the existing MH corrections.
Each final fit uses four fresh chains of 4000 sweeps with 2000 warmup sweeps.
Initial occupancies are 1,3,7,10; common initialization seeds pair corresponding
methods without exposing the true labels. Main-chain and forecast RNG streams
are separate. No early stopping based on mixing and no automatic extension.

Exp-W, SE-W and C preserve the current independent softmax GP specifications.
EucMoDE uses the dense Euclidean Matern covariance, nu=3, tau=1, without trace
normalization; its log-range prior is centered using median correlation .5 on
symmetrized four-nearest-neighbor pairs, using geometry only. MoDE uses the
symmetric Dirichlet(.1,...,.1) prior. PottsMoDE uses fixed raw Exp weights at
h=1; beta is selected from .25,1,4 using origins 96/120 and two 600/300 chains.
Both allocation comparators retain the guide-dependent MH correction. The
fixed Exp graph is also used for STGNN and PNAR (which row-normalizes it).

STGNN and PNAR are freshly tuned/refitted using pinned author sources, with the
same candidate grids and training prefixes as Section 4. STGNN receives a
1800-second per-fit operational ceiling; this does not add epochs or candidates.
No third-party source or environment is vendored or installed. Author-source
pins and current runtime paths are recorded in the external registration.

Every retained Bayesian draw saves labels, baseline discounts, terminal states
and guide moments. A C++ predictor simulates four independent 48-step histories
per draw, updates the guide with simulated aggregate counts and averages the
conditional one-step Poisson means. It never accepts held-out responses.
Classification uses the Dahl representative over the four final chains;
forecast RMSE uses held-out counts. Prior finite-run ARI means and training
intensity RMSE are not substituted for these metrics.

## Validation and lineage

The extension reuses the existing gmde dynamic sampler, discount guide,
multivariate FFBS, integer PG backend and bounded numerical roundoff handling.
PNAR wrapper functions migrate from graphMoDE/R/core_35_benchmarks.R into
bdynets/R/gmde-pnar-benchmark.R. The existing STGNN adapter is reused in
bdynets/inst/python/gmde_stgnn.py; the external author model stays external.

The guide and dynamic fixed-input suites pass 26 and 32 assertions. The new
suite passes 24 checks covering Euclidean closed forms, Gaussian density,
Potts prior/proposal cancellation and predictive recursion against independent
guide replay. Six registered integration tasks precede the fixed full batch;
these are execution checks and are not used to select tuning or infer mixing.

Fresh output, source commit/hashes, build identity, input hashes, task settings,
shared initialization seeds, fitting seeds and predictive seeds are recorded
before any task starts. Every failed task is retained; unavailable methods are
reported explicitly. No diagnostic thresholds are relaxed.

## Completed results

[Comparison table and interpretation](results/README.md). All 168 tasks finished on November 14, 2026 (Asia/Shanghai).

The public launcher accepts GMDE_CORE_REGISTRATION, GMDE_COMPARATOR_REGISTRATION and GMDE_SEED_AUDIT_ROOTS for the corresponding prior registration, external benchmark paths and path-separated seed audit roots. The run manifest records the executed and portable launcher hashes.
