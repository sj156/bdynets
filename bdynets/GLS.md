# GLS: portable fitting and benchmark workflow

GLS is a method module of the `bdynets` package. This is the handoff guide for
running its current fitting and benchmark workflow on another computer. It tells a human or another ChatGPT which code is canonical, how to
fit one count-network data set, how Simulation 1 is run, where every benchmark
comes from, and which parameters were used in the reported experiment.

The two proposed outputs are named as follows.

- **GLS**: the continuous all-ordered-pairs posterior fit.
- **GLSS**: the sparse joint DSS summary selected from the fitted GLS posterior
  by validation predictive fidelity. GLSS is not a second MCMC fit.

Historical tables may contain `BDCN full`, `Revised BDCN continuous`, or
`GLS full` for GLS, and `BDCN + DSS`, `Revised BDCN + joint DSS`, or
`GLS + DSS` for GLSS. Current post-processing scripts translate these old
labels; do not change the numerical results by hand.

## 1. Canonical code and handoff rules

The current repository separates package implementation and paper launchers:

```text
bdynets/
  bdynets/                          installable package, GLS module and Simulation 1 DGP
  GLS-paper/                        study and prepared-data application launchers

simulation1_GLS_directed_grid/      optional legacy external benchmark project
  run_simulation.R
  benchmarks/
  postprocess_*.R
```

Use [GLS-paper/README.md](../GLS-paper/README.md) for the launchers included in
this repository. The legacy external project and its benchmark/post-processing
scripts were not present in the supplied checkout. Commands below naming that
project document its original handoff and require those external sources.
When they are available, use explicit paths:

```sh
BDYNETS=/path/to/bdynets
SIMU1=/path/to/simulation1_GLS_directed_grid

R CMD INSTALL "$BDYNETS/bdynets"
cd "$SIMU1"
Rscript install_R_dependencies.R
```

Important rules for another ChatGPT or analyst:

1. Use exported `bdynets::` functions. Do not copy sampler functions into a new
   analysis script and do not `source()` individual files from `bdynets/R`.
2. Record `packageVersion("bdynets")`, the Git commit, and the complete control
   object for every scientific run.
3. Run a smoke check before a long chain. Smoke and quick profiles are only
   integration checks and are not reportable scientific results.
4. Keep chronological training, validation and test blocks. Validation may be
   used for DSS, early stopping or benchmark tuning; the test block is evaluated
   once and must never select a method or hyperparameter.
5. Run all benchmarks on `benchmark_data/` exported by the GLS run. Do not
   regenerate the DGP inside a benchmark.
6. Never overwrite an existing result directory unless explicitly intended.
   Give each configuration a new output name.
7. Inspect parameter-level split-`Rhat`, ESS and trace plots before interpreting
   coefficient recovery. Good prediction alone is not convergence evidence.

The package is side-effect free when loaded. Files are written only by the
study writer/orchestrator or by scripts in the simulation project.

## 2. Installation and verification

The current GLS implementation was migrated from the standalone 0.5.1 source
into bdynets 0.2.0, including its compiled C reference filter and current study
code. Source installs require Rcpp and a C++17 compiler: matching Rtools on
Windows or the usual R C/C++ build tools on Linux/macOS. No GPU or system BLAS
change is required. Current bdynets traffic and networking dependencies are
retained.

For `gls_fit()` and `gls_fit_general()`, `gls_control(backend = "cpp")` selects
compiled scalar FFBS by default; `backend = "R"` selects its R reference. This
is separate from `reference_backend`, which continues to select the existing
C Newton (`"native"`) or R reference filter. To request both R references, use
`gls_control(backend = "R", reference_backend = "R")`.
The study sampler's FFBS and reference recursion are unchanged. The reusable
`poisson_discount_filter()` exposes a separate generic Brent-based Laplace
filter and does not replace the GLS C reference implementation. See
[CPP-KERNELS.md](CPP-KERNELS.md) for independent R/C++ reuse.

The integrated package passes `R CMD check` with Status: OK. Timings from older
scratch prototypes are not speedup measurements against GLS 0.5.1.

The fast implementation retains the model, priors, full candidate dictionary,
block size, iteration counts and MH covariance correction. Gaussian target
proposals share one design per sweep, form symmetric weighted Gram matrices,
and reuse one Cholesky factor for both the conditional mean and innovation;
they do not form a matrix inverse. General-data reference filtering uses a
bracketed, safeguarded scalar solver in C with a tighter root tolerance than
the retained R implementation. `gls_control(reference_backend = "R")` selects
the original R reference solver for numerical comparisons. Simulation 1 keeps
its own reference update and numerical-limit settings, while sharing the
optimized Gaussian proposal and batched network products.

General and study predictions batch deterministic network products per draw.
Forecasts still precede assimilation of the current counts. Caching does not
cache a data-dependent posterior across draws or reuse target-specific weights
or prior precisions. Different BLAS reduction orders and tighter scalar roots
can change floating-point results and eventually individual MCMC paths; they
do not imply an identical bit-for-bit chain. Compare conditional calculations,
convergence, ESS per second and predictive uncertainty, rather than acceptance
alone. The finite-shape count approximation and one-sweep online assimilation
are unchanged.

The package requires R 4.1 or newer. BayesLogit, Rcpp and parallel are required
alongside the existing bdynets imports. ggplot2 remains a package import;
external study figures may additionally use patchwork.

```sh
R CMD INSTALL /path/to/bdynets/bdynets
Rscript /path/to/simulation1_GLS_directed_grid/install_R_dependencies.R
```

Verify the installed copy rather than assuming R is loading the edited source
directory:

```sh
Rscript -e 'library(bdynets); packageVersion("bdynets"); find.package("bdynets")'
Rscript -e 'library(bdynets); print(gls_study_control("full"))'
```

Python is needed only for STGCN and Graph WaveNet:

```sh
cd /path/to/simulation1_GLS_directed_grid
python3 -m venv .venv
source .venv/bin/activate
python -m pip install --upgrade pip
python -m pip install -r requirements.txt
```

On Windows, activate with `.venv\Scripts\activate`. Current requirements are
`numpy>=1.24`, `pandas>=2.0`, and `torch>=2.1`.

## 3. Fit one observed count-network data set

For a new data set use `gls_fit_general()`. It fits all ordered source-target
pairs, including self, physically adjacent, multi-hop and unreachable pairs.
The older `gls_fit()` interface excludes the separately modeled local domain
and is retained mainly for older examples; it is not the recommended interface
for the current all-pairs formulation.

Since version 0.5.0, the general interface reuses Simulation 1's conditional
Gaussian pCN target proposal. It accepts/rejects small target blocks separately,
retains the full reference-covariance density correction, and adapts the pCN
step during burn-in only. The general-data covariates and fixed graph-affinity
prior are retained; this does not substitute Simulation 1's learned dictionary
or regenerate simulation covariates.

The main controls are `static_target_block_size=4L`,
`static_pcn_initial_step=0.8`, `static_pcn_target_acceptance=0.25`, and the
corresponding `static_pcn_adapt_rate`, `static_pcn_min_step`, and
`static_pcn_max_step`. Use `fit$coefficient_trace` to inspect per-iteration
accepted/proposed target blocks and the applied/next step size. Step adaptation
stops at `burn_mcmc`. `startup_*` controls stop chains with no meaningful
coefficient movement or unstable factors.

Default starting values use training means, nonzero loadings, and the
reference-filter state means. `initial_loading_sd=0.01` and `initial_tau=0.05`
are starting-value controls, not changes to the prior. An explicit
`initialization="full_glm"` uses unrestricted-GLM baseline/static starts, which
can be unstable on sparse, high-dimensional OD data. All-zero training series
are supported using a positive prior-center floor of `0.5/length(train_rows)`;
their counts remain zero. An explicit `means` argument overrides the default.

Each sweep still updates every target and keeps the complete source-target
candidate domain. Target blocking is a sampler choice, not an edge-screening
rule. Existing result directories must not be overwritten when changing this
sampler configuration.

### 3.1 Required inputs

- `Y`: a `T x K` matrix of finite nonnegative integer counts. Columns are
  directed edges/series and rows are consecutive times.
- `edges`: a `K`-row data frame containing at least `id`, `name`, `from`, and
  `to`. Its row order must exactly match the columns of `Y`.
- `A`: a `K x K` directed physical adjacency matrix. The convention is
  `A[target, source]`; `A[a,b]=1` means source edge `b` can feed target edge
  `a` in one physical step.
- `W`: the row-normalized version of `A`, with zero rows left at zero.
- `Q`: a finite `T x p` matrix of known time covariates. Do not add an
  intercept: GLS estimates a static edge-specific baseline `alpha_a`. For a
  period `P` and Fourier order two, use `cos(2*pi*t/P)`, `sin(2*pi*t/P)`,
  `cos(4*pi*t/P)`, and `sin(4*pi*t/P)`.
- Consecutive training, validation and test row indices. A standard split is
  `2/3`, `1/6`, `1/6`.

The package transforms lagged counts with `log1p()`, then centers and scales
them using the training block only. The frozen transformation is reused for
DSS and sequential prediction.

### 3.2 Complete custom-data example

```r
library(bdynets)

# Rows are time; count columns must be in the same order as edges$name.
count_table <- read.csv("counts.csv", check.names = FALSE)
Y <- as.matrix(count_table[, -1, drop = FALSE])
storage.mode(Y) <- "double"

edges <- read.csv("directed_edges.csv", check.names = FALSE)
A <- as.matrix(read.csv("physical_A.csv", check.names = FALSE))
storage.mode(A) <- "double"
W <- A
rs <- rowSums(W)
W[rs > 0, ] <- W[rs > 0, , drop = FALSE] / rs[rs > 0]

stopifnot(
  ncol(Y) == nrow(edges),
  identical(dim(A), c(ncol(Y), ncol(Y))),
  identical(as.character(edges$name), colnames(Y))
)

graph <- structure(
  list(edges = edges, A = A, W = W),
  class = c("gls_graph", "list")
)

T <- nrow(Y)
t <- seq_len(T)
period <- 300
Q <- cbind(
  cos1 = cos(2 * pi * t / period),
  sin1 = sin(2 * pi * t / period),
  cos2 = cos(4 * pi * t / period),
  sin2 = sin(4 * pi * t / period)
)

n_train <- floor(2 * T / 3)
n_validation <- floor(T / 6)
train <- seq_len(n_train)
validation <- n_train + seq_len(n_validation)
test <- seq.int(n_train + n_validation + 1L, T)

control <- gls_control(
  "default",
  n_iter = 5000L,
  burn_mcmc = 4000L,
  n_chains = 3L,
  parallel_chains = TRUE,
  predictive_draws = 600L,
  discount_grid = c(0.90, 0.95, 0.98, 0.995),
  discount_prior = rep(0.25, 4L),
  delta_A = 0.01,
  pip_cutoff = 0.50,
  dss_weight = "intensity",
  dss_n_lambda = 100L,
  dss_validation_draws = 150L,
  dss_max_draws = 600L,
  dss_predictive_tolerance = 0.01
)

pairs <- gls_all_pairs(graph, control)
fit <- gls_fit_general(
  Y = Y,
  graph = graph,
  covariates = Q,
  train_rows = train,
  pairs = pairs,
  label = "GLS",
  control = control
)

diagnostics <- gls_diagnostics(fit)
dss <- gls_dss(fit, validation_rows = validation, control = control)

prediction_gls <- predict(
  fit,
  rows = test,
  conditioning_rows = validation,
  label = "GLS",
  control = control
)

prediction_glss <- predict(
  fit,
  rows = test,
  conditioning_rows = validation,
  d = dss$d,
  label = "GLSS",
  control = control
)

score_gls <- gls_score(prediction_gls, Y, signal = 0, control = control)
score_glss <- gls_score(prediction_glss, Y, signal = 0, control = control)

print(fit)
print(dss)
print(rbind(score_gls, score_glss))
print(subset(diagnostics, !Converged))

saveRDS(
  list(
    control = control,
    graph = graph,
    pairs = pairs,
    fit = fit,
    diagnostics = diagnostics,
    dss = dss,
    prediction = list(GLS = prediction_gls, GLSS = prediction_glss)
  ),
  "gls_analysis.rds"
)
```

For real data the latent DGP intensity is unknown. `gls_score()` therefore
uses counts for scoring; do not interpret its `RMSEIntensity` field as recovery
of an unobserved true intensity. Report Poisson deviance, count MAE, predictive
interval calibration, and application-specific validation metrics.

## 4. Simulation 1 through the installed package

The formal design uses a `3 x 3` node grid, 24 directed road segments, all
`24^2=576` ordered pairs, and signal levels `q=0,0.1,0.2`. The DGP has a
static edge baseline, two Fourier harmonics with period 300, and sparse self,
one-hop, two-hop and far/unreachable propagation. The identifiable network
trajectory is

```text
theta_ab,t = theta_bar_ab + L_ab beta_t.
```

The DGP is in `bdynets/R/gls-simulation.R`. Public orchestration functions are
`gls_study_control()`, `gls_study_design()`, `gls_simulate_grid()`,
`gls_fit_study()`, `gls_dss()`, and `gls_run_grid_study()`.

### 4.1 Smoke check

From `simulation1_GLS_directed_grid`:

```sh
Rscript run_simulation.R --smoke --output=output/smoke_validation
```

### 4.2 Current full run

```sh
Rscript run_simulation.R --full \
  --signals=0,0.1,0.2 \
  --output=output/full_current
```

Current `gls_study_control("full")` settings are:

| Component | Setting |
|---|---|
| data and split | `T=1800`; train 1:1200; validation 1201:1500; test 1501:1800 |
| graph | 24 directed segments; 576 ordered candidate pairs |
| signal grid | `0`, `0.1`, `0.2` |
| baseline/seasonality | static edge-specific `alpha`; Fourier order 2; period 300 |
| latent rank | 1 |
| covariance discount | grid `0.90,0.95,0.98,0.995`, uniform prior |
| MCMC | 3 parallel chains; 5000 iterations; burn-in 4000; thin 1 |
| retained draws | 1000 per chain, 3000 total before balanced subsampling |
| GLS prediction | 600 balanced posterior draws |
| shrinkage | learned affinity dictionary; `c_ell=0.25`; global half-Cauchy prior scale 1; `chi ~ Gamma(1,1)` |
| negative-binomial approximation | time-adaptive `r_nb`; quantile 0.99; min 25; max 5000; rounded to 25 |
| PIP diagnostic | amplitude threshold `delta_A=0.01`; cutoff `c_A=0.5` |
| GLSS path | intensity-weighted; 100 penalties; min ratio `1e-4`; max 600 posterior draws |
| GLSS validation | 150 draws; predictive tolerance `0.01`; every distinct support (`stride=1`) |
| convergence flags | split-`Rhat <= 1.01`; ESS `>=400` |

`delta_A` and `c_A` define the marginal PIP diagnostic. They do **not** make
the joint DSS path sparse. GLSS sparsity is chosen by validation predictive
fidelity.

### 4.3 Existing manuscript result versus current default

The saved manuscript directory
`output/full_mcmc5000_burn4000_all_benchmarks` was produced with the same
5000/4000 three-chain setup but an earlier DSS post-processing choice:

```text
dss_predictive_tolerance = 0.007
dss_path_eval_stride     = 4
```

The current package default is `0.01` and `stride=1`. These configurations
must not be described as the same GLSS result. To reproduce the older DSS
choice with current code, use a new output directory:

```r
library(bdynets)
control <- gls_study_control(
  "full",
  n_iter = 5000L,
  burn_mcmc = 4000L,
  n_chains = 3L,
  signal_grid = c(0, 0.1, 0.2),
  dss_predictive_tolerance = 0.007,
  dss_path_eval_stride = 4L
)
gls_run_grid_study(
  control,
  output_dir = "output/reproduce_manuscript_dss007_stride4"
)
```

Sensitivity presets exist but are never run implicitly:

```r
names(gls_study_sensitivity_presets())
control <- gls_study_control(
  "full",
  sensitivity = "graph_laplace_tau0001"
)
```

## 5. Method-neutral benchmark data

Every GLS study run exports:

```text
benchmark_data/
  counts_signal_*.csv
  intensity_signal_*.csv
  fourier_signal_*.csv
  directed_edges.csv
  physical_A.csv
  physical_W.csv
  chronological_split.csv
```

All benchmark scripts read these files. This guarantees the same realization,
edge ordering, covariates and chronological split. True intensity files are
used only to compute simulation metrics; they are never training inputs.

The supplied sa-PGSS and deep-learning runners are Simulation 1 adapters, not
general real-data APIs: they expect the CSV contract above and read the latent
intensity file only when calculating simulation RMSE. For observed data, keep
the same model, split and validation logic but remove the true-intensity input
and `RMSEIntensity`; do not manufacture a latent intensity file. Report metrics
against observed counts instead. Any reusable real-data adapter should be added
as a new script without changing the archived Simulation 1 runner.

## 6. Benchmark methods: sources and formal parameters

### 6.1 PNAR(1) and PNAR(2)

Implementation: `bdynets/R/gls-benchmark-pnar.R`.

Reference family: Poisson network autoregression of Armillotta, Fokianos and
coauthors. This is a local implementation for the directed-road experiment,
not copied author software.

For each target edge, a Poisson GLM is fit on rows 1:1200.

- PNAR(1): intercept, four Fourier covariates, one self lag, and one
  physical-network lag `W`.
- PNAR(2): all PNAR(1) terms plus second-lag self and second-lag `W` terms.
- `W` is the physical row-normalized directed matrix, with target rows and
  source columns.
- There is no `W+W^2` diagnostic and no edgewise unrestricted Poisson method.
- No hyperparameter is tuned. Test prediction is rolling one step ahead using
  observed lagged counts and known Fourier covariates.

PNAR runs automatically inside `gls_run_grid_study()`.

### 6.2 sa-PGSS

Implementation: `benchmarks/run_sa_pgss.R`.

Reference: Irie, Glynn and Aktekin (2022), state-augmented Poisson-gamma
state-space model, DOI `10.1214/21-AOAS1505`.

No public author implementation was located. The included code is an
independent paper-based reproduction, not author-released code. It uses an
edge-specific Gamma-Beta rate evolution, a stationary AR(1) process for the
logit discount state, and a bootstrap particle filter. It does not use graph
neighbors, other edge counts or Fourier covariates.

| Setting | Value |
|---|---|
| prior Gamma shape | 1 |
| initial mean window | first 24 observations |
| tuning particles | 300 |
| final particles | 2000 |
| discount-centre grid | `0.70,0.75,0.80,0.90,0.95,0.98,0.995` |
| AR coefficient grid | `0.50,0.80,0.95` |
| innovation SD grid | `0.02,0.05,0.15,0.30` |
| candidates | 84 per edge and signal |
| tuning criterion | edge-specific validation log predictive density on 1201:1500 |
| test protocol | rolling one-step prediction on 1501:1800; score before updating |
| seed | 20260912 plus deterministic signal/candidate offsets |

Exact edge-specific selections are saved in
`sa_pgss_benchmark/sa_pgss_selected_hyperparameters.csv`.

### 6.3 STGCN

External source snapshot:
[`VeritasYin/STGCN_IJCAI-18`](https://github.com/VeritasYin/STGCN_IJCAI-18),
commit `4ab75ab69af354d6a0ab86aefa082d389b187127`, BSD-2-Clause.

The snapshot is stored in `benchmarks/external/STGCN_IJCAI-18/`. Because it is
TensorFlow 1 code, the executable adapter in
`benchmarks/run_deep_benchmarks.py` is a PyTorch translation of the two
ST-Conv blocks, Chebyshev graph convolution, temporal GLU layers and output
head.

Formal settings:

- 12 lagged counts; input channels are one standardized count channel plus
  `cos1`, `sin1`, `cos2`, and `sin2`;
- symmetrized physical edge graph with self loops; Chebyshev order 3;
- two ST-Conv blocks, dropout 0.30, batch size 64;
- Adam, learning rate `1e-3`, weight decay `1e-4`;
- standardized L1 loss, gradient norm clipped at 5;
- maximum 180 epochs, patience 24, validation count MAE for early stopping;
- deterministic seed 20260911 plus the signal offset;
- point forecast only; the plotted band is a plug-in Poisson interval and does
  not include parameter uncertainty.

For the stored formal run, selected epochs for `q=0,0.1,0.2` were 22, 13 and
19; the model had 214,049 trainable parameters.

### 6.4 Graph WaveNet

External source snapshot:
[`nnzhan/Graph-WaveNet`](https://github.com/nnzhan/Graph-WaveNet), commit
`6b162e80c59a1d494809252eca055cff93dc66b1`, MIT.

The adapter imports vendored author `model.py`. Three legacy `Conv1d` calls
that receive two-dimensional kernels and four-dimensional tensors were changed
mechanically to `Conv2d`; topology and forward equations are otherwise
unchanged. Data loading and evaluation are in
`benchmarks/run_deep_benchmarks.py`.

Settings shared with STGCN are history 12, the same five input channels, batch
size 64, dropout 0.30, Adam `1e-3`, weight decay `1e-4`, L1 loss, maximum 180
epochs, patience 24, and validation-MAE early stopping. Model-specific settings
are:

- physical forward and reverse supports plus adaptive adjacency;
- 4 blocks and 2 layers per block;
- residual channels 32 and dilation channels 32;
- skip channels 256 and end channels 512;
- deterministic base seed 20260911 plus model and signal offsets;
- point forecast only, with the same plug-in Poisson plotting interval.

For the stored formal run, selected epochs for `q=0,0.1,0.2` were 25, 14 and
25; the model had 300,193 trainable parameters.

External provenance and compatibility changes are recorded in
`benchmarks/external/SOURCE_VERSIONS.md`. Keep each external license file when
copying the project.

## 7. Run the complete benchmark pipeline

From `simulation1_GLS_directed_grid`:

```sh
# Integration check only
./run_all.sh smoke

# Full run; use a fresh explicit directory
./run_all.sh full "$PWD/output/full_current_all_benchmarks"
```

The full pipeline runs:

1. GLS, GLSS, PNAR(1), and PNAR(2) through the installed `bdynets` package;
2. sa-PGSS from exported benchmark data;
3. STGCN and Graph WaveNet from the same exported data;
4. combined performance tables and prediction figures.

If GLS has already finished, do not rerun MCMC:

```sh
./run_benchmarks_only.sh full /absolute/path/to/existing/result
```

Equivalent individual commands, useful on Windows, are:

```sh
Rscript benchmarks/run_sa_pgss.R \
  --signals=0,0.10,0.20 \
  --result-dir=/absolute/path/to/result

python benchmarks/run_deep_benchmarks.py \
  --signals 0 0.10 0.20 \
  --result-dir /absolute/path/to/result \
  --device cpu

Rscript benchmarks/combine_benchmark_results.R \
  --result-dir=/absolute/path/to/result
```

## 8. Rebuild figures without refitting

Run these scripts in order because the seven-method plot reads the table made
by the first script:

```sh
Rscript postprocess_prediction_figures.R \
  --result-dir=/absolute/path/to/result --edge=8

Rscript postprocess_all_benchmark_predictions.R \
  --result-dir=/absolute/path/to/result \
  --benchmark-result-dir=/absolute/path/to/result \
  --edge=8

Rscript make_manuscript_validation_figures.R \
  --result-dir=/absolute/path/to/result \
  --benchmark-result-dir=/absolute/path/to/result \
  --paper-figure-dir=/absolute/path/to/paper/figure \
  --neighbor-edge=e8 \
  --neighbor-signals=0.1,0.2
```

These scripts reuse saved posterior draws and predictions. They do not refit
GLS, GLSS or the benchmarks. The neighbor plot uses source-to-target direction,
marks the target with a thick black outline, and distinguishes true positives,
false positives and false negatives.

## 9. Main package interfaces

| Function | Purpose |
|---|---|
| `gls_control()` | Configure a general/custom-data GLS analysis |
| `gls_grid()` | Construct the directed square-grid graph used in examples |
| `gls_all_pairs()` | Build the complete ordered source-target dictionary |
| `gls_fit_general()` | Fit GLS to arbitrary counts and known covariates |
| `gls_diagnostics()` | Parameter-level split-`Rhat`, ESS and convergence flags |
| `gls_dss()` | Select the joint validation-controlled sparse summary |
| `predict()` | Sequential one-step GLS or GLSS prediction |
| `gls_score()` | Predictive scoring |
| `gls_study_control()` | Configure Simulation 1 |
| `gls_study_design()` | Freeze graph, affinity features and active truth |
| `gls_simulate_grid()` | Generate one Simulation 1 scenario |
| `gls_fit_study()` | Fit the Simulation 1 GLS model |
| `gls_run_grid_scenario()` | Run GLS, GLSS and PNAR for one signal |
| `gls_run_grid_study()` | Run all signals and export benchmark data |
| `gls_write_grid_study()` | Write tables, figures and serialized results |
| `gls_plot_neighbor_recovery()` | Compare true and GLSS source neighborhoods |

## 10. Output checklist

Before handing a result to another computer or writing the paper, preserve:

- `configuration.R` or the serialized control object;
- one `scenario_results.rds` per signal;
- `tables/predictive_performance.csv`, `pair_selection.csv`, `DSS_path.csv`,
  `MCMC_convergence.csv`, and sampler-acceptance tables;
- `benchmark_data/`;
- benchmark manifests, selected hyperparameters and checkpoints;
- post-processing scripts and source-version files;
- the Git commit and any uncommitted patch used for the run.

Do not infer that a run is reproducible merely because the final CSV was
copied. The configuration, posterior objects, benchmark inputs, external
source commits and preprocessing code are all part of the computational
result.
