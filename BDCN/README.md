# BDCN: Bayesian Dynamic Count Networks

This directory is an installable R package for graph-shrinkage Bayesian
dynamic count network models. It provides the posterior sampler, convergence
diagnostics, joint decoupled shrinkage and selection (DSS), sequential one-step
prediction, a sparse multi-hop simulation, and a frozen Divvy bike example.

Loading `BDCN` does not install packages, create output directories, or launch
an experiment. Command-line parsing, plotting, and bulk output logic from the
original research scripts are kept outside the package namespace. The model
engines and reusable analysis interfaces are maintained directly under `R/`.

## Installation

For a local checkout, run this from the repository root:

```r
install.packages("pak") # only if needed
pak::pak("./BDCN", dependencies = TRUE)
library(BDCN)
```

Alternatively, use base R:

```sh
R CMD INSTALL BDCN
```

`BayesLogit` is a runtime dependency. `testthat` and `roxygen2` are used for
development and package validation.

See the [repository overview](../README.md) for the broader `bdynets` project.

## Sparse multi-hop simulation example

The packaged example constructs a directed road-segment grid, injects sparse
heterogeneous two-hop propagation, fits the continuous BDCN posterior, applies
validation-selected joint DSS, and evaluates sequential one-step predictions.

The complete convenience workflow is:

```r
library(BDCN)

result <- bdcn_run_multihop_example(
  profile = "smoke",
  signal = 0.20,
  side = 3
)
result$scores
```

The equivalent step-by-step workflow is:

```r
control <- bdcn_control("smoke")
design <- bdcn_simulate_multihop(signal = 0.20, side = 3, control = control)
sim <- design$simulation

fit <- bdcn_fit(
  sim$Y,
  design$graph,
  train_rows = design$split$train,
  pre_history = sim$pre_history,
  pairs = design$pairs,
  time_covariate = sim$time_covariate,
  season_covariate = sim$season_covariate,
  means = sim$means,
  lag_reference_scale = sim$scale,
  signal = design$signal,
  control = control
)

diagnostics <- bdcn_diagnostics(fit, design$long_pairs)
dss <- bdcn_dss(fit, design$split$validation)

continuous <- predict(
  fit,
  rows = design$split$test,
  conditioning_rows = design$split$validation,
  label = "BDCN continuous"
)
sparse <- predict(
  fit,
  rows = design$split$test,
  conditioning_rows = design$split$validation,
  d = dss$d,
  label = "BDCN joint DSS"
)

rbind(
  bdcn_score(continuous, sim$Y, sim$lambda, design$signal, control),
  bdcn_score(sparse, sim$Y, sim$lambda, design$signal, control)
)
```

The `smoke` and `quick` profiles are workflow checks, not inferential settings.
The `default` profile uses three chains with 6,000 iterations per chain and
discards the first 3,000. Iteration count alone does not establish convergence;
inspect the split-Rhat and ESS output from `bdcn_diagnostics()`.

The runnable script is also available at
[`inst/examples/multihop.R`](inst/examples/multihop.R).

## Divvy bike example

The package includes the frozen aggregate Divvy data and the general all-pairs
BDCN engine needed for the bike analysis. The data contain 1,448 three-hour
observations on 24 directed OD links, 13 known calendar covariates, and 576
ordered source-target candidates. January--April are used for training, May for
validation, and June for testing; the first 12 bins provide lag history.

Load and inspect the bundled design:

```r
library(BDCN)

bike <- bdcn_bike_data()
dim(bike$Y)          # 1448 x 24
dim(bike$covariates) # 1448 x 13
bike$split_table
```

Run the complete short-chain workflow check:

```r
smoke <- bdcn_run_bike_example("smoke")
smoke$scores
```

Run the original 1,500-iteration, three-chain analysis and save its outputs:

```r
result <- bdcn_run_bike_example(
  profile = "analysis",
  output_dir = "bike_output_1500"
)
summary(result$fit)
result$scores
```

Run the 5,000-iteration sensitivity analysis:

```r
result <- bdcn_run_bike_example(
  profile = "long",
  output_dir = "bike_output_5000"
)
```

The same analysis can be launched from a terminal:

```sh
Rscript -e 'library(BDCN); bdcn_run_bike_example("analysis", output_dir="bike_output_1500")'
```

When `output_dir` is supplied, the workflow writes the fit checkpoint, MCMC
diagnostics, DSS validation path, dependence-selection table, June rolling
predictions, scores, and frozen configuration. A subsequent run reuses the fit
only when the control settings, counts, covariates, and graph are identical.

The runnable script is available at [`inst/examples/bike.R`](inst/examples/bike.R),
and the bundled data contract is documented in
[`inst/extdata/divvy/README.md`](inst/extdata/divvy/README.md).

## Main interfaces

| Function | Responsibility |
| --- | --- |
| `bdcn_control()` | Simulation, MCMC, shrinkage, DSS, and prediction settings |
| `bdcn_grid()` | Directed road-segment grid used by the simulation example |
| `bdcn_pairs()` | Nonlocal ordered-pair dictionary with graph affinities |
| `bdcn_all_pairs()` | Complete self and cross-series ordered-pair dictionary |
| `bdcn_simulate_multihop()` | Sparse heterogeneous two-hop data generator |
| `bdcn_fit()` | BDCN with static self/W terms and a nonlocal dynamic domain |
| `bdcn_fit_general()` | All-pairs BDCN with an arbitrary known covariate matrix |
| `bdcn_diagnostics()` | Parameter-level split-Rhat, ESS, intervals, and pass/fail checks |
| `bdcn_dss()` | Validation-selected joint DSS projection of a continuous fit |
| `predict()` | Sequential one-step prediction with observation assimilation |
| `bdcn_score()` | Poisson deviance, intensity RMSE, log score, and interval metrics |
| `bdcn_bike_data()` | Loader for the frozen 24-edge Divvy design |
| `bdcn_bike_control()` | Smoke, 1,500-iteration, and 5,000-iteration bike settings |
| `bdcn_run_bike_example()` | End-to-end Divvy fitting, DSS, and forecasting workflow |

See the generated help pages, especially `?bdcn_fit`, `?bdcn_fit_general`,
`?bdcn_dss`, and `?bdcn_run_bike_example`, for complete argument contracts.

## Scripts

| R script | Responsibility |
| --- | --- |
| `BDCN-package.R` | Package documentation and imports |
| `api.R` | Multi-hop API, validation, diagnostics, DSS dispatch, and S3 methods |
| `engine-internal.R` | Internal nonlocal BDCN sampler and simulation engine |
| `general-api.R` | All-pairs API, arbitrary covariates, and Divvy workflow |
| `general-engine-internal.R` | Internal all-pairs sampler, DSS, and prediction engine |

All maintained package functions live under `R/`. Roxygen generates
`NAMESPACE` and the help pages under `man/`. End-to-end scripts are stored under
`inst/examples/`, bundled aggregate data under `inst/extdata/divvy/`, and
module-specific tests under `tests/testthat/`.

## Model contract and current scope

The current release implements a rank-one graph-shrinkage BDCN. Dynamic
source-target coefficients are products of pair-specific loadings and a common
unit-innovation latent factor. The Poisson likelihood is handled through the
established finite-shape negative-binomial approximation and Pólya--Gamma
augmentation. The time-specific approximation sequence is fixed from training
data before MCMC.

The multi-hop interface estimates static own-lag and physical-network effects
separately from the nonlocal dynamic dictionary. This prevents a local effect
from competing in both static and dynamic channels. The general interface used
by the bike example instead places all 24 x 24 ordered pairs, including self
pairs, in the graph-informed dynamic shrinkage model and gives each target its
own coefficients for the known calendar design.

Joint DSS does not refit the generative model. It constructs a sparse path from
the continuous posterior and selects a common network sparsity level using
one-step Poisson deviance on validation data. The test period is not used for
DSS selection.

Prediction is sequential and one-step ahead: the model predicts time `t`, then
assimilates `Y[t, ]` before predicting `t + 1`. It is not an unconditional
long-horizon forecast. Missing histories, new network units, higher-rank factor
identification, and automatic calendar selection are not currently part of the
public package API.

Smoke and quick profiles validate software execution only. They do not establish
chain convergence, posterior calibration, support recovery, or scientific
performance.

## Development and validation

From the repository root:

```r
roxygen2::roxygenise("BDCN")
testthat::test_local("BDCN")
```

```sh
R CMD build BDCN
R CMD check --no-manual BDCN_0.1.0.tar.gz
```

The current package passes its unit and end-to-end smoke tests and
`R CMD check --no-manual` with **Status: OK**. These checks cover package
structure, input contracts, deterministic simulation, posterior execution,
joint DSS, and sequential prediction for both the multi-hop and Divvy workflows.
They do not replace substantive convergence assessment.

`R/engine-internal.R` was migrated from
`TrafficFlowNTS_section6_grid_long_range.R`.
`R/general-engine-internal.R` was migrated from
`section6_general_extension/run_general_bdcn_extension.R`. Future algorithmic
changes should update the corresponding internal engine, public contracts,
tests, and reproducibility notes together.
