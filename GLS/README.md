# GLS: global-local shrinkage for dynamic count networks

`GLS` is the canonical R implementation of the global-local shrinkage model
used in the directed-network count study. The package contains the reusable
graph, fitting, posterior-diagnostic, DSS, visualization, and
sequential-prediction code. The simulation1 directed-grid data-generating
process is kept in `R/simulation.R`.

The package does not start analyses or write files when loaded. Study output is
written only when `gls_write_grid_study()` is called, or when an `output_dir` is
passed to `gls_run_grid_study()`.

## Method names

- **GLS** is the continuous all-pairs posterior model (formerly “GLS full”).
- **GLSS** is the sparse predictor obtained by applying the joint DSS rule to
  the GLS posterior (formerly “GLS DSS” or “GLS + DSS”).

DSS remains the name of the selection procedure; GLSS is the resulting method.
The implemented selector evaluates the DSS path on validation data and chooses
the smallest support whose Poisson deviance is within the configured relative
tolerance of GLS. The default tolerance is `0.01`.

## Installation

From the repository root:

```r
install.packages("remotes")
remotes::install_local("GLS", dependencies = TRUE, upgrade = "never")
library(GLS)
```

For a machine with the repository already checked out, base R also works:

```sh
R CMD INSTALL GLS
```

## Simulation1: directed-grid propagation

The full design uses a 3 by 3 node grid, 24 directed road segments, and all
24 squared ordered segment pairs. It injects sparse self, one-hop, two-hop, and
far/unreachable effects. A static edge baseline and Fourier seasonality are
shared by the DGP and fitted model. The network coefficient is the identifiable
total trajectory

```text
theta_ab,t = theta_bar_ab + L_ab beta_t.
```

The affinity scale is learned from pair features, the covariance discount for
`beta_t` is sampled on a discrete grid, and DSS selects one common sparse
support using validation prediction. The resulting two proposed methods are
reported as GLS and GLSS.

For the Appendix A.2 Graph-Laplace sensitivity specification, use one option:

```r
control <- gls_study_control(
  "full",
  shrinkage_spec = "graph_laplace_fixed_tau",
  fixed_tau = 0.001,
  graph_laplace_rho0 = 0.90
)
```

This constructs the symmetric edge graph, sets
`Q_E = D_E + I - rho0 * A_E`, uses its standardized correlation score as the
single learned affinity feature, and fixes `tau` at the requested positive
value. The local half-Cauchy
scales and the positive Graph-Laplace feature coefficient remain learned. The
legacy `tau = 1`, `tau = 0.025`, and current `tau = 0.001` settings are
available as `sensitivity = "graph_laplace_tau1"`,
`sensitivity = "graph_laplace_tau0025"`, and
`sensitivity = "graph_laplace_tau0001"`, respectively.

### Complete R example

The following is a small end-to-end version of simulation1. It constructs the
same model and output objects as the formal experiment, but uses the smoke
profile so that the code can be checked quickly.

```r
library(GLS)

control <- gls_study_control("smoke")
design <- gls_study_design(control)

# Generate counts and latent intensities for q = 0.1.
simulation <- gls_simulate_grid(0.1, design, control)

# Fit the continuous all-pairs model (GLS).
fit <- gls_fit_study(simulation, design, control)

# Select a sparse support on validation data and form GLSS.
dss <- gls_dss(fit, fit$split$validation, control)
glss_prediction <- predict(
  fit,
  rows = fit$split$test,
  conditioning_rows = fit$split$validation,
  d = dss$d,
  label = "GLSS",
  control = control
)

sum(dss$selected)             # selected ordered pairs
gls_diagnostics(fit)          # R-hat and effective sample size
head(glss_prediction$mean)    # held-out predictive intensity
```

For the shortest reproducible workflow, the orchestration interface runs GLS,
GLSS, PNAR(1), and PNAR(2) together and writes all standard outputs:

```r
study <- gls_run_grid_study(
  gls_study_control("smoke"),
  output_dir = "output/simulation1_smoke"
)
study$scenarios[[1]]$performance
```

### Formal simulation1 run

This is the manuscript configuration: signals `0`, `0.1`, and `0.2`; three
chains; 5,000 iterations with the first 4,000 discarded. It saves tables,
standard diagnostic figures, scenario objects, and benchmark inputs.

```r
control <- gls_study_control(
  "full",
  n_iter = 5000L,
  burn_mcmc = 4000L,
  n_chains = 3L,
  signal_grid = c(0, 0.1, 0.2)
)

study <- gls_run_grid_study(
  control,
  output_dir = "output/simulation1_mcmc5000_burn4000"
)
```

The main files are:

```text
output/simulation1_mcmc5000_burn4000/
  configuration.R
  GLS_simulation_summary.rds
  tables/predictive_performance.csv
  tables/pair_selection.csv
  signal_0.00/scenario_results.rds
  signal_0.10/scenario_results.rds
  signal_0.20/scenario_results.rds
```

The package writer also creates per-signal convergence, shrinkage, DSS, and
prediction diagnostics. Manuscript composite figures are built by the
post-processing scripts in `simulation1_GLS_directed_grid`; these scripts read
saved posterior draws and do not rerun MCMC:

```sh
Rscript postprocess_prediction_figures.R \
  --result-dir=output/simulation1_mcmc5000_burn4000

Rscript postprocess_all_benchmark_predictions.R \
  --result-dir=output/simulation1_mcmc5000_burn4000 \
  --benchmark-result-dir=output/simulation1_mcmc5000_burn4000

Rscript make_manuscript_validation_figures.R \
  --result-dir=output/simulation1_mcmc5000_burn4000 \
  --benchmark-result-dir=output/simulation1_mcmc5000_burn4000 \
  --paper-figure-dir=/path/to/paper/figure
```

Sensitivity settings are available but never run automatically:

```r
names(gls_study_sensitivity_presets())
control <- gls_study_control("full", sensitivity = "tau_scale_2")
```

## Main interfaces

| Function | Purpose |
|---|---|
| `gls_grid()` | Build a directed square road-segment graph |
| `gls_all_pairs()` | Build the complete ordered-pair candidate domain |
| `gls_study_control()` | Configure the directed-grid study |
| `gls_study_design()` | Freeze graph, pair features, and active truth |
| `gls_simulate_grid()` | Generate one scenario from the study DGP |
| `gls_fit_study()` | Fit the GLS study model |
| `gls_diagnostics()` | Compute parameter-level R-hat and ESS |
| `gls_dss()` | Select the joint sparse network |
| `predict()` | Produce sequential one-step predictions |
| `gls_run_grid_scenario()` | Run GLS, GLSS, and PNAR for one signal |
| `gls_run_grid_study()` | Run every configured signal |
| `gls_write_grid_study()` | Save reproducible tables, figures, and objects |

The older `gls_fit()` and `gls_fit_general()` interfaces remain available for
custom data analyses. The package contains only reusable methods and
simulations, without an application-specific bundled dataset.

## Package layout

```text
GLS/
  R/
    api.R                       core public API
    general-api.R               arbitrary-covariate all-pairs API
    study-api.R                 directed-grid study orchestration
    simulation.R                current study DGP
    study-control.R             profiles and sensitivity presets
    study-sampler.R             learned-affinity MCMC engine
    study-dss.R                 joint DSS and study prediction
    study-diagnostics.R         convergence summaries and figures
    benchmark-pnar.R                PNAR benchmark and benchmark export
    study-utils.R               study-specific numerical utilities
  inst/examples/
    multihop.R                  legacy compact example
  tests/testthat/               package tests
```

`full` runs are intended for scientific reporting. Always inspect the
parameter-level convergence table and trace plots before interpreting
coefficient recovery or shrinkage scales.
