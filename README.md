# bdynets

An R package for Bayesian dynamic network analysis. The installable source is in
`bdynets/`. The paper workflows, data preparation examples and saved study
summaries live alongside it.

## Install

From the root of a downloaded checkout, run the following in R with R 4.1 or
later and a C++17 toolchain. The package compiles its numerical kernels during
installation.

```r
if (!requireNamespace("pak", quietly = TRUE)) install.packages("pak")
pak::pak("./bdynets")
library(bdynets)
```

To install the published GitHub version directly, use
`pak::pak("sj156/bdynets/bdynets")`. For GLS, see the
[installation and eight-link example](GLS-paper/README.md) or
[set up a downloaded checkout](GLS-paper/README.md#reproduce-the-paper).

## Run an analysis

| Task | Entry point | Guide |
| --- | --- | --- |
| Fit GLS to observed network counts | `gls_fit_general()`, `gls_dss()`, `predict()` | [Prepared-data example and input format](GLS-paper/REPLICATION.md#prepared-data-application) |
| Run Simulation 1 | `GLS-paper/run_simulation.R` generates the directed-grid data, fits GLS and runs the PNAR comparisons | [Study launcher](GLS-paper/REPLICATION.md#directed-grid-simulation) · [Model and benchmark settings](bdynets/GLS.md) |
| Fit graph-informed mixtures | `gmde_fit()` for static Gaussian panels, `gmde_dynamic_fit()` for dynamic counts | [Package example](bdynets/README.md#gaussianstatic-graphmode-example) · [Eight-method study and results](graphMoDE/README.md) |
| Draw prepared Flow, Speed and OD maps | `traffic_flow_map()`, `traffic_speed_map()`, `traffic_od_map()` | [Five examples](examples/README.md) · [Data sources](examples/DATA-SOURCES.md) · [Input format](bdynets/inst/INPUT-FORMAT.md) |
| Reproduce the Poisson latent-position study | `lpr_*` functions and study scripts | [GP-LPR runs and figure reproduction](GP-LPR/README.md) |
| Reuse scalar state-space kernels | `gaussian_ffbs()`, `poisson_discount_filter()`, installed C++ header | [R and C++ interfaces](bdynets/CPP-KERNELS.md) |

The [Divvy traffic example](examples/DivvyBike/README.md) prepares one day of
station trips. The projected Divvy GLS study uses six months of three-hour link
counts and its own archived DSS search. The [Divvy replication guide](GLS-paper/REPLICATION.md#divvy-experiment-archive)
identifies the inputs and scripts for that analysis.

Historical dynamic count mixtures and clustering utilities remain available
through `gibbs_sampler()`, `adj_rand_index()` and `best_label_accuracy()`.
See the [package guide](bdynets/README.md) and [migration record](MIGRATION.md).

## Develop and test

Package code lives in [R](bdynets/R), [native sources](bdynets/src) and the
[installed C++ headers](bdynets/inst/include/bdynets).
The [tests](bdynets/tests) cover public interfaces, numerical implementations
and saved-draw compatibility. From the repository root, run in R:

```r
if (!requireNamespace("pak", quietly = TRUE)) install.packages("pak")
pak::pak("./bdynets", dependencies = TRUE)
testthat::test_local("bdynets")
```

See the [package development instructions](bdynets/README.md#development-and-validation)
for building and checking the source archive. Dated migration checks and
historical package-generation material are indexed in [MIGRATION.md](MIGRATION.md).
