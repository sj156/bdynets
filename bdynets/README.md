# bdynets: one package, multiple analysis modules

This directory is the installable package. Graph-informed mixtures, the
historical dynamic count-mixture engine and clustering utilities share one
namespace and version. The `gmde_*` functions implement the graphMoDE module;
there is no separate `library(graphMoDE)` step.

## Installation

Install from the repository's package subdirectory after the changes are pushed:

```r
install.packages("pak") # only if needed
pak::pak("sj156/bdynets/bdynets", dependencies = TRUE)
library(bdynets)
```

For the local checkout, run `pak::pak("./bdynets", dependencies = TRUE)` from
the repository root, or `pak::pak(".", dependencies = TRUE)` from this package
directory. Suggested dependencies are included by `dependencies = TRUE`.

See the [repository overview](../README.md) for the development workflow and
the [graphMoDE research guide](../graphMoDE/readme.md) for historical material.

## Gaussian/static graphMoDE example

```r
ids <- paste0("series", 1:6)
W <- matrix(0, 6, 6)
for (i in 1:5) W[i, i + 1] <- W[i + 1, i] <- 1
graph <- gmde_graph(W, ids, provenance = list(description = "Six-series path"))
B <- cbind(intercept = 1, contrast = c(1, 1, -1, -1))
expert <- gmde_expert("gaussian", B, prior = list(
  m0 = c(0, 0), C0 = diag(c(25, 25)),
  variance = list(shape = 2, scale = 1)
))
Y <- matrix(sin(1:24), 6, 4, dimnames = list(ids, NULL))
fit <- gmde_fit(Y, graph, K = 2, expert = expert,
                gate = gmde_gate(), mcmc = gmde_mcmc(seed = 90601))
partition <- gmde_partition(fit)
partition$psm
partition$partition
```

The default 100 sweeps are smoke-test lengths, not an inferential recommendation.
One allocation belongs to an entire series. IDs must agree between the graph
and panel. Coefficient covariance C0 is independent of observation variance;
the variance prior uses inverse-gamma shape/scale. Raw labels are retained.

Use `gmde_gate(mode = "fixed", a = 0)` for graph-free logistic-normal utilities,
or `a = 1` for forced graph guidance. Binary adaptive fits use one shared weight
and one Beta prior. Multiclass fits default to class-specific weights; use
`shared = TRUE` to tie them. K=1 omits the gate. Full graph rank is the default;
truncations expand to complete zero and tied eigenspaces and change the prior.

The implementation uses no silent graph symmetrization, covariance jitter,
weight clipping or approximate PG fallback. It currently supports complete
Gaussian panels with static experts. Poisson, dynamic states, forecasting,
missing histories and new units are not yet supported by the gmde_* API.
See `?gmde_fit` and the other function help pages for complete contracts.

## Scripts

| R script | Responsibility |
| --- | --- |
| gmde-validation.R | Module-specific input and numerical checks |
| gmde-graph.R | Graph IDs, kernel and complete-eigenspace rank rules |
| gmde-expert.R | Gaussian/static expert specification |
| gmde-gate-spec.R | Gate settings, contrasts and white utility construction |
| gmde-gate-updates.R | Joint ESS and logit guidance MH |
| gmde-gaussian-static.R | Static Gaussian and inverse-gamma draws |
| gmde-allocation.R | Whole-series likelihoods and allocations |
| gmde-mcmc-control.R | Chain, seed and retention settings |
| gmde-fit.R | Sampler and fit metadata |
| gmde-partition.R | Co-clustering and representative partition |
| gibbs.R, gibbs-helpers.R, all-helpers.R | Preserved historical module and utilities |

All modules are maintained directly as R scripts, with module-specific tests
under `tests/testthat/`. Internal `.gmde_*` helpers are separate from legacy
helpers. Roxygen generates NAMESPACE and man pages; litr is no longer a build
dependency. Package authorship and MIT licensing follow bdynets.

## Historical module

`gibbs_sampler()`, `summary.bdynets_mcmc()`, `plot.bdynets_mcmc()`,
`adj_rand_index()`, `best_label_accuracy()` and the existing pipe export remain
available. The historical sampler may use approximate PG draws, clipped weights
and jittered covariance calculations; it does not implement the revised
graphMoDE Poisson correction. Its preservation is not a correctness endorsement.
The Gaussian graphMoDE module never calls those numerical helpers.

Runtime imports are graphics, magrittr, stats and utils. BayesLogit and beepr
are optional historical features; testthat is a test dependency. Notebook-only
libraries are no longer attached by loading the package. Existing notebooks
must explicitly load the libraries their analyses use.

## Development and validation

From the repository root:

```r
roxygen2::roxygenise("bdynets")
testthat::test_local("bdynets")
```

```sh
R CMD build bdynets
R CMD check --no-manual --no-build-vignettes bdynets_0.1.0.tar.gz
```

The September 6, 2026 migration passed 148 assertions and the package check
with Status: OK. pak installation with all suggested dependencies and loading
in a fresh R session also passed. Fixed-seed comparison confirmed unchanged
graphMoDE K=1/2/3 outputs and preserved legacy sampler outputs (apart from
elapsed time). See the [migration record](../MIGRATION.md).

These are focused implementation and compatibility checks. They do not establish
posterior calibration, chain convergence or scientific performance. Keep future
modules in separate prefixed scripts with their own tests and examples.

## Next milestone

Implement graphMoDE Poisson/static with exact integer-shape PG and whole-block
MH, validated against quadrature; then stable Gaussian FFBS and Poisson PG-FFBS.
Prediction and posterior calibration follow. No formal experiments were run
during this package migration.
