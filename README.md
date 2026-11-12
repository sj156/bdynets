# bdynets

Bayesian dynamic network analysis in one modular R package.

The installable package is [`bdynets/`](bdynets/README.md). Graph-informed
mixtures (graphMoDE), structured global-local shrinkage and selection (GLS),
historical dynamic count mixtures, and shared numerical tools use one
DESCRIPTION, NAMESPACE, version and installation. Load `library(bdynets)` to
use their public interfaces. GLS is a method module of bdynets.

The [`graphMoDE/`](graphMoDE/README.md) research driver and its frozen experiment
controls remain in the same repository. Its multidimensional state sampler is
unchanged. The packaged `gmde_*` Gaussian/static API and that research driver
have distinct scopes; see the [shared-kernel bridge](graphMoDE/SHARED-KERNELS.md).

## Install

For copy-and-paste Linux terminal commands, see the
[GLS installation and smoke run](GLS-paper/README.md#linux-installation).

From the repository root:

```r
install.packages("pak") # only if needed
pak::pak("./bdynets", dependencies = TRUE)
library(bdynets)
```

Alternatively, with the required dependencies installed:

```sh
R CMD INSTALL bdynets
```

Source installation compiles C++17 through Rcpp and requires a working C++
compiler. BayesLogit, Rcpp and parallel are package dependencies alongside the
existing R imports. There are no separate GLS or numerical-kernel packages to
install. After these changes are pushed, the GitHub installation is:

```r
pak::pak("sj156/bdynets/bdynets", dependencies = TRUE)
library(bdynets)
```

The first `bdynets` is the repository; the final `bdynets` is the package
subdirectory containing DESCRIPTION.

## Analysis modules

| Module | Entry points | Current scope |
| --- | --- | --- |
| Graph-informed mixtures (graphMoDE) | `gmde_graph()`, `gmde_expert()`, `gmde_gate()`, `gmde_mcmc()`, `gmde_fit()`, `gmde_partition()` | Gaussian/static experts and adaptive graph guidance |
| Structured global-local shrinkage and selection (GLS) | `gls_control()`, `gls_fit()`, `gls_fit_general()`, `gls_study_control()`, `gls_run_grid_study()`, `gls_dss()`, `predict()` | Edge-count shrinkage, static-plus-dynamic propagation, learned study affinities, DSS, directed-grid simulation and PNAR benchmarks |
| Shared scalar numerical kernels | `gaussian_ffbs()`, `poisson_discount_filter()` | Configurable scalar Gaussian FFBS and approximate Poisson Laplace discount filtering, with a reusable C++ header |
| Historical dynamic count mixtures | `gibbs_sampler()`, `summary()`, `plot()` | Preserved experimental engine; not the revised graphMoDE Poisson sampler |
| Clustering comparison | `adj_rand_index()`, `best_label_accuracy()` | Existing comparison utilities |

See the [Gaussian/static example](bdynets/README.md),
[GLS method guide](bdynets/GLS.md),
[shared numerical-kernel guide](bdynets/CPP-KERNELS.md), and
[migration notes](MIGRATION.md).

Paper replication launchers live in [`GLS-paper/`](GLS-paper/README.md), separate
from the package. They cover the package-based simulation and prepared-data
application workflows, with explicit plan, run and saved-result report modes.

## GLS and C++ from R

The main and general GLS fitting interfaces use scalar C++ FFBS by default:

```r
library(bdynets)
control <- gls_control("smoke", backend = "cpp", reference_backend = "native")
reference_control <- gls_control("smoke", backend = "R", reference_backend = "R")
study_control <- gls_study_control("smoke")
```

Pass the general controls to `gls_fit()` or `gls_fit_general()`; study functions
use `gls_study_control()`. `backend` selects main/general FFBS. The independent
`reference_backend` control retains the existing safeguarded C Newton reference
filter (`"native"`) or its R reference (`"R"`). The covariance-density correction,
R RNG and existing GLS numerical policies are retained. The study sampler's
FFBS and reference recursion are unchanged. Smoke and quick profiles check
execution; they do not establish convergence.

The shared numerical interfaces can also be used independently of any analysis
module:

```r
draw <- gaussian_ffbs(
  c(1.1, .8, 1.3), precision = 4, innovation_variance = .05,
  transition = .95, initial_mean = 1, initial_variance = .2
)
reference <- poisson_discount_filter(
  c(2, 3, 1, 4), loadings = .2, offset = log(3), discount = .95
)
```

These kernels model one latent scalar observed by one or more series.
`gaussian_ffbs()` supports configurable transitions and initial states, with
zero observation precision marking missing observations. Its default jitter is
zero; GLS explicitly requests its established jitter convention.
`poisson_discount_filter()` computes a Laplace reference approximation rather
than exact Poisson filtering. The existing multidimensional historical bdynets
FFBS has not been replaced by the scalar kernel.

## Reuse the C++ header

The authoritative numerical implementation is
[`bdynets/inst/include/bdynets/scalar_state_space.h`](bdynets/inst/include/bdynets/scalar_state_space.h).
It is independent of R and accepts caller-supplied normal draws and interrupt
callbacks.

Another Rcpp package can declare `LinkingTo: Rcpp, bdynets`, select C++17, and
include:

```cpp
#include <bdynets/scalar_state_space.h>
```

A plain C++ project can use the same header with
`bdynets/inst/include` on its include path, or copy the header while preserving
its MIT license notice. No R runtime is needed by the header itself. Rebuild
dependent packages when changing the header. See the
[kernel guide](bdynets/CPP-KERNELS.md) for matrix layout, callback and numerical
contracts.

## Repository layout

| Location | Purpose |
| --- | --- |
| [`bdynets/R/`](bdynets/R) | Maintained module implementations and shared R interfaces |
| [`bdynets/src/`](bdynets/src) | Rcpp adapters and registered native bindings |
| [`bdynets/inst/include/bdynets/`](bdynets/inst/include/bdynets) | Shared C++ numerical implementation |
| [`bdynets/tests/testthat/`](bdynets/tests/testthat) | Module-specific, numerical and integration tests |
| [`bdynets/man/`](bdynets/man) | R help pages generated from source comments |
| [`graphMoDE/`](graphMoDE/README.md) | Current research driver, experiment controls, provenance and debugging guides |
| [`docs/legacy-litr/`](docs/legacy-litr) | Archived package-generation instructions, retained as text |
| Root `.Rmd` files | Development guide and historical simulation example |

## Source of truth and development

Maintain code directly in `bdynets/R/`, the shared header and native adapters.
Use module-prefixed scripts and dedicated tests. Roxygen generates help files
and NAMESPACE; R Markdown does not generate package source.

From the repository root:

```r
roxygen2::roxygenise("bdynets")
testthat::test_local("bdynets")
```

```sh
R CMD build bdynets
R CMD check --no-manual bdynets_0.2.1.tar.gz
R CMD INSTALL bdynets
```

See [MIGRATION.md](MIGRATION.md) for the current consolidation and clearly
dated historical validation. Package tests establish implementation and
compatibility within their coverage; posterior calibration, mixing and
scientific comparisons require separate evidence. The old K=10 diagnostic
issue documented in the graphMoDE research driver remains unresolved.

The root `index.Rmd` and `5simulations.Rmd` are a development guide and a
historical simulation example. Obsolete litr chapters are preserved as inert
text in `docs/legacy-litr/retired-chapters/`. Do not regenerate the maintained
package with litr. Paper-specific folders remain research material.

## Poisson latent-position regression pilot

The no-GP GP-LPR study uses the `lpr_*` functions in bdynets. See
[GP-LPR/README.md](GP-LPR/README.md) for batch scripts, checkpoint contracts,
external result-store configuration and manuscript figure reproduction. The
[HTML report](GP-LPR/reports/recovery.html) reads saved historical draws; it
does not silently refit the model.
