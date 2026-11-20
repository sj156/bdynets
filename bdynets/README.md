# bdynets: one package, multiple analysis modules

This directory is the installable package. Graph-informed mixtures (graphMoDE),
structured global-local shrinkage and selection (GLS), historical count
mixtures, network and traffic interfaces, and shared numerical kernels use one
namespace and version. Load `library(bdynets)` for all packaged modules. The
`gmde_*` and `gls_*` functions identify their respective analysis modules.

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
Source installation requires Rcpp and a working C++17 compiler. GLS and the
shared kernels are included in this package; no separate installation is needed.

See the [repository overview](../README.md) for the development workflow and
the [graphMoDE research guide](../graphMoDE/README.md) for historical material.

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

## GLS count-network module

GLS provides edge-count shrinkage, static-plus-dynamic propagation, posterior
diagnostics, joint DSS and sequential one-step prediction. Its current
directed-grid study, learned-affinity model and PNAR benchmark interfaces are
preserved within bdynets.

```r
library(bdynets)
control <- gls_control("smoke", backend = "cpp", reference_backend = "native")
r_control <- gls_control("smoke", backend = "R", reference_backend = "R")
study_control <- gls_study_control("smoke")
```

Pass the general controls to `gls_fit()` or `gls_fit_general()`. `backend`
selects their scalar FFBS implementation; C++ is the default. The independent
`reference_backend` option retains the established safeguarded C Newton
reference filter or its R alternative. The covariance-density correction,
initial factor draw, R RNG and GLS jitter convention remain unchanged. The
study FFBS and study reference recursion are retained separately.

See the [GLS method guide](GLS.md) for full fitting, simulation, PNAR and
post-processing instructions. Smoke and quick profiles check execution rather
than establish convergence. These GLS interfaces do not extend the packaged
`gmde_*` Gaussian/static contract.

## Shared scalar numerical kernels

The public `gaussian_ffbs()` and `poisson_discount_filter()` functions can be
used independently of any analysis module:

```r
set.seed(1)
draw <- gaussian_ffbs(
  c(1.1, .8, 1.3), precision = 4, innovation_variance = .05,
  transition = .95, initial_mean = 1, initial_variance = .2
)
reference <- poisson_discount_filter(
  c(2, 3, 1, 4), loadings = .2, offset = log(3), discount = .95
)
```

The Gaussian kernel handles one latent scalar observed by one or more series,
with configurable transitions, innovation variances and initial distribution.
Zero observation precision marks missing values. Generic jitter defaults to
zero; the GLS FFBS adapter requests its established jitter convention. The
Poisson routine is a Laplace discount-reference approximation with a bracketed
Brent solver, not exact filtering or a replacement for the existing GLS C
Newton reference filter.

The shared header is `inst/include/bdynets/scalar_state_space.h`. Other Rcpp
packages can declare `LinkingTo: Rcpp, bdynets`, select C++17, and include
`<bdynets/scalar_state_space.h>`. Plain C++ programs can use the same header
with `bdynets/inst/include` on their include path and supply their own
normal-draw callback. The header itself has no R dependency. Keep its adjacent
MIT license when copying it, and rebuild consumers after header changes.

See the [kernel guide](CPP-KERNELS.md) for detailed R/C++ contracts and the
[graphMoDE bridge guide](../graphMoDE/SHARED-KERNELS.md) for compatible scalar
reuse. The current graphMoDE research driver has a three-dimensional default
state and is unchanged; neither it nor the historical multidimensional bdynets
FFBS has been migrated to this scalar kernel.

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
| gls-api.R, gls-general-api.R | GLS controls, fitting and prediction APIs |
| gls-study-*.R, gls-simulation.R, gls-benchmark-pnar.R | Current directed-grid study, diagnostics, output and PNAR benchmarks |
| scalar-state-space.R | Reusable Gaussian FFBS and Poisson reference-filter R interfaces |
| gibbs.R, gibbs-helpers.R, all-helpers.R | Preserved historical module and utilities |

All modules are maintained directly as R scripts, with module-specific tests
under `tests/testthat/`. Internal `.gmde_*` helpers are separate from legacy
helpers. Roxygen generates NAMESPACE and man pages; litr is no longer a build
dependency. Package authorship and MIT licensing follow bdynets. Native Rcpp
adapters live in `src/` and use the installed shared header where appropriate;
the established GLS C reference implementation is retained for compatibility.

## Historical module

`gibbs_sampler()`, `summary.bdynets_mcmc()`, `plot.bdynets_mcmc()`,
`adj_rand_index()`, `best_label_accuracy()` and the existing pipe export remain
available. The historical sampler may use approximate PG draws, clipped weights
and jittered covariance calculations; it does not implement the revised
graphMoDE Poisson correction. Its preservation is not a correctness endorsement.
The Gaussian graphMoDE module never calls those numerical helpers.

The current traffic and networking imports are retained, including ggplot2,
data.table, curl and jsonlite. BayesLogit, Rcpp and parallel are required by the
integrated code. The historical beep still uses optional beepr; testthat is a
test dependency. Notebook-only libraries are not attached by loading the package.
Existing notebooks must explicitly load the libraries their analyses use.

## Development and validation

From the repository root:

```r
roxygen2::roxygenise("bdynets")
testthat::test_local("bdynets")
```

```sh
R CMD build bdynets
R CMD check --no-manual --no-build-vignettes bdynets_0.2.0.tar.gz
```

The September 6, 2026 version 0.1.0 migration passed 148 assertions and the package check
with Status: OK. pak installation with all suggested dependencies and loading
in a fresh R session also passed. Fixed-seed comparison confirmed unchanged
graphMoDE K=1/2/3 outputs and preserved legacy sampler outputs (apart from
elapsed time). This is historical evidence for that migration, not a test
count for the current integration. The clean-checkout package needs its own
build and check. See the [migration record](../MIGRATION.md).

These are focused implementation and compatibility checks. They do not establish
posterior calibration, chain convergence or scientific performance. Keep future
modules in separate prefixed scripts with their own tests and examples.

## Next milestone

For the packaged `gmde_*` API, implement Poisson/static with exact integer-shape
PG and whole-block MH, validated against quadrature, before adding dynamic
experts and prediction. The separate `graphMoDE/` research driver already has
its own dynamic state implementation and controls; those remain unchanged.
Shared scalar kernels can support compatible cases without replacing its
multidimensional smoother. No formal experiments were run during the original
September package migration.

## Shared network data

The optional `network_data()` and `network_panel()` interfaces provide explicit
IDs, time-by-unit observations, exposure and missingness. Existing model APIs
are unchanged. `beijing_panel()` and `largest_panel()` feed the same
`network_view_data()` payload and `plot()` method. See the
[collaborator guide](../examples/shared-data/README.md) for migration examples
and the synthetic, no-fitting demonstration.

## Traffic measurements and reported events

The [Beijing](../examples/beijing/README.md),
[PeMS](../examples/largest/README.md),
[City-traffic-M](../examples/metropolis/README.md) and
[Divvy](../examples/DivvyBike/README.md) examples prepare source records for the
same package. Each `prepare.R` reads its original formats, validates source IDs and clocks,
and prepares the tables needed by its map type. Road maps use roads, road-time
observations and optional events or measurements. OD maps use stations and
directed trip records with their own timing. The two input forms are described
in the [input specification](inst/INPUT-FORMAT.md); the
[source index](../examples/DATA-SOURCES.md) records their origins and limits.

From the bundle root, run this in a fresh R session to load the bundled package.

```r
source("bdynets/inst/preparation-session.R")
preparation_session("examples/beijing")
library(bdynets)
traffic_flow_map(readRDS("examples/beijing/data/map-frame-0800.rds"), basemap = NULL)
traffic_od_map(readRDS("examples/DivvyBike/data/spatial-1500m/window-0000-0300.rds"),
               basemap = NULL)
```

`traffic_flow_map()` draws the ready road frame and `traffic_od_map()` draws the
prepared OD stations and routes. Existing unified and mixed-map calls can
continue to use `traffic_map()` with `traffic_input()`. For the full-day road trend, load
`examples/beijing/data/input.rds` with `read_traffic()` and call
`traffic_trend()`. Source units and missing values are retained. Road flow
uses seven log1p green/yellow/red classes.
The default OD style has five trip-count classes; the Divvy report supplies
seven integer-count classes for its grouped display. `traffic_map_html()`
provides the interactive road HTML interface.

The traffic implementation has six source files.

| File | Responsibility |
|---|---|
| `R/traffic-data.R` | Existing data interfaces, field mappings, validation, summaries and trends |
| `R/traffic-preparation.R` | Prepared-data export, source protection, dictionaries and readback |
| `R/traffic-map.R` | Road and OD maps, event layers, spatial layout and figure exports |
| `R/traffic-map-parts.R` | OD selection and shared static map components |
| `R/traffic-basemap.R` | Coordinate-derived OpenStreetMap context and caching |
| `R/aggregate-od-data.R` | Spatial OD aggregation and timestamp parsing |

Beijing contains one simulation day. PeMS contains 24 hours on 2026-06-16,
with Los Angeles and Bay Area flow and CHP reported starts shown separately.
City-traffic-M contains the selected observations for 2024-10-18. Divvy contains
individual station departures on 2026-01-01, with separate hourly station totals
and 1500 m spatial groups. Its map draws a selected time window of the grouped day.

Each case has its own `run.R`. It installs this repository's package into a
temporary library and renders the named R Markdown report. Rendering requires
knitr, rmarkdown and Pandoc alongside the declared package imports; the source
package also needs Rcpp and a C++17 compiler. City-traffic-M additionally uses
Python 3, NumPy, pandas and PyArrow to read its original-format Parquet sample.

Matching verified files in `data/basemap-cache` supply OpenStreetMap context.
New extents or missing cache entries require network access. `run.R --offline`
performs conversion checks and draws source layers without background streets.
Rendered reports embed static figures and can be opened offline.

The reports are [Beijing](../examples/beijing/beijing-traffic-report.html),
[PeMS](../examples/largest/largest-traffic-report-demo.html),
[City-traffic-M](../examples/metropolis/metropolis-traffic-report-demo.html) and
[Divvy](../examples/DivvyBike/divvy-od-report.html).
Each report includes original fields, conversion steps, intermediate data,
map-specific tables, source checks and export/readback. `write_traffic_input()`
exports road or OD RDS/CSV data, a field dictionary, validation and provenance
to a new directory.
