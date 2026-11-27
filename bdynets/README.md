# bdynets: one package, multiple analysis modules

This directory is the installable package. Graph-informed mixtures (graphMoDE),
structured global-local shrinkage and selection (GLS), historical count
mixtures, network and traffic interfaces, and shared numerical kernels use one
namespace and version. Load `library(bdynets)` for all packaged modules. The
`gmde_*` and `gls_*` functions identify their respective analysis modules.

## Installation

To install the published package from GitHub, run in R:

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
the [graphMoDE study](../graphMoDE/README.md) for the current dynamic models and results.

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

The static Gaussian interface takes complete panels. For dynamic Poisson
mixtures and forecasting, see `?gmde_dynamic_fit` and the
[eight-method study](../graphMoDE/README.md).

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

Use `gls_fit_general()` with counts, known covariates and a graph. Supply
`pairs` for a restricted candidate dictionary. `fixed_tau` sets the global
shrinkage scale, and `affinity_model = "graph_laplace"` learns the strength of
the graph-based shrinkage weights. `gls_dss()` selects a sparse summary on
validation data. The directed-grid study has its own `gls_study_control()`.

For Linux installation, a runnable simulation and a prepared-data application,
see the [GLS paper launchers](../GLS-paper/README.md). That guide also lists
the external inputs used for the projected Divvy 970-relation analysis. The
[GLS method guide](GLS.md) gives the model and benchmark settings.

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
reuse. For the current dynamic graphMoDE implementation and its multivariate
state sampler, see the [study guide](../graphMoDE/README.md).

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
| gmde-dynamic*.R, gmde-discount.R, gmde-pnar-benchmark.R | Dynamic count mixtures, discount guides, predictions and study comparisons |
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

From the repository root, install the suggested packages and run the tests in R.

```r
if (!requireNamespace("pak", quietly = TRUE)) install.packages("pak")
pak::pak("./bdynets", dependencies = TRUE)
testthat::test_local("bdynets")
```

After editing roxygen comments, run `roxygen2::roxygenise("bdynets")`.
To build and check the current package version, use the following in a Linux shell.

```sh
version=$(Rscript --vanilla -e 'cat(read.dcf("bdynets/DESCRIPTION", fields = "Version")[1])') &&
R CMD build bdynets &&
R CMD check --no-manual "bdynets_${version}.tar.gz"
```

Dated test counts and backend comparisons are available in the
[migration record](../MIGRATION.md).

## Shared network data

The optional `network_data()` and `network_panel()` interfaces provide explicit
IDs, time-by-unit observations, exposure and missingness. Existing model APIs
are unchanged. `beijing_panel()` and `largest_panel()` feed the same
`network_view_data()` payload and `plot()` method. See the
[collaborator guide](../examples/shared-data/README.md) for migration examples
and the synthetic, no-fitting demonstration.

## Traffic measurements and reported events

The [five traffic examples](../examples/README.md) prepare their source data
and pass the completed drawing inputs to the package. Their reports require
rmarkdown, knitr and Pandoc. City-traffic-M also uses Python 3 with NumPy,
pandas and PyArrow.

### Draw a requested time

Give the corresponding function the complete prepared object or its RDS path.
Select the prepared start using `time_label` or a one-based `index`. Use a
full date and time or a frame index when the same clock time occurs on more
than one date. The file extension in `output_file` selects PNG or PDF.

The following R calls use the files written by `Rscript run.R --export` after
running that command from the Beijing, PeMS Speed and Divvy case directories.
Run the calls from the handover root.

```r
library(bdynets)

traffic_flow_map(
  "examples/beijing/output/prepared-data/flow-frames.rds",
  time_label = "08:00", basemap = NULL,
  output_file = "beijing-flow.png", draw = FALSE
)

traffic_speed_map(
  "examples/pems-speed/output/prepared-data/speed-frames.rds",
  time_label = "08:00", region = "Los Angeles Area", basemap = NULL,
  output_file = "pems-speed-los-angeles.png", draw = FALSE
)

traffic_od_map(
  "examples/DivvyBike/output/prepared-data/od-frames.rds",
  time_label = "00:00", basemap = NULL,
  output_file = "divvy-od.png", draw = FALSE
)
```

The [input specification](inst/INPUT-FORMAT.md) provides runnable examples,
metadata fields and default legend rules. Locate the same guide from an
installed R session with `system.file("INPUT-FORMAT.md", package = "bdynets")`.
Flow uses seven `log1p` classes, Speed uses fixed speed limits for its classes,
and OD computes classes from positive drawable counts across the supplied frames.

The [background guide](inst/INPUT-FORMAT.md#prepare-an-openstreetmap-background)
shows how to prepare and reuse OpenStreetMap context via OpenFreeMap. Calculate
the coverage for the output size and layout you intend to use, then pass the
local GeoJSON or gzipped GeoJSON path as `basemap`. Road maps fit all roads in
the selected area by default. The example reports use bundled local backgrounds
by default. Use `--no-basemap` to omit them.

### Run the examples

The [example index](../examples/README.md) links to the five reports, source
samples and commands. From a case directory, run `Rscript run.R` to build its
HTML report. Its `.Rmd` sets the default map time through `plot_time`. The road
reports use `"08:00"`, and Divvy uses `"00:00"` for its first three-hour window.
Pass a prepared frame start in `HH:MM` to override it for that run.
Each PeMS report draws separate Los Angeles and San Francisco Bay Area figures
for the chosen hour. The Divvy report draws a portrait view of the selected
window and keeps the trip-count scale based on all prepared windows.

Add `--export` to save the complete prepared series in `output/prepared-data/`
and copy the report’s map PNGs to `output/maps/`. Use `--output-dir NEW_DIRECTORY`
with `--export` to use a different destination. Run a case’s `prepare.R` to
prepare its data separately and generate reusable tables and provenance.
