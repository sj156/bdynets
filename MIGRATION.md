# Package integration and migration

## GLS and shared kernels in bdynets — 2026-11-06

`bdynets/` is the single installable package, version 0.2.0. GLS is a method
module alongside the existing graphMoDE, historical mixture, network-data and
traffic interfaces. GLS code is imported from the repository's current
standalone 0.5.1 implementation, including study controls, simulation,
learned-affinity inference, PNAR benchmarks, plots and the compiled reference
filter. This is an additive migration onto the current repository package;
its traffic APIs, assets, examples and tests are retained.

From the repository root:

```r
pak::pak("./bdynets", dependencies = TRUE)
library(bdynets)
control <- gls_control("smoke", backend = "cpp")
```

With dependencies already installed, use `R CMD INSTALL bdynets`. After the
changes are pushed, `pak::pak("sj156/bdynets/bdynets", dependencies = TRUE)`
installs the same package. There is no separate GLS or kernel package to load.

### Existing APIs

The `gls_*` function names and fit classes are retained, including the newer
`gls_study_*`, directed-grid simulation and PNAR workflow interfaces. Replace
`library(GLS)` with `library(bdynets)`, and qualify calls with `bdynets::`
instead of `GLS::`. External study scripts must make the same namespace change.
The former `GLS/` package directory is retired recoverably after its source is
migrated; it is not an additional installation target.

`GLS-paper/` contains the study and prepared-data application launchers. Model
functions stay in `bdynets/R/`; the former package example is now
`GLS-paper/multihop.R`. Datasets, chain outputs and local migration backups are
kept outside the source change. Legacy external benchmark scripts mentioned in
the method guide were not part of the supplied checkout.

See [bdynets/GLS.md](bdynets/GLS.md) for the preserved fitting and study guide,
[bdynets/CPP-KERNELS.md](bdynets/CPP-KERNELS.md) for reusable numerical tools,
and [graphMoDE/SHARED-KERNELS.md](graphMoDE/SHARED-KERNELS.md) for the research
driver's compatibility boundary.

### Numerical behavior and cache identities

`gls_control(backend = "cpp")` selects compiled scalar FFBS for the main and
general GLS engines; `backend = "R"` retains their reference FFBS. The separate
`reference_backend = "native"` or `"R"` control retains the existing safeguarded
C Newton reference filter and its R alternative. Neither the generic Poisson
Brent filter nor an older prototype replaces that current GLS reference path.
The study FFBS and study reference recursion are unchanged.

The GLS covariance-density correction, priors, numerical jitter, initial factor
draw and R random-number generation remain model policies. Small floating-point
differences can accumulate over long chains; fixed seeds do not promise
bitwise-identical chains across numerical backends. Backend and engine controls
identify changed sampler behavior. Existing historical results keep their
original provenance and should not be relabeled as new bdynets runs.

### Reusable kernels and graphMoDE scope

`gaussian_ffbs()` and `poisson_discount_filter()` are public bdynets functions.
Their shared header is `bdynets/inst/include/bdynets/scalar_state_space.h`.
It has no R dependency and accepts caller-supplied normal draws and interrupt
callbacks. Rcpp packages can declare `LinkingTo: Rcpp, bdynets` and include
`<bdynets/scalar_state_space.h>`; plain C++17 projects can use the same header
and retain its adjacent MIT license. Rebuild consumers after header changes.

The generic Gaussian kernel handles one latent scalar observed by one or more
series, with configurable transitions and initial distribution. Zero precision
marks an ignored observation and generic jitter defaults to zero. The Poisson
kernel is a Laplace discount-reference approximation. This does not replace
the graphMoDE research driver's default three-dimensional state sampler or the
historical bdynets multidimensional FFBS. Existing graphMoDE scientific
transitions, diagnostics and experiment controls are unchanged.

### Dependencies and verification

The current bdynets traffic and networking dependencies are preserved, including
its ggplot2 import. BayesLogit, Rcpp and parallel are required for the integrated
code, Rcpp is the only `LinkingTo` dependency, and source builds select C++17.
The existing testthat minimum and optional beepr/posterior dependencies remain.

```r
roxygen2::roxygenise("bdynets")
testthat::test_local("bdynets")
```

```sh
R CMD build bdynets
R CMD check --no-manual bdynets_0.2.0.tar.gz
```

This integration was built and installed into an isolated library from the
current checkout sources. `R CMD check --no-manual --no-vignettes` returned
**Status: OK**: 1,014 testthat assertions passed, with no failures, warnings or
skips, and the existing standalone traffic checks passed. The graphMoDE
environment loaded and called both shared R interfaces without replacing its
sampler. GLS-paper launchers were parsed and checked in plan mode, without
new experiment runs. Older scratch timings are not comparisons against GLS
0.5.1. The dated September record
below applies only to that earlier migration. Short numerical checks assess
software behavior; convergence, ESS and scientific comparisons require their
own evidence. No historical datasets or experiment outputs are part of this
source integration.

## Historical standard-package migration — 2026-09-06

### Maintained package

At the September 6 migration, `bdynets/` became the maintained R package, version 0.1.0. Install after
pushing with `pak::pak("sj156/bdynets/bdynets", dependencies = TRUE)` and load
with `library(bdynets)`. Package source is maintained directly in `bdynets/R/`.
Help pages and NAMESPACE are generated from the R source's roxygen comments.

The graphMoDE milestone-1 implementation is integrated as ten `gmde-*.R` scripts,
with six public entry points and 24 private helpers renamed `.gmde_*` to avoid
collisions with existing and future modules. Public fit classes remain
`gmde_fit`, and fit metadata identifies the containing package as bdynets.
The existing authorship and MIT license are retained. The new graphMoDE code
was written for this project from its specification, not imported third-party
GPL code; the standalone scaffold's provisional metadata is superseded.

### Preserved functionality and material

- The existing `gibbs_sampler()`, summary/plot methods, clustering utilities and
  pipe export remain available. Their algorithms have not been redesigned.
- Legacy numerical helpers remain separate from graphMoDE. The experimental
  sampler's PG fallback, clipping, covariance jitter and lack of the revised
  Poisson correction are documented rather than treated as validated features.
- R Markdown theory and analysis chapters remain in place. `index.Rmd` is now
  an ordinary documentation page, and the metadata/documentation chapters no
  longer generate the package. Their original versions are preserved as inert
  `.Rmd.txt` files in `docs/legacy-litr/`.
- The older research snapshot in `graphMoDE/R/`, other project folders, datasets
  and rendered historical books are unchanged.
- The duplicate standalone package formerly at
  `graphMoDE/packages/graphMoDE/` is moved out of the checkout into the local
  migration backup. The updated `graphMoDE/GRAPHMODE-PACKAGE.md` points to the
  canonical module. The former standalone pak path is superseded.

### Dependency changes

At that migration, the package imported only graphics, magrittr, stats and utils. BayesLogit and
beepr are declared optional (`Suggests`), and testthat is the test dependency.
BayesLogit was already detected optionally by the legacy sampler; requesting
the optional beep now gives a clear dependency error if beepr is absent.
Previously declared notebook-only libraries are no longer attached by loading
bdynets. Notebooks must explicitly load packages used by their analysis code.

`dependencies = TRUE` installs the suggested packages too. Use pak's default
dependency selection when only the core Gaussian module is needed.

### Verification performed

On R 4.6.0/macOS aarch64:

- Package tests: 148 assertions passed, zero failures, warnings or skips.
- `R CMD build`: succeeded.
- `R CMD check --no-manual --no-build-vignettes`: final Status: OK, including
  documentation, examples, installation and installed-package tests.
- Actual `pak::pak(local_package_path, dependencies = TRUE, ask = FALSE)`:
  succeeded in an isolated workspace library; no system-wide R installation
  was changed. All suggested dependencies were present for the final check.
- A fresh vanilla R session loaded the installed package and found both APIs.
- With fixed seeds, the original and integrated graphMoDE K=1/2/3 draws,
  final states and diagnostics are identical. The original and retained legacy
  sampler outputs are identical after excluding elapsed time. Legacy comparison
  was run both without BayesLogit and with BayesLogit available.
- An inherited malformed Rd title was corrected during checking; a nonexistent
  `accept_rate` output item was removed from the legacy sampler documentation.

These checks establish migration compatibility and preserve the existing
focused validation. They do not certify posterior calibration, mixing or the
legacy sampler's statistical target. No formal scientific runs were launched.
The remote GitHub install still requires these local changes to be pushed.

### Local recovery and audit

Before-migration snapshots, staged sources, comparison script and isolated
installation library are under:

`/Users/sheng/Documents/ChatGPT/zhengwei/migrations/bdynets-standard/`

The retired standalone package is backed up there in
`retired-standalone/graphMoDE/`. The earlier development copy is also preserved
at `/Users/sheng/Documents/ChatGPT/zhengwei/packages/graphMoDE/`.
Check logs are under `/Users/sheng/Documents/ChatGPT/zhengwei/bdynets.Rcheck/`.

No Git staging, commit or push is performed by this migration.

### Subsequent development

Add modules as separate, prefixed R scripts with dedicated tests and examples.
Share numerical helpers only after checking that their assumptions agree.
Next for graphMoDE: Poisson/static exact integer-shape PG plus whole-block MH,
then stable dynamic engines; prediction and posterior calibration follow.
