# bdynets

Bayesian dynamic network analysis in one modular R package.

The maintained package is in [`bdynets/`](bdynets). The graphMoDE method is an
analysis module of that package, accessed through `gmde_*` functions after
`library(bdynets)`. The separate [`graphMoDE/`](graphMoDE/readme.md) folder holds
research material and the preserved development snapshot.

## Install

After the package changes are pushed to GitHub:

```r
install.packages("pak") # only if needed
pak::pak("sj156/bdynets/bdynets", dependencies = TRUE)
library(bdynets)
```

The first `bdynets` is the repository; the final `bdynets` is the package
subdirectory containing DESCRIPTION. Before pushing, install from a local
checkout by running this at the repository root:

```r
pak::pak("./bdynets", dependencies = TRUE)
library(bdynets)
```

The previous standalone `graphMoDE/packages/graphMoDE` installation path has
been retired. Both the graphMoDE and historical modules now use the one
`bdynets` installation.

## Analysis modules

| Module | Entry points | Current scope |
| --- | --- | --- |
| Graph-informed mixtures (graphMoDE) | `gmde_graph()`, `gmde_expert()`, `gmde_gate()`, `gmde_mcmc()`, `gmde_fit()`, `gmde_partition()` | Reference Gaussian/static implementation; adaptive graph guidance |
| Historical dynamic count mixtures | `gibbs_sampler()`, `summary()`, `plot()` | Preserved experimental engine; not the revised graphMoDE Poisson sampler |
| Clustering comparison | `adj_rand_index()`, `best_label_accuracy()` | Existing comparison utilities |

See the [graphMoDE example](bdynets/README.md) and [migration notes](MIGRATION.md).
Poisson/static graphMoDE, dynamic experts and prediction are later milestones.

## Repository layout

| Location | Purpose |
| --- | --- |
| [`bdynets/R/`](bdynets/R) | Maintained package code; `gmde-*.R` contains the graphMoDE module |
| [`bdynets/tests/testthat/`](bdynets/tests/testthat) | Module-specific and integration tests |
| [`bdynets/man/`](bdynets/man) | R help pages generated from source comments |
| [`bdynets/README.md`](bdynets/README.md) | Working Gaussian/static example and model contract |
| [`graphMoDE/`](graphMoDE/readme.md) | Historical research snapshot, provenance, data attribution and debugging guides |
| [`docs/legacy-litr/`](docs/legacy-litr) | Archived package-generation instructions, retained as text |
| Root `.Rmd` files | Documentation, historical derivations and analysis examples |

## Validation status

The September 6, 2026 package migration passed **148 test assertions** and
`R CMD check --no-manual --no-build-vignettes` with **Status: OK**. Local
installation through pak with `dependencies = TRUE` was also verified in an
isolated library. See [MIGRATION.md](MIGRATION.md) for commands, compatibility
checks and remaining limitations.

These checks cover implementation and migration compatibility. Default chains
are smoke tests; posterior calibration, mixing assessment and formal scientific
experiments remain separate work. The old K=10 diagnostic issue documented in
the graphMoDE research snapshot is still unresolved.

## Source of truth and development

Maintain code directly in `bdynets/R/*.R`. Use module-prefixed scripts and private
helpers: `gmde-*.R` and `.gmde_*` for graphMoDE. All modules share one DESCRIPTION,
one NAMESPACE, one version and one package installation. Roxygen comments generate
help files and NAMESPACE; R Markdown does not generate package source.

From the repository root:

```r
roxygen2::roxygenise("bdynets")
testthat::test_local("bdynets")
```

```sh
R CMD build bdynets
R CMD check --no-manual bdynets_0.1.0.tar.gz
R CMD INSTALL bdynets
```

The root `*.Rmd` chapters are historical explanations and analysis examples.
`index.Rmd` is now an ordinary R Markdown development guide. The original
package-generation setup is preserved as inert text in `docs/legacy-litr/`.
Do not regenerate the maintained package with litr. Existing paper-specific
folders (`graphMoDE/`, `BDCN/`, `dynamicGraphMaternGP/`) remain research material;
only code under `bdynets/R/` is included in the installed package.

Optional historical features use BayesLogit and beepr when installed. The
Gaussian graphMoDE module does not require them. `dependencies = TRUE` also
installs suggested packages; pak's default dependency selection is sufficient
for the core Gaussian module.
