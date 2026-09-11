# Standard-package migration — 2026-09-06

## Maintained package

`bdynets/` is the single maintained R package, version 0.1.0. Install after
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

## Preserved functionality and material

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

## Dependency changes

The package now imports only graphics, magrittr, stats and utils. BayesLogit and
beepr are declared optional (`Suggests`), and testthat is the test dependency.
BayesLogit was already detected optionally by the legacy sampler; requesting
the optional beep now gives a clear dependency error if beepr is absent.
Previously declared notebook-only libraries are no longer attached by loading
bdynets. Notebooks must explicitly load packages used by their analysis code.

`dependencies = TRUE` installs the suggested packages too. Use pak's default
dependency selection when only the core Gaussian module is needed.

## Verification performed

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

## Local recovery and audit

Before-migration snapshots, staged sources, comparison script and isolated
installation library are under:

`/Users/sheng/Documents/ChatGPT/zhengwei/migrations/bdynets-standard/`

The retired standalone package is backed up there in
`retired-standalone/graphMoDE/`. The earlier development copy is also preserved
at `/Users/sheng/Documents/ChatGPT/zhengwei/packages/graphMoDE/`.
Check logs are under `/Users/sheng/Documents/ChatGPT/zhengwei/bdynets.Rcheck/`.

No Git staging, commit or push is performed by this migration.

## Subsequent development

Add modules as separate, prefixed R scripts with dedicated tests and examples.
Share numerical helpers only after checking that their assumptions agree.
Next for graphMoDE: Poisson/static exact integer-shape PG plus whole-block MH,
then stable dynamic engines; prediction and posterior calibration follow.
