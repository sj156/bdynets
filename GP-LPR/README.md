# GP-LPR: no-GP Poisson latent-position pilot

The authoritative implementation is the existing **bdynets** R package in
`../bdynets`. This study folder contains thin entrypoints and intended reports;
it does not contain another sampler or a duplicate package. The GP component
is deferred. Work is local and uncommitted; no GitHub push has been made.

## Layout and installation

```text
bdynets/                       # repository root
  bdynets/                     # existing installable R package
    R/lpr-helpers.R            # data, centering, alignment, storage
    R/lpr-mcmc.R               # inference and exact checkpoint continuation
    R/lpr-reporting.R          # diagnostics, simulations, figures and tables
    tests/testthat/test-lpr.R
  GP-LPR/
    scripts/run.R              # explicit report, quick, or pilot mode
    scripts/render.R           # render saved results only
    recovery.Rmd               # executable report, no fitting
    reports/recovery.html      # readable selected historical analysis
    reports/aligned_positions.pdf
    reports/recovery.csv
```

From the repository root, install the package with `R CMD INSTALL bdynets`.
The report additionally needs `rmarkdown` and `knitr`; chain diagnostics need
`posterior`. The sampler itself uses base/recommended R packages. R 4.6.0 was
used for this port. Use an isolated library if preferred:

```sh
mkdir -p /tmp/bdynets-library
R CMD INSTALL --library=/tmp/bdynets-library bdynets
export R_LIBS_USER=/tmp/bdynets-library
```

## Reproduce the manuscript figure without refitting

Set the external results store explicitly. The current local store is outside
this repository, at `/Users/sheng/Documents/ChatGPT/freeway_accidents/results/gp_lpr`.
A collaborator can use a different path after copying the result artifact.
Large draws, checkpoints, and caches are not committed or placed in Overleaf.

```sh
export GP_LPR_RESULTS_ROOT=/Users/sheng/Documents/ChatGPT/freeway_accidents/results/gp_lpr
cd GP-LPR
Rscript scripts/run.R report "$GP_LPR_RESULTS_ROOT/historical-python-20261015/study.rds" reports
Rscript scripts/render.R "$GP_LPR_RESULTS_ROOT/historical-python-20261015/study.rds"
```

Both entrypoints call `bdynets::lpr_report()`. Report generation reads the
specified file; it neither fits models nor silently substitutes another run.
The PDF in `reports/` is the intended Overleaf asset. `recovery.html` is a
self-contained, readable record, including session information.

## Run the R implementation

```sh
# Execution check: n=8, two chains, 60 warmup + 60 sampling sweeps.
Rscript scripts/run.R quick "$GP_LPR_RESULTS_ROOT/my-quick-run/study.rds" "$GP_LPR_RESULTS_ROOT/my-quick-run/report"
# Optional new study, not needed to reproduce the selected historical figure:
Rscript scripts/run.R pilot "$GP_LPR_RESULTS_ROOT/my-r-pilot/study.rds" "$GP_LPR_RESULTS_ROOT/my-r-pilot/report"
```

A result file must not already exist. The pilot profile uses n=30, four chains,
3,000 warmup + 6,000 sampling sweeps and storage interval three. New R RNG draws
are different from historical Python draws, even with the same integer seeds.
Quick results are execution checks, not convergence evidence. R fits preserve
chain boundaries, state, RNG, tuning counters, target identity and implementation
identity for continuation. Use `lpr_continue(fit, sweeps=...)`; it refuses changed
targets or inference code. Use `lpr_save()`/`lpr_read()` for complete round trips.
`lpr_diagnostics(fits)` returns rank/folded Rhat and bulk/tail ESS; these ESS values
are not identical to the historical Python original-scale ESS estimator.

## Selected evidence and provenance

The manuscript figure uses only the historical Python Poisson fits at exposures
1 and 10. Their arrays are preserved without thinning or dropping draws, and
were imported into RDS with original NPZ SHA-256 hashes and source-code hash.
The importer checked the RDS round trip. R independently reproduced all 16,000
stored Poisson log likelihoods and the reported aligned coordinate RMSEs
(0.3724690 and 0.1480685). Original data seeds are 1001/1002 and chain seeds
10100--10103/10200--10203; configuration seed 20261015. The original source
version is a file hash, not a Git commit. It is not attributed to a later R commit.
Historical imports lack raw endpoint/RNG checkpoints and cannot be continued.

Baseline repository commit before these additions: `652ca34`. Changes are local;
implementation fingerprints distinguish new R checkpoints from this baseline.
All 167 package tests passed, including 19 new LPR expectations; `R CMD check --no-manual --no-build-vignettes` returned `Status: OK`. The new R quick
profile was executed and the HTML was rendered from saved historical results.
The original Python implementation is retired to a recoverable local archive.

The selected exposure-10 result has maximum interaction Rhat 1.0121, above 1.01;
its interaction interval inclusion is 0.906 and its intercept interval misses
truth. These limitations remain in the draft and report. One configuration and
complete-pair support do not establish uncertainty calibration or sparse-road
recovery. Pending: longer-chain checks for the selected high-exposure case
(requires a fresh run, because its checkpoint is incomplete), repeated
configurations, prior sensitivity, sparse support, and an endpoint-free ablation.
No GP fitting or larger study was performed for this reporting/port task.
