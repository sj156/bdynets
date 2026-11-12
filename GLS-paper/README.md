# GLS paper replication

GLS is a module of the `bdynets` package. This folder holds thin study and
prepared-data launchers; fitting, DSS, prediction, simulation and numerical
kernels are implemented in `bdynets`. Download and extract the full repository,
then open a terminal in its top-level directory, which contains both `bdynets/`
and `GLS-paper/`.

## Linux installation

Use R 4.1 or later and a C++17 toolchain. On Debian or Ubuntu, install the
system prerequisites if they are missing.

```sh
sudo apt-get update &&
sudo apt-get install -y r-base r-base-dev build-essential pkg-config \
  libcurl4-openssl-dev libssl-dev ca-certificates fonts-dejavu-core
```

On a managed server, use the site's R and compiler environment. The following
commands install the R dependencies and the downloaded package in your home
directory.

```sh
export R_LIBS_USER="$HOME/.local/lib/bdynets"
mkdir -p "$R_LIBS_USER" &&
Rscript --vanilla -e '
  deps <- c("BayesLogit", "curl", "data.table", "ggplot2", "jsonlite", "magrittr", "Rcpp")
  missing <- deps[!vapply(deps, requireNamespace, logical(1), quietly = TRUE)]
  if (length(missing))
    install.packages(missing, lib = Sys.getenv("R_LIBS_USER"), repos = "https://cloud.r-project.org")
' &&
R CMD INSTALL --preclean --clean --library="$R_LIBS_USER" ./bdynets &&
Rscript --vanilla -e 'library(bdynets); print(packageVersion("bdynets"))'
```

Run the launchers from the repository root. In a new terminal, repeat the
`export R_LIBS_USER` line so R can find the same installation.

Both launchers default to `--action=plan --profile=smoke --cores=12` and
`--chunk-size=100`. Plan mode prints the configuration. The `smoke` and `quick`
profiles check execution. Select `--action=run` to fit, and supply a new
absolute results directory outside the repository with an existing parent.

During MCMC, each chain buffers at most `--chunk-size` retained draws at a
time and writes completed batches under `<output>/draws/`. The chunk size is
a positive integer and controls storage only. Iteration budgets, thinning,
chain counts and model settings still come from the selected control. Draw
chunks contain retained samples. A new run uses a new output directory.

Postprocessing loads the current fit's retained history for DSS, prediction
and diagnostics. Its memory use depends on the full history of that fit as
well as the prediction arrays. Keep the `draws/` directory with the saved
results. When moving an output directory, copy its full directory structure
so report mode can find the adjacent draw files. Report mode also accepts
earlier results that store posterior draws inside the fit object.

## Directed-grid simulation

This example generates its own data and can be pasted after installation.
Each run creates a fresh directory under `$HOME/bdynets-results`.

```sh
(
  set -e
  mkdir -p "$HOME/bdynets-results"
  run_root="$(mktemp -d "$HOME/bdynets-results/gls-smoke.XXXXXX")"
  output="$run_root/run"

  Rscript --vanilla GLS-paper/run_simulation.R --action=plan --profile=smoke \
    --cores=1 --output="$output"
  Rscript --vanilla GLS-paper/run_simulation.R --action=run --profile=smoke \
    --cores=1 --chunk-size=100 --output="$output"
  Rscript --vanilla GLS-paper/run_simulation.R --action=report --output="$output"
  printf 'GLS results: %s\n' "$output"
)
```

Use `--profile=full` for the package's declared full study budget. The runner
calls `gls_study_control()` and `gls_run_grid_study()`, including the package's
GLS, selected GLS and PNAR comparisons and benchmark-data export. It preserves
the profile's chain count and all model settings. Each scenario loads its fit
for postprocessing and retains a draw index in the saved scenario and
`GLS_simulation_summary.rds`. Draws live in `draws/`. Other outputs include
tables, figures, `configuration.R`, and launcher provenance. Report mode calls
`gls_write_grid_study()` on the saved study and rebuilds tables and figures
from its saved outputs and draw files.

The earlier GLS README named external runners such as `run_all.sh`,
`run_benchmarks_only.sh`, sa-PGSS and deep-learning benchmark scripts. Those
files were not present in the supplied checkout and are not reconstructed
here. This launcher does not claim to reproduce their absent implementations
or a completed seven-method comparison.

## Prepared-data application

Create an RDS list with this structure using the dataset's preprocessing code:

```r
prepared <- list(
  Y = counts,                    # time x series, finite nonnegative integers
  graph = graph,                 # edges, A and W matching the series columns
  covariates = calendar_design,  # time x known covariates, including held-out times
  split = list(train = train_rows, validation = validation_rows, test = test_rows),
  control = list(n_iter = 5000L, burn_mcmc = 4000L, thin = 1L,
                 n_chains = 3L, parallel_chains = TRUE)
)
saveRDS(prepared, "prepared.rds")
```

The three windows must be consecutive, increasing and adjacent; training needs
at least three rows. Rows before training may supply lag context. The graph's
`edges` table must align with `Y` columns; `A` and `W` must be square matrices
of that same size. Optional `pre_history`, `pairs` and positive `means` are
forwarded to `gls_fit_general()`. Optional `control` contains named
`gls_control()` replacements and takes precedence over profile settings.
The package validates the full model input contract when fitting.

For the projected Divvy application, transfer
`bdynets_input_r1000_dt03h_n5000_b4000_thin1_c3.rds` to `$HOME/data/`. The prepared input
lives outside the source repository. For a different prepared RDS,
set `input` to its path. The following block starts a full fit after printing
its plan. The Divvy file specifies three chains with 5,000 iterations and
4,000 warmup iterations per chain. With `thin = 1`, each chain retains
iterations 4,001 through 5,000, for 3,000 retained draws in total.

```sh
(
  set -e
  input="$HOME/data/bdynets_input_r1000_dt03h_n5000_b4000_thin1_c3.rds"
  if [ ! -r "$input" ]; then
    printf 'Cannot read prepared input: %s\n' "$input" >&2
    exit 1
  fi
  mkdir -p "$HOME/bdynets-results"
  run_root="$(mktemp -d "$HOME/bdynets-results/gls-application.XXXXXX")"
  output="$run_root/run"
  export OPENBLAS_NUM_THREADS=1 OMP_NUM_THREADS=1 MKL_NUM_THREADS=1

  Rscript --vanilla GLS-paper/run_application.R --action=plan --profile=full \
    --cores=3 --input="$input" --output="$output"
  Rscript --vanilla GLS-paper/run_application.R --action=run --profile=full \
    --cores=3 --chunk-size=100 --input="$input" --output="$output"
  Rscript --vanilla GLS-paper/run_application.R --action=report --output="$output"
  printf 'GLS results: %s\n' "$output"
)
```

The application runner fits the training window, selects joint DSS on
validation, then obtains continuous and selected sequential one-step test
predictions through `predict()`. Validation observations are assimilated before
test prediction. Test observations are never used for DSS selection. No unknown
latent intensities or synthetic truth are supplied, and no intensity-recovery
score is reported.

The runner loads the retained draws once for DSS and both prediction sets.
`application_results.rds` saves the fit and its draw index alongside DSS,
predictions, controls, split, input MD5, package version and session
provenance. The posterior draw files remain in `draws/`. A failed run leaves
its configuration and failure record. Report mode uses the saved predictions
and draws to export prediction summaries, count intervals, observed counts,
diagnostics and DSS multipliers.

## Core budgets and other projects

`--cores` limits the number of chain workers. The application launcher supports
Windows socket workers with bdynets 0.2.1 or later and forked workers on Unix.
For three parallel chains, set `n_chains = 3L` and `parallel_chains = TRUE` in the
prepared input's `control` list and use `--cores=3` or higher. The plan shows the
worker count. The saved application records the actual worker process IDs.
Each worker writes to its own chain subdirectory under the fit's draw store.
When the requested budget cannot accommodate the parallel schedule, the
launchers run all declared chains serially. On Windows the simulation launcher
uses serial chains. Each worker inherits numeric-library thread settings.

The `graphMoDE` project and its launchers are unchanged. These files do not
launch new graphMoDE experiments. Shared scalar state-space helpers can be used
through `bdynets`; graphMoDE's existing multivariate-state FFBS remains its own
implementation.
