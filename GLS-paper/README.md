# GLS paper replication

GLS is a module of the `bdynets` package. This folder holds thin study and
prepared-data launchers; fitting, DSS, prediction, simulation and numerical
kernels are implemented in `bdynets`. Install that package from the repository
root before using these scripts:

```sh
R CMD INSTALL bdynets
```

Both launchers default to `--action=plan --profile=smoke --cores=12`. A plan
does not fit models, generate simulation data or create output files. `smoke`
and `quick` are software checks, not converged posterior analyses. Explicitly
select `--action=run` to fit. Store results in a new absolute directory outside
the repository, with an existing parent directory. An existing destination is
rejected for a new run; failed or completed runs are not overwritten or resumed.

## Directed-grid simulation

```sh
Rscript GLS-paper/run_simulation.R --action=plan --profile=smoke --cores=12
Rscript GLS-paper/run_simulation.R --action=run --profile=smoke --cores=12 \
  --output=/absolute/results/gls-simulation-smoke
Rscript GLS-paper/run_simulation.R --action=report \
  --output=/absolute/results/gls-simulation-smoke
```

Use `--profile=full` for the package's declared full study budget. The runner
calls `gls_study_control()` and `gls_run_grid_study()`, including the package's
GLS, selected GLS and PNAR comparisons and benchmark-data export. It preserves
the profile's chain count and all model settings. Saved outputs include
`GLS_simulation_summary.rds`, scenario checkpoints, tables, figures,
`configuration.R`, and launcher provenance. Report mode calls
`gls_write_grid_study()` on the saved study; it rebuilds these exports without
refitting or recomputing predictions. It can replace the study's own exported
tables and figures in that results directory.

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
  control = list(n_iter = 1500L, burn_mcmc = 750L, n_chains = 3L)
)
saveRDS(prepared, "/absolute/data/prepared.rds")
```

The three windows must be consecutive, increasing and adjacent; training needs
at least three rows. Rows before training may supply lag context. The graph's
`edges` table must align with `Y` columns; `A` and `W` must be square matrices
of that same size. Optional `pre_history`, `pairs` and positive `means` are
forwarded to `gls_fit_general()`. Optional `control` contains named
`gls_control()` replacements and takes precedence over profile settings.
The package validates the full model input contract when fitting.

```sh
Rscript GLS-paper/run_application.R --action=plan --profile=full --cores=12 \
  --input=/absolute/data/prepared.rds
Rscript GLS-paper/run_application.R --action=run --profile=full --cores=12 \
  --input=prepared.rds --data-root=/absolute/data \
  --output=/absolute/results/gls-application
Rscript GLS-paper/run_application.R --action=report \
  --output=/absolute/results/gls-application
```

The application runner fits the training window, selects joint DSS on
validation, then obtains continuous and selected sequential one-step test
predictions through `predict()`. Validation observations are assimilated before
test prediction. Test observations are never used for DSS selection. No unknown
latent intensities or synthetic truth are supplied, and no intensity-recovery
score is reported.

`application_results.rds` retains the fit, DSS, predictions, controls, split,
input MD5, package version and session provenance. The full fit is intentionally
retained for subsequent diagnostics and saved-result reporting. A failed run
leaves its configuration and failure record. Report mode reads the saved object
and exports prediction summaries, count intervals, observed counts, diagnostics
and DSS multipliers; it never fits, predicts or cleans up stored results.

## Core budgets and other projects

`--cores` limits the number of chain workers. The application launcher supports
Windows socket workers with bdynets 0.2.1 or later and forked workers on Unix.
For three parallel chains, set `n_chains = 3L` and `parallel_chains = TRUE` in the
prepared input's `control` list and use `--cores=3` or higher. The plan shows the
worker count. The saved application records the actual worker process IDs.
When the requested budget cannot accommodate the parallel schedule, the
launchers run all declared chains serially. On Windows the simulation launcher
uses serial chains. Each worker inherits numeric-library thread settings.

The `graphMoDE` project and its launchers are unchanged. These files do not
launch new graphMoDE experiments. Shared scalar state-space helpers can be used
through `bdynets`; graphMoDE's existing multivariate-state FFBS remains its own
implementation.
