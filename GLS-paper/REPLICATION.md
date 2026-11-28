# Reproduce the GLS analyses

Run these commands from the repository root after [installing the checkout](README.md#install-from-github).
The shell examples use Linux or macOS paths. Use an absolute path with an existing
parent, outside the checkout, for every output directory. Choose a new directory
for each fit and use the same directory to rebuild its reports.

## Environment

Use R 4.1 or later with C and C++17 build tools. On a Debian or Ubuntu machine
where you manage the system packages, these commands install the prerequisites.

```sh
sudo apt-get update
sudo apt-get install -y r-base r-base-dev build-essential pkg-config libcurl4-openssl-dev libssl-dev ca-certificates fonts-dejavu-core
```

Use Rtools matching your R release on Windows or the Xcode command line tools on
macOS. On a managed cluster, load the site's R and compiler environment first.
The [setup script](setup_environment.R) reads the package dependencies from
[DESCRIPTION](../bdynets/DESCRIPTION). To call it from R, run the following from
the repository root.

```r
source("GLS-paper/setup_environment.R")
setup_gls_environment()
```

For the 50-replicate comparisons, see the guides to [graph error](study1-README.md),
[coefficient recovery](study2-README.md) and [upstream surges](study3-README.md).
The directed-grid launcher below reproduces the original manuscript illustration.

## Directed-grid simulation

The [simulation launcher](run_simulation.R) uses the package's
[directed-grid data generator](../bdynets/R/gls-simulation.R) and
[study functions](../bdynets/R/gls-study-api.R). The full profile uses the
settings in `gls_study_control("full")`. These commands preview the workload,
fit the study and rebuild its tables and figures from the saved draws.

```sh
Rscript --vanilla GLS-paper/run_simulation.R --action=plan --profile=full --cores=3
Rscript --vanilla GLS-paper/run_simulation.R --action=run --profile=full --cores=3 --output="$HOME/gls-grid-full"
Rscript --vanilla GLS-paper/run_simulation.R --action=report --output="$HOME/gls-grid-full"
```

The study saves `GLS_simulation_summary.rds`, the configuration, posterior draws,
tables, figures and benchmark inputs. Begin with `tables/predictive_performance.csv`
for test accuracy and `tables/pair_selection.csv` for recovery of the simulated
relationships. The launcher includes the PNAR comparisons. The sa-PGSS, STGCN
and Graph WaveNet fits require the separate Simulation 1 benchmark sources.
See the [model and benchmark settings](../bdynets/GLS.md).

## Prepared-data application

**Try the general fitting workflow on example data.** This generator uses the
same eight-link example as the README and writes only the inputs available to
an analyst. The application uses `gls_fit_general()` with all 64 ordered link
pairs on this grid, including self and one-hop relationships. It selects
relationships on validation and makes sequential one-step-ahead predictions
on the test window.

```sh
Rscript --vanilla GLS-paper/data/generate_example_data.R "$HOME/gls-example.rds"
Rscript --vanilla GLS-paper/run_application.R --action=run --profile=smoke --cores=1 --input="$HOME/gls-example.rds" --output="$HOME/gls-example-fit"
Rscript --vanilla GLS-paper/run_application.R --action=report --output="$HOME/gls-example-fit"
```

Open `predictions.csv` for the observed counts, mean intensities and 95% count
intervals from the continuous and DSS fits. `DSS_selection.csv` contains the
multipliers, and `MCMC_diagnostics.csv` contains the chain diagnostics.
`application_results.rds` holds the fit index, selections, predictions and
provenance. You can also inspect the example input directly in R.

```r
source("GLS-paper/data/generate_example_data.R")
prepared <- generate_gls_example_data()
str(prepared, max.level = 1)
```

**Supply your own observations.** Save a named list as an RDS file. Each
column of `Y` must correspond to the same directed link in the graph.

| Field | Contents |
| --- | --- |
| `Y` | Time-by-link matrix of finite, nonnegative integer counts. |
| `graph` | List with `edges`, adjacency `A` and weights `W` in the same link order as `Y`. |
| `covariates` | Finite matrix of known predictors, one row per time point, including validation and test. |
| `split` | Lists of integer row indices named `train`, `validation` and `test`. Use consecutive, adjacent windows and at least three training rows. |
| Optional | `pre_history` for counts preceding `Y`, `pairs` for the candidate relationships, `means` for positive prior centering means and `control` for named `gls_control()` overrides. |

With the data at `$HOME/data/prepared.rds`, inspect the effective settings
before starting a full fit. Input control settings override profile defaults.

```sh
Rscript --vanilla GLS-paper/run_application.R --action=plan --profile=full --cores=3 --input="$HOME/data/prepared.rds"
Rscript --vanilla GLS-paper/run_application.R --action=run --profile=full --cores=3 --input="$HOME/data/prepared.rds" --output="$HOME/gls-observed-fit"
Rscript --vanilla GLS-paper/run_application.R --action=report --output="$HOME/gls-observed-fit"
```

The general application uses the package's default DSS candidate search.
The manuscript used the archived expanded search described below.

## Divvy experiment archive

The Divvy manuscript used three-hour counts on 1,427 projected directed links.
An expanded validation search selected 970 of 8,429 candidate relationships.
The run used three parallel chains with 4,000 iterations per chain, 3,200
warmup iterations, `thin = 1`, `fixed_tau = 0.02`,
`affinity_model = "graph_laplace"` and `interweave_steps = 0L`.

Reproducing that table requires the separate `divvy simulation` workspace and
the Simulation 1 benchmark sources. Within that workspace, the run is at
`results_local_projection/graph_laplace_tau002_sparse_reuse_n4000_b3200_thin1_c3_20261117_235025/`.

| Location in the external run | Contents |
| --- | --- |
| `run_manifest.json`, `provenance/` | Prepared input, checksums, package source archive, worktree patch and fitting script |
| `fit/settings.rds`, `fit/fit_index.rds`, `fit/draws/` | Controls, chain index and all 2,400 retained draws |
| `prediction_evaluation_20261118/`, `dss_three_way_20261118/` | Evaluation settings, expanded DSS search, selected relationships and test forecasts |
| `simulation1_benchmarks_20261119/` | PNAR, sa-PGSS and neural benchmark results, combined test metrics and paired comparisons |

The external workspace's `scripts/` directory contains the projection, DSS,
scoring and benchmark scripts. The [one-day DivvyBike example](../examples/DivvyBike/README.md)
illustrates how to prepare station trips.

## Workers and saved draws

The historical grid uses the [launcher helpers](R/launcher_helpers.R) for
options, output paths, workers and provenance. The prepared-data launcher uses
the [application workflow](applications/run.R), which can also resume an
interrupted fit with its original input, output and controls. Both default to `--action=plan`,
`--profile=smoke`, `--cores=12` and `--chunk-size=100`. Use `smoke` or `quick`
to check the workflow and `full` for the study settings.

For a three-chain application, put `n_chains = 3L` and `parallel_chains = TRUE`
in the prepared input's `control` list and allow `--cores=3` on a machine with
three available physical cores. Application chains use fork workers on Unix or
socket workers on Windows. The application caps workers at the number of
chains, available physical cores and the requested core budget. The historical
grid runs serially on Windows or when the requested budget is below its planned
worker count. On a Linux server, this command gives each
worker one BLAS thread when run in the same terminal before launching the fit.

```sh
export OPENBLAS_NUM_THREADS=1 OMP_NUM_THREADS=1 MKL_NUM_THREADS=1
```

Each chain writes chunks of retained samples under `draws/`. DSS, prediction
and diagnostics can load the full retained history and additional prediction
arrays into memory. When moving a run, copy the entire output directory so
the index can find the draws. Report mode also reads earlier fits with draws
stored inside the fit object.
