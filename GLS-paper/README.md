# GLS paper workflows

The installable [bdynets package](../bdynets) owns the samplers, graph construction, prediction, model fitting and draw storage. The code here defines the paper’s simulation designs, cleaning rules, comparison methods and reports. Run the commands below from the repository root.

| Block | Contents | Entry point |
| --- | --- | --- |
| R package | Reusable computation, disk-backed draws and sampler restart | [Package APIs and historical settings](../bdynets/GLS.md) |
| Experiments | Two fitting arms per study, 50 shared datasets per condition and paired comparisons | [Designs, methods and command-line options](experiments/README.md) |
| Applications | Clean raw records, visualize the count panel and fit models on chronological splits | [Divvy](applications/divvy/prepare.R) · [PeMS](applications/pems/prepare.R) · [analysis runner](applications/run.R) |
| Reports | One executable R Markdown walkthrough and one self-contained HTML per case | [Render a report from saved results](reports/render.R) |

## Cases for collaborator review

The three study reports combine GLS and competitor fits on the same synthetic datasets. The bundled HTMLs show archived execution checks with one replicate per implemented condition. The full studies are designed for 50 replicates per condition. Their reports give the number of matched panels and the Monte Carlo standard error for each estimable comparison.

| Case | Scientific question | Walkthrough | Rendered example |
| --- | --- | --- | --- |
| [1. Error in the supplied graph](study1-README.md) | How does forecast or recovery loss change when a known fraction of true PNAR edges is rewired? | [Rmd](reports/01-graph-error.Rmd) | [HTML](reports/01-graph-error.html) |
| [2. Sparse directed grid](study2-README.md) | How well do physical graph shrinkage, time variation and validation-selected sparsification recover lag effects? | [Rmd](reports/02-sparse-grid.Rmd) | [HTML](reports/02-sparse-grid.html) |
| [3. Upstream surges](study3-README.md) | Does the first observation of an upstream event improve the next downstream forecast when propagation is present? | [Rmd](reports/03-surges.Rmd) | [HTML](reports/03-surges.html) |
| Divvy | Forecast departures on six training-selected origin–destination links during the bundled Chicago day | [Rmd](reports/divvy.Rmd) | [HTML](reports/divvy.html) |
| PeMS | Forecast five-minute flow at four adjacent US-101 northbound stations during the bundled California day | [Rmd](reports/pems.Rmd) | [HTML](reports/pems.html) |

## Linux installation

Use R 4.1 or later and a C++17 toolchain. On Debian or Ubuntu, install the system prerequisites if needed.

```sh
sudo apt-get update &&
sudo apt-get install -y r-base r-base-dev build-essential pkg-config \
  libcurl4-openssl-dev libssl-dev ca-certificates fonts-dejavu-core
```

On a managed server, use the site's R and compiler environment. Install the package from this checkout into a personal R library. The command reads its required dependencies from [DESCRIPTION](../bdynets/DESCRIPTION).

```sh
export R_LIBS_USER="$HOME/.local/lib/bdynets"
mkdir -p "$R_LIBS_USER" &&
Rscript --vanilla -e '
  fields <- read.dcf("bdynets/DESCRIPTION", fields = c("Depends", "Imports", "LinkingTo"))
  entries <- unlist(strsplit(paste(fields[!is.na(fields)], collapse = ","), ","))
  deps <- setdiff(unique(trimws(sub("[(].*$", "", entries))), c("R", ""))
  missing <- deps[!vapply(deps, requireNamespace, logical(1), quietly = TRUE)]
  if (length(missing))
    install.packages(missing, lib = Sys.getenv("R_LIBS_USER"), repos = "https://cloud.r-project.org")
  stopifnot(all(vapply(deps, requireNamespace, logical(1), quietly = TRUE)))
' &&
R CMD INSTALL --preclean --clean --library="$R_LIBS_USER" ./bdynets &&
Rscript --vanilla -e 'library(bdynets); print(packageVersion("bdynets"))'
```

Repeat the `export R_LIBS_USER` line when starting a new terminal.

Install the R packages `rmarkdown` and `knitr` and make Pandoc available for report rendering. The Graph WaveNet benchmark also uses Python with NumPy and PyTorch. Set `BDYNETS_PYTHON` to that interpreter. For offline use, set `BDYNETS_GWNET_MODEL` to the [upstream model.py at the pinned commit](https://github.com/nnzhan/Graph-WaveNet/blob/6b162e80c59a1d494809252eca055cff93dc66b1/model.py). The adapter verifies its SHA-256 before fitting.

## Run the three experiments

Choose an absolute SSD directory for the shared panels, fits and draws. The full profile runs 50 replicates per condition. The example covers all 16 implemented conditions. Use `profile=smoke` with a separate output root for a one-replicate execution check. The [six study drivers](tools) default to a plan of the selected conditions and methods. The [experiment guide](experiments/README.md) documents the options.

```sh
profile=full
RESULTS="$HOME/bdynets-results/gls-$profile"
TOOLS=GLS-paper/tools
mkdir -p "$RESULTS"
export OPENBLAS_NUM_THREADS=1 OMP_NUM_THREADS=1 MKL_NUM_THREADS=1
for study in 1 2 3; do
  Rscript "$TOOLS/study${study}-gls.R" --action=data --profile="$profile" --output="$RESULTS" || break
  Rscript "$TOOLS/study${study}-gls.R" --action=run --profile="$profile" --output="$RESULTS" --cores=2 || break
  Rscript "$TOOLS/study${study}-competitors.R" --action=run --profile="$profile" --output="$RESULTS" || break
  Rscript "$TOOLS/study${study}-gls.R" --action=report --profile="$profile" --output="$RESULTS" || break
done
```

The collector records pending and failed jobs, cell-wise metrics, replicate-level contrasts and Monte Carlo uncertainty. The grid study also saves test deviance across the frozen validation path. These files let collaborators inspect both the model comparison and the effect of sparse selection.

## Run the applications

Both preparations read the repository’s raw example files, choose series using the training period, and write a prepared count panel and a count plot. Run the following for either `divvy` or `pems`. The default competitors are PNAR(1) and PNAR(2). The example also requests sa-PGSS and Graph WaveNet.

```sh
case_id=divvy
export RESULTS="$HOME/bdynets-results/gls-smoke"
mkdir -p "$RESULTS"
Rscript "GLS-paper/applications/$case_id/prepare.R" --output="$RESULTS/$case_id-prepared.rds"
Rscript GLS-paper/applications/run.R --action=run --profile=smoke --cores=2 \
  --benchmarks=pnar1,pnar2,sa-pgss,graph-wavenet --chunk-size=100 --checkpoint-every=100 \
  --input="$RESULTS/$case_id-prepared.rds" --output="$RESULTS/$case_id"
Rscript GLS-paper/applications/run.R --action=report --output="$RESULTS/$case_id"
Rscript GLS-paper/reports/render.R "$case_id" "$RESULTS/$case_id" "$RESULTS/reports"
```

Use `--action=plan` to inspect settings before fitting. To restart an interrupted application, repeat its fit command with `--action=resume` and the same inputs and settings. The application fits the training window, selects the DSS projection on validation, and compares sequential one-step forecasts on the test window. Compact outputs include cell-wise predictive summaries, diagnostics, selection and provenance. `--retention=draws` additionally saves predictive draws. The historical [application launcher](run_application.R) retains its original output convention.

## Prepared-data application

Provide an RDS list with the following fields. The names on the right refer to objects created by your preprocessing code.

```r
prepared <- list(
  Y = counts,                    # time by series, finite nonnegative integers
  graph = graph,                 # edges, A and W aligned with the columns of Y
  covariates = calendar_design,  # one row per time, including validation and test
  split = list(train = train_rows, validation = validation_rows, test = test_rows)
)
saveRDS(prepared, "prepared.rds")
```

The three windows must be consecutive and adjacent, with at least three training rows. Earlier rows can supply lag history. The launcher forwards optional `pre_history`, `pairs` and positive `means` to `gls_fit_general()`. Optional named settings in `prepared$control` override settings from the profile. For example, the earlier three-chain Divvy input `bdynets_input_r1000_dt03h_n5000_b4000_thin1_c3.rds` specifies 5,000 iterations, 4,000 warmup iterations and `thin = 1`.

To analyze a different prepared panel, pass its path to the application runner with `--input`. The case reports use the named case metadata added by their preparation scripts.

## SSD storage and workers

Dynamic general GLS fits support on-disk draw chunks, a checkpoint interval in **MCMC iterations** (including warmup), and restart from the last committed state. Chunk size controls the number of **retained draws** buffered per chain. Each worker has its own chain directory. Keep the complete run directory and its input for restart. For a selected subset of draws or fields, use `gls_read_general_draws()`. Diagnostics and some DSS/post-processing operations can still require substantial memory on large panels.

Set `--cores` to budget chain workers. A three-chain general GLS fit can use two workers, and `parallel_chains = FALSE` in the prepared control selects serial execution. Use one layer of parallelism at a time, or divide the available cores between simultaneous method jobs. General GLS supports Unix fork and Windows socket workers. The static comparison and older directed-grid launcher have their own worker settings. The result tables report the actual configuration and diagnostics for review.

## Directed-grid simulation

This smoke run generates its data, fits the simulation and rebuilds its report. Each invocation creates a fresh output directory.

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

`--profile=full` uses the settings from `gls_study_control("full")`. The simulation saves the study in `GLS_simulation_summary.rds`, together with draws, tables, figures, benchmark inputs and configuration. Report mode rebuilds the tables and figures from saved results. The historical Simulation 1 benchmark outputs and post-processing use separate archived projects.

The packaged two-hop example uses the same exported function as the original example script.

```sh
Rscript --vanilla -e 'x <- bdynets::gls_run_multihop_example(profile = "smoke", signal = 0.20, side = 3L); print(x$fit); print(x$dss); print(x$scores)'
```

## Divvy experiment archive

The Divvy manuscript table uses three-hour counts on 1,427 projected directed links. The recorded run used three parallel chains, 4,000 iterations per chain, 3,200 warmup, `thin = 1`, `fixed_tau = 0.02`, `affinity_model = "graph_laplace"` and `interweave_steps = 0L`. An expanded DSS search on validation selected 970 of 8,429 candidate relations.

For reproduction, obtain the separate `divvy simulation` workspace and the Simulation 1 benchmark sources. Inside the workspace the run is under `results_local_projection/graph_laplace_tau002_sparse_reuse_n4000_b3200_thin1_c3_20261117_235025/`. Keep the following together.

| Location in the external run | Purpose |
| --- | --- |
| `run_manifest.json`, `provenance/` | Input and package checksums, prepared data, package source archive, worktree patch and fit launcher |
| `fit/settings.rds`, `fit/fit_index.rds`, `fit/draws/` | Fit controls, chain index and all 2,400 retained draws |
| `prediction_evaluation_20261118/`, `dss_three_way_20261118/` | Evaluation settings, expanded DSS search, frozen selection and test forecasts |
| `simulation1_benchmarks_20261119/` | PNAR, sa-PGSS and neural benchmark artifacts, combined test metrics and paired comparisons |

The external workspace's `scripts/` directory contains the projection, DSS, scoring and benchmark drivers. The repository's [DivvyBike example](../examples/DivvyBike/README.md) illustrates data preparation for one day. For the reported 970-relation result, use the archived input, package version, scripts and evaluation protocol together.
