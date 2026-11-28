# GLS for dynamic count networks

The GLS functions in `bdynets` model how earlier counts on one directed link
help predict later counts on another. The continuous GLS fit estimates these
relationships. Its sparse summary (GLSS) uses decoupled shrinkage and selection
(DSS) on a validation window to select relationships for forecasting.

## Install from GitHub

<a id="linux-installation"></a>

Use R 4.1 or later and the [C and C++17 build tools](REPLICATION.md#environment).
These terminal commands download the package and paper scripts, skip the large
datasets for other studies, and install `bdynets` in your R user library.

```sh
git clone --depth 1 --filter=blob:none --sparse https://github.com/sj156/bdynets.git
cd bdynets
git sparse-checkout set bdynets GLS-paper
Rscript --vanilla GLS-paper/setup_environment.R
```

If you downloaded the [full repository ZIP](https://github.com/sj156/bdynets/archive/refs/heads/main.zip),
unpack it, open a terminal in the directory containing `GLS-paper/`, and run
just the last command. Then load the package in R.

```r
library(bdynets)
```

<details>
<summary>Install directly from an R session with remotes</summary>

This route downloads the entire repository archive, including the datasets for
other studies. Use a longer timeout on slower connections.

```r
options(timeout = max(3600, getOption("timeout")))
if (!requireNamespace("remotes", quietly = TRUE)) install.packages("remotes", repos = "https://cloud.r-project.org")
remotes::install_github("sj156/bdynets", subdir = "bdynets", upgrade = "never")
library(bdynets)
```

</details>

## Fit a network, select relationships, and predict counts

Imagine four intersections connected in a square. Each direction on each street
has its own count series, giving eight directed links. This example simulates
160 time points with two injected two-hop effects. It uses the first 106 time
points for fitting, the next 26 for selection, and the final 28 for forecasting.
It runs using the installed package alone.

**Fit the network.** Run the example with short chains and inspect the fitted
model. The fit summary prints the number of candidate relationships and saved draws.

```r
demo <- gls_run_multihop_example("smoke", side = 2L)
print(demo$fit)
```

**Inspect the sparse selection.** This example models self and one-hop
effects separately, leaving 48 candidate relationships for DSS. The table
gives the source and target link for each relationship retained on validation.

```r
keep <- demo$dss$selected
demo$fit$pairs[keep, c("source", "target", "pair"), drop = FALSE]
```

**Compare test forecasts.** The two rows compare continuous GLS and sparse GLSS.
`MAECount` measures absolute error in counts. `Coverage95` is the fraction of
held-out counts inside the model's 95% predictive interval.

```r
demo$scores[, c("Model", "MAECount", "Coverage95")]
```

The short chains are for learning the workflow. For inference, use longer
chains and examine the diagnostics for the parameters you report.

## Check the paper environment

From the [downloaded checkout](#install-from-github), check that R can load
the installed package, its compiled numerical code and the interfaces the
paper scripts use.

```sh
Rscript --vanilla GLS-paper/setup_environment.R --check
```

Rendering the case reports requires the R packages `rmarkdown` and `knitr` and
an installation of Pandoc. For the Graph WaveNet comparison, point
`BDYNETS_PYTHON` to a Python environment with NumPy and PyTorch. For offline
use, set `BDYNETS_GWNET_MODEL` to the checksum-verified
[upstream model.py](https://github.com/nnzhan/Graph-WaveNet/blob/6b162e80c59a1d494809252eca055cff93dc66b1/model.py).

## Cases for collaborator review

The three study reports combine GLS and competitor fits on the same synthetic datasets. The bundled HTMLs show archived execution checks with one replicate per implemented condition. The full studies are designed for 50 replicates per condition. Their reports give the number of matched panels and the Monte Carlo standard error for each estimable comparison.

| Case | Scientific question | Walkthrough | Rendered example |
| --- | --- | --- | --- |
| [1. Error in the supplied graph](study1-README.md) | How does forecast or recovery loss change when a known fraction of true PNAR edges is rewired? | [Rmd](reports/01-graph-error.Rmd) | [HTML](reports/01-graph-error.html) |
| [2. Sparse directed grid](study2-README.md) | How well do physical graph shrinkage, time variation and validation-selected sparsification recover lag effects? | [Rmd](reports/02-sparse-grid.Rmd) | [HTML](reports/02-sparse-grid.html) |
| [3. Upstream surges](study3-README.md) | Does the first observation of an upstream event improve the next downstream forecast when propagation is present? | [Rmd](reports/03-surges.Rmd) | [HTML](reports/03-surges.html) |
| Divvy | Forecast departures on six training-selected origin–destination links during the bundled Chicago day | [Rmd](reports/divvy.Rmd) | [HTML](reports/divvy.html) |
| PeMS | Forecast five-minute flow at four adjacent US-101 northbound stations during the bundled California day | [Rmd](reports/pems.Rmd) | [HTML](reports/pems.html) |

## Run the three experiments

The six [study drivers](tools) share the package's fitting, forecasting and scoring functions. Running either arm without arguments prints its plan. Use one arm to generate the data, then run both arms on the saved panels. Either arm can collect their paired scores and render one joint report. The example runs one short graph-error condition from the repository root.

```sh
RESULTS="$HOME/bdynets-results/graph-error-smoke"
TOOLS=GLS-paper/tools
Rscript "$TOOLS/study1-gls.R" --action=data --profile=smoke --scenario=T100-error25 --output="$RESULTS"
Rscript "$TOOLS/study1-gls.R" --action=run --profile=smoke --scenario=T100-error25 --output="$RESULTS"
Rscript "$TOOLS/study1-competitors.R" --action=run --profile=smoke --scenario=T100-error25 --output="$RESULTS"
Rscript "$TOOLS/study1-gls.R" --action=report --profile=smoke --output="$RESULTS"
```

Open `$RESULTS/reports/01-graph-error.html`. In the full profile each arm defaults to replicate IDs 1–50. For a separate frozen dataset directory, pass the same `--data-root` to all commands. The study guides above give the complete runs and outputs. [Driver options and scenarios](experiments/README.md)

## Run the applications

For a sparse checkout, add the input directories for the cases you want to run. The full repository archive already contains them.

```sh
git sparse-checkout add examples/DivvyBike/data/raw examples/pems-flow/data/raw
```

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

## SSD storage and workers

Dynamic general GLS fits support on-disk draw chunks, a checkpoint interval in **MCMC iterations** (including warmup), and restart from the last committed state. Chunk size controls the number of **retained draws** buffered per chain. Each worker has its own chain directory. Keep the complete run directory and its input for restart. For a selected subset of draws or fields, use `gls_read_general_draws()`. Diagnostics and some DSS/post-processing operations can still require substantial memory on large panels.

Set `--cores` to budget chain workers. A three-chain general GLS fit can use two workers, and `parallel_chains = FALSE` in the prepared control selects serial execution. Use one layer of parallelism at a time, or divide the available cores between simultaneous method jobs. General GLS supports Unix fork and Windows socket workers. The static comparison and older directed-grid launcher have their own worker settings. The result tables report the actual configuration and diagnostics for review.

## Original directed-grid and prepared-data workflows

<a id="directed-grid-simulation"></a>

**Run a small version of Simulation 1.** Preview its settings, then fit the
directed-grid study. The run creates a new output directory, including
`tables/predictive_performance.csv`. Use a fresh output name for a second fit.

```sh
Rscript --vanilla GLS-paper/run_simulation.R --action=plan --profile=smoke --cores=1
Rscript --vanilla GLS-paper/run_simulation.R --action=run --profile=smoke --cores=1 --output="$HOME/gls-grid-smoke"
```

<a id="prepared-data-application"></a>
<a id="divvy-experiment-archive"></a>

| Analysis | Inputs |
| --- | --- |
| [Simulation 1 on a directed grid](REPLICATION.md#directed-grid-simulation) | The launcher generates counts and fits GLS, GLSS and the PNAR comparisons. |
| [Fit GLS to prepared counts](REPLICATION.md#prepared-data-application) | Use the example data generator or supply your own counts, graph, covariates and time split. |
| [Reproduce the Divvy manuscript result](REPLICATION.md#divvy-experiment-archive) | Requires the external projected Divvy data, archived fit and expanded DSS search that selected 970 relationships. |

The [general GLS functions](../bdynets/R/gls-general-api.R) and [compiled kernels](../bdynets/src)
implement the models. [Data generation](data/generate_example_data.R) creates
a prepared example without fitting. The [simulation](run_simulation.R) and
[prepared-data](run_application.R) scripts call the package and save results.
