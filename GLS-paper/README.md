# GLS for dynamic count networks

The GLS functions in `bdynets` model how earlier counts on one directed link
help predict later counts on another. The continuous GLS fit estimates these
relationships. Its sparse summary (GLSS) uses decoupled shrinkage and selection
(DSS) on a validation window to select relationships for forecasting.

## Install from GitHub

With R 4.1 or later and the build tools for C and C++17, run the following in R.
The installable package is in the `bdynets` subdirectory. GitHub sends the entire
repository archive to `remotes`, including other studies' data, so allow time
for the download. The [terminal installation below](#reproduce-the-paper)
downloads just the files needed for GLS.

```r
options(timeout = max(900, getOption("timeout")))
if (!requireNamespace("remotes", quietly = TRUE)) install.packages("remotes", repos = "https://cloud.r-project.org")
remotes::install_github("sj156/bdynets", subdir = "bdynets", upgrade = "never")
library(bdynets)
```

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

## Reproduce the paper

<a id="linux-installation"></a>

For the package and paper scripts together, use a sparse checkout in a
terminal. This skips the large datasets for the other studies. You can also
download and unpack the [full repository ZIP](https://github.com/sj156/bdynets/archive/refs/heads/main.zip).

```sh
git clone --depth 1 --filter=blob:none --sparse https://github.com/sj156/bdynets.git
cd bdynets
git sparse-checkout set bdynets GLS-paper
```

From the directory containing `GLS-paper/`, run the following in a terminal.
The first command installs dependencies and builds this checkout in your R user
library. The second checks the installation. See the
[platform prerequisites](REPLICATION.md#environment) if you need a compiler.

```sh
Rscript --vanilla GLS-paper/setup_environment.R
Rscript --vanilla GLS-paper/setup_environment.R --check
```

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
