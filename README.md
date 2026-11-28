# bdynets

`bdynets` is an R package for Bayesian analysis of time series observed on a network. Its GLS model asks which links help forecast counts on another link one time step later. It uses the physical graph to guide shrinkage and allows the strength of each relationship to change over time. The package also fits graph-informed mixtures for clustering series with similar patterns.

## Install from GitHub

Use R 4.1 or later with a C++17 compiler. The revision below installs bdynets 0.3.0, the version used in this walkthrough.

```r
if (!requireNamespace("remotes", quietly = TRUE)) install.packages("remotes")
remotes::install_github("sj156/bdynets", subdir = "bdynets", ref = "d8e936984a")
library(bdynets)
```

If you have cloned this revision, you can install the local package with `remotes::install_local("bdynets")` from its root. [Linux installation guide](GLS-paper/README.md#linux-installation)

## Which road links help forecast downstream traffic?

Suppose vehicles travel along three consecutive road links, A → B → C → D. In each time bin, the mean count on AB is randomly set to 5 or 25 vehicles. We simulate BC using the previous count on AB, and CD using the previous count on BC. Those two known relationships let us check which ones the model selects.

```r
edges <- data.frame(
  name = c("AB", "BC", "CD"),
  from = c("A", "B", "C"), to = c("B", "C", "D")
)
graph <- gls_graph(edges)

set.seed(42)
n <- 72L
Y <- matrix(0L, nrow = n, ncol = 3L, dimnames = list(NULL, edges$name))
Y[1, ] <- c(10L, 8L, 6L)
for (t in 2:n) {
  rates <- c(sample(c(5, 25), 1),
             2 + 0.8 * Y[t - 1, "AB"],
             2 + 0.8 * Y[t - 1, "BC"])
  Y[t, ] <- rpois(3, rates)
}
```

**Fit and select the network.** Each row of `Y` is a time bin, and its columns follow the order of `edges`. Fit GLS on the first 48 bins and use the next 12 to select a sparse set of lagged predictors by decoupled shrinkage and selection (DSS). The model considers all nine ordered pairs, including each link’s own previous count.

```r
split <- gls_split_rows(n, train_end = 48L, validation_end = 60L)
Q <- matrix(numeric(), nrow = n, ncol = 0L)  # No calendar predictors here
fit <- gls_fit_general(Y, graph, covariates = Q, train_rows = split$train,
                       control = gls_control("smoke"))
dss <- gls_dss(fit, validation_rows = split$validation)
fit$pairs$pair[dss$selected]
```

In this seeded run, DSS retains both simulated relationships, `AB -> BC` and `BC -> CD`. It also retains `AB -> AB`, an extra self-lag.

**Forecast the next count.** Condition on the validation history, then forecast the final 12 bins one step at a time. Each observed test count becomes available only for later forecasts. The table shows the first test bin, with a predictive interval for each link’s vehicle count.

```r
pred <- predict(fit, rows = split$test, conditioning_rows = split$validation,
                d = dss$d, label = "DSS")
forecast <- gls_forecast_summary(pred, Y)
forecast$link <- edges$name[forecast$series]
forecast[forecast$row == split$test[1],
         c("link", "observed", "mean_intensity", "count_lower", "count_upper")]
```

For BC, the forecast mean is 25.1 vehicles and the 95% count interval is 20–34. The observed count is 26. The example uses short chains. For an analysis, check `gls_diagnostics(fit)` for mixing and effective sample size. For longer runs, `gls_fit_general()` supports periodic checkpoints, disk-backed draws and a limit on parallel chain workers. [GLS fitting guide](bdynets/GLS.md)

## Reproduce the GLS paper workflows

For each condition, the GLS and competitor drivers use the same 50 synthetic datasets. A shared generator saves the panel and split once for each replicate. Each arm reads these inputs, and the study report compares their forecasts on matched panels. Running a driver without arguments prints its plan. The following commands run a short check on one graph-error condition. Set `RESULTS` to an SSD directory outside the checkout. HTML rendering requires `rmarkdown`, `knitr` and Pandoc.

```sh
RESULTS="$HOME/bdynets-results/readme-smoke"
TOOLS=GLS-paper/tools
Rscript "$TOOLS/study1-gls.R" --action=data --profile=smoke --scenario=T100-error25 --output="$RESULTS"
Rscript "$TOOLS/study1-gls.R" --action=run --profile=smoke --scenario=T100-error25 --output="$RESULTS"
Rscript "$TOOLS/study1-competitors.R" --action=run --profile=smoke --scenario=T100-error25 --output="$RESULTS"
Rscript "$TOOLS/study1-gls.R" --action=report --profile=smoke --output="$RESULTS"
```

Open `$RESULTS/reports/01-graph-error.html` for the joint report. The [experiment guide](GLS-paper/experiments/README.md) lists all six drivers, their methods and the full 50-replicate command.

| Walkthrough | Question and data |
| --- | --- |
| [Experiment 1 · Graph error](GLS-paper/study1-README.md) | How do forecasts and link recovery change as true edges are removed and false edges inserted? |
| [Experiment 2 · Sparse dynamic grid](GLS-paper/study2-README.md) | Which signed, time-varying lag effects can GLS recover, and how does test error change as the selected network gets smaller? |
| [Experiment 3 · Upstream surges](GLS-paper/study3-README.md) | Does observing an upstream surge improve the next downstream forecast when a transmission effect is present? |
| [Divvy · Chicago bike departures](GLS-paper/reports/divvy.Rmd) | Forecast one day of departures on six origin–destination links. The six-month projected-link analysis uses a [separate archive](GLS-paper/README.md#divvy-experiment-archive). |
| [PeMS · California freeway counts](GLS-paper/reports/pems.Rmd) | Forecast five-minute vehicle counts at four adjacent northbound US-101 stations. |

The [paper workflow guide](GLS-paper/README.md#cases-for-collaborator-review) pairs each walkthrough with its rendered HTML and links the data-preparation scripts. The [original directed-grid study](GLS-paper/README.md#directed-grid-simulation) has its own replication instructions.

## Other analyses in bdynets

| Task | Where to start |
| --- | --- |
| Cluster network series with graphMoDE | [Gaussian example](bdynets/README.md#gaussianstatic-graphmode-example) · [Dynamic count models and eight-method comparison](graphMoDE/README.md) |
| Fit a Poisson latent-position model | [GP-LPR pilot and figure reproduction](GP-LPR/README.md) |
| Map traffic flow, speed or origin–destination trips | [Five traffic examples and their data](examples/README.md) · [Prepared input format](bdynets/inst/INPUT-FORMAT.md) |
| Reuse Gaussian and Poisson state-space kernels | [R functions and C++ interface](bdynets/CPP-KERNELS.md) |

See the [package guide](bdynets/README.md) for the full set of modules and development tests, and the [migration record](MIGRATION.md) for historical samplers.
