# bdynets

`bdynets` is an R package for Bayesian network analysis. GLS estimates how counts
propagate between links, graphMODE groups series observed on a graph, and LPR
estimates latent node positions from counts observed on pairs of nodes.

<a id="install-from-github"></a>

## Install

Source installation requires R 4.1 or later and compilers for C and C++17.
Run the following in R to install from GitHub. If you already have a checkout,
use `pak::pak("./bdynets")` from the repository root in place of the GitHub command.

```r
if (!requireNamespace("pak", quietly = TRUE)) install.packages("pak")
pak::pak("sj156/bdynets/bdynets")
library(bdynets)
```

For a smaller download that includes the package and the GLS paper scripts, see
the [sparse checkout instructions](GLS-paper/README.md#install-from-github).
Each demo below uses only the installed package and runs short chains to
illustrate the workflow.

## GLS

GLS estimates which links' past counts help predict the next count on a given
link. This demo simulates eight directed links around a square, fits GLS, and
uses a validation window to select a sparse set of lagged relationships (GLSS).
Compare the two models on held-out counts using mean absolute error and
95% predictive interval coverage.

```r
demo <- gls_run_multihop_example("smoke", side = 2L)
demo$scores[, c("Model", "MAECount", "Coverage95")]
```

<details>
<summary>Worked example: forecast downstream traffic on three links</summary>

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

</details>

Each replicated study compares GLS and its competitors on the same 50 saved
datasets per condition. Use a driver with no arguments to inspect the default
plan, then follow the study guide to generate the panels once, fit the two
method groups, and read their joint HTML report.

| Replicated study | Question |
| --- | --- |
| [Study 1 · Graph error](GLS-paper/study1-README.md) | How do forecasts and link recovery change when the supplied graph is inaccurate? |
| [Study 2 · Sparse directed grid](GLS-paper/study2-README.md) | Which signed, time-varying lag effects can GLS recover? |
| [Study 3 · Upstream surges](GLS-paper/study3-README.md) | When does an upstream count improve the next downstream forecast? |

- **Directed-grid illustration.** Run the directed-grid experiment and its forecast benchmarks. [Simulation 1 instructions](GLS-paper/REPLICATION.md#directed-grid-simulation)
- **Paper application (Divvy).** The Chicago Divvy study uses three-hour counts on projected bike routes. Reproducing its results requires the archived inputs, fitted draws, and selection scripts. [Divvy replication guide](GLS-paper/REPLICATION.md#divvy-experiment-archive)
- **Traffic application walkthroughs.** Forecast Chicago bike departures or California freeway counts from one day of source records and compare models on a held-out window. [Divvy report](GLS-paper/reports/divvy.Rmd) · [PeMS report](GLS-paper/reports/pems.Rmd) · [Preparation scripts and rendered reports](GLS-paper/README.md#cases-for-collaborator-review)
- **Use your own data.** Fit GLS to a count matrix, graph, and covariates. [Prepared-data example](GLS-paper/REPLICATION.md#prepared-data-application)

## graphMODE

graphMODE uses the graph to inform which series share a trajectory. In this
Gaussian example, sites A, B, and C form a path. The first two series fall
after the second observation while the third rises. The final matrix shows the
fraction of retained draws in which each pair shares a group.

```r
Y <- rbind(A = c(1.0, 0.8, -1.0, -0.8),
           B = c(0.9, 1.1, -0.9, -1.1),
           C = c(-1.0, -0.8, 1.0, 0.8))
W <- rbind(c(0, 1, 0), c(1, 0, 1), c(0, 1, 0))
graph <- gmde_graph(W, rownames(Y))
design <- cbind(intercept = 1, contrast = c(1, 1, -1, -1))
expert <- gmde_expert("gaussian", design, prior = list(
  m0 = c(0, 0), C0 = diag(c(25, 25)), variance = list(shape = 2, scale = 1)
))
fit <- gmde_fit(Y, graph, K = 2, expert = expert, mcmc = gmde_mcmc(seed = 1))
gmde_partition(fit)$psm
```

- **Paper simulation.** The dynamic Poisson study compares eight methods on two synthetic road networks using group recovery and 48-step forecasts. [Study scripts, results, and chain diagnostics](graphMoDE/README.md)
- **Paper application.** TODO — link the real-data application when available.

## LPR

The Poisson latent-position model relates the expected count on a pair of
nodes to their endpoint effects, the inner product of their positions, and
exposure. This demo simulates counts for eight nodes at two exposure levels.
For each level, it aligns the estimated positions with the generating
positions and reports the root mean squared coordinate error.

```r
study <- lpr_run_study(tempfile(fileext = ".rds"), profile = "quick")
sapply(study$cases, function(x) lpr_recovery(x$fits, x$truth)$position_rmse)
```

- **Paper simulation.** The manuscript figure compares position recovery at exposures 1 and 10. The study guide explains how to run the R pilot and how to regenerate the figure from the archived draws. [Study guide](GP-LPR/README.md) · [Manuscript figure](GP-LPR/reports/aligned_positions.pdf)
- **Paper application.** TODO — link the planned PeMS freeway accident analysis when available.

## Traffic data, numerical kernels and package development

- Prepare and map traffic flow, speed and origin–destination trips. [Worked traffic examples](examples/README.md) · [Prepared input format](bdynets/inst/INPUT-FORMAT.md)
- Reuse the scalar Gaussian state-space sampler and Poisson filtering kernels from R or C++. [Numerical kernel guide](bdynets/CPP-KERNELS.md)
- Build the package, run its tests, and locate historical samplers. [Package guide](bdynets/README.md) · [Migration record](MIGRATION.md)
