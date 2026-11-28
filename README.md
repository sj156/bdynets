# bdynets

`bdynets` is an R package for Bayesian network analysis. GLS estimates how counts
propagate between links, graphMODE groups series observed on a graph, and LPR
estimates latent node positions from counts observed on pairs of nodes.

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

- **Paper simulation.** Run the directed-grid experiment and its forecast benchmarks. [Simulation 1 instructions](GLS-paper/REPLICATION.md#directed-grid-simulation)
- **Paper application (Divvy).** The Chicago Divvy study uses three-hour counts on projected bike routes. Reproducing its results requires the archived inputs, fitted draws, and selection scripts. [Divvy replication guide](GLS-paper/REPLICATION.md#divvy-experiment-archive)
- **Paper application (PeMS).** TODO — link the traffic analysis when available.
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
