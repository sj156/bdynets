# Study 1 — Graph error

How much does GLS gain by admitting sources outside an inaccurate network map? For each underlying count panel we change only the graph supplied to the forecaster, so errors in the graph can be distinguished from changes in the observations.

## Design and methods

The full generator uses 20 count series. It draws a directed five-block graph, generates counts from a Poisson network autoregression (PNAR), and replaces true edges by degree-preserving swaps. Training uses 100 or 200 observations, followed by 100 for validation and 200 for testing. At each training length, the target fractions of displaced edges are 0%, 25% and 50%. The discrete swaps can give a different realized fraction.

| Driver | Fitted models |
| --- | --- |
| [GLS](tools/study1-gls.R) | `gls` admits all ordered pairs with distance-based shrinkage, `gls-restricted` admits only self and supplied neighbors, and `gls-flat` uses constant affinity. The flat fit can be reused across supplied maps for the same underlying panel. |
| [Competitors](tools/study1-competitors.R) | `pooled-pnar` fits the common own-lag and neighbor-average coefficients using the supplied graph. |

Six conditions × 50 replicate IDs give **300 condition–replicate combinations** and 1,200 scheduled method jobs. Graph-error conditions at the same training length share counts and splits within replicate. The manuscript's longer published reference case and four intermediate-error conditions are planned extensions.

## Run the study

Install the [paper environment](README.md#linux-installation) and run from the repository root. Each arm defaults to a plan. Choose absolute paths on your system for the shared synthetic data and the joint results directory.

```sh
TOOLS=GLS-paper/tools
DATA="$HOME/bdynets-data/full"
RESULTS="$HOME/bdynets-results/full"
Rscript "$TOOLS/study1-gls.R" --action=plan --profile=full
Rscript "$TOOLS/study1-competitors.R" --action=plan --profile=full

Rscript "$TOOLS/study1-gls.R" --action=data --profile=full --data-root="$DATA" --output="$RESULTS"
Rscript "$TOOLS/study1-gls.R" --action=run --profile=full --data-root="$DATA" --output="$RESULTS" --cores=2
Rscript "$TOOLS/study1-competitors.R" --action=run --profile=full --data-root="$DATA" --output="$RESULTS"
Rscript "$TOOLS/study1-gls.R" --action=report --profile=full --data-root="$DATA" --output="$RESULTS"
```

Generate the panels once. Both fitting arms can run on machines that can read the generated data and write to the same result store. Omit `--data-root` to keep inputs and fits under `--output`. The full profile uses IDs 1–50. For a one-ID execution check, use `--profile=smoke` throughout and choose separate data and output directories. To inspect one condition, add `--scenario=T100-error25` to data generation and both fits. The report action collects the entire saved study.

To resume an interrupted fit, repeat the same run command with the original output directory and sampler settings. The GLS driver resumes committed MCMC checkpoints by default. A completed method job is reused after its input and fit identities are checked. Use the same installed R and package environment on workers that share the result store.

## Read the results

Open `$RESULTS/reports/01-graph-error.html` for the joint report. The [R Markdown source](reports/01-graph-error.Rmd) and [archived one-replicate HTML](reports/01-graph-error.html) are available in the repository. The manifest under `$DATA/01-graph-error/panels.csv` identifies each panel by condition, replicate, seed and checksum. The following exports are under `$RESULTS/01-graph-error/`.

Start with test count deviance and the realized displacement recorded in `jobs.csv`. The joint report shows each method minus pooled PNAR. For the paper's primary contrast, use `comparison.csv` to subtract restricted-GLS deviance from all-pairs GLS deviance **within replicate** at each error level. Subtract the zero-error difference from the 50%-error difference and summarize separately at each training length. A negative change favors admitting sources outside the supplied graph as the map deteriorates.

Use `pairs-removed` recall and `pairs-inserted` selection rate to check which edges account for that gain. In the correct-graph condition these groups are empty. Their undefined rates should retain missing values.

Check `jobs.csv` and `failures.csv` for incomplete fits. In `paired.csv`, read `paired_n`, `finite_n` and `nonfinite_n` with each difference and its Monte Carlo standard error. Retain split R-hat and effective-sample-size flags from the dynamic GLS fits' `diagnostics.csv` files when presenting comparisons. The default monitoring thresholds are split R-hat ≤ 1.01 and ESS ≥ 400. These diagnostics describe within-fit sampling accuracy. The paired standard error describes variability across independent replicates.
