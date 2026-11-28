# Study 2 — Sparse directed grid

Can GLS recover the size and timing of a signed lag effect when its source is distant or its coefficient changes over time? A known generating path lets us distinguish accurate forecasts from accurate coefficient estimates.

## Design and methods

A 24-link directed grid gives 576 ordered pairs. The 1,200 training bins are followed by 300 validation bins and 300 test bins. Each non-null panel has 24 self effects and 20 active cross-link effects. The moderate cells cross local or mixed-distance support with static or sinusoidal effects at signal 0.10. A seasonal null and a stronger mixed-dynamic cell at 0.20 complete the six conditions. Signal is the RMS combined lag contribution to log intensity.

Separate pilot draws calibrate signal. If the initial scale is unstable or misses its RMS target by over 2%, the generator brackets and bisects using the same pilot innovations. The scale is fixed before the evaluation counts are drawn. Each saved panel records its realized RMS.

| Driver | Fitted models |
| --- | --- |
| [GLS](tools/study2-gls.R) | `gls` fits dynamic coefficients with distance-based shrinkage. `gls-static` uses constant coefficients with the same distance weights, and `gls-flat` fits the dynamic model with constant affinity across pairs. |
| [Competitors](tools/study2-competitors.R) | `poisson-lasso` applies an absolute-value penalty to lag coefficients, leaves the calendar and intercept unpenalized, and tunes the penalty on validation. |

Six conditions × 50 replicate IDs give **300 panels** and 1,200 scheduled method jobs. The manuscript's two intermediate mixed-dynamic signals, 0.05 and 0.15, are planned extensions.

## Run the study

Install the [paper environment](README.md#linux-installation) and run from the repository root. Each arm defaults to a plan. Choose absolute paths on your system for the shared synthetic data and the joint results directory.

```sh
TOOLS=GLS-paper/tools
DATA="$HOME/bdynets-data/full"
RESULTS="$HOME/bdynets-results/full"
Rscript "$TOOLS/study2-gls.R" --action=plan --profile=full
Rscript "$TOOLS/study2-competitors.R" --action=plan --profile=full

Rscript "$TOOLS/study2-gls.R" --action=data --profile=full --data-root="$DATA" --output="$RESULTS"
Rscript "$TOOLS/study2-gls.R" --action=run --profile=full --data-root="$DATA" --output="$RESULTS" --cores=2
Rscript "$TOOLS/study2-competitors.R" --action=run --profile=full --data-root="$DATA" --output="$RESULTS"
Rscript "$TOOLS/study2-gls.R" --action=report --profile=full --data-root="$DATA" --output="$RESULTS"
```

Generate the panels once. Both fitting arms can run on machines that can read the generated data and write to the same result store. Omit `--data-root` to keep inputs and fits under `--output`. The full profile uses IDs 1–50. For a one-ID execution check, use `--profile=smoke` throughout and choose separate data and output directories. To inspect one condition, add `--scenario=mixed-dynamic` to data generation and both fits. The report action collects the entire saved study.

To resume an interrupted fit, repeat the same run command with the original output directory and sampler settings. The GLS driver resumes committed MCMC checkpoints by default. A completed method job is reused after its input and fit identities are checked. Use the same installed R and package environment on workers that share the result store.

## Read the results

Open `$RESULTS/reports/02-sparse-grid.html` for the joint report. The [R Markdown source](reports/02-sparse-grid.Rmd) and [archived one-replicate HTML](reports/02-sparse-grid.html) are available in the repository. The manifest under `$DATA/02-sparse-grid/panels.csv` identifies each panel by condition, replicate, seed and checksum. The following exports are under `$RESULTS/02-sparse-grid/`.

The main structural outcome is training-period trajectory RMSE for cross-link coefficients. In `comparison.csv`, use `window=pairs-cross` and `metric=trajectory_rmse`. Within each support pattern, compute dynamic GLS minus static GLS on each replicate, then compare that difference under dynamic and static truth. Inspect `active_amplitude_bias` and recall alongside RMSE. All 552 cross-links enter the cross-link RMSE, including inactive ones.

The joint report also shows held-out deviance relative to Poisson lasso. `support-path.csv` relates the number of retained pairs to held-out deviance along the frozen validation path. Use the saved validation selection when reporting the chosen graph. The historical [single-realization grid](run_simulation.R) uses a different signal definition.

Check `jobs.csv` and `failures.csv` for incomplete fits. In `paired.csv`, read `paired_n`, `finite_n` and `nonfinite_n` with each difference and its Monte Carlo standard error. Retain split R-hat and effective-sample-size flags from the dynamic GLS fits' `diagnostics.csv` files when presenting comparisons. The default monitoring thresholds are split R-hat ≤ 1.01 and ESS ≥ 400. These diagnostics describe within-fit sampling accuracy. The paired standard error describes variability across independent replicates.
