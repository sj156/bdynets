# Study 3 — Upstream surges

Can an unexpected increase in a source count improve its target's next forecast? The first downstream forecast after onset measures the value of the upstream observation before the forecaster has seen the target's own response.

## Design and methods

Each full panel contains eight source–target pairs and eight unaffected series. Training, validation and testing use 600, 150 and 250 bins. Source surges recur in all conditions. Cross independent target-local surges (off or on) with transmission from the designated source (off or on). When transmission is on, a one-standard-deviation rise in the observed source log count multiplies the next target mean by 1.25. The same replicate ID links counts driven by common observation noise and source-event times across conditions.

| Driver | Fitted models |
| --- | --- |
| [GLS](tools/study3-gls.R) | `gls` uses all candidate lag pairs and `gls-own` uses each target's own history. |
| [Competitors](tools/study3-competitors.R) | `pnar` uses the supplied network average, `sa-pgss` fits each stream separately and tunes its discounts on validation, and `graph-wavenet` learns an adaptive graph for point forecasts. |

Four conditions × 50 replicate IDs give **200 panels** and 1,000 scheduled method jobs. The published univariate sa-PGSS filter and two additional transmission strengths are planned extensions. The [software requirements](README.md#linux-installation) include the Python and upstream model settings for Graph WaveNet.

## Run the study

Install the [paper environment](README.md#linux-installation) and run from the repository root. Each arm defaults to a plan. Choose absolute paths on your system for the shared synthetic data and the joint results directory.

```sh
TOOLS=GLS-paper/tools
DATA="$HOME/bdynets-data/full"
RESULTS="$HOME/bdynets-results/full"
Rscript "$TOOLS/study3-gls.R" --action=plan --profile=full
Rscript "$TOOLS/study3-competitors.R" --action=plan --profile=full

Rscript "$TOOLS/study3-gls.R" --action=data --profile=full --data-root="$DATA" --output="$RESULTS"
Rscript "$TOOLS/study3-gls.R" --action=run --profile=full --data-root="$DATA" --output="$RESULTS" --cores=2
Rscript "$TOOLS/study3-competitors.R" --action=run --profile=full --data-root="$DATA" --output="$RESULTS"
Rscript "$TOOLS/study3-gls.R" --action=report --profile=full --data-root="$DATA" --output="$RESULTS"
```

Generate the panels once. Both fitting arms can run on machines that can read the generated data and write to the same result store. Omit `--data-root` to keep inputs and fits under `--output`. The full profile uses IDs 1–50. For a one-ID execution check, use `--profile=smoke` throughout and choose separate data and output directories. To inspect one condition, add `--scenario=local-on-transmit-on` to data generation and both fits. The report action collects the entire saved study.

To resume an interrupted fit, repeat the same run command with the original output directory and sampler settings. The GLS driver resumes committed MCMC checkpoints by default. A completed method job is reused after its input and fit identities are checked. Use the same installed R and package environment on workers that share the result store.

## Read the results

Open `$RESULTS/reports/03-surges.html` for the joint report. The [R Markdown source](reports/03-surges.Rmd) and [archived one-replicate HTML](reports/03-surges.html) are available in the repository. The manifest under `$DATA/03-surges/panels.csv` identifies each panel by condition, replicate, seed and checksum. The following exports are under `$RESULTS/03-surges/`.

Start with downstream count MAE in the `first` window, one bin after each source onset. The report gives each method minus own-history GLS. A negative value for full GLS means that the source history improved that forecast. For the paper's primary contrast, compute own-history minus full GLS at transmission on and off within the same replicate, then take their difference separately for local surges on and off.

Inspect offsets 1–3 (`early`), 4–8 (`late`) and `quiet` periods to see how long any gain lasts. The event masks in `comparison.csv` also distinguish source-only and local-only events. `pairs-planted`, `pairs-reverse` and `pairs-unaffected` show whether selected directions agree with the generator. The number of eligible target–time cells can vary by window, and the independent sampling unit for paired uncertainty is the panel.

Check `jobs.csv` and `failures.csv` for incomplete fits. In `paired.csv`, read `paired_n`, `finite_n` and `nonfinite_n` with each difference and its Monte Carlo standard error. Retain split R-hat and effective-sample-size flags from the dynamic GLS fits' `diagnostics.csv` files when presenting comparisons. The default monitoring thresholds are split R-hat ≤ 1.01 and ESS ≥ 400. These diagnostics describe within-fit sampling accuracy. The paired standard error describes variability across independent replicates.
