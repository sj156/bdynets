# Replicated count-network experiments

Each study has a GLS driver and a competitor driver. Generate the synthetic datasets once per study, then give both drivers the same output directory. For each condition and replicate they read the same saved counts, graph, truth and chronological split. The collector matches their scores by condition and replicate.

| Study | GLS arm | Competitor arm |
| --- | --- | --- |
| 1. Graph error | [study1-gls.R](../tools/study1-gls.R) runs GLS with graph-informed, restricted and flat specifications | [study1-competitors.R](../tools/study1-competitors.R) runs pooled PNAR |
| 2. Sparse directed grid | [study2-gls.R](../tools/study2-gls.R) runs graph-informed, static and flat GLS | [study2-competitors.R](../tools/study2-competitors.R) runs Poisson lasso |
| 3. Directed surges | [study3-gls.R](../tools/study3-gls.R) runs graph-informed and own-history GLS | [study3-competitors.R](../tools/study3-competitors.R) runs targetwise PNAR, sa-PGSS and Graph WaveNet |

The study guides give the design, method assignments, runnable commands and result locations. [Study 1](../study1-README.md) · [Study 2](../study2-README.md) · [Study 3](../study3-README.md)

## Shared options

The default action is `plan`. Generate common panels with `--action=data`, fit each arm with `--action=run`, and collect both arms with `--action=report`. All steps share a result root. Use `--data-root` when frozen panels are stored on another volume. The two arms can fit in either order. Full runs default to replicate IDs 1–50. For job-level parallelism, pass a subset such as `--replicate=1:5` or `--replicate=1,3` to each fitting job. The smoke and pilot profiles default to one replicate and use independent random streams.

The 16 implemented conditions give 800 condition–replicate combinations at full scale. The manuscript also specifies two published reference cases and eight finer sensitivity conditions, each with 50 planned replicates. Those ten conditions require additional implementation. The bundled HTML reports show the archived one-replicate smoke run for each implemented condition.

Use `--scenario` on generation or fitting and `--method` on an arm to pass one name or a comma-separated list. By default an arm runs all its methods on all implemented conditions of that study. Collect and render once per study across both arms.

The `--cores` option budgets chain workers per method job. For external job-level parallelism, budget cores across jobs and constrain BLAS threads. General and static MCMC fits take `--chunk-size`, `--checkpoint-every` and `--resume=true|false`. Put `--output` on the disk holding the draw and checkpoint stores. A repeat command verifies job inputs before reusing complete work. Rerunning an interrupted job with matching options resumes the last committed checkpoint. A new output directory is appropriate when the requested settings or installed code have changed.

Cached fits include the R runtime, RNG, numerical libraries, and installed package code. Graph WaveNet also records its upstream source hash, imported Python, NumPy, and PyTorch versions and PyTorch build. Fitting checks these identities before reusing a completed job. The collector checks the stored panel, job and scored-curve identities and requires matching scientific code and runtimes across each method’s replicates. It can run offline after fitting, even on a machine without Python. See `provenance.csv` for the job identities and whether the current checkout still matches the source used for the saved fits.

All arms call the installed `bdynets` package for common calculations. The [evaluation helpers](../../bdynets/man/gls_evaluation_helpers.Rd) standardize count lags on training rows, summarize Poisson predictive draws, and score forecasts and coefficient recovery. Study scripts supply the event windows and groups of links. Studies and applications use the [atomic file writer](../../bdynets/man/bdynets_atomic_write.Rd) for shared outputs.

## Data generation and comparison

Full dynamic models use the physical distance affinity across all ordered pairs. `gls-flat` uses constant affinity, `gls-restricted` uses supplied one-hop neighbors plus self, and `gls-own` uses only the self lag. Each dynamic fit supplies the continuous prediction, a `-dss` decision (smallest graph within one paired block standard error of the validation minimum), and a `-tolerance` decision (smallest graph within 0.7% of continuous validation deviance). Structural summaries for the continuous prediction use a marginal posterior RMS exceedance probability above 0.5 at amplitude 0.01. DSS summaries use positive retained trajectory multipliers. Coefficient metrics compare posterior-mean trajectories on the training-scaled lag scale. Every method forecasts sequentially one step ahead. For the replicated grid, each frozen candidate graph is then evaluated on the test window with common forecast draws, reported in `support-path.csv`. All choices still refer exclusively to validation.

Graph WaveNet needs NumPy and PyTorch. Set `BDYNETS_PYTHON` to the Python executable and `BDYNETS_GWNET_MODEL` to the pinned upstream `model.py` for offline use. Without an upstream file the adapter attempts a checksum-verified download pinned to the source commit. Its output supplies point forecasts. The edgewise sa-PGSS benchmark uses the documented particle filter and validation tuning.

## Results

The shared generator writes `panels.csv` with the scenario, replicate, seed, relative panel path and checksum. Each generated panel contains the counts, actual intensities, known coefficients, splits, prehistory, graph and design-specific event or graph truth. Every job records its identity, panel checksum, seed, R session and elapsed time. The collector writes `jobs.csv` (all scheduled method jobs, including pending full-profile replicates), `failures.csv` (failed or pending jobs), `comparison.csv` (one row per metric/window/method/replicate), `paired-replicates.csv` and `paired.csv`. Paired estimates give method minus reference. The independent panel is the unit for Monte Carlo standard errors. Counts of finite and nonfinite differences retain information about divergent fits. Scores for empty windows or undefined precision/recall are missing. See the saved validation path and MCMC diagnostics in each fit directory; smoke-budget chains provide execution checks.

The original manuscript grid launcher remains available at [its historical entry point](../run_simulation.R). It runs the earlier single realization at each signal using its original fit conventions. The replicated grid study uses the designs and decision rules above.
