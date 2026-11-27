# Replicated count-network experiments

Run the scripts from a checkout with its included R package installed. Each case has its own `generate.R`, `fit.R`, and `compare.R`. A method job only reads its case's saved panel. Generating, fitting and collecting results are separate operations.

| Case | Panel conditions | Independent method jobs |
| --- | --- | --- |
| `01-graph-error` | `T100` or `T200` followed by `-error0`, `-error25` or `-error50` | `pooled-pnar`, `gls-restricted`, `gls`, `gls-flat` |
| `02-sparse-grid` | `seasonal-null`, `local-static`, `local-dynamic`, `mixed-static`, `mixed-dynamic`, `mixed-dynamic-strong` | `poisson-lasso`, `gls-static`, `gls`, `gls-flat` |
| `03-surges` | `local-off` or `local-on` followed by `-transmit-off` or `-transmit-on` | `gls-own`, `gls`, `pnar`, `sa-pgss`, `graph-wavenet` |

```sh
RESULTS="$HOME/bdynets-results/study-smoke"
CASE=GLS-paper/experiments/01-graph-error
Rscript "$CASE/generate.R" --output="$RESULTS" --profile=smoke --replicate=1
Rscript "$CASE/fit.R" --output="$RESULTS" --profile=smoke --replicate=1 --method=all
Rscript "$CASE/compare.R" --output="$RESULTS" --profile=smoke
Rscript GLS-paper/reports/render.R 01-graph-error "$RESULTS/01-graph-error"
```

Supply one scenario or a comma-separated list with `--scenario`. `--method` also accepts a comma-separated list. The default for each is `all`. To fit a single competitor, select its name in the fit command. Select another replication with the same integer `--replicate` in generate and fit. The profile, master seed and generator identity are fixed per output directory. Smoke uses short panels and chains. Pilot and full profiles use the scientific designs and independent random streams. Run separate pilot replicates before fixing the number of confirmatory replicates. The scripts execute the one specified replicate at a time.

The `--cores` option budgets chain workers per method job. For external job-level parallelism, budget cores across jobs and constrain BLAS threads. General and static MCMC fits take `--chunk-size`, `--checkpoint-every` and `--resume=true|false`. Put `--output` on the disk holding the draw and checkpoint stores. A repeat command verifies job inputs before reusing complete work. Rerunning an interrupted job with matching options resumes the last committed checkpoint. A new output directory is appropriate when the requested settings or installed code have changed.

Cached fits include the R runtime, RNG, numerical libraries, and installed package code. Graph WaveNet also records its upstream source hash, imported Python, NumPy, and PyTorch versions and PyTorch build. Fitting checks these identities before reusing a completed job. The collector checks the stored panel, job and scored-curve identities and requires matching scientific code and runtimes across each method’s replicates. It can run offline after fitting, even on a machine without Python. See `provenance.csv` for the job identities and whether the current checkout still matches the source used for the saved fits.

## Data generation and comparison

The graph-error panel uses equiprobable stochastic block labels, directed within/between-block edge probabilities of N^(-0.3) and 1/N, a Gaussian copula on exponential waiting times, and the published log-mean PNAR coefficients. Supplied graphs preserve in- and out-degrees under edge swaps. The numerator of realized graph error counts displaced true directed edges. Rewiring preserves observed counts, splits and truth. Full-dictionary constant-affinity GLS shares one posterior fit across graph conditions. Pooled PNAR uses raw log(1 + count) lags. Its fitted Poisson scores concern individual conditional margins.

The grid cells have 24 self and 20 cross effects when the signal is positive. They preserve targets and signed weights across changes in support location, and use an independently calibrated RMS lag contribution of 0.1 or 0.2. A bounded sinusoid with a randomized phase generates the dynamic variation. A separate pilot panel calibrates RMS using common random numbers. Baseline intensities range from 12 to 20. The static comparator samples graph-weighted horseshoe coefficients by elliptical slice sampling. Both static and dynamic fits use the same finite-shape approximation to the Poisson likelihood. Both use the same fixed distance affinities, global prior scale and baseline predictors. The static Poisson lasso leaves calendar coefficients and intercepts unpenalized, applies an absolute-value penalty to the lag coefficients and selects its penalty using held-out validation likelihood.

In the surge panel upstream events occur in every scenario. Each stream's local event times are drawn independently subject to a minimum separation and recurrent events within each chronological split. Shared driving uniforms yield matched source streams and observation noise across conditions. A baseline, rising ramp, plateau, falling ramp and return to baseline produce short episodes. Transmission multiplies downstream intensity by 1.25 per unit rise in the standardized lag. The first informative target bin occurs one observation after source onset. The event and quiet masks support both marginal and disjoint event-window scoring.

Full dynamic models use the physical distance affinity across all ordered pairs. `gls-flat` uses constant affinity, `gls-restricted` uses supplied one-hop neighbors plus self, and `gls-own` uses only the self lag. Each dynamic fit supplies the continuous prediction, a `-dss` decision (smallest graph within one paired block standard error of the validation minimum), and a `-tolerance` decision (smallest graph within 0.7% of continuous validation deviance). Structural summaries for the continuous prediction use a marginal posterior RMS exceedance probability above 0.5 at amplitude 0.01. DSS summaries use positive retained trajectory multipliers. Coefficient metrics compare posterior-mean trajectories on the training-scaled lag scale. Every method forecasts sequentially one step ahead. For the replicated grid, each frozen candidate graph is then evaluated on the test window with common forecast draws, reported in `support-path.csv`. All choices still refer exclusively to validation.

Graph WaveNet needs NumPy and PyTorch. Set `BDYNETS_PYTHON` to the Python executable and `BDYNETS_GWNET_MODEL` to the pinned upstream `model.py` for offline use. Without an upstream file the adapter attempts a checksum-verified download pinned to the source commit. Its output supplies point forecasts. The edgewise sa-PGSS benchmark uses the documented particle filter and validation tuning.

## Results

Each generated panel contains the counts, actual intensities, known coefficients, splits, prehistory, graph and design-specific event or graph truth. Every job records its identity, panel checksum, seed, R session and elapsed time. The collector writes `jobs.csv` (all expected method jobs), `failures.csv` (failed or pending jobs), `comparison.csv` (one row per metric/window/method/replicate), `paired-replicates.csv` and `paired.csv`. Paired estimates give method minus reference. The independent panel is the unit for Monte Carlo standard errors. Counts of finite and nonfinite differences retain information about divergent fits. Scores for empty windows or undefined precision/recall are missing. See the saved validation path and MCMC diagnostics in each fit directory; smoke-budget chains provide execution checks.

The original manuscript grid launcher remains available at [its historical entry point](../run_simulation.R). It runs the earlier single realization at each signal using its original fit conventions. The replicated grid study uses the designs and decision rules above.
