# Two-data, eight-method comparison

The supplementary entry reuses the exact two registered core inputs. It adds EucMoDE, MoDE, PottsMoDE, STGNN and PNAR. It does not regenerate data, rerun existing Exp-W/SE-W/C draws, or extend beyond 4000. The six Bayesian models each use K=10, four chains and 2000 warmup iterations. Every failed validation cell stays in the selection table; no candidate fallback or scientific retry is permitted.

The supplementary plan contains 138 scientific fits and six selection tasks:

| Work | Fits | Frozen rule |
|---|---:|---|
| Final EucMoDE, MoDE, PottsMoDE | 24 | 2 datasets × 3 methods × 4 chains, 4000/2000 |
| Potts beta validation | 24 | beta .25/1/4 × origins 96/120 × chains 1/3 × 2 datasets; 600/300 |
| PNAR validation and final | 16 + 2 | p=1/2/6/24, both training origins, maxeval=1000, xtol_rel=1e-8 |
| STGNN validation and final | 64 + 8 | K=3/5/7/10 × pool=.1/1 × both origins × 2 restarts; final four restarts |

STGNN validation runs up to 300 epochs, scoring every 25. Selection uses pooled training-validation SSE and the historical tie rules. Final restarts use the selected epoch count. STGNN is not assigned 4000 epochs. Original per-fit caps remain 600 seconds for STGNN, 300 for PNAR and 1800 for Potts validation. Potts pools chain predictive means before RMSE; its validation starts depend only on their training prefix. Its final starts share the core's original dispersed initial draws.

Potts, PNAR and STGNN receive the same fixed, mean-one Exp-reference weights on all physical direct edges. This is their historical graph input, not an additional fixed-h W model. EucMoDE receives coordinates and a full-rank Euclidean covariance; its four-neighbor subset is only for geometry-based prior-center calibration, not graph-edge deletion. MoDE uses the existing symmetric Dirichlet(.1) mixture. Original kernels and q priors remain unchanged.

## Dependencies

No third-party author source is vendored. `external-lock.json` pins PNAR 1.8 (GPL >=2, exact archive and source SHA-256) and STGNN author commit `303c8cc6c5142aa1a4eab06677ce44104bd95355` (MIT, individual source hashes). Obtain these exact public versions separately. The PNAR adapter sources `PNAR/R/lin_estimnarpq.R` and requires installed Rfast and nloptr, in addition to core R dependencies. It does not install packages.

The locally available STGNN runtime has Python 3.11, NumPy1.26.4, Torch2.5.1, torch-geometric2.6.1, torch-scatter2.1.2, torch-sparse0.6.18, torch-spatiotemporal0.9.4, pytorch-lightning2.6.6, einops0.8.2 and pygsp0.5.1. Exact installed package versions are registered at launch. Binary dependency installation is platform-specific; the original CUDA environment file is not a Mac installer. Run the import preflight in the chosen existing environment before using it. No GPU or multithreaded numerical jobs are used here.

## One-command full entry

With the pinned author source and compatible environments already installed, run from the repository root:

```sh
python3 graphMoDE/all_methods.py \
  --output "$HOME/graphmode-eight-methods" \
  --pnar-source "$HOME/dependencies/PNAR/R/lin_estimnarpq.R" \
  --stgnn-source "$HOME/dependencies/Time-Series-Clustering-with-GNNs" \
  --stgnn-python "$HOME/dependencies/stgnn-venv/bin/python"
```

These are example dependency locations, not embedded paths. The entry verifies the pinned source hashes, records the actual Python runtime, writes the local configuration and seeds, runs lightweight preflight, then executes the two-data core followed by the five-method supplement. It creates `core/summary-4000`, `supplement/summary`, and `all-methods.csv` with up to 16 data-method rows. Missing or failed results remain explicit. It requires six free numeric slots at startup; the sequential stages never add a second six-worker batch. Output directories must be new. No package downloads or installs happen automatically.

To reuse an already terminated portable core output, add `--core-output PATH`. To only generate a private configuration from existing core inputs without fitting, also add `--configure-only`; this works while other simulations continue. Do not point a reuse command at the initial historical wrong-C-prior run. This live project uses the separate verified corrected-C / W-continuation sources in its frozen local configuration.

The full entry is assembled from the tested core, supplementary transitions and benchmark adapters. Configuration generation, missing-dependency checks and table merging were tested; a new end-to-end eight-method run has not finished. The existing authorized run remains in progress and is not restarted by this release.

## Explicit queued entry

Create the plan outside the source tree:

```sh
python3 graphMoDE/comparison.py plan "$HOME/comparison-plan.json"
```

Create a private JSON configuration with these fields (use your own paths): `plan_file`, `core_input`, `external` containing `pnar`, `stgnn`, `python`; `python_runtime` containing the JSON returned by `PYTHON graphMoDE/comparison.py runtime`; `queue_after`, a list of `{pid, command_contains}` for existing controllers; and `combined_outputs`, a list of existing output directories to include in the shared 30 GiB budget. Optional `core_results` has `C` and `W` output directories for the corrected-C / W-continuation receipt formats used by this round. Do not publish this configuration because it contains local paths.

```sh
python3 graphMoDE/comparison.py queue "$HOME/graphmode-comparison" --config "$HOME/comparison-config.json"
```

The output directory must not exist. An adjacent `.queue` directory records source/configuration/plan hashes immediately. The process waits for the specified controllers and a numerical slot, then executes the focused R transition test, author import test and full R registration. Only successful preflight releases science jobs. The scheduler enforces at most six numeric workers including detected external R/heavy Python processes, sets numeric threads to one, and uses process-bound macOS caffeinate without keeping the display awake. It preserves failures, checkpoints, logs and source/seed identities. Resource or supervisor faults pause writers; they do not drain indefinitely or automatically resume.

For a custom comparison fit use `gm$graphmode_extra_fit(x, coordinates, "EucMoDE")` (or `"MoDE"`, `"PottsMoDE"`). `x` follows the main README: Y[n,T], adjacency[n,n], edge_lengths[n,n], Fmat[T,3], Ffuture[H,3], ids[n]. Coordinates are n×dimension, aligned with those same IDs. This historical baseline constructor currently requires the intercept/sin/cos design, 42<=T<=168 and n>=10. STGNN's adapter instead takes training counts as T×n and weights as n×n; PNAR receives Y[n,T] through its adapter and transposes exactly once for its author API. The registered comparison selection/diagnostics specifically use n=121, T=168, H=48.

## Test status

Python syntax and the two focused plan/concurrency tests passed. The R transition test passed for EucMoDE, MoDE and PottsMoDE, including original Potts selection/failure cases. The pinned STGNN import/shape preflight passed without model initialization or training. The tests used one naturally released slot; running scientific chains were not paused. At actual launch the queued controller repeats its bounded runtime preflight before registration. The previously published core small example and core tests passed as recorded in TESTING.md. No new eight-method scientific result or convergence claim is made by this code handoff.
