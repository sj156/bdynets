# Figure 3(a): four additional models, c6 fixed development comparison

2026-09-17. The user explicitly authorized supplementing graphMoDE-C, EucMoDE,
MoDE and PottsMoDE on the already observed panel and instructed that all models'
settings be reconsidered together for the future larger server study. This
supersedes older stop-only notes solely for this one registered batch and its
once-only evaluation. Existing statistical flags remain visible; they are not
an instruction to optimize, extend chains or relax thresholds.

## Fixed comparison

- Reuse the c4 blinded observations underlying c5 and Figure 3(a): intertwined
  spiral, n=121, T=168, true five groups, fitting capacity K=10. This is one
  development dataset, not five independent datasets or a formal benchmark.
- Each additional model: four chains, 600 total iterations, first 300 discarded,
  thin=1. Preserve all 1,200 retained partitions per model.
- Reuse c5's saved iteration-zero allocations and complete expert paths, with
  occupancy 1/3/7/10. C and Euc reuse the same standard-normal whitened gate
  coordinates and guidance logits; their physical utilities differ with the
  model covariance. MoDE starts with uniform mixing weights; Potts has no
  Gaussian gate. The same numeric seed would not imply the same random draws
  across different kernels; all 16 new sampling seeds are distinct.
- The full dynamic Poisson expert specification is identical to c5: fixed
  seasonal basis, prior, innovation covariance, rho=4 and temporal block42 with
  uniform offset. C/Euc use the existing cached contrast-block ESS and four
  gate/MH scans with the c5 fixed proposal scale. No new sampler formulas.
- C uses the binary version of the same q=4 dependency graph. Euc uses the
  existing trace-normalized Matern kernel (nu=1) with range equal to the median
  positive pairwise Euclidean distance, computed solely from geometry. MoDE
  uses its existing Dirichlet(0.1,...,0.1) prior. Potts uses the same weighted
  dependency graph and fixed beta=1 with the existing sequential Gibbs update.
  These are fixed development settings, not response-validation-selected or
  optimal hyperparameters. A tuned model-ranking claim is outside this run.
- Use c5 W-reference as the common-base five-model comparison. Display c5
  W-joint separately because its extra joint move is W-specific. Neither W arm
  is rerun. Reuse its original convergence diagnostics; only compute matching
  descriptive summaries from its saved draws.

## Execution and evaluation

The seed mapping is fixed in `graphmode_compare_spec()`: 2026121101--2026121116,
chain-major order, model order reversed on even chains. Check every available
local and archived registration for collisions before registration. Freeze the
new source files in a narrow local commit, retain all unrelated dirty work,
and record complete runtime/source hashes, input hashes, configurations,
starts and fresh output destination in registration.rds before sampling.

Run sixteen workers sequentially, one numeric thread, C locale. Worker cap:
1,200 seconds; science process cap: 14,400 seconds; once-only diagnostic process
cap: 1,800 seconds. Controller overhead is additional. These are stop limits,
not promised durations or targets. Keep at most 32 GiB in this output, require
64 GiB free before start and 8 GiB during execution. Checkpoint every 100 steps
for evidence only; no resuming, automatic retries, extensions or replacement of
failed chains. Display progress every 25 steps and retain full worker time,
including checks and output writes, rather than just an inner kernel timer.

Only after science completion, evaluate against saved truth: truth-blind Dahl
representative partition, ARI, class count, posterior Pr(Kocc=5), pairwise Brier
score, exact-partition frequency and unit-profile RMSE/empirical coverage.
Report the original Rhat/ESS/PSM rules once for each new model, with constant
variables distinguished from passing diagnostics. Do not adjust the thresholds
or make the old six W-joint flags the optimization target. Different model
results under this short budget are descriptive; no claim that every posterior
has converged or that this budget identifies the best method.

No manuscript edit, GitHub publication, package installation, multi-agent audit
or formal simulation release is part of this authorization. These settings are
not new canonical defaults. The future larger comparison must jointly specify
iterations, warmup, chains, starting rules, model-specific tuning and reporting
for all five models.
