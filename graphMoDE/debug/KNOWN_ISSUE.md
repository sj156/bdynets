# Known K=10 conditional-allocation failure

## Evidence status

The latest registered run is incomplete and does not authorize a formal
simulation. The result archive itself is intentionally not included because it
is 362,075,857 bytes and is mostly large chain objects. This handoff retains the
exact frozen source, the small truth-blinded inputs, trace fingerprints, rules,
and the original 824-byte failure record needed for diagnosis.

| item | recorded value |
|---|---|
| diagnostic API | `countdlm-road-k10-conditional-allocation-diagnostic-2026-09-03-v1` |
| sampler | `exact-poisson-info-ffbs-joint-ess-devroye-2026-08-31-v2` |
| failing task | `GMDE-W-mode-A-seed-2` |
| seed | `2026094102` |
| dimensions | `n=100`, `T=168`, `K_fit=10`, `m=40` |
| controls | fixed `rho=1`; 3,000 iterations; 1,000 burn-in; `substantive_min=5` |
| elapsed before stop | 306.968 seconds |
| warning count | 0 |
| error | `Poisson-binomial threshold recursion failed its mass audit.` |
| completed diagnostic tasks | 11 of 12 |
| failed result ZIP SHA-256 | `bd7f945165dc99282fa2914b684174ced85e0bf7c3cf5b96ac54a6931712f4d9` |

The mode-A initial partition hash is
`63ff60b740f1dfcca7cda950b626dc934748ca5c5c339409092fd6130530a2d2`.
The neutral theta and gamma hashes are respectively
`f64af3dd151911d9e176188acf7404ddaae963f4611b53737c6975f312ffec60`
and `5337ae0b2ba7653f5539a1b7571dc220c86d04407a9302879e354476fbe6945a`.
The corresponding parent scientific-trace fingerprint is
`4146dc66cf553170155dacd7f6660ecb16492426b2fd25674434aea17c7f1ced`.

## Exact location

- `R/gmde-classifier.R:145` defines
  `gmde_poisson_binomial_threshold()`.
- `R/gmde-classifier.R:186-189` computes the total probability-mass error and
  stops when a state is outside tolerance or `mass_error > 1e-12`.
- `R/gmde-classifier.R:271` calls it once per component from
  `gmde_allocation_conditional_mechanics()`.
- `R/gmde-mcmc.R:790` invokes that passive diagnostic immediately before the
  categorical allocation draw.

The diagnostic is intended to be deterministic and consume no random numbers.
Its unit test passes exact enumeration for small inputs and confirms that
turning the passive diagnostics on does not change a tiny MCMC trajectory or
the R RNG state.

## What is and is not known

Known:

- the failure is deterministic conditional-diagnostic work performed before an
  allocation draw;
- 11 other registered chains completed with the passive diagnostic enabled;
- all 12 parent K=10 scientific traces, generated without this added passive
  storage path, matched their frozen fingerprints;
- the original failure record did not retain a completed fit or terminal state.

Not yet known:

- the exact iteration, component, probability vector and violated part of the
  mass audit in the original run;
- whether the cause is recursion round-off accumulation, an out-of-range
  probability that survived an upstream check, or another implementation
  defect;
- whether a mathematically different diagnostic algorithm is required.

Therefore this evidence does not justify saying that the main sampler is
broken, and it also does not justify saying that the sampler is ready.

`scripts/reproduce_k10_failure.R` wraps only the deterministic threshold helper
in memory. It does not edit the scientific source and does not draw random
numbers. On the first error it records the probability vector, threshold,
inferred iteration/component, pre-call RNG hash, state values and mass error in
the git-ignored `debug-output/` directory.

## Minimal repair boundary

Limit any fix to the smallest code and tests required to explain and correct
this threshold calculation. Do not refactor the sampler, change the DGP,
replace the graph, change K, change rho, change seeds, relax scientific gates,
or alter unrelated diagnostics. In particular, merely increasing the
`1e-12` tolerance is not an acceptable repair without a numerical error bound
and tests demonstrating why the new criterion is correct.

## Pass criteria

All of the following must hold:

1. Capture the first failing probability vector and identify which audit term
   fails, at which iteration and component.
2. Explain the root cause as a mathematical, implementation, RNG/reproduction,
   or diagnostic-only defect, with a regression case that fails before the fix.
3. Exact-enumeration tests for small vectors, boundary probabilities, extreme
   but valid 100-element vectors, and repeated stress cases pass without
   suppressing errors or warnings.
4. The previously failing task completes with no warning using the same input,
   mode, seed, K, rho, state controls and run length.
5. Enabling the passive diagnostic leaves the complete scientific trajectory
   and terminal RNG state byte-identical to a run with it disabled; the
   scientific trace must match
   `4146dc66cf553170155dacd7f6660ecb16492426b2fd25674434aea17c7f1ced`.
6. All 12 registered K=10 tasks complete without warning, and every scientific
   trace matches the corresponding fingerprint in
   `data/debug/parent-scientific-trace-audit.csv`.
7. No threshold, seed, diagnostic rule, truth-blinding rule, or failure-retention
   rule is loosened after observing the result.

Passing these criteria clears this debugging blocker only. It does not by
itself authorize the formal simulation.

