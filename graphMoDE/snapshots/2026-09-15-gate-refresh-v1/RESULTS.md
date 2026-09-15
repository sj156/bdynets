# Reviewed short-screen outcome — 2026-09-15

This handoff summarizes the already completed D-053 experiment and the subsequent
user-requested read-only partition check. No sampling or statistical diagnostic
is rerun for publication. Full scientific payloads remain external/private.

## Design

- One fresh synthetic intertwined-spiral road panel: 121 nodes, 120 segments,
  168 observation times, five true classes, fitted capacity K=10.
- Only graphMoDE-W, not a five/seven-model comparison or a two-road benchmark.
- Fixed-count gate updates m=1 versus m=4; four chains per arm, shared data and
  paired complete dispersed starts, independent sampling streams.
- Each chain: 1,200 outer sweeps, discard 600, retain 600, thin 1. Inner updates
  are not additional posterior draws. Fixed guidance scale
  `2.2688962687759413`, provisional rho=4, no adaptation in this comparison.
- Registered science budget 11,700 seconds plus one diagnostic budget 900 seconds.
  Actual science child 6,937.658 seconds and diagnostic child 32.340 seconds.
  No timeout, retry, resumed checkpoint or additional run.

## Results and interpretation

| Existing acceptance/report item | m1 | m4 |
| --- | ---: | ---: |
| Execution/numerical checks | 4/4 chains passed | 4/4 chains passed |
| Original 37 scalar checks: passed | 7 | 8 |
| Original 37 scalar checks: failed | 19 | 18 |
| Constant discrete, uninformative | 11 | 11 |
| Overall statistical `valid` | FALSE | FALSE |
| Six within-arm PSM RMS comparisons | all 0 | all 0 |
| Longest retained expert rejection streak | 60 | 108 |

Original thresholds remain rank/folded Rhat <= 1.01, bulk/tail ESS >= 400 and
pairwise PSM RMS <= 0.05. Constants or NA are not automatically passed.
m4 improves guidance movement and some descriptive ESS estimates, but does not
resolve all continuous-parameter mixing/precision failures. It is not selected
automatically. Log-likelihood and several unit-level log means still fail.

The later read-only truth check compared pairwise class membership, ignoring
arbitrary class-label numbering. **All eight chains, each at all 600 retained
states, recover this panel's true partition exactly:** five occupied classes
with sorted sizes 10, 10, 26, 26, 49; five of ten fitted slots are empty.
This is eight sampling chains on ONE dataset, not eight independent simulation
replicates or an estimated 100% general success rate. Truth was checked after
sampling, not used to initialize or tune the fit. Exact partition recovery and
identical PSMs do not certify convergence of all parameters or exploration of
all posterior modes. Neither arm's original validity decision changes.

Focused read-only follow-up found an m4 chain-1 expert path unchanged for 109
retained states after 108 rejected proposals; it covers a 49-node class. This
helps explain one unit-mean tail-precision bottleneck, but is not proof that all
remaining failures have one cause, or evidence of a newly established core bug.
Further proposal-efficiency work remains separate from this frozen handoff.

## Evidence identities

- Executed source: `903f4d26a98ec43aba9ab57bc5df1299dea78e81`.
- Original registration signature:
  `f02af40323d1358378fe56293604226dccecf756e4ea0a15c735f277e5475aa4`.
- Original diagnostic-report file SHA-256:
  `a788fd4b6c332b6e4894286d022c4d1ca7f8ba671e2b433c7b63afa5cfe53a88`.
- 115 external result/log/checkpoint files (2,186,699,528 bytes) remain retained;
  none is uploaded here. These hashes identify retained evidence, not a claim
  that the data or report payload is included in this public snapshot.

The original A/B outcomes are historical and unchanged. No formal simulation,
new m/rho selection, package-interface change, or overwrite of prior snapshots
is authorized by this publication.
