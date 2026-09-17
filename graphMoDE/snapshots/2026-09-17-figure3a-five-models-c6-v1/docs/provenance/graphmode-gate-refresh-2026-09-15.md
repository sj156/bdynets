# D-050 conditional gate-refresh provenance

2026-09-15. User authorizes focused code work on the current problems, not
other methods/roads or a new simulation. This is an uncommitted development
addition on HEAD `2c5b862a3f70f618789d3bee59824594ccb767cc`, not a source freeze.

## Evidence reused and scope

- Existing `GRAPHMODE_PHASE_B_RUN_2026-09-14.md`: measured expert time16162.146s
  of16551.682s (97.65%); gate ESS43.306s and guidance130.682s. This motivates
  investigating update frequency, not a proved cause of the remaining failure.
- Existing focused B explanation and D-049 report: guidance_7 rankRhat
  1.01150255819 and proportion_10 tailESS=NA remain original failures. No
  B trace statistic, original Rhat/ESS/PSM, random proposal or checkpoint is rerun.
- Actual current Dropbox computation source, especially guidance update and
  allocation implementation: SHA-256
  `36bf053c3de4be4746d0435e3b9c6184d52a58c1c6f42e159fd1d03f76c87bcb`.
- Current Dropbox Section5: SHA-256
  `7a0db4db8cd8ee2f4780729fad4b466751325bb21e1aaf1eb740c08886828096`.
  Neither manuscript was changed; no canonical pointer is promoted.

## New authored files

```text
7f1533088e16a0ee167d9cd9815c505e1dd8cfd82ec52d619e73e07c60c9bf41  R/graphmode_gate_refresh.R
97992375024b048ee0f11febf3fa725aaa9f3b46c16adf433c9c63479ca83ee7  scripts/graphmode-gate-refresh.R
528f1485489ef71d7c3f51cce98ba4efa006ab443e09c486497465bba3ee2977  scripts/tests/graphmode-gate-refresh-deterministic.R
cbc9c48f1770b2cc158c5859b98221d9af4818b741a6f32b9857217094fed03e  docs/GRAPHMODE_GATE_REFRESH_2026-09-15.md
```

All old55 B source files match the original registration's `identity$sha256`.
Existing D-049 code and all other scientific kernels are unchanged. Inventory,
decisions, lineage and changelog receive only the new scoped status; no experiment
registry or prior freeze manifest is changed.

## Validation

Run `Rscript --vanilla scripts/graphmode-gate-refresh.R check` with LC_ALL=C:
22 fixed-input groups pass, using the original kernels with predetermined
normal/uniform/PG/gamma interface outputs. These are not random scientific
samples. Single-pass state/draw-order equality, manually composed inner steps,
final utility/state handoff, one expert/allocation update, acceptance denominators,
shared/empty-label handling, original rejected-proposal observations and failures
are covered. An independent fixed8-state transition-matrix calculation checks
conditional invariance for1 through64 repetitions, not empirical convergence.
RNG kind/state is unchanged. CLI status and `git diff --check` pass.

No old test suite or original B diagnostic is rerun. The full repository-policy
audit is outside this focused locally authored implementation; no policy or
imported third-party source changes. Known pre-existing audit-path portability
issues remain separate and are not claimed resolved.

## Remaining boundaries

No performance or mixing improvement has been observed yet. No chosen default
m, tuning campaign, new controller/worker registration, simulation, package
installation, checkpoint continuation, threshold change, source freeze/commit,
paper edit or public/shared-folder sync. The new CLI has no run command and
old executors cannot silently load the underscore module. Future use requires
focused review, explicit computational-step documentation, a new source-bound
controlled execution plan and separate run authorization. B stays valid=FALSE;
this change does not supply a replacement discrete-precision acceptance rule.
