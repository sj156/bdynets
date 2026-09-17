# D-051 execution preparation provenance — not a freeze or run registration

2026-09-15. User approved the proposed next step of executor integration and
budget planning, with actual running reserved for later confirmation. HEAD stays
`2c5b862a3f70f618789d3bee59824594ccb767cc`; the mixed user worktree is preserved.
No commit, index update, push, package export, manuscript change, science input
generation, PG/MCMC, old-result statistical rerun or new external registration.

## Evidence reused

- D-050 independent audit:
  `/private/tmp/graphmode-code-audit-20260915-gate-refresh-un95e_si/audit.md`.
  This audit covers D-050, not the D-051 execution layer added afterward.
- Audited D-050 main module remains SHA-256
  `7f1533088e16a0ee167d9cd9815c505e1dd8cfd82ec52d619e73e07c60c9bf41`.
- All55 B source-closure files match their blobs in the original2c5b862 commit;
  the B freeze manifest itself contains five **new** entries, not the full55.
  D-051 identity checks both the composed original55 and the B-specific manifest.
- Budget basis is B's already recorded16949.071-second batch for24000 steps,
  not a new benchmark. The1200/600 eight-chain proposal estimates2–2.5h with
 11700s science+900s diagnosis process limits; source-freeze/seed/path checks and
  actual registration/launch are still pending.

## New tested files

| SHA-256 | File |
|---|---|
| `695c3867e8a89ba7ffd31a3d241035220568a75b8dde396c59b85eb4b514903d` | `R/graphmode_refresh_run.R` |
| `db6da58edb2d84c69c3d7936a1579ddce481617663a5518de5671938b4c9d5f8` | `scripts/graphmode-refresh-run.R` |
| `bbf49aa1c0571cceebcafa0f897a87c4b566dd048c3627b700ac8d82d5342bff` | `scripts/tests/graphmode-refresh-run-deterministic.R` |
| `b74de832159872b7e2ba370d52578b4a88080c94b009b4389e59413cd00a9142` | `docs/GRAPHMODE_REFRESH_COMPARISON_2026-09-15.md` |

These are preparation hashes, not a committed canonical source pointer.
Operational notes appended to CHANGELOG, inventory, lineage and DECISIONS do
not replace historical entries. No EXPERIMENT_REGISTRY row was added.

## Verification

The separate `check` command passes23 fixed-input/mock-process groups, exit0:
explicit authorization, old-schema/unfrozen refusal, paired inputs, fixed scales,
actual D-050 m1/m4 state and random-interface-call equivalence, outer-only saved
draws, complete substep counts, numerical/trace checks, worker failure retention,
canonical path checks, correct parent exit/receipt binding, timeout with a saved
result, pending publication quota/postflight failure, eight mocked job dispatches,
two separate original37-scalar/six-PSM reducers on fixed states, once-only execution,
report completeness/unchanged validity/correct ESS-cost checks, and late failure
invalidation. RNG kind/state and the old source closure remain unchanged.

Final fixed artifacts retained at
`/private/tmp/graphmode-refresh-run-fixed-27937854c9e2/`.
They use eight-step **preset** states and mocked subprocess calls. Printed
statistical failures are expected test evidence, not results of the proposed
1200-step experiment. No real random responses, normal/PG draws or scientific
worker subprocess was launched. Earlier passing fixture
`/private/tmp/graphmode-refresh-run-fixed-26be210b4a83/` remains as prior test
evidence; it predates removal of an obsolete nested one-pass report placeholder.

Initial syntax/test-environment/handler-shadowing mistakes were fixed before
the final pass. The preparation source check was corrected to distinguish the
five-entry B incremental manifest from its full55-file closure; no scientific
trial was used to identify these issues. Parse and scoped whitespace checks pass.
Default CLI status is non-running; a run command without explicit authorization
fails before loading inputs. No repository-policy/imported-source change was
made, so the unrelated repository-wide audit was not repeated.

## Remaining boundary

User review of the numerical plan and budget; focused executor audit if desired;
source freeze with actual commit and full identity; targeted seed collision,
storage and fresh canonical destination verification; then external registration
and explicit run authorization. The current work does not certify statistical
improvement, approve m4 as a winner, reinterpret B's two failures or authorize
formal simulation. Computational-step manuscript wording is not edited here.
