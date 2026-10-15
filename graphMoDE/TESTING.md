# Validation record

2026-10-15 preparation validation:

- Dependency closure: 180 functions/constants from 33 original modules. Original module bytes all match development source commit `e3fde164335fd921b97ea0e7cee863ac6ecfdde2`. Every extracted function body/formals and constant compared identical after loading.
- Ten focused test groups pass via `python3 run.py test`: plan/seeds, geometry/gaps, equal mean, priors/full rank/support, input rejection, Gamma Jacobian/covariance, monotonic clock, full fixed-tape transitions, exact checkpoint/RNG continuation and corruption rejection, full summary/extension/stop paths with explicit failed fixtures.
- `example`: all three methods completed 8 actual sampling steps on a synthetic 12-node, 48-training-time instance. This is an execution check only.
- `smoke`: 24 chains completed 4 actual steps each with separate seeds, six workers, zero process failures; total 25.34 seconds including generation. No convergence claim follows from this check. Startup overhead means this is not a full-run timing estimate.
- Public-tree scan: code, documentation and synthetic generator only; no personal absolute paths, credentials, manuscript, raw/private/OSM data, large chains or vendored third-party source.
- The existing development repository's global verifier reports a pre-existing personal absolute path in an unrelated pilot module. That module is excluded from this portable closure. The original dirty worktree was not edited.

Formal core-pilot execution is separate from these checks. Read its external `completion.json` and stage diagnostic files; repository code availability does not imply the pilot passed or even finished. No eight-method batch was run by this release validation.

C calibration correction: the focused tests explicitly require the two W centers to match and C to retain its own binary-adjacency center (2 on the path). The initial pilot stopped on an SE edge-weight underflow guard; its failure records remain retained.

Controlled-underflow update (2026-10-15):

- All ten existing groups and two additional underflow groups passed: logical support/finite log weights, genuine binary64 boundary, covariance/target diagnostics, and stopping on exceeded bounds.
- Normal-range weights and principal roots match the predecessor bit-for-bit. An actual saved Exp-W step1001 reproduces the predecessor state, MH updates, utilities and RNG bit-for-bit.
- The original SE-W step1209 proposal was reproduced from the exact recovered step1208 state/RNG, including the RNG state at the old failure. Relative spectral residual was about 1.52e-15, covariance residual 1.09e-14, and alternate gate-target difference 9.77e-15. The unchanged MH test rejected that candidate and the full transition completed.
- The continuation plumbing was separately checked with real Exp transitions: uninterrupted and checkpoint-recovered states/RNG were identical. Capacity, diagnostic reservation, exclusive supervisor and peer-failure isolation checks passed. Those checks do not establish posterior convergence.
- Strict interval-MH certification was neither implemented nor claimed. No full eight-method batch was started.

Resource-boundary correction (2026-10-15): `python3 tests/controller.py` uses mocks only; it launches no R process or simulation. It verifies that an individual chain failure allows its valid peer to finish, a resource exception pauses owned writers without waiting, failure to write the initial emergency record cannot bypass pausing, the72-hour limit uses the same emergency exception class, and a failed4000-stage statistical diagnostic does not launch8000. All checks passed. No statistical kernel, prior, seed or diagnostic threshold changed in this correction.

Eight-method supplementary plumbing: Python syntax and two plan/capacity tests passed. Focused R sampler transitions and original Potts selection/failure tests passed in about1 second. Pinned STGNN author import/shape preflight passed in about13 seconds without model initialization/training. Supplementary registration and fits remain queued. See COMPARISON.md. No additional scientific kernel was rewritten.

Portable `all_methods.py`: dependency-hash verification and real configuration-only invocation passed using the existing external environment; a synthetic16-row merge retained failed statuses, and a duplicate configuration was refused. This verifies packaging/dispatch interfaces, not completion of all scientific fits.
