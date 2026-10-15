# Validation record

2026-10-15 preparation validation:

- Dependency closure: 180 functions/constants from 33 original modules. Original module bytes all match development source commit `e3fde164335fd921b97ea0e7cee863ac6ecfdde2`. Every extracted function body/formals and constant compared identical after loading.
- Ten focused test groups pass via `python3 run.py test`: plan/seeds, geometry/gaps, equal mean, priors/full rank/support, input rejection, Gamma Jacobian/covariance, monotonic clock, full fixed-tape transitions, exact checkpoint/RNG continuation and corruption rejection, full summary/extension/stop paths with explicit failed fixtures.
- `example`: all three methods completed 8 actual sampling steps on a synthetic 12-node, 48-training-time instance. This is an execution check only.
- `smoke`: 24 chains completed 4 actual steps each with separate seeds, six workers, zero process failures; total 25.34 seconds including generation. No convergence claim follows from this check. Startup overhead means this is not a full-run timing estimate.
- Public-tree scan: code, documentation and synthetic generator only; no personal absolute paths, credentials, manuscript, raw/private/OSM data, large chains or vendored third-party source.
- The existing development repository's global verifier reports a pre-existing personal absolute path in an unrelated pilot module. That module is excluded from this portable closure. The original dirty worktree was not edited.

Formal core-pilot execution is separate from these checks. Read its external `completion.json` and stage diagnostic files; repository code availability does not imply the pilot passed or even finished. No eight-method batch was run by this release validation.
