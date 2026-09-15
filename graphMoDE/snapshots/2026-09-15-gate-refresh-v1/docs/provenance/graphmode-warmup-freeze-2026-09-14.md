# D-046: phase A diagnostic-acceptance repair and local source freeze

Date: 2026-09-14. User authority: repair the reported P2, run focused fixed-input
regressions, then freeze this source locally. No experiment registration, actual
initialization draws, pilot/simulation, old-result diagnostic rerun, package
installation, manuscript/interface change, upload or phase-B launch is authorized
or performed as part of this freeze.

## Repair and evidence

The implementation window's focused D-045 review found that writing the final
diagnostic acceptance could itself exceed the storage budget while leaving a
successful-looking receipt. Its original fixed-file review and evidence are
preserved unchanged in the local `graphmode-d045-review-20260914-e4ZXjjtb`
temporary directory. This is not an independent-window audit.

The repaired controller writes a pending receipt first, audits the complete
written bytes, rechecks source/tree and bound request/report/zero-exit evidence,
then publishes by same-directory rename as its last fallible operation. Nothing
is overwritten, retried or automatically deleted. Controller and child failures
are separate immutable records. A failed emergency write does not make a pending
receipt successful. The v2 read-only acceptance contract also rejects missing or
changed final receipts, mismatched evidence, pending files and either failure
record; a report or `postflight_passed` boolean alone is not acceptance.

Verification: 36 fixed-input/mocked-process groups pass (28 existing + 8 focused
repair groups), including successful publication, quota failures during request
and acceptance writes, late pre-publication exhaustion, emergency-write failure,
source-postflight/publication failure, changed evidence and no retry/bypass.
The fixed tests preserve RNG state/kind and use no real PG or scientific draws.
R parsing/usage checks and scoped whitespace checks pass. All 31+4+4 old frozen
files match their original manifests; no old mathematical tests or scientific
diagnostics are rerun. Raw validation is retained in the local
`graphmode-warmup-fix-freeze-20260914-5VobHOMe` temporary directory.

## Source identity and freeze scope

The repaired source is the local Git commit containing this note and
`graphmode-warmup-freeze-2026-09-14.sha256`. The manifest fingerprints the three
phase-A code/entry/test files, the plan and implementation guide, this note, and
the two preserved historical preparation-provenance files. The commit itself
is not placed in its own content hash. D-044's baseline remains
`f6e02056cb4adcce94cdf728c9751833d7327b5e`; it cannot identify the new phase-A code.

Only these nine explicit new paths are included in the local source commit.
Mixed, preexisting changes elsewhere are left separate, including the global
inventory/decision/lineage/changelog working notes. Historical snapshots and
published GitHub/Dropbox copies are unchanged. The earlier
`graphmode-warmup-preparation-2026-09-14.sha256` remains a PRE-REPAIR review
fingerprint, not the active freeze manifest; mismatches against repaired files
are expected and must not be silently updated.

The new phase-A source-identity guard must report committed, prior layers
unchanged, and loaded definitions matching this checkout after the commit.
The target, seeds, priors, 1200/1000 schedule with adaptation through800,
5400+600-second budget, and numerical/statistical thresholds are unchanged.
No experiment-registry row or external registration/output directory is created.
Next registration must perform external seed-collision review, bind the actual
new commit/configuration/runtime and a fresh directory, and obtain explicit run
authorization. This freeze is not convergence evidence or formal-run approval.
