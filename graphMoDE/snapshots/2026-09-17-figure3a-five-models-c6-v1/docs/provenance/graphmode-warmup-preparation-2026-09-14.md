# D-045 phase A preparation provenance

Date: 2026-09-14. This is an uncommitted implementation fingerprint for focused
review, NOT a frozen run registration, independent audit pass or scientific result.

The matching five-file SHA-256 list is
`graphmode-warmup-preparation-2026-09-14.sha256`. Development baseline remains
the D-044 commit `f6e02056cb4adcce94cdf728c9751833d7327b5e`; it does not identify
these new files. All 31+4+4 files in the original rho/development/dev-run frozen
manifests verified unchanged.

Validation: the new entry's `check` command passed all 28 fixed-input/mock-process
groups with C locale and one numerical thread. The last completed check printed
`PASS: 28 fixed-input/mocked-process groups; no scientific simulation.`
Focused R parsing and usage checks returned no findings; `git diff --check`
passed. Tests used fixed numeric tapes, no actual PG draws, and restored/verified
the original RNG kind and state. Temporary test artifacts were isolated from
scientific outputs; raw validation console output is retained in the local
`graphmode-warmup-validation-20260914-aSKEc5zI` temporary directory.

No source commit, experiment registration, actual initialization draw, simulation,
old-result diagnostic rerun, package installation, manuscript/public-interface
change, upload or phase-B launch was performed. The plan/implementation guide,
decision log, inventory, lineage and changelog record implementation-only approval.
The existing experiment registry is intentionally unchanged.

Review next: initialization streams/prior indexing, private D-044 adaptation,
trace integrity/timing, storage/timeout/exit receipts, one-shot diagnostic entry,
and prospective phase-A scale rules. Source freezing, external seed-collision
review/registration and explicit launch approval remain separate steps.
