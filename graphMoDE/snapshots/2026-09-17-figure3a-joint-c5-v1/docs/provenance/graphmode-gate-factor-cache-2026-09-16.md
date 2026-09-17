# D-065 local guidance-factor cache provenance

- User request: continue optimization after the saved-c3 diagnosis. Authorized
  work is local candidate implementation and fixed-input validation; no new
  simulation, old-statistic rerun, source freeze or synchronization is performed.
- Parent HEAD: `c961923f867e2886c1a5ab5683997a1aa60eb304`.
- Reused scientific kernel: D-060, `R/graphmode_gate_blocks.R`, SHA-256
  `f9a36d04681b33f4a2229c77d86b7eadc7ce41353f42f7beba6c302ec0403258`.
  All92 entries in `graphmode-gate-block-freeze-2026-09-16.sha256` match.
- Motivation reuses the preceding saved-c3 comparison, without reopening a
  scientific run or computing Rhat/ESS/PSM. Source report SHA-256 remains
  `58099497324217a10cda763e22a2621db41377c4e73029db76d643faf9b5c40a`;
  supplemental receipt SHA-256 remains
  `34fb735d5e4f78daa5382e120993130da5d588bfee21ad444d9a0daf7c48d568`.
  Prior read-only comparison evidence is under the local temporary basename
  `graphmode-c3-readonly-20260916-yjnNlQ`. This work does not rewrite either file.
- The new locally authored module reuses the original utility function and
  D-060/D-050/D-055 composition through private bindings. It caches only the
  pure factors of fixed v/K/guidance within ONE ESS scan. The cache never spans
  an MH update or a new ESS invocation. No imported source or paper change.
- New source hashes:
  - `R/graphmode_gate_factor_cache.R`: `c55259b982b48ee37c60ebb4be2aea2d7a41734fa8c7c6a993036884e9bcc06c`
  - `scripts/graphmode-gate-factor-cache.R`: `4322cf08a8341112d78c4083ea3224d0526821647c74bd1ab53a1da108399e37`
  - `scripts/tests/graphmode-gate-factor-cache-deterministic.R`: `e608368eb238cc14c4cd64df46b0e40e7a2706cd15dfe671ad3c3af0f6cb12a2`
- Final fixed-input check log: `graphmode-d065-cache-mzxi196v/checks-04.log`
  outside the repository, SHA-256
  `62023db75fde42302bcfa3c4d1bdb6f218eba5093e51ef14a2eb253676155bbf`.
  All22 groups pass; only mocked normal/uniform/PG interfaces are used.
  RNG kind/state unchanged. Earlier local harness/debug logs are retained;
  an initial environment-assignment setup error and then direct comparison of
  wall-clock expert timing fields were corrected in the test harness only.
- Five alternating paired batches of40 reset fixed-input scans each have
  median original/cached time ratio1.358752. Every call starts at the same
  artificial fixture; outputs are never chained. This is microtiming, not
  complete-worker speed, empirical ESS improvement or statistical calibration.
- Before editing, hashes of294 tracked/untracked existing files were recorded
  in the local temporary `repo-before.json`. Only current inventory, lineage,
  decision, validation and changelog notes are intentionally appended/updated;
  new implementation/CLI/tests/guide/provenance files are additive. All other
  existing files, including the receipt-repair code and frozen sources, remain.
- Final comparison verifies exactly those five existing note files changed;
  the other289 existing files are byte-identical and exactly five new files
  were added. Source pins and all92 frozen entries verify again. Three R files
  parse; CLI status and `git diff --check` pass. Local evidence is recorded in
  `final-verification.json`. No policy/imported-source change occurred, so the
  unrelated whole-repository policy check was not rerun or claimed repaired.
- No registered executor, new experiment, canonical pointer change, commit,
  upload, independent audit, simulation or formal-run release. The experiment
  registry remains unchanged. Original statistical failures remain.
