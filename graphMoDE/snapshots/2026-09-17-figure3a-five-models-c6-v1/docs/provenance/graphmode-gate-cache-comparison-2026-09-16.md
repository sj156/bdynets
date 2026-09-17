# D-066 prospective paired cache comparison provenance

- User reports the D-065 audit passed and requests continuation. Scope here is
  a separate executor/design and fixed-input checks, not freeze, registration,
  simulation, old-result reanalysis, publication or formal release.
- Parent HEAD remains `c961923f867e2886c1a5ab5683997a1aa60eb304`.
- Actual independent audit: local temporary directory
  `graphmode-code-audit-20260916-factor-cache-v9t2w7f9`, `audit.md` SHA-256
  `1d05db1bdd97e31a494e83a48b93fb5d06b0fe82df57eb051ffeec013f68adcf`.
  It passed22 existing and7 independent fixed-input groups. Verified associated
  `existing-checks.log` SHA-256
  `a8fbc830c75440f2358ac99e1f8a625566d4b98daaa204b4334c73f7ddf1e3f1`,
  `independent-check.R` SHA-256
  `8df3d02bf02fe7f347e0c49922328f7bb2d19461cf39016ff36c86b18d83dcab`,
  and `independent-check.log` SHA-256
  `43b04a53fca882895c3936b69c427dc15830905fc6ab45b9ccdc323862d2cb21`.
  These checks are reused, not presented as an audit of new D-066 integration.
- D-065 source pins match exactly:
  - `R/graphmode_gate_factor_cache.R`: `c55259b982b48ee37c60ebb4be2aea2d7a41734fa8c7c6a993036884e9bcc06c`
  - `scripts/graphmode-gate-factor-cache.R`: `4322cf08a8341112d78c4083ea3224d0526821647c74bd1ab53a1da108399e37`
  - `scripts/tests/graphmode-gate-factor-cache-deterministic.R`: `e608368eb238cc14c4cd64df46b0e40e7a2706cd15dfe671ad3c3af0f6cb12a2`
  Its guide/provenance and all92 frozen parent entries are also unchanged.
- D-064 is not part of that audit. Only its exact one-expression checker
  correction is reused by the new private report adapter; the c3 repair entry,
  old-result reading, report reconstruction and supplemental output flow are
  never invoked. The unchanged module pin is
  `bf42af3108b9fdcfccd33e8a04a88c52b33c08d50d12b8252446e93e75f444be`.
  Legitimate success, PSM-only failure and unchanged threshold boundaries are
  explicitly tested through the new complete report adapter.
- New locally authored source reuses own existing controllers and fixtures;
  no external scientific source is imported. Source hashes:
  - `R/graphmode_gate_cache_run.R`: `80bb1d6568b8f7f185b87b7536cab5ad8b01f07c3c8f5f42b5271e28e3d329ea`
  - `scripts/graphmode-gate-cache-run.R`: `419aff920746b433be3570d3480c04dd0dd85cd6092796e36888bd6a31a94e4d`
  - `scripts/tests/graphmode-gate-cache-run-deterministic.R`: `434b2b7b64b4ead7f88256ac846cd7a6bf9cb313eb1135f52132957c27a68352`
- Source identity covers105 files, including source/tests/design and all old
  dependencies. Loaded definitions match; committed isFALSE as expected.
  This is a preparation identity, not a frozen execution commit or registration.
- Proposed design is reference/cached contrast ESS, both expert block42/m4/
  provisional rho4/exact A scale, four1200/600 each, paired complete starts and
  sampling seeds. Four independent streams, not eight; compare complete-worker
  time and require bitwise scientific/RNG equivalence. All37 scalar/six-PSM
  checks remain. No empirical speed or mixing outcome is supplied this turn.
- 24 unique candidate seeds differ from current D-051/D-056/D-061 code lists;
  full historical collision review remains pending before registration. The
  proposed external basename `graphmode-gate-cache-20260916-c4` does not exist
  and is not created. Science16200s/diagnosis1800s are proposed child-process
  caps; parent evidence verification has additional cost.
- Final command: `LC_ALL=C Rscript --vanilla scripts/graphmode-gate-cache-run.R check`.
  All30 fixed-input/mock-process groups pass. No real PG/MCMC/scientific child
  or historical-result diagnostic rerun; RNG state/kind restored unchanged.
  Mock records/results are retained only under the local temporary basename
  `graphmode-gate-cache-run-fixed-e939c27fb4f`.
- Development evidence directory: `graphmode-d066-executor-gceqp0vk` outside
  Git. Final `checks-04.log` SHA-256:
  `2882c1a2bab6d7e7be1c7db708d3e28f2584388dc3939d1f103100e872cdf675`.
  Earlier logs remain; the first test run exposed an old fixture's renamed
  arm being passed as an ESS scheme. Only the test was corrected to invoke
  the cached conditional kernel with scheme contrast; production code did not
  require a correctness fix. Later checks add explicit paired/receipt coverage.
- Before edits, hashes of299 tracked/untracked existing files were saved.
  Only current inventory, lineage, decisions, validation and changelog notes
  are updated; all other prior files and existing user changes remain intact.
  Experiment registry, canonical manuscript/result pointers and old outcomes
  do not change. No independent new integration audit, commit or upload.
- Final verification confirms exactly five existing note files updated and
  five new files added; the other294 existing files are byte-identical, including
  all five D-065 files and all92 frozen entries. New source/log pins match.
  Three new R files parse, status and `git diff --check` pass, and the final
  105-file loaded source identity matches. Evidence: `final-verification.json`
  and non-registration `source-plan.rds` in the temporary development directory.
  No policy/imported-source change occurred, so the unrelated repository-policy
  check is not rerun or claimed repaired.
