# D-060 local development provenance

- Date: 2026-09-16. User authorizes focused diagnosis and optimization, not a
  new simulation, diagnostic acceptance rerun, source freeze or upload.
- Parent execution HEAD: `d357f04ec25dfe41068f03dd0689712646048fe5`.
- Parent registration: `5685a4cb31d8f57b21d7c90d5f4c2601082b98c79a32145c75a389ee072123c2`.
- Saved c2 report file SHA-256:
  `966d9c65d6513231f542610e4faec57a5cfb507dfb04597dc1fe5932a50893a7`.
- Verified accepted serialized report digest:
  `96688773a72fa92ee5ed9cde9dff49a16690276c9718b6918b6f0cb1e9642b0d`.
- One post-hoc read-only inspection at
  `/private/tmp/graphmode-focused-c2-20260916.5TXWHx/inspect.R`, SHA-256
  `dd60929baf3577b0564a9428e8a13431e51368000a83995d5dbd46e90d76564c`.
  It reads the accepted report/blinded inputs, never truth for parameter
  selection; computes descriptive means/ACF/correlations only. RNG unchanged.
  Original result directory is not written. The temporary script is local
  evidence, not a portable public run entry or permanent result baseline.
- Actual local Dropbox `graphMoDE-Appendix.tex` SHA-256:
  `39ff8a8be492a3bd40d5a4d32e513441ed971a29e013ef98d2474284a83f878d`.
  Relevant lines497–528 define independent white coordinates, linear utilities,
  and whole-vector ESS;538–564 define guidance joint target/composition. The
  new conditional-coordinate scan is a derived candidate, not already the
  paper's explicitly prescribed whole-vector step. No cloud-sync assertion or
  canonical manuscript promotion.
- Local implementation reuses D-050 composition, original guidance MH and
  D-055 expert-block driver by private bindings. No imported scientific source.
- New source pins:
  - `R/graphmode_gate_blocks.R`: `f9a36d04681b33f4a2229c77d86b7eadc7ce41353f42f7beba6c302ec0403258`
  - `scripts/graphmode-gate-blocks.R`: `86ecc09735392f2fcbdf243577e55acae81e36f905db0277480b9e4afb5a3dc9`
  - `scripts/tests/graphmode-gate-blocks-deterministic.R`: `c3ea2d149e78b22fde1df512dfab3768f2a99df4e35837ce5127b31341f840a6`
- `env LC_ALL=C Rscript --vanilla scripts/graphmode-gate-blocks.R check`:
  24 fixed-input groups pass, RNG state/kind unchanged, all80 parent frozen
  entries exact. No independent review or stochastic-performance claim yet.
- Original statistical decisions, thresholds, registry and result identities
  remain unchanged. No run adapter, package export, registration, commit or push.
