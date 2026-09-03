# Snapshot provenance

| item | value |
|---|---|
| handoff date | 2026-09-03, Asia/Shanghai |
| source repository base HEAD | `ec79ed13f2844e814b88a986eeaa1abdd96d76f0` |
| source worktree at execution | dirty; file hashes are authoritative |
| sampler version | `exact-poisson-info-ffbs-joint-ess-devroye-2026-08-31-v2` |
| road workflow | `countdlm-central-beijing-road-candidate-2026-09-02-v2` |
| selection seed | `2026090201` |
| selected road-vertex ID hash | `3e76c75c869c35774f167f08280e6e44f8104c8cd0f6fa3a31d7f63dc27e8dbf` |
| excluded original context SHA-256 | `992e41c34dff68f876a04e4ed63814f48c57592a6c600f4f9140070318521bc0` |
| excluded full road graph SHA-256 | `3fde9b8b64048ebb3be1e72834edd86ca0f589684fa8a0673c02db21194602ae` |
| K=10 blinded `Y`/`Fmat` hash | `1e5d62f5c8d2978b62c862dbb3d30e84b217af22b87208563f41fc08742ce845` |
| failed diagnostic ZIP SHA-256 | `bd7f945165dc99282fa2914b684174ced85e0bf7c3cf5b96ac54a6931712f4d9` |
| failed diagnostic status | `RUN-INCOMPLETE`; formal simulation not launched |

`FROZEN_SOURCE_SHA256.csv` was extracted directly from the failed diagnostic
archive. Every listed hash matches the corresponding file in this handoff's
`R/` directory. `CHECKSUMS.sha256` covers the final public handoff after all
documentation and portable scripts were added.

Excluded material includes all large fit objects, terminal states,
checkpoints, diagnostic arrays, parent archives, the full road graph, private
local provenance, real traffic/population data, reference PDFs and caches.

