# graphMoDE — frozen research source, 2026-09-15

本目录保存截至 2026-09-15 已冻结并实际用于 m1/m4 对照的开发代码。
这是研究快照，不是新的 R 包版本；旧快照和 `bdynets` 包接口保持不变。
**8 条链均完成，但两组统计验收仍未通过，不能视为正式模拟已就绪。**

## Identity and contents

- Executed development commit: `903f4d26a98ec43aba9ab57bc5df1299dea78e81`.
- All 68 entries in the [freeze manifest](docs/provenance/graphmode-refresh-freeze-2026-09-15.sha256)
  are byte-for-byte exports of that commit. They include the 65-file runtime
  identity closure plus freeze/preparation provenance. The manifest and
  [MIT license](LICENSE.md) are also exported from that commit.
- This README, [outcome note](RESULTS.md) and `TRANSFER.sha256` are handoff
  documentation, not changes to the executed source or retrospective registration.
- The development commit is an execution identity. The public transfer commit is
  a separate identity; private development Git history is not uploaded.
- No manuscript, private/raw dataset, MCMC draw, checkpoint, registration RDS,
  private log, reference PDF, cache or third-party PNARM source is included.
  Unfrozen working-tree edits, including the separate D-049 reporting layer,
  are outside this execution snapshot and remain local.

## Code map

| Location | Purpose |
| --- | --- |
| `R/gmde-helpers.R`, `R/gmde-state-update.R` | Reused helper and state-update foundation |
| `R/graphmode-*.R` | Gate, corrected MH, stable FFBS and sampling core |
| `R/graphmode4-*.R` | r4 geometry/design, diagnostics and controlled experiments |
| `R/graphmode_dev.R` | Cached FFBS and passive proposal/allocation diagnostics |
| `R/graphmode_warmup.R`, `R/graphmode_validation.R` | Dispersed-start A/B execution layers |
| `R/graphmode_gate_refresh.R` | Fixed-count conditional gate/guidance composition |
| `R/graphmode_refresh_run.R` | Separate m1/m4 registered comparison executor |
| `scripts/`, `scripts/tests/` | Source-level entry points and fixed-input checks |

For the latest design, see [D-050](docs/GRAPHMODE_GATE_REFRESH_2026-09-15.md)
and [D-051](docs/GRAPHMODE_REFRESH_COMPARISON_2026-09-15.md).
Historical preparation/status text is intentionally preserved for exact source
identity; the completed-run outcome is in [RESULTS.md](RESULTS.md).

## Verify and reuse boundaries

From this directory, these commands only verify file contents:

```sh
shasum -a 256 -c docs/provenance/graphmode-refresh-freeze-2026-09-15.sha256
shasum -a 256 -c TRANSFER.sha256
```

This is an inspectable source archive, **not a portable, immediately runnable
experiment release**. The original guarded executors still require historical
development Git objects and external A-registration/receipt evidence, which are
not included. A clean public checkout alone does not satisfy these guards.
Some tests inspect those identities too. Do not bypass guards, replace the
historical commit with a public commit, or reuse old commands/checkpoints.
Historical local paths and earlier preparation notes are provenance, not files
provided by this snapshot. Older manifests may reference historical audit inputs
outside the current export; use the two manifests above for this transfer.

Dependencies include existing installations of `digest`, `BayesLogit` and
`posterior`; this handoff installs nothing. Portability/package integration and
any future experiment require separate implementation, verification and a new
approved registration with source identity, settings, seeds, budget and fresh
external outputs. Neither formal simulation nor automatic continuation is enabled.
