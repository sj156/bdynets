# graphMoDE — frozen research code, 2026-09-12

本目录是本轮 Poisson/dynamic 开发代码的冻结交接，不是新的 R 包发布。
现有 [`bdynets` 包](../../../bdynets/)及其 `gmde_*` 接口不变；
[`graphMoDE` 旧研究文件](../../readme.md)也保持原样。

## Version and scope

- Executed development-source commit: `c2968a148fcc5bd9793ca3eabd6260db5f8506b0`.
- Local freeze tag: `graphmode-rho-screen-2026-09-12-v1`.
- The 31 files in the [source manifest](docs/provenance/graphmode-rho-freeze-2026-09-12.sha256)
  are byte-for-byte exports of that commit, including its runtime dependency closure,
  fixed-input tests and required implementation notes. The development Git history
  is intentionally not included. The source commit is an identity, not a promise
  that this commit exists in the public repository.
- The [reviewed result summary](docs/GRAPHMODE_R4_RHO_RESULTS_2026-09-12.md)
  was exported from documentation commit `b4f499196214301d9005830ea3ca382e77d15518`.
  This documentation commit was not the executed source revision.
- This README and `TRANSFER.sha256` describe the handoff. Publication does not
  re-sign old experiments or authorize new ones. No manuscript, private dataset,
  raw chain, checkpoint, registration RDS, private log or third-party PNARM source
  is included. Code retains the [MIT license](LICENSE.md), copyright Sheng Jiang.

## Where to start

| Location | Purpose |
| --- | --- |
| `R/gmde-helpers.R`, `R/gmde-state-update.R` | Reused helper/state-update foundation |
| `R/graphmode-*.R` | Audited gate, MH, stable FFBS, sampler and source guards |
| `R/graphmode4-*.R` | r4 design, diagnostics, selection/forecast contracts, controlled tuning and rho orchestration |
| `scripts/` | Source-checkout entry points; not package exports |
| `scripts/tests/` | Fixed-input checks; not scientific simulations |
| `docs/` | Frozen preparation notes, implementation requirements and reviewed outcome |

The latest executed workflow is described in
[D-041](docs/GRAPHMODE_R4_RHO_EXPERIMENT_2026-09-12.md).
Earlier preparation notes and embedded status strings are retained unchanged for
source identity; they describe their historical stage, not the latest outcome.
In particular, earlier four-chain preparation was subsequently implemented;
the latest rho screen used **two starts per rho**, not a four-chain convergence test.
The old `graphmode-pilot.R` helper module is retained as a dependency; the separate
September 11 legacy pilot CLI is outside this 31-file handoff.

## Outcome and limitations

本轮 rho=1/2/4/8、各两个起点，共 8 个分支均正常完成，数值检查通过。
但起点与计分半段的效率排序不一致，`rho` **仍未决**；保留期分区未移动。
这不是收敛认证，也不是正式模拟结果，不能仅凭接受率决定下一轮设置。
详见[完整结果说明](docs/GRAPHMODE_R4_RHO_RESULTS_2026-09-12.md)。

Full 48-origin refitting, exact external STGNN/PNAR adapters and independently
calibrated formal experiments remain separate work. Do not treat implementation
contracts or fixed-input tests as completed scientific validation.

## Inspect and verify without simulation

From this snapshot directory, verify the exported source bytes:

```sh
shasum -a 256 -c docs/provenance/graphmode-rho-freeze-2026-09-12.sha256
shasum -a 256 -c TRANSFER.sha256
```

The older P2 manifest is historical audit evidence, not the current complete
export manifest; its non-runtime entries are intentionally not all included.

With existing R dependencies available, fixed-input checks are exposed through
`Rscript --vanilla scripts/graphmode.R check`,
`scripts/graphmode-r4.R check`, `scripts/graphmode-r4-pilot.R check`,
`scripts/graphmode-r4-tuning.R check` and
`scripts/graphmode-r4-rho-experiment.R check` (use `Rscript --vanilla` for each).
Dependencies include `digest`, `BayesLogit` and `posterior`; nothing auto-installs.
Some checks inspect Git/source identity, so use a clean Git checkout, not a ZIP
without Git metadata. This upload only verifies transfer/static integrity; it
does not rerun the historical tests or experiments.

Any future stochastic run requires a fresh approved registration binding its
actual public-checkout commit, source bytes, runtime, configuration, seeds,
budget and a new external output directory. Keep outputs outside the entire
shared repository. Do not reuse old registrations/checkpoints or weaken guards
to substitute the publication commit for the historical execution commit.
