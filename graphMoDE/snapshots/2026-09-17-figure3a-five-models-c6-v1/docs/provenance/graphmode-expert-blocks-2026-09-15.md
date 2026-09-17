# D-055 expert time-block development provenance

2026-09-15. User requests continued diagnosis/optimization. Uncommitted,
locally authored addition on HEAD903f4d26a98ec43aba9ab57bc5df1299dea78e81;
this record is not a source freeze, run authorization or public synchronization.
All prior mixed working changes remain separate.

## Existing evidence reused

- `docs/GRAPHMODE_REFRESH_RESULTS_2026-09-15.md` and the previously completed
  focused review, external basename `graphmode-refresh-focused-20260915.YCFVf3`.
  Its `diagnosis.md` SHA-256 is
  `03959ac5a0f6c8809aeddf7bd0a1b6f7e1f9d7b03c7997fbd6d3b981ae0d27fc`.
  Only this saved account is reread; its scripts, raw traces, figures and private
  logs are not imported or rerun. Original c1 report identity remains
  `a788fd4b6c332b6e4894286d022c4d1ca7f8ba671e2b433c7b63afa5cfe53a88`.
- Applicable actual local Dropbox `graphMoDE/paper/graphMoDE-Appendix.tex`,
  especially the marginal PG/MH argument and contiguous-block fallback:
  SHA-256 `39ff8a8be492a3bd40d5a4d32e513441ed971a29e013ef98d2474284a83f878d`.
- Companion computation source SHA-256
  `36bf053c3de4be4746d0435e3b9c6184d52a58c1c6f42e159fd1d03f76c87bcb`;
  Section5 SHA-256
  `7a0db4db8cd8ee2f4780729fad4b466751325bb21e1aaf1eb740c08886828096`.
  No paper file is edited or copied; canonical manuscript pointers are unchanged.

## Newly authored implementation identity

```text
cc3bb0c18afe25adb11a988e185fba149b250b733406bfd49da8428bf5214122  R/graphmode_expert_blocks.R
5183e33ddf42c4b5a18b6cabd4ae46b381c9923e4f3a9fb70486313e9d222bdc  scripts/graphmode-expert-blocks.R
d5822964a79df5a92c843e678b721e35c05807395b39dda852da04861c71e699  scripts/tests/graphmode-expert-blocks-deterministic.R
31b9398b3c6c45026d1cb82e9c64ceec0470256fc4d974281e3e01990ab08256  docs/GRAPHMODE_EXPERT_BLOCKS_2026-09-15.md
```

Original68 entries in `graphmode-refresh-freeze-2026-09-15.sha256` still match.
Existing FFBS/PG/MH, D-043 cache/observer, D-050 gate composition and D-051
execution/diagnostics are unedited. New code privately calls those functions;
only the first left-boundary Gaussian prediction and terminal right-boundary
conditioning are routed through the new block filter. Original numerical guard
thresholds are untouched. No historical code import or package interface change.

## Checks and limits

`Rscript --vanilla scripts/graphmode-expert-blocks.R check`, with LC_ALL=C and
single-thread BLAS settings: all26 fixed-input groups pass. Preset normal,
uniform and PG interface outputs exercise the actual composition; untaped
random calls fail. RNG kind/state is unchanged. Full-block state/draw-order
equivalence is checked against D-050 for m1/m4, both cache modes. Independent
dense Gaussian conditioning verifies complete short-block moments and a168-time
three-dimensional bridge mean. Enumerated16-state transition matrices check
local MH invariance and systematic composition, not real-PG distribution or
empirical mixing. Boundary errors, all rejections, empty/zero-count experts and
later-block failure are explicitly covered. Status succeeds; run is refused.

No old scientific diagnostic or broad old test suite is rerun. No repository
policy or imported-source change is made, so the full repository-policy audit
is not repeated for this scoped authored module. Existing audit-path portability
issues are not claimed fixed. Whitespace checks cover the new files and scoped
operational notes. The experiment registry and all prior manifests stay unchanged.

No execution adapter, chosen block length, resolved fallback trigger, new seed,
run budget, registration, source freeze/commit, package installation, shared-folder
sync or GitHub push. The old commands do not load this underscore-named layer.
Both c1 arms and old B keep their original validity failures. Candidate review
and prospective block-aware comparison design must precede any new execution;
formal simulation remains unauthorized.
