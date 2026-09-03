# Suggested pull request

Title:

```text
Add graphMoDE simulation debugging handoff
```

Body:

```text
This PR adds the current graphMoDE/GMDE development snapshot for joint debugging.

Included:
- the exact 16-file R source snapshot used by the 2026-09-03 K=10 diagnostic;
- the minimized 100-node q=4 road-graph inputs and reviewed network figure;
- truth-blinded synthetic K=10 input, starts, seeds, trace fingerprints and the small failure record;
- source/input integrity checks, focused conditional-allocation tests and a one-chain reproduction script;
- English/Chinese code, data, provenance and known-issue documentation.

Current blocker:
- 11/12 passive conditional-allocation diagnostic tasks completed;
- GMDE-W-mode-A-seed-2 (seed 2026094102) stopped with "Poisson-binomial threshold recursion failed its mass audit.";
- formal simulation remains explicitly unauthorized.

Large chains, checkpoints, result ZIPs, the full OSM road database and all private traffic/population data are excluded. OSM-derived graph data and the figure include ODbL attribution.
```

