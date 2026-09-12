# graphMoDE: graph-informed mixtures in bdynets

The maintained graphMoDE implementation is now a module of the
[`bdynets` R package](../bdynets/README.md). Its source is in
[`../bdynets/R/`](../bdynets/R), in scripts named `gmde-*.R`.
This research folder preserves the earlier code snapshot, supporting data,
provenance and debugging material.

## September 12, 2026 frozen research code

The latest Poisson/dynamic research handoff is in
[`snapshots/2026-09-12-rho-screen-v1/`](snapshots/2026-09-12-rho-screen-v1/).
It preserves 31 runtime/source/test/requirement files from execution commit
`c2968a148fcc5bd9793ca3eabd6260db5f8506b0`, with SHA-256 manifests and a
[reviewed short-test summary](snapshots/2026-09-12-rho-screen-v1/docs/GRAPHMODE_R4_RHO_RESULTS_2026-09-12.md).

This is a separate source-checkout snapshot, **not an update to the installed
package or its `gmde_*` interfaces**. All eight rho-screen branches completed
and numerical checks passed, but rho selection remains unresolved and
convergence is not certified. No formal simulation is authorized by this
handoff. Raw data, chains, checkpoints and private logs are not uploaded.
The maintained package and September 3 snapshot described below are unchanged.

## Install and use the maintained module

After the package changes are pushed to GitHub:

```r
install.packages("pak") # only if needed
pak::pak("sj156/bdynets/bdynets", dependencies = TRUE)
library(bdynets)
```

Before pushing, run `pak::pak("../bdynets", dependencies = TRUE)` from this
`graphMoDE/` folder to install the adjacent local package.
The former standalone `graphMoDE/packages/graphMoDE` path is retired; use
`library(bdynets)` for the integrated module.

| Entry point | Purpose |
| --- | --- |
| `gmde_graph()` | Validate graph IDs and construct the graph covariance |
| `gmde_expert()` | Specify Gaussian/static experts and proper priors |
| `gmde_gate()` | Choose adaptive, graph-free or forced graph guidance |
| `gmde_mcmc()` | Set iterations, warmup, thinning and seed |
| `gmde_fit()` | Sample the fixed-K model with whole-series allocations |
| `gmde_partition()` | Compute co-clustering probabilities and a representative partition |

The [package README](../bdynets/README.md) contains a complete runnable example.
See also the [integration guide](GRAPHMODE-PACKAGE.md) and
[repository migration record](../MIGRATION.md).

## Current package scope

The integrated reference implementation supports complete Gaussian panels with
static experts and the revised adaptive structured-plus-independent graph gate.
Each series has one cluster label for its entire history. K is fixed, empty
components are allowed, and graph and panel IDs must agree. Raw chain labels
are retained; partition summaries use posterior co-clustering probabilities.

The migration passed 148 assertions, R CMD check with Status: OK, and local pak
installation. Default chain lengths are smoke tests, not an inference protocol.
Poisson/static with exact PG plus MH is the next implementation milestone,
followed by dynamic engines, prediction and posterior calibration. The Poisson
code in the historical snapshot below is not the current package's supported
graphMoDE engine.

## Preserved September 3, 2026 handoff

> The historical snapshot remains a debugging artifact. Its registered K=10
> conditional-allocation diagnostic failure is unresolved; see
> [`debug/KNOWN_ISSUE.md`](debug/KNOWN_ISSUE.md). The package migration did not
> repair or validate that diagnostic.

The original contents of this directory are a handoff of the graph-informed mixture of
dynamic experts (graphMoDE/GMDE) code used on 3 September 2026. It is intended
for Sheng Jiang to continue debugging without receiving the hundreds of
megabytes of MCMC chains, checkpoints, private working files, or the full
OpenStreetMap road database.

The 16 files in `R/` are byte-for-byte identical to the frozen source embedded
in the latest failed diagnostic archive. Their identity is recorded in
[`provenance/FROZEN_SOURCE_SHA256.csv`](provenance/FROZEN_SOURCE_SHA256.csv).
Git commit `ec79ed13f2844e814b88a986eeaa1abdd96d76f0` is only the base commit: the
executed development source was in a dirty worktree, so the file hashes—not
that commit alone—identify this snapshot.

### Historical snapshot contents

- `R/`: the exact 16-file implementation snapshot.
- `workflows/`: readable guides corresponding to `2helpers`, `4gibbs`, and
  the current `5simulations` debugging status. These do not duplicate the
  implementation and do not start a run when opened or knitted.
- `data/osm-derived/`: the selected 100 locations and the minimum q=4 road
  graph inputs. The large source road database and road-segment table are not
  distributed.
- `data/debug/`: truth-blinded synthetic counts, reviewed K=10 starts and
  seeds, rule tables, trace fingerprints, and the 824-byte failure record.
- `figures/`: the reviewed 100-node road-network figure.
- `scripts/`: portable source, input-audit, test, and one-chain failure
  reproduction entry points.
- `tests/`: a focused exact-enumeration and RNG-passivity regression test for
  the conditional-allocation diagnostic.

The historical handoff excludes Wang/Liang traffic or population data, private observations, PNARM source,
raw road data, MCMC chain, terminal state, checkpoint, cache, result ZIP, or
local absolute paths in its distributed source and input files.

### Historical model and code map

The September 3 implementation combines:

1. Poisson dynamic experts with a Metropolis-corrected
   negative-binomial--Polya--Gamma proposal;
2. information-form forward filtering followed by backward sampling (FFBS)
   for state paths;
3. a graph classifier updated jointly by elliptical slice sampling;
4. categorical allocation updates for GMDE-W, GMDE-C, Euc-MDE and MoDE, plus
   a Potts allocation alternative;
5. road-based, binary-connectivity and Euclidean graph constructions;
6. fail-closed experiment registration, runtime, checkpoint and diagnostic
   helpers.

[`CODE_MAP.md`](CODE_MAP.md) explains the responsibility of every source file
and the relationship among the five internal comparison methods.

### Historical environment

The failing run used:

- R 4.4.0;
- `BayesLogit` 2.1 for the exact Devroye Polya--Gamma draw;
- `digest` 0.6.38 for SHA-256 identities;
- `posterior` 1.6.1 for some higher-level diagnostic summaries.

The focused input audit and regression test require `digest`; exact failure
reproduction additionally requires `BayesLogit`. The scripts never install or
update packages automatically.

### Checks for the historical snapshot

These checks concern the preserved snapshot, not the installed package tests.
Start R in this `graphMoDE` directory, then run:

```r
source("scripts/check_inputs.R")
source("scripts/run_tests.R")
```

These are short checks. They do not launch the formal simulation and do not
write scientific results.

To replay only the known single-chain failure and capture its first offending
probability vector in a git-ignored directory, run:

```r
source("scripts/reproduce_k10_failure.R")
```

The replay uses `n=100`, `T=168`, `K=10`, graph-basis rank `m=40`, fixed
`rho=1`, 3,000 transitions, 1,000 burn-in transitions, substantive component
threshold 5, mode A and seed 2026094102. It can take several minutes. It is a
debug replay, not a simulation study.

### Recorded K=10 diagnostic result

The parent K=10 scientific traces were verified before the passive diagnostic
was added. In the latest passive-diagnostic run, 11 of 12 registered tasks
completed; `GMDE-W-mode-A-seed-2` stopped after about 307 seconds with:

```text
Poisson-binomial threshold recursion failed its mass audit.
```

The failure arises while calculating deterministic, pre-draw diagnostic
summaries. The evidence currently isolates the failure to that diagnostic path;
it does **not** yet establish whether the underlying cause is a mathematical
error, a floating-point implementation error, or an invalid input produced
upstream. It also does not establish that the main sampler is either correct or
incorrect. See the known-issue document for the exact call chain and acceptance
criteria.

### Historical formal-run gate

Do not start, report, or publish a formal simulation from this snapshot. The
debug blocker is cleared only after all criteria in
[`debug/KNOWN_ISSUE.md`](debug/KNOWN_ISSUE.md) pass. Clearing this one blocker
still does not automatically authorize the formal simulation; the scientific
design and runtime gate must be reviewed separately.

## Data and licensing

Code in this handoff is provided under
[`LICENSE-CODE.md`](LICENSE-CODE.md). The files in `data/osm-derived/` and the
map-like road figure are based on OpenStreetMap information and are separately
subject to the Open Database License (ODbL) 1.0. Map/data attribution:

**Map data © OpenStreetMap contributors; available under the Open Database
License (ODbL) 1.0.** See the
[OpenStreetMap copyright page](https://www.openstreetmap.org/copyright/) and
the [ODbL 1.0 legal text](https://opendatacommons.org/licenses/odbl/1-0/).

The road lines in the figure use light, medium and dark styling only to depict
local, middle-class and major road classes. Those shades are background map
styling; they do not represent estimated weights, uncertainty, traffic volume,
or simulation results. The thin gray qNN edges connect selected nodes for the
statistical graph and are not drawn road routes.
