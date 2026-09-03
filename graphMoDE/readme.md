# graphMoDE development handoff

> **Status: debugging snapshot, not a release. Formal simulation is not
> authorized.** One registered K=10 conditional-allocation diagnostic still
> fails reproducibly; see [`debug/KNOWN_ISSUE.md`](debug/KNOWN_ISSUE.md).

This directory is a self-contained handoff of the graph-informed mixture of
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

## What is included

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

No Wang/Liang traffic or population data, private observations, PNARM source,
raw road data, MCMC chain, terminal state, checkpoint, cache, result ZIP, or
local absolute path is included.

## Model and code map

The implementation combines:

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

## Environment

The failing run used:

- R 4.4.0;
- `BayesLogit` 2.1 for the exact Devroye Polya--Gamma draw;
- `digest` 0.6.38 for SHA-256 identities;
- `posterior` 1.6.1 for some higher-level diagnostic summaries.

The focused input audit and regression test require `digest`; exact failure
reproduction additionally requires `BayesLogit`. The scripts never install or
update packages automatically.

## First checks

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

## Current result

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

## Formal-run gate

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

