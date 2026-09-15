# D-041: authorized isolated rho experiment

User confirmed D-040 on 2026-09-12. This supersedes preparation-only status in
the historical tuning guide. Formal simulations and automatic extensions remain
unauthorized. Audited tuning base: 7a6fda1e45d2810219de3103caf5408fbe9172b2.

Only the orchestration layer is added. It calls the existing deterministic
D-036 panel constructor, original initial-state constructor, v3 rho compiler,
fresh-process v3 controlled launcher/worker and strict evidence reducer. The
FFBS/PG/MH/gate, guidance .25 and all numerical thresholds remain unchanged.

One fresh synthetic graphMoDE-W panel: intertwined spiral, 121 units, 168 times,
road-Voronoi truth with 5 profiles, Kfit=10, amplitude .3, innovation variance
.002, same provisional priors as D-036. Truth is retained for generation
provenance only; no ARI or truth-based initialization/selection. D-036's nested
panel specification documents these constants, NOT an additional old run.
Its old seeds, four-start execution and PG moment screen are not executed.

New seeds: innovations 2026091251; profile permutation 2026091252; unit
permutation 2026091253; responses 2026091254; truth-blind balanced starts with
3/7 occupied classes use 2026091261/2026091262. Chain seeds
2026091271/2026091272 are shared across rho candidates within a start stratum.
Candidates 1/2/4/8 start independently, ordered start-1 then start-2, each in
increasing rho order. Each gets 400 sweeps, 200 settling, thin=1, checkpoint=50.
No guidance adaptation, carry-over, old checkpoint, package install or retry.

Register source closure/commit/runtime/seeds/role/new canonical external output
BEFORE data draws. Persist the generated panel and all eight signed plans before
any chain. Each branch uses a fresh public controlled CLI process, which itself
starts the audited worker. Worker hard budget 600 seconds; dispatcher 630;
whole batch hard budget 3600 seconds (including data generation and branch
pre/postflight, excluding later read-only diagnosis). Reserve a full dispatcher
plus 30 seconds before starting another branch; insufficient time leaves those
branches unrun and rho unresolved. First failed branch stops the batch without
retry. Keep all failures, partial outputs, raw dispatcher exits and audited
worker final receipts. Never fabricate acceptance for a saved result.

Predetermined single-sweep movement share maximum .20. Score only retained
occupied-expert accepted information movement divided by ALL expert update
seconds, including empty priors. Require agreement across starts and the two
100-sweep scored halves using the audited v3 reducer. Even a resolved result is
only a single-method development candidate, not the paper's common rho.

Read-only follow-up: strict evidence checks, per-expert rejection/movement,
numerical protection values, guidance acceptance, occupancy, and pairwise PSM
per rho. Two starts per rho cannot invoke the four-chain validity contract or
certify convergence. Do not pool different-rho chains as that four-chain test.

The CLI has no default stochastic action. `prepare` and `preflight` draw nothing;
`run REGISTRATION.rds --authorized` runs only this registration. `terminal.log`,
`batch-execution.rds`, and `run/report.rds` preserve output/status. No shared
folder changes, canonical manuscript promotion or Git push.

Verification: 13 new fixed-input/mocked-dispatch checks and the existing 46
controlled-tuning groups passed. The audited four-file v3 manifest remains
unchanged. These tests generated no scientific random input or chain.
