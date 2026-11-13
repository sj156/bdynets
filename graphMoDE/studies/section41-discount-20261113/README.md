# Section 4.1 discount-model development

This study prepares and checks Exp-W, SE-W and Connectivity on the preserved D01/D02 synthetic inputs. It is a new sampler and a new output lineage. Historical fits, starts and checkpoints are not reused. No manuscript text or numerical setting is edited.

The user accepted the baseline discount prior Beta(49,1), protection constant log(2), and initial mean (log(25),0,0). The initial mean is explicitly design-informed by the synthetic population level; it is not estimated using true classes or held-out counts. K=10, p=3, C0=diag(1,0.5,0.5), nu=3 and tau=1 remain fixed. Beta(99,1) is reserved for a later sensitivity analysis.

`plan.json` fixes the computational choices, seeds, short smoke phase, subsequent four-start development phase, launch gate and diagnostic thresholds before results are observed. Source commit, per-file hashes, input hashes, binary hash, exact task configuration and fresh external output are registered before any random draw. A 200-sweep chain is a development probe, not a substitute for a converged production run. The 100 retained draws per chain also make the ESS=400 threshold deliberately demanding across four chains.

## Implementation and reuse

Reusable interfaces are in `bdynets/R/gmde-discount.R`, `gmde-dynamic.R` and `gmde-dynamic-diagnostics.R`. New numerical kernels are in `bdynets/src/gmde-discount.cpp` and `gmde-dynamic.cpp`. The experiment runner only configures and calls these functions.

The implementation reuses package scalar validation, stable allocation helpers and sufficient statistics. The inverse-trigamma guide reuses the shared C++ root solver. Exact integer-shape PG checks derive from the existing graphMoDE `core_02_state_update.R` module and call the installed BayesLogit backend. R base LAPACK supplies the graph eigendecomposition; it is not reimplemented. The existing scalar FFBS cannot represent the correlated three-dimensional state, so a multivariate information-form kernel was added. Existing static gmde and GLS APIs and native registration entries remain intact.

The frozen target includes guide covariances during the state proposal, complete guide replay in discount and sequential label MH, joint empty-expert refreshes, all K independent GP columns in ESS and covariance densities, and the correct log-coordinate prior Jacobians. Raw graph weights are unnormalized. The SE exponent has no factor one half. Block FFBS includes both temporal boundaries and time zero in the first block. Covariance sums are symmetrized only at roundoff level inside factorizations; no jitter or eigenvalue clipping is used. Edge-weight underflow stops a task instead of silently changing support. A recorded failure remains a failure.

`verification.json` records fixed-input checks against independent dense Gaussian calculations and complete target ratios. These support the implemented formulas for tested cases; they do not establish empirical mixing or universal numerical stability.

## Reproducibility and outputs

`graphMoDE/scripts/run-section41-discount.R` runs a single registered task against a source-hashed external shared-library build, without installing or updating any package. The driver preserves progress, partial checkpoints, RNG state, errors and completed fits in a fresh external directory. Only reviewed aggregate reports and manifests are intended for Git. Raw observations, historical payloads and new chains are excluded.

Training fit and class recovery can be assessed after fitting. No held-out predictive score is claimed here: the future-guide simulation remains a separate implementation task. No full eight-method comparison or formal simulation is released by this development run.
