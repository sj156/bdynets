# Section 4.1 discount-model development

Completed development run: all 30 registered tasks finished without numerical failure, and all 24 terminal cache checks passed. However, **0 of 28 convergence checks passed**. Recorded nonempty-class counts stayed at the four initial values (1, 3, 7, 10) across every chain. These are failed-mixing development results, not a formal method comparison. See [the English report](results/report.html), [report source](results/report.Rmd), [diagnostics](results/diagnostics.csv), and [source/output manifest](results/run-manifest.json).

The exact local runtime code commit is `54c5abdc96b54d6e7bedf946bd98613d4683c8ff`. Its [published source commit](https://github.com/sj156/bdynets/commit/7970718751cb1dc930fa10da33e13f1e307717b4) has the identical complete tree `61e622305d43c3eee56022e7144101d2fd8e022f`; GitHub API publication changes commit metadata only. Later commits add reporting and metadata, not a different sampler for these results.

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

## Reproduction entry points

These commands require an explicitly authorized new run and a fresh external output directory. They do not install packages. The original launcher and registration are archived externally and hashed in the manifest; the portable CLI wrappers below were syntax-checked after the completed run.

```sh
python3 graphMoDE/scripts/prepare-section41-discount.py REPOSITORY ARCHIVE NEW_OUTPUT
python3 graphMoDE/scripts/launch-section41-discount.py REPOSITORY NEW_OUTPUT
Rscript graphMoDE/scripts/summarize-section41-discount.R REPOSITORY NEW_OUTPUT NEW_SUMMARY
```

Render `results/report.Rmd` from its directory with `rmarkdown::render("report.Rmd")`, or supply a newly generated summary directory through the `summary_dir` parameter. The historical initialization and raw payloads are never loaded into the current sampler. The broader repository-policy verifier in the separate old working directory still reports a pre-existing absolute path in `R/graphmode_c9_pilot.R`; that file was not changed or copied into this checkout.
