# Code map

All scientific functions live in `R/*.R`. The R Markdown files are explanatory
entry guides only; they do not contain a second implementation.

## Core sampler

| file | responsibility |
|---|---|
| `R/bdynets-package.R` | marks this tree as the current exact-target development candidate; no formal result claim |
| `R/gmde-helpers.R` | input validation, linear algebra, graph bases, initialization, label handling and sampler version |
| `R/gmde-state-update.R` | exact Devroye Polya--Gamma draws, information-form FFBS proposal and Poisson Metropolis correction |
| `R/gmde-classifier.R` | graph-classifier utilities, joint elliptical slice update, allocation probabilities and passive conditional-allocation diagnostics |
| `R/gmde-mcmc.R` | end-to-end GMDE, graph-free MoDE and Potts-MDE MCMC loops, fixed/calibrated rho controls, stored traces and terminal-state contracts |
| `R/gmde-heterogeneous.R` | exact-target variant for expert-specific state dimensions |

The sampler identity in this snapshot is:

```text
exact-poisson-info-ffbs-joint-ess-devroye-2026-08-31-v2
```

## Road design and method comparison

| file | responsibility |
|---|---|
| `R/gmde-road-design.R` | creates/audits the central-Beijing sampling window, spatially balanced 5x5-by-4 point sample, road distances, qNN graph and figure; rebuilding requires the excluded full road database and `dodgr` |
| `R/gmde-road-benchmark.R` | moderate synthetic DGP, method-specific graph inputs, five-method dispatcher, quick timing and benchmark summaries |
| `R/gmde-road-calibration.R` | blinded rho calibration and short stability pilot |
| `R/gmde-road-stability.R` | longer truth-blinded stability follow-up |
| `R/gmde-road-targeted-stability.R` | targeted follow-up for unresolved graph-gating variants |
| `R/gmde-road-partition-diagnostic.R` | paired K=10 partition-mode restart diagnostic |
| `R/gmde-road-k6-diagnostic.R` | K=6 target-sensitivity diagnostic; it does not validate K=10 |
| `R/gmde-road-conditional-allocation-diagnostic.R` | passive K=10 allocation-mechanism diagnostic that exposed the current failure |

Several high-level diagnostic functions deliberately require exact hashes of
large parent archives that are not distributed here. They preserve the original
fail-closed provenance contracts. For the self-contained handoff, use the
low-level scripts in `scripts/`, which consume only the included public graph
input and blinded debug fixture.

## Older Stage-I orchestration retained for lineage

| file | responsibility |
|---|---|
| `R/gmde-simulation-design.R` | current-algorithm Stage-I scientific configuration and data generation |
| `R/gmde-simulation-runner.R` | safe task orchestration, checkpoints, summaries and prediction gates for that configuration |

These two files are retained because they are part of the exact frozen
execution snapshot. Their old context hashes and result contracts must not be
mistaken for authorization to run the new road experiment.

## Five internal comparison methods

| method | expert likelihood/state model | allocation mechanism |
|---|---|---|
| GMDE-W | same Poisson DLM experts | graph classifier using the weighted road qNN basis |
| GMDE-C | same Poisson DLM experts | graph classifier using the binary support graph basis |
| Euc-MDE | same Poisson DLM experts | graph classifier using a Euclidean Matérn basis |
| MoDE | same Poisson DLM experts | global Dirichlet mixture weights, no graph |
| Potts-MDE | same Poisson DLM experts | weighted Potts local-interaction allocation prior |

The first five methods share `K_fit=10` in the planned overfitted comparison,
but that does not make their priors identical. External PNARM and AR(1) latent
class implementations are not bundled and cannot be treated as an exactly
matched `K_max=10` ablation.

## Call path for the current failure

```text
R/gmde-mcmc.R:790
  gmde_allocation_conditional_mechanics()
    R/gmde-classifier.R:271
      gmde_poisson_binomial_threshold()  # defined at line 145
        R/gmde-classifier.R:189          # mass-audit stop
```

Line numbers refer to the byte-identical source files in this handoff.

