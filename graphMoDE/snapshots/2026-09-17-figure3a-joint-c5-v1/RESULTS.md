# Figure 3(a): graphMoDE five-class development result

This result concerns **dynamic Poisson graphMoDE-W on the two-arm intertwined
spiral in Figure 3(a)**. It does not concern Figure 3(b)'s ring-and-spoke network
or the earlier 100-site Beijing example. There is one synthetic observed panel,
with n=121, T=168, five generating classes and fitted capacity K=10. It had
already been used in development and is not an independent replicated benchmark.

## How Figure 3(a) is divided into five classes

The two arms are connected only through the central road segment. Distances
follow the curved roads. Start at geometric site 1, repeatedly choose the site
farthest from its nearest existing seed, and break ties by site ID. The five
seeds, in selection order, are **1, 121, 61, 22, 100**. Assign each site to its
nearest seed, then use the realized cell-to-profile permutation (1,5,4,2,3).

| Profile class/color | Geometric site IDs | Number of sites |
| --- | --- | ---: |
| 1, blue | 1–10 | 10 |
| 2, orange | 11–36 | 26 |
| 3, green | 86–111 | 26 |
| 4, purple | 37–85 | 49 |
| 5, gold | 112–121 | 10 |

Figure 4 shows the true and recovered classes and the same road unfolded by
road distance. The central class includes the connector and parts of both arms.
No true class labels were supplied to the sampler or joint proposals. Alignment
to truth was done only after fitting, for descriptive scoring and display colors.

## What was compared

Both arms used cached contrast ESS, expert blocks of length 42 with uniform
offsets, four gate/guidance scans per sweep and fixed rho=4. The joint arm added
the audited fixed-capacity joint partition/path/gate MH move every ten sweeps.
Four fresh starts, with 1/3/7/10 occupied classes, were shared across arms;
sampling seeds were distinct. Each of the eight chains ran 600 outer sweeps,
with the first 300 discarded, thin=1 and no outcome-driven extension.

| Quantity | Reference | Joint move |
| --- | ---: | ---: |
| Retained partitions exactly matching truth, chains 1–4 (out of 300 each) | 101, 300, 216, 300 | 300, 300, 300, 300 |
| Final partitions exactly matching truth | 4/4 | 4/4 |
| Scalar checks passed / failed | 6 / 25 | 20 / 6 |
| Constant discrete summaries, uninformative | 6 | 11 |
| Pairwise PSM checks passed | 3/6 | 6/6 |
| Complete worker time, seconds | 1868.903 | 2201.178 |

All eight chains completed their numerical/state-consistency checks. The joint
arm recovered all **121/121 sites** in every retained partition, with five
occupied and five empty components. Figure 4 uses the first retained draw of
joint chain 1 (sweep 301), not a draw chosen for a high recovery score. Two
reference chains recovered the true partition later, during retention.

The joint arm accepted six splits during warm-up (2/1/2/1 by chain) and no
merges; there were no retained allocation changes. It cost 17.8% more complete
worker time on this panel. This supports the observed improvement in reaching
the correct partition from these starts, not a claim of globally optimal
sampling, exploration of all modes, or superiority across datasets.

## Read the figures accurately

- **Figure 5:** occupancy and pair-disagreement traces include all sweeps 1–600;
  likelihood uses only retained sweeps 301–600. Gray shading is warm-up, and
  each row uses the same vertical scale across arms. All eight chains appear.
- **Figure 6:** dashed curves are generating Poisson means, colored curves are
  pooled fitted latent means, and bands are pointwise empirical 2.5%–97.5%
  quantiles from four joint chains × 300 retained draws. Gray dots average
  observed counts within each class. These are not future-count prediction
  intervals or verified coverage; saved draws are correlated.
- **Figure 7:** empirical PSMs average all 1,200 retained draws per arm, with
  identical geometric-ID ordering. The joint matrix equals generating
  co-membership, but a constant retained partition does not certify all-mode
  exploration or calibrated allocation uncertainty.
- **Figure 8:** the six failed continuous scalar summaries remain visible.
  Guidance 7–10 denotes sample-wise order statistics, not fixed expert labels.
  Displayed Rhat and ESS are copied from the original saved diagnostics.

Both arms **fail the full original statistical rule**. The joint arm's six
remaining failures are:

| Summary | Rank Rhat | Folded Rhat | Bulk ESS | Tail ESS |
| --- | ---: | ---: | ---: | ---: |
| Ordered guidance 7 | 1.010486 | 1.000475 | 347.904 | 885.783 |
| Ordered guidance 8 | 1.016912 | 1.004476 | 228.955 | 690.880 |
| Ordered guidance 9 | 1.020792 | 1.002880 | 192.373 | 688.648 |
| Ordered guidance 10 | 1.012240 | 1.002910 | 361.660 | 685.824 |
| Site 31, hour 84 log mean | 1.018001 | 1.004272 | 492.351 | 441.954 |
| Site 91, hour 168 log mean | 1.019405 | 1.010797 | 384.638 | 566.619 |

The unchanged thresholds are rank and folded Rhat ≤1.01, bulk/tail ESS ≥400,
and pairwise PSM RMS ≤0.05. Eleven constant joint discrete summaries remain
uninformative. Exact classification recovery is reported separately from full
posterior convergence and uncertainty precision. This is a development
illustration, not a completed formal simulation study.

## Evidence and publication

The registered c5 run completed on 17 September 2026 at 10:27:08 +0800.
Science took 4075.966 seconds and the single diagnostic stage 30.723 seconds.
Original completion JSON and controller timing are preserved under `results/`;
additional descriptive metadata and source hashes are in `results/summary.json`.
The original runtime `formal_authorized=false` field describes that run's role;
the user separately authorized publication of this code and these figures.
No simulation, diagnostic rerun, threshold relaxation or scientific-source
revision was performed to publish this snapshot. Older snapshots remain intact.
