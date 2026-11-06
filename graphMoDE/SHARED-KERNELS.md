# Accessing bdynets shared kernels from graphMoDE

The graphMoDE research scripts can call the shared kernels from the same
installed `bdynets` package. From the repository root:

```sh
R CMD INSTALL bdynets
```

Use `bdynets::gaussian_ffbs()` and `bdynets::poisson_discount_filter()` in R;
no separate kernel package or source copy is needed. See the
[full numerical interface](../bdynets/CPP-KERNELS.md).

## Compatible scalar example

For a separate one-state Gaussian case, graphMoDE-style observation information
with positive precision maps to response `natural / precision`:

```r
precision <- rep(2, 8)
natural <- seq(-1, 1, length.out = 8)
Fmat <- matrix(1, 8, 1)
set.seed(1)
draw <- bdynets::gaussian_ffbs(
  response = natural / precision,
  loadings = Fmat,
  precision = precision,
  innovation_variance = .05,
  transition = .95,
  initial_mean = 0,
  initial_variance = 1,
  jitter = 0
)
draw$state
draw$initial
```

The shared Gaussian function supports **one latent scalar**, possibly observed
by several series. The current graphMoDE research driver defaults to three
latent coordinates; its existing FFBS, numerical diagnostics and scientific
transitions remain unchanged. Several observed series do not turn the shared
function into a multivariate-state smoother. This guide establishes access and
a compatible usage example, not a migration of that sampler.

The separate `bdynets::poisson_discount_filter()` is a deterministic Laplace
reference approximation. It does not replace graphMoDE's Poisson likelihood
or Metropolis correction.

## C++ access

An Rcpp consumer can declare `LinkingTo: Rcpp, bdynets`, select C++17, and use
`#include <bdynets/scalar_state_space.h>`. For `sourceCpp()` adapters, add
`// [[Rcpp::depends(bdynets)]]` and `// [[Rcpp::plugins(cpp17)]]` to the consumer
source. The header accepts caller-provided RNG and interrupt callbacks.

Plain C++ code can put `bdynets/inst/include` on its include path. The header
itself has no R dependency; retain its adjacent MIT license and rebuild
consumers after header changes. See [CPP-KERNELS.md](../bdynets/CPP-KERNELS.md)
for the executable C++ example and callback contracts.
