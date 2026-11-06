# Shared C++ kernels in bdynets

Shared numerical routines developed in GLS, separated from the model so other
R and C++ projects can use them. GLS is one method module in bdynets; these numerical routines are shared package infrastructure.

The current kernels support **one scalar latent state observed by one or more
series**. They provide Gaussian FFBS and a Poisson Laplace discount-reference
filter. They do not implement a complete MCMC algorithm or a multivariate
latent-state smoother.

## Install and call from R

From the repository root, install bdynets:

```sh
R CMD INSTALL bdynets
```

The numerical layer uses Rcpp and a C++17 compiler, with no Rfast or
RcppArmadillo dependency. Install the other bdynets dependencies as usual.

```r
library(bdynets)
set.seed(1)
y <- sin(seq_len(60) / 10) + rnorm(60, sd = .2)
draw <- gaussian_ffbs(
  y, precision = 25, innovation_variance = .05, transition = .95,
  initial_mean = 0, initial_variance = 2
)
draw$state
draw$initial

reference <- poisson_discount_filter(
  c(2, 3, 1, 4), loadings = .2, offset = log(3),
  discount = .95, initial_mean = 0, initial_variance = 2
)
reference$innovation_variance
reference$filtered_mean
reference$terminal_variance
```

Both functions accept time-by-series matrices or single-series vectors.
Loadings, observation precisions, and offsets accept scalars, length-T time
vectors repeated over series, or full T-by-K matrices. State transitions and
innovation variances accept scalars or length-T vectors. A Gaussian observation
with precision zero is ignored and may contain missing values.

For Gaussian FFBS, the first transition and innovation occur between the
time-zero prior and the first observation. The result includes both the state
path and a conditional time-zero draw. R bindings use R's RNG in terminal,
backward, then initial order. The default `jitter = 0` follows the supplied
Gaussian model; callers can explicitly request diagonal jitter.

The Poisson filter is a deterministic **Laplace reference approximation**, not
exact filtering or a posterior sampler. Its innovation variance is computed
before each observation. Root tolerance, initial conditions, and the iteration
limit are explicit. No GLS-specific variance threshold or fallback is applied.

## Reuse from another Rcpp package

Add these build dependencies to that package's `DESCRIPTION`:

```text
Imports: Rcpp
LinkingTo: Rcpp, bdynets
SystemRequirements: C++17
```

Then include the header and call the C++ interface:

```cpp
#include <bdynets/scalar_state_space.h>
```

`MatrixView` refers to read-only column-major storage owned by the caller.
The two numerical entry points are `mcmc_kernels::ffbs_scalar()` and
`mcmc_kernels::poisson_reference_path()`. Gaussian sampling receives a
standard-normal callback; both routines accept an optional interruption
callback. They throw standard C++ exceptions and make no R or Rcpp calls.

For an Rcpp adapter, use an RNG scope (provided by ordinary Rcpp exports),
pass `[](){ return R::rnorm(0,1); }`, and pass
`[](){ Rcpp::checkUserInterrupt(); }` as the interruption callback. Mark
deterministic exports `rng = false`. bdynets' `src/gls-kernels.cpp` is a complete
consumer example.

The numerical code is compiled into each consumer's binary. After updating
the shared header, rebuild dependent external packages to pick up changes;
updating the installed bdynets package alone does not update their binaries.

## Use without R

The installed header has no R dependency. The executable example accepts
`std::mt19937` and `std::normal_distribution` as its RNG:

```sh
c++ -std=c++17 -Wall -Wextra -Werror -pedantic \
  -I bdynets/inst/include \
  bdynets/inst/examples/scalar-state-space.cpp \
  -o /tmp/mcmc-kernels-example
/tmp/mcmc-kernels-example
```

The header and its adjacent MIT license can be distributed together for an
independent C++ project. Do not assume identical seeded output across C++
standard-library normal distributions or across R and C++ RNGs.

## Model adapters and bdynets

The GLS adapter inside bdynets includes the shared header and sets its own initial
conditions, unit transitions and legacy jitter for main/general FFBS. These
Gaussian calculations are maintained once in the shared header. The latest
GLS safeguarded-Newton C reference filter and directed-grid study FFBS/reference
implementations are preserved separately, together with their numerical
settings, covariance Metropolis correction, priors and shrinkage updates.
`backend = "cpp"` selects shared FFBS; `reference_backend = "native"` selects
the existing C reference filter. Their R comparisons are selected separately.

Other bdynets modules can consume the same R functions or include the same
header in compatible samplers. Its existing multivariate-state FFBS routine has not been
replaced by this scalar-state implementation. A multivariate-state kernel
requires a separate implementation and validation.

## Validation

Tests cover independent Gaussian smoothing calculations, analytic one-step
posterior means/covariances, custom transitions/priors, missing observations,
broadcasting, root score and curvature, and predictable reference variances.
GLS separately tests its adapters, fixed-seed transition behavior, and native
reference comparisons. Package tests establish software behavior, not MCMC convergence.

```r
testthat::test_local("bdynets")
```
