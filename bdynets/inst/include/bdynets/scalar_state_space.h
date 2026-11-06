// Copyright (c) 2026 Ying Wu and Sheng Jiang.
// SPDX-License-Identifier: MIT
// Original scalar state-space kernels developed for GLS and generalized here.
// The Brent-Dekker implementation below is independent; no R solver source is
// incorporated. This header has no dependency on R, Rcpp, or a particular RNG.
#ifndef MCMC_KERNELS_SCALAR_STATE_SPACE_H
#define MCMC_KERNELS_SCALAR_STATE_SPACE_H

#include <algorithm>
#include <cmath>
#include <cstddef>
#include <functional>
#include <limits>
#include <stdexcept>
#include <string>
#include <utility>
#include <vector>

namespace mcmc_kernels {

// Non-owning, read-only column-major matrix. The caller owns the storage and
// guarantees that rows*cols elements remain available throughout the call.
struct MatrixView {
    const double* data;
    int rows;
    int cols;

    double operator()(int time, int series) const noexcept {
        return data[static_cast<std::size_t>(time) +
                    static_cast<std::size_t>(rows)*series];
    }
};

struct ReferencePath {
    std::vector<double> W;
    double m;
    double C;
    std::vector<double> filtered_mean;
    std::vector<double> filtered_variance;
};

struct ScalarFFBS {
    std::vector<double> state;
    double initial;
};

namespace detail {

inline void validate_view(MatrixView matrix, const char* name) {
    if (matrix.rows < 1 || matrix.cols < 1)
        throw std::invalid_argument(std::string(name) +
            " must have at least one row and column.");
    if (matrix.data == nullptr)
        throw std::invalid_argument(std::string(name) + " storage is null.");
    if (static_cast<std::size_t>(matrix.rows) >
        std::numeric_limits<std::size_t>::max()/static_cast<std::size_t>(matrix.cols))
        throw std::invalid_argument(std::string(name) + " dimensions overflow.");
}

inline void validate_same_shape(MatrixView first, MatrixView other,
                                const char* name) {
    validate_view(other,name);
    if (first.rows != other.rows || first.cols != other.cols)
        throw std::invalid_argument(std::string(name) +
            " dimensions must match the observation matrix.");
}

inline bool same_sign(double x, double y) noexcept {
    return (x > 0 && y > 0) || (x < 0 && y < 0);
}

// Independent Brent-Dekker root finder: inverse quadratic interpolation,
// secant interpolation, and bisection safeguards. No statistical model or
// random-number generator is involved in this helper.
template<class Score>
double reference_root(Score& score, double left, double right,
                      double root_tolerance, int max_iterations) {
    double a = left, b = right;
    double fa = score(a), fb = score(b);
    if (!std::isfinite(fa) || !std::isfinite(fb) || same_sign(fa,fb))
        throw std::runtime_error("Could not bracket reference-filter mode.");
    if (fa == 0) return a;
    if (fb == 0) return b;
    if (std::abs(fa) < std::abs(fb)) {
        std::swap(a,b); std::swap(fa,fb);
    }
    double c = a, fc = fa, d = c;
    bool bisected = true;
    for (int iteration = 0; iteration < max_iterations; ++iteration) {
        const double tol = .5*root_tolerance +
            2*std::numeric_limits<double>::epsilon()*std::abs(b);
        if (fb == 0 || std::abs(b-a) <= 2*tol) return b;
        double candidate;
        if (fa != fc && fb != fc) {
            // Interpolate the inverse score at zero. Ratios avoid multiplying
            // three potentially large score values before division.
            candidate = a*(fb/(fa-fb))*(fc/(fa-fc)) +
                b*(fa/(fb-fa))*(fc/(fb-fc)) +
                c*(fa/(fc-fa))*(fb/(fc-fb));
        } else {
            candidate = b - (fb/(fb-fa))*(b-a);
        }
        const double interior = .75*a + .25*b;
        const bool outside = candidate <= std::min(interior,b) ||
            candidate >= std::max(interior,b);
        const bool slow = bisected ? std::abs(candidate-b) >= .5*std::abs(b-c) :
            std::abs(candidate-b) >= .5*std::abs(c-d);
        const bool stagnant = bisected ? std::abs(b-c) < tol : std::abs(c-d) < tol;
        if (!std::isfinite(candidate) || outside || slow || stagnant) {
            candidate = .5*a + .5*b;
            bisected = true;
        } else {
            bisected = false;
        }
        if (candidate == a || candidate == b) return b;
        const double value = score(candidate);
        if (!std::isfinite(value))
            throw std::runtime_error("Non-finite reference-filter score.");
        if (value == 0) return candidate;
        d = c; c = b; fc = fb;
        if (same_sign(fa,value)) { a = candidate; fa = value; }
        else { b = candidate; fb = value; }
        if (std::abs(fa) < std::abs(fb)) {
            std::swap(a,b); std::swap(fa,fb);
        }
    }
    throw std::runtime_error("Reference-filter root did not converge.");
}

// The default GLS jitter convention adds a diagonal jitter even to positive
// scalar covariances and scalar precision matrices. It is exposed explicitly
// here so other callers can request jitter=0 rather than inherit that policy.
inline double scalar_chol(double value, double jitter, int jitter_attempts) {
    for (int attempt = 0; attempt < jitter_attempts; ++attempt) {
        const double adjusted = value+jitter;
        if (std::isfinite(adjusted) && adjusted > 0) return std::sqrt(adjusted);
        if (jitter == 0) break;
        jitter *= 10;
    }
    throw std::runtime_error("Scalar Cholesky factorization failed.");
}

inline double scalar_inverse(double value, double jitter, int jitter_attempts) {
    const double inverse_chol = 1/scalar_chol(value,jitter,jitter_attempts);
    return inverse_chol*inverse_chol;
}

inline double scalar_normal(double mean, double variance, double jitter,
                             int jitter_attempts,
                             const std::function<double()>& standard_normal) {
    if (!std::isfinite(mean))
        throw std::runtime_error("Non-finite FFBS conditional mean.");
    // A zero conditional variance with no requested jitter is a deterministic
    // Gaussian. Still consume one callback draw, retaining T+1 calls per path.
    const double sd = variance == 0 && jitter == 0 ? 0 :
        scalar_chol(variance,jitter,jitter_attempts);
    const double z = standard_normal();
    if (!std::isfinite(z))
        throw std::runtime_error("The standard-normal callback returned a non-finite value.");
    const double draw = mean+sd*z;
    if (!std::isfinite(draw))
        throw std::runtime_error("Non-finite FFBS state draw.");
    return draw;
}

} // namespace detail

// Scalar Poisson Laplace reference covariance recursion. The state evolves as
// a random walk with prior variance C/discount. Counts, loadings, and offsets
// are time-by-series views. W[t] is determined before observing counts[t,].
// This is a Laplace reference recursion, not exact Poisson filtering.
inline ReferencePath poisson_reference_path(
        MatrixView counts, MatrixView loadings, MatrixView offset,
        double discount, double initial_mean = 0, double initial_variance = 1,
        double root_tolerance = 1e-10, int max_iterations = 1000,
        std::function<void()> check_interrupt = []() {}) {
    detail::validate_view(counts,"counts");
    detail::validate_same_shape(counts,loadings,"loadings");
    detail::validate_same_shape(counts,offset,"offset");
    if (!std::isfinite(discount) || discount <= 0 || discount >= 1)
        throw std::invalid_argument("discount must be finite and strictly between zero and one.");
    if (!std::isfinite(initial_mean))
        throw std::invalid_argument("initial_mean must be finite.");
    if (!std::isfinite(initial_variance) || initial_variance <= 0)
        throw std::invalid_argument("initial_variance must be finite and positive.");
    if (!std::isfinite(root_tolerance) || root_tolerance <= 0)
        throw std::invalid_argument("root_tolerance must be finite and positive.");
    if (max_iterations < 1)
        throw std::invalid_argument("max_iterations must be positive.");
    const int nt = counts.rows, k = counts.cols;
    for (int a = 0; a < k; ++a) {
        for (int t = 0; t < nt; ++t) {
            if (!std::isfinite(counts(t,a)) || counts(t,a) < 0 ||
                !std::isfinite(loadings(t,a)) || !std::isfinite(offset(t,a)))
                throw std::invalid_argument("counts must be finite and nonnegative; loadings and offset must be finite.");
        }
    }
    ReferencePath out{std::vector<double>(nt),initial_mean,initial_variance,
                      std::vector<double>(nt),std::vector<double>(nt)};
    std::vector<double> z(k);
    double m = initial_mean, C = initial_variance;
    for (int t = 0; t < nt; ++t) {
        if ((t & 127) == 0 && check_interrupt) check_interrupt();
        out.W[t] = (1/discount-1)*C;
        const double R = C/discount;
        if (!std::isfinite(out.W[t]) || out.W[t] <= 0 || !std::isfinite(R) || R <= 0)
            throw std::runtime_error("Non-finite or nonpositive reference-filter variance.");
        long double sum_hy = 0;
        for (int a = 0; a < k; ++a) sum_hy += loadings(t,a)*counts(t,a);
        const double hy = static_cast<double>(sum_hy);
        auto score = [&](double b) {
            double scale = 0;
            for (int a = 0; a < k; ++a) {
                // Preserve separate multiplication/addition, matching the
                // existing vectorized R calculations rather than fused FMA.
                volatile double product = loadings(t,a)*b;
                z[a] = offset(t,a)+product;
                scale = std::max(scale,z[a]);
            }
            long double sum = 0;
            for (int a = 0; a < k; ++a)
                sum += loadings(t,a)*std::exp(z[a]-scale);
            return ((b-m)/R-hy)*std::exp(-scale)+static_cast<double>(sum);
        };
        double width = std::max(1.0,std::sqrt(R));
        double lower = m-width, upper = m+width;
        for (int i = 0; i < 100; ++i) {
            const double lo = score(lower), hi = score(upper);
            if (std::isfinite(lo) && std::isfinite(hi) && lo <= 0 && hi >= 0) break;
            width *= 2; lower = m-width; upper = m+width;
        }
        if (!std::isfinite(lower) || !std::isfinite(upper))
            throw std::runtime_error("Could not bracket reference-filter mode.");
        const double b = detail::reference_root(score,lower,upper,
                                                root_tolerance,max_iterations);
        long double sum = 0;
        for (int a = 0; a < k; ++a) {
            const double h = loadings(t,a);
            volatile double product = h*b;
            const double zz = offset(t,a)+product;
            sum += (h*h)*std::exp(zz);
        }
        const double precision = 1/R+static_cast<double>(sum);
        if (!std::isfinite(precision) || precision <= 0)
            throw std::runtime_error("Non-finite reference-filter precision.");
        m = b; C = 1/precision;
        if (!std::isfinite(C) || C <= 0)
            throw std::runtime_error("Non-finite or nonpositive reference-filter variance.");
        out.filtered_mean[t] = m;
        out.filtered_variance[t] = C;
    }
    out.m = m; out.C = C;
    return out;
}

// Draw one scalar Gaussian state path by forward filtering/backward sampling:
//   b[t] = transition[t]*b[t-1] + N(0, innovation_variance[t]),
//   response[t,a] = loadings[t,a]*b[t] + N(0, 1/precision[t,a]).
// Precision zero marks a missing observation; its response/loading is ignored.
// The normal callback is called in terminal, backward, then initial-state order.
inline ScalarFFBS ffbs_scalar(
        MatrixView response, MatrixView loadings, MatrixView precision,
        const std::vector<double>& innovation_variance,
        const std::vector<double>& transition,
        double initial_mean, double initial_variance, double jitter,
        int jitter_attempts, std::function<double()> standard_normal,
        std::function<void()> check_interrupt = []() {}) {
    detail::validate_view(response,"response");
    detail::validate_same_shape(response,loadings,"loadings");
    detail::validate_same_shape(response,precision,"precision");
    const int nt = response.rows, k = response.cols;
    if (innovation_variance.size() != static_cast<std::size_t>(nt))
        throw std::invalid_argument("innovation_variance must have one value per time point.");
    if (transition.size() != static_cast<std::size_t>(nt))
        throw std::invalid_argument("transition must have one value per time point.");
    if (!std::isfinite(initial_mean))
        throw std::invalid_argument("initial_mean must be finite.");
    if (!std::isfinite(initial_variance) || initial_variance <= 0)
        throw std::invalid_argument("initial_variance must be finite and positive.");
    if (!std::isfinite(jitter) || jitter < 0)
        throw std::invalid_argument("jitter must be finite and nonnegative.");
    if (jitter_attempts < 1)
        throw std::invalid_argument("jitter_attempts must be positive.");
    if (!standard_normal)
        throw std::invalid_argument("A standard-normal callback is required.");
    for (int t = 0; t < nt; ++t) {
        if (!std::isfinite(innovation_variance[t]) || innovation_variance[t] <= 0)
            throw std::invalid_argument("innovation_variance must contain finite positive variances.");
        if (!std::isfinite(transition[t]))
            throw std::invalid_argument("transition must contain finite values.");
    }
    for (int a = 0; a < k; ++a) {
        for (int t = 0; t < nt; ++t) {
            const double w = precision(t,a);
            if (!std::isfinite(w) || w < 0)
                throw std::invalid_argument("precision must be finite and nonnegative.");
            if (w != 0 && (!std::isfinite(response(t,a)) ||
                           !std::isfinite(loadings(t,a))))
                throw std::invalid_argument("response and loadings must be finite where precision is positive.");
        }
    }
    std::vector<double> filtered_mean(nt), predicted_mean(nt),
        filtered_variance(nt), predicted_variance(nt);
    double mp = initial_mean, cp = initial_variance;
    for (int t = 0; t < nt; ++t) {
        if ((t & 1023) == 0 && check_interrupt) check_interrupt();
        const double phi = transition[t];
        predicted_mean[t] = phi*mp;
        const double rt = (phi*cp)*phi + innovation_variance[t];
        if (!std::isfinite(rt) || rt <= 0)
            throw std::runtime_error("Non-finite or nonpositive predicted variance.");
        const double inverse_rt = detail::scalar_inverse(rt,jitter,jitter_attempts);
        double observation_precision = 0, rhs = 0;
        for (int a = 0; a < k; ++a) {
            const double weight = precision(t,a);
            if (weight == 0) continue;
            const double h = loadings(t,a);
            observation_precision += h*(h*weight);
            rhs += h*(weight*response(t,a));
        }
        const double ct = detail::scalar_inverse(inverse_rt+observation_precision,
                                                 jitter,jitter_attempts);
        const double mt = ct*(inverse_rt*predicted_mean[t]+rhs);
        if (!std::isfinite(mt))
            throw std::runtime_error("Non-finite FFBS conditional mean.");
        filtered_mean[t] = mt; filtered_variance[t] = ct;
        predicted_variance[t] = rt; mp = mt; cp = ct;
    }
    ScalarFFBS out{std::vector<double>(nt),0};
    out.state[nt-1] = detail::scalar_normal(filtered_mean[nt-1],
        filtered_variance[nt-1],jitter,jitter_attempts,standard_normal);
    for (int t = nt-2; t >= 0; --t) {
        if ((t & 1023) == 0 && check_interrupt) check_interrupt();
        const double rn = predicted_variance[t+1];
        const double b = (filtered_variance[t]*transition[t+1])*
            detail::scalar_inverse(rn,jitter,jitter_attempts);
        const double mean = filtered_mean[t]+b*(out.state[t+1]-predicted_mean[t+1]);
        const double variance = filtered_variance[t]-(b*rn)*b;
        out.state[t] = detail::scalar_normal(mean,variance,jitter,jitter_attempts,
                                            standard_normal);
    }
    const double j0 = (initial_variance*transition[0])*
        detail::scalar_inverse(predicted_variance[0],jitter,jitter_attempts);
    const double mean0 = initial_mean+j0*(out.state[0]-predicted_mean[0]);
    // Keep C0-J0*phi0*C0 rather than C0-J0*R0*J0. They are mathematically
    // equivalent with no jitter; this grouping retains the GLS convention
    // C0=1, phi0=1 => variance=1-inverse(R0), including its scalar jitter.
    const double variance0 = initial_variance-(j0*transition[0])*initial_variance;
    out.initial = detail::scalar_normal(mean0,variance0,jitter,jitter_attempts,
                                        standard_normal);
    return out;
}

} // namespace mcmc_kernels

#endif // MCMC_KERNELS_SCALAR_STATE_SPACE_H
