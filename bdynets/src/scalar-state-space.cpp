// [[Rcpp::plugins(cpp17)]]
#include <Rcpp.h>
#include "../inst/include/bdynets/scalar_state_space.h"

namespace {
mcmc_kernels::MatrixView view(const Rcpp::NumericMatrix& x) {
    return {x.begin(), x.nrow(), x.ncol()};
}
}

// [[Rcpp::export(name = ".poisson_reference_cpp", rng = false)]]
Rcpp::List poisson_reference_cpp(Rcpp::NumericMatrix counts,
    Rcpp::NumericMatrix loadings, Rcpp::NumericMatrix offset, double discount,
    double initial_mean, double initial_variance, double tolerance,
    int max_iterations) {
    const auto out = mcmc_kernels::poisson_reference_path(view(counts),
        view(loadings), view(offset), discount, initial_mean, initial_variance,
        tolerance, max_iterations, [](){ Rcpp::checkUserInterrupt(); });
    return Rcpp::List::create(
        Rcpp::_["innovation_variance"] = out.W,
        Rcpp::_["filtered_mean"] = out.filtered_mean,
        Rcpp::_["filtered_variance"] = out.filtered_variance,
        Rcpp::_["terminal_mean"] = out.m,
        Rcpp::_["terminal_variance"] = out.C);
}

// [[Rcpp::export(name = ".ffbs_scalar_cpp")]]
Rcpp::List ffbs_scalar_cpp(Rcpp::NumericMatrix response,
    Rcpp::NumericMatrix loadings, Rcpp::NumericMatrix precision,
    Rcpp::NumericVector innovation_variance, Rcpp::NumericVector transition,
    double initial_mean, double initial_variance, double jitter) {
    const auto out = mcmc_kernels::ffbs_scalar(view(response), view(loadings),
        view(precision), Rcpp::as<std::vector<double>>(innovation_variance),
        Rcpp::as<std::vector<double>>(transition), initial_mean, initial_variance,
        jitter, 10, [](){ return R::rnorm(0,1); },
        [](){ Rcpp::checkUserInterrupt(); });
    return Rcpp::List::create(Rcpp::_["state"]=out.state,
                              Rcpp::_["initial"]=out.initial);
}
