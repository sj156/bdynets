// GLS adapters for the shared, model-independent bdynets C++ header.
// [[Rcpp::plugins(cpp17)]]
#include <Rcpp.h>
#include <algorithm>
#include <numeric>
#include "../inst/include/bdynets/scalar_state_space.h"

namespace {
mcmc_kernels::MatrixView view(const Rcpp::NumericMatrix& x) {
    return {x.begin(), x.nrow(), x.ncol()};
}
mcmc_kernels::MatrixView factor_view(const Rcpp::NumericVector& H, int nt, int k) {
    if (!H.hasAttribute("dim"))
        Rcpp::stop("H must be a time-by-series-by-one numeric array.");
    const Rcpp::IntegerVector hd = H.attr("dim");
    if (hd.size() != 3 || hd[0] != nt || hd[1] != k || hd[2] != 1)
        Rcpp::stop("H must be a time-by-series-by-one numeric array.");
    return {H.begin(),nt,k};
}
}

// Preserve GLS 0.5.1's safeguarded Newton C reference implementation.
extern "C" SEXP gls_reference_path(SEXP Y, SEXP H, SEXP offset, SEXP discount,
                                   SEXP state, SEXP keep_mean);

// [[Rcpp::export(name = ".gls_reference_path_native", rng = false)]]
SEXP gls_reference_path_native(SEXP Y, SEXP H, SEXP offset, SEXP discount,
                               SEXP state, SEXP keep_mean) {
    return gls_reference_path(Y,H,offset,discount,state,keep_mean);
}

// [[Rcpp::export(name = ".gls_ffbs_rank1_cpp")]]
Rcpp::NumericMatrix gls_ffbs_rank1_cpp(Rcpp::NumericMatrix response,
    Rcpp::NumericVector H, Rcpp::NumericMatrix omega,
    Rcpp::Nullable<Rcpp::NumericVector> W = R_NilValue) {
    const int nt=response.nrow(), k=response.ncol();
    // GLS requires strictly positive PG precisions and finite observations.
    // The shared Gaussian kernel also permits zero-precision missing values.
    if (omega.nrow()!=nt || omega.ncol()!=k)
        Rcpp::stop("omega must have the same dimensions as response.");
    for (R_xlen_t j=0;j<response.size();++j)
        if (!std::isfinite(response[j]) || !std::isfinite(omega[j]) || omega[j]<=0)
            Rcpp::stop("response must be finite; omega must be finite and positive.");
    std::vector<double> q;
    if (W.isNull()) q.assign(nt,1);
    else q=Rcpp::as<std::vector<double>>(Rcpp::NumericVector(W));
    const auto out = mcmc_kernels::ffbs_scalar(view(response),
        factor_view(H,nt,k), view(omega), q, std::vector<double>(nt,1),
        0, 1, 1e-10, 10, [](){ return R::rnorm(0,1); },
        [](){ Rcpp::checkUserInterrupt(); });
    Rcpp::NumericMatrix draw(nt,1);
    std::copy(out.state.begin(),out.state.end(),draw.begin());
    draw.attr("initial")=Rcpp::NumericVector::create(out.initial);
    return draw;
}

// [[Rcpp::export(name = ".gls_pair_product_cpp", rng = false)]]
Rcpp::NumericMatrix gls_pair_product_cpp(const Rcpp::NumericMatrix& X,
    const Rcpp::IntegerVector& source, const Rcpp::IntegerVector& target,
    const Rcpp::NumericVector& value, int k) {
    const int nt = X.nrow();
    const R_xlen_t count = source.size();
    if (k < 1 || X.ncol() != k || target.size() != count || value.size() != count)
        Rcpp::stop("Pair coefficients must match the sources, targets, and columns of X.");

    std::vector<R_xlen_t> order(count);
    std::iota(order.begin(), order.end(), R_xlen_t{0});
    for (R_xlen_t i = 0; i < count; ++i) {
        if (source[i] < 1 || source[i] > k || target[i] < 1 || target[i] > k)
            Rcpp::stop("Pair source and target indices must be within the columns of X.");
    }
    // Match the dense coefficient matrix's source order and combine repeated
    // entries before multiplication. Stable sorting preserves their sum order.
    std::stable_sort(order.begin(), order.end(), [&](R_xlen_t a, R_xlen_t b) {
        if (target[a] != target[b]) return target[a] < target[b];
        return source[a] < source[b];
    });

    Rcpp::NumericMatrix product(nt, k);
    for (R_xlen_t i = 0; i < count;) {
        const R_xlen_t row = order[i++];
        const int from = source[row] - 1;
        const int to = target[row] - 1;
        double coefficient = value[row];
        while (i < count && source[order[i]] - 1 == from && target[order[i]] - 1 == to)
            coefficient += value[order[i++]];
        const double* input = X.begin() + static_cast<R_xlen_t>(nt) * from;
        double* output = product.begin() + static_cast<R_xlen_t>(nt) * to;
        for (int t = 0; t < nt; ++t) output[t] += input[t] * coefficient;
        if (i % 512 == 0) Rcpp::checkUserInterrupt();
    }
    return product;
}
