// Deterministic multivariate discount guide for the graphMoDE Poisson model.
// No likelihood approximation from this guide is added to the posterior target.
// [[Rcpp::plugins(cpp17)]]
#include <Rcpp.h>
#include "../inst/include/bdynets/scalar_state_space.h"

namespace {
using Vec = std::vector<double>;
using Rcpp::NumericMatrix;
using Rcpp::NumericVector;

void finite_vector(const NumericVector& x, const char* name) {
    for (double v : x) if (!std::isfinite(v))
        Rcpp::stop("%s must be finite.", name);
}

// Column-major lower Cholesky. Never add jitter or clip eigenvalues.
Vec lower_chol(const Vec& a, int p) {
    Vec l(p*p, 0.0);
    for (int j=0; j<p; ++j) for (int i=j; i<p; ++i) {
        double x=a[i+p*j];
        if (!std::isfinite(x)) Rcpp::stop("Covariance is not finite.");
        for (int k=0; k<j; ++k) x-=l[i+p*k]*l[j+p*k];
        if (i==j) {
            if (!(x>0) || !std::isfinite(x))
                Rcpp::stop("Covariance is not numerically positive definite; no jitter was added.");
            l[i+p*j]=std::sqrt(x);
        } else l[i+p*j]=x/l[j+p*j];
    }
    return l;
}

double inverse_trigamma(double v, double tolerance, int max_iterations) {
    if (!(v>0) || !std::isfinite(v))
        Rcpp::stop("Inverse trigamma requires a finite positive variance.");
    if (!(tolerance>=1e-14 && tolerance<=1e-6) || max_iterations<1)
        Rcpp::stop("Use tolerance in [1e-14, 1e-6] and a positive iteration limit.");
    // 1/x < trigamma(x) < 1/x + 1/x^2 gives this positive bracket.
    double lo=1/v, hi=lo+1/std::sqrt(v), log_v=std::log(v);
    if (!(lo>0) || !std::isfinite(hi))
        Rcpp::stop("Inverse-trigamma bracket is not representable.");
    auto score=[log_v](double z) { return std::log(R::trigamma(std::exp(z)))-log_v; };
    double a=std::log(lo), b=std::log(hi), root;
    if (std::abs(score(a))<=tolerance) root=a;
    else if (std::abs(score(b))<=tolerance) root=b;
    else root=mcmc_kernels::detail::reference_root(score,a,b,tolerance,max_iterations);
    double out=std::exp(root);
    if (!(out>0) || !std::isfinite(out) ||
        std::abs(std::log(R::trigamma(out))-log_v)>20*tolerance)
        Rcpp::stop("Inverse-trigamma residual exceeds the declared tolerance.");
    return out;
}

NumericVector cube(int p, int T) {
    NumericVector out(static_cast<R_xlen_t>(p)*p*T);
    out.attr("dim")=Rcpp::IntegerVector::create(p,p,T);
    return out;
}

void validate_matrix(const NumericMatrix& x, const char* name) {
    mcmc_kernels::detail::validate_view({x.begin(),x.nrow(),x.ncol()},name);
    for (double v : x) if (!std::isfinite(v)) Rcpp::stop("%s must be finite.",name);
}
}

// [[Rcpp::export(name = ".gmde_inverse_trigamma_cpp", rng = false)]]
NumericVector gmde_inverse_trigamma_cpp(NumericVector variance,
                                      double tolerance, int max_iterations) {
    NumericVector out(variance.size());
    for (R_xlen_t j=0;j<variance.size();++j) {
        Rcpp::checkUserInterrupt();
        out[j]=inverse_trigamma(variance[j],tolerance,max_iterations);
    }
    return out;
}

// [[Rcpp::export(name = ".gmde_discount_guide_cpp", rng = false)]]
Rcpp::List gmde_discount_guide_cpp(NumericVector sums, double exposure,
    NumericMatrix design, NumericMatrix transition, NumericVector initial_mean,
    NumericMatrix initial_covariance, double baseline, double adjustment,
    double tolerance, int max_iterations) {
    validate_matrix(design,"design"); validate_matrix(transition,"transition");
    validate_matrix(initial_covariance,"initial_covariance");
    finite_vector(sums,"sums"); finite_vector(initial_mean,"initial_mean");
    int T=design.nrow(),p=design.ncol();
    if (sums.size()!=T || initial_mean.size()!=p || transition.nrow()!=p ||
        transition.ncol()!=p || initial_covariance.nrow()!=p || initial_covariance.ncol()!=p)
        Rcpp::stop("Incompatible guide dimensions.");
    if (!std::isfinite(exposure) || exposure<0 || exposure!=std::floor(exposure))
        Rcpp::stop("Exposure must be a nonnegative integer class size.");
    for (double x:sums) if (x<0 || x!=std::floor(x) || (exposure==0 && x!=0))
        Rcpp::stop("Sums must be nonnegative integer counts, and zero for an empty class.");
    if (!(baseline>0 && baseline<1) || !(adjustment>0) || !std::isfinite(adjustment))
        Rcpp::stop("Require baseline in (0,1) and a finite positive adjustment.");
    Vec m(initial_mean.begin(),initial_mean.end()),C(initial_covariance.begin(),initial_covariance.end());
    for (int j=0;j<p;++j) for (int i=0;i<p;++i)
        if (C[i+p*j]!=C[j+p*i]) Rcpp::stop("Initial covariance must be symmetric.");
    lower_chol(C,p);
    NumericMatrix means(T,p),predicted_means(T,p);
    NumericVector W=cube(p,T),R=cube(p,T),filtered=cube(p,T),P_all=cube(p,T);
    NumericVector information(T),discount(T),predicted_variance(T),updated_variance(T),
        predicted_predictor(T),updated_predictor(T),shapes(T),log_rates(T);
    for (int t=0;t<T;++t) {
        Rcpp::checkUserInterrupt();
        Vec a(p,0.0),P(p*p,0.0),GC(p*p,0.0),RF(p,0.0);
        for(int i=0;i<p;++i) for(int j=0;j<p;++j) a[i]+=transition(i,j)*m[j];
        for(int j=0;j<p;++j) for(int i=0;i<p;++i) for(int q=0;q<p;++q)
            GC[i+p*j]+=transition(i,q)*C[q+p*j];
        for(int j=0;j<p;++j) for(int i=j;i<p;++i) {
            double x=0;for(int q=0;q<p;++q)x+=GC[i+p*q]*transition(j,q);
            P[i+p*j]=P[j+p*i]=x;
        }
        lower_chol(P,p);
        double vbar=0,f=0;
        for(int i=0;i<p;++i) {
            f+=design(t,i)*a[i];
            for(int j=0;j<p;++j) vbar+=design(t,i)*P[i+p*j]*design(t,j);
        }
        double info=inverse_trigamma(vbar,tolerance,max_iterations);
        double complement=(1-baseline)*(-std::expm1(-adjustment*info));
        double delta=1-complement;
        if (!(delta>0 && delta<1) || !(complement>0))
            Rcpp::stop("Effective discount reached a floating-point boundary; no clipping was applied.");
        double v=vbar/delta;
        double alpha=inverse_trigamma(v,tolerance,max_iterations);
        double log_beta=R::digamma(alpha)-f;
        if (!std::isfinite(f) || !std::isfinite(log_beta))
            Rcpp::stop("Guide log-rate is not finite.");
        double g=f,vplus=v;
        if (exposure>0) {
            double log_N=std::log(exposure),large=std::max(log_beta,log_N);
            double log_beta_plus=large+std::log1p(std::exp(-std::abs(log_beta-log_N)));
            if (!std::isfinite(alpha+sums[t])) Rcpp::stop("Updated Gamma shape overflow.");
            g=R::digamma(alpha+sums[t])-log_beta_plus;
            // Enforce the exact no-variance-change identity at zero counts.
            vplus=sums[t]==0 ? v : R::trigamma(alpha+sums[t]);
        }
        for(int i=0;i<p;++i) for(int j=0;j<p;++j) RF[i]+=P[i+p*j]*design(t,j)/delta;
        for(int j=0;j<p;++j) for(int i=j;i<p;++i) {
            double x=P[i+p*j]/delta+(RF[i]/v)*RF[j]*(vplus/v-1);
            C[i+p*j]=C[j+p*i]=x;
        }
        lower_chol(C,p);
        for(int i=0;i<p;++i) {
            m[i]=a[i]+RF[i]*(g-f)/v;
            if(!std::isfinite(m[i]))Rcpp::stop("Guide mean is not finite.");
            means(t,i)=m[i];predicted_means(t,i)=a[i];
        }
        for(int j=0;j<p*p;++j) {
            R_xlen_t at=static_cast<R_xlen_t>(t)*p*p+j;
            P_all[at]=P[j]; W[at]=(complement/delta)*P[j];
            R[at]=P[j]/delta;filtered[at]=C[j];
        }
        information[t]=info;discount[t]=delta;predicted_variance[t]=v;updated_variance[t]=vplus;
        predicted_predictor[t]=f;updated_predictor[t]=g;shapes[t]=alpha;log_rates[t]=log_beta;
    }
    return Rcpp::List::create(Rcpp::_["mean"]=means,Rcpp::_["predicted_mean"]=predicted_means,
        Rcpp::_["covariance"]=filtered,Rcpp::_["propagated_covariance"]=P_all,
        Rcpp::_["predicted_covariance"]=R,Rcpp::_["innovation_covariance"]=W,
        Rcpp::_["information"]=information,Rcpp::_["discount"]=discount,
        Rcpp::_["predictor_mean"]=predicted_predictor,Rcpp::_["predictor_variance"]=predicted_variance,
        Rcpp::_["updated_predictor_mean"]=updated_predictor,Rcpp::_["updated_predictor_variance"]=updated_variance,
        Rcpp::_["gamma_shape"]=shapes,Rcpp::_["log_gamma_rate"]=log_rates);
}

// [[Rcpp::export(name = ".gmde_transition_log_density_cpp", rng = false)]]
double gmde_transition_log_density_cpp(NumericMatrix state, NumericMatrix transition,
                                      NumericVector innovation_covariance) {
    validate_matrix(state,"state"); validate_matrix(transition,"transition");
    finite_vector(innovation_covariance,"innovation_covariance");
    int T=state.nrow()-1,p=state.ncol();
    if(T<1 || transition.nrow()!=p || transition.ncol()!=p)
        Rcpp::stop("Invalid state or transition dimensions.");
    Rcpp::IntegerVector dims=innovation_covariance.attr("dim");
    if(dims.size()!=3 || dims[0]!=p || dims[1]!=p || dims[2]!=T)
        Rcpp::stop("Innovation covariance must have dimensions p by p by T.");
    double out=0;
    for(int t=0;t<T;++t) {
        Rcpp::checkUserInterrupt();
        const double* begin=innovation_covariance.begin()+static_cast<R_xlen_t>(t)*p*p;
        Vec W(begin,begin+p*p),e(p),z(p);
        for(int j=0;j<p;++j)for(int i=0;i<p;++i)
            if(W[i+p*j]!=W[j+p*i])Rcpp::stop("Innovation covariance must be symmetric.");
        Vec L=lower_chol(W,p);
        for(int i=0;i<p;++i) {
            e[i]=state(t+1,i);
            for(int j=0;j<p;++j)e[i]-=transition(i,j)*state(t,j);
            z[i]=e[i];for(int j=0;j<i;++j)z[i]-=L[i+p*j]*z[j];
            z[i]/=L[i+p*i];out-=std::log(L[i+p*i])+0.5*z[i]*z[i];
        }
    }
    if(!std::isfinite(out))Rcpp::stop("Transition log density is not finite.");
    return out;
}
