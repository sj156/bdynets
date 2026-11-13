// Multivariate information-form Gaussian proposals and Poisson/softmax kernels.
// Supplied standard normals make the Gaussian map independently testable.
// [[Rcpp::plugins(cpp17)]]
#include <Rcpp.h>
#include <vector>
#include <cmath>
namespace {
using V=std::vector<double>;
using Rcpp::NumericMatrix; using Rcpp::NumericVector;
V mat(const NumericMatrix& a) { V x(a.nrow()*a.ncol()); for(int i=0;i<a.nrow();++i)for(int j=0;j<a.ncol();++j)x[i*a.ncol()+j]=a(i,j); return x; }
V tr(const V&a,int p){V b(p*p);for(int i=0;i<p;++i)for(int j=0;j<p;++j)b[i*p+j]=a[j*p+i];return b;}
V mm(const V&a,const V&b,int p){V c(p*p,0);for(int i=0;i<p;++i)for(int j=0;j<p;++j)for(int k=0;k<p;++k)c[i*p+j]+=a[i*p+k]*b[k*p+j];return c;}
V mv(const V&a,const V&b,int p){V c(p,0);for(int i=0;i<p;++i)for(int j=0;j<p;++j)c[i]+=a[i*p+j]*b[j];return c;}
V chol(const V&a,int p){V l(p*p,0);for(int i=0;i<p;++i)for(int j=0;j<=i;++j){double v=(a[i*p+j]+a[j*p+i])/2;for(int k=0;k<j;++k)v-=l[i*p+k]*l[j*p+k];if(i==j){if(!(v>0)||!std::isfinite(v))Rcpp::stop("Non-positive Gaussian factor; no jitter applied.");l[i*p+j]=std::sqrt(v);}else l[i*p+j]=v/l[j*p+j];}return l;}
V solve_chol(const V&l,V b,int p){for(int i=0;i<p;++i){for(int j=0;j<i;++j)b[i]-=l[i*p+j]*b[j];b[i]/=l[i*p+i];}for(int i=p-1;i>=0;--i){for(int j=i+1;j<p;++j)b[i]-=l[j*p+i]*b[j];b[i]/=l[i*p+i];}return b;}
V inv(const V&a,int p){V l=chol(a,p),b(p*p);for(int j=0;j<p;++j){V e(p,0);e[j]=1;e=solve_chol(l,e,p);for(int i=0;i<p;++i)b[i*p+j]=e[i];}for(int i=0;i<p;++i)for(int j=0;j<i;++j)b[i*p+j]=b[j*p+i]=(b[i*p+j]+b[j*p+i])/2;return b;}
void finite(const NumericVector& x){for(double v:x)if(!std::isfinite(v))Rcpp::stop("Kernel inputs must be finite.");}
V getW(const NumericVector&w,int t,int p){V b(p*p);for(int i=0;i<p;++i)for(int j=0;j<p;++j)b[i*p+j]=w[(t-1)*p*p+i+p*j];return b;}
V row(const NumericMatrix&a,int t){V v(a.ncol());for(int j=0;j<a.ncol();++j)v[j]=a(t,j);return v;}
void condition(V& m,V& C,const V&G,const V&W,const V&next,int p){
  V ci=inv(C,p),wi=inv(W,p),gt=tr(G,p),prec=mm(mm(gt,wi,p),G,p);
  V b=mv(ci,m,p),r=mv(mm(gt,wi,p),next,p);
  for(int j=0;j<p*p;++j)prec[j]+=ci[j];for(int j=0;j<p;++j)b[j]+=r[j];
  C=inv(prec,p);m=mv(C,b,p);
}
}
// [[Rcpp::export(name = ".gmde_information_ffbs_cpp", rng = false)]]
NumericMatrix gmde_information_ffbs_cpp(NumericMatrix state,NumericMatrix design,
  NumericVector precision,NumericVector natural,NumericMatrix transition,
  NumericVector initial_mean,NumericMatrix initial_covariance,NumericVector W,
  int start,int end,NumericMatrix normals){
  finite(state);finite(design);finite(precision);finite(natural);finite(transition);
  finite(initial_mean);finite(initial_covariance);finite(W);finite(normals);
  int T=design.nrow(),p=design.ncol();Rcpp::IntegerVector wd=W.attr("dim");
  if(p<1||start<1||end<start||end>T||state.nrow()!=T+1||state.ncol()!=p||
    precision.size()!=T||natural.size()!=T||transition.nrow()!=p||transition.ncol()!=p||
    initial_mean.size()!=p||initial_covariance.nrow()!=p||initial_covariance.ncol()!=p||
    normals.nrow()!=T+1||normals.ncol()!=p||wd.size()!=3||wd[0]!=p||wd[1]!=p||wd[2]!=T)
    Rcpp::stop("Incompatible FFBS dimensions or block boundaries.");
  for(double x:precision)if(x<0)Rcpp::stop("Negative observation precision.");
  V G=mat(transition),GT=tr(G,p),C=mat(initial_covariance),m(initial_mean.begin(),initial_mean.end());
  int left=start==1?0:start;std::vector<V> ms(T+1),cs(T+1);
  if(start==1){chol(C,p);ms[0]=m;cs[0]=C;}
  for(int t=start;t<=end;++t){
    Rcpp::checkUserInterrupt();
    if(t==start && start>1){m=mv(G,row(state,t-1),p);C=getW(W,t,p);}
    else {m=mv(G,m,p);C=mm(mm(G,C,p),GT,p);V w=getW(W,t,p);for(int j=0;j<p*p;++j)C[j]+=w[j];}
    V pr=inv(C,p),b=mv(pr,m,p);
    for(int i=0;i<p;++i){b[i]+=natural[t-1]*design(t-1,i);for(int j=0;j<p;++j)pr[i*p+j]+=precision[t-1]*design(t-1,i)*design(t-1,j);}
    C=inv(pr,p);m=mv(C,b,p);ms[t]=m;cs[t]=C;
  }
  NumericMatrix out=Rcpp::clone(state);
  for(int t=end;t>=left;--t){
    m=ms[t];C=cs[t];
    if(t<T)condition(m,C,G,getW(W,t+1,p),row(out,t+1),p);
    V l=chol(C,p),z=mv(l,row(normals,t),p);
    for(int j=0;j<p;++j){out(t,j)=m[j]+z[j];if(!std::isfinite(out(t,j)))Rcpp::stop("Nonfinite Gaussian proposal.");}
  }
  return out;
}
// [[Rcpp::export(name = ".gmde_poisson_block_ratio_cpp", rng = false)]]
double gmde_poisson_block_ratio_cpp(NumericVector sums,double N,NumericVector r,
  NumericVector old_eta,NumericVector new_eta){
  finite(sums);finite(r);finite(old_eta);finite(new_eta);
  if(!(N>0)||!std::isfinite(N)||r.size()!=sums.size()||old_eta.size()!=sums.size()||new_eta.size()!=sums.size())Rcpp::stop("Invalid correction dimensions.");
  long double total=0;
  for(int t=0;t<sums.size();++t){if(sums[t]<0||r[t]<=0)Rcpp::stop("Invalid counts or NB size.");
    long double a=(long double)N*std::exp((long double)old_eta[t]),b=(long double)N*std::exp((long double)new_eta[t]);
    // Exact difference: avoid subtracting two large complete log corrections.
    long double diff=b-a;
    total+=-diff+((long double)r[t]+sums[t])*std::log1p(diff/((long double)r[t]+a));
  }
  if(!std::isfinite((double)total))Rcpp::stop("Nonfinite Poisson/NB correction; intensities were not clipped.");return (double)total;
}
// [[Rcpp::export(name = ".gmde_poisson_emission_cpp", rng = false)]]
NumericMatrix gmde_poisson_emission_cpp(NumericMatrix Y,NumericMatrix eta){
  finite(Y);finite(eta);if(Y.ncol()!=eta.nrow())Rcpp::stop("Incompatible emission dimensions.");
  NumericMatrix out(Y.nrow(),eta.ncol());
  for(int k=0;k<eta.ncol();++k)for(int i=0;i<Y.nrow();++i){long double x=0;for(int t=0;t<Y.ncol();++t){if(Y(i,t)<0)Rcpp::stop("Negative count.");x+=(long double)Y(i,t)*eta(t,k)-std::exp((long double)eta(t,k));}out(i,k)=(double)x;if(!std::isfinite(out(i,k)))Rcpp::stop("Nonfinite Poisson emission.");}return out;
}
// [[Rcpp::export(name = ".gmde_softmax_loglik_cpp", rng = false)]]
double gmde_softmax_loglik_cpp(NumericMatrix field,Rcpp::IntegerVector labels){
  finite(field);if(labels.size()!=field.nrow()||field.ncol()<1)Rcpp::stop("Invalid softmax dimensions.");double ans=0;
  for(int i=0;i<field.nrow();++i){if(labels[i]<1||labels[i]>field.ncol())Rcpp::stop("Invalid label.");double top=field(i,0),s=0;for(int k=1;k<field.ncol();++k)top=std::max(top,field(i,k));for(int k=0;k<field.ncol();++k)s+=std::exp(field(i,k)-top);ans+=field(i,labels[i]-1)-top-std::log(s);}return ans;
}
// [[Rcpp::export(name = ".gmde_raw_laplacian_cpp", rng = false)]]
NumericMatrix gmde_raw_laplacian_cpp(NumericMatrix adjacency,NumericMatrix distance,std::string method,double h){
  finite(adjacency);finite(distance);int n=adjacency.nrow();if(n<2||adjacency.ncol()!=n||distance.nrow()!=n||distance.ncol()!=n||!(h>0)||!std::isfinite(h)||(method!="Exp-W"&&method!="SE-W"&&method!="C"))Rcpp::stop("Invalid graph inputs.");
  NumericMatrix L(n,n);
  for(int i=0;i<n;++i){if(adjacency(i,i)!=0)Rcpp::stop("Self edges are excluded.");for(int j=0;j<i;++j){if(adjacency(i,j)!=adjacency(j,i)||(adjacency(i,j)!=0&&adjacency(i,j)!=1)||distance(i,j)!=distance(j,i))Rcpp::stop("Require symmetric binary support and distances.");if(adjacency(i,j)==0)continue;if(!(distance(i,j)>0))Rcpp::stop("Positive edge length required.");double z=distance(i,j)/h,w=method=="C"?1:std::exp(method=="Exp-W"?-z:-z*z);if(!(w>0))Rcpp::stop("Edge-weight underflow; no graph truncation was applied.");L(i,j)=L(j,i)=-w;L(i,i)+=w;L(j,j)+=w;}}
  return L;
}
