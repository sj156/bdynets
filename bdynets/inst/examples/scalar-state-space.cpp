// Compile with -std=c++17 -I bdynets/inst/include; no R dependencies.
#include <bdynets/scalar_state_space.h>
#include <iostream>
#include <random>

int main() {
    const std::vector<double> y{.2,.4,.1,.6};
    const std::vector<double> h(y.size(),1), precision(y.size(),25);
    const std::vector<double> q(y.size(),.05), phi(y.size(),.95);
    const int nt=static_cast<int>(y.size());
    std::mt19937 generator(1);
    std::normal_distribution<double> normal(0,1);
    const auto sample=mcmc_kernels::ffbs_scalar(
        {y.data(),nt,1},{h.data(),nt,1},{precision.data(),nt,1},
        q,phi,0,2,0,10,[&](){ return normal(generator); });
    std::cout << "Initial state: " << sample.initial << "\nPath:";
    for (double x:sample.state) std::cout << ' ' << x;
    std::cout << '\n';
}
