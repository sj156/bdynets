# bdynets

**Bayesian Dynamic Networks for Time-Series Data**

`bdynets` is an R package for fitting Bayesian dynamic network models to time-series data. It uses MCMC methods for parameter estimation and supports flexible network structures with time-varying parameters.

---

## Installation

### From GitHub (Recommended)

```r
# Install devtools if not already installed
if (!requireNamespace("devtools", quietly = TRUE)) {
  install.packages("devtools")
}

# Install bdynets from GitHub
devtools::install_github("your-username/bdynets", dependencies = TRUE)

# Load the package
library(bdynets)
