# Selected Gaussian Process Papers

## 1. Hierarchical Nearest-Neighbor Gaussian Process Models for Large Geostatistical Datasets

**Reference:**  
Datta, A., Banerjee, S., Finley, A. O., and Gelfand, A. E.  
*Hierarchical Nearest-Neighbor Gaussian Process Models for Large Geostatistical Datasets*.  
**Journal of the American Statistical Association**, 2016.

**Main topic:**  
Scalable Bayesian Gaussian process modeling for large spatial/geostatistical datasets.

**Core idea:**  
The paper proposes the **nearest-neighbor Gaussian process (NNGP)**, which replaces the dense dependence structure of a full GP with a sparse conditional dependence structure based on nearest neighbors.

**Main contributions:**

- Develops a valid GP model using nearest-neighbor conditional distributions.
- Greatly reduces computational cost from full GP inference.
- Preserves a hierarchical Bayesian modeling framework.
- Makes GP-based spatial inference feasible for large geostatistical datasets.
- Provides a practically useful alternative to low-rank spatial approximations.

**Why it is worth reading:**  
This is one of the most representative papers on scalable Bayesian spatial GP models. It is especially useful for understanding how local dependence can be used to approximate or construct large-scale spatial processes.

---

## 2. A Case Study Competition Among Methods for Analyzing Large Spatial Data

**Reference:**  
Heaton, M. J., Datta, A., Finley, A. O., Furrer, R., Guinness, J., Guhaniyogi, R., Gerber, F., Gramacy, R. B., Hammerling, D., Katzfuss, M., Lindgren, F., Nychka, D. W., Sun, F., and Zammit-Mangion, A.  
*A Case Study Competition Among Methods for Analyzing Large Spatial Data*.  
**Journal of Agricultural, Biological and Environmental Statistics**, 2019.

**Main topic:**  
Comparison of modern methods for large-scale spatial data analysis.

**Core idea:**  
The paper compares multiple scalable spatial modeling approaches on common large spatial datasets, including several GP-related approximations.

**Main contributions:**

- Provides a benchmark comparison of major large-scale spatial methods.
- Evaluates methods in terms of prediction accuracy, uncertainty quantification, and computational cost.
- Includes approaches such as low-rank methods, sparse precision methods, multi-resolution approximations, and nearest-neighbor methods.
- Shows that no single method dominates in all settings.
- Gives practical guidance for choosing scalable spatial models.

**Why it is worth reading:**  
This paper is very suitable for a discussion seminar because it helps students compare different spatial GP approximation strategies in a concrete and practical way.

---

## 3. Scalar-on-Image Regression via the Soft-Thresholded Gaussian Process

**Reference:**  
Kang, J., Reich, B. J., and Staicu, A.-M.  
*Scalar-on-Image Regression via the Soft-Thresholded Gaussian Process*.  
**Biometrika**, 2018.

**Main topic:**  
Bayesian scalar-on-image regression with spatial sparsity.

**Core idea:**  
The paper introduces the **soft-thresholded Gaussian process**, which transforms a latent GP through a soft-thresholding operation to produce spatially sparse regression coefficients.

**Main contributions:**

- Extends GP priors to perform spatial variable selection.
- Allows image regions with negligible effects to be shrunk exactly or nearly to zero.
- Preserves spatial smoothness among important regions.
- Provides a Bayesian framework for uncertainty quantification in scalar-on-image regression.
- Offers an interpretable way to identify influential image regions.

**Why it is worth reading:**  
This paper is a nice example of modifying the GP prior itself to encode structural sparsity. It is especially interesting for students because it shows that GP models can be used not only for smooth interpolation, but also for structured Bayesian variable selection.