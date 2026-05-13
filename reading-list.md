# Gaussian Process Reading Group


## Core List

### 1. Rasmussen & Williams — *Gaussian Processes for Machine Learning*  
**Citation:** Carl Edward Rasmussen and Christopher K. I. Williams, *Gaussian Processes for Machine Learning*, MIT Press, 2006.  

This is still the best single book for understanding Gaussian processes as used in machine learning.

**Read first:**

- Ch. 2: Gaussian process regression
- Ch. 3: Gaussian process classification
- Ch. 4: Covariance functions / kernels
- Ch. 5: Hyperparameter learning and model selection
- Ch. 8: Sparse and approximate GP methods

**Use as:** the main textbook for the group.

---

### 2. Gramacy — *Surrogates: Gaussian Process Modeling, Design, and Optimization for the Applied Sciences*  
**Citation:** Robert B. Gramacy, *Surrogates: Gaussian Process Modeling, Design, and Optimization for the Applied Sciences*, Chapman & Hall/CRC, 2020.  
**Role:** Practical methodology, computation, design, and optimization.

This is one of the best modern applied books on GP modeling, especially for surrogate modeling and computer experiments.

**Focus on:**

- GP regression in applied modeling
- Design of computer experiments
- Prediction and uncertainty quantification
- Bayesian optimization
- Local / scalable GP approximations

**Use as:** the practical companion to Rasmussen & Williams.

---

### 3. Stein — *Interpolation of Spatial Data: Some Theory for Kriging*  
**Citation:** Michael L. Stein, *Interpolation of Spatial Data: Some Theory for Kriging*, Springer, 1999.  
**Role:** Statistical theory of kriging, covariance functions, and spatial prediction.

This is the key book if the group wants to understand GP methodology from the spatial-statistics / kriging point of view.

**Focus on:**

- Kriging and best linear unbiased prediction
- Covariance modeling
- Matérn covariance functions
- Smoothness and mean-square differentiability
- Asymptotic properties of spatial prediction

**Use as:** the theoretical spatial-statistics supplement.

---

### 4. Santner, Williams & Notz — *The Design and Analysis of Computer Experiments*  
**Citation:** Thomas J. Santner, Brian J. Williams, and William I. Notz, *The Design and Analysis of Computer Experiments*, Springer, 2003 / 2nd ed. 2018.  
**Role:** GP modeling for deterministic simulators and computer experiments.

This book is especially useful if your group cares about simulation, engineering, uncertainty quantification, calibration, or emulation.

**Focus on:**

- Gaussian process models for computer experiments
- Experimental design
- Prediction and uncertainty quantification
- Model validation
- Calibration

**Use as:** the applied-statistics / UQ reference.

---

### 5. Murphy — *Probabilistic Machine Learning: Advanced Topics*  
**Citation:** Kevin P. Murphy, *Probabilistic Machine Learning: Advanced Topics*, MIT Press, 2023.  
**Role:** Modern probabilistic-ML context.

This book gives a broader modern ML perspective around Bayesian modeling, latent-variable models, approximate inference, and Gaussian processes.

**Focus on:**

- Bayesian regression
- Kernel methods
- Gaussian processes
- Approximate inference
- Variational inference
- Sparse approximations

**Use as:** background for members who want the modern probabilistic ML view.

---

## Important Supplementary Books

### 6. Bishop — *Pattern Recognition and Machine Learning*  
**Citation:** Christopher M. Bishop, *Pattern Recognition and Machine Learning*, Springer, 2006.  
**Role:** Fast background on Bayesian regression and kernel methods.

Useful for participants who need a quick bridge into GPML.

**Focus on:**

- Bayesian linear regression
- Kernel methods
- Gaussian process regression
- Gaussian process classification

**Use as:** a lighter preparatory text before Rasmussen & Williams.

---

### 7. Schölkopf & Smola — *Learning with Kernels*  
**Citation:** Bernhard Schölkopf and Alexander J. Smola, *Learning with Kernels*, MIT Press, 2002.  
**Role:** Kernel and RKHS background.

Gaussian processes and kernel methods are deeply connected. This book is useful if the group wants to understand kernels beyond the purely Bayesian GP formulation.

**Focus on:**

- Positive definite kernels
- Reproducing kernel Hilbert spaces
- Kernel machines
- Connections between regularization and kernels

**Use as:** the kernel-theory supplement.

---

### 8. Garnett — *Bayesian Optimization*  
**Citation:** Roman Garnett, *Bayesian Optimization*, Cambridge University Press, 2023.  
**Role:** Gaussian processes for sequential decision-making and optimization.

If the reading group is interested in Bayesian optimization, active learning, experiment design, or sample-efficient optimization, this is the natural follow-up to GP regression.

**Focus on:**

- GP surrogate models
- Acquisition functions
- Expected improvement
- Upper confidence bound methods
- Entropy-search methods
- Batch and constrained Bayesian optimization

**Use as:** the Bayesian optimization extension.

---

### 9. Cressie — *Statistics for Spatial Data*  
**Citation:** Noel A. C. Cressie, *Statistics for Spatial Data*, Wiley, revised ed. 1993.  
**Role:** Broad spatial-statistics background.

This is not only about Gaussian processes, but it gives important background on spatial random fields, covariance modeling, variograms, and kriging.

**Focus on:**

- Random fields
- Variograms
- Covariance models
- Kriging
- Spatial prediction

**Use as:** the broad spatial-statistics reference.

---

### 10. Cressie & Wikle — *Statistics for Spatio-Temporal Data*  
**Citation:** Noel Cressie and Christopher K. Wikle, *Statistics for Spatio-Temporal Data*, Wiley, 2011.  
**Role:** Spatio-temporal Gaussian processes and hierarchical modeling.

Useful if the group wants to move from static GP models to spatio-temporal processes.

**Focus on:**

- Spatio-temporal covariance functions
- Dynamic spatial models
- Hierarchical Bayesian modeling
- State-space formulations

**Use as:** the spatio-temporal GP extension.

---

## More Theoretical Options

### 11. Ghosal & van der Vaart — *Fundamentals of Nonparametric Bayesian Inference*  
**Citation:** Subhashis Ghosal and Aad van der Vaart, *Fundamentals of Nonparametric Bayesian Inference*, Cambridge University Press, 2017.  
**Role:** Bayesian nonparametric theory of GP priors.

For a mathematically mature group, this is useful for understanding Gaussian processes as priors over function spaces.

**Focus on:**

- Bayesian nonparametric priors
- Posterior consistency
- Posterior contraction
- Gaussian process priors
- Small-ball probabilities and concentration

**Use as:** the theoretical Bayesian supplement.

---

### 12. Adler & Taylor — *Random Fields and Geometry*  
**Citation:** Robert J. Adler and Jonathan E. Taylor, *Random Fields and Geometry*, Springer, 2007.  
**Role:** Advanced theory of Gaussian random fields.

This is not necessary for a first GP reading group, but it is valuable for advanced members interested in sample-path geometry, level sets, and extrema.

**Focus on:**

- Gaussian random fields
- Smoothness
- Excursion sets
- Geometry of random fields
- Extremes of Gaussian processes

**Use as:** the advanced random-field theory reference.

---

## Papers

TBD

---

## 8-Week Reading Plan

### Week 1 — Bayesian Regression and GP Basics

**Main reading:**

- Rasmussen & Williams, Ch. 2

**Supplement:**

- Bishop, sections on Bayesian linear regression and Gaussian processes

**Topics:**

- Gaussian distributions
- Bayesian linear regression
- GP prior
- GP posterior
- Predictive mean and variance

---

### Week 2 — Kernels and Covariance Functions

**Main reading:**

- Rasmussen & Williams, Ch. 4

**Supplement:**

- Stein, covariance-modeling sections
- Schölkopf & Smola, kernel background

**Topics:**

- Positive definite kernels
- Squared-exponential kernel
- Matérn kernel
- Periodic kernels
- Additive and product kernels
- Smoothness and sample paths

---

### Week 3 — Hyperparameter Learning and Model Selection

**Main reading:**

- Rasmussen & Williams, Ch. 5

**Supplement:**

- Gramacy, GP modeling chapters

**Topics:**

- Marginal likelihood
- Evidence maximization
- Length-scale learning
- Noise variance estimation
- Overfitting and identifiability
- Priors on hyperparameters

---

### Week 4 — GP Classification and Non-Gaussian Likelihoods

**Main reading:**

- Rasmussen & Williams, Ch. 3

**Supplement:**

- Murphy, sections on approximate inference

**Topics:**

- GP classification
- Logistic/probit likelihoods
- Laplace approximation
- Expectation propagation
- Variational approximations

---

### Week 5 — Computation and Sparse Approximations

**Main reading:**

- Rasmussen & Williams, Ch. 8

**Supplement:**

- Gramacy, scalable/local GP chapters

**Topics:**

- Cubic cost of exact GPs
- Inducing-point methods
- Low-rank approximations
- Local approximate GPs
- Sparse variational GPs
- Practical numerical issues

---

### Week 6 — Kriging and Spatial Statistics

**Main reading:**

- Stein, selected chapters

**Supplement:**

- Cressie, kriging and variogram sections

**Topics:**

- Kriging
- Variograms
- Stationarity
- Isotropy and anisotropy
- Matérn covariance
- Spatial prediction theory

---

### Week 7 — Design, Surrogates, and Computer Experiments

**Main reading:**

- Gramacy, design and surrogate-modeling chapters

**Supplement:**

- Santner, Williams & Notz

**Topics:**

- Computer experiments
- Space-filling designs
- Latin hypercube designs
- Emulator validation
- Calibration
- Uncertainty quantification

---

### Week 8 — Bayesian Optimization or Advanced Theory

Choose one direction.

#### Option A: Bayesian Optimization

**Main reading:**

- Garnett, selected chapters

**Topics:**

- Acquisition functions
- Expected improvement
- Gaussian process upper confidence bound
- Entropy search
- Batch Bayesian optimization

#### Option B: Bayesian Nonparametric Theory

**Main reading:**

- Ghosal & van der Vaart, GP-prior-related sections

**Topics:**

- GP priors
- Posterior consistency
- Posterior contraction
- Concentration functions
- Function-space support

---

Overleaf link: https://www.overleaf.com/7823943244fhhdpbqnkjbb#3f1c67
