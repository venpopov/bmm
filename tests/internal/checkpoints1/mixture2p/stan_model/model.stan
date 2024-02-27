// generated with brms 2.20.12
functions {
  /* compute the tan_half link
   * Args:
   *   x: a scalar in (-pi, pi)
   * Returns:
   *   a scalar in (-Inf, Inf)
   */
   real tan_half(real x) {
     return tan(x / 2);
   }
  /* compute the tan_half link (vectorized)
   * Args:
   *   x: a vector in (-pi, pi)
   * Returns:
   *   a vector in (-Inf, Inf)
   */
   vector tan_half_vector(vector x) {
     return tan(x / 2);
   }
  /* compute the inverse of the tan_half link
   * Args:
   *   y: a scalar in (-Inf, Inf)
   * Returns:
   *   a scalar in (-pi, pi)
   */
   real inv_tan_half(real y) {
     return 2 * atan(y);
   }
  /* compute the inverse of the tan_half link (vectorized)
   * Args:
   *   y: a vector in (-Inf, Inf)
   * Returns:
   *   a vector in (-pi, pi)
   */
   vector inv_tan_half_vector(vector y) {
     return 2 * atan(y);
   }

  /* von Mises log-PDF of a single response
   * for kappa > 100 the normal approximation is used
   * for reasons of numerial stability
   * Args:
   *   y: the response vector between -pi and pi
   *   mu: location parameter vector
   *   kappa: precision parameter
   * Returns:
   *   a scalar to be added to the log posterior
   */
   real von_mises_real_lpdf(real y, real mu, real kappa) {
     if (kappa < 100) {
       return von_mises_lpdf(y | mu, kappa);
     } else {
       return normal_lpdf(y | mu, sqrt(1 / kappa));
     }
   }
  /* von Mises log-PDF of a response vector
   * for kappa > 100 the normal approximation is used
   * for reasons of numerial stability
   * Args:
   *   y: the response vector between -pi and pi
   *   mu: location parameter vector
   *   kappa: precision parameter
   * Returns:
   *   a scalar to be added to the log posterior
   */
   real von_mises_vector_lpdf(vector y, vector mu, real kappa) {
     if (kappa < 100) {
       return von_mises_lpdf(y | mu, kappa);
     } else {
       return normal_lpdf(y | mu, sqrt(1 / kappa));
     }
   }
}
data {
  int<lower=1> N;  // total number of observations
  vector[N] Y;  // response variable
  int<lower=1> K_kappa;  // number of population-level effects
  matrix[N, K_kappa] X_kappa;  // population-level design matrix
  int<lower=1> K_thetat;  // number of population-level effects
  matrix[N, K_thetat] X_thetat;  // population-level design matrix
  int prior_only;  // should the likelihood be ignored?
}
transformed data {
}
parameters {
  vector[K_kappa] b_kappa;  // regression coefficients
  vector[K_thetat] b_thetat;  // regression coefficients
}
transformed parameters {
  real Intercept_mu1;  // temporary intercept for centered predictors
  real Intercept_mu2;  // temporary intercept for centered predictors
  real Intercept_kappa2;  // temporary intercept for centered predictors
  real lprior = 0;  // prior contributions to the log posterior
  Intercept_mu1 = 0;
  Intercept_mu2 = 0;
  Intercept_kappa2 = -100;
  lprior += normal_lpdf(b_kappa | 2,1);
  lprior += logistic_lpdf(b_thetat | 0, 1);
}
model {
  // likelihood including constants
  if (!prior_only) {
    // initialize linear predictor term
    vector[N] nlp_kappa = rep_vector(0.0, N);
    // initialize linear predictor term
    vector[N] nlp_thetat = rep_vector(0.0, N);
    // initialize linear predictor term
    vector[N] mu1 = rep_vector(0.0, N);
    // initialize non-linear predictor term
    vector[N] kappa1;
    // initialize linear predictor term
    vector[N] mu2 = rep_vector(0.0, N);
    // initialize linear predictor term
    vector[N] kappa2 = rep_vector(0.0, N);
    // initialize non-linear predictor term
    vector[N] theta1;
    vector[N] theta2 = rep_vector(0.0, N);
    real log_sum_exp_theta;
    nlp_kappa += X_kappa * b_kappa;
    nlp_thetat += X_thetat * b_thetat;
    mu1 += Intercept_mu1;
    mu2 += Intercept_mu2;
    kappa2 += Intercept_kappa2;
    mu1 = inv_tan_half_vector(mu1);
    for (n in 1:N) {
      // compute non-linear predictor values
      kappa1[n] = exp(nlp_kappa[n]);
    }
    mu2 = inv_tan_half_vector(mu2);
    kappa2 = exp(kappa2);
    for (n in 1:N) {
      // compute non-linear predictor values
      theta1[n] = (nlp_thetat[n]);
    }
    for (n in 1:N) {
      // scale theta to become a probability vector
      log_sum_exp_theta = log(exp(theta1[n]) + exp(theta2[n]));
      theta1[n] = theta1[n] - log_sum_exp_theta;
      theta2[n] = theta2[n] - log_sum_exp_theta;
    }
    // likelihood of the mixture model
    for (n in 1:N) {
      array[2] real ps;
      ps[1] = theta1[n] + von_mises_real_lpdf(Y[n] | mu1[n], kappa1[n]);
      ps[2] = theta2[n] + von_mises_real_lpdf(Y[n] | mu2[n], kappa2[n]);
      target += log_sum_exp(ps);
    }
  }
  // priors including constants
  target += lprior;
}
generated quantities {
  // actual population-level intercept
  real b_mu1_Intercept = Intercept_mu1;
  // actual population-level intercept
  real b_mu2_Intercept = Intercept_mu2;
  // actual population-level intercept
  real b_kappa2_Intercept = Intercept_kappa2;
}

