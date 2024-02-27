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
  int<lower=1> K_thetant;  // number of population-level effects
  matrix[N, K_thetant] X_thetant;  // population-level design matrix
  // covariates for non-linear functions
  vector[N] C_mu2_1;
  // covariates for non-linear functions
  vector[N] C_mu3_1;
  // covariates for non-linear functions
  vector[N] C_mu4_1;
  // covariates for non-linear functions
  vector[N] C_theta2_1;
  vector[N] C_theta2_2;
  // covariates for non-linear functions
  vector[N] C_theta3_1;
  vector[N] C_theta3_2;
  // covariates for non-linear functions
  vector[N] C_theta4_1;
  vector[N] C_theta4_2;
  int prior_only;  // should the likelihood be ignored?
}
transformed data {
}
parameters {
  vector[K_kappa] b_kappa;  // regression coefficients
  vector[K_thetat] b_thetat;  // regression coefficients
  real par_b_thetant_2;
  real par_b_thetant_3;
  real par_b_thetant_4;
}
transformed parameters {
  vector[K_thetant] b_thetant;  // regression coefficients
  real Intercept_mu1;  // temporary intercept for centered predictors
  real Intercept_mu5;  // temporary intercept for centered predictors
  real Intercept_kappa5;  // temporary intercept for centered predictors
  real lprior = 0;  // prior contributions to the log posterior
  b_thetant[1] = -100;
  b_thetant[2] = par_b_thetant_2;
  b_thetant[3] = par_b_thetant_3;
  b_thetant[4] = par_b_thetant_4;
  Intercept_mu1 = 0;
  Intercept_mu5 = 0;
  Intercept_kappa5 = -100;
  lprior += normal_lpdf(b_kappa | 2,1);
  lprior += logistic_lpdf(b_thetat | 0, 1);
  b_thetant[1] = -100;
  lprior += logistic_lpdf(b_thetant[2] | 0, 1);
  lprior += logistic_lpdf(b_thetant[3] | 0, 1);
  lprior += logistic_lpdf(b_thetant[4] | 0, 1);
}
model {
  // likelihood including constants
  if (!prior_only) {
    // initialize linear predictor term
    vector[N] nlp_kappa = rep_vector(0.0, N);
    // initialize linear predictor term
    vector[N] nlp_thetat = rep_vector(0.0, N);
    // initialize linear predictor term
    vector[N] nlp_thetant = rep_vector(0.0, N);
    // initialize linear predictor term
    vector[N] mu1 = rep_vector(0.0, N);
    // initialize non-linear predictor term
    vector[N] kappa1;
    // initialize non-linear predictor term
    vector[N] mu2;
    // initialize non-linear predictor term
    vector[N] kappa2;
    // initialize non-linear predictor term
    vector[N] mu3;
    // initialize non-linear predictor term
    vector[N] kappa3;
    // initialize non-linear predictor term
    vector[N] mu4;
    // initialize non-linear predictor term
    vector[N] kappa4;
    // initialize linear predictor term
    vector[N] mu5 = rep_vector(0.0, N);
    // initialize linear predictor term
    vector[N] kappa5 = rep_vector(0.0, N);
    // initialize non-linear predictor term
    vector[N] theta1;
    // initialize non-linear predictor term
    vector[N] theta2;
    // initialize non-linear predictor term
    vector[N] theta3;
    // initialize non-linear predictor term
    vector[N] theta4;
    vector[N] theta5 = rep_vector(0.0, N);
    real log_sum_exp_theta;
    nlp_kappa += X_kappa * b_kappa;
    nlp_thetat += X_thetat * b_thetat;
    nlp_thetant += X_thetant * b_thetant;
    mu1 += Intercept_mu1;
    mu5 += Intercept_mu5;
    kappa5 += Intercept_kappa5;
    for (n in 1:N) {
      // compute non-linear predictor values
      kappa1[n] = exp(nlp_kappa[n]);
    }
    for (n in 1:N) {
      // compute non-linear predictor values
      mu2[n] = (C_mu2_1[n]);
    }
    for (n in 1:N) {
      // compute non-linear predictor values
      kappa2[n] = exp(nlp_kappa[n]);
    }
    for (n in 1:N) {
      // compute non-linear predictor values
      mu3[n] = (C_mu3_1[n]);
    }
    for (n in 1:N) {
      // compute non-linear predictor values
      kappa3[n] = exp(nlp_kappa[n]);
    }
    for (n in 1:N) {
      // compute non-linear predictor values
      mu4[n] = (C_mu4_1[n]);
    }
    for (n in 1:N) {
      // compute non-linear predictor values
      kappa4[n] = exp(nlp_kappa[n]);
    }
    kappa5 = exp(kappa5);
    for (n in 1:N) {
      // compute non-linear predictor values
      theta1[n] = (nlp_thetat[n]);
    }
    for (n in 1:N) {
      // compute non-linear predictor values
      theta2[n] = (C_theta2_1[n] * (nlp_thetant[n] + log(C_theta2_2[n])) + (1 - C_theta2_1[n]) * ( - 100));
    }
    for (n in 1:N) {
      // compute non-linear predictor values
      theta3[n] = (C_theta3_1[n] * (nlp_thetant[n] + log(C_theta3_2[n])) + (1 - C_theta3_1[n]) * ( - 100));
    }
    for (n in 1:N) {
      // compute non-linear predictor values
      theta4[n] = (C_theta4_1[n] * (nlp_thetant[n] + log(C_theta4_2[n])) + (1 - C_theta4_1[n]) * ( - 100));
    }
    for (n in 1:N) {
      // scale theta to become a probability vector
      log_sum_exp_theta = log(exp(theta1[n]) + exp(theta2[n]) + exp(theta3[n]) + exp(theta4[n]) + exp(theta5[n]));
      theta1[n] = theta1[n] - log_sum_exp_theta;
      theta2[n] = theta2[n] - log_sum_exp_theta;
      theta3[n] = theta3[n] - log_sum_exp_theta;
      theta4[n] = theta4[n] - log_sum_exp_theta;
      theta5[n] = theta5[n] - log_sum_exp_theta;
    }
    // likelihood of the mixture model
    for (n in 1:N) {
      array[5] real ps;
      ps[1] = theta1[n] + von_mises_real_lpdf(Y[n] | mu1[n], kappa1[n]);
      ps[2] = theta2[n] + von_mises_real_lpdf(Y[n] | mu2[n], kappa2[n]);
      ps[3] = theta3[n] + von_mises_real_lpdf(Y[n] | mu3[n], kappa3[n]);
      ps[4] = theta4[n] + von_mises_real_lpdf(Y[n] | mu4[n], kappa4[n]);
      ps[5] = theta5[n] + von_mises_real_lpdf(Y[n] | mu5[n], kappa5[n]);
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
  real b_mu5_Intercept = Intercept_mu5;
  // actual population-level intercept
  real b_kappa5_Intercept = Intercept_kappa5;
}

