// log-PDF of the shifted Wald distribution
real swald_lpdf(real rt, real mu, real drift, real bound, real ndt, real sigma) {
  // compute shifted response time
  real t_shifted = rt - ndt;

  // non-decision time exceeded observed time
  if (t_shifted <= 0) {
    return negative_infinity();
  }

  // compute likelihood
  real normalization = bound / sqrt(2 * pi() * sigma^2 * pow(t_shifted, 3));
  real likelihood = exp( -1 * square( bound - drift * t_shifted ) / (2 * sigma^2 * t_shifted) );

  // return the summed log_normalization and log_likelihood
  return log(normalization) + log(likelihood);
}

// log shifted Wald survivor function
real swald_lccdf(real rt, real mu, real drift, real bound, real ndt, real sigma) {
  // compute shifted response time
  real t_shifted = rt - ndt;

  // non-decision time exceeded observed time
  if (t_shifted <= 0) {
    return negative_infinity();
  }

  // compute sqrt_t for simpler formulas down the line
  real sqrt_t = sqrt(t_shifted);

  // compute the three terms of the survivor function
  real term1 = Phi( (drift * t_shifted - bound) / (sigma * sqrt_t) );
  real term2 = exp((2 * bound * v) / square(sigma));
  real term3 = Phi( -(drift * t_shifted + bound) / (sigma * sqrt_t) );

  // return the log survivor function
  return log(1 - term1 - (term2 * term3));
}

// log-PDF of the censored shifted Wald model
real cswald_lpdf(real rt, int response, real drift, real, bound, real ndt, real s) {
  return response * swald_lpdf(rt, drift, bound, ndt, s) + (1-response) * swald_lccdf(rt, drift, bound, ndt, s)
}

// log-PDF of competing risks shifted Wald model
real cswald_crisk_lpdf(real rt, int response, real drift, real, bound, real zr, real ndt, real s) {
  real bound_upper = bound - zr * bound
  real bound_lower = bound + zr * bound
  real lpdf_correct = response * swald_lpdf(rt, drift, bound_upper, ndt, s) + (1-response) * swald_lccdf(rt, drift, bound_upper, ndt, s);
  real lpdf_incorrect = (response -1) * swald_lpdf(rt, -drift, bound_lower, ndt, s) + (response) * swald_lccdf(rt, -drift, bound_lower, ndt, s)
  return lpdf_correct + lpdf_incorrect
}
