// Specify additional hyperbolic functions
real csch (real x) {
  return 1/sinh(x);
}

real coth (real x) {
  return cosh(x) / sinh(x);
}

// Specify likelihood for ezDM
real ezDM_lpdf (real y, real mu, real bound, real ndt, real vrt, int hits, int trials) {
  // get standardized boundary and start point
  real k_z = mu * bound/2;

  // get proportion correct
  real pC = 1 - (1 - exp(-2 * k_z)) / (exp(2 * k_z) - exp(-2 * k_z));

  // Calculate moments based on DM parameters
  real MDT = 1/(mu^2)*(2*k_z*coth(2*k_z) - (k_z)*coth(k_z));
  real VRT = 1/(mu^4)*(4*k_z^2*(csch(2*k_z))^2 + 2*k_z*coth(2*k_z) - (k_z)^2*(csch(k_z))^2 - (k_z)*coth(k_z));

  // sample from binomial for correct responses
  real out = binomial_lpmf(hits | trials, pC);

  out += normal_lpdf(vrt | VRT, sqrt((2*pow(VRT,2))/(trials-1)));
  out += normal_lpdf(y | ndt + MDT, sqrt(VRT/trials));

  return out;
}
