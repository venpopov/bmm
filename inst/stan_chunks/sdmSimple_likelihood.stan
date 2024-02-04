  // adaptive calculation of the normalization constant
  real z;
  for (n in 1:N) {
  	if (n == 1 || c[n] != c[n-1] || kappa[n] != kappa[n-1]) {
  		z = sdm_simple_ldenom_chquad_adaptive(c[n],kappa[n],COSN);
  	}
  	target += z;
  }
  target += -(log2()+log(pi()))*N;
