// generated with brms 2.20.12
functions {
    // utility function trick for converting real to integer type
  int bin_search(real x, int min_val, int max_val) {
      int mid_p;
      int L = min_val;
      int R = max_val;
      while(L < R) {
        mid_p = (R-L) %/% 2;
        if (L + mid_p < x) {
          L += mid_p + 1;
        } else if (L + mid_p > x) {
          R = L + mid_p - 1;
        } else {
          return(L + mid_p);
        }
      }
      return(L);
    }

  // utility function for determining optimal number of chebyshev points for the denominator approximation
  int get_m(real c, real kappa) {
    real m = floor(2 * exp(0.4*c) * kappa^(fma(c,0.0145,0.7)) + 0.5)+2;
    int M = bin_search(m, 2, 200);
    return(M);
  }

  // log of the numerator of the sdm likelihood
  real sdm_simple_lpdf(vector y, vector mu, vector c, vector kappa) {
    int N = size(y);
    vector[N] num = exp(fma(kappa,cos(y-mu)-1,c)) ;
    real out = dot_product(num, sqrt(kappa));
    out *= inv(sqrt2()) * inv_sqrt(pi());
    return(out);
  }

  // log of the normalization constant, approximated by chebyshev quadrature
  real sdm_simple_ldenom_chquad_adaptive(real c, real kappa, matrix CN) {
    int m = get_m(c,kappa);
    vector[m] cosn = CN[1:m,m];
    vector[m] fn = exp(fma(kappa,cosn,c)) * sqrt(kappa) * inv(sqrt2()) * inv_sqrt(pi());
    real out = -log_sum_exp(fn)+log(m);
    return(out);
  }
}
data {
  int<lower=1> N;  // total number of observations
  vector[N] Y;  // response variable
  int<lower=1> K_c;  // number of population-level effects
  matrix[N, K_c] X_c;  // population-level design matrix
  int prior_only;  // should the likelihood be ignored?
}
transformed data {
    // precompute chebyshev points
  matrix[200,200] COSN;
  for (m in 1:200) {
    for (i in 1:m) {
      COSN[i,m] = cos((2*i-1)*pi()/(2*m))-1;
    }
  }
}
parameters {
  vector[K_c] b_c;  // regression coefficients
  real Intercept_kappa;  // temporary intercept for centered predictors
}
transformed parameters {
  real Intercept;  // temporary intercept for centered predictors
  real lprior = 0;  // prior contributions to the log posterior
  Intercept = 0;
  lprior += student_t_lpdf(b_c | 5,2,0.75);
  lprior += student_t_lpdf(Intercept_kappa | 5,1.75,0.75);
}
model {
  // likelihood including constants
  if (!prior_only) {
    // initialize linear predictor term
    vector[N] mu = rep_vector(0.0, N);
    // initialize linear predictor term
    vector[N] c = rep_vector(0.0, N);
    // initialize linear predictor term
    vector[N] kappa = rep_vector(0.0, N);
    mu += Intercept;
    c += X_c * b_c;
    kappa += Intercept_kappa;
    kappa = exp(kappa);
    target += sdm_simple_lpdf(Y | mu, c, kappa);
      // adaptive calculation of the normalization constant
    real z;
    for (n in 1:N) {
    	if (n == 1 || c[n] != c[n-1] || kappa[n] != kappa[n-1]) {
    		z = sdm_simple_ldenom_chquad_adaptive(c[n],kappa[n],COSN);
    	}
    	target += z;
    }
    target += -(log2()+log(pi()))*N;
  }
  // priors including constants
  target += lprior;
}
generated quantities {
  // actual population-level intercept
  real b_Intercept = Intercept;
  // actual population-level intercept
  real b_kappa_Intercept = Intercept_kappa;
}

