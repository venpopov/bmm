logit <- function(p) {
  log(p) - log1p(-p)
}

inv_logit <- function(x) {
  exp(x) / (1 + exp(x))
}

cloglog <- function (x) {
  log(-log1p(-x))
}

inv_cloglog <- function(x) {
  1 - exp(-exp(x))
}

log_expm1 <- function (x) {
  out <- log(expm1(x))
  ifelse(out < Inf, out, x)
}

log1p_exp <- function (x) {
  out <- log1p(exp(x))
  ifelse(out < Inf, out, x)
}

log1p_exp_custom <- function(x, mu = 0, a = 0, b = 1) {
  out <- log1p(a + exp(b * (x - mu)))/b
  ifelse(out < Inf, out, x)
}

softfit <- function(x) {
  log_expm1(x/(1-x))
}

inv_softfit <- function(x) {
  y <- log1p_exp(x)
  y/(1 + y)
}
