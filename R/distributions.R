#' @title The Signal Discrimination Model (SDM) Distribution
#'
#' @description Density, distribution function, and random generation for the
#'   Signal Discrimination Model (SDM) Distribution with location `mu`,
#'   memory strength `c`, and precision `kappa`. Currently only a
#'   single activation source is supported.
#'
#' @name SDMdist
#'
#' @param x Vector of quantiles
#' @param q Vector of quantiles
#' @param p Vector of probabilities
#' @param n Number of observations to sample
#' @param mu Vector of location values in radians
#' @param c Vector of memory strength values
#' @param kappa Vector of precision values
#' @param log Logical; if `TRUE`, values are returned on the log scale.
#' @param parametrization Character; either `"bessel"` or `"sqrtexp"`
#'   (default). See `vignette("sdm-simple")` for details on the
#'   parameterization.
#' @param log.p Logical; if `TRUE`, probabilities are returned on the log
#'   scale.
#' @param lower.bound Numeric; Lower bound of integration for the cumulative
#'   distribution
#' @param lower.tail Logical; If `TRUE` (default), return P(X <= x). Else,
#'   return P(X > x)
#' @keywords distribution
#'
#' @references Oberauer, K. (2023). Measurement models for visual working
#'   memory - A factorial model comparison. Psychological Review, 130(3), 841–852
#'
#' @return `dsdm` gives the density, `psdm` gives the distribution
#'   function, `qsdm` gives the quantile function, `rsdm` generates
#'   random deviates, and `.dsdm_integrate` is a helper function for
#'   calculating the density of the SDM distribution.
#'
#' @details **Parametrization**
#'
#' See `vignette("sdm-simple")` for details on the parameterization.
#' Oberauer (2023) introduced the SDM with the bessel parametrization. The
#' sqrtexp parametrization is the default in the `bmm` package for
#' numerical stability and efficiency. The two parametrizations are related by
#' the functions `c_bessel2sqrtexp()` and `c_sqrtexp2bessel()`.
#'
#' **The cumulative distribution function**
#'
#' Since responses are on the circle, the cumulative distribution function
#' requires you to choose a lower bound of integration. The default is
#' \eqn{-\pi}, as for the brms::pvon_mises() function but you can choose any
#' value in the argument `lower_bound` of `psdm`. Another useful
#' choice is the mean of the response distribution minus \eqn{\pi}, e.g.
#' `lower_bound = mu-pi`. This is the default in
#' `circular::pvonmises()`, and it ensures that 50% of the cumulative
#' probability mass is below the mean of the response distribution.
#'
#' @export
#'
#' @examples
#' # plot the density of the SDM distribution
#' x <- seq(-pi,pi,length.out=10000)
#' plot(x,dsdm(x,0,2,3),type="l", xlim=c(-pi,pi),ylim=c(0,1),
#'      xlab="Angle error (radians)",
#'      ylab="density",
#'      main="SDM density")
#' lines(x,dsdm(x,0,9,1),col="red")
#' lines(x,dsdm(x,0,2,8),col="green")
#' legend("topright",c("c=2, kappa=3.0, mu=0",
#'                     "c=9, kappa=1.0, mu=0",
#'                     "c=2, kappa=8, mu=1"),
#'        col=c("black","red","green"),lty=1, cex=0.8)
#'
#' # plot the cumulative distribution function of the SDM distribution
#' p <- psdm(x, mu = 0, c = 3.1, kappa = 5)
#' plot(x,p,type="l")
#'
#' # generate random deviates from the SDM distribution and overlay the density
#' r <- rsdm(10000, mu = 0, c = 3.1, kappa = 5)
#' d <- dsdm(x, mu = 0, c = 3.1, kappa = 5)
#' hist(r, breaks=60, freq=FALSE)
#' lines(x,d,type="l", col="red")
#'
dsdm <- function(x, mu = 0, c = 3, kappa = 3.5, log = FALSE,
                 parametrization = "sqrtexp") {
  if (isTRUE(any(kappa < 0))) {
    stop("kappa must be non-negative")
  }

  if (isTRUE(any(c < 0))) {
    stop("c must be non-negative")
  }

  .dsdm_numer <- switch(parametrization,
                        "bessel" = .dsdm_numer_bessel,
                        "sqrtexp" = .dsdm_numer_sqrtexp)

  lnumerator <- .dsdm_numer(x, mu, c, kappa, log=TRUE)

  if (any(length(mu) > 1, length(c) > 1, length(kappa) > 1)) {
    denom <- .dsdm_integrate_numer_v(.dsdm_numer, mu, c, kappa, lower = mu,
                                     upper = mu+pi)
  } else {
    denom <- .dsdm_integrate_numer(.dsdm_numer, mu, c, kappa, lower = mu,
                                   upper = mu+pi)
  }

  denom <- 2*denom

  if(!log) {
    return(exp(lnumerator)/denom)
  }
  return(lnumerator - log(denom))
}

#' @rdname SDMdist
#' @export
psdm <- function(q, mu = 0, c = 3, kappa = 3.5, lower.tail = TRUE, log.p = FALSE,
                 lower.bound = -pi, parametrization = "sqrtexp") {
  # parts adapted from brms::pvon_mises
  if (isTRUE(any(kappa < 0))) {
    stop("kappa must be non-negative")
  }

  if (isTRUE(any(c < 0))) {
    stop("c must be non-negative")
  }

  pi <- base::pi
  pi2 <- 2 * pi
  q <- (q + pi) %% pi2
  mu <- (mu + pi) %% pi2
  lower.bound <- (lower.bound + pi) %% pi2

  .dsdm_integrate <- function(mu, c, kappa, lower, upper, parametrization) {
    stats::integrate(dsdm, lower = lower, upper = upper, mu, c, kappa,
                     parametrization = parametrization)$value
  }

  .dsdm_integrate_v <- Vectorize(.dsdm_integrate)

  if (any(length(q) > 1, length(mu) > 1, length(c) > 1, length(kappa) > 1)) {
    out <- .dsdm_integrate_v(mu, c, kappa, lower = lower.bound, upper = q,
                             parametrization = parametrization)
  } else {
    out <-  .dsdm_integrate(mu, c, kappa, lower = lower.bound, upper = q,
                            parametrization = parametrization)
  }

  if (!lower.tail) {
    out <- 1 - out
  }
  if (log.p) {
    out <- log(out)
  }
  out
}

#' @rdname SDMdist
#' @export
qsdm <- function(p, mu=0, c=3, kappa=3.5, parametrization = "sqrtexp") {
  .NotYetImplemented()
}


#' @rdname SDMdist
#' @export
rsdm <- function(n, mu = 0, c = 3, kappa = 3.5, parametrization = "sqrtexp") {
  if (isTRUE(any(kappa < 0))) {
    stop("kappa must be non-negative")
  }

  if (isTRUE(any(c < 0))) {
    stop("c must be non-negative")
  }

  if (length(n) > 1) {
    stop("n must be a single integer")
  }

  maxy <- dsdm(0, 0, c, kappa)
  xa <- c()

  .rsdm_inner <- function(n, mu, c, kappa, parametrization, xa) {
    x <- stats::runif(n, -pi, pi)
    y <- stats::runif(n, 0, 1) * maxy
    accept <- y < dsdm(x, mu, c, kappa, parametrization=parametrization)
    xa <- c(xa, x[accept])

    if (length(xa) < n) {
      return(.rsdm_inner(n, mu, c, kappa, parametrization, xa))
    }

    return(xa[1:n])
  }

  .rsdm_inner(n, mu, c, kappa, parametrization, xa)
}




# helper functions for calculating the density of the SDM distribution
.dsdm_numer_bessel <- function(x, mu, c, kappa, log=FALSE) {
  if (isTRUE(any(kappa < 0))) {
    stop("kappa must be non-negative")
  }

  if (isTRUE(any(c < 0))) {
    stop("c must be non-negative")
  }

  be <- besselI(kappa, nu = 0, expon.scaled = TRUE)
  out <- c * exp(kappa * (cos(x-mu)-1))/(2*pi*be)
  if (!log) {
    out <- exp(out)
  }
  out
}

.dsdm_numer_sqrtexp <- function(x, mu, c, kappa, log=FALSE) {
  if (isTRUE(any(kappa < 0))) {
    stop("kappa must be non-negative")
  }

  if (isTRUE(any(c < 0))) {
    stop("c must be non-negative")
  }

  out <- c * exp(kappa * (cos(x-mu)-1)) * sqrt(kappa) / sqrt(2 * pi)
  if (!log) {
    out <- exp(out)
  }
  out
}



.dsdm_integrate_numer <- function(fun, mu, c, kappa, lower, upper) {
  stats::integrate(fun, lower = lower, upper = upper, mu, c, kappa)$value
}

.dsdm_integrate_numer_v <- Vectorize(.dsdm_integrate_numer,
                                     vectorize.args = c("mu", "c", "kappa", 'lower','upper'))


