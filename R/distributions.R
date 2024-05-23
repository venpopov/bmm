#' @title Distribution functions for the Signal Discrimination Model (SDM)
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
#'   (default). See [the online article](https://venpopov.github.io/bmm/articles/bmm_sdm_simple.html) for details on the
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
#' See [the online article](https://venpopov.github.io/bmm/articles/bmm_sdm_simple.html) for details on the parameterization.
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
dsdm <- function(x, mu = 0, c = 3, kappa = 3.5, log = FALSE,
                 parametrization = "sqrtexp") {
  stopif(isTRUE(any(kappa < 0)), "kappa must be non-negative")
  stopif(isTRUE(any(c < 0)), "c must be non-negative")

  .dsdm_numer <- switch(parametrization,
                        "bessel" = .dsdm_numer_bessel,
                        "sqrtexp" = .dsdm_numer_sqrtexp)

  lnumerator <- .dsdm_numer(x, mu, c, kappa, log = TRUE)

  if (any(length(mu) > 1, length(c) > 1, length(kappa) > 1)) {
    denom <- .dsdm_integrate_numer_v(.dsdm_numer, mu, c, kappa, lower = mu,
                                     upper = mu + pi)
  } else {
    denom <- .dsdm_integrate_numer(.dsdm_numer, mu, c, kappa, lower = mu,
                                   upper = mu + pi)
  }

  denom <- 2*denom

  if (!log) {
    return(exp(lnumerator)/denom)
  }
  lnumerator - log(denom)
}

#' @rdname SDMdist
#' @export
psdm <- function(q, mu = 0, c = 3, kappa = 3.5, lower.tail = TRUE, log.p = FALSE,
                 lower.bound = -pi, parametrization = "sqrtexp") {
  # parts adapted from brms::pvon_mises
  stopif(isTRUE(any(kappa < 0)), "kappa must be non-negative")
  stopif(isTRUE(any(c < 0)), "c must be non-negative")

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
  stopif(isTRUE(any(kappa < 0)), "kappa must be non-negative")
  stopif(isTRUE(any(c < 0)), "c must be non-negative")
  stopif(length(n) > 1, "n must be a single integer")

  maxy <- dsdm(0, 0, c, kappa)
  xa <- c()

  .rsdm_inner <- function(n, mu, c, kappa, parametrization, xa) {
    x <- stats::runif(n, -pi, pi)
    y <- stats::runif(n, 0, 1) * maxy
    accept <- y < dsdm(x, mu, c, kappa, parametrization = parametrization)
    xa <- c(xa, x[accept])

    if (length(xa) < n) {
      return(.rsdm_inner(n, mu, c, kappa, parametrization, xa))
    }

    xa[1:n]
  }

  .rsdm_inner(n, mu, c, kappa, parametrization, xa)
}

# helper functions for calculating the density of the SDM distribution
.dsdm_numer_bessel <- function(x, mu, c, kappa, log = FALSE) {
  stopif(isTRUE(any(kappa < 0)), "kappa must be non-negative")
  stopif(isTRUE(any(c < 0)), "c must be non-negative")

  be <- besselI(kappa, nu = 0, expon.scaled = TRUE)
  out <- c * exp(kappa * (cos(x - mu) - 1)) / (2 * pi * be)
  if (!log) {
    out <- exp(out)
  }
  out
}

.dsdm_numer_sqrtexp <- function(x, mu, c, kappa, log = FALSE) {
  stopif(isTRUE(any(kappa < 0)), "kappa must be non-negative")
  stopif(isTRUE(any(c < 0)), "c must be non-negative")

  out <- c * exp(kappa * (cos(x - mu) - 1)) * sqrt(kappa) / sqrt(2 * pi)
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



#' @title Distribution functions for the two-parameter mixture model (mixture2p)
#'
#' @description Density, distribution, and random generation functions for the
#'   two-parameter mixture model with the location of `mu`, precision of memory
#'   representations `kappa` and probability of recalling items from memory
#'   `p_mem`.
#'
#' @name mixture2p_dist
#'
#' @param x Vector of observed responses
#' @param q Vector of quantiles
#' @param p Vector of probability
#' @param n Number of observations to generate data for
#' @param mu Vector of locations
#' @param kappa Vector of precision values
#' @param p_mem Vector of probabilities for memory recall
#' @param log Logical; if `TRUE`, values are returned on the log scale.
#'
#' @keywords distribution
#'
#' @references Zhang, W., & Luck, S. J. (2008). Discrete fixed-resolution
#'   representations in visual working memory. Nature, 453.
#'
#' @return `dmixture2p` gives the density of the two-parameter mixture model,
#'   `pmixture2p` gives the cumulative distribution function of the
#'   two-parameter mixture model, `qmixture2p` gives the quantile function of
#'   the two-parameter mixture model, and `rmixture2p` gives the random
#'   generation function for the two-parameter mixture model.
#'
#' @export
#'
#' @examples
#' # generate random samples from the mixture2p model and overlay the density
#' r <- rmixture2p(10000, mu = 0, kappa = 4, p_mem = 0.8)
#' x <- seq(-pi,pi,length.out=10000)
#' d <- dmixture2p(x, mu = 0, kappa = 4, p_mem = 0.8)
#' hist(r, breaks=60, freq=FALSE)
#' lines(x,d,type="l", col="red")
#'
dmixture2p <- function(x, mu=0, kappa=5, p_mem = 0.6, log = FALSE) {
  stopif(isTRUE(any(kappa < 0)), "kappa must be non-negative")
  stopif(isTRUE(any(p_mem < 0)), "p_mem must be larger than zero.")
  stopif(isTRUE(any(p_mem > 1)), "p_mem must be smaller than one.")

  density <- matrix(data = NaN, nrow = length(x), ncol = 2)

  density[,1] <- log(p_mem) + brms::dvon_mises(x = x,mu = mu , kappa = kappa, log = T)
  density[,2] <- log(1 - p_mem) + brms::dvon_mises(x = x,mu = 0 , kappa = 0, log = T)

  density <- matrixStats::rowLogSumExps(density)

  if (!log) {
    return(exp(density))
  }

  density
}

#' @rdname mixture2p_dist
#' @export
pmixture2p <- function(q, mu=0, kappa=7, p_mem = 0.8) {
  .NotYetImplemented()
}

#' @rdname mixture2p_dist
#' @export
qmixture2p <- function(p, mu=0, kappa=5, p_mem = 0.6) {
  .NotYetImplemented()
}

#' @rdname mixture2p_dist
#' @export
rmixture2p <- function(n, mu=0, kappa=5, p_mem = 0.6) {
  stopif(isTRUE(any(kappa < 0)), "kappa must be non-negative")
  stopif(isTRUE(any(p_mem < 0)), "p_mem must be larger than zero.")
  stopif(isTRUE(any(p_mem > 1)), "p_mem must be smaller than one.")

  maxy <- dmixture2p(0, 0, kappa, p_mem)
  xa <- c()

  .rmixture2p_inner <- function(n, mu, c, kappa, p_mem, xa) {
    x <- stats::runif(n, -pi, pi)
    y <- stats::runif(n, 0, 1) * maxy
    accept <- y < dmixture2p(x, mu, kappa, p_mem)
    xa <- c(xa, x[accept])

    if (length(xa) < n) {
      return(.rmixture2p_inner(n, mu, c, kappa, p_mem, xa))
    }

    xa[1:n]
  }

  .rmixture2p_inner(n, mu, c, kappa, p_mem, xa)
}


#' @title Distribution functions for the three-parameter mixture model (mixture3p)
#'
#' @description Density, distribution, and random generation functions for the
#'   three-parameter mixture model with the location of `mu`, precision of
#'   memory representations `kappa`, probability of recalling items from memory
#'   `p_mem`, and probability of recalling non-targets `p_nt`.
#'
#' @name mixture3p_dist
#'
#' @param x Vector of observed responses
#' @param q Vector of quantiles
#' @param p Vector of probability
#' @param n Number of observations to generate data for
#' @param mu Vector of locations. First value represents the location of the
#'   target item and any additional values indicate the location of non-target
#'   items.
#' @param kappa Vector of precision values
#' @param p_mem Vector of probabilities for memory recall
#' @param p_nt Vector of probabilities for swap errors
#' @param log Logical; if `TRUE`, values are returned on the log scale.
#'
#' @keywords distribution
#'
#' @references Bays, P. M., Catalao, R. F. G., & Husain, M. (2009). The
#'   precision of visual working memory is set by allocation of a shared
#'   resource. Journal of Vision, 9(10), 7.
#'
#' @return `dmixture3p` gives the density of the three-parameter mixture model,
#'   `pmixture3p` gives the cumulative distribution function of the
#'   two-parameter mixture model, `qmixture3p` gives the quantile function of
#'   the two-parameter mixture model, and `rmixture3p` gives the random
#'   generation function for the two-parameter mixture model.
#'
#' @export
#'
#' @examples
#' # generate random samples from the mixture3p model and overlay the density
#' r <- rmixture3p(10000, mu = c(0, 2, -1.5), kappa = 4, p_mem = 0.6, p_nt = 0.2)
#' x <- seq(-pi,pi,length.out=10000)
#' d <- dmixture3p(x, mu = c(0, 2, -1.5), kappa = 4, p_mem = 0.6, p_nt = 0.2)
#' hist(r, breaks=60, freq=FALSE)
#' lines(x,d,type="l", col="red")
#'
dmixture3p <- function(x, mu=c(0,2,-1.5), kappa = 5, p_mem = 0.6, p_nt = 0.2, log = FALSE) {
  stopif(isTRUE(any(kappa < 0)), "kappa must be non-negative")
  stopif(isTRUE(any(p_mem < 0)), "p_mem must be larger than zero.")
  stopif(isTRUE(any(p_nt < 0)), "p_nt must be larger than zero.")
  stopif(isTRUE(any(p_mem + p_nt > 1)), "The sum of p_mem and p_nt must be smaller than one.")

  density <- matrix(data = NaN, nrow = length(x), ncol = length(mu) + 1)
  probs <- c(p_mem,
             rep(p_nt/(length(mu) - 1), each = length(mu) - 1),
             (1 - p_mem - p_nt))

  for (i in 1:(length(mu))) {
    density[,i] <- log(probs[i]) +
      brms::dvon_mises(x = x, mu = mu[i], kappa = kappa, log = T)
  }

  density[,length(mu) + 1] <- log(probs[length(mu) + 1]) +
    stats::dunif(x = x,-pi, pi, log = T)

  density <- matrixStats::rowLogSumExps(density)

  if (!log) {
    return(exp(density))
  }

  density
}

#' @rdname mixture3p_dist
#' @export
pmixture3p <- function(q, mu=c(0,2,-1.5), kappa = 5, p_mem = 0.6, p_nt = 0.2) {
  .NotYetImplemented()
}

#' @rdname mixture3p_dist
#' @export
qmixture3p <- function(p, mu=c(0,2,-1.5), kappa = 5, p_mem = 0.6, p_nt = 0.2) {
  .NotYetImplemented()
}

#' @rdname mixture3p_dist
#' @export
rmixture3p <- function(n, mu=c(0,2,-1.5), kappa = 5, p_mem = 0.6, p_nt = 0.2) {
  stopif(isTRUE(any(kappa < 0)), "kappa must be non-negative")
  stopif(isTRUE(any(p_mem < 0)), "p_mem must be larger than zero.")
  stopif(isTRUE(any(p_nt < 0)), "p_nt must be larger than zero.")
  stopif(isTRUE(any(p_mem + p_nt > 1)), "The sum of p_mem and p_nt must be smaller than one.")

  x <- seq(-pi,pi,length.out = 361)
  maxy <- max(dmixture3p(x, mu, kappa, p_mem, p_nt))
  xa <- c()

  .rmixture3p_inner <- function(n, mu, c, kappa, p_mem, p_nt, xa) {
    x <- stats::runif(n, -pi, pi)
    y <- stats::runif(n, 0, 1) * maxy
    accept <- y < dmixture3p(x, mu, kappa, p_mem, p_nt)
    xa <- c(xa, x[accept])

    if (length(xa) < n) {
      return(.rmixture3p_inner(n, mu, c, kappa, p_mem, p_nt, xa))
    }

    xa[1:n]
  }

  .rmixture3p_inner(n, mu, c, kappa, p_mem, p_nt, xa)
}

#' @title Distribution functions for the Interference Measurement Model (IMM)
#'
#' @description Density, distribution, and random generation functions for the
#'   interference measurement model with the location of `mu`, strength of cue-
#'   dependent activation `c`, strength of cue-independent activation `a`, the
#'   generalization gradient `s`, and the precision of memory representations
#'   `kappa`.
#'
#' @name IMMdist
#'
#' @param x Vector of observed responses
#' @param q Vector of quantiles
#' @param p Vector of probability
#' @param n Number of observations to generate data for
#' @param mu Vector of locations
#' @param dist Vector of distances of the item locations to the cued location
#' @param kappa Vector of precision values
#' @param c Vector of strengths for cue-dependent activation
#' @param a Vector of strengths for cue-independent activation
#' @param s Vector of generalization gradients
#' @param b Vector of baseline activation
#' @param log Logical; if `TRUE`, values are returned on the log scale.
#'
#' @keywords distribution
#'
#' @references Oberauer, K., Stoneking, C., Wabersich, D., & Lin, H.-Y. (2017).
#'   Hierarchical Bayesian measurement models for continuous reproduction of
#'   visual features from working memory. Journal of Vision, 17(5), 11.
#'
#' @return `dimm` gives the density of the interference measurement model,
#'   `pimm` gives the cumulative distribution function of the interference
#'   measurement model, `qimm` gives the quantile function of the interference
#'   measurement model, and `rimm` gives the random generation function for the
#'   interference measurement model.
#'
#' @export
#'
#' @examples
#' # generate random samples from the imm and overlay the density
#' r <- rimm(10000, mu = c(0, 2, -1.5), dist = c(0, 0.5, 2),
#'           c = 5, a = 2, s = 2, b = 1, kappa = 4)
#' x <- seq(-pi,pi,length.out=10000)
#' d <- dimm(x, mu = c(0, 2, -1.5), dist = c(0, 0.5, 2),
#'           c = 5, a = 2, s = 2, b = 1, kappa = 4)
#' hist(r, breaks=60, freq=FALSE)
#' lines(x,d,type="l", col="red")
#'
dimm <- function(x, mu=c(0,2,-1.5), dist = c(0,0.5,2),
                 c=5, a = 2, b = 1, s = 2, kappa=5, log = FALSE) {
  stopif(isTRUE(any(kappa < 0)), "kappa must be non-negative")
  stopif(length(mu) != length(dist),
         "The number of items does not match the distances provided from the cued location.")
  stopif(isTRUE(any(s < 0)), "s must be non-negative")
  stopif(isTRUE(any(dist < 0)), "all distances have to be positive.")

  # compute activation for all items
  weights <- rep(c, length(mu)) * exp(-s*dist) + rep(a, length(mu))

  # add activation of background noise
  weights <- c(weights,b)

  # compute probability for responding stemming from each distribution
  probs <- weights/sum(weights)

  density <- matrix(data = NaN, nrow = length(x), ncol = length(mu) + 1)

  for (i in 1:(length(mu))) {
    density[,i] <- log(probs[i]) +
      brms::dvon_mises(x = x, mu = mu[i], kappa = kappa, log = T)
  }

  density[,length(mu) + 1] <- log(probs[length(mu) + 1]) +
    stats::dunif(x = x,-pi, pi, log = T)

  density <- matrixStats::rowLogSumExps(density)

  if (!log) {
    return(exp(density))
  }

  density
}

#' @rdname IMMdist
#' @export
pimm <- function(q, mu=c(0,2,-1.5), dist = c(0,0.5,2),
                 c=1, a = 0.2, b = 0, s = 2, kappa=5) {
  .NotYetImplemented()
}

#' @rdname IMMdist
#' @export
qimm <- function(p, mu=c(0,2,-1.5), dist = c(0,0.5,2),
                 c=1, a = 0.2, b = 0, s = 2, kappa=5) {
  .NotYetImplemented()
}

#' @rdname IMMdist
#' @export
rimm <- function(n, mu=c(0,2,-1.5), dist = c(0,0.5,2),
                 c=1, a = 0.2, b = 1, s = 2, kappa=5) {
  stopif(isTRUE(any(kappa < 0)), "kappa must be non-negative")
  stopif(length(mu) != length(dist),
         "The number of items does not match the distances provided from the cued location.")
  stopif(isTRUE(any(s < 0)), "s must be non-negative")
  stopif(isTRUE(any(dist < 0)), "all distances have to be positive.")

  x <-  seq(-pi,pi,length.out = 361)
  maxy <- max(dimm(x, mu, dist, c, a, b, s, kappa))
  xa <- c()

  .rimm_inner <- function(n, mu, dist, c, a, b, s, kappa, xa) {
    x <- stats::runif(n, -pi, pi)
    y <- stats::runif(n, 0, 1) * maxy
    accept <- y < dimm(x, mu, dist, c, a, b, s, kappa)
    xa <- c(xa, x[accept])

    if (length(xa) < n) {
      return(.rimm_inner(n, mu, dist, c, a, b, s, kappa, xa))
    }

    xa[1:n]
  }

  .rimm_inner(n, mu, dist, c ,a ,b ,s ,kappa , xa)
}
