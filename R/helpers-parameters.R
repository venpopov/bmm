#' Transform kappa of the von Mises distribution to the circular standard deviation
#' @description This function transforms the precision parameter kappa of the von Mises
#'    distribution to the circular standard deviation.
#'    Adapted from Matlab code by Paul Bays (https://www.paulbays.com/code.php)
#'
#' @param K numeric. A vector of kappa values.
#' @return A vector of sd values.
#' @export
#' @examples
#' kappas <- runif(1000, 0.01, 100)
#'
#' # calcualte SD (in radians)
#' SDs <- k2sd(kappas)
#'
#' # transform SDs from radians to degrees
#' SDs_degress <- SDs * 180 / pi
#'
#' # plot the relationship between kappa and circular SD
#' plot(kappas,SDs)
#' plot(kappas,SDs_degress)
#'
k2sd <- function (K) {
  S <- matrix(0,1,length(K))
  for (j in 1:length(K)) {
    if (K[j]==0) S[j] = Inf
    if (is.infinite(K[j])) S[j] = 0
    if (K[j] >= 0 & !is.infinite(K[j])) {
      S[j] = sqrt(-2*log(besselI(K[j],1)/besselI(K[j],0)));
    }
  }
  return(as.numeric(S))
}


#' Convert between parametrizations of the c parameter of the SDM distribution
#'
#' @name c_parametrizations
#' @inheritParams SDMdist
#' @return \code{c_bessel2sqrtexp} converts the memory strength parameter (c)
#'   from the bessel parametrization to the sqrtexp parametrization,
#'   \code{c_sqrtexp2bessel} converts from the sqrtexp parametrization to the
#'   bessel parametrization.
#' @details See \code{vignette("bmm_models")} for details on the
#'   parameterization. The sqrtexp parametrization is the default in the
#'   \code{bmm} package.
#' @export
c_sqrtexp2bessel <- function(c, kappa) {
  if (isTRUE(any(kappa < 0))) {
    stop("kappa must be non-negative")
  }

  if (isTRUE(any(c < 0))) {
    stop("c must be non-negative")
  }

  c * besselI(kappa,0, expon.scaled = TRUE) * sqrt(2 * pi * kappa)
}

#' @rdname c_parametrizations
#' @export
c_bessel2sqrtexp <- function(c, kappa) {
  if (isTRUE(any(kappa < 0))) {
    stop("kappa must be non-negative")
  }
  if (isTRUE(any(c < 0))) {
    stop("c must be non-negative")
  }
  c / (besselI(kappa,0, expon.scaled = TRUE) * sqrt(2 * pi * kappa))
}
