#' Transform kappa of the von Mises distribution to the circular standard
#' deviation
#' @description This function transforms the precision parameter kappa of the
#'   von Mises distribution to the circular standard deviation. Adapted from
#'   Matlab code by Paul Bays (https://www.paulbays.com/code.php)
#'
#' @param K numeric. A vector of kappa values.
#' @return A vector of sd values.
#' @keywords transform
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
k2sd <- function(K) {
  S <- matrix(0, 1, length(K))
  for (j in 1:length(K)) {
    if (K[j] == 0) S[j] <- Inf
    if (is.infinite(K[j])) S[j] <- 0
    if (K[j] >= 0 & !is.infinite(K[j])) {
      S[j] <- sqrt(-2 * log(besselI(K[j], 1, expon.scaled = T) / besselI(K[j], 0, expon.scaled = T)))
    }
  }
  as.numeric(S)
}


#' Convert between parametrizations of the c parameter of the SDM distribution
#'
#' @name c_parametrizations
#' @inheritParams SDMdist
#' @return A numeric vector of the same length as `c` and `kappa`.
#' @details
#' `c_bessel2sqrtexp` converts the memory strength parameter (c)
#'   from the bessel parametrization to the sqrtexp parametrization,
#'   `c_sqrtexp2bessel` converts from the sqrtexp parametrization to the
#'   bessel parametrization.
#' @keywords transform
#' @details See [the online article](https://venpopov.github.io/bmm/articles/bmm_sdm_simple.html) for details on the
#'   parameterization. The sqrtexp parametrization is the default in the
#'   `bmm` package.
#' @export
#'
#' @examples
#' c_bessel <- c_sqrtexp2bessel(c = 4, kappa = 3)
#' c_sqrtexp <- c_bessel2sqrtexp(c = c_bessel, kappa = 3)
#'
c_sqrtexp2bessel <- function(c, kappa) {
  stopif(isTRUE(any(kappa < 0)), "kappa must be non-negative")
  stopif(isTRUE(any(c < 0)), "c must be non-negative")
  c * besselI(kappa,0, expon.scaled = TRUE) * sqrt(2 * pi * kappa)
}

#' @rdname c_parametrizations
#' @keywords transform
#' @export
c_bessel2sqrtexp <- function(c, kappa) {
  stopif(isTRUE(any(kappa < 0)), "kappa must be non-negative")
  stopif(isTRUE(any(c < 0)), "c must be non-negative")
  c / (besselI(kappa,0, expon.scaled = TRUE) * sqrt(2 * pi * kappa))
}
