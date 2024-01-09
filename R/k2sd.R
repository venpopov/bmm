#' Transform kappa of the von Mises distribution to the circular standard deviation
#' @description This function transforms the precision parameter kappa of the von Mises
#'    distribution to the circular standard deviation.
#'    Adapted from Matlab code by Paul Bays (https://www.paulbays.com/code.php)
#'
#' @param K numeric. A vector of kappa values.
#'
#' @return A vector of sd values.
#'
#' @export
#'
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
