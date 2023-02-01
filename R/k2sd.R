#' Tranform kappa to sd
#' @description Adapted from Matlab code by Paul Bays (https://www.paulbays.com/code.php)
#' @param K A vector of kappa values
#'
#' @return A vector of sd values
#' @export
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
  return(as.numeric(x))
}
