#' Pipe operator
#'
#' See \code{magrittr::\link[magrittr:pipe]{\%>\%}} for details.
#'
#' @name %>%
#' @rdname pipe
#' @keywords internal
#' @export
#' @importFrom magrittr %>%
#' @usage lhs \%>\% rhs
#' @param lhs A value or the magrittr placeholder.
#' @param rhs A function call using the magrittr semantics.
#' @return The result of calling `rhs(lhs)`.
NULL


#' Softmax and logsoftmax functions and their inverse functions
#'
#' \code{softmax} returns the value of the softmax function
#' \code{softmaxinv} returns the value of the inverse-softmax function
#'
#' The softmax function is a bijective function that maps a real vector with length \code{m-1} to a probability vector
#' with length \code{m} with all non-zero probabilities.  The present functions define the softmax function and its inverse, both with a tuning
#' parameter.
#'
#' The current functions define the softmax as:
#'
#' \deqn{\Large P(\eta_i) = \frac{e^{\lambda \eta_i}}{1+ \sum_{j=1}^m e^{\lambda \eta_j}}}
#'
#' @param eta A numeric vector input
#' @param lambda Tuning parameter (a single positive value)
#' @param p A probability vector (i.e., numeric vector of non-negative values that sum to one)
#' @param eta A numeric vector input
#' @return Value of the softmax function or its inverse
#'
#' @details Code adapted from the [utilities](https://github.com/ben-oneill/utilities/) package
#'
#' @export
#' @examples
#' softmax(5:7)
#' softmaxinv(softmax(5:7))
softmax <- function(eta, lambda = 1) {

  stopifnot(requireNamespace("matrixStats", quietly = TRUE))

  #Compute the softmax function
  m     <- length(eta)+1
  DEN   <- matrixStats::logSumExp(c(lambda*eta, 0))
  LSOFT <- c(lambda*eta, 0) - DEN
  SOFT  <- exp(LSOFT)
  SOFT
}

#' @rdname softmax
#' @export
softmaxinv <- function(p, lambda = 1) {

  #Compute the inverse-softmax function
  m <- length(p)
  if (m > 1) {
    SOFTINV <- (base::log(p) - base::log(p[m]))[1:(m-1)]/lambda } else {
      SOFTINV <- numeric(0) }
  SOFTINV
}

# helper function to configure local options during model fitting
configure_options <- function(opts, env=parent.frame()) {
  if (opts$parallel) {
    withr::local_options(list(mc.cores = parallel::detectCores()), .local_envir=env)
    if (chains >  parallel::detectCores()) {
      chains <-  parallel::detectCores()
    }
  }
}

not_in <- function(value, vector) {
  !(value %in% vector)
}


not_in_list <- function(key, list) {
  !(key %in% names(list))
}
