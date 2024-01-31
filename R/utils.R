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


#' Execute a Function Call
#'
#' Execute a function call similar to \code{\link{do.call}}, but without
#' deparsing function arguments. For large number of arguments (i.e., more
#' than a few thousand) this function currently is somewhat inefficient
#' and should be used with care in this case.
#'
#' @param what Either a function or a non-empty character string naming the
#'   function to be called.
#' @param args A list of arguments to the function call. The names attribute of
#'   \code{args} gives the argument names.
#' @param pkg Optional name of the package in which to search for the
#'   function if \code{what} is a character string.
#' @param envir An environment within which to evaluate the call.
#'
#' @return The result of the (evaluated) function call.
#'
#' @details
#' code adapted from [brms]
#'
#'
#' @keywords internal
#' @export
do_call <- function(what, args, pkg = NULL, envir = parent.frame()) {
  call <- ""
  if (length(args)) {
    if (!is.list(args)) {
      stop2("'args' must be a list.")
    }
    fun_args <- names(args)
    if (is.null(fun_args)) {
      fun_args <- rep("", length(args))
    } else {
      nzc <- nzchar(fun_args)
      fun_args[nzc] <- paste0("`", fun_args[nzc], "` = ")
    }
    names(args) <- paste0(".x", seq_along(args))
    call <- paste0(fun_args, names(args), collapse = ",")
  } else {
    args <- list()
  }
  if (is.function(what)) {
    args$.fun <- what
    what <- ".fun"
  } else {
    what <- paste0("`", as_one_character(what), "`")
    if (!is.null(pkg)) {
      what <- paste0(as_one_character(pkg), "::", what)
    }
  }
  call <- paste0(what, "(", call, ")")
  eval2(call, envir = args, enclos = envir)
}


# create a named list using object names
nlist <- function(...) {
  m <- match.call()
  dots <- list(...)
  no_names <- is.null(names(dots))
  has_name <- if (no_names) FALSE else nzchar(names(dots))
  if (all(has_name)) return(dots)
  nms <- as.character(m)[-1]
  if (no_names) {
    names(dots) <- nms
  } else {
    names(dots)[!has_name] <- nms[!has_name]
  }
  dots
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
