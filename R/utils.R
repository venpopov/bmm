#' Pipe operator
#'
#' See `magrittr::[\%>\%][magrittr::pipe]` for details.
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
#' `softmax` returns the value of the softmax function
#' `softmaxinv` returns the value of the inverse-softmax function
#'
#' The softmax function is a bijective function that maps a real vector with length `m-1` to a probability vector
#' with length `m` with all non-zero probabilities.  The present functions define the softmax function and its inverse, both with a tuning
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
#' @keywords transform
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

#' @title Configure local options during model fitting
#
#' @description Currently it serves to set local options for parallel processing and update
#' number of chains if the number of chains is greater than the number of cores.
#'
#' @param opts A list of options
#' @param env The environment in which to set the options - when set to parent.frame()
#' the changes would apply to the environment of the function that called it. In our
#' case, this is the environment of the fit_model() function. Changes will not be
#' propagated to the user environment.
#' @keywords internal
#' @returns A list of options to pass to brm()
configure_options <- function(opts, env=parent.frame()) {
  if (opts$parallel) {
    cores = parallel::detectCores()
    if (opts$chains >  parallel::detectCores()) {
      opts$chains <- parallel::detectCores()
    }
  } else {
    cores = NULL
  }
  withr::local_options(
    list(
      mc.cores = cores,
      bmm.silent = opts$silent
    ),
    .local_envir=env)

  # return only options that can be passed to brms/rstan/cmdstanr
  exclude_args <- c('parallel')
  opts <- opts[not_in(names(opts), exclude_args)]
  return(opts)
}

# ------------------------------------------------------------------------------
# check if a value is not in a vector
not_in <- function(value, vector) {
  !(value %in% vector)
}

# ------------------------------------------------------------------------------
# check if a key is not in a list
not_in_list <- function(key, list) {
  !(key %in% names(list))
}


#' wrappers to construct a brms nlf and lf formulas from multiple string arguments
#' @param ... string parts of the formula separated by commas
#' @examples
#' kappa_nts <- paste0('kappa_nt', 1:4)
#' glue_nlf(kappa_nts[i], ' ~ kappa')  ## same as brms::nlf(kappa_nt1 ~ kappa)
#' @noRd
glue_nlf <- function(...) {
  dots = list(...)
  brms::nlf(stats::as.formula(collapse(...)))
}

# like glue_nlf but for lf formulas
glue_lf <- function(...) {
  dots = list(...)
  brms::lf(stats::as.formula(collapse(...)))
}

#' wrapper function to call brms, saving fit_args if backend is mock for testing
#' not used directly, but called by fit_model(). If fit_model() is run with
#' backend="mock", then we can perform tests on the fit_args to check if the
#' model configuration is correct. Avoids compiling and running the model
#' @noRd
call_brm <- function(fit_args) {
  fit <- brms::do_call(brms::brm, fit_args)
  if (!is.null(fit_args$backend) && fit_args$backend == "mock") {
    fit$fit_args <- fit_args
  }
  return(fit)
}


# function to ensure that if the user wants to overwrite an argument (such as
# init), they can
combine_args <- function(args) {
  config_args <- args$config_args
  opts <- args$opts
  dots <- args$dots
  if (is.null(dots)) {
    return(c(config_args, opts))
  }
  for (i in names(dots)) {
    if (not_in(i, c('family'))) {
      config_args[[i]] <- dots[[i]]
    } else {
      stop('You cannot provide a family argument to fit_model. Please use the model argument instead.')
    }
  }
  c(config_args, opts)
}



message2 <- function(...) {
  silent <- getOption('bmm.silent', 1)
  if (silent < 2) {
    message(...)
  }
  invisible()
}


# function to ensure proper reading of stan files
read_lines2 <- function (con) {
  lines <- readLines(con, n = -1L, warn = FALSE)
  paste(lines, collapse = "\n")
}


# for testing purposes
install_and_load_bmm_version <- function(version) {
  if ("package:bmm" %in% search()) {
    detach("package:bmm", unload=TRUE)
  }
  path <- paste0(.libPaths()[1], "/bmm-", version)
  if (!dir.exists(path) || length(list.files(path)) == 0 || length(list.files(paste0(path, "/bmm"))) == 0) {
    dir.create(path)
    remotes::install_github(paste0("venpopov/bmm@",version), lib=path)
  }
  library(bmm, lib.loc=path)
}


#' Extract information from a brmsfit object
#' @param fit A brmsfit object, or a list of brmsfit objects
#' @param what String. What to return:
#'  - "time" for the sampling time per chain
#'  - "time_mean" for the mean sampling time
#' @return Depends on `what` and the class of `fit`. For `brmsfit` objects,
#'   information about the single fit is returned. For `brmsfit_list` objects, a
#'   list or data.frame with the information for each fit is returned.
#'  - "time": A data.frame with the sampling time per chain
#'  - "time_mean": A named numeric vector with the mean sampling time
#' @export
fit_info <- function(fit, what) {
  UseMethod("fit_info")
}

#' @export
fit_info.brmsfit <- function(fit, what) {
  fit_attr <- attributes(fit$fit)
  metadata <- fit_attr$metadata
  switch(what,
         time = metadata$time$chains,
         time_mean = colMeans(metadata$time$chains),
  )
}

#' @export
fit_info.brmsfit_list <- function(fit, what) {
  .NotYetImplemented()
}

# if x is a variable present in the data, return x, else error
is_data_var <- function(x, data) {
  is.character(x) && length(x) == 1 && x %in% names(data)
}

is_try_warning <- function(x) {
  inherits(x, "warning")
}

as_numeric_vector <- function(x) {
  out <- tryCatch(as.numeric(as.character(x)), warning = function(w) w)
  if (is_try_warning(out)) {
    stop2("Cannot coerce '", x, "' to a numeric vector")
  }
  out
}
