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
      mc.cores = parallel::detectCores(),
      bmm.sort_data = opts$sort_data
    ),
    .local_envir=env)

  # return only options that can be passed to brms
  brms_args <- names(formals(brms::brm))
  opts <- opts[names(opts) %in% brms_args]
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

stop_quietly <- function() {
  opt <- options(show.error.messages = FALSE)
  on.exit(options(opt))
  stop()
}


message_not_ordered <- function(model, data, formula) {
  message("\n\nData is not ordered by predictors.\nYou can speed up the model ",
          "estimation up to several times (!) by ordering the data by all your ",
          "predictor columns.\n\n")
  caution_msg <- paste(strwrap("* caution: if you chose Option 2, you need to be careful
    when using brms postprocessing methods that rely on the data order, such as
    generating predictions. Assuming you assigned the result of fit_model to a
    variable called `fit`, you can extract the sorted data from the fitted object
    with:\n\n   data_sorted <- fit$fit_args$data", width=80), collapse = "\n")
  caution_msg <- crayon::red(caution_msg)

  if(interactive()) {
    var <- utils::menu(c("Yes (note: you will receive code to sort your data)",
             paste0("Let bmm sort the data for you and continue with the faster model fitting ",
                    crayon::red("(*)")),
             paste0("No, I want to continue with the slower estimation\n\n", caution_msg, collapse = "\n")),
             title="Do you want to stop and sort your data? (y/n): ")
    if(var == 1) {
      message("Please sort your data by all predictors and then re-run the model.")
      data_name <- attr(data, "data_name")
      if (is.null(data_name)) {
        data_name <- deparse(substitute(data))
      }
      # TODO: maybe just write a function which given a formula and data,
      # returns the sorted data? not necessary for now
      message("To sort your data, use the following code:\n\n")
      message(crayon::green("library(dplyr)"))
      message(crayon::green(data_name, "_sorted <- ", data_name, " %>% arrange(",
                            paste(extract_vars(formula), collapse = ", "),
                            ")\n\n",
                            sep=""))
      message("Then re-run the model with the newly sorted data.")
      stop_quietly()
    } else if (var == 2) {
      message("Your data has been sorted by the following predictors: ", paste(extract_vars(formula), collapse = ", "),'\n')
      preds <- extract_vars(formula)
      data <- dplyr::arrange_at(data, preds)
    }
  }
  data
}


