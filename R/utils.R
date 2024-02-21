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
  m     <- length(eta)+1
  DEN   <- matrixStats::logSumExp(c(lambda*eta, 0))
  LSOFT <- c(lambda*eta, 0) - DEN
  exp(LSOFT)
}

#' @rdname softmax
#' @export
softmaxinv <- function(p, lambda = 1) {
  m <- length(p)
  if (m > 1) {
    return((log(p) - log(p[m]))[1:(m-1)]/lambda)
  }
  numeric(0)
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
      bmm.silent = opts$silent,
      bmm.sort_data = opts$sort_data
    ),
    .local_envir=env)

  # return only options that can be passed to brms/rstan/cmdstanr
  exclude_args <- c('parallel', 'sort_data')
  opts[not_in(names(opts), exclude_args)]
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
  fit$bmm_fit_args <- fit_args
  class(fit) <- c('bmmfit','brmsfit')
  fit
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
#' @keywords extract_info
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

stop_quietly <- function() {
  opt <- options(show.error.messages = FALSE)
  on.exit(options(opt))
  stop()
}


# for some models it is faster to sample if the normalizing constant is calculated
# only once for all trials that have the same value for the predictors. Currently
# this is only used in the sdmSimple model, and to work it requires that the
# data is ordered by the predictor variables. This function checks if the data is
# ordered by the predictors, and if not, it suggests to the user to sort the data
order_data_query <- function(model, data, formula) {
  sort_data <- getOption("bmm.sort_data", NULL)
  if(is.null(sort_data) & !is_data_ordered(data, formula)) {
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
        message("To sort your data, use the following code:\n\n")
        message(crayon::green("library(dplyr)"))
        message(crayon::green(data_name, "_sorted <- ", data_name, " %>% arrange(",
                              paste(rhs_vars(formula), collapse = ", "),
                              ")\n\n",
                              sep=""))
        message("Then re-run the model with the newly sorted data.")
        stop_quietly()
      } else if (var == 2) {
        message("Your data has been sorted by the following predictors: ", paste(rhs_vars(formula), collapse = ", "),'\n')
        preds <- rhs_vars(formula)
        data <- dplyr::arrange_at(data, preds)
      }
    }
  } else if (isTRUE(sort_data)) {
    preds <- rhs_vars(formula)
    data <- dplyr::arrange_at(data, preds)
    message("\nYour data has been sorted by the following predictors: ", paste(rhs_vars(formula), collapse = ", "),'\n')
    caution_msg <- paste(strwrap("* caution: you have set `sort_data=TRUE`. You need to be careful
        when using brms postprocessing methods that rely on the data order, such as
        generating predictions. Assuming you assigned the result of fit_model to a
        variable called `fit`, you can extract the sorted data from the fitted object
        with:\n\n   data_sorted <- fit$fit_args$data", width=80), collapse = "\n")
    caution_msg <- crayon::red(caution_msg)
    message(caution_msg)
  }
  data
}

#' @title Pass options to save parameters from `bmm` t0 `brms`
#' @description
#'   When calling `fit_model` with additional information to save parameters you can use this
#'   function to pass information about saving parameter draws to `brms` without having to load `brms`.
#'   Alternatively, you can also load `brms` and call `save_pars`. For details see ?brms::save_pars().
#' @param group logical. Indicates if group-level parameter for each level of the grouping variable should be saved.
#'   Default is `TRUE`.
#' @param all logical. Indicates if draws from all variables defined in the STAN parameter block
#'   should be saved. Default is `FALSE`. If you want to use methods such as `bridge_sampler` or
#'   `bayes_factor` you need to set this to `TRUE`.
#' @param manual character vector. Vector naming the STAN variables for which draws should be saved.
#'   These names need to match the names inside the STAN code before renaming. You can access these
#'   by having a lot at the STAN code using the `get_stancode` or `get_stancode_parblock`.
#' @return A list of `brms` class "save_pars.
#' @export
#' @examples
#' # example code
#'
save_pars_bmm2brms <- function(group = TRUE, all = FALSE, manual = NULL) {
  save_pars <- brms::save_pars(group = group, all = all, manual = manual)
  save_pars
}
