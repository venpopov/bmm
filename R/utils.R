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
  if (isTRUE(opts$parallel)) {
    cores = parallel::detectCores()
    if (opts$chains >  parallel::detectCores()) {
      opts$chains <- parallel::detectCores()
    }
  } else {
    cores = NULL
  }
  if (not_in_list('silent', opts)) {
    opts$silent <- getOption('bmm.silent', 1)
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
#' if checkpoints is not NULL, use [chkptstanr]
#' @noRd
run_model <- function(fit_args, checkpoints, checkpoints_folder, checkpoints_path) {
  if (is.null(checkpoints)) {
    fit <- brms::do_call(brms::brm, fit_args)
    return(fit)
  }

  if (is.null(checkpoints_folder)) {
    stop2("You must provide a folder name to save the checkpoints")
  }

  # needed because of silly setup in chkptstanr::create_dir(). Eventually can remove
  # if I rework their function
  if (xfun::is_abs_path(checkpoints_folder)) {
    stop2("The checkpoints_folder argument must be a relative path.\n",
          "You can provide a base path in which to create the folder",
          " with the checkpoints_path argument, or leave it as NULL to use",
          " the current working directory.")
  }

  if (!requireNamespace("chkptstanr", quietly = TRUE)) {
    stop2(
      "\nPackage \"chkptstanr\" must be installed to use this function.\n",
      "The current CRAN version of chkptstanr has a bug that prevents it from",
      "working. Until the issue is fixed, you can install a working forked version",
      "of chkptstanr with:\n\n",
      "remotes::install_github(\"venpopov/chkptstanr\")"
    )
  }

  if (fit_args$backend == "rstan") {
    stop2("Checkpoints are not supported for rstan. Use backend='cmdstanr' instead.")
  }

  if (!requireNamespace("cmdstanr", quietly = TRUE)) {
    stop2("Package \"cmdstanr\" must be installed to use this function.")
  }

  if (!is.null(fit_args$iter)) {
    fit_args$iter_warmup <- ifelse(is.null(fit_args$warmup), fit_args$iter/2, fit_args$warmup)
    fit_args$iter_sampling <- fit_args$iter - fit_args$iter_warmup
    fit_args$iter <- NULL
    fit_args$warmup <- NULL
  }

  fit_args$iter_per_chkpt <- checkpoints
  fit_args$path <- file_path2(checkpoints_path, checkpoints_folder)
  if (!dir.exists(fit_args$path)) {
    # TODO: this check should really be implemented in chkptstanr::create_folder
    fit_args$path <- chkptstanr::create_folder(checkpoints_folder, path = checkpoints_path)
  }
  attr(fit_args$path, "info") <- "chkpt_brms folder"

  fit <- brms::do_call(chkptstanr::chkpt_brms, fit_args)
  fit
}

# wrapper around file path ignoring null values
file_path2 <- function(...) {
  dots <- list(...)
  dots <- dots[!sapply(dots, is.null)]
  do.call(file.path, dots)
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

is_bmmmodel <- function(x) {
  inherits(x, "bmmmodel")
}

is_supported_bmmmodel <- function(x) {
  valid_models <- supported_models(print_call = FALSE)
  is_bmmmodel(x) && inherits(x, valid_models)
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
  dpars <- names(formula)
  predictors <- rhs_vars(formula)
  predictors <- predictors[not_in(predictors, dpars)]
  predictors <- predictors[predictors %in% colnames(data)]

  if(is.null(sort_data) && !is_data_ordered(data, formula)) {
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
                              paste(predictors, collapse = ", "),
                              ")\n\n",
                              sep=""))
        message("Then re-run the model with the newly sorted data.")
        stop_quietly()
      } else if (var == 2) {
        message("Your data has been sorted by the following predictors: ", paste(predictors, collapse = ", "),'\n')
        data <- dplyr::arrange_at(data, predictors)
      }
    }
  } else if (isTRUE(sort_data)) {
    data <- dplyr::arrange_at(data, predictors)
    message("\nYour data has been sorted by the following predictors: ", paste(predictors, collapse = ", "),'\n')
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

# when called from another function, it will return a vector of arguments that are
# missing from the call
missing_args <- function(which = -1) {
  parent_objects <- as.list(sys.frame(which))
  parent_args <- names(as.list(args(as.character(sys.call(which)[[1]]))))
  parent_args <- parent_args[!parent_args %in% c("...", "")]
  symbols <- names(parent_objects)[sapply(parent_objects, is.symbol)]
  missing <- symbols[symbols %in% parent_args]
  missing
}

# when called from another function, it will stop the execution if any of the
# required arguments are missing
stop_missing_args <- function() {
  missing <- missing_args(-2)
  fun <- as.character(sys.call(-1)[[1]])
  if (length(missing) > 0) {
    stop2("The following required arguments are missing in ", fun, "(): ", paste(missing, collapse = ", "))
  }
}

# custom method form printing nicely formatted character values via cat instead of print
#' @export
print.message <- function(x, ...) {
  cat(x, ...)
}


# returns either x, or all variables that match the regular expression x
# @param x character vector or regular expression
# @param all_variables character vector of all variables within which to search
# @param regex logical. If TRUE, x is treated as a regular expression
get_variables <- function(x, all_variables, regex = FALSE) {
  if (regex) {
    variables <- all_variables[grep(x, all_variables)]
    if (length(variables) == 0) {
      stop2("No variables found that match the regular expression '", x, "'")
    }
    return(variables)
  }
  x
}

# replace the base identical function with a S3 method
identical <- function(x, y, ...) {
  UseMethod("identical")
}

#' @export
identical.default <- function(x, y, ...) {
  base::identical(x, y, ...)
}

#' @export
identical.brmsformula <- function(x, y, ...) {
  res <- waldo::compare(x, y, ignore_formula_env = TRUE)
  length(res) == 0
}

#' @export
identical.bmmformula <- function(x, y, ...) {
  res <- waldo::compare(x, y, ignore_formula_env = TRUE)
  length(res) == 0
}

#' @export
identical.formula <- function(x, y, ...) {
  res <- waldo::compare(x, y, ignore_formula_env = TRUE)
  length(res) == 0
}
