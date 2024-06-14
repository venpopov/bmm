############################################################################# !
# CHECK_DATA METHODS                                                     ####
############################################################################# !


#' @title Generic S3 method for checking data based on model type
#' @description Called by [bmm()] to automatically perform checks on the
#'   data depending on the model type. It will call the appropriate check_data
#'   methods based on the list of classes defined in the .model_* functions. For
#'   models with several classes listed, it will call the functions in the order
#'   they are listed. Thus, any operations that are common to a group of models
#'   should be defined in the appropriate check_data.* function, where \*
#'   corresponds to the shared class. For example, for the .model_imm_abc model,
#'   this corresponds to the following order of check_data.* functions:
#'   check_data() -> check_data.circular(), check_data.non_targets() the output of the
#'   final function is returned to bmm().
#' @param model A model list object returned from check_model()
#' @param data The user supplied data.frame containing the data to be checked
#' @param formula The user supplied formula
#' @return A data.frame with the same number of rows as the input data, but with
#'   additional columns added as necessary, any necessary transformations
#'   applied, and attributes added to the data.frame for later use. If you need
#'   to reuse variables created by the check_data.* functions in subsequent
#'   stages (e.g. in configure_model()), you can store and access them using the
#'   attr() function.
#' @export
#'
#' @keywords internal developer
#'
#' @examples
#' data <- oberauer_lin_2017
#' model <- sdmSimple(resp_error = "dev_rad")
#' formula <- bmf(c ~ 1, kappa ~ 1)
#' checked_data <- check_data(model, data, formula)
check_data <- function(model, data, formula) {
  UseMethod("check_data")
}

#' @export
check_data.default <- function(model, data, formula) {
  return(data)
}

#' @export
check_data.bmmodel <- function(model, data, formula) {
  stopif(missing(data), "Data must be specified using the 'data' argument.")
  data <- try(as.data.frame(data), silent = TRUE)
  stopif(is_try_error(data), "Argument 'data' must be coercible to a data.frame.")
  stopif(!isTRUE(nrow(data) > 0L), "Argument 'data' does not contain observations.")

  attr(data, "data_name") <- substitute_name(data, envir = eval(parent.frame()))
  attr(data, "checked") <- TRUE
  NextMethod("check_data")
}


#' @export
check_data.circular <- function(model, data, formula) {
  resp_name <- model$resp_vars[[1]]
  stopif(
    not_in(resp_name, colnames(data)),
    "The response variable '{resp_name}' is not present in the data."
  )
  warnif(
    max(abs(data[[resp_name]]), na.rm = T) > 2 * pi,
    "It appears your response variable is in degrees.
          The model requires the response variable to be in radians.
          The model will continue to run, but the results may be compromised."
  )

  NextMethod("check_data")
}


#' @export
check_data.non_targets <- function(model, data, formula) {
  nt_features <- model$other_vars$nt_features
  warnif(
    max(abs(data[, nt_features]), na.rm = T) > 2 * pi,
    "It appears at least one of your non_target variables are in degrees.
          The model requires these variable to be in radians.
          The model will continue to run, but the results may be compromised."
  )

  ss <- check_var_set_size(model$other_vars$set_size, data)
  max_set_size <- ss$max_set_size
  ss_numeric <- ss$ss_numeric

  stopif(
    !isTRUE(all.equal(length(nt_features), max_set_size - 1)),
    "The number of columns for non-target values in the argument \\
         'nt_features' should equal max(set_size)-1"
  )

  # create index variables for nt_features and correction variable for theta due to set_size
  lure_idx_vars <- paste0("LureIdx", 1:(max_set_size - 1))
  for (i in 1:(max_set_size - 1)) {
    data[[lure_idx_vars[i]]] <- ifelse(ss_numeric >= (i + 1), 1, 0)
  }
  data$ss_numeric <- ss_numeric
  data$inv_ss <- 1 / (ss_numeric - 1)
  data$inv_ss <- ifelse(is.infinite(data$inv_ss), 1, data$inv_ss)
  data[, nt_features][is.na(data[, nt_features])] <- 0

  # save some variables for later use
  attr(data, "max_set_size") <- max_set_size
  attr(data, "lure_idx_vars") <- lure_idx_vars

  NextMethod("check_data")
}


check_var_set_size <- function(set_size, data) {
  stopif(
    length(set_size) > 1,
    "The set_size variable '{set_size}' must be a single numeric value or \\
          a single variable in your data. You provided a vector of length \\
          {length(set_size)}"
  )

  # class check - is set_size a single numeric value or a variable in the data
  # coericble to a numeric vector?
  if (is_data_var(set_size, data)) {
    ss_numeric <- try(as_numeric_vector(data[[set_size]]), silent = T)

    stopif(
      is_try_error(ss_numeric),
      "The set_size variable '{set_size}' must be coercible to a numeric \\
           vector. Did you code your set size as a character vector?"
    )

    max_set_size <- max(ss_numeric, na.rm = T)
  } else {
    max_set_size <- try(as_one_integer(set_size), silent = T)

    stopif(
      is_try_error(max_set_size) | is.logical(set_size),
      "The set_size variable '{set_size}' must be either a variable in your \\
       data or a single numeric value"
    )

    ss_numeric <- rep(max_set_size, nrow(data))
  }

  # value check
  stopif(
    any(ss_numeric < 1, na.rm = T),
    "Values of the set_size variable '{set_size}' must be greater than 0"
  )

  stopif(
    any(ss_numeric %% 1 != 0, na.rm = T),
    "Values of the set_size variable '{set_size}' must be whole numbers"
  )

  list(max_set_size = max_set_size, ss_numeric = ss_numeric)
}


############################################################################# !
# HELPER FUNCTIONS                                                       ####
############################################################################# !

#' Calculate response error relative to non-target values
#'
#' @description Given a vector of responses, and the values of non-targets, this
#'   function computes the error relative to each of the non-targets.
#' @param data A `data.frame` object where each row is a single observation
#' @param response Character. The name of the column in `data` which contains
#'   the response
#' @param nt_features Character vector. The names of the columns in `data` which
#'   contain the values of the non-targets
#' @keywords transform
#' @return A `data.frame` with n*m rows, where n is the number of rows of `data`
#'   and m is the number of non-target variables. It preserves all other columns
#'   of `data`, except for the non-target locations, and adds a column `y_nt`,
#'   which contains the transformed response error relative to the non-targets
#'
#' @export
#'
#' @examples
#' data <- oberauer_lin_2017
#' data <- calc_error_relative_to_nontargets(data, "dev_rad", paste0("col_nt", 1:7))
#' hist(data$y_nt, breaks = 100)
#'
calc_error_relative_to_nontargets <- function(data, response, nt_features) {
  y <- y_nt <- non_target_name <- non_target_value <- NULL
  data <- data %>%
    tidyr::gather(non_target_name, non_target_value, eval(nt_features))

  data$y_nt <- wrap(data[[response]] - data[["non_target_value"]])
  data
}

#' @title Wrap angles that extend beyond (-pi;pi)
#' @description On the circular space, angles can be only in the range (-pi;pi
#'   or -180;180). When subtracting angles, this can result in values outside of
#'   this range. For example, when calculating the difference between a value of
#'   10 degrees minus 340 degrees, this results in a difference of 330 degrees.
#'   However, the true difference between these two values is -30 degrees. This
#'   function wraps such values, so that they occur in the circle
#' @param x A numeric vector, matrix or data.frame of angles to be wrapped. In
#'   radians (default) or degrees.
#' @param radians Logical. Is x in radians (default=TRUE) or degrees (FALSE)
#' @return An object of the same type as x
#' @keywords transform
#' @export
#' @examples
#' x <- runif(1000, -pi, pi)
#' y <- runif(1000, -pi, pi)
#' diff <- x - y
#' hist(diff)
#' wrapped_diff <- wrap(x - y)
#' hist(wrapped_diff)
#'
wrap <- function(x, radians = TRUE) {
  stopifnot(is.logical(radians))
  if (radians) {
    return(((x + pi) %% (2 * pi)) - pi)
  }
  ((x + 180) %% (2 * 180)) - 180
}

#' @title Convert degrees to radians or radians to degrees.
#' @description The helper functions `deg2rad` and `rad2deg` should add
#' convenience in transforming data from degrees to radians and from radians to
#' degrees.
#'
#' @name circle_transform
#' @param deg A numeric vector of values in degrees.
#' @param rad A numeric vector of values in radians.
#' @return A numeric vector of the same length as `deg` or `rad`.
#' @keywords transform
#' @export
#' @examples
#' degrees <- runif(100, min = 0, max = 360)
#' radians <- deg2rad(degrees)
#' degrees_again <- rad2deg(radians)
deg2rad <- function(deg) {
  deg * pi / 180
}

#' @rdname circle_transform
#' @export
rad2deg <- function(rad) {
  rad * 180 / pi
}

#' @title Stan data for `bmm` models
#' @description Given the `model`, the `data` and the `formula` for the model,
#'   this function will return the combined stan data generated by `bmm` and
#'   `brms`
#'
#' @inheritParams bmm
#' @aliases standata
#' @param object A `bmmformula` object
#' @param ... Further arguments passed to [brms::standata()]. See the
#'   description of [brms::standata()] for more details
#'
#' @return A named list of objects containing the required data to fit a bmm
#'   model with Stan.
#'
#' @seealso [supported_models()], [brms::standata()]
#'
#' @keywords extract_info
#'
#' @examples
#' sdata1 <- standata(bmf(c ~ 1, kappa ~ 1),
#'   data = oberauer_lin_2017,
#'   model = sdm(resp_error = "dev_rad")
#' )
#' str(sdata1)
#' @importFrom brms standata
#' @export
standata.bmmformula <- function(object, data, model, prior = NULL, ...) {
  # check model, formula and data, and transform data if necessary
  formula <- object
  configure_options(list(...))
  model <- check_model(model, data, formula)
  data <- check_data(model, data, formula)
  formula <- check_formula(model, data, formula)

  # generate the model specification to pass to brms later
  config_args <- configure_model(model, data, formula)

  # configure the default prior and combine with user-specified prior
  prior <- configure_prior(model, data, config_args$formula, prior)

  # extract stan code
  dots <- list(...)
  fit_args <- combine_args(nlist(config_args, dots, prior))
  fit_args$object <- fit_args$formula
  fit_args$formula <- NULL
  brms::do_call(brms::standata, fit_args)
}

# check if the data is sorted by the predictors
is_data_ordered <- function(data, formula) {
  dpars <- names(formula)
  predictors <- rhs_vars(formula)
  predictors <- predictors[not_in(predictors, dpars)]
  predictors <- predictors[predictors %in% colnames(data)]
  data <- data[, predictors]
  if (length(predictors) > 1) {
    gr_idx <- do.call(paste, c(data, list(sep = "_")))
  } else {
    gr_idx <- unlist(data)
  }
  is_ordered <- !has_nonconsecutive_duplicates(gr_idx)
  is_ordered
}

# checks if all repetitions of a given value are consecutive in a vector
# by iterating over unique values and checking if all their positions are
# consecutive
has_nonconsecutive_duplicates <- function(vec) {
  unique_vals <- unique(vec)
  cond <- TRUE
  for (val in unique_vals) {
    positions <- which(vec == val)
    cond <- cond & all(diff(positions) == 1)
  }
  !cond
}
