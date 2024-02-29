#############################################################################!
# CHECK_DATA METHODS                                                     ####
#############################################################################!


#' @title Generic S3 method for checking data based on model type
#' @description Called by fit_model() to automatically perform checks on the
#'   data depending on the model type. It will call the appropriate check_data
#'   methods based on the list of classes defined in the .model_* functions. For
#'   models with several classes listed, it will call the functions in the order
#'   they are listed. Thus, any operations that are common to a group of models
#'   should be defined in the appropriate check_data.* function, where \*
#'   corresponds to the shared class. For example, for the .model_IMMabc model,
#'   this corresponds to the following order of check_data.* functions:
#'   check_data() -> check_data.vwm(), check_data.nontargets() the output of the
#'   final function is returned to fit_model().
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
#' @keywords internal, developer
check_data <- function(model, data, formula) {
  UseMethod("check_data")
}

#' @export
check_data.default <- function(model, data, formula) {
  return(data)
}

#' @export
check_data.bmmmodel <- function(model, data, formula) {
  if (missing(data)) {
    stop("Data must be specified using the 'data' argument.")
  }
  data <- try(as.data.frame(data), silent = TRUE)
  if (is_try_error(data)) {
    stop("Argument 'data' must be coercible to a data.frame.")
  }
  if (!isTRUE(nrow(data) > 0L)) {
    stop("Argument 'data' does not contain observations.")
  }

  attr(data, 'data_name') <- substitute_name(data, envir = eval(parent.frame()))
  attr(data, 'checked') <- TRUE
  NextMethod("check_data")
}


#' @export
check_data.vwm <- function(model, data, formula) {
  resp_name <- model$resp_vars[[1]]
  if (not_in(resp_name, colnames(data))) {
    stop(paste0("The response variable '", resp_name, "' is not present in the data."))
  }
  if (max(abs(data[[resp_name]]), na.rm = T) > 2*pi) {
    warning('It appears your response variable is in degrees.\n
             The model requires the response variable to be in radians.\n
             The model will continue to run, but the results may be compromised.')
  }

  NextMethod("check_data")
}


#' @export
check_data.nontargets <- function(model, data, formula) {
  nt_features <- model$other_vars$nt_features
  if (max(abs(data[,nt_features]), na.rm = T) > 2*pi) {
    warning('It appears at least one of your non_target variables are in degrees.\n
             The model requires these variable to be in radians.\n
             The model will continue to run, but the results may be compromised.')
  }

  ss <- check_var_setsize(model$other_vars$setsize, data)
  max_setsize <- ss$max_setsize
  ss_numeric <- ss$ss_numeric

  if (!isTRUE(all.equal(length(nt_features), max_setsize - 1))) {
    stop("The number of columns for non-target values in the argument ",
         "'nt_features' should equal max(setsize)-1")
  }


  # create index variables for nt_features and correction variable for theta due to setsize
  lure_idx_vars <- paste0('LureIdx',1:(max_setsize - 1))
  for (i in 1:(max_setsize - 1)) {
    data[[lure_idx_vars[i]]] <- ifelse(ss_numeric >= (i + 1), 1, 0)
  }
  data$ss_numeric <- ss_numeric
  data$inv_ss = 1/(ss_numeric - 1)
  data$inv_ss = ifelse(is.infinite(data$inv_ss), 1, data$inv_ss)
  data[,nt_features][is.na(data[,nt_features])] <- 0

  # save some variables for later use
  attr(data, 'max_setsize') <- max_setsize
  attr(data, 'lure_idx_vars') <- lure_idx_vars

  NextMethod("check_data")
}


check_var_setsize <- function(setsize, data) {
  if (length(setsize) > 1) {
    stop2("The setsize variable '", setsize, "' must be a single numeric value or a single variable in your data",
          " You provided a vector of length ", length(setsize))
  }
  # class check - is setsize a single numeric value or a variable in the data
  # coericble to a numeric vector?
  if (is_data_var(setsize, data)) {
    ss_numeric <- try(as_numeric_vector(data[[setsize]]), silent=T)
    if (is_try_error(ss_numeric)) {
      stop2("The setsize variable '", setsize, "' must be coercible to a numeric vector.\n",
            "Did you code your set size as a character vector?")
    }
    max_setsize <- max(ss_numeric, na.rm = T)
  } else {
    max_setsize <- try(as_one_integer(setsize), silent=T)
    if (is_try_error(max_setsize) | is.logical(setsize)) {
      stop2("The setsize variable '", setsize, "' must be either a variable in your data or ",
            "a single numeric value")
    }
    ss_numeric <- rep(max_setsize, nrow(data))
  }
  # value check
  if (any(ss_numeric < 1, na.rm = T)) {
    stop2("Values of the setsize variable '", setsize, "' must be greater than 0")
  }
  if (any(ss_numeric %% 1 != 0, na.rm = T)) {
    stop2("Values of the setsize variable '", setsize, "' must be whole numbers")
  }

  list(max_setsize = max_setsize, ss_numeric = ss_numeric)
}



check_data.M3 <- function(model, data, formula) {

  # Get the vector of the response variables
  resp_name <- model$resp_vars$resp_cats
  # Get the names for each columns
  col_name <- colnames(data)

  # Check if the response variables are legal or not.
  if (sum(grepl("[[:punct:]]|\\s", resp_name)) > 0) {
    stop("Space and punctuation are not allowed in the response variable names.")
  }

  # Check if the response variables are all present in the data
  missing_list <- setdiff(resp_name, intersect(resp_name, col_name))
  if (length(missing_list) > 0) {
    stop(paste0(
      "The response variable(s) '",
      paste0(missing_list,collapse="', '"),
      "' is not present in the data."))
  }

  # Transfer all of the response variables to a matrix and name it 'Y'
  data$Y <- as.matrix(data[,resp_name])
  data <- dplyr::select(data, -all_of(resp_name))

  # Get the vector of the options variables
  nOpt_vect <- model$other_vars$num_options

  # Check whether the option variables have the same length as the response variables.
  if (length(nOpt_vect) != length(resp_name)) {
    stop("The option variables should have the same length as the response variables.")
  }

  # If the number of options is a string, then it is the name of the column in the data
  if (is.character(nOpt_vect)) {
    option_name <- nOpt_vect

    # Check if the name of the number of options is legal or not.
    if (sum(grepl("[[:punct:]]|\\s", option_name)) > 0) {
      stop("Space and punctuation are not allowed in the number of options variable name.")
    }

    # Check if the number of options is present in the data
    missing_list <- setdiff(option_name, intersect(option_name, col_name))
    if (length(missing_list) > 0) {
      stop(paste0(
        "The variable(s) '",
        paste0(missing_list,collapse="', '"),
        "' is not present in the data."))
    }
    # If the number of options is a numeric vector,
    # then it represents the number of options for each response variable in all conditions.
    } else if (is.numeric(nOpt_vect)) {

      nOpt_name <- paste0("nOpt",resp_name)

      nOpt_data <- data.frame(nOpt_name, nOpt_vect) %>%
        tidyr::pivot_wider(names_from = nOpt_name, values_from = nOpt_vect)

      # Add the number of options to the data
      data <- dplyr::cross_join(data, nOpt_data)

    } else {
      stop("The number of options should be a string or a numeric vector.")
    }

  NextMethod("check_data")
}




#############################################################################!
# HELPER FUNCTIONS                                                       ####
#############################################################################!

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
calc_error_relative_to_nontargets <- function(data, response, nt_features) {
  y <- y_nt <- non_target_name <- non_target_value <- NULL
  data <- data %>%
    tidyr::gather(non_target_name, non_target_value, eval(nt_features))

  data$y_nt <- wrap(data[[response]]-data[["non_target_value"]])
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
#' diff <- x-y
#' hist(diff)
#' wrapped_diff <- wrap(x-y)
#' hist(wrapped_diff)
#'
wrap <- function(x, radians=TRUE) {
  stopifnot(is.logical(radians))
  if (radians) {
    return(((x+pi) %% (2*pi)) - pi)
  }
  ((x+180) %% (2*180)) - 180
}

#' @title Convert degrees to radians or radians to degrees.
#' @description
#'   The helper functions `deg2rad` and `rad2deg` should add convenience in transforming
#'   data from degrees to radians and from radians to degrees.
#'
#' @name circle_transform
#' @param deg A numeric vector of values in degrees.
#' @param rad A numeric vector of values in radians.
#' @return A numeric vector of the same length as `deg` or `rad`.
#' @keywords transform
#' @export
#' @examples
#'   degrees <- runif(100, min = 0, max = 360)
#'   radians <- deg2rad(degrees)
#'   degrees_again <- rad2deg(radians)
deg2rad <- function(deg){
  deg * pi / 180
}

#' @rdname circle_transform
#' @export
rad2deg <- function(rad){
  rad * 180 / pi
}


#' @title Generate data for `bmm` models to be passed to Stan
#' @description A wrapper around `brms::make_standata()` for models specified
#'   with `bmm`. Given the `model`, the `data` and the `formula` for the model,
#'   this function will return the combined stan data generated by `bmm` and
#'   `brms`
#'
#'   If you are using *brms* version 2.20.14 or later, you can use
#'   [make_standata()] directly with the `bmmformula` object. If you are using
#'   an older version of *brms*, you have to use [get_standata()] to obtain the
#'   default priors. The usage of [get_standata()] is deprecated and will be
#'   removed in future versions of *bmm*.
#'
#' @inheritParams fit_model
#' @param ... Further arguments passed to [brms::make_standata()]. See the
#'   description of [brms::make_standata()] for more details
#'
#' @returns A named list of objects containing the required data to fit a bmm
#'   model with Stan.
#'
#'
#' @seealso [supported_models()], [brms::make_standata()]
#'
#' @export
#'
#' @keywords extract_info
#'
#' @examples
#' \dontrun{
#' # generate artificial data from the Signal Discrimination Model
#' dat <- data.frame(y=rsdm(n=2000))
#'
#' # define formula
#' ff <- bmf(c ~ 1,
#'           kappa ~ 1)
#'
#'
#' if (utils::packageVersion('brms') >= "2.20.14") {
#'   # generate the stan data (if using brms version 2.20.14 or later)
#'   make_standata(formula = ff,
#'                 data = dat,
#'                 model = sdmSimple(resp_err = "y"))
#' } else {
#'   # generate the stan data (if using an older version of brms)
#'   get_standata(formula = ff,
#'                data = dat,
#'                model = sdmSimple(resp_err = "y"))
#' }
#' }
#'
get_standata <- function(formula, data, model, prior=NULL, ...) {
  if (utils::packageVersion('brms') >= "2.20.14") {
    message("get_standata is deprecated. Please use make_standata() instead.")
    return(brms::make_standata(formula = formula, data = data,
                               model = model, prior = prior, ...))
  }
  make_standata.bmmformula(formula = formula, data = data,
                           model = model, prior = prior, ...)
}

#' @rdname get_standata
make_standata.bmmformula <- function(formula, data, model, prior = NULL, ...) {
  # check model, formula and data, and transform data if necessary
  model <- check_model(model, data)
  data <- check_data(model, data, formula)
  formula <- check_formula(model, data, formula)

  # generate the model specification to pass to brms later
  config_args <- configure_model(model, data, formula)

  # combine the default prior plus user given prior
  config_args$prior <- combine_prior(config_args$prior, prior)

  # extract stan code
  dots <- list(...)
  fit_args <- c(config_args, dots)
  brms::do_call(brms::make_standata, fit_args)
}


# check if the data is sorted by the predictors
is_data_ordered <- function(data, formula) {
  dpars <- names(formula)
  predictors <- rhs_vars(formula)
  predictors <- predictors[not_in(predictors, dpars)]
  predictors <- predictors[predictors %in% colnames(data)]
  data <- data[,predictors]
  if (length(predictors) > 1) {
    gr_idx <- do.call(paste, c(data, list(sep="_")))
  } else {
    gr_idx <- unlist(data)
  }
  is_ordered <- !has_nonconsecutive_duplicates(gr_idx)
  is_ordered
}

# checks if all repetitions of a given value are consecutive in a vector
# by iterating over unique values and checking if all their positions are consecutive
has_nonconsecutive_duplicates <- function(vec) {
  unique_vals <- unique(vec)
  cond <- TRUE
  for(val in unique_vals) {
    positions <- which(vec == val)
    cond <- cond & all(diff(positions) == 1)
  }
  !cond
}




