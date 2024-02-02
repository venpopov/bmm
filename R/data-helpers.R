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
#' @param ... Additional arguments passed to the check_data.* functions
#' @return A data.frame with the same number of rows as the input data, but with
#'   additional columns added as necessary, any necessary transformations
#'   applied, and attributes added to the data.frame for later use. If you need
#'   to reuse variables created by the check_data.* functions in subsequent
#'   stages (e.g. in configure_model()), you can store and access them using the
#'   attr() function.
#' @export
#' @keywords internal
check_data <- function(model, data, formula, ...) {
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
  resp_name <- get_response(formula$formula)
  if (not_in(resp_name, colnames(data))) {
    stop(paste0("The response variable '", resp_name, "' is not present in the data."))
  }

  UseMethod("check_data")
}



#' @export
check_data.default <- function(model, data, formula, ...) {
  return(data)
}


#' @export
check_data.vwm <- function(model, data, formula, ...) {
  resp_name <- get_response(formula$formula)
  if (max(abs(data[[resp_name]]), na.rm=T) > 10) {
    data[[resp_name]] <- data[[resp_name]]*pi/180
    warning('It appears your response variable is in degrees. We will transform it to radians.')
  }

  # wrap recoded responses around the circle (range = -pi to pi)
  data[[resp_name]] <- wrap(data[[resp_name]])

  data = NextMethod("check_data")

  return(data)
}


#' @export
check_data.nontargets <- function(model, data, formula, ...) {
  dots <- list(...)

  # check if arguments are valid
  if(is.null(dots$non_targets)) {
    stop("Argument 'non_targets' must be specified.")
  }

  non_targets <- dots$non_targets
  if (max(abs(data[,non_targets]), na.rm=T) > 10) {
    data[,non_targets] <- data[,non_targets]*pi/180
    warning('It appears your lure variables are in degrees. We will transform it to radians.')
  }

  if(is.null(dots$setsize)) {
    stop(paste0("Argument 'setsize' is not specified. For the ",
                attr(model, "name"),
                ",\n  please set the 'setsize' argument either to a number, if the setsize \n",
                "  is fixed, or to the name of the variable containing the setsize, \n",
                "  if the setsize varies in your dataset\n"))
  }
  setsize <- dots$setsize
  if (is.character(setsize) && length(setsize) == 1) {
    # Variable setsize
    ss_numeric <- as.numeric(as.character(data[[setsize]]))
    max_setsize <- max(ss_numeric)
  } else if (is.numeric(setsize) && length(setsize) == 1) {
    # Fixed setsize
    ss_numeric <- rep(setsize, times = nrow(data))
    max_setsize <- setsize
  } else {
    stop("Argument 'setsize' must be either a single numeric value or a character string.")
  }

  if (length(non_targets) < max_setsize - 1) {
    stop("The number of columns for non-target values in the argument ",
         "'non_targets' is less than max(setsize)-1")
  } else if (length(non_targets) > max_setsize - 1) {
    stop('The number of columns for non-target values in the argument ',
         '`non_targets` is more than max(setsize)-1')
  }

  # wrap lure variables around the circle (range = -pi to pi)
  data[,non_targets] <- wrap(data[,non_targets])

  # create index variables for non_targets and correction variable for theta due to setsize
  lure_idx_vars <- paste0('LureIdx',1:(max_setsize - 1))
  for (i in 1:(max_setsize - 1)) {
    data[[lure_idx_vars[i]]] <- ifelse(ss_numeric >= (i + 1), 1, 0)
  }
  data$ss_numeric <- ss_numeric
  data$inv_ss = 1/(ss_numeric - 1)
  data$inv_ss = ifelse(is.infinite(data$inv_ss), 1, data$inv_ss)
  data[,non_targets][is.na(data[,non_targets])] <- 0

  # save some variables as attributes of the data for later use
  attr(data, "max_setsize") <- max_setsize
  attr(data, "non_targets") <- non_targets
  attr(data, "lure_idx_vars") <- lure_idx_vars
  attr(data, "setsize_var") <- setsize

  data = NextMethod("check_data")

  return(data)
}


#' @export
check_data.IMMspatial <- function(model, data, formula, ...) {
  dots <- list(...)
  if(is.null(dots$spaPos)) {
    stop("Argument 'spaPos' must be specified.")
  }
  spaPos <- dots$spaPos

  if (length(spaPos) < attr(data, 'max_setsize') - 1) {
    stop(paste0("The number of columns for spatial positions in the argument",
                "'spaPos' is less than max(setsize)-1"))
  } else if (length(spaPos) > attr(data, 'max_setsize')-1) {
    stop(paste0("The number of columns for spatial positions in the argument",
                "spaPos'' is more than max(setsize)-1"))
  }

  spaPos <- dots$spaPos
  if (max(abs(data[,spaPos]), na.rm=T) > 10) {
    data[,spaPos] <- data[,spaPos]*pi/180
    warning('It appears your spatial position variables are in degrees. We will transform it to radians.')
  }
  # wrap spatial position variables around the circle (range = -pi to pi)
  data[,spaPos] <- bmm::wrap(data[,spaPos])

  # save some variables as attributes of the data for later use
  attr(data, "spaPos") <- spaPos

  data = NextMethod("check_data")

  return(data)
}
