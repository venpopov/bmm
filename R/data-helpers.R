# generic S3 method for checking data based on model type
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
  if(not_in_list("non_targets", dots)) {
    stop("Argument 'non_targets' must be specified.")
  }

  non_targets <- dots$non_targets
  if (max(abs(data[,non_targets]), na.rm=T) > 10) {
    data[,non_targets] <- data[,non_targets]*pi/180
    warning('It appears your lure variables are in degrees. We will transform it to radians.')
  }

  if(not_in_list("setsize", dots)) {
    stop(paste0("Argument 'setsize' is not specified. For the ",
                attr(model, "name"),
                ", please set the 'setsize' argument either to a number, if the setsize ",
                "is fixed, or to the name of the variable containing the setsize, ",
                "if the setsize varies in your dataset"))
  }
  setsize <- dots$setsize
  if (is.character(setsize)) {
    # Variable setsize
    ss_numeric <- as.numeric(as.character(data[[setsize]]))
    max_setsize <- max(ss_numeric)
  } else if (is.numeric(setsize)) {
    # Fixed setsize
    ss_numeric <- rep(setsize, times = nrow(data))
    max_setsize <- setsize
  } else {
    stop("Argument 'setsize' must be either a numeric value or a character string.")
  }

  if (length(non_targets) < max_setsize - 1) {
    stop(paste0('The number of columns for non-target values in the argument',
                '`non_targets` is less than max(setsize)-1'))
  } else if (length(non_targets) > max_setsize - 1) {
    stop(paste0('The number of columns for non-target values in the argument',
                '`non_targets` is more than max(setsize)-1'))
  }

  # wrap lure variables around the circle (range = -pi to pi)
  data[,non_targets] <- wrap(data[,non_targets])

  # create index variables for non_targets and correction variable for theta due to setsize
  lure_idx_vars <- paste0('LureIdx',1:(max_setsize - 1))
  for (i in 1:(max_setsize - 1)) {
    data[[lure_idx_vars[i]]] <- ifelse(ss_numeric >= (i + 1), 1, 0)
  }
  data$inv_ss = 1/(ss_numeric - 1)
  data$inv_ss = ifelse(is.infinite(data$inv_ss), 1, data$inv_ss)
  data[,non_targets][is.na(data[,non_targets])] <- 0

  # save some variables as attributes of the data for later use
  attr(data, "max_setsize") <- max_setsize
  attr(data, "non_targets") <- non_targets
  attr(data, "lure_idx_vars") <- lure_idx_vars

  data = NextMethod("check_data")

  return(data)
}


#' @export
check_data.IMMspatial <- function(model, data, formula, ...) {
  dots <- list(...)
  if(not_in_list("spaPos", dots)) {
    stop("Argument 'spaPos' must be specified.")
  }

  if (length(spaPos) < attr(data, 'max_setsize') - 1) {
    stop(paste0('The number of columns for spatial positions in the argument',
                '`spaPos` is less than max(setsize)-1'))
  } else if (length(spaPos) > attr(data, 'max_setsize')-1) {
    stop(paste0('The number of columns for spatial positions in the argument',
                '`spaPos` is more than max(setsize)-1'))
  }

  spaPos <- dots$spaPos
  if (max(abs(data[,spaPos]), na.rm=T) > 10) {
    data[,spaPos] <- data[,spaPos]*pi/180
    warning('It appears your spatial position variables are in degrees. We will transform it to radians.')
  }
  # wrap spatial position variables around the circle (range = -pi to pi)
  data[,spaPos] <- bmm::wrap(data[,spaPos])

  data = NextMethod("check_data")

  return(data)
}


# ff <- brms::bf(y~x)
# m <- .model_2p()
# check_data(m, data = data.frame(x = 1:12, y = 1:12), formula=ff)
#
# check_data(m, data = data.frame(x = 1:12, y = 1:12), formula=ff)
#
# m2 <- .model_3p()
# check_data(m2, data = data.frame(x = 1:12, y = 1:12), formula=ff, non_targets = c("x"))

#
# m2 <- .model_IMMfull()
# check_data(m2, data = data.frame(x = 1:12, y = 1:12, t = sqrt(1:12), s = 1:12),
#            formula=ff, non_targets = c("x"),
#            spaPos = c("s"))
