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


## TODO: improve consistency of argument names by using only dots <- list()
# and then get variable names from there

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
  if(not_in_list("non_targets", dots)) {
    stop("Argument 'non_targets' must be specified.")
  }
  non_targets <- dots$non_targets
  if (max(abs(data[,non_targets]), na.rm=T) > 10) {
    data[,non_targets] <- data[,non_targets]*pi/180
    warning('It appears your lure variables are in degrees. We will transform it to radians.')
  }
  # wrap lure variables around the circle (range = -pi to pi)
  data[,non_targets] <- bmm::wrap(data[,non_targets])

  data = NextMethod("check_data")

  return(data)
}

#' @export
check_data.IMMspatial <- function(model, data, formula, ...) {
  dots <- list(...)
  if(not_in_list("spaPos", dots)) {
    stop("Argument 'spaPos' must be specified.")
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
#
# m2 <- .model_IMMfull()
# check_data(m2, data = data.frame(x = 1:12, y = 1:12, t = sqrt(1:12), s = 1:12),
#            formula=ff, non_targets = c("x"),
#            spaPos = c("s"))
