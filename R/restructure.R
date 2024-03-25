#' Restructure Old \code{bmmfit} Objects
#'
#' Restructure old \code{bmmfit} objects to work with
#' the latest \pkg{bmm} version. This function is called
#' internally when applying post-processing methods.
#'
#' @param x An object of class \code{bmmfit}.
#' @param ... Currently ignored.
#'
#' @return A \code{bmmfit} object compatible with the latest version
#'   of \pkg{bmm} and \pkg{brms}.
#' @keywords transform
#' @export
#' @importFrom utils packageVersion
#' @importFrom brms restructure
restructure.bmmfit <- function(x, ...) {
  version <- x$version$bmm
  if (is.null(version)) {
    version <- as.package_version('0.2.1')
    x$version$bmm <- version
  }
  if (!inherits(x, 'bmmfit')) {
    class(x) <- c('bmmfit', class(x))
  }
  current_version <- packageVersion('bmm')
  restr_version <- restructure_version.bmm(x)

  if (restr_version >= current_version) {
    x <- NextMethod('restructure')
    return(x)
  }

  if (restr_version < "0.3.0") {
    class(x) <- c("bmmfit", class(x))
    x <- add_bmm_info(x)
  }

  if (restr_version < "0.4.3") {
    info <- x$bmm$model$info
    x$bmm$model$info <- NULL
    x$bmm$model[names(info)] <- info
    x <- add_links(x)
    x$bmm$user_formula <- assign_nl(x$bmm$user_formula)
  }

  if (restr_version < "0.4.4") {
    x$bmm$fit_args <- NULL
  }

  if (restr_version < "0.4.5") {
    prev_model <- x$bmm$model
    prev_model_name <- class(prev_model)[length(class(prev_model))]
    new_model <- get_model(prev_model_name)
    new_model <- brms::do_call(new_model, c(prev_model$resp_vars, prev_model$other_vars))
    x$bmm$model <- check_model(new_model, x$data, x$bmm$user_formula)
  }

  if (restr_version < "0.4.8") {
    cl <- class(x$bmm$model)
    cl[1] <- "bmmodel"
    class(x$bmm$model) <- cl
  }

  x$version$bmm_restructure <- current_version
  NextMethod('restructure')
}

restructure_version.bmm <- function(x) {
  out <- x$version$bmm_restructure
  if (!is.package_version(out)) {
    out <- x$version$bmm
  }
  out
}

add_links <- function(x) {
  UseMethod("add_links")
}

#' @export
add_links.bmmfit <- function(x) {
  x$bmm$model <- add_links(x$bmm$model)
  x
}

#' @export
add_links.bmmodel <- function(x) {
  model_name <- class(x)[length(class(x))]
  new_model <- get_model(model_name)()
  x$links <- new_model$links
  x
}

add_bmm_info <- function(x) {
  env <- x$family$env
  if (is.null(env)) {
    stop2("Unable to restructure the object for use with the latest version \\
           of bmm. Please refit.")
  }
  pforms <- env$formula$pforms
  names(pforms) <- NULL
  user_formula <- brms::do_call("bmf", pforms)
  model = env$model
  model$resp_vars <- list(resp_error = env$formula$resp)
  model$other_vars <- list()
  if (inherits(model, 'sdmSimple')) {
    model$info$parameters$mu <- glue('Location parameter of the SDM distribution \\
                                     (in radians; by default fixed internally to 0)')
  } else {
    model$info$parameters$mu1 = glue(
      "Location parameter of the von Mises distribution for memory responses \\
       (in radians). Fixed internally to 0 by default."
    )
  }

  x$bmm <- nlist(model, user_formula)
  x
}
