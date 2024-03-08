#' Restructure Old \code{bmmfit} Objects
#'
#' Restructure old \code{bmmfit} objects to work with
#' the latest \pkg{bmm} version. This function is called
#' internally when applying post-processing methods.
#' However, in order to avoid unnecessary run time caused
#' by the restructuring, I recommend explicitly calling
#' \code{restructure} once per model after updating \pkg{bmm}.
#'
#' @param x An object of class \code{bmmfit}.
#' @param ... Currently ignored.
#'
#' @return A \code{bmmfit} object compatible with the latest version
#'   of \pkg{bmm} and \pkg{brms}.
#' @keywords transform
#' @export
#' @importFrom utils packageVersion
restructure.bmmfit <- function(x, ...) {
  version <- x$version$bmm
  if (is.null(version)) {
    version <- as.package_version('0.1.1')
  }
  current_version <- utils::packageVersion('bmm')
  restr_version <- restructure_version.bmm(x)

  if (restr_version >= current_version) {
    if (packageVersion("brms") >= "2.20.15") {
      x <- NextMethod('restructure')
    } else {
      brms::restructure(x)
    }
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

  x$version$bmm_restructure <- current_version
  if (packageVersion("brms") >= "2.20.15") {
    x <- NextMethod('restructure')
  } else {
    brms::restructure(x)
  }
  x
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
add_links.bmmmodel <- function(x) {
  model_name <- class(x)[length(class(x))]
  new_model <- get_model(model_name)()
  x$links <- new_model$links
  x
}

add_bmm_info <- function(x) {
  # TODO:
  x
}
