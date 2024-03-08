#' @importFrom assertthat assert_that
restructure.bmm <- function(x) {
  assert_that(is_bmmfit(x) | !is.null(x$version$bmm), msg = "Please provide a bmmfit object")
  version <- x$version$bmm
  if (is.null(version)) {
    version <- as.package_version('0.1.1')
  }
  current_version <- utils::packageVersion('bmm')
  restr_version <- restructure_version.bmm(x)

  if (restr_version >= current_version) {
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

  x$version$bmm_restructure <- current_version
  brms::restructure(x)
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
