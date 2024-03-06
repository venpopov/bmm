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
    add_bmm_info(x)
  }

  if (restr_version < "0.4.3") {
    x <- add_links(x)
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
add_links.brmsfit <- function(x) {
  x$bmm$model <- add_links(x$bmm$model)
  x
}

#' @export
add_links.bmmmodel <- function(x) {
  model_name <- class(x)[length(class(x))]
  new_model <- get_model(model_name)()
  x$info$links <- new_model$info$links
  x
}

add_bmm_info <- function(x) {
  # TODO:

}
