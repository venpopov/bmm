restructure.bmm <- function(x) {
  assert_that(is_bmmfit(x), msg = "Please provide a bmmfit object")
  version <- x$version$bmm
  if (is.null(version)) {
    version <- as.package_version('0.1.1')
  }
  current_version <- utils::packageVersion('bmm')
  restr_version <- restructure_version.bmm(x)

  if (restr_version >= current_version) {
    return(x)
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
  model_name <- class(x$bmm$model)[length(class(x$bmm$model))]
  new_model <- get_model(model_name)()
  x$bmm$model$info$links <- new_model$info$links
  x
}


