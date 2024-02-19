#' @title Generic S3 method for postprocessing the fitted brm model
#' @description Called by fit_model() to automatically perform some type of postprocessing
#'   depending on the model type. It will call the appropriate postprocess_brm.*
#'   methods based on the list of classes defined in the .model_* functions. For
#'   models with several classes listed, it will call the functions in the order
#'   they are listed. Thus, any operations that are common to a group of models
#'   should be defined in the appropriate postprocess_brm.* function, where \*
#'   corresponds to the shared class. For example, for the sdmSimple model, the
#'   postprocessing involves setting the link function for the c parameter to "log",
#'   because it was coded manually in the stan code, but it was specified as "identity"
#'   in the brms custom family. If your model requires no postprocessing, you can
#'   skip this method, and the default method will be used (which returns the same
#'   brmsfit object that was passed to it).
#' @param model A model list object returned from check_model()
#' @param fit the fitted brm model returned by `call_brm()`
#' @return An object of class brmsfit, with any necessary postprocessing applied
#' @export
#' @keywords internal, developer
postprocess_brm <- function(model, fit) {
  UseMethod('postprocess_brm')
}

#' @export
postprocess_brm.bmmmodel <- function(model, fit) {
  # save bmm version
  fit$version$bmm <- utils::packageVersion('bmm')
  NextMethod('postprocess_brm')
}

#' @export
postprocess_brm.vwm <- function(model, fit) {
  NextMethod('postprocess_brm')
}

#' @export
postprocess_brm.default <- function(model, fit) {
  fit
}
