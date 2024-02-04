postprocess_brm <- function(model, fit) {
  UseMethod('postprocess_brm')
}

#' @export
postprocess_brm.bmmmodel <- function(model, fit) {
  fit <- NextMethod('postprocess_brm')
  return(fit)
}

#' @export
postprocess_brm.vwm <- function(model, fit) {
  fit <- NextMethod('postprocess_brm')
  return(fit)
}

#' @export
postprocess_brm.default <- function(model, fit) {
  return(fit)
}
