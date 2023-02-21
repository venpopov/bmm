#' Extracts the name of the response variable in a formula
#'
#' @param formula an object of type bfformula
#'
#'
#' @return String. Name of the response variable
get_response <- function(formula) {
  tt <- stats::terms(formula)
  vars <- as.character(attr(tt, "variables"))[-1] ## [1] is the list call
  response <- attr(tt, "response") # index of response var
  vars[response]
}
