#' checks if the formula is valid for the specified model
#' @param model a model list object returned from check_model()
#' @param formula user supplied formula
#' @return the formula object
#' @keywords internal, developer
check_formula <- function(model, formula) {
  # Pre-Check: was a valid brms formula provided
  if (!inherits(formula, 'brmsformula')) {
    stop("The provided formula is not a brms formula.
        Please specify formula with the bf() function.
        E.g.: bf(y ~ 1, kappa ~ 1, thetat ~ 1")
  }

  # Check: is the formula valid for the specified model type
  ## TODO: additional checks for formula terms needed for each model type

  return(formula)
}



#' Extracts the name of the response variable in a formula
#'
#' to use this with brmsformula objects, you have to pass the internal formula object
#' from the brmsformula object. E.g. formula$formula
#' @param formula an object of class formula
#' @noRd
#' @return String. Name of the response variable
get_response <- function(formula) {
  tt <- stats::terms(formula)
  vars <- as.character(attr(tt, "variables"))[-1] ## [1] is the list call
  response <- attr(tt, "response") # index of response var
  vars[response]
}
