#' checks if the formula is valid for the specified model
#' @param model a model list object returned from check_model()
#' @param formula user supplied formula
#' @return the formula object
#' @keywords internal, developer
check_formula <- function(model, formula) {
  # Pre-Check: was a valid brms formula provided
  if (inherits(formula, 'brmsformula')) {
    stop("The provided formula is a brms formula.
        Please specify formula with the bmm_formula() function instead of
        the brmsformula() or bf() function.
        E.g.: bmm_formula(kappa ~ 1, thetat ~ 1")
  }

  # Check: is the formula valid for the specified model type
  ## TODO: additional checks for formula terms needed for each model type
  par_names <- names(model$info$parameters)
  for (i in length(par_names)) {
    if (!par_names[i] %in% names(formula)) {
      warning(paste("No formula for parameter",par_names[i],"provided","\n",
                    "For this parameter only a fixed intercept will be estimated."))
      par_formula <- stats::as.formula(paste(par_names[i],"~ 1"))
      init_names <- names(formula)
      formula <- c(formula, par_formula)
      names(formula) <- c(init_names,par_names[i])
    }
  }

  return(formula)
}


#' Extracts the name of the response variable in a formula
#'
#' @param formula an object of type bfformula
#' @noRd
#' @return String. Name of the response variable
get_response <- function(formula) {
  tt <- stats::terms(formula)
  vars <- as.character(attr(tt, "variables"))[-1] ## [1] is the list call
  response <- attr(tt, "response") # index of response var
  vars[response]
}


#' @title Create formula for predicting parameters of a `bmmmodel`
#'
#' @description
#'   This function is used to specify the formulas predicting the different
#'   parameters of a `bmmmodel`.
#'
#' @param formula Formula for predicting a `bmmmodel` parameter.
#' @param ... Additional formulas for more than a single model parameter.
#'
#' @return A list of formulas for each parameters being predicted
#' @export
#' @examples
#' imm_formula <- bmm_formula(
#'   c ~ 0 + setsize + (0 + setsize | id),
#'   a ~ 1,
#'   kappa ~ 0 + setsize + (0 + setsize | id)
#' )
#'
#' imm_formula
#'
bmm_formula <- function(formula, ...){
  # paste formulas into a single list
  dots <- list(...)
  formula <- list(formula)
  formula <- c(formula, dots)

  # extract parameter names
  pform_names <- character()
  for (i in 1:length(formula)) {
    form_line <- formula[[i]]
    parm <- formula.tools::lhs(form_line)
    pform_names <- c(pform_names, as.character(parm))
  }

  # label the different formulas according to the parameter predicted
  names(formula) <- pform_names
  return(formula)
}
