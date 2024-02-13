#' checks if the formula is valid for the specified model
#' @param model a model list object returned from check_model()
#' @param formula user supplied formula
#' @return the formula object
#' @keywords internal, developer
check_formula <- function(model, formula) {
  # Pre-Check: was a valid brms formula provided
  if (inherits(formula, 'brmsformula')) {
    stop("The provided formula is a brms formula.
        Please specify formula with the bmmformula() function instead of
        the brmsformula() or bf() function.
        E.g.: bmmformula(kappa ~ 1, thetat ~ 1")
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
#' @description This function is used to specify the formulas predicting the
#' different parameters of a `bmmmodel`.
#'
#' @details # General formula structure
#'
#' The formula argument accepts formulas of the following syntax:
#'
#'   ``` r
#'   parameter ~ fixed_effects + (random_effects | grouping_variable)
#'   ```
#'
#' `bmm` formulas are built on `brms` formulas and function in nearly the same
#' way, so you can use most of the `brms` formula syntax. The main differences
#' is that in `bmm` formulas, the response variable is not specified in the
#' formula. Instead, each parameter of the model is explicitly specified as the
#' left-hand side of the formula. In `brms`, the response variable is always
#' specified as the left-hand side of the first formula, which implicitly means
#' that any predictors in the first formula are predictors of the `mu` parameter
#' of the model. In general, measurement models do not all have a `mu`
#' parameter, therefore it is more straigthforward to explicitely predict each
#' parameter of the model.
#'
#' For example, in the following `brms` formula for the drift diffusion model,
#' the first line corresponds to the drift rate parameter, but this is not
#' explicitely stated.
#'
#'
#'           brmsformula(rt | dec(response) ~ condition + (condition | id),
#'                       bs ~ 1 + (1 | id),
#'                       ndt ~ 1 + (1 | id),
#'                       bias ~ 1 + (1 | id))
#'
#'
#'
#'
#' In `bmm`, the same formula would be written as:
#'
#'           bmmformula(drift ~ condition + (condition | id),
#'                      bs ~ 1 + (1 | id),
#'                      ndt ~ 1 + (1 | id),
#'                      bias ~ 1 + (1 | id))
#'
#' and the rt and response variables would be specified in the model argument of
#' the `fit_model` function.
#'
#' Aside from that, the `bmm` formula syntax is the same as the `brms` formula
#' syntax. For more information on the `brms` formula syntax, see
#' [brms::brmsformula()].
#'
#'
#' @param formula Formula for predicting a `bmmmodel` parameter.
#' @param ... Additional formulas for more than a single model parameter.
#'
#' @return A list of formulas for each parameters being predicted
#' @export
#' @examples
#' imm_formula <- bmmformula(
#'   c ~ 0 + setsize + (0 + setsize | id),
#'   a ~ 1,
#'   kappa ~ 0 + setsize + (0 + setsize | id)
#' )
#'
#' imm_formula
#'
bmmformula <- function(formula, ...){
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
