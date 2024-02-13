#' @title Create formula for predicting parameters of a `bmmmodel`
#'
#' @description This function is used to specify the formulas predicting the
#' different parameters of a `bmmmodel`.
#'
#' @aliases bmf
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
#' You can also use the `bmf()` function as a shorthand for `bmmformula()`.
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
#' # or use the shorter alias 'bmf'
#' imm_formula2 <- bmf(
#'   c ~ 0 + setsize + (0 + setsize | id),
#'   a ~ 1,
#'   kappa ~ 0 + setsize + (0 + setsize | id)
#' )
#' identical(imm_formula, imm_formula2)
#'
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


# alias of bmmformula
#' @export
bmf <- function(formula, ...) {
  brmsformula(bmmformula(formula, ...))
}
