#' checks if the formula is valid for the specified model
#' @param model a model list object returned from check_model()
#' @param formula user supplied formula
#' @return the formula object
#' @keywords internal, developer
check_formula <- function(model, formula) {
  # Pre-Check: was a valid bmm formula provided
  if (!inherits(formula, 'bmmformula')) {
    if (inherits(formula, 'brmsformula')) {
      stop("The provided formula is a brms formula.
        Please specify formula with the bmmformula() function instead of
        the brmsformula() or bf() function.
        E.g.: bmmformula(kappa ~ 1, thetat ~ 1) or bmf(kappa ~ 1, thetat ~ 1)")
    } else {
      stop("The provided formula is not a valid bmm formula.
        Please specify formula with the bmmformula() function.
        E.g.: bmmformula(kappa ~ 1, thetat ~ 1) or bmf(kappa ~ 1, thetat ~ 1)")
    }
  }

  wpar <- wrong_parameters(model, formula)
  if (length(wpar) > 0) {
    stop("The formula contains parameters that are not part of the model: ",
         collapse_comma(wpar))
  }

  formula <- add_missing_parameters(model, formula)

  return(formula)
}


add_missing_parameters <- function(model, formula) {
  formula_pars <- names(formula)
  model_pars <- names(model$info$parameters)
  for (mpar in model_pars) {
    if (not_in(mpar, formula_pars)) {
      message(paste("No formula for parameter",mpar,"provided","\n",
                    "For this parameter only a fixed intercept will be estimated."))
      formula[[mpar]] <- stats::as.formula(paste(mpar,"~ 1"))
    }
  }
  formula <- formula[model_pars] # reorder formula to match model parameters
  class(formula) <- "bmmformula"
  return(formula)
}


wrong_parameters <- function(model, formula) {
  formula_pars <- names(formula)
  model_pars <- names(model$info$parameters)
  wpar <- not_in(formula_pars, model_pars)
  formula_pars[wpar]
}
