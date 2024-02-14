#' checks if the formula is valid for the specified model
#' @param model a model list object returned from check_model()
#' @param formula user supplied formula
#' @return the formula object
#' @keywords internal, developer
check_formula <- function(model, formula) {
  # Pre-Check: was a valid bmm formula provided
  if (!is.bmmformula(formula)) {
    if (is.brmsformula(formula)) {
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
  fixed_pars <- names(model$info$fixed_parameters)
  missing_pars <- setdiff(model_pars,formula_pars)
  is_fixed <- missing_pars %in% fixed_pars
  names(is_fixed) <- missing_pars
  for (mpar in missing_pars) {
    formula <- formula + stats::as.formula(paste(mpar,"~ 1"))
    if (!is_fixed[mpar]) {
      message2(paste("No formula for parameter",mpar,"provided","\n",
                     "For this parameter only a fixed intercept will be estimated."))
    }
  }
  formula <- formula[model_pars] # reorder formula to match model parameters order
  return(formula)
}


wrong_parameters <- function(model, formula) {
  fpars <- names(formula)
  mpars <- names(model$info$parameters)
  rhs_vars <- rhs_vars(formula)
  wpars <- not_in(fpars, mpars) & not_in(fpars, rhs_vars)
  fpars[wpars]
}


bmf2bf <- function(model, formula) {
  UseMethod("bmf2bf")
}

# default method for all bmmmodels with 1 response variable
# TODO: add support for multiple response variables
#' @export
bmf2bf.bmmmodel <- function(model, formula) {
  # check if the model has only one response variable and extract if TRUE
  resp <- model$resp_vars
  if (length(resp) > 1) {
    NextMethod("bmf2bf")
  }
  resp <- resp[[1]]

  # set base brms formula based on response
  brms_formula <- brms::bf(paste0(resp, "~ 1"))

  # for each dependent parameter, check if it is used as a non-linear predictor of
  # another parameter and add the corresponding brms function
  dpars <- names(formula)
  for (dpar in dpars) {
    pform <- formula[[dpar]]
    predictors <- rhs_vars(pform)
    if (any(predictors %in% dpars)) {
      brms_formula <- brms_formula + brms::nlf(pform)
    } else {
      brms_formula <- brms_formula + brms::lf(pform)
    }
  }
  brms_formula
}





