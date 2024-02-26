#' checks if the formula is valid for the specified model
#' @param model a model list object returned from check_model()
#' @param data user supplied data
#' @param formula user supplied formula
#' @return the formula object
#' @keywords internal, developer
check_formula <- function(model, data, formula) {
  UseMethod('check_formula')
}

#' @export
check_formula.bmmmodel <- function(model, data, formula) {
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
  NextMethod("check_formula")
}

#' @export
check_formula.default <- function(model, data, formula) {
  return(formula)
}

#' @export
check_formula.nontargets <- function(model, data, formula) {
  setsize_var <- model$other_vars$setsize
  dpars <- names(formula)
  for (dpar in dpars) {
    dpar_pred <- rhs_vars(formula[[dpar]])
    if (setsize_var %in% dpar_pred) {
      ss_form <- formula[[dpar]]
      if (has_intercept(ss_form)) {
        stop2("The formula for parameter ", dpar, " contains an intercept and also uses setsize as a predictor.",
             " This model requires that the intercept is supressed when setsize is used as predictor.")
      }
    }
  }
  NextMethod("check_formula")
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
  all_pars <- unique(c(model_pars,formula_pars))
  formula[all_pars] # reorder formula to match model parameters order
}


wrong_parameters <- function(model, formula) {
  fpars <- names(formula)
  mpars <- names(model$info$parameters)
  rhs_vars <- rhs_vars(formula)

  if ("M3custom" %in% class(model)) {
    wpars <- not_in(fpars, mpars) & not_in(fpars, rhs_vars) & not_in(fpars, model$resp_vars$resp_cats)
  } else {
    wpars <- not_in(fpars, mpars) & not_in(fpars, rhs_vars)
  }
  fpars[wpars]
}

#' @title Convert `bmmformula` objects to `brmsformula` objects
#' @description
#'  Called by configure_model() inside fit_model() to convert the `bmmformula` into a
#'  `brmsformula` based on information in the model object. It will call the
#'  appropriate bmf2bf.\* methods based on the classes defined in the model_\* function.
#' @param model The model object defining one of the supported `bmmmodels``
#' @param formula The `bmmformula` that should be converted to a `brmsformula`
#' @returns A `brmsformula` defining the response variables and the additional parameter
#'   formulas for the specified `bmmmodel`
#' @keywords internal, developer
#' @examples
#'   model <- mixture2p(resp_err = "error")
#'
#'   formula <- bmmformula(
#'     thetat ~ 0 + setsize + (0 + setsize | id),
#'     kappa ~ 1 + (1 | id)
#'   )
#'
#'   brms_formula <- bmf2bf(model, formula)
#' @export
bmf2bf <- function(model, formula) {
  UseMethod("bmf2bf")
}

# default method for all bmmmodels with 1 response variable
#' @export
bmf2bf.bmmmodel <- function(model, formula) {
  # check if the model has only one response variable and extract if TRUE
  resp <- model$resp_vars
  if (length(resp) > 1 | "M3" %in% class(model)) {
    brms_formula <- NextMethod("bmf2bf")
  } else {
    resp <- resp[[1]]

    # set base brms formula based on response
    brms_formula <- brms::bf(paste0(resp, "~ 1"))
  }

  # for each dependent parameter, check if it is used as a non-linear predictor of
  # another parameter and add the corresponding brms function
  dpars <- names(formula)
  for (dpar in dpars[!dpars %in% model$resp_vars$resp_cats]) {
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


has_intercept <- function(formula) {
  if (is.null(formula)) {
    return(FALSE)
  } else if (!is.formula(formula)) {
    stop("The formula must be a formula object.")
  }
  as.logical(attr(stats::terms(formula), "intercept"))
}
