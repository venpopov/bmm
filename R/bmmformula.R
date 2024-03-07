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
  names(formula) <- all.vars(formula[[1]])[1]
  class(formula) <- "bmmformula"
  for (f in dots) {
    formula <- formula + f
  }
  # assign attribute nl TRUE/FALSE to each component of the formula
  assign_nl(formula)
}


# alias of bmmformula
#' @export
bmf <- function(formula, ...) {
  bmmformula(formula, ...)
}


# method for adding formulas to a bmmformula
#' @export
"+.bmmformula" <- function(f1,f2) {
  if (!is_bmmformula(f1)) {
    stop("The first argument must be a bmmformula.")
  }
  if (is_formula(f2)) {
    par2 <- all.vars(f2)[1]
    if (par2 %in% names(f1)) {
      message2(paste("The parameter", par2, "is already part of the formula.",
                    "Overwriting the initial formula."))
    }
    f1[[par2]] <- f2
  } else if (is_bmmformula(f2)) {
    for (par2 in names(f2)) {
      if (par2 %in% names(f1)) {
        message2(paste("The parameter", par2, "is already part of the formula.",
                      "Overwriting the initial formula."))
      }
      f1[[par2]] <- f2[[par2]]
    }
  } else if(!is.null(f2)) {
    stop("The second argument must be a formula or a bmmformula.")
  }
  # reassign attribute nl to each component of the formula
  assign_nl(f1)
}


# method for subsetting a bmmformula, ensuring the attributes are preserved
#' @export
'[.bmmformula' <- function(formula, pars) {
  attrs <- attributes(formula)
  out <- unclass(formula)
  out <- out[pars]
  attributes(out) <- attrs
  names(out) <- pars
  # reassign attribute nl to each component of the formula
  assign_nl(out)
}

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
  if (!is_bmmformula(formula)) {
    if (is_brmsformula(formula)) {
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
  if (length(resp) > 1) {
    formula <- NextMethod("bmf2bf")
    return(formula)
  }
  resp <- resp[[1]]

  # set base brms formula based on response
  brms_formula <- brms::bf(paste0(resp, "~ 1"))

  # for each dependent parameter, check if it is used as a non-linear predictor of
  # another parameter and add the corresponding brms function
  for (pform in formula) {
    if (is_nl(pform)) {
      brms_formula <- brms_formula + brms::nlf(pform)
    } else {
      brms_formula <- brms_formula + brms::lf(pform)
    }
  }
  brms_formula
}



add_missing_parameters <- function(model, formula) {
  formula_pars <- names(formula)
  model_pars <- names(model$parameters)
  fixed_pars <- names(model$fixed_parameters)
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
  mpars <- names(model$parameters)
  rhs_vars <- rhs_vars(formula)
  wpars <- not_in(fpars, mpars) & not_in(fpars, rhs_vars)
  fpars[wpars]
}

has_intercept <- function(formula) {
  if (is.null(formula)) {
    return(FALSE)
  } else if (!is_formula(formula)) {
    stop("The formula must be a formula object.")
  }
  as.logical(attr(stats::terms(formula), "intercept"))
}

rhs_vars <- function(formula, ...) {
  UseMethod("rhs_vars")
}

# @param formula a bmmformula object
# @param collapse Logical. Should it return a single vector with all the variables
#  or a list with the variables for each parameter?
#' @export
rhs_vars.bmmformula <- function(formula, collapse = TRUE, ...) {
  lhs_vars <- names(formula)
  rhs_vars <- list()
  for (var in lhs_vars) {
    rhs_vars[[var]] <- rhs_vars(formula[[var]])
  }
  if (!collapse) {
    return(rhs_vars)
  }
  out <- unlist(rhs_vars, use.names=F)
  unique(out)
}

#' @export
rhs_vars.formula <- function(formula, ...) {
  all_vars <- all.vars(formula)
  all_vars[-1]
}


# adds an attribute nl to each component of the the formula indicating if the
# any of the predictors of the component are also predicted in another component
assign_nl <- function(formula) {
  dpars <- names(formula)
  preds <- rhs_vars(formula, collapse = FALSE)
  for (dpar in dpars) {
    if (any(preds[[dpar]] %in% dpars)) {
      attr(formula[[dpar]], "nl") <- TRUE
    } else {
      attr(formula[[dpar]], "nl") <- FALSE
    }
  }
  formula
}

is_nl <- function(object, ...) {
  UseMethod("is_nl")
}

#' @export
is_nl.bmmformula <- function(object, ...) {
  unlist(sapply(object, is_nl))
}

#' @export
is_nl.formula <- function(object, ...) {
  attr(object, "nl")
}

is_formula <- function(x) {
  inherits(x, "formula")
}

is_bmmformula <- function(x) {
  inherits(x, "bmmformula")
}

is_brmsformula <- function(x) {
  inherits(x, "brmsformula")
}
