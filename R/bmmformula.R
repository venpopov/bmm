#' @title Create formula for predicting parameters of a `bmmodel`
#'
#' @description This function is used to specify the formulas predicting the
#' different parameters of a `bmmodel`.
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
#' the `bmm()` function.
#'
#' Aside from that, the `bmm` formula syntax is the same as the `brms` formula
#' syntax. For more information on the `brms` formula syntax, see
#' [brms::brmsformula()].
#'
#' You can also use the `bmf()` function as a shorthand for `bmmformula()`.
#'
#'
#' @param ... Formulas for predicting a `bmmodel` parameter. Each formula for a
#' parameter should be specified as a separate argument, separated by commas
#' @return A list of formulas for each parameters being predicted
#' @export
#' @examples
#' imm_formula <- bmmformula(
#'   c ~ 0 + set_size + (0 + set_size | id),
#'   a ~ 1,
#'   kappa ~ 0 + set_size + (0 + set_size | id)
#' )
#'
#' # or use the shorter alias 'bmf'
#' imm_formula2 <- bmf(
#'   c ~ 0 + set_size + (0 + set_size | id),
#'   a ~ 1,
#'   kappa ~ 0 + set_size + (0 + set_size | id)
#' )
#' identical(imm_formula, imm_formula2)
bmmformula <- function(...) {
  out <- list(...)
  is_form_or_num <- function(x) is_formula(x) || (is.numeric(x) && length(x) == 1)
  stopif(!all(sapply(out, is_form_or_num)), "Arguments must be formulas or numeric assignments.")

  par_names <- sapply(seq_along(out), function(i) {
    ifelse(is_formula(out[[i]]), all.vars(out[[i]])[1], names(out[i]))
  })

  duplicates <- duplicated(par_names)
  stopif(any(duplicates), "Duplicate formula for parameter(s) {par_names[duplicates]}")
  names(out) <- par_names
  class(out) <- "bmmformula"

  out <- assign_nl_attr(out)
  assign_constants(out)
}


# alias of bmmformula
#' @rdname bmmformula
#' @export
bmf <- function(...) {
  bmmformula(...)
}


# method for adding formulas to a bmmformula
#' @export
"+.bmmformula" <- function(f1, f2) {
  stopif(!is_bmmformula(f1), "The first argument must be a bmmformula.")
  f2_not_a_formula <- !(is_formula(f2) || is_bmmformula(f2)) && !is.null(f2)
  stopif(f2_not_a_formula, "The second argument must be a formula or a bmmformula.")

  if (is_formula(f2)) {
    dvar <- all.vars(f2)[1]
    f2 <- setNames(list(f2), dvar)
  }

  for (par in names(f2)) {
    if (par %in% names(f1)) {
      message2("Duplicate parameter: {par}. Overwriting the initial formula.")
    }
    f1[[par]] <- f2[[par]]
  }

  # we need to recompute which formulas are non-linear
  f1 <- assign_nl_attr(f1)
  assign_constants(f1)
}


# method for subsetting a bmmformula, ensuring the attributes are preserved
#' @export
`[.bmmformula` <- function(formula, pars) {
  attrs <- attributes(formula)
  attrs <- attrs[sapply(attrs, length) == 1]
  out <- unclass(formula)
  out <- out[pars]
  attributes(out) <- attrs
  if (is.character(pars)) {
    names(out) <- pars
  } else if (is.numeric(pars) | is.logical(pars)) {
    names(out) <- names(formula)[pars]
  }

  # reassign attribute nl to each component of the formula
  assign_nl_attr(out)
}

#' @export
`[<-.bmmformula` <- function(formula, pars, value) {
  if (!is.list(value)) {
    values <- list(value)
  }
  out <- unclass(formula)
  out[pars] <- value
  class(out) <- "bmmformula"
  assign_nl_attr(out)
}


#' Generic S3 method for checking if the formula is valid for the specified model
#' @param model a model list object returned from check_model()
#' @param data user supplied data
#' @param formula user supplied formula
#' @return the formula object
#' @keywords internal developer
check_formula <- function(model, data, formula) {
  UseMethod("check_formula")
}

#' @export
check_formula.bmmodel <- function(model, data, formula) {
  stopif(
    !is_bmmformula(formula),
    "The provided formula is not a bmm formula. Please use the bmf() function. E.g.:
    bmmformula(kappa ~ 1, thetat ~ 1) or bmf(kappa ~ 1, thetat ~ 1)"
  )

  wpar <- wrong_parameters(model, formula)
  stopif(length(wpar), "Unrecognized model parameters: {collapse_comma(wpar)}")

  formula <- add_missing_parameters(model, formula)
  NextMethod("check_formula")
}

#' @export
check_formula.default <- function(model, data, formula) {
  formula
}

#' @export
check_formula.non_targets <- function(model, data, formula) {
  set_size_var <- model$other_vars$set_size
  pred_list <- rhs_vars(formula, collapse = FALSE)
  has_set_size <- sapply(pred_list, function(x) set_size_var %in% x)
  ss_forms <- formula[has_set_size]
  intercepts <- sapply(ss_forms, has_intercept)
  stopif(
    any(intercepts),
    "The formula for parameter(s) {names(ss_forms)[intercepts]} contains \\
    an intercept and also uses set_size as a predictor. This model requires \\
    that the intercept is supressed when set_size is used as predictor. \\
    Try using 0 + {set_size_var} instead."
  )
  NextMethod("check_formula")
}

#' @title Convert `bmmformula` objects to `brmsformula` objects
#' @description
#'  Called by [configure_model()] inside [bmm()] to convert the `bmmformula` into a
#'  `brmsformula` based on information in the model object. It will call the
#'  appropriate bmf2bf.\* methods based on the classes defined in the model_\* function.
#' @param model The model object defining one of the supported `bmmodels``
#' @param formula The `bmmformula` that should be converted to a `brmsformula`
#' @return A `brmsformula` defining the response variables and the additional parameter
#'   formulas for the specified `bmmodel`
#' @keywords internal developer
#' @examples
#' model <- mixture2p(resp_error = "error")
#'
#' formula <- bmmformula(
#'   thetat ~ 0 + set_size + (0 + set_size | id),
#'   kappa ~ 1 + (1 | id)
#' )
#'
#' brms_formula <- bmf2bf(model, formula)
#' @export
bmf2bf <- function(model, formula) {
  UseMethod("bmf2bf")
}

# default method to paste the full brms formula for all bmmodels
#' @export
bmf2bf.bmmodel <- function(model, formula) {
  # check if the model has only one response variable and extract if TRUE
  brms_formula <- NextMethod("bmf2bf")

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

# paste first line of the brms formula for all bmmodels with 1 response variable
#' @export
bmf2bf.default <- function(model, formula) {
  # set base brms formula based on response
  brms::bf(paste0(model$resp_vars[[1]], "~ 1"))
}

add_missing_parameters <- function(model, formula, replace_fixed = TRUE) {
  formula_pars <- names(formula)
  model_pars <- names(model$parameters)
  fixed_pars <- names(model$fixed_parameters)
  # remove constants
  if (replace_fixed) {
    formula_pars <- formula_pars[!formula_pars %in% fixed_pars]
  }
  missing_pars <- setdiff(model_pars, formula_pars)
  is_fixed <- missing_pars %in% fixed_pars
  names(is_fixed) <- missing_pars
  for (mpar in missing_pars) {
    add <- stats::as.formula(paste(mpar, "~ 1"))
    if (is_fixed[mpar]) {
      attr(add, "constant") <- TRUE
    } else {
      message2("No formula for parameter {mpar} provided. Only a fixed intercept will be estimated.")
    }
    formula[mpar] <- list(add)
  }
  all_pars <- unique(c(model_pars, formula_pars))
  formula[all_pars] # reorder formula to match model parameters order
}

wrong_parameters <- function(model, formula) {
  fpars <- names(formula)
  mpars <- names(model$parameters)
  rhs_vars <- rhs_vars(formula)
  wpars <- not_in(fpars, mpars) & not_in(fpars, rhs_vars)
  fpars[wpars]
}

has_intercept <- function(object) {
  UseMethod("has_intercept")
}

#' @export
has_intercept.terms <- function(object) {
  as.logical(attr(object, "intercept"))
}

#' @export
has_intercept.formula <- function(object) {
  try_terms <- try(terms(object), silent = TRUE)
  if (is_try_error(try_terms)) {
    return(FALSE)
  }
  has_intercept(try_terms)
}

rhs_vars <- function(object, ...) {
  UseMethod("rhs_vars")
}

# @param formula a bmmformula object
# @param collapse Logical. Should it return a single vector with all the variables
#  or a list with the variables for each parameter?
#' @export
rhs_vars.bmmformula <- function(object, collapse = TRUE, ...) {
  rhs_vars <- lapply(object, function(x) rhs_vars(x))
  if (!collapse) {
    return(rhs_vars)
  }
  out <- unlist(rhs_vars, use.names = F)
  unique(out)
}

#' @export
rhs_vars.formula <- function(object, ...) {
  rhs <- object[[length(object)]]
  all.vars(rhs)
}

#' @export
rhs_vars.default <- function(object, ...) {
  character(0)
}

# adds an attribute nl to each component of the the formula indicating if the
# any of the predictors of the component are also predicted in another component
assign_nl_attr <- function(formula) {
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
is_nl.default <- function(object, ...) {
  isTRUE(attr(object, "nl"))
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

# add attribute if parameter is fixed or predicted
assign_constants <- function(formula) {
  dpars <- names(formula)
  for (dpar in dpars) {
    if (is_formula(formula[[dpar]])) {
      attr(formula[[dpar]], "constant") <- FALSE
    } else {
      attr(formula[[dpar]], "constant") <- TRUE
    }
  }
  formula
}

is_constant <- function(x) {
  UseMethod("is_constant")
}

#' @export
is_constant.bmmformula <- function(x) {
  unlist(sapply(x, is_constant))
}

#' @export
is_constant.default <- function(x) {
  isTRUE(attr(x, "constant"))
}
