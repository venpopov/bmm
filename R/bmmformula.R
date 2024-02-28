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
  formula
}


# alias of bmmformula
#' @export
bmf <- function(formula, ...) {
  bmmformula(formula, ...)
}


# method for adding formulas to a bmmformula
#' @export
"+.bmmformula" <- function(f1,f2) {
  if (!is.bmmformula(f1)) {
    stop("The first argument must be a bmmformula.")
  }
  if (is.formula(f2)) {
    par2 <- all.vars(f2)[1]
    if (par2 %in% names(f1)) {
      message2(paste("The parameter", par2, "is already part of the formula.",
                    "Overwriting the initial formula."))
    }
    f1[[par2]] <- f2
  } else if (is.bmmformula(f2)) {
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
  f1
}


# method for subsetting a bmmformula, ensuring the attributes are preserved
#' @export
'[.bmmformula' <- function(formula, pars) {
  attrs <- attributes(formula)
  out <- unclass(formula)
  out <- out[pars]
  attributes(out) <- attrs
  names(out) <- pars
  out
}

rhs_vars <- function(formula) {
  UseMethod("rhs_vars")
}

#' @export
rhs_vars.bmmformula <- function(formula) {
  lhs_vars <- names(formula)
  rhs_vars <- list()
  for (var in lhs_vars) {
    rhs_vars[[var]] <- rhs_vars.formula(formula[[var]])
  }
  out <- unlist(rhs_vars, use.names=F)
  unique(out)
}

#' @export
rhs_vars.formula <- function(formula) {
  all_vars <- all.vars(formula)
  all_vars[-1]
}


is.formula <- function(x) {
  inherits(x, "formula")
}

is.bmmformula <- function(x) {
  inherits(x, "bmmformula")
}

is.brmsformula <- function(x) {
  inherits(x, "brmsformula")
}
