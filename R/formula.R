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



