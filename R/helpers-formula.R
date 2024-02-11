#' checks if the formula is valid for the specified model
#' @param model a model list object returned from check_model()
#' @param formula user supplied formula
#' @return the formula object
#' @keywords internal, developer
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



#' Extracts the name of the response variable in a formula
#'
#' to use this with brmsformula objects, you have to pass the internal formula object
#' from the brmsformula object. E.g. formula$formula
#' @param formula an object of class formula
#' @noRd
#' @return String. Name of the response variable
get_response <- function(formula) {
  tt <- stats::terms(formula)
  vars <- as.character(attr(tt, "variables"))[-1] ## [1] is the list call
  response <- attr(tt, "response") # index of response var
  vars[response]
}

# extract vars from brmsformula
# @param formula: brms formula
# @param include_resp: include response variable in the output
extract_vars <- function(formula, family = NULL, include_resp = FALSE) {
  if (!is.null(family)) {
    formula$family <- family
  }
  bterms <- tryCatch({
    brms::brmsterms(formula)
  }, error = function(e) {
    if (grepl('not a valid distributional', e$message)) {
      stop(glue::glue("brmsterms() returned the following error:\n '{e$message}'.\n\n",
                 "You have to provide the family to the family argument. E.g. family = 'von_mises'"))
    } else {
      stop(e)
    }
  })

  all_vars <- all.vars(bterms$allvars)
  if (include_resp) {
    return(all_vars)
  }
  resp <- formula$resp
  all_vars[not_in(all_vars, resp)]
}


# check if the data is sorted by the predictors
is_data_ordered <- function(data, formula, ...) {
  predictors <- extract_vars(formula, ...)
  data <- data[,predictors]
  gr_idx <- do.call(paste, c(data, list(sep="_")))
  is_ordered <- !has_nonconsecutive_duplicates(gr_idx)
  is_ordered
}

# checks if all repetitions of a given value are consecutive in a vector
# by iterating over unique values and checking if all their positions are consecutive
has_nonconsecutive_duplicates <- function(vec) {
  unique_vals <- unique(vec)
  cond <- TRUE
  for(val in unique_vals) {
    positions <- which(vec == val)
    cond <- cond & all(diff(positions) == 1)
  }
  !cond
}
