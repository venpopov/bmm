#' Generic S3 method for constructing a function to generate initial values for a bmmodel
#'
#' Called by bmm() to automatically constructing a function that generates initial values
#' to ensure proper starting values. This function is particularly relevant for accumulation
#' models that require non-decition times smaller than the minimal RT and sufficiently large
#' boundary separation values to ensure proper sampling.
#' @param model A `bmmodel` object
#' @param data A data.frame containing the data used in the model
#' @param formula A `brmsformula` object returned from configure_model()
#' @param user_prior A `brmsprior` object given by the user as an argument to
#'  bmm()
#' @param ... Additional arguments passed to the method
#'
#' @return A `brmsprior` object containing the default priors for the model
#'
#' @export
#'
#' @examplesIf isTRUE(Sys.getenv("BMM_EXAMPLES"))
#' configure_prior.mixture3p <- function(model, data, formula, user_prior, ...) {
#'   # if there is set_size 1 in the data, set constant prior over thetant for set_size1
#'   prior <- brms::empty_prior()
#'   set_size_var <- model$other_vars$set_size
#'   prior_cond <- any(data$ss_numeric == 1) && !is.numeric(data[[set_size_var]])
#'
#'   thetant_preds <- rhs_vars(formula$pforms$thetant)
#'   if (prior_cond && set_size_var %in% thetant_preds) {
#'     prior <- prior + brms::prior_("constant(-100)",
#'       class = "b",
#'       coef = paste0(set_size_var, 1),
#'       nlpar = "thetant"
#'     )
#'   }
#'   # check if there is a random effect on theetant that include set_size as predictor
#'   bterms <- brms::brmsterms(formula$pforms$thetant)
#'   re_terms <- bterms$dpars$mu$re
#'   if (!is.null(re_terms)) {
#'     for (i in 1:nrow(re_terms)) {
#'       group <- re_terms$group[[i]]
#'       form <- re_terms$form[[i]]
#'       thetant_preds <- rhs_vars(form)
#'
#'       if (prior_cond && set_size_var %in% thetant_preds) {
#'         prior <- prior + brms::prior_("constant(1e-8)",
#'           class = "sd",
#'           coef = paste0(set_size_var, 1),
#'           group = group,
#'           nlpar = "thetant"
#'         )
#'       }
#'     }
#'   }
#'
#'   prior
#' }
#'
#' @keywords internal developer
construct_initfun <- function(model, data, formula,  ...) {
  UseMethod("construct_initfun")
}

#' @export
construct_initfun.default <- function(model, data, formula, ...) {
  NULL
}

#' @export
construct_initfun.bmmodel <- function(model, data, formula, ...) {
  NextMethod("construct_initfun")
  return(1)
}
