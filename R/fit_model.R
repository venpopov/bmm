#' @title Fit Measurement Models using BRMS
#' @description Fit a Bayesian multilevel measurement model. Currently implemented
#'   are the two-parameter mixture model by Zhang and Luck (2008),
#'   the three-parameter mixture model by Bays et al (2009),
#'   and three different versions of the Interference Measurement Model
#'   (Oberauer et al., 2017). This is a wrapper function for [brms::brm], which is
#'   used to estimate the model.
#'
#' @param formula An object of class `brmsformula`. A symbolic description of
#'   the model to be fitted.
#' @param data An object of class data.frame, containing data of all variables
#'   used in the model. Response, target and lure values must be in radians.
#'   There must be as many lure value columns as the maximum setsize-1. For
#'   setsizes smaller than the maximum, values for non-existing non_targets must be
#'   coded as NA. The outcome variable must be response error relative to the
#'   target, not the raw response. Similarly, the lure values must be coded
#'   relative to the target. If the lure values are absolute, you must subtract
#'   from them the value of the target before running the model
#' @param model A description of the measurement model. "2p" for the 2
#'   parameter mixture model of Zhang and Luck (2008), "3p" for the 3 parameter
#'   mixture model of Bays et al (2009). "IMMabc" for the interference measurement model assuming swap
#'   errors to occur independent of spatial proximity between target and non-target
#'   items, "IMMbsc" for the interference measurement model assuming swap errors to occur
#'   only as a function of spatial proximity between target and non-targets, and "IMMfull"
#'   for the full interference measurement model accounting for swap both dependent and
#'   independent of proximity between target and non-targets (Oberauer et al., 2017).
#' @param parallel Logical; If TRUE, the number of cores on your machine will be
#'   detected and brms will fit max(chains, cores) number of chains (specified
#'   by the `chain` argument) in parallel using the parallel package
#' @param chains Numeric. Number of Markov chains (defaults to 4)
#' @param prior One or more `brmsprior` objects created by [brms::set_prior()] or related
#'   functions and combined using the c method or the + operator. See also
#'   [brms::get_prior()] for more help. Not necessary for the default model fitting, but
#'   you can provide prior constraints to model parameters
#' @param ... Further arguments passed to [brms::brm()] or Stan. See
#'   the description of [brms::brm()] for more details
#'
#' @returns An object of class brmsfit which contains the posterior draws along
#'   with many other useful information about the model. Use methods(class =
#'   "brmsfit") for an overview on available methods.
#'
#' @seealso [brms::brm()]
#'
#' @export
#'
fit_model <- function(formula, data, model, parallel = FALSE, chains = 4, prior = NULL, ...) {
  # TODO: generalize model-specific arguments

  # enable parallel sampling if parallel equals TRUE
  opts <- configure_options(nlist(parallel, chains))

  # check model, formula and data, and transform data if necessary
  model <- check_model(model)
  formula <- check_formula(model, formula)
  data <- check_data(model, data, formula)

  # generate the model specification to pass to brms later
  config_args <- configure_model(model, data, formula)

  # combine the default prior plus user given prior
  config_args <- combine_prior(config_args, prior)

  # estimate the model
  dots <- list(...)
  fit_args <- c(config_args, opts, dots)
  fit <- call_brm(fit_args)

  return(fit)
}

