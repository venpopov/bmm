#############################################################################!
# MODELS                                                                 ####
#############################################################################!

.model_mixture2p <- function(resp_err, ...) {
  out <- list(
    resp_vars = nlist(resp_err),
    other_vars = nlist(),
    info = list(
      domain = "Visual working memory",
      task = "Continuous reproduction",
      name = "Two-parameter mixture model by Zhang and Luck (2008).",
      version = "NA",
      citation = paste0("Zhang, W., & Luck, S. J. (2008). Discrete fixed-resolution ",
                        "representations in visual working memory. Nature, 453(7192), 233-235"),
      requirements = paste0('- The response vairable should be in radians and ',
                            'represent the angular error relative to the target'),
      parameters = list(
        mu1 = paste0("Location parameter of the von Mises distribution for memory responses",
                     "(in radians). Fixed internally to 0 by default."),
        kappa = "Concentration parameter of the von Mises distribution (log scale)",
        thetat = "Mixture weight for target responses"
      ),
      fixed_parameters = list(
        mu1 = 0
      )),
    void_mu = FALSE
  )
  class(out) <- c("bmmmodel", "vwm", "mixture2p")
  out
}

# user facing alias
#' @title `r .model_mixture2p(NA)$info$name`
#' @details `r model_info(mixture2p(NA))`
#' @param resp_err The name of the variable in the provided dataset containing
#'   the response error. The response Error should code the response relative to
#'   the to-be-recalled target in radians. You can transform the response error
#'   in degrees to radian using the `deg2rad` function.
#' @param ... used internally for testing, ignore it
#' @return An object of class `bmmmodel`
#' @keywords bmmmodel
#' @examples
#' \dontrun{
#' # generate artificial data
#' dat <- data.frame(y = rmixture2p(n=2000))
#'
#' # define formula
#' ff <- bmmformula(kappa ~ 1,
#'                  thetat ~ 1)
#'
#' model <- mixture2p(resp_err = "y")
#'
#' # fit the model
#' fit <- fit_model(formula = ff,
#'                  data = dat,
#'                  model = model,
#'                  parallel=T,
#'                  iter=500,
#'                  backend='cmdstanr')
#' }
#' @export
mixture2p <- .model_mixture2p

#############################################################################!
# CONFIGURE_MODEL METHODS                                                ####
#############################################################################!
# Each model should have a corresponding configure_model.* function. See
# ?configure_model for more information.

#' @export
configure_model.mixture2p <- function(model, data, formula) {
  # construct main brms formula from the bmm formula
  bmm_formula <- formula
  formula <- bmf2bf(model, bmm_formula)

  # provide additional formulas for implementing the mixture2p
  formula <- formula +
    brms::lf(kappa2 ~ 1, mu2 ~ 1) +
    brms::nlf(kappa1 ~ kappa) +
    brms::nlf(theta1 ~ thetat)

  # specify the mixture family
  family <- brms::mixture("von_mises", "von_mises", order = "none")

  # set priors for the estimated parameters
  additional_constants <- list(kappa2 = -100, mu2 = 0)
  prior <- fixed_pars_priors(model, additional_constants) +
    brms::prior_("normal(2, 1)", class = "b", nlpar = "kappa") +
    brms::prior_("logistic(0, 1)", class = "b", nlpar = "thetat")

  out <- nlist(formula, data, family, prior)
  return(out)
}
