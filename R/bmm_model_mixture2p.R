#############################################################################!
# MODELS                                                                 ####
#############################################################################!

.model_mixture2p <- function(...) {
  out <- list(
      vars = list(...),
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
          kappa = "Concentration parameter of the von Mises distribution (log scale)",
          thetat = "Mixture weight for target responses"
        )
      ))
  class(out) <- c("bmmmodel", "vwm", "mixture2p")
  out
}

# user facing alias
#' @title `r .model_mixture2p()$info$name`
#' @details `r model_info(mixture2p())`
#' @param ... no required arguments, call as `mixture2p()`
#' @return An object of class `bmmmodel`
#' @keywords bmmmodel
#' @examples
#' \dontrun{
#' # generate artificial data
#' dat <- gen_3p_data(N=2000, pmem=0.6, pnt=0.3, kappa=10, setsize=4, relative_resp=T)
#'
#' # define formula
#' ff <- brms::bf(y ~ 1,
#'               kappa ~ 1,
#'               thetat ~ 1)
#'
#' model <- mixture2p()
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
  # specify the formula for the mixture model
  formula <- formula +
    brms::lf(kappa2 ~ 1, mu1 ~ 1, mu2 ~ 1) +
    brms::nlf(kappa1 ~ kappa) +
    brms::nlf(theta1 ~ thetat)

  # specify the mixture family
  family <- brms::mixture("von_mises", "von_mises", order = "none")

  # set priors for the estimated parameters
  prior <- # fix mean of the first von Mises to zero
    brms::prior_("constant(0)", class = "Intercept", dpar = "mu1") +
    # fix mean of the second von Mises to zero
    brms::prior_("constant(0)", class = "Intercept", dpar = "mu2") +
    # fix kappa of the second von Mises to (alomst) zero
    brms::prior_("constant(-100)", class = "Intercept", dpar = "kappa2") +
    # set reasonable default priors for the estimated parameters
    brms::prior_("normal(2, 1)", class = "b", nlpar = "kappa") +
    brms::prior_("logistic(0, 1)", class = "b", nlpar = "thetat")

  out <- nlist(formula, data, family, prior)
  return(out)
}
