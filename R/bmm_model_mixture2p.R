#############################################################################!
# MODELS                                                                 ####
#############################################################################!
# Each model should have a corresponding .model_* function which returns a list
# with the following attributes:
#   domain: the domain of the model (e.g. "Visual working memory")
#   name: the name of the model (e.g. "Two-parameter mixture model by Zhang and Luck (2008).")
#   citation: the citation for the model (e.g. "Zhang, W., & Luck, S. J. (2008).
#             Discrete fixed-resolution representations in visual working memory.
#             Nature, 453(7192), 233–235. https://doi.org/10.1038/nature06860")
#   class: a character vector with the class of the model (e.g. c("vwm","mixture2p"))
#
# The class attribute is used by generic S3 functions to perform data checks and
# model configuration. The classes should be ordered from most general to most
# specific c("vwm","nontargets","mixture3p"). A general class exists when the same operations
# can be performed on multiple models. For example, the 'mixture3p', 'IMMabc', 'IMMbsc'
# and 'IMMfull' models all have non-targets and setsize arguments, so the same
# data checks can be performed on all of them. The 'mixture2p' model does not have
# non-targets or setsize arguments, so it has a different class.

mixture2p <- function(...) {
  .model_mixture2p()
}

.model_mixture2p <- function(...) {
  out <- list(
      vars = list(...),
      info = list(
        domain = "Visual working memory",
        task = "Continuous reproduction",
        name = "Two-parameter mixture model by Zhang and Luck (2008).",
        version = "",
        citation = paste0("Zhang, W., & Luck, S. J. (2008). Discrete fixed-resolution",
          "representations in visual working memory. Nature, 453(7192), 233-235")
      ))
  class(out) <- c("bmmmodel", "vwm", "mixture2p")
  out
}

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
