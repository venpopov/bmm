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

mixture3p <- function(non_targets, setsize, ...) {
  .model_mixture3p(non_targets = non_targets,
                   setsize = setsize)
}

.model_mixture3p <- function(...) {
  out <- list(
    vars = nlist(...),
    info = list(
      domain = "Visual working memory",
      task = "Continuous reproduction",
      name = "Three-parameter mixture model by Bays et al (2009).",
      version = "",
      citation = paste0("Bays, P. M., Catalao, R. F. G., & Husain, M. (2009). ",
                        "The precision of visual working memory is set by allocation ",
                        "of a shared resource. Journal of Vision, 9(10), 1-11")
    ))
  class(out) = c("bmmmodel", "vwm", "nontargets", "mixture3p")
  out
}

#############################################################################!
# CONFIGURE_MODEL METHODS                                                ####
#############################################################################!
# Each model should have a corresponding configure_model.* function. See
# ?configure_model for more information.

#' @export
configure_model.mixture3p <- function(model, data, formula) {
  # retrieve arguments from the data check
  max_setsize <- attr(data, 'max_setsize')
  lure_idx_vars <- attr(data, "lure_idx_vars")
  non_targets <- model$vars$non_targets
  setsize_var <- model$vars$setsize

  # names for parameters
  kappa_nts <- paste0('kappa', 2:max_setsize)
  kappa_unif <- paste0('kappa',max_setsize + 1)
  theta_nts <- paste0('theta',2:max_setsize)
  mu_nts <- paste0('mu', 2:max_setsize)
  mu_unif <- paste0('mu', max_setsize + 1)

  # construct formula
  formula <- formula +
    brms::lf(mu1 ~ 1) +
    glue_lf(kappa_unif,' ~ 1') +
    glue_lf(mu_unif, ' ~ 1') +
    brms::nlf(theta1 ~ thetat) +
    brms::nlf(kappa1 ~ kappa)
  for (i in 1:(max_setsize-1)) {
    formula <- formula +
      glue_nlf(kappa_nts[i], ' ~ kappa') +
      glue_nlf(theta_nts[i], ' ~ ', lure_idx_vars[i], '*(thetant + log(inv_ss)) + ',
               '(1-', lure_idx_vars[i], ')*(-100)') +
      glue_nlf(mu_nts[i], ' ~ ', non_targets[i])
  }

  # define mixture family
  vm_list = lapply(1:(max_setsize+1), function(x) brms::von_mises(link="identity"))
  vm_list$order = "none"
  family <- brms::do_call(brms::mixture, vm_list)

  # define prior
  prior <-
    brms::prior_("constant(0)", class = "Intercept", dpar = "mu1") +
    brms::prior_("constant(0)", class = "Intercept", dpar = mu_unif) +
    brms::prior_("constant(-100)", class = "Intercept", dpar = kappa_unif) +
    brms::prior_("normal(2, 1)", class = "b", nlpar = "kappa") +
    brms::prior_("logistic(0, 1)", class = "b", nlpar = "thetat") +
    brms::prior_("logistic(0, 1)", class = "b", nlpar = "thetant")

  # if there is setsize 1 in the data, set constant prior over thetant for setsize1
  if ((1 %in% data$ss_numeric) && !is.numeric(data[[setsize_var]])) {
    prior <- prior +
      brms::prior_("constant(-100)", class="b", coef = paste0(setsize_var, 1), nlpar="thetant")
  }

  out <- nlist(formula, data, family, prior)
  return(out)
}
