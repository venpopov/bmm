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
#   class: a character vector with the class of the model (e.g. c("vwm","2p"))
#
# The class attribute is used by generic S3 functions to perform data checks and
# model configuration. The classes should be ordered from most general to most
# specific c("vwm","nontargets","3p"). A general class exists when the same operations
# can be performed on multiple models. For example, the '3p', 'IMMabc', 'IMMbsc'
# and 'IMMfull' models all have non-targets and setsize arguments, so the same
# data checks can be performed on all of them. The '2p' model does not have
# non-targets or setsize arguments, so it has a different class.

.model_IMMabc <- function() {
  out <- list()
  attr(out, "domain") <- "Visual working memory"
  attr(out, "name") <- "Interference measurement model by Oberauer and Lin (2017)."
  class(out) <- c("vwm","nontargets","IMMabc")
  out
}

.model_IMMbsc <- function() {
  out <- list()
  attr(out, "domain") <- "Visual working memory"
  attr(out, "name") <- "Interference measurement model by Oberauer and Lin (2017)."
  class(out) <- c("vwm","nontargets", "IMMspatial","IMMbsc")
  out
}

.model_IMMfull <- function() {
  out <- list()
  attr(out, "domain") <- "Visual working memory"
  attr(out, "name") <- "Interference measurement model by Oberauer and Lin (2017)."
  class(out) <- c("vwm","nontargets","IMMspatial","IMMfull")
  out
}


#############################################################################!
# CHECK_DATA S3 methods                                                  ####
#############################################################################!
# A check_data.* function should be defined for each class of the model.
# If a model shares methods with other models, the shared methods should be
# defined in data-helpers.R. Put here only the methods that are specific to
# the model. See ?check_data for details

#' @export
check_data.IMMspatial <- function(model, data, formula, ...) {
  dots <- list(...)
  if(is.null(dots$spaPos)) {
    stop("Argument 'spaPos' must be specified.")
  }
  spaPos <- dots$spaPos

  if (length(spaPos) < attr(data, 'max_setsize') - 1) {
    stop(paste0("The number of columns for spatial positions in the argument",
                "'spaPos' is less than max(setsize)-1"))
  } else if (length(spaPos) > attr(data, 'max_setsize')-1) {
    stop(paste0("The number of columns for spatial positions in the argument",
                "spaPos'' is more than max(setsize)-1"))
  }

  spaPos <- dots$spaPos
  if (max(abs(data[,spaPos]), na.rm=T) > 10) {
    data[,spaPos] <- data[,spaPos]*pi/180
    warning('It appears your spatial position variables are in degrees. We will transform it to radians.')
  }
  # wrap spatial position variables around the circle (range = -pi to pi)
  data[,spaPos] <- bmm::wrap(data[,spaPos])

  # save some variables as attributes of the data for later use
  attr(data, "spaPos") <- spaPos

  data = NextMethod("check_data")

  return(data)
}

#############################################################################!
# CONFIGURE_MODEL METHODS                                                ####
#############################################################################!
# Each model should have a corresponding configure_model.* function. See
# ?configure_model for more information.

#' @export
configure_model.IMMabc <- function(model, data, formula, ...) {
  # retrieve arguments from the data check
  max_setsize <- attr(data, "max_setsize")
  non_targets <- attr(data, "non_targets")
  lure_idx_vars <- attr(data, "lure_idx_vars")
  setsize_var <- attr(data, "setsize_var")

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
    brms::nlf(theta1 ~ c + a) +
    brms::nlf(kappa1 ~ kappa)
  for (i in 1:(max_setsize-1)) {
    formula <- formula +
      glue_nlf(kappa_nts[i], ' ~ kappa') +
      glue_nlf(theta_nts[i], ' ~ ', lure_idx_vars[i], '*(a) + ',
               '(1-', lure_idx_vars[i], ')*(-100)') +
      glue_nlf(mu_nts[i], ' ~ ', non_targets[i])
  }

  # define mixture family
  vm_list = lapply(1:(max_setsize+1), function(x) brms::von_mises(link="identity"))
  vm_list$order = "none"
  family <- brms::do_call(brms::mixture, vm_list)

  # define prior
  prior <- # fix mean of the first von Mises to zero
    brms::prior_("constant(0)", class = "Intercept", dpar = "mu1") +
    # fix mean of the guessing distribution to zero
    brms::prior_("constant(0)", class = "Intercept", dpar = mu_unif) +
    # fix kappa of the second von Mises to (alomst) zero
    brms::prior_("constant(-100)", class = "Intercept", dpar = kappa_unif) +
    # set reasonable priors fpr the to be estimated parameters
    brms::prior_("normal(2, 1)", class = "b", nlpar = "kappa") +
    brms::prior_("normal(0, 1)", class = "b", nlpar = "c") +
    brms::prior_("normal(0, 1)", class = "b", nlpar = "a")

  # if there is setsize 1 in the data, set constant prior over thetant for setsize1
  if ((1 %in% data$ss_numeric) && !is.numeric(data[[setsize_var]])) {
    prior <- prior +
      brms::prior_("constant(0)", class="b", coef = paste0(setsize_var, 1), nlpar="a")
  }

  out <- nlist(formula, data, family, prior)
  return(out)
}

#' @export
configure_model.IMMbsc <- function(model, data, formula, ...) {
  # retrieve arguments from the data check
  max_setsize <- attr(data, "max_setsize")
  non_targets <- attr(data, "non_targets")
  lure_idx_vars <- attr(data, "lure_idx_vars")
  spaPos <- attr(data, "spaPos")
  setsize_var <- attr(data, "setsize_var")

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
    brms::nlf(theta1 ~ c) +
    brms::nlf(kappa1 ~ kappa) +
    brms::nlf(expS ~ exp(s))
  for (i in 1:(max_setsize-1)) {
    formula <- formula +
      glue_nlf(kappa_nts[i], ' ~ kappa') +
      glue_nlf(theta_nts[i], ' ~ ', lure_idx_vars[i], '*(exp(-expS*',spaPos[i],')*c) + ',
               '(1-', lure_idx_vars[i], ')*(-100)') +
      glue_nlf(mu_nts[i], ' ~ ', non_targets[i])
  }

  # define mixture family
  vm_list = lapply(1:(max_setsize+1), function(x) brms::von_mises(link="identity"))
  vm_list$order = "none"
  family <- brms::do_call(brms::mixture, vm_list)

  # define prior
  prior <- # fix mean of the first von Mises to zero
    brms::prior_("constant(0)", class = "Intercept", dpar = "mu1") +
    # fix mean of the guessing distribution to zero
    brms::prior_("constant(0)", class = "Intercept", dpar = mu_unif) +
    # fix kappa of the second von Mises to (alomst) zero
    brms::prior_("constant(-100)", class = "Intercept", dpar = kappa_unif) +
    # set reasonable priors fpr the to be estimated parameters
    brms::prior_("normal(2, 1)", class = "b", nlpar = "kappa") +
    brms::prior_("normal(0, 1)", class = "b", nlpar = "c") +
    brms::prior_("normal(0, 1)", class = "b", nlpar = "s")

  # if there is setsize 1 in the data, set constant prior over thetant for setsize1
  if ((1 %in% data$ss_numeric) && !is.numeric(data[[setsize_var]])) {
    prior <- prior +
      brms::prior_("constant(0)", class="b", coef = paste0(setsize_var, 1), nlpar="s")
  }

  out <- nlist(formula, data, family, prior)
  return(out)
}

#' @export
configure_model.IMMfull <- function(model, data, formula, ...) {
  # retrieve arguments from the data check
  max_setsize <- attr(data, "max_setsize")
  non_targets <- attr(data, "non_targets")
  lure_idx_vars <- attr(data, "lure_idx_vars")
  spaPos <- attr(data, "spaPos")
  setsize_var <- attr(data, "setsize_var")

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
    brms::nlf(theta1 ~ c + a) +
    brms::nlf(kappa1 ~ kappa) +
    brms::nlf(expS ~ exp(s))
  for (i in 1:(max_setsize-1)) {
    formula <- formula +
      glue_nlf(kappa_nts[i], ' ~ kappa') +
      glue_nlf(theta_nts[i], ' ~ ', lure_idx_vars[i], '*(exp(-expS*',spaPos[i],')*c + a) + ',
               '(1-', lure_idx_vars[i], ')*(-100)') +
      glue_nlf(mu_nts[i], ' ~ ', non_targets[i])
  }

  # define mixture family
  vm_list = lapply(1:(max_setsize+1), function(x) brms::von_mises(link="identity"))
  vm_list$order = "none"
  family <- brms::do_call(brms::mixture, vm_list)

  # define prior
  prior <- # fix mean of the first von Mises to zero
    brms::prior_("constant(0)", class = "Intercept", dpar = "mu1") +
    # fix mean of the guessing distribution to zero
    brms::prior_("constant(0)", class = "Intercept", dpar = mu_unif) +
    # fix kappa of the second von Mises to (alomst) zero
    brms::prior_("constant(-100)", class = "Intercept", dpar = kappa_unif) +
    # set reasonable priors fpr the to be estimated parameters
    brms::prior_("normal(2, 1)", class = "b", nlpar = "kappa") +
    brms::prior_("normal(0, 1)", class = "b", nlpar = "c") +
    brms::prior_("normal(0, 1)", class = "b", nlpar = "a") +
    brms::prior_("normal(0, 1)", class = "b", nlpar = "s")

  # if there is setsize 1 in the data, set constant prior over thetant for setsize1
  if ((1 %in% data$ss_numeric) && !is.numeric(data[[setsize_var]])) {
    prior <- prior +
      brms::prior_("constant(0)", class="b", coef = paste0(setsize_var, 1), nlpar="a")+
      brms::prior_("constant(0)", class="b", coef = paste0(setsize_var, 1), nlpar="s")
  }

  out <- nlist(formula, data, family, prior)
  return(out)
}
