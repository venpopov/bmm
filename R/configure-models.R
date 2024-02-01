# generic S3 method for configuring the formula, data, and family based on model type
#' @inheritParams fit_model
configure_model <- function(model, data, formula, ...) {
  UseMethod("configure_model")
}

#' @export
configure_model.default <- function(model, data, formula, ...) {
  ##
}

#' @export
configure_model.2p <- function(model, data, formula, ...) {
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

#' @export
configure_model.3p <- function(model, data, formula, ...) {
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


