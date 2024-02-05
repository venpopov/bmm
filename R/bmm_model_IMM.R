#############################################################################!
# MODELS                                                                 ####
#############################################################################!

.model_IMMabc <- function(non_targets, setsize, ...) {
  out <- list(
    vars = nlist(non_targets, setsize),
    info = list(
      domain = "Visual working memory",
      task = "Continuous reproduction",
      name = "Interference measurement model by Oberauer and Lin (2017).",
      version = "abc",
      citation = paste0("Oberauer, K., & Lin, H.Y. (2017). An interference model ",
                        "of visual working memory. Psychological Review, 124(1), 21-59"),
      requirements = paste0('- The response vairable should be in radians and ',
                            'represent the angular error relative to the target\n  ',
                            '- The non-target variables should be in radians and be ',
                            'centered relative to the target'),
      parameters = list(
        kappa = "Concentration parameter of the von Mises distribution (log scale)",
        a = "General activation of memory items",
        b = "Background activation (internally fixed to 0)",
        c = "Context activation"
      )
    ))
  class(out) <- c("bmmmodel", "vwm","nontargets","IMMabc")
  out
}

.model_IMMbsc <- function(non_targets, setsize, spaPos, ...) {
  out <- list(
    vars = nlist(non_targets, setsize, spaPos),
    info = list(
      domain = "Visual working memory",
      task = "Continuous reproduction",
      name = "Interference measurement model by Oberauer and Lin (2017).",
      version = "bsc",
      citation = paste0("Oberauer, K., & Lin, H.Y. (2017). An interference model ",
                        "of visual working memory. Psychological Review, 124(1), 21-59"),
      requirements = paste0('- The response vairable should be in radians and ',
                            'represent the angular error relative to the target\n  ',
                            '- The non-target variables should be in radians and be ',
                            'centered relative to the target'),
      parameters = list(
        kappa = "Concentration parameter of the von Mises distribution (log scale)",
        b = "Background activation (internally fixed to 0)",
        c = "Context activation",
        s = "Spatial similarity gradient"
      )
    ))
  class(out) <- c("bmmmodel","vwm","nontargets","IMMspatial","IMMbsc")
  out
}

.model_IMMfull <- function(non_targets, setsize, spaPos, ...) {
  out <- list(
    vars = nlist(non_targets, setsize, spaPos),
    info = list(
      domain = "Visual working memory",
      task = "Continuous reproduction",
      name = "Interference measurement model by Oberauer and Lin (2017).",
      version = "full",
      citation = paste0("Oberauer, K., & Lin, H.Y. (2017). An interference model ",
                        "of visual working memory. Psychological Review, 124(1), 21-59"),
      requirements = paste0('- The response vairable should be in radians and ',
                            'represent the angular error relative to the target\n  ',
                            '- The non-target variables should be in radians and be ',
                            'centered relative to the target'),
      parameters = list(
        kappa = "Concentration parameter of the von Mises distribution (log scale)",
        a = "General activation of memory items",
        b = "Background activation (internally fixed to 0)",
        c = "Context activation",
        s = "Spatial similarity gradient"
      )
    ))
  class(out) <- c("bmmmodel","vwm","nontargets","IMMspatial","IMMfull")
  out
}

# user facing alias
#' @title `r .model_IMMfull(NA, NA, NA)$info$name`
#' @name IMM
#' @details `r model_info(IMMfull(NA, NA, NA), components =c('domain', 'task', 'name', 'citation'))`
#' #### Version: `IMMfull`
#' `r model_info(IMMfull(NA, NA, NA), components =c('requirements', 'parameters'))`
#' #### Version: `IMMbsc`
#' `r model_info(IMMbsc(NA, NA, NA), components =c('requirements', 'parameters'))`
#' #### Version: `IMMabc`
#' `r model_info(IMMabc(NA, NA, NA), components =c('requirements', 'parameters'))`
#' @param non_targets A character vector with the names of the non-target variables.
#'   The non_target variables should be in radians and be centered relative to the
#'   target.
#' @param setsize Name of the column containing the set size variable (if
#'   setsize varies) or a numeric value for the setsize, if the setsize is
#'   fixed.
#' @param spaPos A vector of names of the columns containing the spatial distances of
#'   non-target items to the target item. Only necessary for the `IMMbsc` and `IMMfull` models
#' @param ... used internally for testing, ignore it
#' @return An object of class `bmmmodel`
#' @keywords bmmmodel
#' @export
IMMfull <- .model_IMMfull

#' @rdname IMM
#' @keywords bmmmodel
#' @export
IMMbsc <- .model_IMMbsc

#' @rdname IMM
#' @keywords bmmmodel
#' @export
IMMabc <- .model_IMMabc



#############################################################################!
# CHECK_DATA S3 methods                                                  ####
#############################################################################!
# A check_data.* function should be defined for each class of the model.
# If a model shares methods with other models, the shared methods should be
# defined in data-helpers.R. Put here only the methods that are specific to
# the model. See ?check_data for details

#' @export
check_data.IMMspatial <- function(model, data, formula) {
  spaPos <- model$vars$spaPos
  max_setsize <- attr(data, 'max_setsize')

  if (length(spaPos) < max_setsize - 1) {
    stop(paste0("The number of columns for spatial positions in the argument",
                "'spaPos' is less than max(setsize)-1"))
  } else if (length(spaPos) > max_setsize-1) {
    stop(paste0("The number of columns for spatial positions in the argument",
                "spaPos'' is more than max(setsize)-1"))
  }

  if (max(abs(data[,spaPos]), na.rm=T) > 10) {
    data[,spaPos] <- data[,spaPos]*pi/180
    warning('It appears your spatial position variables are in degrees. We will transform it to radians.')
  }
  # wrap spatial position variables around the circle (range = -pi to pi)
  data[,spaPos] <- bmm::wrap(data[,spaPos])
  data = NextMethod("check_data")

  return(data)
}

#############################################################################!
# CONFIGURE_MODEL METHODS                                                ####
#############################################################################!
# Each model should have a corresponding configure_model.* function. See
# ?configure_model for more information.

#' @export
configure_model.IMMabc <- function(model, data, formula) {
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
configure_model.IMMbsc <- function(model, data, formula) {
  # retrieve arguments from the data check
  max_setsize <- attr(data, 'max_setsize')
  lure_idx_vars <- attr(data, "lure_idx_vars")
  non_targets <- model$vars$non_targets
  setsize_var <- model$vars$setsize
  spaPos <- model$vars$spaPos

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
configure_model.IMMfull <- function(model, data, formula) {
  # retrieve arguments from the data check
  max_setsize <- attr(data, 'max_setsize')
  lure_idx_vars <- attr(data, "lure_idx_vars")
  non_targets <- model$vars$non_targets
  setsize_var <- model$vars$setsize
  spaPos <- model$vars$spaPos

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
