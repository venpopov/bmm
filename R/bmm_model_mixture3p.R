#############################################################################!
# MODELS                                                                 ####
#############################################################################!

.model_mixture3p <- function(respErr, non_targets, setsize, ...) {
  out <- list(
    vars = nlist(respErr, non_targets, setsize),
    info = list(
      domain = "Visual working memory",
      task = "Continuous reproduction",
      name = "Three-parameter mixture model by Bays et al (2009).",
      version = "NA",
      citation = paste0("Bays, P. M., Catalao, R. F. G., & Husain, M. (2009). ",
                        "The precision of visual working memory is set by allocation ",
                        "of a shared resource. Journal of Vision, 9(10), 1-11"),
      requirements = paste0('- The response vairable should be in radians and ',
                            'represent the angular error relative to the target\n  ',
                            '- The non-target variables should be in radians and be ',
                            'centered relative to the target'),
      parameters = list(
        kappa = "Concentration parameter of the von Mises distribution (log scale)",
        thetat = "Mixture weight for target responses",
        thetant = "Mixture weight for non-target responses"
      )
    ))
  class(out) = c("bmmmodel", "vwm", "nontargets", "mixture3p")
  out
}


# user facing alias
#' @title `r .model_mixture3p(NA, NA, NA)$info$name`
#' @details `r model_info(mixture3p(NA, NA, NA))`
#' @param respErr The name of the variable in the provided dataset containing the response error. The response Error should code the response relative to the to-be-recalled target in radians. You can transform the response error in degrees to radian using the `deg2rad` function.
#' @param non_targets A character vector with the names of the non-target variables.
#'   The non_target variables should be in radians and be centered relative to the
#'   target.
#' @param setsize Name of the column containing the set size variable (if
#'   setsize varies) or a numeric value for the setsize, if the setsize is
#'   fixed.
#' @param ... used internally for testing, ignore it
#' @return An object of class `bmmmodel`
#' @keywords bmmmodel
#' @export
#' @examples
#' \dontrun{
#' # generate artificial data from the Bays et al (2009) 3-parameter mixture model
#' dat <- data.frame(
#'   y = rmixture3p(n=2000, mu = c(0,1,-1.5,2)),
#'   nt1_loc = 1,
#'   nt2_loc = -1.5,
#'   nt3_loc = 2
#' )
#'
#' # define formula
#' ff <- brms::bf(y ~ 1,
#'               kappa ~ 1,
#'               thetat ~ 1,
#'               thetant ~ 1)
#'
#' # specify the 3-parameter model
#' model <- mixture3p(non_targets = paste0('nt',1:3,'_loc'), setsize = 4)
#'
#' # fit the model
#' fit <- fit_model(formula = ff,
#'                  data = dat,
#'                  model = model,
#'                  parallel=T,
#'                  iter = 500,
#'                  backend='cmdstanr')
#' }
mixture3p <- .model_mixture3p

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
  respErr <- model$vars$respErr
  non_targets <- model$vars$non_targets
  setsize_var <- model$vars$setsize

  # extract formulas for parameters
  pform_names <- names(formula)
  pform <- formula

  # add fixed intercept for bias if no formula was included
  if (!"bias" %in% pform_names) {
    bias_form <- bias ~ 1
    pform <- c(pform, bias_form)
    names(pform) <- c(pform_names,"bias")
  }

  # names for parameters
  kappa_nts <- paste0('kappa', 2:max_setsize)
  kappa_unif <- paste0('kappa',max_setsize + 1)
  theta_nts <- paste0('theta',2:max_setsize)
  mu_nts <- paste0('mu', 2:max_setsize)
  mu_unif <- paste0('mu', max_setsize + 1)

  # construct formula
  formula <- brms::bf(paste0(respErr,"~ bias"), nl = T)

  # add parameter formulas to model formula
  for (i in 1:length(pform)) {
    predictors <- rsample::form_pred(pform[[i]])
    if (any(predictors %in% names(pform))) {
      formula <- formula + brms::nlf(pform[[i]])
    } else {
      formula <- formula + brms::lf(pform[[i]])
    }
  }

  formula <- formula +
    glue_lf(kappa_unif,' ~ 1') +
    glue_lf(mu_unif, ' ~ 1') +
    brms::nlf(theta1 ~ thetat) +
    brms::nlf(kappa1 ~ kappa)
  for (i in 1:(max_setsize - 1)) {
    formula <- formula +
      glue_nlf(kappa_nts[i], ' ~ kappa') +
      glue_nlf(theta_nts[i], ' ~ ', lure_idx_vars[i], '*(thetant + log(inv_ss)) + ',
               '(1-', lure_idx_vars[i], ')*(-100)') +
      glue_nlf(mu_nts[i], ' ~ ', non_targets[i])
  }

  # define mixture family
  vm_list = lapply(1:(max_setsize + 1), function(x) brms::von_mises(link="identity"))
  vm_list$order = "none"
  family <- brms::do_call(brms::mixture, vm_list)

  # define prior
  prior <-
    brms::prior_("constant(0)", nlpar = "bias") +
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
