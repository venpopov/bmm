#' @title Fit Mixture Model for Visual Working Memory using BRMS
#' @description Fit a Bayesian multilevel mixture model for visual working
#'   memory. Currently implemented are the two-parameter mixture model by Zhang
#'   and Luck (2008), and the three-parameter mixture model by Bays et al
#'   (2009). This is a wrapper function for \code{\link{brms::brm}}, which is
#'   used to estimate the model
#'
#' @param formula An object of class `brmsformula`. A symbolic description of
#'   the model to be fitted.
#' @param data An object of class data.frame, containing data of all variables
#'   used in the model. Response, target and lure values must be in radians.
#'   There must be as many lure value columns as the maximum setsize-1. For
#'   setsizes smaller than the maximum, values for non-existing lures must be
#'   coded as NA. The outcome variable must be response error relative to the
#'   target, not the raw response. Similarly, the lure values must be coded
#'   relative to the target. If the lure values are absolute, you must subtract
#'   from them the value of the target before running the model
#' @param model_type A description of the mixture model. "2p" for the 2
#'   parameter mixture model of Zhang and Luck (2008), "3p" for the 3 parameter
#'   mixture model of Bays et al (2009)
#' @param target Name of the column containing the values of the target. Only
#'   necessary if argument `relative==F`
#' @param lures A vector of names of the columns containing the non-target
#'   values. Only necessary if `model_type=="3p"`. If the response is the
#'   response error centered on the target, then the values for the non-target
#'   items also have to be centered on the target. If the response is the raw
#'   response and not centered on the target, then the non-target values should
#'   also not be centered on the target
#' @param setsize Name of the column containing the set size variable (if
#'   setsize varies) or a numeric value for the setsize, if the setsize is
#'   fixed. Only necessary if fitting the 3 parameter mixture model.
#' @param relative Logical; TRUE if the response is the response error centered
#'   on the target value, and the lure positions (for the 3 parameter model) are
#'   also centered relative to the target. FALSE if the response and the lures
#'   are the absolute values not centered on the target. Default is TRUE.
#' @param parallel Logical; If TRUE, the number of cores on your machine will be
#'   detected and brms will fit max(chains, cores) number of chains (specified
#'   by the `chain` argument) in parallel using the parallel package
#' @param chains Numeric. Number of Markov chains (defaults to 4)
#' @param prior One or more brmsprior objects created by set_prior or related
#'   functions and combined using the c method or the + operator. See also
#'   get_prior for more help. Not necessary for the default model fitting, but
#'   you can provide prior constraints to model parameters
#' @param ... Further arguments passed to \code{\link{brms::brm}} or Stan. See
#'   the description of `brm()` for more details
#' @returns An object of class brmsfit which contains the posterior draws along
#'   with many other useful information about the model. Use methods(class =
#'   "brmsfit") for an overview on available methods.
#' @seealso \code{\link{brms::brm}}
#' @export
fit_model <- function(formula, data, model_type, target=NULL, lures=NULL, setsize=NULL, relative=T, parallel=FALSE, chains=4, prior=NULL, ...) {
  if (parallel){
    withr::local_options(list(mc.cores =  parallel::detectCores()))
    if (chains >  parallel::detectCores()) {
      chains <-  parallel::detectCores()
    }
  }

  if (!inherits(formula, 'brmsformula')) {
    stop("The provided formula is not a brms formula.
          Please specify formula with the bf() function. E.g.: bf(y ~ 1, kappa ~ 1, thetat ~ 1")
  }

  # check data
  if (max(abs(data[[formula$resp]])) > 10) {
    data[[formula$resp]] <- data[[formula$resp]]*pi/180
    warning('It appears your response variable is in degrees. We will transform it to radians.')
  }
  data[[formula$resp]] <- wrap(data[[formula$resp]])


  # 2 parameter model
  if (model_type == "2p") {
    ff <- formula +
      brms::lf(kappa2 ~ 1, mu1 ~ 1, mu2 ~ 1) +
      brms::nlf(kappa1 ~ kappa) +
      brms::nlf(theta1 ~ thetat)
    mix_family <- brms::mixture("von_mises", "von_mises", order="none")
    mix_prior <- brms::prior_("constant(0)", class = "Intercept", dpar = "mu1") +    # fix mean of the first von Mises to zero
      brms::prior_("constant(0)", class = "Intercept", dpar = "mu2") +               # fix mean of the second von Mises to zero
      brms::prior_("constant(-100)", class = "Intercept", dpar = "kappa2") +         # fix kappa of the second von Mises to (alomst) zero
      brms::prior_("normal(5.0, 0.8)", class = "b", nlpar = "kappa") +
      brms::prior_("logistic(0, 1)", class = "b", nlpar = "thetat")


    # 3 parameter model
  } else if (model_type == "3p") {
    if (is.null(setsize)) {
      stop(paste0("Argument 'setsize' is not specified. For a 3 parameter mixture model, ",
                  "please set the 'setsize' argument either to a number, if the setsize ",
                  "is fixed, or to the name of the variable containing the setsize, ",
                  "if the setsize varies in your dataset"))

    } else if (is.character(setsize)) {
      # Variable setsize
      ss_numeric <- as.numeric(as.character(data[[setsize]]))
      max_setsize <- max(ss_numeric)

    } else if (is.numeric(setsize)) {
      # Fixed setsize
      ss_numeric <- rep(setsize, times=nrow(data))
      max_setsize <- setsize
    }

    if (length(lures) < max_setsize-1) {
      stop(paste0('The number of columns for non-target values in the argument',
                  '`lures` is less than max(setsize)-1'))
    } else if (length(lures) > max_setsize-1) {
      stop(paste0('The number of columns for non-target values in the argument',
                  '`lures` is more than max(setsize)-1'))
    }

    # create index variables for lures and correction variable for theta due to setsize
    lure_idx_vars <- paste0('LureIdx',1:(max_setsize-1))
    for(i in 1:(max_setsize-1)) {
      data[[lure_idx_vars[i]]] <- ifelse(ss_numeric >= (i+1), 1, 0)
    }
    data$inv_ss = 1/(ss_numeric-1)
    data$inv_ss = ifelse(is.infinite(data$inv_ss), 1, data$inv_ss)
    data[,lures][is.na(data[,lures])] <- 0

    # names for parameters
    kappa_nts <- paste0('kappa', 2:max_setsize)
    kappa_unif <- paste0('kappa',max_setsize+1)
    theta_nts <- paste0('theta',2:max_setsize)
    mu_nts <- paste0('mu', 2:max_setsize)
    mu_unif <- paste0('mu', max_setsize+1)

    # construct formula
    ff <- formula +
      brms::lf(mu1 ~ 1,
         stats::as.formula(paste0(kappa_unif,' ~ 1')),
         stats::as.formula(paste0(mu_unif, ' ~ 1'))) +
      brms::nlf(theta1 ~ thetat) +
      brms::nlf(kappa1 ~ kappa)
    for (i in 1:(max_setsize-1)) {
      ff <- ff +
        brms::nlf(stats::as.formula(paste0(kappa_nts[i], ' ~ kappa'))) +
        brms::nlf(stats::as.formula(paste0(theta_nts[i], ' ~ ', lure_idx_vars[i], '*(thetant + log(inv_ss)) + (1-', lure_idx_vars[i], ')*(-100)'))) +
        brms::nlf(stats::as.formula(paste0(mu_nts[i], ' ~ ', lures[i])))
    }

    # define mixture family
    vm_list = lapply(1:(max_setsize+1), function(x) brms::von_mises(link="identity"))
    vm_list$order = "none"
    mix_family <- brms::do_call(brms::mixture, vm_list)

    # define prior
    mix_prior <-
      brms::prior_("constant(0)", class = "Intercept", dpar = "mu1") +
      brms::prior_("constant(0)", class = "Intercept", dpar = mu_unif) +
      brms::prior_("constant(-100)", class = "Intercept", dpar = kappa_unif) +
      brms::prior_("normal(5.0, 0.8)", class = "b", nlpar = "kappa") +
      brms::prior_("logistic(0, 1)", class = "b", nlpar = "thetat") +
      brms::prior_("logistic(0, 1)", class = "b", nlpar = "thetant")

    # if there is setsize 1 in the data, set constant prior over thetant for setsize1
    if (1 %in% ss_numeric) {
      mix_prior <- mix_prior +
        brms::prior_("constant(-100)", class="b", coef=paste0(setsize, 1), nlpar="thetant")
    }

  } else {
    stop(paste0('Invalid value for argument model_type: ', model_type, '\nThe model_type ',
                'should be "2p" for the 2 parameter mixture model or "3p" for the 3 parameter mixture model. '))
  }

  # estimate model
  fit <- brms::brm(
    formula = ff,
    data    = data,
    family  = mix_family,
    prior   = mix_prior+prior,
    ...)

  return(fit)
}

