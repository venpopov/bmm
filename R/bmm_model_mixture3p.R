#############################################################################!
# MODELS                                                                 ####
#############################################################################!

.model_mixture3p <- function(resp_err = NULL, nt_features = NULL, setsize = NULL,
                             regex = FALSE, links = NULL, ...) {

  out <- structure(
    list(
      resp_vars = nlist(resp_err),
      other_vars = nlist(nt_features, setsize),
      domain = "Visual working memory",
      task = "Continuous reproduction",
      name = "Three-parameter mixture model by Bays et al (2009).",
      version = "NA",
      citation = glue(
        "Bays, P. M., Catalao, R. F. G., & Husain, M. (2009). \\
        The precision of visual working memory is set by allocation \\
        of a shared resource. Journal of Vision, 9(10), 1-11"
      ),
      requirements = glue(
        '- The response vairable should be in radians and \\
        represent the angular error relative to the target
        - The non-target features should be in radians and be \\
        centered relative to the target'
      ),
      parameters = list(
        mu1 = glue(
          "Location parameter of the von Mises distribution for memory responses \\
          (in radians). Fixed internally to 0 by default."
        ),
        kappa = "Concentration parameter of the von Mises distribution",
        thetat = "Mixture weight for target responses",
        thetant = "Mixture weight for non-target responses"
      ),
      links = list(
        mu1 = "identity",
        kappa = "log",
        thetat = "identity",
        thetant = "identity"
      ),
      fixed_parameters = list(mu1 = 0),
      void_mu = FALSE
    ),
    # attributes
    regex = regex,
    regex_vars = c('nt_features'),
    class =  c("bmmmodel", "vwm", "nontargets", "mixture3p")
  )
  out$links[names(links)] <- links
  out
}


# user facing alias
#' @title `r .model_mixture3p()$name`
#' @details `r model_info(.model_mixture3p())`
#' @param resp_err The name of the variable in the dataset containing
#'   the response error. The response error should code the response relative to
#'   the to-be-recalled target in radians. You can transform the response error
#'   in degrees to radians using the `deg2rad` function.
#' @param nt_features A character vector with the names of the non-target
#'   feature values. The non_target feature values should be in radians and
#'   centered relative to the target. Alternatively, if regex=TRUE, a regular
#'   expression can be used to match the non-target feature columns in the
#'   dataset.
#' @param setsize Name of the column containing the set size variable (if
#'   setsize varies) or a numeric value for the setsize, if the setsize is
#'   fixed.
#' @param regex Logical. If TRUE, the `nt_features` argument is interpreted as
#'  a regular expression to match the non-target feature columns in the dataset.
#' @param links A list of links for the parameters. *Currently does not affect
#'   the model fits, but it will in the future.*
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
#' ff <- bmmformula(
#'   kappa ~ 1,
#'   thetat ~ 1,
#'   thetant ~ 1
#' )
#'
#' # specify the 3-parameter model with explicit column names for non-target features
#' model1 <- mixture3p(resp_err = "y", nt_features = paste0('nt',1:3,'_loc'), setsize = 4)
#'
#' # fit the model
#' fit <- fit_model(formula = ff,
#'                  data = dat,
#'                  model = model1,
#'                  parallel=T,
#'                  iter = 500,
#'                  backend='cmdstanr')
#'
#' # alternatively specify the 3-parameter model with a regular expression to match non-target features
#' # this is equivalent to the previous call, but more concise
#' model2 <- mixture3p(resp_err = "y", nt_features = "nt.*_loc", setsize = 4, regex = TRUE)
#'
#' # fit the model
#' fit <- fit_model(formula = ff,
#'                  data = dat,
#'                  model = model2,
#'                  parallel=T,
#'                  iter = 500,
#'                  backend='cmdstanr')
#' }
mixture3p <- function(resp_err, nt_features, setsize, regex = FALSE,
                      links = NULL, ...) {
  stop_missing_args()
  .model_mixture3p(resp_err = resp_err, nt_features = nt_features,
                   setsize = setsize, regex = regex, links = links, ...)
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
  nt_features <- model$other_vars$nt_features
  setsize_var <- model$other_vars$setsize

  # construct main brms formula from the bmm formula
  bmm_formula <- formula
  formula <- bmf2bf(model, bmm_formula)

  # additional internal terms for the mixture model formula
  kappa_nts <- paste0('kappa', 2:max_setsize)
  kappa_unif <- paste0('kappa',max_setsize + 1)
  theta_nts <- paste0('theta',2:max_setsize)
  mu_nts <- paste0('mu', 2:max_setsize)
  mu_unif <- paste0('mu', max_setsize + 1)

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
      glue_nlf(mu_nts[i], ' ~ ', nt_features[i])
  }

  # define mixture family
  vm_list = lapply(1:(max_setsize + 1), function(x) brms::von_mises(link="identity"))
  vm_list$order = "none"
  family <- brms::do_call(brms::mixture, vm_list)

  # define prior
  additional_constants <- list()
  additional_constants[[kappa_unif]] <- -100
  additional_constants[[mu_unif]] <- 0
  prior <- fixed_pars_priors(model, additional_constants)
  if (getOption("bmm.default_priors", TRUE)) {
    prior <- prior +
      set_default_prior(bmm_formula, data,
                        prior_list=list(kappa=list(main='normal(2,1)',effects='normal(0,1)', nlpar=T),
                                        thetat=list(main='logistic(0, 1)', nlpar=T),
                                        thetant=list(main='logistic(0, 1)', nlpar=T)))
  }

  # if there is setsize 1 in the data, set constant prior over thetant for setsize1
  thetant_preds <- rhs_vars(bmm_formula$thetant)
  if (any(data$ss_numeric == 1) && !is.numeric(data[[setsize_var]]) && setsize_var %in% thetant_preds) {
    prior <- combine_prior(prior, brms::prior_("constant(-100)", class="b", coef = paste0(setsize_var, 1), nlpar="thetant"))
  }

  nlist(formula, data, family, prior)
}
