#############################################################################!
# MODELS                                                                 ####
#############################################################################!

.model_IMMabc <-
  function(resp_err = NULL,
           nt_features = NULL,
           setsize = NULL,
           regex = FALSE,
           links = NULL,
           ...) {
    out <- structure(
      list(
        resp_vars = nlist(resp_err),
        other_vars = nlist(nt_features, setsize),
        domain = "Visual working memory",
        task = "Continuous reproduction",
        name = "Interference measurement model by Oberauer and Lin (2017).",
        version = "abc",
        citation = glue(
          "Oberauer, K., & Lin, H.Y. (2017). An interference model \\
          of visual working memory. Psychological Review, 124(1), 21-59"
        ),
        requirements = glue(
          '- The response vairable should be in radians and \\
          represent the angular error relative to the target
          - The non-target features should be in radians and be \\
          centered relative to the target'
        ),
        parameters = list(
          mu1 = glue(
            "Location parameter of the von Mises distribution for memory \\
            responses (in radians). Fixed internally to 0 by default."
          ),
          kappa = "Concentration parameter of the von Mises distribution",
          a = "General activation of memory items",
          c = "Context activation"
        ),
        links = list(kappa = "log",
                     a = "identity",
                     c = "identity"),
        fixed_parameters = list(mu1 = 0),
        void_mu = FALSE
      ),
      # attributes
      regex = regex,
      regex_vars = c('nt_features'),
      class = c("bmmmodel", "vwm", "nontargets", "IMMabc")
    )
    out$links[names(links)] <- links
    out
}


.model_IMMbsc <-
  function(resp_err = NULL,
           nt_features = NULL,
           nt_distances = NULL,
           setsize = NULL,
           regex = FALSE,
           links = NULL,
           ...) {
    out <- structure(
      list(
        resp_vars = nlist(resp_err),
        other_vars = nlist(nt_features, nt_distances, setsize),
        domain = "Visual working memory",
        task = "Continuous reproduction",
        name = "Interference measurement model by Oberauer and Lin (2017).",
        version = "bsc",
        citation = glue(
          "Oberauer, K., & Lin, H.Y. (2017). An interference model \\
          of visual working memory. Psychological Review, 124(1), 21-59"
        ),
        requirements = glue(
          '- The response vairable should be in radians and \\
          represent the angular error relative to the target
          - The non-target features should be in radians and be \\
          centered relative to the target'
        ),
        parameters = list(
          mu1 = glue(
            "Location parameter of the von Mises distribution for memory \\
            responses (in radians). Fixed internally to 0 by default."
          ),
          kappa = "Concentration parameter of the von Mises distribution",
          c = "Context activation",
          s = "Spatial similarity gradient"
        ),
        links = list(kappa = "log",
                     c = "identity",
                     s = "log"),
        fixed_parameters = list(mu1 = 0),
        void_mu = FALSE
      ),
      # attributes
      regex = regex,
      regex_vars = c('nt_features', 'nt_distances'),
      class = c("bmmmodel", "vwm", "nontargets", "IMMspatial", "IMMbsc")
    )
  out$links[names(links)] <- links
  out
}

.model_IMMfull <-
  function(resp_err = NULL,
           nt_features = NULL,
           nt_distances = NULL,
           setsize = NULL,
           regex = FALSE,
           links = NULL,
           ...) {
    out <- structure(
      list(
        resp_vars = nlist(resp_err),
        other_vars = nlist(nt_features, nt_distances, setsize),
        domain = "Visual working memory",
        task = "Continuous reproduction",
        name = "Interference measurement model by Oberauer and Lin (2017).",
        version = "full",
        citation = glue(
          "Oberauer, K., & Lin, H.Y. (2017). An interference model \\
          of visual working memory. Psychological Review, 124(1), 21-59"
        ),
        requirements = glue(
          '- The response vairable should be in radians and \\
          represent the angular error relative to the target
          - The non-target features should be in radians and be \\
          centered relative to the target'
        ),
        parameters = list(
          mu1 = glue(
            "Location parameter of the von Mises distribution for memory \\
            responses (in radians). Fixed internally to 0 by default."
          ),
          kappa = "Concentration parameter of the von Mises distribution",
          a = "General activation of memory items",
          c = "Context activation",
          s = "Spatial similarity gradient"
        ),
        links = list(
          kappa = "log",
          a = "identity",
          c = "identity",
          s = "log"
        ),
        fixed_parameters = list(mu1 = 0),
        void_mu = FALSE
      ),
      # attributes
      regex = regex,
      regex_vars = c('nt_features', 'nt_distances'),
      class = c("bmmmodel", "vwm", "nontargets", "IMMspatial", "IMMfull")
    )
    out$links[names(links)] <- links
    out
  }

# user facing alias

#' @title `r .model_IMMfull()$name`
#' @name IMM
#' @details `r model_info(.model_IMMfull(), components =c('domain', 'task', 'name', 'citation'))`
#' #### Version: `IMMfull`
#' `r model_info(.model_IMMfull(), components =c('requirements', 'parameters', 'fixed_parameters'))`
#' #### Version: `IMMbsc`
#' `r model_info(.model_IMMbsc(), components =c('requirements', 'parameters', 'fixed_parameters'))`
#' #### Version: `IMMabc`
#' `r model_info(.model_IMMabc(), components =c('requirements', 'parameters', 'fixed_parameters'))`
#'
#' Additionally, all IMM models have an internal parameter that is fixed to 0 to
#' allow the model to be identifiable. This parameter is not estimated and is not
#' included in the model formula. The parameter is:
#'
#'   - b = "Background activation (internally fixed to 0)"
#'
#' @param resp_err The name of the variable in the provided dataset containing
#'   the response error. The response Error should code the response relative to
#'   the to-be-recalled target in radians. You can transform the response error
#'   in degrees to radian using the `deg2rad` function.
#' @param nt_features A character vector with the names of the non-target
#'   variables. The non_target variables should be in radians and be centered
#'   relative to the target. Alternatively, if regex=TRUE, a regular
#'   expression can be used to match the non-target feature columns in the
#'   dataset.
#' @param nt_distances A vector of names of the columns containing the distances
#'   of non-target items to the target item. Alternatively, if regex=TRUE, a regular
#'   expression can be used to match the non-target distances columns in the
#'   dataset. Only necessary for the `IMMbsc` and `IMMfull` models.
#' @param setsize Name of the column containing the set size variable (if
#'   setsize varies) or a numeric value for the setsize, if the setsize is
#'   fixed.
#' @param regex Logical. If TRUE, the `nt_features` and `nt_distances` arguments
#'   are interpreted as a regular expression to match the non-target feature
#'   columns in the dataset.
#' @param links A list of links for the parameters. *Currently does not affect
#'   the model fits, but it will in the future.*
#' @param ... used internally for testing, ignore it
#' @return An object of class `bmmmodel`
#' @keywords bmmmodel
#' @examples
#' \dontrun{
#' # load data
#' data <- OberauerLin_2017
#'
#' # define formula
#' ff <- bmmformula(
#'   kappa ~ 0 + set_size,
#'   c ~ 0 + set_size,
#'   a ~ 0 + set_size,
#'   s ~ 0 + set_size
#' )
#'
#' # specify the full IMM model with explicit column names for non-target features and distances
#' model1 <- IMMfull(resp_err = "dev_rad",
#'                   nt_features = paste0('col_nt',1:7),
#'                   nt_distances = paste0('dist_nt',1:7),
#'                   setsize = 'set_size')
#'
#' # fit the model
#' fit <- fit_model(formula = ff,
#'                  data = data,
#'                  model = model1,
#'                  parallel = T,
#'                  iter = 500,
#'                  backend = 'cmdstanr')
#'
#' # alternatively specify the IMM model with a regular expression to match non-target features
#' # this is equivalent to the previous call, but more concise
#' model2 <- IMMfull(resp_err = "dev_rad",
#'                   nt_features = 'col_nt',
#'                   nt_distances = 'dist_nt',
#'                   setsize = 'set_size',
#'                   regex = TRUE)
#'
#' # fit the model
#' fit <- fit_model(formula = ff,
#'                  data = data,
#'                  model = model2,
#'                  parallel=T,
#'                  iter = 500,
#'                  backend='cmdstanr')
#'}
#' @export
IMMfull <- function(resp_err, nt_features, nt_distances, setsize, regex = FALSE,
                    links = NULL,
                    ...) {
  stop_missing_args()
  .model_IMMfull(resp_err = resp_err, nt_features = nt_features,
                 nt_distances = nt_distances, setsize = setsize, regex = regex,
                 links = links, ...)
}

#' @rdname IMM
#' @keywords bmmmodel
#' @export
IMMbsc <- function(resp_err, nt_features, nt_distances, setsize, regex = FALSE,
                   links = NULL, ...) {
  stop_missing_args()
  .model_IMMbsc(resp_err = resp_err, nt_features = nt_features,
                nt_distances = nt_distances, setsize = setsize, regex = regex,
                links = links, ...)
}

#' @rdname IMM
#' @keywords bmmmodel
#' @export
IMMabc <- function(resp_err, nt_features, setsize, regex = FALSE, links = NULL,
                   ...) {
  stop_missing_args()
  .model_IMMabc(resp_err = resp_err, nt_features = nt_features,
                setsize = setsize, regex = regex, links = links, ...)
}

#############################################################################!
# CHECK_DATA S3 methods                                                  ####
#############################################################################!
# A check_data.* function should be defined for each class of the model.
# If a model shares methods with other models, the shared methods should be
# defined in data-helpers.R. Put here only the methods that are specific to
# the model. See ?check_data for details

#' @export
check_data.IMMspatial <- function(model, data, formula) {
  nt_distances <- model$other_vars$nt_distances
  max_setsize <- attr(data, 'max_setsize')

  if (!isTRUE(all.equal(length(nt_distances), max_setsize - 1))) {
    stop("The number of columns for non-target distances in the argument ",
         "'nt_distances' should equal max(setsize)-1")
  }

  if (any(data[,nt_distances] < 0)) {
    stop('All non-target distances to the target need to be postive.')
  }

  NextMethod("check_data")
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
    brms::nlf(theta1 ~ c + a) +
    brms::nlf(kappa1 ~ kappa)

  for (i in 1:(max_setsize - 1)) {
    formula <- formula +
      glue_nlf(kappa_nts[i], ' ~ kappa') +
      glue_nlf(theta_nts[i], ' ~ ', lure_idx_vars[i], '*(a) + ',
               '(1-', lure_idx_vars[i], ')*(-100)') +
      glue_nlf(mu_nts[i], ' ~ ', nt_features[i])
  }

  # define mixture family
  vm_list = lapply(1:(max_setsize + 1), function(x) brms::von_mises(link = "identity"))
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
                        prior_list=list(kappa=list(main = 'normal(2,1)', effects = 'normal(0,1)', nlpar=T),
                                        a=list(main = 'normal(0,1)', effects = 'normal(0,1)', nlpar=T),
                                        c=list(main = 'normal(0,1)', effects = 'normal(0,1)', nlpar=T)))
  }

  # if there is setsize 1 in the data, set constant prior over a for setsize1
  a_preds <- rhs_vars(bmm_formula$a)
  if (any(data$ss_numeric == 1) && !is.numeric(data[[setsize_var]]) && setsize_var %in% a_preds) {
    prior <- combine_prior(prior, brms::prior_("constant(0)", class="b", coef = paste0(setsize_var, 1), nlpar="a"))
  }

  nlist(formula, data, family, prior)
}

#' @export
configure_model.IMMbsc <- function(model, data, formula) {
  # retrieve arguments from the data check
  max_setsize <- attr(data, 'max_setsize')
  lure_idx_vars <- attr(data, "lure_idx_vars")
  nt_features <- model$other_vars$nt_features
  setsize_var <- model$other_vars$setsize
  nt_distances <- model$other_vars$nt_distances

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
    brms::nlf(theta1 ~ c) +
    brms::nlf(kappa1 ~ kappa) +
    brms::nlf(expS ~ exp(s))

  for (i in 1:(max_setsize - 1)) {
    formula <- formula +
      glue_nlf(kappa_nts[i], ' ~ kappa') +
      glue_nlf(theta_nts[i], ' ~ ', lure_idx_vars[i], '*(exp(-expS*',nt_distances[i],')*c) + ',
               '(1-', lure_idx_vars[i], ')*(-100)') +
      glue_nlf(mu_nts[i], ' ~ ', nt_features[i])
  }

  # define mixture family
  vm_list = lapply(1:(max_setsize + 1), function(x) brms::von_mises(link = "identity"))
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
                                        c=list(main='normal(0,1)',effects='normal(0,1)', nlpar=T),
                                        s=list(main='normal(0,1)',effects='normal(0,1)', nlpar=T)))
  }

  # if there is setsize 1 in the data, set constant prior over s for setsize1
  s_preds <- rhs_vars(bmm_formula$s)
  if (any(data$ss_numeric == 1) && !is.numeric(data[[setsize_var]]) && setsize_var %in% s_preds) {
    prior <- combine_prior(prior, brms::prior_("constant(0)", class="b", coef = paste0(setsize_var, 1), nlpar="s"))
  }

  nlist(formula, data, family, prior)
}

#' @export
configure_model.IMMfull <- function(model, data, formula) {
  # retrieve arguments from the data check
  max_setsize <- attr(data, 'max_setsize')
  lure_idx_vars <- attr(data, "lure_idx_vars")
  nt_features <- model$other_vars$nt_features
  setsize_var <- model$other_vars$setsize
  nt_distances <- model$other_vars$nt_distances

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
    brms::nlf(theta1 ~ c + a) +
    brms::nlf(kappa1 ~ kappa) +
    brms::nlf(expS ~ exp(s))

  for (i in 1:(max_setsize - 1)) {
    formula <- formula +
      glue_nlf(kappa_nts[i], ' ~ kappa') +
      glue_nlf(theta_nts[i], ' ~ ', lure_idx_vars[i], '*(exp(-expS*',nt_distances[i],')*c + a) + ',
               '(1-', lure_idx_vars[i], ')*(-100)') +
      glue_nlf(mu_nts[i], ' ~ ', nt_features[i])
  }

  # define mixture family
  vm_list = lapply(1:(max_setsize + 1), function(x) brms::von_mises(link = "identity"))
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
                                        a=list(main='normal(0,1)',effects='normal(0,1)', nlpar=T),
                                        c=list(main='normal(0,1)',effects='normal(0,1)', nlpar=T),
                                        s=list(main='normal(0,1)',effects='normal(0,1)', nlpar=T)))
  }

  # if there is setsize 1 in the data, set constant prior over a and s for setsize1
  a_preds <- rhs_vars(bmm_formula$a)
  s_preds <- rhs_vars(bmm_formula$s)
  if (any(data$ss_numeric == 1) && !is.numeric(data[[setsize_var]])) {
    if (setsize_var %in% a_preds) {
      prior <- combine_prior(prior, brms::prior_("constant(0)", class="b", coef = paste0(setsize_var, 1), nlpar="a"))
    }
    if (setsize_var %in% s_preds) {
      prior <- combine_prior(prior, brms::prior_("constant(0)", class="b", coef = paste0(setsize_var, 1), nlpar="s"))
    }
  }

  nlist(formula, data, family, prior)
}
