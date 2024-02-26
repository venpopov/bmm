# internal function to combine two priors (e.g. the default prior with the user given prior)
# parts present in prior2 will overwrite the corresponding parts in prior1
combine_prior <- function(prior1, prior2) {
  if (!is.null(prior2)) {
    combined_prior <- dplyr::anti_join(prior1, prior2, by=c('class', 'dpar','nlpar','coef','group','resp'))
    prior <- combined_prior + prior2
  } else {
    prior <- prior1
  }
  return(prior)
}


#' @title Get Default priors for Measurement Models specified in BRMS
#' @description Obtain the default priors for a Bayesian multilevel measurement model,
#'   as well as information for which parameters priors can be specified.
#'   Given the `model`, the `data` and the `formula` for the model, this function will return
#'   the default priors that would be used to estimate the model. Additionally, it will
#'   return all model parameters that have no prior specified (flat priors). This can help to
#'   get an idea about which priors need to be specified and also know which priors were
#'   used if no user-specified priors were passed to the [fit_model()] function.
#' @param formula An object of class `bmmformula`. A symbolic description of
#'   the model to be fitted.
#' @param data An object of class data.frame, containing data of all variables
#'   used in the model. The names of the variables must match the variable names
#'   passed to the `bmmmodel` object for required argurments.
#' @param model A description of the model to be fitted. This is a call to a
#'   `bmmmodel` such as `mixture3p()` function. Every model function has a
#'   number of required arguments which need to be specified within the function
#'   call. Call [supported_models()] to see the list of supported models and
#'   their required arguments
#' @param ... Further arguments passed to [brms::get_prior()]. See the
#'   description of [brms::get_prior()] for more details
#'
#' @details `r a= supported_models(); a`
#'
#'   Type `help(package=bmm)` for a full list of available help topics.
#'
#' @returns A data.frame with columns specifying the `prior`, the `class`, the `coef` and `group`
#'    for each of the priors specified. Separate rows contain the information on the
#'    parameters (or parameter classes) for which priors can be specified.
#'
#'
#' @seealso [supported_models()], [brms::get_prior()]
#'
#' @keywords extract_info
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # generate artificial data from the Signal Discrimination Model
#' dat <- data.frame(y = rsdm(n=2000))
#'
#' # define formula
#' ff <- bmf(y ~ 1,
#'           c ~ 1,
#'           kappa ~ 1)
#'
#' # fit the model
#' get_model_prior(formula = ff,
#'                 data = dat,
#'                 model = sdmSimple()
#' )
#' }
#'
get_model_prior <- function(formula, data, model, ...) {
  model <- check_model(model, data)
  data <- check_data(model, data, formula)
  formula <- check_formula(model, data, formula)
  config_args <- configure_model(model, data, formula)

  dots <- list(...)
  prior_args <- c(config_args, dots)
  brms_priors <- brms::do_call(brms::get_prior, prior_args)

  combine_prior(brms_priors, prior_args$prior)
}


#' @title construct constant priors to fix fixed model parameters
#' @param model a `bmmmodel` object
#' @param additional_pars a list of name=value pairs to fix additional
#'   parameters where the name is the parameter name and the value is the fixed
#'   value
#' @details This function is used to fix the parameters of a model that are
#'   specified as fixed in the model definition. It can also be used to fix any
#'   additional internal parameters that are not specified in the model
#'   definition. it should be used in the configure_model.* function for the
#'   model.
#'
#'   the function puts a constant(value) prior on an Intercept with
#'   dpar=parameter_name
#' @return an object of class brmsprior of the form prior("constant(value)",
#'   class="Intercept", dpar=parameter_name) for all fixed parameters in the
#'   model
#' @noRd
fixed_pars_priors <- function(model, additional_pars = list(), class = "Intercept", partype = "dpar") {
  par_list <- c(model$info$fixed_parameters, additional_pars)
  pars <- names(par_list)
  values <- unlist(par_list)
  priors <- glue::glue("constant({values})")
  if(partype == "dpar") {
    brms::set_prior(priors, class = class, dpar = pars)
  } else {
    brms::set_prior(priors, class = class, nlpar = pars)
  }
}


#' Set default priors for a bmmmodel
#'
#' For developers to use within configure_model.* functions. This function
#' allows you to specify default priors flexibly regardless of the formula the
#' user has supplied. The function will automatically recognize when intercepts
#' are present or suppressed, and will set the default priors accordingly. You
#' can specify priors of intercepts/main levels of a predictor with supressed
#' intercept, and priors on the effects of the predictors relative to the
#' intercept.
#'
#' @param bmmformula A `bmmformula` object
#' @param data A data.frame containing the data used in the model
#' @param prior_list A list of lists containing the priors for the parameters.
#'   The list should have the same names as the parameters in the `bmmformula`
#'   for which you want to set the default prior. Each parameter should be
#'   assigned a list of priors. The 'main' entry will be used for the Intercept
#'   or any of the main levels if the intercept is suppressed. The 'effects' entry
#'   is optional. If given, it will be used as the prior for effects of the
#'   predictor relative to the intercept. Finally, an 'nlpar = TRUE' entry can be given
#'   in the list for each predictor, to specify that the prior should be set on
#'   `nlpar` (see `bmm_model_mixture3p.R` for an example). By default the priors
#'   will be set on dpar
#' @examples
#' data <- OberauerLin_2017
#' data$session <- as.factor(data$session)
#' # suppressed intercept on thetat, intercept present for kappa
#' formula <- bmf(thetat ~ 0 + set_size, kappa ~ session)
#' prior_list <- list(thetat = list(main = 'logistic(0,1)', nlpar=TRUE),
#'                    kappa = list(main = 'normal(2,1)', effects = 'normal(0,1)', nlpar=TRUE))
#' prior <- set_default_prior(formula, data, prior_list)
#' print(prior)
#'
#' # suppressed intercept on both thetat and kappa
#' formula <- bmf(thetat ~ 0 + set_size, kappa ~ 0 + session)
#' prior_list <- list(thetat = list(main = 'logistic(0,1)', nlpar=TRUE),
#'                    kappa = list(main = 'normal(2,1)', effects = 'normal(0,1)', nlpar=TRUE))
#' prior <- set_default_prior(formula, data, prior_list)
#' print(prior)
#'
#' # suppressed intercept on both thetat and kappa, with interaction for kappa
#' formula <- bmf(thetat ~ 0 + set_size, kappa ~ 0 + set_size*session)
#' prior_list <- list(thetat = list(main = 'logistic(0,1)', nlpar=TRUE),
#'                    kappa = list(main = 'normal(2,1)', effects = 'normal(0,1)', nlpar=TRUE))
#' prior <- set_default_prior(formula, data, prior_list)
#' print(prior)
#' @export
#' @keywords internal, developer
set_default_prior <- function(bmmformula, data, prior_list) {
  dpars <- names(bmmformula)
  pars_key <- names(prior_list)
  prior <- brms::empty_prior()
  if (any(not_in(pars_key, dpars))) {
    stop("You are trying to set a default prior on a parameter that is not part of the model")
  }
  if (!is.list(prior_list)) {
    stop("The prior_list should be a list of lists")
  }
  for (i in 1:length(prior_list)) {
    if(!is.list(prior_list[[i]])) {
      stop("The prior_list should be a list of lists")
    }
  }

  pars <- dpars[dpars %in% pars_key]
  is_nlpar <- sapply(prior_list[pars], function(x) {isTRUE(x$nlpar)})
  for (par in pars) {
    bform <- bmmformula[[par]]
    bterms <- stats::terms(bform)
    prior_desc <- prior_list[[par]]
    has_effects_prior <- !is.null(prior_desc$effects)

    all_rhs_names <- rhs_vars(bform)
    all_rhs_terms <- attr(bterms, "term.labels")
    fixef <- all_rhs_terms[all_rhs_terms %in% all_rhs_names]
    inter <- all_rhs_terms[attr(bterms,'order') > 1]
    nfixef <- length(fixef)
    ninter <- length(inter)
    interaction_only <- nfixef == 0 && ninter > 0

    ## if the user has specified a non-linear predictor on a model parameter, do not set prior
    if (any(all_rhs_names %in% dpars)) {
      next
    }

    # # by default set the effects prior on the class 'b'. The intercept can be overwritten later
    if (has_effects_prior && nfixef > 0) {
      if (is_nlpar[par]) {
        prior <- combine_prior(prior, brms::prior_(prior_desc$effects, class = "b", nlpar = par))
      } else {
        prior <- combine_prior(prior, brms::prior_(prior_desc$effects, class = "b", dpar = par))
      }
    }

    # check if intercept is present and set prior_desc[[1]] on the intercept
    if (attr(bterms, "intercept")) {
      if (is_nlpar[par]) {
        prior <- combine_prior(prior, brms::prior_(prior_desc$main, class = "b", coef = "Intercept", nlpar = par))
      } else {
        prior <- combine_prior(prior, brms::prior_(prior_desc$main, class = "Intercept", dpar = par))
      }
      next
    }

    # next check if there is only one predictor, in which case set the main prior on all levels
    # same if there are multiple predictors, but they are specified only as an interaction
    # get individual predictors, and the formula terms. Fixed effects are those that match
    if ((nfixef == 1 && ninter == 0) || interaction_only) {
      if (is_nlpar[par]) {
        prior <- combine_prior(prior, brms::prior_(prior_desc[[1]], class = "b", nlpar = par))
      } else {
        prior <- combine_prior(prior, brms::prior_(prior_desc[[1]], class = "b", dpar = par))
      }
      next
    }

    # if there are multiple predictors, set the main prior on the levels of the first predictor
    first_term <- attr(bterms,"term.labels")[1]
    levels <- levels(data[[first_term]])
    coefs <- paste0(first_term, levels)
    for (coef in coefs) {
      if (is_nlpar[par]) {
        prior <- combine_prior(prior, brms::prior_(prior_desc[[1]], class = "b", coef = coef, nlpar = par))
      } else {
        prior <- combine_prior(prior, brms::prior_(prior_desc[[1]], class = "b", coef = coef, dpar = par))
      }
    }
  }
  prior
}
