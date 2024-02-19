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
  model <- check_model(model)
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
fixed_pars_priors <- function(model, additional_pars = list()) {
  par_list <- c(model$info$fixed_parameters, additional_pars)
  pars <- names(par_list)
  values <- unlist(par_list)
  priors <- glue::glue("constant({values})")
  brms::set_prior(priors, class = "Intercept", dpar = pars)
}

