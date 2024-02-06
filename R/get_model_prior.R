#' @title Get Default priors for Measurement Models specified in BRMS
#' @description Obtain the default priors for a Bayesian multilevel measurement model,
#'   as well as information for which parameters priors can be specified.
#'   Given the `model`, the `data` and the `formula` for the model, this function will return
#'   the default priors that would be used to estimate the model. Additionally, it will
#'   return all model parameters that have no prior specified (flat priors). This can help to
#'   get an idea about which priors need to be specified and also know which priors were
#'   used if no user-specified priors were passed to the [fit_model()] function.
#' @param formula An object of class `brmsformula`. A symbolic description of
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
#' @export
#'
#' @examples
#' \dontrun{
#' # generate artificial data from the Bays et al (2009) 3-parameter mixture model
#' dat <- gen_3p_data(N=2000, pmem=0.6, pnt=0.3, kappa=10, setsize=4, relative_resp=T)
#'
#' # define formula
#' ff <- brms::bf(y ~ 1,
#'                kappa ~ 1,
#'                thetat ~ 1,
#'                thetant ~ 1)
#'
#' # simulate data
#' dat <- gen_3p_data(N = 200)
#'
#' # fit the model
#' get_model_prior(formula = ff,
#'                 data = dat,
#'                 model = mixture3p(non_targets = paste0('nt',1,'_loc'), setsize = 2)
#' )
#' }
#'
get_model_prior <- function(formula, data, model, ...) {

  # check model, formula and data, and transform data if necessary
  model <- check_model(model)
  formula <- check_formula(model, formula)
  data <- check_data(model, data, formula)

  # generate the model specification to pass to brms later
  config_args <- configure_model(model, data, formula)

  # get priors for the model
  dots <- list(...)
  prior_args <- c(config_args, dots)
  brms_priors <- brms::do_call(brms::get_prior, prior_args)

  # combine the brms prior with the model default prior
  combined_prior <- combine_prior(brms_priors, prior_args$prior)

  return(combined_prior)
}

