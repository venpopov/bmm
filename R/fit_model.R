#' @title Fit Measurement Models using BRMS
#' @description Fit a Bayesian multilevel measurement model. Currently
#'   implemented are the two-parameter mixture model by Zhang and Luck (2008),
#'   the three-parameter mixture model by Bays et al (2009), and three different
#'   versions of the Interference Measurement Model (Oberauer et al., 2017).
#'   This is a wrapper function for [brms::brm], which is used to estimate the
#'   model.
#'
#' @param formula An object of class `bmmformula`. A symbolic description of the
#'   model to be fitted.
#' @param data An object of class data.frame, containing data of all variables
#'   used in the model. The names of the variables must match the variable names
#'   passed to the `bmmmodel` object for required argurments.
#' @param model A description of the model to be fitted. This is a call to a
#'   `bmmmodel` such as `mixture3p()` function. Every model function has a
#'   number of required arguments which need to be specified within the function
#'   call. Call [supported_models()] to see the list of supported models and
#'   their required arguments
#' @param parallel Logical; If TRUE, the number of cores on your machine will be
#'   detected and brms will fit max(chains, cores) number of chains (specified
#'   by the `chain` argument) in parallel using the parallel package
#' @param chains Numeric. Number of Markov chains (defaults to 4)
#' @param prior One or more `brmsprior` objects created by [brms::set_prior()]
#'   or related functions and combined using the c method or the + operator. See
#'   also [get_model_prior()] for more help. Not necessary for the default model
#'   fitting, but you can provide prior constraints to model parameters
#' @param sort_data Logical. If TRUE, the data will be sorted by the predictor
#'   variables for faster sampling. If FALSE, the data will not be sorted, but
#'   sampling will be slower. If NULL (the default), `fit_model()` will check if
#'   the data is sorted, and ask you via a console prompt if it should be
#'   sorted. You can set the default value for this option using global
#'   `options(bmm.sort_data = TRUE/FALSE)`
#' @param silent Verbosity level between 0 and 2. If 1 (the default), most of the
#'   informational messages of compiler and sampler are suppressed. If 2, even
#'   more messages are suppressed. The actual sampling progress is still
#'   printed. Set refresh = 0 to turn this off as well. If using backend =
#'   "rstan" you can also set open_progress = FALSE to prevent opening
#'   additional progress bars.
#' @param ... Further arguments passed to [brms::brm()] or Stan. See the
#'   description of [brms::brm()] for more details
#'
#' @details `r a= supported_models(); a`
#'
#'   Type `help(package=bmm)` for a full list of available help topics.
#'
#' @returns An object of class brmsfit which contains the posterior draws along
#'   with many other useful information about the model. Use methods(class =
#'   "brmsfit") for an overview on available methods.
#'
#' @references Frischkorn, G. T., & Popov, V. (2023). A tutorial for estimating
#'   mixture models for visual working memory tasks in brms: Introducing the
#'   Bayesian Measurement Modeling  (bmm) package for R.
#'   https://doi.org/10.31234/osf.io/umt57
#'
#' @seealso [supported_models()], [brms::brm()]
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # generate artificial data from the Signal Discrimination Model
#' dat <- data.frame(y=rsdm(n=2000))
#'
#' # define formula
#' ff <- bmmformula(c ~ 1,
#'                  kappa ~ 1)
#'
#' # fit the model
#' fit <- fit_model(formula = ff,
#'                  data = dat,
#'                  model = sdmSimple(resp_err = "y"),
#'                  parallel=T,
#'                  iter=500,
#'                  backend='cmdstanr')
#' }
#'
fit_model <- function(formula, data, model, parallel = FALSE, chains = 4,
                      prior = NULL, sort_data = getOption('bmm.sort_data', NULL), 
                      silent = getOption('bmm.silent', 1), ...) {
  # warning for using old version
  dots <- list(...)
  if ("model_type" %in% names(dots)) {
    stop('The "model_type" argument was deprecated on Feb 3, 2024. Either:
         - See ?fit_model for the new usage;
         - or install the old version of the package with: devtools::install_github("venpopov/bmm@v0.0.1")')
  }

  # set temporary global options and return modified arguments for brms
  opts <- configure_options(nlist(parallel, chains, sort_data, silent))

  # check model, formula and data, and transform data if necessary
  model <- check_model(model)
  formula <- check_formula(model, formula)
  data <- check_data(model, data, formula)

  # generate the model specification to pass to brms later
  config_args <- configure_model(model, data, formula)

  # combine the default prior plus user given prior
  config_args$prior <- combine_prior(config_args$prior, prior)

  # estimate the model
  dots <- list(...)
  fit_args <- combine_args(nlist(config_args, opts, dots))
  fit <- call_brm(fit_args)

  # model postprocessing
  postprocess_brm(model, fit)
}



#' @export
update.bmmfit <- function(fit, ...) {
  stop("The update method for bmmfit is not yet implemented, but is planned for a future release.", call. = FALSE)
  # do any necessary preprocessing to accomondate bmm changes into brms
  # ....

  fit = NextMethod(fit)  # pass back to brms::update.brmsfit

  # do any necessary postprocessing to convert back to bmmfit
  # ....
  class(fit) <- c('bmmfit', class(fit))  # reassing class
  return(fit)
}

