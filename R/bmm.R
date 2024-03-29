#' @title Fit Bayesian Measurement Models
#' @description Fit a Bayesian measurement model using **brms** as a
#'  backend interface to Stan.
#'
#' @name bmm
#'
#' @param formula An object of class `bmmformula`. A symbolic description of the
#'   model to be fitted.
#' @param data An object of class data.frame, containing data of all variables
#'   used in the model. The names of the variables must match the variable names
#'   passed to the `bmmodel` object for required argurments.
#' @param model A description of the model to be fitted. This is a call to a
#'   `bmmodel` such as `mixture3p()` function. Every model function has a
#'   number of required arguments which need to be specified within the function
#'   call. Call [supported_models()] to see the list of supported models and
#'   their required arguments
#' @param prior One or more `brmsprior` objects created by [brms::set_prior()]
#'   or related functions and combined using the c method or the + operator. See
#'   also [default_prior()] for more help. Not necessary for the default model
#'   fitting, but you can provide prior constraints to model parameters
#' @param sort_data Logical. If TRUE, the data will be sorted by the predictor
#'   variables for faster sampling. If FALSE, the data will not be sorted, but
#'   sampling will be slower. If "check" (the default), [bmm()] will check if
#'   the data is sorted, and ask you via a console prompt if it should be
#'   sorted. You can set the default value for this option using global
#'   `options(bmm.sort_data = TRUE/FALSE/"check)`)` or via `bmm_options(sort_data)`
#' @param silent Verbosity level between 0 and 2. If 1 (the default), most of the
#'   informational messages of compiler and sampler are suppressed. If 2, even
#'   more messages are suppressed. The actual sampling progress is still
#'   printed. Set refresh = 0 to turn this off as well. If using backend =
#'   "rstan" you can also set open_progress = FALSE to prevent opening
#'   additional progress bars.
#' @param backend Character. The backend to use for fitting the model. Can be
#'  "rstan" or "cmdstanr". If NULL (the default), "cmdstanr" will be used if
#'  the cmdstanr package is installed, otherwise "rstan" will be used. You can
#'  set the default backend using global `options(brms.backend = "rstan"/"cmdstanr")`
#' @param file Either `NULL` or a character string. If a string, the fitted
#'   model object is saved via [saveRDS] in a file named after the string. The
#'   `.rds extension is added automatically. If the file already exists, `bmm`
#'   will load and return the saved model object. Unless you specify the
#'   `file_refit` argument as well, the existing files won't be overwritten, you
#'   have to manually remove the file in order to refit and save the model under
#'   an existing file name. The file name is stored in the `bmmfit` object for
#'   later usage. If the directory of the file does not exist, it will be created.
#' @param file_compress Logical or a character string, specifying one of the
#'   compression algorithms supported by [saveRDS] when saving
#'   the fitted model object.
#' @param file_refit Logical. Modifies when the fit stored via the `file` argument is
#'   re-used. Can be set globally for the current \R session via the
#'   `"bmm.file_refit"` option (see [options]). If `TRUE` (the default), the
#'   model is re-used if the file exists. If `FALSE`, the model is re-fitted. Note
#'   that unlike `brms`, there is no "on_change" option
#' @param ... Further arguments passed to [brms::brm()] or Stan. See the
#'   description of [brms::brm()] for more details
#'
#' @details # Supported Models
#'
#'   `r a= supported_models(); a`
#'
#'   # bmmformula syntax
#'
#'   see `vignette("bmm_bmmformula", package = "bmm")` for a detailed description of the syntax and how
#'   it differs from the syntax for **brmsformula**
#'
#'   # Default priors, Stan code and Stan data
#'
#'   For more information about the default priors in **bmm** and about who to extract the Stan code and data generated by bmm and #'   brms, see `vignette("bmm_extract_info", package = "bmm")`.
#'
#'   # Miscellaneous
#'
#'   Type `help(package=bmm)` for a full list of available help topics.
#'
#'   **fit_model()** is a deprecated alias for **bmm()**.
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
#' @seealso [supported_models()], [brms::brm()], [default_prior()][default_prior.bmmformula()], [bmmformula()], [stancode()][stancode.bmmformula()], [standata()][standata.bmmformula()]
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # generate artificial data from the Signal Discrimination Model
#' dat <- data.frame(y = rsdm(2000))
#'
#' # define formula
#' ff <- bmmformula(c ~ 1, kappa ~ 1)
#'
#' # fit the model
#' fit <- bmm(formula = ff,
#'            data = dat,
#'            model = sdm(resp_error = "y"),
#'            cores = 4,
#'            backend = 'cmdstanr')
#' }
#'
bmm <- function(formula, data, model,
                prior = NULL,
                sort_data = getOption('bmm.sort_data', "check"),
                silent = getOption('bmm.silent', 1),
                backend = getOption('brms.backend', NULL),
                file = NULL, file_compress = TRUE,
                file_refit = getOption('bmm.file_refit', FALSE), ...) {
  deprecated_args(...)
  dots <- list(...)

  x <- read_bmmfit(file, file_refit)
  if (!is.null(x)) return(x)

  # set temporary global options and return modified arguments for brms
  configure_opts <- nlist(sort_data, silent, backend, parallel = dots$parallel,
                          cores = dots$cores)
  opts <- configure_options(configure_opts)
  dots$parallel <- NULL

  # check model, formula and data, and transform data if necessary
  user_formula <- formula
  model <- check_model(model, data, formula)
  data <- check_data(model, data, formula)
  formula <- check_formula(model, data, formula)

  # generate the model specification to pass to brms later
  config_args <- configure_model(model, data, formula)

  # configure the default prior and combine with user-specified prior
  prior <- configure_prior(model, data, config_args$formula, prior)

  # estimate the model
  fit_args <- combine_args(nlist(config_args, opts, dots, prior))
  fit <- call_brm(fit_args)

  # model post-processing
  fit <- postprocess_brm(model, fit, fit_args = fit_args, user_formula = user_formula,
                         configure_opts = configure_opts)

  # save the fitted model object if !is.null
  save_bmmfit(fit, file, compress = file_compress)
}


#' @rdname bmm
#' @export
fit_model <- function(formula, data, model,
                      prior = NULL,
                      sort_data = getOption('bmm.sort_data', "check"),
                      silent = getOption('bmm.silent', 1),
                      backend = getOption('brms.backend', NULL),
                      ...) {
  message("You are using the deprecated `fit_model()` function. Please use `bmm()` instead.")
  bmm(formula = formula, data = data, model = model, prior = prior,
      sort_data = sort_data, silent = silent, backend = backend, ...)
}
