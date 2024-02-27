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
#'   sampling will be slower. If "check" (the default), `fit_model()` will check if
#'   the data is sorted, and ask you via a console prompt if it should be
#'   sorted. You can set the default value for this option using global
#'   `options(bmm.sort_data = TRUE/FALSE/"check)`)` or via `bmm_options(sort_data)`
#' @param silent Verbosity level between 0 and 2. If 1 (the default), most of the
#'   informational messages of compiler and sampler are suppressed. If 2, even
#'   more messages are suppressed. The actual sampling progress is still
#'   printed. Set refresh = 0 to turn this off as well. If using backend =
#'   "rstan" you can also set open_progress = FALSE to prevent opening
#'   additional progress bars.
#' @param checkpoint_every Experimental. The number of iterations per checkpoint
#'   at which to save intermediary sampling results. Note that \code{iter} is
#'   divided by \code{checkpoint_every} to determine the number of checkpoints.
#'   This must result in an integer (if not, there will be an error). This
#'   option uses the [chkptstanr][chkptstanr::chkptstanr-package] package to
#'   allow interrupted sampling to be resumed. Disabled by default. Enabling
#'   this option requires the [chkptstanr][chkptstanr::chkptstanr-package]
#'   package to be installed (see details).
#' @param stop_after Experimental. (positive integer). If checkpoint_every is
#'   not NULL, this specifies the number of iterations to sample before
#'   stopping. If \code{NULL}, then all iterations are sampled (defaults to
#'   \code{NULL}). Note that sampling will stop at the end of the fir
#'   stcheckpoint which has an iteration number greater than or equal to
#'   \code{stop_after}.
#' @param reset Experimental. (logical). If checkpoint_every is not NULL, reset
#'   = TRUE specifies to reset the checkpoints to 0 and restart sampling without
#'   recompiling the model. If reset = FALSE, the sampling will resume from the
#'   last checkpoint. Default is FALSE.
#'
#' @param checkpoints_folder If checkpoint_every is not NULL, this argument
#'   specifies the directory where the checkpoints will be saved. Default is
#'   'cp_folder' in the current working directory. Can be a relative or absolute
#'   path.
#' @param ... Further arguments passed to [brms::brm()], Stan or chkptstanr. See
#'   the description of [brms::brm()] for more details
#'
#' @details `r a= supported_models(); a`
#'
#'   Type `help(package=bmm)` for a full list of available help topics.
#'
#'   ## Using checkpoints
#'
#'   The `checkpoint_every` argument allows you to save the current state of the
#'   sampler at specific iteration numbers. This can be useful if you want to
#'   interrupt the sampling process and resume it later. This feature requires
#'   the chkptstanr package to be installed, and to use "backend = cmdstanr".
#'   The current CRAN version of chkptstanr has a bug that prevents it from
#'   working. Until the issue is fixed, you can install a working forked version
#'   of chkptstanr with:
#'
#'   ``` r
#'   remotes::install_github("venpopov/chkptstanr")
#'   ```
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
#'
#'
#' # Alternatively, fit the model via checkpointing - save the state of the
#' # sampler every 500 iterations # and stop it after 2000 iterations to check the
#' # results (Experimental feature)
#' remotes::install_github("venpopov/chkptstanr")
#' library(chkptstanr)
#'
#' fit <- fit_model(formula = ff,
#'                 data = dat,
#'                 model = sdmSimple(resp_err = "y"),
#'                 parallel=T,
#'                 backend='cmdstanr',
#'                 warmup = 1000,
#'                 iter = 4000,
#'                 checkpoint_every = 500,
#'                 stop_after = 2000,
#'                 checkpoints_folder = 'checkpoints/sdm_fit1')
#'
#' # The output will look like this:
#'
#' # > Sampling will stop after checkpoint 4
#' # > Model executable is up to date!
#' # >   Initial Warmup (Typical Set)
#' # > Chkpt: 1 / 8; Iteration: 500 / 4000 (warmup)
#' # > Chkpt: 2 / 8; Iteration: 1000 / 4000 (warmup)
#' # > Chkpt: 3 / 8; Iteration: 1500 / 4000 (sample)
#' # > Chkpt: 4 / 8; Iteration: 2000 / 4000 (sample)
#' # > Stopping after 4 checkpoints
#'
#' # The intermediate results are saved to the fit object:
#' summary(fit)
#'
#' # You can also resume the sampling from the last checkpoint, as long as you
#' # don't change the arguments to fit_model():
#'
#' fit2 <- fit_model(formula = ff,
#'                 data = dat,
#'                 model = sdmSimple(resp_err = "y"),
#'                 parallel=T,
#'                 backend='cmdstanr',
#'                 warmup = 1000,
#'                 iter = 4000,
#'                 checkpoint_every = 500,
#'                 stop_after = 2000,
#'                 checkpoints_folder = 'checkpoints/sdm_fit1')
#'
#' }
#'
fit_model <- function(formula, data, model,
                      prior = NULL,
                      chains = 4,
                      parallel = getOption('bmm.parallel', FALSE),
                      sort_data = getOption('bmm.sort_data', "check"),
                      silent = getOption('bmm.silent', 1),
                      checkpoint_every = NULL,
                      stop_after = NULL,
                      checkpoints_folder = 'cp_folder',
                      reset = FALSE,
                      ...) {
  # warning for using old version
  dots <- list(...)
  if ("model_type" %in% names(dots)) {
    stop('The "model_type" argument was deprecated on Feb 3, 2024. Either:
         - See ?fit_model for the new usage;
         - or install the old version of the package with: devtools::install_github("venpopov/bmm@v0.0.1")')
  }

  # set temporary global options and return modified arguments for brms
  configure_opts <- nlist(parallel, chains, sort_data, silent)
  opts <- configure_options(configure_opts)

  # check model, formula and data, and transform data if necessary
  model <- check_model(model, data)
  data <- check_data(model, data, formula)
  user_formula <- formula
  formula <- check_formula(model, data, formula)


  # generate the model specification to pass to brms later
  config_args <- configure_model(model, data, formula)

  # combine the default prior plus user given prior
  config_args$prior <- combine_prior(config_args$prior, prior)

  # estimate the model
  dots <- list(...)
  fit_args <- combine_args(nlist(config_args, opts, dots))
  fit <- run_model(fit_args,
                   checkpoints_folder = checkpoints_folder,
                   checkpoint_every = checkpoint_every,
                   stop_after = stop_after,
                   reset = reset)

  # model postprocessing
  postprocess_brm(model, fit, fit_args = fit_args, user_formula = user_formula,
                  configure_opts = configure_opts)
}

