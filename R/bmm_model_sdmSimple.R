#############################################################################!
# MODELS                                                                 ####
#############################################################################!

.model_sdmSimple <- function(...) {
   out <- list(
      vars = nlist(),
      info = list(
         domain = 'Visual working memory',
         task = 'Continuous reproduction',
         name = 'Signal Discrimination Model (SDM) by Oberauer (2023)',
         citation = paste0('Oberauer, K. (2023). Measurement models for visual working memory - ',
                           'A factorial model comparison. Psychological Review, 130(3), 841-852'),
         version = 'Simple (no non-targets)',
         requirements = '- The response variable should be in radians and represent the angular error relative to the target',
         parameters = list(
            # mu = 'Location parameter of the SDM distribution (in radians; fixed internally to 0)',
            c = 'Memory strength parameter of the SDM distribution',
            kappa = 'Precision parameter of the SDM distribution (log scale)'
         )
      ))
   class(out) <- c('bmmmodel', 'vwm', 'sdmSimple')
   out
}

# user facing alias
# information in the title and details sections will be filled in
# automatically based on the information in the .model_sdmSimple()$info
#' @title `r .model_sdmSimple()$info$name`
#' @name SDM
#' @details
#' see [vignette("sdm-simple")] for a detailed description of the model and how to use it.
#' `r model_info(sdmSimple())`
#' @param ... used internally for testing, ignore it
#' @return An object of class `bmmmodel`
#' @export
#' @keywords bmmmodel
#' @examples
#' \dontrun{
#' # simulate data from the model
#' library(bmm)
#' library(brms)
#' dat <- data.frame(y = rsdm(n = 1000, c = 4, kappa = 3))
#'
#' # specify formula
#' ff <- bf(y ~ 1,
#'                c ~ 1,
#'                kappa ~ 1)
#'
#' # specify prior
#' prior <- prior(normal(1,2), class='Intercept', dpar='c')+
#'    prior(normal(1,2), class='Intercept', dpar='kappa')
#'
#' # specify the model
#' fit <- fit_model(formula = ff,
#'                  data = dat,
#'                  model = sdmSimple(),
#'                  prior = prior,
#'                  parallel=T,
#'                  iter=2000,
#'                  backend='cmdstanr')
#'
#' # extract coefficients and plot fit
#' coef <- exp(brms::fixef(fit)[2:3,1])
#' hist(dat$y, breaks=60, freq=F)
#' x <- seq(-pi,pi,0.01)
#' lines(x, dsdm(x, mu=0, c=coef['c_Intercept'],
#'               kappa=coef['kappa_Intercept']), col='red')
#' }
sdmSimple <- .model_sdmSimple



#############################################################################!
# CONFIGURE_MODEL S3 METHODS                                             ####
#############################################################################!
# Each model should have a corresponding configure_model.* function. See
# ?configure_model for more information.


#' @export
configure_model.sdmSimple <- function(model, data, formula) {
    # construct the family
    # note - c has a log link, but I've coded it manually for computational efficiency
    sdm_simple <- brms::custom_family(
      "sdm_simple", dpars = c("mu", "c","kappa"),
      links = c("identity","identity", "log"), lb = c(NA, NA, NA),
      type = "real", loop=FALSE,
    )
    family <- sdm_simple

    # prepare initial stanvars to pass to brms, model formula and priors
    sc_path <- system.file("stan_chunks", package="bmm")
    stan_funs <- readChar(paste0(sc_path, '/sdmSimple_funs.stan'),
                          file.info(paste0(sc_path, '/sdmSimple_funs.stan'))$size)
    stan_tdata <- readChar(paste0(sc_path, '/sdmSimple_tdata.stan'),
                           file.info(paste0(sc_path, '/sdmSimple_tdata.stan'))$size)
    stan_likelihood <- readChar(paste0(sc_path, '/sdmSimple_likelihood.stan'),
                               file.info(paste0(sc_path, '/sdmSimple_likelihood.stan'))$size)
    stanvars <- brms::stanvar(scode = stan_funs, block = "functions") +
      brms::stanvar(scode = stan_tdata, block = 'tdata') +
      brms::stanvar(scode = stan_likelihood, block = 'likelihood', position="end")


   # construct the default prior
   # TODO: add a proper prior
   prior <-
     # fix mu to 0 (when I change mu to be the center, not c)
     brms::prior_("constant(0)", class = "Intercept", dpar = "mu")


   # return the list
   out <- nlist(formula, data, family, prior, stanvars)
   return(out)
}


#############################################################################!
# POSTPROCESS METHODS                                                    ####
#############################################################################!

#' @export
postprocess_brm.sdmSimple <- function(model, fit) {
  # manually set link_c to "log" since I coded it manually
  fit$family$link_c <- "log"
  fit$formula$family$link_c <- "log"
  return(fit)
}
