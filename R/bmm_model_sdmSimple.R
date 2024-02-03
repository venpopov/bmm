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
#' @details `r model_info(sdmSimple())`2
#' @param ... used internally for testing, ignore it
#' @return An object of class `bmmmodel`
#' @export
#' @examples
#' \dontrun{
#' # put a full example here (see 'R/bmm_model_mixture3p.R' for an example)
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
    sdm_simple <- brms::custom_family(
      "sdm_simple", dpars = c("mu", "kappa"),
      links = c("identity", "log"), lb = c(NA, NA),
      type = "real", loop=FALSE,
    )
    family <- sdm_simple

    # prepare initial stanvars to pass to brms, model formula and priors
    stan_funs <- readChar('inst/stan_chunks/sdmSimple_funs.stan',
                          file.info('inst/stan_chunks/sdmSimple_funs.stan')$size)
    stan_tdata <- readChar('inst/stan_chunks/sdmSimple_tdata.stan',
                           file.info('inst/stan_chunks/sdmSimple_tdata.stan')$size)
    stan_likelihood <- readChar('inst/stan_chunks/sdmSimple_likelihood.stan',
                               file.info('inst/stan_chunks/sdmSimple_likelihood.stan')$size)
    stanvars <- brms::stanvar(scode = stan_funs, block = "functions") +
      brms::stanvar(scode = stan_tdata, block = 'tdata') +
      brms::stanvar(scode = stan_likelihood, block = 'likelihood', position="end")


   # construct the default prior
   # TODO: add a proper prior
   prior <-
     # fix mu to 0 (when I change mu to be the center, not c)
     # brms::prior_("constant(0)", class = "Intercept", dpar = "mu")


   # return the list
   out <- nlist(formula, data, family, prior, stanvars)
   return(out)
}

