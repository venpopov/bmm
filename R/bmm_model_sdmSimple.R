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
                           'A factorial model comparison. Psychological Review, 130(3), 841–852'),
         version = 'Simple (no non-targets)',
         requirements = '- The response variable should be in radians and represent the angular error relative to the target',
         parameters = list(
            mu = 'Location parameter of the SDM distribution (radians)',
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
#' @details `r model_info(sdmSimple(NA,NA))`2
#' @param ... used internally for testing, ignore it
#' @return An object of class `bmmmodel`
#' @export
#' @examples
#' \dontrun{
#' # put a full example here (see 'R/bmm_model_mixture3p.R' for an example)
#' }
sdmSimple <- .model_sdmSimple



#############################################################################!
# CHECK_DATA S3 methods                                                  ####
#############################################################################!
# A check_data.* function should be defined for each class of the model.
# If a model shares methods with other models, the shared methods should be
# defined in data-helpers.R. Put here only the methods that are specific to
# the model. See ?check_data for details


#' @export
check_data.sdmSimple <- function(model, data, formula) {
   # check the data (required)


   # compute any necessary transformations (optional)


   # save some variables as attributes of the data for later use (optional)


   data = NextMethod('check_data')

   return(data)
}



#############################################################################!
# CONFIGURE_MODEL S3 METHODS                                             ####
#############################################################################!
# Each model should have a corresponding configure_model.* function. See
# ?configure_model for more information.


#' @export
configure_model.sdmSimple <- function(model, data, formula) {
   # retrieve arguments from the data check
   # my_precomputed_var <- attr(data, 'my_precomputed_var')


   # construct the formula
   formula <- formula + brms::lf()


   # construct the family
   family <- NULL


   # construct the default prior
   prior <- NULL


   # return the list
   out <- nlist(formula, data, family, prior)
   return(out)
}

