#############################################################################!
# MODELS                                                                 ####
#############################################################################!

.model_sdmSimple <- function(required_arg1, required_arg2, ...) {
   out <- list(
      vars = nlist(required_arg1, required_arg2),
      info = list(
         domain = '',
         task = '',
         name = '',
         citation = '',
         version = '',
         requirements = '',
         parameters = list()
      ))
   class(out) <- c('bmmmodel', 'sdmSimple')
   out
}

# user facing alias
# information in the title and details sections will be filled in
# automatically based on the information in the .model_sdmSimple()$info
#' @title `r .model_sdmSimple()$info$name`
#' @details `r model_info(sdmSimple(NA,NA))`
#' @param required_arg1 A description of the required argument
#' @param required_arg2 A description of the required argument
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
   # retrieve required arguments
   required_arg1 <- model$vars$required_arg1
   required_arg2 <- model$vars$required_arg2


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
   # retrieve required arguments
   required_arg1 <- model$vars$required_arg1
   required_arg2 <- model$vars$required_arg2


   # retrieve arguments from the data check
   my_precomputed_var <- attr(data, 'my_precomputed_var')


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

