#############################################################################!
# MODELS                                                                 ####
#############################################################################!
# see file 'R/bmm_model_mixture3p.R' for an example

.model_M3 <- function(resp_var1 = NULL, required_arg1 = NULL, required_arg2 = NULL, ...) {
   out <- list(
      resp_vars = nlist(resp_var1),
      other_vars = nlist(required_arg1, required_arg2),
      info = list(
         domain = '',
         task = '',
         name = '',
         citation = '',
         version = '',
         requirements = '',
         parameters = list(),
         fixed_parameters = list()
      ),
      void_mu = FALSE
   )
   class(out) <- c('bmmmodel', 'M3')
   out
}
# user facing alias
# information in the title and details sections will be filled in
# automatically based on the information in the .model_M3()$info
 
#' @title `r .model_M3()$info$name`
#' @name Model Name#' @details `r model_info(model_M3())`
#' @param resp_var1 A description of the response variable
#' @param required_arg1 A description of the required argument
#' @param required_arg2 A description of the required argument
#' @param ... used internally for testing, ignore it
#' @return An object of class `bmmmodel`
#' @export
#' @examples
#' \dontrun{
#' # put a full example here (see 'R/bmm_model_mixture3p.R' for an example)
#' }
M3 <- function(resp_var1, required_arg1, required_arg2, ...) {
   stop_missing_args()
   .model_M3(resp_var1 = resp_var1, required_arg1 = required_arg1, required_arg2 = required_arg2, ...)
}


#############################################################################!
# CHECK_DATA S3 methods                                                  ####
#############################################################################!
# A check_data.* function should be defined for each class of the model.
# If a model shares methods with other models, the shared methods should be
# defined in data-helpers.R. Put here only the methods that are specific to
# the model. See ?check_data for details.
# (YOU CAN DELETE THIS SECTION IF YOU DO NOT REQUIRE ADDITIONAL DATA CHECKS)

#' @export
check_data.M3 <- function(model, data, formula) {
   # retrieve required arguments
   required_arg1 <- model$other_vars$required_arg1
   required_arg2 <- model$other_vars$required_arg2

   # check the data (required)


   # compute any necessary transformations (optional)

   # save some variables as attributes of the data for later use (optional)

   data = NextMethod('check_data')

   return(data)
}


#############################################################################!
# Convert bmmformula to brmsformla methods                               ####
#############################################################################!
# A bmf2bf.* function should be defined if the default method for consructing
# the brmsformula from the bmmformula does not apply
# The shared method for all `bmmmodels` is defined in helpers-formula.R.
# See ?bmf2bf for details.
# (YOU CAN DELETE THIS SECTION IF YOUR MODEL USES A STANDARD FORMULA WITH 1 RESPONSE VARIABLE)

#' @export
bmf2bf.M3 <- function(model, formula) {
   # retrieve required response arguments
   resp_var1 <- model$resp_vars$resp_var1
   resp_var2 <- model$resp_vars$resp_arg2

   # set the base brmsformula based 
   brms_formula <- brms::bf(paste0(resp_var1," | ", vreal(resp_var2), " ~ 1" ),)

   # add bmmformula to the brms_formula
   # check if parameters are used as non-linear predictors in other formulas
   # and use the brms::lf() or brms::nlf() accordingly.
   dpars <- names(formula)
   for (dpar in dpars) {
     pform <- formula[[dpar]]
     predictors <- rhs_vars(pform)
     if (any(predictors %in% dpars)) {
       brms_formula <- brms_formula + brms::nlf(pform)
     } else {
       brms_formula <- brms_formula + brms::lf(pform)
     }
   }

   return(brms_formula)
}


#############################################################################!
# CONFIGURE_MODEL S3 METHODS                                             ####
#############################################################################!
# Each model should have a corresponding configure_model.* function. See
# ?configure_model for more information.

#' @export
configure_model.M3 <- function(model, data, formula) {
   # retrieve required arguments
   required_arg1 <- model$other_vars$required_arg1
   required_arg2 <- model$other_vars$required_arg2

   # retrieve arguments from the data check
   my_precomputed_var <- attr(data, 'my_precomputed_var')

   # construct brms formula from the bmm formula
   bmm_formula <- formula
   formula <- bmf2bf(model, bmm_formula)

   # construct the family
   family <- NULL

   # construct the default prior
   prior <- NULL

   # return the list
   out <- nlist(formula, data, family, prior)
   return(out)
}


#############################################################################!
# POSTPROCESS METHODS                                                    ####
#############################################################################!
# A postprocess_brm.* function should be defined for the model class. See 
# ?postprocess_brm for details

#' @export
postprocess_brm.M3 <- function(model, fit) {
   # any required postprocessing (if none, delete this section)

   return(fit)
}

