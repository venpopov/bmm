#############################################################################!
# MODELS                                                                 ####
#############################################################################!
# see file 'R/bmm_model_mixture3p.R' for an example

.model_M3custom <- function(resp_cats = NULL, num_options = NULL, choice_rule = "softmax", ...) {
   out <- list(
      resp_vars = nlist(resp_cats),
      other_vars = nlist(num_options, choice_rule),
      info = list(
         domain = 'Working Memory (categorical)',
         task = 'n-AFC retrieval',
         name = 'The Memory Measurement Model by Oberauer & Lewandowsky (2019)',
         citation = 'Oberauer, K., & Lewandowsky, S. (2019). Simple measurement models for complex working-memory tasks. Psychological Review, 126.',
         version = 'custom',
         requirements = '- Provide names for variables specifying the number of responses in a set of response categories.',
         parameters = list(
            custom_activations = "Dependent of the provided activation functions, the user
              decides which activation sources for the different categories exist and how
              they are labeled",
            b = "Background activation. This source of activation should be added to the
              activation function for each response category, and represents the background
              noise. This parameter is fixed for scaling, but needs to be included in all
              models."
         ),
         fixed_parameters = list(
            b = 0
         )
      ),
      void_mu = FALSE
   )
   class(out) <- c('bmmmodel','M3', 'M3custom')
   out
}
# user facing alias
# information in the title and details sections will be filled in
# automatically based on the information in the .model_M3()$info

#' @title `r .model_M3custom()$info$name`
#' @name M3
#'
#' @details
#'   #### Version: `M3custom`
#'   `r model_info(.model_M3custom(), components =c('domain', 'task', 'name', 'citation'))`
#'
#'
#' @param resp_cats The variable names that contain the number of responses for each of the
#'   response categories used for the M3.
#' @param num_options Either an integer vector of the same length as `resp_cats` if the number
#'   of candidates in the respective response categories are constant across all conditions
#'   in the experiment. Or a vector specifying the variable names that contain the number of
#'   candidates in each response category. The order of these variables should be in the
#'   same order as the names of the response categories passed to `resp_cats`
#' @param choice_rule The choice rule that should be used for the M3. The options are "softmax"
#'   or "luce". The "softmax" option implements the softmax normalization of activation into
#'   probabilities for choosing the different response categories. The "luce" option implements
#'   the normalization of the different activations over the sum of all activations without
#'   exponentiating them. For details on the differences of these choice rules please see
#'   the appendix of Oberauer & Lewandowsky (2019) "Simple measurement models for complex
#'   working memory tasks. Psychological Review"
#' @param ... used internally for testing, ignore it
#' @return An object of class `bmmmodel`
#'
#' @keywords bmmmodel
#'
#' @examples
#' \dontrun{
#' # put a full example here (see 'R/bmm_model_mixture3p.R' for an example)
#' }
#'
#' @export
M3custom <- function(resp_cats, num_options, choice_rule, ...) {
   stop_missing_args()
   .model_M3(resp_cats = resp_cats, num_options = num_options, choice_rule = choice_rule, ...)
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

