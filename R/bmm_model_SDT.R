#############################################################################!
# MODELS                                                                 ####
#############################################################################!
# see file 'R/bmm_model_mixture3p.R' for an example

.model_SDT <- function(response, stimulus, nTrials = NULL, dist_noise = "gumbel", data_aggregated = FALSE, ...) {
   out <- list(
      resp_vars = nlist(response, stimulus, nTrials),
      other_vars = nlist(dist_noise, data_aggregated, ...),
      info = list(
         domain = 'Perception & Recognition',
         task = 'Signal/Memory Recognition',
         name = 'Models of Signal Detection Theory',
         citation = 'DeCarlo, L. T. (1998). Signal detection theory and generalized linear models. Psychological Methods, 3(2), 186-205.',
         version = '',
         requirements = '',
         parameters = list(
            dprime = "The level of signal from a perceptual or memory stimulus to be recognized",
            crit = "The criterion of activation to be reached for a stimulus to be recognized. This is parmaeterized as the bias relative to the optimal criterion at dprime/2"
         ),
         fixed_parameters = list()
      ),
      void_mu = FALSE
   )
   class(out) <- c('bmmmodel', 'SDT')
   out
}

# user facing alias
# information in the title and details sections will be filled in
# automatically based on the information in the .model_SDT()$info
#' @title `r .model_SDT(NA,NA)$info$name`
#' @name SDT
#' @details `r model_info(SDT(NA,NA))`
#' @param response A description of the response variable
#' @param stimulus A description of the response variable
#' @param nTrials A description of the response variable
#' @param dist_noise A description of the required argument
#' @param data_aggregated A description of the required argument
#' @param ... used internally for testing, ignore it
#' @return An object of class `bmmmodel`
#' @export
#' @examples
#' \dontrun{
#' # put a full example here (see 'R/bmm_model_mixture3p.R' for an example)
#' }
SDT <- .model_SDT


#############################################################################!
# CHECK_DATA S3 methods                                                  ####
#############################################################################!
# A check_data.* function should be defined for each class of the model.
# If a model shares methods with other models, the shared methods should be
# defined in data-helpers.R. Put here only the methods that are specific to
# the model. See ?check_data for details.
# (YOU CAN DELETE THIS SECTION IF YOU DO NOT REQUIRE ADDITIONAL DATA CHECKS)

#' @export
check_data.SDT <- function(model, data, formula) {
   # retrieve required arguments
   resp_names <- unlist(model$resp_vars)

   # check the data (required)
   if (any(!resp_names %in% colnames(data))) {
      missing_resp_name <- resp_names[which(!resp_names %in% colnames(data))]
      if (length(missing_resp_name == 1)) {
         stop(paste0("The response variable '", missing_resp_name, "' is not present in the data."))
      } else {
         stop(paste0("The response variables '", missing_resp_name, "' are not present in the data."))
      }
   }

   # ask user if data should be aggregated to speed up model fitting

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
bmf2bf.SDT <- function(model, formula) {
   # retrieve required response arguments
   response <- model$resp_vars$response
   stimulus <- model$resp_vars$stimulus
   if (model$other_vars$data_aggregated == T) {
      nTrials <- model$resp_vars$nTrials
   }

   # set the base brmsformula given the variable names
   if (model$other_vars$data_aggregated) {
      brms_formula <- brms::bf(paste0(response," | ", "trials(",nTrials,")", " ~ dprime*",stimulus," - crit" ), nl = TRUE)
   } else {
      brms_formula <- brms::bf(paste0(response," ~ dprime*",stimulus," - crit"),nl = TRUE)
   }


   # return brms formula to add the rest of the bmmformula to it
   return(brms_formula)
}


#############################################################################!
# CONFIGURE_MODEL S3 METHODS                                             ####
#############################################################################!
# Each model should have a corresponding configure_model.* function. See
# ?configure_model for more information.

#' @export
configure_model.SDT <- function(model, data, formula) {
   # retrieve required arguments
   dist_noise <- model$other_vars$dist_noise

   # construct brms formula from the bmm formula
   bmm_formula <- formula
   formula <- bmf2bf(model, bmm_formula)

   # construct the family
   if (tolower(dist_noise) == "normal") {
      link_fun <- "probit"
   } else if (tolower(dist_noise) == "gumbel") {
      link_fun <- "cloglog"
   } else if (tolower(dist_noise) == "cauchy") {
      link_fun <- "cauchit"
   } else if (tolower(dist_noise) == "logistic") {
      link_fun <- "logit"
   } else {
      stop("The selected noise distributions is not supported.\n",
           "Please select one of the following noise distributions:
           \"normal\", \"gumbel\". \"cauchy\". or \"logistic\".")
   }

   if (model$other_vars$data_aggregated) {
      family <- stats::binomial(link = link_fun)
   } else {
      family <- brms::bernoulli(link = link_fun)
   }

   # no default priors required
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
postprocess_brm.SDT <- function(model, fit) {
   # any required postprocessing (if none, delete this section)

   return(fit)
}

