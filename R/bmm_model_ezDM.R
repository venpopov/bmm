#############################################################################!
# MODELS                                                                 ####
#############################################################################!
# see file 'R/bmm_model_mixture3p.R' for an example

.model_ezDM <- function(meanRT, varRT, nCorr, nTrials, data_aggregated = FALSE, ...) {
   out <- list(
      resp_vars = nlist(meanRT, varRT, nCorr, nTrials),
      other_vars = nlist(data_aggregated, ...),
      info = list(
         domain = 'Reaction Time Models',
         task = 'Two-alternative forced choice',
         name = 'ez Diffusion Model (DM)',
         citation = 'Wagenmakers, E.-J., Van Der Maas, H. L. J., & Grasman, R. P. P. P. (2007). An EZ-diffusion model for response time and accuracy. Psychonomic Bulletin & Review, 14(1), 3–22. https://doi.org/10/fk447c',
         version = '',
         requirements = '',
         parameters = list(
            drift = "The drift rate capturing the speed of evidence accumulation towards the decision boundaries.",
            bound = "The boundary separation capturing the amoung of evidence require to initiate a response.",
            ndt = "The non-decision time capturing additive processes unrelated to the actual decision process, such as encoding or motor execution."
         )
      ),
      void_mu = FALSE
   )
   class(out) <- c('bmmmodel', 'ezDM')
   out
}

# user facing alias
# information in the title and details sections will be filled in
# automatically based on the information in the .model_ezDM()$info
#' @title `r .model_ezDM(NA,NA,NA,NA)$info$name`
#' @details `r model_info(ezDM(NA,NA,NA,NA))`
#'
#' @description
#' The ez-Diffusion Model provides a simplified and quick estimation of the drift rate,
#' boundary separation, and non-decision time of the standard 4-parameter diffusion model.
#' The starting point in this model is always fixed in the middle between the two decision
#' boundaries (i.e. unbiased decision process). The noise of the evidence accumulation process
#' s is fixed to 1 for scaling purposes.
#'
#' @param meanRT The name of the variable containing the mear reaction time of correct responses in the data.
#' @param varRT The name of the variable containing the variance of reaction times of correct responses in the data.
#' @param nCorr The name of the variable containing the number of correct responses in the data.
#' @param nTrials The name of the variable containing the total number of trials in the data.
#' @param ... used internally for testing, ignore it
#'
#' @references
#' 1) Wagenmakers, E.-J., Van Der Maas, H. L. J., & Grasman, R. P. P. P. (2007). An EZ-diffusion model for response time and accuracy. Psychonomic Bulletin & Review, 14(1), 3–22. https://doi.org/10/fk447c
#'
#' 2) Peña, A. F. C. D. la, & Vandekerckhove, J. (2024). An EZ Bayesian hierarchical drift diffusion model for response time and accuracy. https://doi.org/10.31234/osf.io/yg9b5
#'
#' @return An object of class `bmmmodel`
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # generate data using the rtdists package
#'
#' }
ezDM <- .model_ezDM



#############################################################################!
# CHECK_DATA S3 methods                                                  ####
#############################################################################!
# A check_data.* function should be defined for each class of the model.
# If a model shares methods with other models, the shared methods should be
# defined in data-helpers.R. Put here only the methods that are specific to
# the model. See ?check_data for details


#' @export
check_data.ezDM <- function(model, data, formula) {
   # retrieve required arguments
   # required_arg1 <- model$other_vars$required_arg1
   # required_arg2 <- model$other_vars$required_arg2


   # check the data (required)


   # compute any necessary transformations (optional)


   # save some variables as attributes of the data for later use (optional)


   data = NextMethod('check_data')

   return(data)
}

#############################################################################!
# Convert bmmformula to brmsformla S3 methods for the ezDM                ####
#############################################################################!

#' @export
bmf2bf.ezDM <- function(model, formula) {
   # retrieve required arguments
   mrt_var <- model$resp_vars$meanRT
   vrt_var <- model$resp_vars$varRT
   nCorr_var <- model$resp_vars$nCorr
   nTrial_var <- model$resp_vars$nTrials

   # meanRT | vreal(varRT) + vint(nHits) + trials(nTrials)  ~ drift
   # set base brms formula based on response
   brms_formula <- brms::bf(paste0(mrt_var, " | vreal(", vrt_var, ") + vint(",nCorr_var,") + trials(",nTrial_var,")" , "~ drift"), nl = T)

   # for each dependent parameter, check if it is used as a non-linear predictor of
   # another parameter and add the corresponding brms function
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
configure_model.ezDM <- function(model, data, formula) {
   # construct the formula
   bmm_formula <- formula
   formula <- bmf2bf(model, bmm_formula)

   # set up the custom family
   ezDM_family <- function(link = "identity", link_bound = "log", link_ndt = "log") {
      custom_family(
         "ezDM",
         dpars = c("mu", "bound", "ndt"), # Those will be estimated
         links = c(link, link_bound, link_ndt),
         lb = c(NA, 0, 0), # bounds for the parameters
         ub = c(NA, NA, NA),
         vars = c("vreal1[n]" , "vint1[n]", "trials[n]"),
         loop = TRUE,
         type = "real"
      )
   }
   family <- ezDM_family

   # prepare initial stanvars to pass to brms, model formula and priors
   sc_path <- system.file('stan_chunks', package='bmm')
   stan_functions <- readChar(paste0(sc_path, '/ezDM_functions.stan'),
      file.info(paste0(sc_path, '/ezDM_functions.stan'))$size)

   stanvars <- stanvar(scode = stan_functions, block = 'functions')

   # construct the default prior
   prior <- prior(normal(0,1), class = "b", nlpar = "drift") +
      prior(normal(0,0.5), class = "Intercept", dpar = "bound") +
      prior(normal(-2,0.5), class = "Intercept", dpar = "ndt")

   # return the list
   out <- nlist(formula, data, family, prior, stanvars)
   return(out)
}



#############################################################################!
# POSTPROCESS METHODS                                                    ####
#############################################################################!
# A postprocess_brm.* function should be defined for the model class. See
# ?postprocess_brm for details


#' @export
postprocess_brm.ezDM <- function(model, fit) {
   # any required postprocessing (if none, delete this section)


   return(fit)
}

