#############################################################################!
# MODELS                                                                 ####
#############################################################################!
# see file 'R/model_mixture3p.R' for an example

.model_cs_wald <- function(resp_var1 = NULL, required_arg1 = NULL, required_arg2 = NULL, links = NULL, version = NULL, call = NULL, ...) {
  out <- structure(
    list(
      resp_vars = nlist(resp_var1),
      other_vars = nlist(required_arg1, required_arg2),
      domain = "",
      task = "",
      name = "",
      citation = "",
      version = version,
      requirements = "",
      parameters = list(),
      links = list(),
      fixed_parameters = list(),
      default_priors = list(par1 = list(), par2 = list()),
      void_mu = FALSE
    ),
    class = c("bmmodel", "cs_wald"),
    call = call
  )
  if(!is.null(version)) class(out) <- c(class(out), paste0("cs_wald_",version))
  out$links[names(links)] <- links
  out
}
# user facing alias
# information in the title and details sections will be filled in
# automatically based on the information in the .model_cs_wald()$info
 
#' @title `r .model_cs_wald()$name`
#' @name Model Name,
#' @details `r model_info(.model_cs_wald())`
#' @param resp_var1 A description of the response variable
#' @param required_arg1 A description of the required argument
#' @param required_arg2 A description of the required argument
#' @param links A list of links for the parameters.
#' @param version A character label for the version of the model. Can be empty or NULL if there is only one version. 
#' @param ... used internally for testing, ignore it
#' @return An object of class `bmmodel`
#' @export
#' @examples
#' \dontrun{
#' # put a full example here (see 'R/model_mixture3p.R' for an example)
#' }
cs_wald <- function(resp_var1, required_arg1, required_arg2, links = NULL, version = NULL, ...) {
   call <- match.call()
   stop_missing_args()
   .model_cs_wald(resp_var1 = resp_var1, required_arg1 = required_arg1, required_arg2 = required_arg2,
                links = links, version = version,call = call, ...)
}

#############################################################################!
# CHECK_DATA S3 methods                                                  ####
#############################################################################!
# A check_data.* function should be defined for each class of the model.
# If a model shares methods with other models, the shared methods should be
# defined in helpers-data.R. Put here only the methods that are specific to
# the model. See ?check_data for details.
# (YOU CAN DELETE THIS SECTION IF YOU DO NOT REQUIRE ADDITIONAL DATA CHECKS)

#' @export
check_data.cs_wald <- function(model, data, formula) {
   # retrieve required arguments
   required_arg1 <- model$other_vars$required_arg1
   required_arg2 <- model$other_vars$required_arg2

   # check the data (required)

   # compute any necessary transformations (optional)

   # save some variables as attributes of the data for later use (optional)

   NextMethod('check_data')
}

#############################################################################!
# Convert bmmformula to brmsformla methods                               ####
#############################################################################!
# A bmf2bf.* function should be defined if the default method for constructing
# the brmsformula from the bmmformula does not apply (e.g if aterms are required).
# The shared method for all `bmmodels` is defined in bmmformula.R.
# See ?bmf2bf for details.
# (YOU CAN DELETE THIS SECTION IF YOUR MODEL USES A STANDARD FORMULA WITH 1 RESPONSE VARIABLE)

#' @export
bmf2bf.cs_wald <- function(model, formula) {
   # retrieve required response arguments
   resp_var1 <- model$resp_vars$resp_var1
   resp_var2 <- model$resp_vars$resp_arg2

   # set the base brmsformula based 
   brms_formula <- brms::bf(paste0(resp_var1, " | ", vreal(resp_var2), " ~ 1"))

   # return the brms_formula to add the remaining bmmformulas to it.
   brms_formula
}

#############################################################################!
# CONFIGURE_MODEL S3 METHODS                                             ####
#############################################################################!
# Each model should have a corresponding configure_model.* function. See
# ?configure_model for more information.

#' @export
configure_model.cs_wald <- function(model, data, formula) {
   # retrieve required arguments
   required_arg1 <- model$other_vars$required_arg1
   required_arg2 <- model$other_vars$required_arg2

   # retrieve arguments from the data check
   my_precomputed_var <- attr(data, 'my_precomputed_var')

   # construct brms formula from the bmm formula
   formula <- bmf2bf(model, formula)

   # construct the family & add to formula object
   cs_wald_family <- brms::custom_family(
     'cs_wald',
     dpars = c(),
     links = c(),
     lb = c(), # upper bounds for parameters
     ub = c(), # lower bounds for parameters
     type = '', # real for continous dv, int for discrete dv
     loop = TRUE, # is the likelihood vectorized
   )
   formula$family <- cs_wald_family

   # prepare initial stanvars to pass to brms, model formula and priors
   sc_path <- system.file('stan_chunks', package='bmm')
   stan_functions <- read_lines2(paste0(sc_path, '/cs_wald_functions.stan'))

   stanvars <- stanvar(scode = stan_functions, block = 'functions')

   # return the list
   nlist(formula, data, stanvars)
}
#############################################################################!
# POSTPROCESS METHODS                                                    ####
#############################################################################!
# A postprocess_brm.* function should be defined for the model class. See
# ?postprocess_brm for details

#' @export
postprocess_brm.cs_wald <- function(model, fit) {
   # any required postprocessing (if none, delete this section)
   fit
}
