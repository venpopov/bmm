#############################################################################!
# MODELS                                                                 ####
#############################################################################!
.cswald_version_table <- list(
  simple = list(
    parameters = list(
      drift = "drift rate",
      bound = "boundary seperation",
      ndt = "non-decision time",
      s = "diffusion constant"
    ),
    links = list(
      drift = "log",
      bound = "log",
      ndt = "log",
      s = "log"
    ),
    priors = list(
      drift = list(main = "normal(0,1)", effects = "normal(0,0.3)"),
      bound = list(main = "normal(0,0.3)", effects = "normal(0,0.3)"),
      ndt = list(main = "normal(-2,0.3)", effects = "normal(0,0.3)"),
      s = list(main = "normal(0,0.5)", effects = "normal(0,0.2)")
    )
  ),
  crisk = list(
    parameters = list(
      drift = "drift rate",
      bound = "boundary seperation",
      ndt = "non-decision time",
      zr = "relative starting point",
      s = "diffusion constant"
    ),
    links = list(
      drift = "log",
      bound = "log",
      ndt = "log",
      zr = "logit",
      s = "log"
    ),
    priors = list(
      drift = list(main = "normal(0,1)", effects = "normal(0,0.3)"),
      bound = list(main = "normal(0,0.3)", effects = "normal(0,0.3)"),
      ndt = list(main = "normal(-2,0.3)", effects = "normal(0,0.3)"),
      zr = list(main = "normal(0,0.3)", effects = "normal(0,0.2)"),
      s = list(main = "normal(0,0.5)", effects = "normal(0,0.2)")
    )
  )
)


.model_cswald <- function(rt = NULL, response = NULL, links = NULL, version = "simple", call = NULL, ...) {
  out <- structure(
    list(
      resp_vars = nlist(rt, response),
      other_vars = nlist(),
      domain = "Processing Speed, Decision Making",
      task = "Choice Reaction Time tasks (with few errors)",
      name = "Censored-Shifted Wald Model",
      citation = "Miller, R., Scherbaum, S., Heck, D. W., Goschke, T., & Enge, S. (2017).
        On the Relation Between the (Censored) Shifted Wald and the Wiener Distribution as Measurement Models
        for Choice Response Times. Applied Psychological Measurement, 42(2), 116-135. https://doi.org/10.1177/0146621617710465",
      version = version,
      requirements = "",
      parameters = .cswald_version_table[[version]][["parameters"]],
      links = .cswald_version_table[[version]][["links"]],
      fixed_parameters = list(mu = 0, s = 0),
      default_priors = .cswald_version_table[[version]][["priors"]],
      void_mu = TRUE
    ),
    class = c("bmmodel", "cswald", paste0("cswald_", version)),
    call = call
  )

  if(!is.null(version)) class(out) <- c(class(out))

  out$links[names(links)] <- links
  out
}

# user facing alias
# information in the title and details sections will be filled in
# automatically based on the information in the .model_cswald()$info

#' @title `r .model_cswald()$name`
#' @name cswald
#' @details `r model_info(.model_cswald())`
#' @param rt A description of the response variable
#' @param response A description of the response variable
#' @param links A list of links for the parameters.
#' @param ... used internally for testing, ignore it
#' @return An object of class `bmmodel`
#' @export
#' @examples
#' \dontrun{
#' # put a full example here (see 'R/model_mixture3p.R' for an example)
#' }
cswald <- function(rt, response, links = NULL, version = "simple", ...) {
  call <- match.call()
  stop_missing_args()
  .model_cswald(rt = rt, response = response, links = links, version = version, call = call, ...)
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
check_data.cswald <- function(model, data, formula) {
  # retrieve required arguments
  rt_var <- model$resp_vars$rt
  response_var <- model$resp_vars$response

  # stop due to missing information
  stopif(
    not_in(rt_var, colnames(data)),
    "The response variable '{rt_var}' is not present in the data."
  )

  stopif(
    not_in(response_var, colnames(data)),
    "The response variable '{response_var}' is not present in the data."
  )

  # checks for rt_var
  if (typeof(data[,rt_var]) %in% c("double","numerical")) {
    stopif(any(data[,rt_var] < 0),
           glue("Some reaction times are lower than zero, please check your data."))
  } else {
    stop(glue("The response variable: ", rt_var, " needs to be of type integer, numerical, or logical."))
  }

  # checks for response_var
  if (typeof(data[,response_var]) %in% c("integer","double","numerical")) {
    stopif(any(!data[,response_var] %in% c(0,1)),
           glue("Only values of zero"))
  } else if (typeof(data[,response_var]) == "logical"){
    warning(glue("The response variable you provided is boolean, it will be internally transformed ",
                 "to an integer variable with values 0 for FALSE and 1 for TRUE."))
    data[,response_var] <- ifelse(data[,response_var],1,0)
  } else {
    stop(glue("The response variable: ", response_var, " needs to be of type integer, numerical, or logical."))
  }

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
bmf2bf.cswald <- function(model, formula) {
  # retrieve required response arguments
  rt_var <- model$resp_vars$rt
  response_var <- model$resp_vars$response

  # set the base brmsformula based
  brms_formula <- brms::bf(glue(rt_var, " | trials(", response_var, ") ~ 1"))

  # return the brms_formula to add the remaining bmmformulas to it.
  brms_formula
}

#############################################################################!
# CONFIGURE_MODEL S3 METHODS                                             ####
#############################################################################!
# Each model should have a corresponding configure_model.* function. See
# ?configure_model for more information.

#' @export
configure_model.cswald_simple <- function(model, data, formula) {
  # retrieve required arguments
  rt_var <- model$other_vars$rt
  response_var <- model$other_vars$response
  trials <- data[,response_var]
  links <- model$links

  # construct brms formula from the bmm formula
  formula <- bmf2bf(model, formula)

  # construct the family & add to formula object
  cswald_family <- function (link_drift = links$drift, link_bound = links$bound, link_ndt = links$ndt, link_s = links$s) {
    brms::custom_family(
      'cswald',
      dpars = c("mu","drift","bound","ndt","s"),
      links = c("identity", link_drift, link_bound, link_ndt, link_s),
      lb = c(NA, NA, NA, NA, NA), # upper bounds for parameters
      ub = c(NA, 0, 0, 0, 0), # lower bounds for parameters
      type = 'real', # real for continous dv, int for discrete dv
      vars = 'trials[n]',
      loop = TRUE, # is the likelihood vectorized
    )
  }
  formula$family <- cswald_family()

  # prepare initial stanvars to pass to brms, model formula and priors
  sc_path <- system.file('stan_chunks', package='bmm')
  stan_functions <- read_lines2(paste0(sc_path, '/cswald_functions.stan'))

  stanvars <- brms::stanvar(scode = stan_functions, block = 'functions')

  # return the list
  nlist(formula, data, stanvars)
}

#' @export
configure_model.cswald_crisk <- function(model, data, formula) {
  # retrieve required arguments
  rt_var <- model$other_vars$rt
  response_var <- model$other_vars$response
  trials <- data[,response_var]
  links <- model$links

  # construct brms formula from the bmm formula
  formula <- bmf2bf(model, formula)

  # construct the family & add to formula object
  cswald_crsik_family <- function (link_drift = links$drift, link_bound = links$bound, link_ndt = links$ndt, link_zr = links$zr, link_s = links$s) {
    brms::custom_family(
      'cswald_crisk',
      dpars = c("mu","drift","bound","ndt","zr","s"),
      links = c("identity", link_drift, link_bound, link_ndt, link_zr, link_s),
      lb = c(NA, NA, NA, NA, 1, NA), # upper bounds for parameters
      ub = c(NA, 0, 0, 0, 0, 0), # lower bounds for parameters
      type = 'real', # real for continous dv, int for discrete dv
      vars = 'trials[n]',
      loop = TRUE, # is the likelihood vectorized
    )
  }
  formula$family <- cswald_crsik_family()

  # prepare initial stanvars to pass to brms, model formula and priors
  sc_path <- system.file('stan_chunks', package='bmm')
  stan_functions <- read_lines2(paste0(sc_path, '/cswald_crisk_functions.stan'))

  stanvars <- brms::stanvar(scode = stan_functions, block = 'functions')

  # return the list
  nlist(formula, data, stanvars)
}

#############################################################################!
# POSTPROCESS METHODS                                                    ####
#############################################################################!
# A postprocess_brm.* function should be defined for the model class. See
# ?postprocess_brm for details

#' @export
postprocess_brm.cswald <- function(model, fit, ...) {
  # any required postprocessing (if none, delete this section)
  fit
}
