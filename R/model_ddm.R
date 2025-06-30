#############################################################################!
# MODELS                                                                 ####
#############################################################################!
# see file 'R/model_mixture3p.R' for an example
.ddm_version_table <- list(
  three_par = list(
    parameters = list(
      drift = "Drift rate = Average rate of evidence accumulation of the decision processes",
      bound = "Boundary separation = Distance between the decision boundaries that need to be reached",
      ndt = "Non-decision time = Additional time required beyond the evidence accumulation process",
      zr = "Relative startin point = Starting point between the decision thresholds relative to the upper bound.",
      sdrift = "Trial-to-trial variability in the drift rate",
      sndt = "Trial-to-trial variability in the non-decision time",
      szr = "Trial-to-trial variability in the relative starting point"
    ),
    links = list(
      drift = "identity", bound = "log", ndt = "log", zr = "identity",
      sdrift = "identity", sndt = "identity", szr = "identity"
    ),
    fixed_parameters = list(
      mu = 0, zr = 0.5, sdrift = 0, sndt = 0, szr = 0
    ),
    priors = list(
      drift = list(main = "cauchy(0,1)", effects = "normal(0,0.5)"),
      bound = list(main = "normal(0,0.5)", effects = "normal(0,0.5)"),
      ndt = list(main = "normal(-1.5,0.5)", effects = "normal(0,0.3)")
    ),
    init_ranges = list(
      drift = list(main = c(-1,1), effects = c(-0.1,0.1)),
      bound = list(main = c(1,2), effects = c(-0.1,0.1)),
      ndt = list(main = c(0.05,0.1), effects = c(-0.01,0.01))
    )
  ),
  four_par = list(
    parameters = list(
      drift = "Drift rate = Average rate of evidence accumulation of the decision processes",
      bound = "Boundary separation = Distance between the decision boundaries that need to be reached",
      ndt = "Non-decision time = Additional time required beyond the evidence accumulation process",
      zr = "Relative startin point = Starting point between the decision thresholds relative to the upper bound.",
      sdrift = "Trial-to-trial variability in the drift rate",
      sndt = "Trial-to-trial variability in the non-decision time",
      szr = "Trial-to-trial variability in the relative starting point"
    ),
    links = list(
      drift = "identity", bound = "log", ndt = "log", zr = "logit",
      sdrift = "identity", sndt = "identity", szr = "identity"
    ),
    fixed_parameters = list(
      mu = 0, sdrift = 0, sndt = 0, szr = 0
    ),
    priors = list(
      drift = list(main = "cauchy(0,1)", effects = "normal(0,0.5)"),
      bound = list(main = "normal(0,0.5)", effects = "normal(0,0.5)"),
      ndt = list(main = "normal(-1.5,0.5)", effects = "normal(0,0.3)"),
      zr = list(main = "normal(0,0.5)", effects = "normal(0,0.3)")
    ),
    init_ranges = list(
      drift = list(main = c(-1,1), effects = c(-0.1,0.1)),
      bound = list(main = c(1,2), effects = c(-0.1,0.1)),
      ndt = list(main = c(0.05,0.1), effects = c(-0.01,0.01)),
      zr = list(main = c(0.4,0.6), effects = c(-0.01,0.01))
    )
  ),
  seven_par = list(
    parameters = list(
      drift = "Drift rate = Average rate of evidence accumulation of the decision processes",
      bound = "Boundary separation = Distance between the decision boundaries that need to be reached",
      ndt = "Non-decision time = Additional time required beyond the evidence accumulation process",
      zr = "Relative startin point = Starting point between the decision thresholds relative to the upper bound.",
      sdrift = "Trial-to-trial variability in the drift rate",
      sndt = "Trial-to-trial variability in the non-decision time",
      szr = "Trial-to-trial variability in the relative starting point"
    ),
    links = list(
      drift = "identity", bound = "log", ndt = "log", zr = "logit",
      sdrift = "log", sndt = "log", szr = "logit"
    ),
    fixed_parameters = list(
      mu = 0
    ),
    priors = list(
      drift = list(main = "cauchy(0,1)", effects = "normal(0,0.5)"),
      bound = list(main = "normal(0,0.5)", effects = "normal(0,0.5)"),
      ndt = list(main = "normal(-1.5,0.5)", effects = "normal(0,0.3)"),
      zr = list(main = "normal(0,0.5)", effects = "normal(0,0.3)"),
      sdrift = list(main = "normal(0,0.5)", effects = "normal(0,0.5)"),
      sndt = list(main = "normal(0,0.5)", effects = "normal(0,0.5)"),
      szr = list(main = "normal(0,0.5)", effects = "normal(0,0.5)")
    ),
    init_ranges = list(
      drift = list(main = c(-1,1), effects = c(-0.1,0.1)),
      bound = list(main = c(1,2), effects = c(-0.1,0.1)),
      ndt = list(main = c(0.05,0.1), effects = c(-0.01,0.01)),
      zr = list(main = c(0.4,0.6), effects = c(-0.05,0.05)),
      sdrift = list(main = c(0,1), effects = c(-0.05,0.05)),
      sndt = list(main = c(0,0.1), effects = c(-0.05,0.05)),
      szr = list(main = c(0,0.1), effects = c(-0.05,0.05))
    )
  )
)


.model_ddm <- function(rt = NULL, response = NULL, links = NULL, version = "three_par", call = NULL, ...) {
  out <- structure(
    list(
      resp_vars = nlist(rt, response),
      other_vars = nlist(),
      domain = "Decision Making / Processing Speed",
      task = "Two-Alternative Force Choice RT",
      name = "Diffusion Decision Model",
      citation = "",
      version = version,
      requirements = "",
      parameters = .ddm_version_table[[version]][["parameters"]],
      links = .ddm_version_table[[version]][["links"]],
      fixed_parameters = .ddm_version_table[[version]][["fixed_parameters"]],
      default_priors = .ddm_version_table[[version]][["priors"]],
      init_ranges = .ddm_version_table[[version]][["init_ranges"]],
      void_mu = FALSE
    ),
    class = c("bmmodel", "ddm"),
    call = call
  )
  if(!is.null(version)) class(out) <- c(class(out), paste0("ddm_",version))
  out$links[names(links)] <- links
  out
}
# user facing alias
# information in the title and details sections will be filled in
# automatically based on the information in the .model_ddm()$info

#' @title `r .model_ddm()$name`
#' @name ddm,
#' @details `r model_info(.model_ddm())`
#' @param rt Name of the reaction time variable coding reaction time in seconds in the data.
#' @param response Name of the response variable coding the response numerically (0 = lower response / incorrect, 1 = upper response / correct)
#' @param links A list of links for the parameters.
#' @param version A character label for the version of the model. Can be empty or NULL if there is only one version.
#' @param ... used internally for testing, ignore it
#' @return An object of class `bmmodel`
#' @export
#' @examples
#' \dontrun{
#' # put a full example here (see 'R/model_mixture3p.R' for an example)
#' }
ddm <- function(rt, response, links = NULL, version = "three_par", ...) {
  call <- match.call()
  stop_missing_args()
  .model_ddm(rt = rt, response = response,
             links = links, version = version, call = call, ...)
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
check_data.ddm <- function(model, data, formula) {
  # retrieve required arguments

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
bmf2bf.ddm <- function(model, formula) {
  # retrieve required response arguments
  rt <- model$resp_vars$rt
  response <- model$resp_vars$response

  # set the base brmsformula based
  brms_formula <- brms::bf(paste0(rt, " | dec(", response, ") ~ 1"))

  # return the brms_formula to add the remaining bmmformulas to it.
  brms_formula
}

#############################################################################!
# CONFIGURE_MODEL S3 METHODS                                             ####
#############################################################################!
# Each model should have a corresponding configure_model.* function. See
# ?configure_model for more information.

#' @export
configure_model.ddm <- function(model, data, formula) {
  # construct brms formula from the bmm formula
  formula <- bmf2bf(model, formula)

  # construct the family & add to formula object
  ddm_family <- function(link_drift = "identity", link_bound = "log", link_ndt = "log", link_zr = "logit",
                         link_sdrift = "log", link_sndt = "log", link_szr = "log") {
    brms::custom_family(
      'ddm',
      dpars = c("mu","drift","bound","ndt","zr","sdrift","sndt","szr"),
      links = c("identity",link_drift, link_bound, link_ndt, link_zr, link_sdrift, link_sndt, link_szr),
      lb = c(NA,NA,0.1, 0,0,0 ,0 ,0),
      ub = c(NA,NA,NA ,NA,1,NA,NA,NA),
      type = 'real',
      vars = 'dec[n]',
      loop = TRUE
    )
  }
  formula$family <- ddm_family(
    link_drift = model$links$drift,
    link_bound = model$links$bound,
    link_ndt = model$links$ndt,
    link_zr = model$links$zr,
    link_sdrift = model$links$sdrift,
    link_sndt = model$links$sndt,
    link_szr = model$links$szr
  )

  # prepare initial stanvars to pass to brms, model formula and priors
  sc_path <- system.file('stan_chunks', package='bmm')
  stan_functions <- read_lines2(paste0(sc_path, '/ddm_functions.stan'))

  stanvars <- brms::stanvar(scode = stan_functions, block = 'functions')

  # return the list
  nlist(formula, data, stanvars)
}
