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
    fixed_parameters = list(
      mu = 0,
      s = 0
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
      drift = "identity",
      bound = "log",
      ndt = "log",
      zr = "logit",
      s = "log"
    ),
    fixed_parameters = list(
      mu = 0,
      zr = 0,
      s = 0
    ),
    priors = list(
      drift = list(main = "normal(1,1)", effects = "normal(0,0.3)"),
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
      fixed_parameters = .cswald_version_table[[version]][["fixed_parameters"]],
      default_priors = .cswald_version_table[[version]][["priors"]],
      init_ranges = list(
        drift = list(main = c(0.5,2), effects = c(-0.1,0.1)),
        bound = list(main = c(1,2), effects = c(-0.1,0.1)),
        ndt = list(main = c(0.05,0.1), effects = c(-0.1,0.1)),
        zr = list(main = c(0.4,0.6), effects = c(-0.1,0.1)),
        s = list(main = c(0.9,1.1), effects = c(-0.1,0.1))
      ),
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
  brms_formula <- brms::bf(glue(rt_var, " | dec(", response_var, ") ~ 1"))

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
      ub = c(NA, NA, NA, NA, NA), # upper bounds for parameters
      lb = c(NA, 0, 0, 0, 0), # lower bounds for parameters
      type = 'real', # real for continous dv, int for discrete dv
      vars = 'dec[n]',
      loop = TRUE, # is the likelihood vectorized
    )
  }
  formula$family <- cswald_family()

  # prepare initial stanvars to pass to brms, model formula and priors
  sc_path <- system.file('stan_chunks', package='bmm')
  stan_functions <- read_lines2(paste0(sc_path, '/cswald_functions.stan'))

  stanvars <- brms::stanvar(scode = stan_functions, block = 'functions')

  # get initfun for cswald model
  # initfun_cswald <- construct_initfun(model, data, formula)

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
      ub = c(NA, NA, NA, NA, 1, NA), # upper bounds for parameters
      lb = c(NA, 0, 0, 0, 0, 0), # lower bounds for parameters
      type = 'real', # real for continous dv, int for discrete dv
      vars = 'dec[n]',
      loop = TRUE, # is the likelihood vectorized
    )
  }
  formula$family <- cswald_crsik_family()

  # prepare initial stanvars to pass to brms, model formula and priors
  sc_path <- system.file('stan_chunks', package='bmm')
  stan_functions <- read_lines2(paste0(sc_path, '/cswald_crisk_functions.stan'))

  stanvars <- brms::stanvar(scode = stan_functions, block = 'functions')

  # get initfun for cswald model
  # initfun_cswald_crisk <- construct_initfun(model, data, formula)

  # return the list
  nlist(formula, data, stanvars)
}

#' @export
construct_initfun.cswald <- function(model, data, formula) {
  family <- formula$family

  # get model info
  standata <- make_standata(formula, data, family)
  prior_data <- get_prior(formula, data, family)
  prior_data <- prior_data[which(prior_data$class %in% c("b","Intercept")),]
  prior_data$base_par <- paste0(prior_data$nlpar,prior_data$dpar)
  prior_data$par_name <- paste0(prior_data$class,"_",prior_data$base_par)
  prior_data$K_name <- paste0("K_",prior_data$base_par)

  prior_data <- prior_data[,c("base_par","par_name","K_name")]

  # create initfun
  # --- 2. The tailored init function to be returned ---
  the_init_function <- function(chain_id = 1) {

    init_list <- list()

    # --- Initialize ALL Fixed Effects ---
    for (i in 1:nrow(prior_data)) {

      par <- prior_data$base_par[i]
      K_name <- prior_data$K_name[i]
      par_name <- prior_data$par_name[i]

      if (K_name %in% names(standata)) {
        n_coefs <- standata[[K_name]]

        # --- THIS IS THE NEW LINK-AWARE LOGIC ---

        # Get the link function for this parameter from the family object
        link_fun <- family[[paste0("link_", par)]]

        # Define safe value ranges for each parameter
        if (par_name %in% c("b_bs", "b_ndt") && paste0("Intercept_", par) %in% prior_data$par_name) {
          # For Intercept parameters, use a fixed safe value
          safe_value <- switch(
            par,
            ndt = model$init_ranges,
            bs  = c(-0.1, 0.1),
            drift = c(-0.1, 0.1),
            zr =
          )
          link_fun = "identity"
        } else {
          # Safe values for intercept or parameters in the formula without intercept
          safe_value <- switch(
            par,
            ndt = c(0.05, 0.1),
            bs  = c(0.5, 2),
            drift = c(0.1, 1))
        }

        # Transform safe values according to the link function
        init_val <- switch(
          link_fun,
          log      = log(safe_value),
          logit    = qlogis(safe_value),
          identity = safe_value,
          {
            warning(paste("Unrecognized link for", par, ". Using log-scale init."))
            log(safe_value)
          }
        )

        # Assign to init_list
        init_list[[par_name]] <- runif(n_coefs, init_val[1], init_val[2])

      }
    }

    # --- Initialize Random Effects ('sd', 'z', 'L') ---
    # This logic remains the same, as it's already robust.
    re_groups <- stringr::str_extract(names(standata), "M_\\d+") %>%
      na.omit() %>% stringr::str_extract("\\d+") %>% as.integer()

    for (i in re_groups) {
      M <- standata[[paste0("M_", i)]]
      N <- standata[[paste0("N_", i)]]

      init_list[[paste0("sd_", i)]] <- runif(M, 0.01, 0.1) # Tiny but random sd
      init_list[[paste0("z_", i)]] <- matrix(rnorm(M * N, 0, 0.01), nrow = M, ncol = N) # Tiny variance in z
      init_list[[paste0("L_", i)]] <- diag(M) # Start with no correlation
    }

    return(init_list)
  }

  return(the_init_function)
}

#' @export
construct_initfun.cswald_simple <- function(model, data, formula) {
  # get model info
  standata <- make_standata(formula, data, family)
  prior_data <- get_prior(formula, data, family) %>%
    filter(class %in% c("b", "Intercept")) %>%
    mutate(
      base_par = paste0(nlpar,dpar),
      par_name = paste0(class, "_", base_par),
      K_name = paste0("K_", base_par)) %>%
    select(base_par, par_name, K_name) %>%
    distinct()

  return()
}

#' @export
construct_initfun.cswald_crisk <- function(model, data, formula) {
  # get model info
  standata <- make_standata(formula, data, family)
  prior_data <- get_prior(formula, data, family) %>%
    filter(class %in% c("b", "Intercept")) %>%
    mutate(
      base_par = paste0(nlpar,dpar),
      par_name = paste0(class, "_", base_par),
      K_name = paste0("K_", base_par)) %>%
    select(base_par, par_name, K_name) %>%
    distinct()

  return()
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
