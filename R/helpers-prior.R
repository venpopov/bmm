#' @title Get Default priors for Measurement Models specified in BMM
#' @description Obtain the default priors for a Bayesian multilevel measurement
#'   model, as well as information for which parameters priors can be specified.
#'   Given the `model`, the `data` and the `formula` for the model, this
#'   function will return the default priors that would be used to estimate the
#'   model. Additionally, it will return all model parameters that have no prior
#'   specified (flat priors). This can help to get an idea about which priors
#'   need to be specified and also know which priors were used if no
#'   user-specified priors were passed to the [bmm()] function.
#'
#'   The default priors in `bmm` tend to be more informative than the default
#'   priors in `brms`, as we use domain knowledge to specify the priors.
#'
#' @inheritParams bmm
#' @aliases default_prior
#' @param object A `bmmformula` object
#' @param ... Further arguments passed to [brms::default_prior()]
#'
#' @return A data.frame with columns specifying the `prior`, the `class`, the
#'   `coef` and `group` for each of the priors specified. Separate rows contain
#'   the information on the parameters (or parameter classes) for which priors
#'   can be specified.
#'
#' @seealso [supported_models()], [brms::default_prior()]
#'
#' @keywords extract_info
#'
#' @examples
#' default_prior(bmf(c ~ 1, kappa ~ 1),
#'   data = oberauer_lin_2017,
#'   model = sdm(resp_error = "dev_rad")
#' )
#' @export
default_prior.bmmformula <- function(object, data, model, formula = object, ...) {
  withr::local_options(bmm.sort_data = FALSE)

  formula <- object
  model <- check_model(model, data, formula)
  data <- check_data(model, data, formula)
  formula <- check_formula(model, data, formula)
  config_args <- configure_model(model, data, formula)
  prior <- configure_prior(model, data, config_args$formula, user_prior = NULL)

  dots <- list(...)
  prior_args <- combine_args(nlist(config_args, dots, prior))
  prior_args$object <- prior_args$formula
  prior_args$formula <- NULL

  brms_priors <- brms::do_call(brms::default_prior, prior_args)

  combine_prior(brms_priors, prior_args$prior)
}


#' @title construct constant priors to fix fixed model parameters
#' @param model a `bmmodel` object
#' @param formula a `brmsformula` object
#' @param additional_pars a list of name=value pairs to fix additional
#'   parameters where the name is the parameter name and the value is the fixed
#'   value
#' @details This function is used to fix the parameters of a model that are
#'   specified as fixed in the model definition. It can also be used to fix any
#'   additional internal parameters that are not specified in the model
#'   definition. it should be used in the configure_model.* function for the
#'   model.
#'
#'   the function puts a constant(value) prior on an Intercept with
#'   dpar=parameter_name
#' @return an object of class brmsprior of the form prior("constant(value)",
#'   class="Intercept", dpar=parameter_name) for all fixed parameters in the
#'   model
#' @noRd
fixed_pars_priors <- function(model, formula, additional_pars = list()) {
  fix_pars <- model$fixed_parameters
  if (length(fix_pars) == 0) {
    return(brms::empty_prior())
  }

  # construct parameter names and prior values
  par_list <- c(model$fixed_parameters, additional_pars)
  pars <- names(par_list)
  values <- unlist(par_list)
  priors <- glue::glue("constant({values})")

  # determine type of parameters
  bterms <- brms::brmsterms(formula)
  dpars <- names(bterms$dpars)
  nlpars <- names(bterms$nlpars)

  # flexibly set the variables for set_prior
  classes <- ifelse(pars %in% dpars, "Intercept", "b")
  coefs <- ifelse(pars %in% dpars, "", "Intercept")
  dpars <- ifelse(pars %in% dpars, pars, "")
  nlpars <- ifelse(pars %in% nlpars, pars, "")
  brms::set_prior(priors, class = classes, coef = coefs, dpar = dpars, nlpar = nlpars)
}


#' Set default priors for a bmmodel
#'
#' This function
#' allows you to specify default priors flexibly regardless of the formula the
#' user has supplied. The function will automatically recognize when intercepts
#' are present or suppressed, and will set the default priors accordingly. You
#' can specify priors of intercepts/main levels of a predictor with supressed
#' intercept, and priors on the effects of the predictors relative to the
#' intercept.
#'
#' @param model A `bmmodel` object
#' @param formula A `brmsformula` object
#' @param data A data.frame containing the data used in the model
#' @noRd
#' @keywords internal developer
set_default_prior <- function(model, data, formula) {
  if (isFALSE(getOption("bmm.default_priors", TRUE))) {
    return(NULL)
  }

  default_priors <- model$default_priors
  stopif(
    !is.list(default_priors) || !all(sapply(default_priors, is.list)),
    "The default_priors should be a list of lists"
  )

  prior <- brms::empty_prior()
  bterms <- brms::brmsterms(formula)
  bterms$allpars <- c(bterms$dpars, bterms$nlpars)
  nlpars <- names(bterms$nlpars)
  pars <- names(bterms$allpars)

  pars_key <- names(default_priors)
  pars <- pars[pars %in% pars_key]

  for (par in pars) {
    bform <- bterms$allpars[[par]]$fe
    terms <- stats::terms(bform)
    prior_desc <- default_priors[[par]]
    has_effects_prior <- !is.null(prior_desc$effects)


    all_rhs_terms <- attr(terms, "term.labels")
    fixef <- all_rhs_terms[attr(terms, "order") == 1]
    inter <- all_rhs_terms[attr(terms, "order") > 1]
    nfixef <- length(fixef)
    ninter <- length(inter)
    interaction_only <- nfixef == 0 && ninter > 0

    # if the user has specified a non-linear predictor on a model parameter, do
    # not set prior
    if (any(all_rhs_terms %in% pars)) {
      next
    }

    # by default set the effects prior on the class 'b'. The intercept can be
    # overwritten later
    if (has_effects_prior && nfixef > 0) {
      if (par %in% nlpars) {
        prior2 <- brms::prior_(prior_desc$effects, class = "b", nlpar = par)
      } else {
        prior2 <- brms::prior_(prior_desc$effects, class = "b", dpar = par)
      }
      prior <- combine_prior(prior, prior2)
    }

    # check if intercept is present and set prior_desc[[1]] on the intercept
    if (attr(terms, "intercept")) {
      if (par %in% nlpars) {
        prior2 <- brms::prior_(prior_desc$main,
          class = "b",
          coef = "Intercept", nlpar = par
        )
      } else {
        prior2 <- brms::prior_(prior_desc$main, class = "Intercept", dpar = par)
      }
      prior <- combine_prior(prior, prior2)
      next
    }

    # next check if there is only one predictor, in which case set the main
    # prior on all levels same if there are multiple predictors, but they are
    # specified only as an interaction get individual predictors, and the
    # formula terms. Fixed effects are those that match
    if ((nfixef == 1 && ninter == 0) || interaction_only) {
      if (par %in% nlpars) {
        prior2 <- brms::prior_(prior_desc[[1]], class = "b", nlpar = par)
      } else {
        prior2 <- brms::prior_(prior_desc[[1]], class = "b", dpar = par)
      }
      prior <- combine_prior(prior, prior2)
      next
    }

    # if there are multiple predictors, set the main prior on the levels of the
    # first predictor
    first_term <- attr(terms, "term.labels")[1]
    levels <- levels(data[[first_term]])
    coefs <- paste0(first_term, levels)
    for (coef in coefs) {
      if (par %in% nlpars) {
        prior2 <- brms::prior_(prior_desc[[1]], class = "b", coef = coef, nlpar = par)
      } else {
        prior2 <- brms::prior_(prior_desc[[1]], class = "b", coef = coef, dpar = par)
      }
      prior <- combine_prior(prior, prior2)
    }
  }
  prior
}

#' Generic S3 method for configuring the default prior for a bmmodel
#'
#' Called by bmm() to automatically construct the priors for a given
#' model, data and formula, and combine it with the prior given by the user. The
#' first method executed is configure_prior.bmmodel, which will build the prior
#' based on information from the model object such as fixed_parameters,
#' default_priors, etc. Thus it is important to define these values in the model
#' object. The function will also recognize if the user has specified that some
#' parameters should be fixed to a constant and put the appropriate constant
#' priors. Any additional priors that a developer wants to specify, which are
#' not based on information in the model object, can be defined in the
#' configure_prior.* method for the model. See configure_prior.imm_full for an
#' example.
#' @param model A `bmmodel` object
#' @param data A data.frame containing the data used in the model
#' @param formula A `brmsformula` object returned from configure_model()
#' @param user_prior A `brmsprior` object given by the user as an argument to
#'  bmm()
#' @param ... Additional arguments passed to the method
#'
#' @return A `brmsprior` object containing the default priors for the model
#'
#' @export
#'
#' @examplesIf isTRUE(Sys.getenv("BMM_EXAMPLES"))
#' configure_prior.mixture3p <- function(model, data, formula, user_prior, ...) {
#'   # if there is set_size 1 in the data, set constant prior over thetant for set_size1
#'   prior <- brms::empty_prior()
#'   set_size_var <- model$other_vars$set_size
#'   prior_cond <- any(data$ss_numeric == 1) && !is.numeric(data[[set_size_var]])
#'
#'   thetant_preds <- rhs_vars(formula$pforms$thetant)
#'   if (prior_cond && set_size_var %in% thetant_preds) {
#'     prior <- prior + brms::prior_("constant(-100)",
#'       class = "b",
#'       coef = paste0(set_size_var, 1),
#'       nlpar = "thetant"
#'     )
#'   }
#'   # check if there is a random effect on theetant that include set_size as predictor
#'   bterms <- brms::brmsterms(formula$pforms$thetant)
#'   re_terms <- bterms$dpars$mu$re
#'   if (!is.null(re_terms)) {
#'     for (i in 1:nrow(re_terms)) {
#'       group <- re_terms$group[[i]]
#'       form <- re_terms$form[[i]]
#'       thetant_preds <- rhs_vars(form)
#'
#'       if (prior_cond && set_size_var %in% thetant_preds) {
#'         prior <- prior + brms::prior_("constant(1e-8)",
#'           class = "sd",
#'           coef = paste0(set_size_var, 1),
#'           group = group,
#'           nlpar = "thetant"
#'         )
#'       }
#'     }
#'   }
#'
#'   prior
#' }
#'
#' @keywords internal developer
configure_prior <- function(model, data, formula, user_prior, ...) {
  UseMethod("configure_prior")
}

#' @export
configure_prior.default <- function(model, data, formula, user_prior, ...) {
  NULL
}

#' @export
configure_prior.bmmodel <- function(model, data, formula, user_prior, ...) {
  prior <- fixed_pars_priors(model, formula)
  default_prior <- set_default_prior(model, data, formula)
  prior <- combine_prior(default_prior, prior)
  prior <- combine_prior(prior, user_prior)
  additional_prior <- NextMethod("configure_prior")
  combine_prior(prior, additional_prior)
}


# internal function to combine two priors (e.g. the default prior with the user
# given prior) parts present in prior2 will overwrite the corresponding parts in
# prior1
combine_prior <- function(prior1, prior2) {
  if (!is.null(prior2)) {
    combined_prior <- dplyr::anti_join(prior1, prior2,
      by = c("class", "dpar", "nlpar", "coef", "group", "resp")
    )
    prior <- combined_prior + prior2
  } else {
    prior <- prior1
  }
  return(prior)
}


summarise_default_prior <- function(prior_list) {
  pars <- names(prior_list)
  prior_info <- ""
  for (par in pars) {
    prior_info <- paste0(prior_info, "   - `", par, "`:\n")
    types <- names(prior_list[[par]])
    for (type in types) {
      prior <- prior_list[[par]][[type]]
      prior_info <- paste0(prior_info, "      - `", type, "`: ", prior, "\n")
    }
  }
  prior_info
}
