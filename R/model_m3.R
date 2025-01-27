############################################################################# !
# MODELS                                                                 ####
############################################################################# !
# see file 'R/bmm_model_mixture3p.R' for an example
.model_m3 <- function(resp_cats = NULL, num_options = NULL,
                      choice_rule = "softmax", version = "custom", ...) {
  # combine additional arguments passed to the model function in list
  dots <- list(...)
  if (is.null(dots$links)) dots$links <- vector("list", length = 0)
  if (is.null(dots$default_priors)) dots$default_priors <- vector("list", length = 0)

  # save model information
  out <- list(
    resp_vars = nlist(resp_cats),
    other_vars = nlist(num_options, choice_rule),
    links = dots$links,
    domain = "Working Memory (categorical)",
    task = "n-AFC retrieval",
    name = "The Memory Measurement Model by Oberauer & Lewandowsky (2019)",
    citation = "Oberauer, K., & Lewandowsky, S. (2019). Simple measurement models for complex working-memory tasks. Psychological Review, 126.",
    version = version,
    requirements = '- Provide names for variables specifying the number of responses in a set of response categories.\n
         - Specify activation sources for each response categories \n
         - Include at least an activation source "b" for all response categories \n
         - Predict the specified activation at least by a fixed intercept and any additional predictors from your data',
    parameters = list(
      b = "Background activation. Activation added to the activation function for each response category, and represents the background
              noise. This parameter is fixed for scaling, but needs to be included in all models."
    ),
    fixed_parameters = list(
      b = ifelse(choice_rule == "softmax", 0, 0.1)
    ),
    default_priors = dots$default_priors,
    void_mu = FALSE
  )

  # add version specific information
  if (version == "ss") {
    ss_parameters <- list(
      c = "Context activation. This source of activation is added to the item cued to be recalled, that is the correct item.",
      a = "General activation. This source of activation is added to all items that were presented during the current trial."
    )

    if (tolower(out$other_vars$choice_rule) == "simple") {
      ss_links <- list(
        c = "log",
        a = "log"
      )
      ss_default_priors <- list(
        a = list(main = "normal(1,0.5)", effects = "normal(0,0.5)"),
        c = list(main = "normal(1.5,0.5)", effects = "normal(0,0.5)")
      )
    } else {
      ss_links <- list(
        c = "identity",
        a = "identity"
      )
      ss_default_priors <- list(
        a = list(main = "normal(2,1)", effects = "normal(0,0.5)"),
        c = list(main = "normal(3,1)", effects = "normal(0,2)")
      )
    }

    # check if links or priors have been provided
    missing_links <- ss_links[!names(ss_links) %in% names(out$links)]
    missing_priors <- ss_default_priors[!names(ss_default_priors) %in% names(out$default_priors)]

    # add version specific info to the model object
    out$parameters <- c(out$parameters, ss_parameters)
    out$links <- c(out$links, missing_links)
    out$default_priors <- c(out$default_priors, missing_priors)
  } else if (version == "cs") {
    cs_parameters <- list(
      c = "Context activation. This source of activation is added to the item cued to be recalled, that is the correct item.",
      a = "General activation. This source of activation is added to all items that were presented during the current trial.",
      f = "Filtering. This parameter captures the extent to which distractors remained in working memory."
    )

    if (tolower(out$other_vars$choice_rule) == "simple") {
      cs_links <- list(
        c = "log",
        a = "log",
        f = "logit"
      )
      cs_default_priors <- list(
        a = list(main = "normal(1,0.5)", effects = "normal(0,.5)"),
        c = list(main = "normal(1.5,0.5)", effects = "normal(0,.5)"),
        f = list(main = "logistic(0,1)", effects = "normal(0,1)")
      )
    } else {
      cs_links <- list(
        c = "identity",
        a = "identity",
        f = "logit"
      )
      cs_default_priors <- list(
        a = list(main = "normal(3,1)", effects = "normal(0,0.5)"),
        c = list(main = "normal(3,1)", effects = "normal(0,2)"),
        f = list(main = "logisitic(0,1)", effects = "normal(0,1)")
      )
    }

    # check if links or priors have been provided
    missing_links <- cs_links[!names(cs_links) %in% names(out$links)]
    missing_priors <- cs_default_priors[!names(cs_default_priors) %in% names(out$default_priors)]

    # add version specific info to the model object
    out$parameters <- c(out$parameters, cs_parameters)
    out$links <- c(out$links, missing_links)
    out$default_priors <- c(out$default_priors, missing_priors)
  }

  class(out) <- c("bmmodel", "m3", paste0("m3_", version))
  out
}


# user facing alias
# information in the title and details sections will be filled in
# automatically based on the information in the .model_M3()$info

#' @title `r .model_m3()$name`
#' @name m3
#'
#' @description
#' The Memory Measurement Model (M3) is a measurement model for working memory
#' tasks with categorical responses. It assumes that each candidate in each response
#' category is activated by a combination of sources of activation. The probability
#' of choosing a response category is determined by the activation of the candidates.
#'
#' @param resp_cats The variable names that contain the number of responses for each of the
#'   response categories used for the M3.
#' @param num_options Either an integer vector of the same length as `resp_cats` if the number
#'   of candidates in the respective response categories are constant across all conditions
#'   in the experiment. Or a vector specifying the variable names that contain the number of
#'   candidates in each response category. The order of these variables should be in the
#'   same order as the names of the response categories passed to `resp_cats`
#' @param choice_rule The choice rule that should be used for the M3. The options are "softmax"
#'   or "simple". The "softmax" option implements the softmax normalization of activation into
#'   probabilities for choosing the different response categories. The "simple" option implements
#'   a simple normalization of the absolute activations over the sum of all activations. For details
#'   on the differences of these choice rules please see the appendix of Oberauer & Lewandowsky (2019)
#'   "Simple measurement models for complex working memory tasks" published in Psychological Review.
#' @param version Character. The version of the M3 model to use. Can be one of
#'  `ss`, `cs`, or `custom`. The default is `custom`.
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
m3 <- function(resp_cats, num_options, choice_rule = "softmax", version = "custom", ...) {
  stop_missing_args()
  stopif(
    !version %in% c("custom", "cs", "ss"), 
    'Unknown version: {version}. It should be one of "ss", "cs" or "custom"'
  )
  stopif(
    !tolower(choice_rule) %in% c("softmax", "simple"),
    'Unsupported choice rule "{choice_rule}. Must be one of "simple" or "softmax"'
  )

  .model_m3(
    resp_cats = resp_cats, num_options = num_options,
    choice_rule = choice_rule, version = version, ...
  )
}

############################################################################# !
# CHECK_Model S3 methods                                                 ####
############################################################################# !

#' @export
check_model.m3 <- function(model, data = NULL, formula = NULL) {
  # name number of response options if names are empty
  if (is.null(names(model$other_vars$num_options))) names(model$other_vars$num_options) <- model$resp_vars$resp_cats
  NextMethod("check_model")
}

#' @export
check_model.m3_custom <- function(model, data = NULL, formula = NULL) {
  existing_par_names <- names(model$parameters)

  if(!is.null(formula)) {
    user_pars <- rhs_vars(formula[is_nl(formula)])
    user_pars <- user_pars[-which(user_pars %in% names(formula[is_nl(formula)]))]
    user_pars <- user_pars[which(!user_pars %in% names(model$parameters))]
    if (length(user_pars > 0)) {
      model$parameters <- append(model$parameters, user_pars)
      names(model$parameters) <- c(existing_par_names, user_pars)
    }
  }


  # add link functions if missing
  stopif(
    length(model$links) < (length(model$parameters) - 1),
    glue(
      "You have not provided link functions for any of the specified parameters.\n",
      "Please provide link functions for all model parameters to ensure proper identification \n",
      "of your model"
    )
  )

  # add default priors if missing
  missing_priors <- names(model$parameters[which(!model$parameters %in% names(model$default_priors))])
  missing_priors <- missing_priors[which(!missing_priors %in% names(model$fixed_parameters))]
  warnif(
    length(missing_priors) > 0 && getOption("bmm.default_priors"),
    glue(
      "You have not provided default_priors for at least one parameter in the model.\n",
      "Default priors will be specified internally based on the provided link function.\n",
      "Please check if the used priors are reasonable for your application"
    )
  )
  for (m in missing_priors) {
    if (model$links[[m]] == "log") {
      prior <- list(main = "normal(1,1)", effect = "normal(0,0.5)")
    } else if (model$links[[m]] == "identity") {
      prior <- list(main = "normal(0,1)", effect = "normal(0,1)")
    } else if (model$links[[m]] == "logit") {
      prior <- list(main = "normal(0,1)", effect = "normal(0,1)")
    } else {
      stop2(glue(
        "Invalid link function provided!\n",
        "Please use one of the following link functions: identity, log, logit"
      ))
    }
    model$default_priors[[m]] <- prior
  }

  NextMethod("check_model")
}

############################################################################# !
# CHECK_data S3 methods                                                  ####
############################################################################# !

#' @export
check_data.m3 <- function(model, data, formula) {
  # Get the vector of the response variables
  resp_name <- model$resp_vars$resp_cats
  # Get the names for each columns
  col_name <- colnames(data)

  # Check if the response variables are legal or not.
  if (sum(grepl("[[:punct:]]|\\s", resp_name)) > 0) {
    stop2("Space and punctuation are not allowed in the response variable names.")
  }

  # Check if the response variables are all present in the data
  missing_list <- setdiff(resp_name, intersect(resp_name, col_name))
  if (length(missing_list) > 0) {
    stop(paste0(
      "The response variable(s) '",
      paste0(missing_list, collapse = "', '"),
      "' is not present in the data."
    ))
  }

  # Transfer all of the response variables to a matrix and name it 'Y'
  data$nTrials <- rowSums(data[, resp_name], na.rm = TRUE)
  resp_matrix <- as.matrix(data[, resp_name])
  resp_matrix[is.na(resp_matrix)] <- 0
  data$Y <- resp_matrix
  data <- dplyr::select(data, -dplyr::all_of(resp_name))

  # Get the vector of the options variables
  nOpt_vect <- model$other_vars$num_options

  # Check whether the option variables have the same length as the response variables.
  if (length(nOpt_vect) != length(resp_name)) {
    stop2("The option variables should have the same length as the response variables.")
  }

  # If the number of options is a string, then it is the name of the column in the data
  if (is.character(nOpt_vect)) {
    option_name <- nOpt_vect

    # Check if the name of the number of options is legal or not.
    if (sum(grepl("[[:punct:]]|\\s", option_name)) > 0) {
      stop2("Space and punctuation are not allowed in the number of options variable name.")
    }

    # Check if the number of options is present in the data
    missing_list <- setdiff(option_name, intersect(option_name, col_name))
    if (length(missing_list) > 0) {
      stop(paste0(
        "The variable(s) '",
        paste0(missing_list, collapse = "', '"),
        "' is not present in the data."
      ))
    }

    # create index variables for any number of Option being zero in one row
    nOpt_idx_vars <- paste0("Idx_", resp_name)
    for (i in 1:length(nOpt_vect)) {
      if (all(data[[nOpt_vect[i]]] == 0)) {
        stop2("At least one of the specified number of candidates in the response categories is zero for all oberservations.\n
            Please remove this category from the model, as it is not identified.")
      }

      data[[nOpt_idx_vars[i]]] <- ifelse(data[[nOpt_vect[i]]] > 0, 1, 0)
      data[[nOpt_vect[i]]] <- ifelse(data[[nOpt_vect[i]]] == 0, 0.0001, data[[nOpt_vect[i]]])
    }

    # If the number of options is a numeric vector,
    # then it represents the number of options for each response variable in all conditions.
  } else if (is.numeric(nOpt_vect)) {
    if (any(nOpt_vect == 0)) {
      stop2("At least one of the specified number of candidates in the response categories is zero for all oberservations.\n
            Please remove this category from the model, as it is not identified.")
    }

    nOpt_name <- paste0("nOpt_", resp_name)

    nOpt_data <- data.frame(nOpt_name, nOpt_vect) %>%
      tidyr::pivot_wider(names_from = nOpt_name, values_from = nOpt_vect)

    # Add the number of options to the data
    data <- dplyr::cross_join(data, nOpt_data)

    # create index variables for any number of Option being zero in one row
    nOpt_idx_vars <- paste0("Idx_", resp_name)
    for (i in 1:length(nOpt_vect)) {
      if (all(data[[nOpt_vect[i]]] == 0)) {
        stop2("At least one of the specified number of candidates in the response categories is zero for all oberservations.\n
            Please remove this category from the model, as it is not identified.")
      }

      data[[nOpt_idx_vars[i]]] <- ifelse(data[[nOpt_vect[i]]] > 0, 1, 0)
      data[[nOpt_vect[i]]] <- ifelse(data[[nOpt_vect[i]]] == 0, 0.0001, data[[nOpt_vect[i]]])
    }
  } else {
    stop2("The number of options should be a string or a numeric vector.")
  }
  NextMethod("check_data")
}


############################################################################# !
# CHECK_Formula S3 methods                                               ####
############################################################################# !

#' @export
check_formula.m3 <- function(model, data, formula) {
  if (model$version != "custom") {
    formula <- construct_m3_act_funs(model, warnings = FALSE) + formula
  }

  formula <- apply_links(formula, model$links)
  formula <- assign_nl(formula)

  NextMethod("check_formula")
}

#' @export
check_formula.m3_custom <- function(model, data, formula) {
  # test if activation functions for all categories are provided
  missing_act_funs <- which(!model$resp_vars$resp_cats %in% names(formula))
  stopif(
    length(missing_act_funs) > 0,
    paste0(
      "You did not provide activation functions for all response categories.\n ",
      "Please provide activation functions for the following response categories in your bmmformula:\n ",
      model$resp_vars$resp_cats[missing_act_funs]
    )
  )

  # test if all activation functions contain background noise "b"
  act_funs <- formula[model$resp_vars$resp_cats]
  form_miss_b <- unlist(lapply(act_funs, missing_b))
  stopif(
    any(form_miss_b),
    paste0(
      "Some of your activation functions do not contain the background noise parameter \"b\".\n ",
      "The following activation functions need a background noise parameter: \n",
      model$resp_vars$resp_cats[which(form_miss_b)]
    )
  )

  NextMethod("check_formula")
}

# helper to test if background noise par is missing
missing_b <- function(formula) {
  !("b" %in% rhs_vars(formula))
}

############################################################################# !
# Convert bmmformula to brmsformla methods                               ####
############################################################################# !
# A bmf2bf.* function should be defined if the default method for consructing
# the brmsformula from the bmmformula does not apply
# The shared method for all `bmmmodels` is defined in helpers-formula.R.
# See ?bmf2bf for details.
# (YOU CAN DELETE THIS SECTION IF YOUR MODEL USES A STANDARD FORMULA WITH 1 RESPONSE VARIABLE)

#' @export
bmf2bf.m3 <- function(model, formula) {
  # retrieve required response arguments
  resp_cats <- model$resp_vars$resp_cats
  nOpt_idx_vars <- paste0("Idx_", resp_cats)
  names(nOpt_idx_vars) <- resp_cats
  if (is.character(model$other_vars$num_options)) {
    num_options <- model$other_vars$num_options
  } else {
    num_options <- paste0("nOpt_", resp_cats)
    names(num_options) <- resp_cats
  }


  # retrieve choice rule
  choice_rule <- tolower(model$other_vars$choice_rule)

  # add transformation to activation according to choice rules
  transform_act <- ifelse(choice_rule == "simple", "log(", "")
  end_act <- ifelse(choice_rule == "simple", ")", "")
  zero_Opt <- ifelse(model$other_vars$choice_rule == "softmax", "(-100)", "(exp(-100))")
  op_Nopts <- ifelse(model$other_vars$choice_rule == "softmax", "+", "*")
  trans_Nopts <- ifelse(model$other_vars$choice_rule == "softmax", "log(", "")
  end_Nopts <- ifelse(model$other_vars$choice_rule == "softmax", ")", "")

  # set the base brmsformula based
  brms_formula <- brms::bf(paste0(
    "Y | trials(nTrials)", " ~", transform_act, nOpt_idx_vars[resp_cats[1]], "*(", resp_cats[1],
    op_Nopts, trans_Nopts, num_options[resp_cats[1]], end_Nopts, ")",
    " + (1-", nOpt_idx_vars[resp_cats[1]], ") *", zero_Opt, end_act
  ), nl = TRUE)

  # for each dependent parameter, check if it is used as a non-linear predictor of
  # another parameter and add the corresponding brms function
  for (i in 2:length(resp_cats)) {
    brms_formula <- brms_formula +
      glue_nlf(
        paste0("mu", resp_cats[i]), " ~", transform_act, nOpt_idx_vars[resp_cats[i]], "*(", resp_cats[i],
        op_Nopts, trans_Nopts, num_options[resp_cats[i]], end_Nopts, ")",
        " + (1-", nOpt_idx_vars[resp_cats[i]], ") *", zero_Opt, end_act
      )
  }

  brms_formula
}


############################################################################# !
# CONFIGURE_MODEL S3 METHODS                                             ####
############################################################################# !
# Each model should have a corresponding configure_model.* function. See
# ?configure_model for more information.

#' @export
configure_model.m3 <- function(model, data, formula) {
  # construct brms formula from the bmm formula
  bmm_formula <- formula
  formula <- bmf2bf(model, bmm_formula)

  # construct the family
  formula$family <- brms::multinomial(refcat = NA)

  formula$family$cats <- model$resp_vars$resp_cats
  formula$family$dpars <- paste0("mu", model$resp_vars$resp_cats)

  # return the list
  out <- nlist(formula, data)
  return(out)
}


#' @title Get Activation Functions for different M3 versions
#'
#' @description
#' This function generates the activation functions for different versions of the Memory
#' Measurement Model (m3) implemented in the `bmm` package. If no `bmmodel` object is
#' passed then it will print the available model versions.
#'
#' @param model A bmmodel object that specifies the M3 model for which the
#'  activation functions should be generated. If no model is passed the available
#'  M3 versions will be printed to the console.
#' @param warnings Logical flag to indicate if information about the generated model formulas
#'  should be printed when the function is called.
#'
#' @return A bmmformula object with the activation functions for the m3 version specified in
#'  the model object. The activation functions use the names of the response categories
#'  specified in the model object.
#'
#' @examplesIf isTRUE(Sys.getenv("BMM_EXAMPLES"))
#' model <- m3(
#'  resp_cats = c("correct","other", "npl"),
#'  num_options = c(1, 4, 5),
#'  version = "ss"
#' )
#'
#' ss_act_funs <- construct_m3_act_funs(model, warnings = FALSE)
#' 
#' @export
construct_m3_act_funs <- function(model = NULL, warnings = TRUE) {
  if (is.null(model)) {
    message2(
      'Available m3 versions with pre-defined activation functions are:
          - "ss" for simple span tasks: 3 response categories (correct, other, npl)
          - "cs" for complex span tasks. 5 response categories (correct, dist_context, other, dist_other, npl)'
    )
    return(invisible())
  }

  stopif(
    !inherits(model, "m3") || !model$version %in% c("ss", "cs"), 
    'Activation functions can only be generated for "m3" models "ss" and "cs"'
  )

  resp_cats <- model$resp_vars$resp_cats
  if (model$version == "ss") {
    warnif(
      warnings,
      glue(
        '\nThe "ss" version of the m3 requires that response categories are ordered as follows:
        1) correct: correct responses
        2) other: other list responses
        3) npl: not presented lures'
      )
    )

    act_funs <- bmf(
      formula(glue("{resp_cats[1]} ~ b + a + c")),
      formula(glue("{resp_cats[2]} ~ b + a")),
      formula(glue("{resp_cats[3]} ~ b"))
    )
  } else if (model$version == "cs") {
    warnif(
      warnings,
      glue(
        "\n", "The \"cs\" version of the m3 requires that response categories are ordered as follows:\n",
        " 1) correct: correct responses\n",
        " 2) dist_context: distractor responses close in context to the correct item\n",
        " 3) other: other list responses\n",
        " 4) dist_other: all distractor responses not close in context to the correct item\n",
        " 5) npl: not presented lures"
      )
    )

    act_funs <- bmf(
      formula(glue("{resp_cats[1]} ~ b + a + c")),
      formula(glue("{resp_cats[2]} ~ b + f * a + f * c")),
      formula(glue("{resp_cats[3]} ~ b + a")),
      formula(glue("{resp_cats[4]} ~ b + f * a")),
      formula(glue("{resp_cats[5]} ~ b"))
    )
  } 

  reset_env(act_funs)
}
