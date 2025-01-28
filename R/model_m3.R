############################################################################# !
# MODELS                                                                 ####
############################################################################# !
# see file 'R/bmm_model_mixture3p.R' for an example
# Define lookup tables for parameters, links, and default priors
.m3_version_table <- list(
  ss = list(
    parameters = list(
      c = "Context activation. Added to the item cued to be recalled, that is the correct item.",
      a = "General activation. Added to all items that were presented during the current trial."
    ),
    links = list(
      simple = list(c = "log", a = "log"),
      softmax = list(c = "identity", a = "identity")
    ),
    priors = list(
      simple = list(
        a = list(main = "normal(1,0.5)", effects = "normal(0,0.5)"),
        c = list(main = "normal(1.5,0.5)", effects = "normal(0,0.5)")
      ),
      softmax = list(
        a = list(main = "normal(2,1)", effects = "normal(0,0.5)"),
        c = list(main = "normal(3,1)", effects = "normal(0,2)")
      )
    )
  ),
  cs = list(
    parameters = list(
      c = "Context activation. Added to the item cued to be recalled, that is the correct item.",
      a = "General activation. Added to all items that were presented during the current trial.",
      f = "Filtering. This parameter captures the extent to which distractors remained in working memory."
    ),
    links = list(
      simple = list(c = "log", a = "log", f = "logit"),
      softmax = list(c = "identity", a = "identity", f = "logit")
    ),
    priors = list(
      simple = list(
        a = list(main = "normal(1,0.5)", effects = "normal(0,.5)"),
        c = list(main = "normal(1.5,0.5)", effects = "normal(0,.5)"),
        f = list(main = "logistic(0,1)", effects = "normal(0,1)")
      ),
      softmax = list(
        a = list(main = "normal(3,1)", effects = "normal(0,0.5)"),
        c = list(main = "normal(3,1)", effects = "normal(0,2)"),
        f = list(main = "logistic(0,1)", effects = "normal(0,1)")
      )
    )
  )
)

.model_m3 <- function(resp_cats = NULL, num_options = NULL,
                      choice_rule = "softmax", version = "custom", links = NULL,
                      default_priors = NULL, call = NULL, ...) {
  out <- structure(
    list(
      resp_vars = nlist(resp_cats),
      other_vars = nlist(num_options, choice_rule),
      domain = "Working Memory (categorical)",
      task = "n-AFC retrieval",
      name = "The Memory Measurement Model by Oberauer & Lewandowsky (2019)",
      citation = glue(
        "Oberauer, K., & Lewandowsky, S. (2019). Simple measurement models \\
        for complex working-memory tasks. Psychological Review, 126."
      ),
      version = version,
      requirements = paste0(
        '- Provide names for variables specifying the number of responses in a set of response categories.\n',
        '  - Specify activation sources for each response categories\n',
        '  - Include at least an activation source "b" for all response categories\n',
        '  - Predict the specified activation at least by a fixed intercept and any additional predictors from your data\n'
      ),
      parameters = c(
        list(b = "Background activation. Added to each response category. Fixed for scaling, necessary in all models."),
        .m3_version_table[[version]][["parameters"]]
      ),
      fixed_parameters = list(
        b = if (choice_rule == "softmax") 0 else 0.1
      ),
      links = .m3_version_table[[version]][["links"]][[choice_rule]],
      default_priors = .m3_version_table[[version]][["priors"]][[choice_rule]],
      void_mu = FALSE
    ),
    class = c("bmmodel", "m3", paste0("m3_", version)),
    call = call
  )

  out$links[names(links)] <- links
  out$default_priors[names(default_priors)] <- default_priors
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
#' @details `r model_info(.model_m3(), components =c('domain', 'task', 'name', 'citation'))`
#' #### Version: `ss`
#' `r model_info(.model_m3(version = "ss"), components = c('requirements', 'parameters', 'fixed_parameters', 'links', 'prior'))`
#' #### Version: `cs`
#' `r model_info(.model_m3(version = "cs"), components =c('requirements', 'parameters', 'fixed_parameters', 'links', 'prior'))`
#' #### Version: `custom`
#' `r model_info(.model_m3(version = "custom"), components = c('requirements', 'parameters', 'fixed_parameters', 'links', 'prior'))`
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
  call <- match.call()
  stop_missing_args()
  stopif(
    !version %in% c("custom", "cs", "ss"), 
    'Unknown version: {version}. It should be one of "ss", "cs" or "custom"'
  )
  stopif(
    !tolower(choice_rule) %in% c("softmax", "simple"),
    'Unsupported choice rule "{choice_rule}. Must be one of "simple" or "softmax"'
  )
  stopif(
    length(num_options) != length(resp_cats), 
    "The option variables should have the same length as the response variables."
  )
  names(num_options) <- names(num_options) %||% paste0("n_opt_",resp_cats)

  .model_m3(
    resp_cats = resp_cats, num_options = num_options,
    choice_rule = choice_rule, version = version, call = call, ...
  )
}

############################################################################# !
# CHECK_Model S3 methods                                                 ####
############################################################################# !

#' @export
check_model.m3_custom <- function(model, data = NULL, formula = NULL) {
  if (!is.null(formula)) {
    user_pars <- rhs_vars(formula[is_nl(formula)])
    user_pars <- setdiff(user_pars, names(formula[is_nl(formula)]))
    user_pars <- setdiff(user_pars, names(model$parameters))
    model$parameters <- c(model$parameters, setNames(user_pars, user_pars))
  }

  stopif(
    length(model$links) < length(model$parameters) - 1,
    "Please provide link functions for all model parameters to ensure proper identification of your model"
  )

  # add default priors if missing
  missing_priors <- setdiff(names(model$parameters), names(model$default_priors))
  missing_priors <- setdiff(missing_priors, names(model$fixed_parameters))
  warnif(
    length(missing_priors) > 0 && getOption("bmm.default_priors"),
    "You have not provided default_priors for at least one parameter in the model.
    Default priors will be specified internally based on the provided link function.
    Please check if the used priors are reasonable for your application"
  )
  additional_priors <- lapply(missing_priors, function(m) {
    switch(model$links[[m]],
      log = list(main = "normal(1,1)", effect = "normal(0,0.5)"),
      identitiy = list(main = "normal(0,1)", effect = "normal(0,1)"),
      logit = list(main = "normal(0,1)", effect = "normal(0,1)"),
      stop2("Invalid link function provided! Please use one of the following link functions: identity, log, logit")
    )
  })
  model$default_priors <- c(model$default_priors, additional_priors)
  
  NextMethod("check_model")
}

############################################################################# !
# CHECK_data S3 methods                                                  ####
############################################################################# !

#' @export
check_data.m3 <- function(model, data, formula) {
  resp_name <- model$resp_vars$resp_cats
  n_opt_vect <- model$other_vars$num_options
  col_names <- colnames(data)

  missing_variables <- setdiff(resp_name, col_names)
  stopif(length(missing_variables), "The response variable(s) {paste0(missing_variables, collapse = ', ')} missing in the data")

  # Transfer all of the response variables to a matrix and name it 'Y'
  resp_matrix <- as.matrix(data[, resp_name])
  resp_matrix[is.na(resp_matrix)] <- 0
  data <- data[not_in(col_names, resp_name)]
  data$nTrials <- rowSums(resp_matrix)
  data$Y <- resp_matrix

  if (is.character(n_opt_vect)) {
    # If the number of options is a string, then it is the name of the column in the data
    missing_options <- setdiff(n_opt_vect, col_names)
    stopif(length(missing_options), "The variable(s) {paste0(missing_options, collapse = ', ')} missing in the data")
  } else if (is.numeric(n_opt_vect)) {
    # If the number of options is numeric, then it represents the number of options for each response variable
    for (opt in names(n_opt_vect)) {
      stopif(opt %in% names(data), "The variable {opt} already exists in the data. Give explicit names to your num_options vector")
      data[opt] <- n_opt_vect[opt]
    }
  } else {
    stop2("The number of options should be a string or a numeric vector.")
  }

  # create index variables for any number of Option being zero in one row
  n_opt_idx_vars <- paste0("Idx_", resp_name)
  for (i in 1:length(n_opt_vect)) {
    stopif(
      sum(data[[n_opt_vect[i]]]) == 0,
      "At least one of the specified number of candidates in the response categories is zero for all oberservations.
      Please remove this category from the model, as it is not identified."
    )

    data[[n_opt_idx_vars[i]]] <- ifelse(data[[n_opt_vect[i]]] > 0, 1, 0)
    data[[n_opt_vect[i]]] <- ifelse(data[[n_opt_vect[i]]] == 0, 0.0001, data[[n_opt_vect[i]]])
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
  resp_cats <- model$resp_vars$resp_cats
  # test if activation functions for all categories are provided
  missing_act_funs <- !resp_cats %in% names(formula)
  stopif(
    any(missing_act_funs),
    "You did not provide activation functions for all response categories.
    Please provide activation functions for the following response categories in your bmmformula:
    {resp_cats[missing_act_funs]}"
  )

  # test if all activation functions contain background noise "b"
  act_funs <- formula[resp_cats]
  form_miss_b <- vapply(act_funs, function(f) !("b" %in% rhs_vars(f)), logical(1))
  stopif(
    any(form_miss_b),
    "Some of your activation functions do not contain the background noise parameter \"b\".
    The following activation functions need a background noise parameter:
    {resp_cats[form_miss_b]}"
  )

  NextMethod("check_formula")
}

############################################################################# !
# Convert bmmformula to brmsformla methods                               ####
############################################################################# !
#' @export
bmf2bf.m3 <- function(model, formula) {
  # retrieve required response arguments
  if (is.character(model$other_vars$num_options)) {
    options_vars <- model$other_vars$num_options
  } else {
    options_vars <- names(model$other_vars$num_options)
  }
  resp_cats <- model$resp_vars$resp_cats
  n_opt_idx_vars <- paste0("Idx_", resp_cats)
  names(n_opt_idx_vars) <- resp_cats
  names(options_vars) <- resp_cats

  # add transformation to activation according to choice rules
  choice_rule <- tolower(model$other_vars$choice_rule)
  open <- ifelse(choice_rule == "simple", "log(", "")
  close <- ifelse(choice_rule == "simple", ")", "")
  zero_opt <- ifelse(choice_rule == "softmax", "(-100)", "exp(-100)")
  operator <- ifelse(choice_rule == "softmax", "+", "*")
  open_n_opts <- ifelse(choice_rule == "softmax", "log(", "")
  close_n_opts <- ifelse(choice_rule == "softmax", ")", "")

  # set the base brmsformula based
  cat <- resp_cats[1]
  brms_formula <- brms::bf(glue(
    "Y | trials(nTrials) ~ 
    {open}
    {n_opt_idx_vars[cat]} * ({cat} {operator} {open_n_opts}{options_vars[cat]}{close_n_opts}) + (1 - {n_opt_idx_vars[cat]}) * {zero_opt}
    {close}"
  ), nl = TRUE)

  # for each dependent parameter, check if it is used as a non-linear predictor of
  # another parameter and add the corresponding brms function
  for (cat in resp_cats[-1]) {
    brms_formula <- brms_formula + glue_nlf(
      "mu{cat} ~ 
      {open}
      {n_opt_idx_vars[cat]} * ({cat} {operator} {open_n_opts}{options_vars[cat]}{close_n_opts}) + (1 - {n_opt_idx_vars[cat]}) * {zero_opt}
      {close}"
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
  formula <- bmf2bf(model, formula)

  # construct the family
  formula$family <- brms::multinomial(refcat = NA)
  formula$family$cats <- model$resp_vars$resp_cats
  formula$family$dpars <- paste0("mu", model$resp_vars$resp_cats)

  nlist(formula, data)
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
      '\nThe "ss" version of the m3 requires that response categories are ordered as follows:
      1) correct: correct responses
      2) other: other list responses
      3) npl: not presented lures'
    )

    act_funs <- bmf(
      formula(glue("{resp_cats[1]} ~ b + a + c")),
      formula(glue("{resp_cats[2]} ~ b + a")),
      formula(glue("{resp_cats[3]} ~ b"))
    )
  } else if (model$version == "cs") {
    warnif(
      warnings,
      "\nThe \"cs\" version of the m3 requires that response categories are ordered as follows:
      1) correct: correct responses
      2) dist_context: distractor responses close in context to the correct item
      3) other: other list responses
      4) dist_other: all distractor responses not close in context to the correct item
      5) npl: not presented lures"
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
