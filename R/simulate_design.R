#' @title Simulate data for an experimental design from a bmmodel
#'
#' @description
#' A short description...
#'
#'
#' @export
#'
simulate_design <- function(model,
                            sample_size, n_response,
                            formula,
                            generating_pars,
                            predictors) {
  UseMethod("simulate_design")
}

#' @export
simulate_design.m3 <- function(model, sample_size, n_response,
                               formula,
                               generating_pars,
                               predictors) {
  # simulate generating parameters for the specified sample_size
  df_parameters <- simulate_parameters(sample_size, generating_pars)

  # create data frame with all predictors
  df_predictors <- expand.grid(
    ID = 1:sample_size,
    unlist(predictors)
  )
  colnames(df_predictors) <- c("ID", names(predictors))

  # combine the predictor data frame with the generating parameters
  df_predictors <- merge(df_predictors, df_parameters)

  # separate formula
  par_formula <- formula[!is_nl(formula)]
  if (model$version == "custom") {
    resp_formula <- formula[names(formula) %in% model$resp_vars$resp_cats]
  } else if (model$version == "ss") {
    resp_formula <- bmf(
      formula(paste(model$resp_vars$resp_cats[1], "~ b + a + c")),
      formula(paste(model$resp_vars$resp_cats[2], "~ b + a")),
      formula(paste(model$resp_vars$resp_cats[3], "~ b"))
    )
  } else if (model$version == "cs") {
    resp_formula <- bmf(
      formula(paste(model$resp_vars$resp_cats[1], "~ b + a + c")),
      formula(paste(model$resp_vars$resp_cats[2], "~ b + f * (a + c)")),
      formula(paste(model$resp_vars$resp_cats[3], "~ b + a")),
      formula(paste(model$resp_vars$resp_cats[4], "~ b + f * a")),
      formula(paste(model$resp_vars$resp_cats[5], "~ b"))
    )
  }
  trans_formula <- formula[!names(formula) %in% (c(names(par_formula),names(resp_formula)))]

  # predictor formulas
  if (rhs_vars(par_formula) %in% names(predictors)) {
    # transform parameters with linear model effects
  }

  # Compute transformed parameters
  if (!is.null(trans_formula)) {
    df_predictors <- df_predictors %>%
      mutate_form(trans_formula)
  }

  sim_data <- expand.grid(
    ID = 1:sample_size,
    unlist(predictors)
  )
  colnames(sim_data) <- c("ID", names(predictors))
  sim_data[,model$resp_vars$resp_cats] <- NaN

  sim_data <- sort_by(sim_data, formula(paste("~", paste(colnames(sim_data)[1:(length(predictors) + 1)], collapse = " + " ))))
  df_predictors <- sort_by(df_predictors, formula(paste("~", paste(colnames(sim_data)[1:(length(predictors) + 1)], collapse = " + " ))))

  for (i in 1:nrow(sim_data)) {
    sim_data[i,model$resp_vars$resp_cats] <- rm3(1, size = n_response,
                                                 pars = df_predictors[i,rhs_vars(resp_formula)],
                                                 num_options = model$other_vars$num_options,
                                                 choice_rule = model$other_vars$choice_rule,
                                                 version = model$version,
                                                 act_funs = resp_formula)
  }

  # return the simulated data set
  sim_data
}

#' @title Simulate parameters for a bmmodel based on sampling functions provided for each parameter
#'
#' @description
#' A short description...
#'
#' @param sample_size The number of subjects parameters should be generated for
#' @param generating_pars. Either a named list specifying the function (`fun`) to use to sample subject parameters,
#'   as well as the parameters (`input_pars`) of the functions that should be used for sampling, or a data frame
#'   with the generating parameters for all subjects
#'
#'
#' @export
simulate_parameters <- function(sample_size, generating_pars) {
  # parameters
  list_pars <- list(ID = 1:sample_size)
  for (par in names(generating_pars)) {

    # if the parameter is a list, sample from a normal distribution
    # otherwise, repeat the value nPart times
    if (is.list(generating_pars[[par]])) {
      input_pars <- list(n = sample_size) %>% append(generating_pars[[par]]$pars)
      list_pars[[par]] <- do.call(generating_pars[[par]]$fun, input_pars)
    } else {
      if (length(generating_pars[[par]] == 1)) {
        list_pars[[par]] <- rep(generating_pars[[par]], sample_size)
      } else if (length(generating_pars[[pars]] == sample_size)) {
        list_pars[[par]] <- generating_pars[[par]]
      } else {
        # throw error: number of generating pars mismatches the specified sample_size
      }
    }
  }

  # return the simulated parameters
  as.data.frame(list_pars)
}


#' @title Helper function that calculates new variables from data given a formula
#'
#' @description
#' A short description...
#'
#' @param .data A data frame or vector containing variables or named elements that are used to
#'   calculate new variables or values given the `formulas`
#' @param formula A bmmformula object specifying the transformations that should be used to calculate
#'   new variables or values.
#'
mutate_form <- function(.data, formulas) {

  # convert formulas to list
  formulas = c(formulas)

  # calculate the formula one by one
  for (i in 1:length(formulas)) {

    # get the formula
    form = formulas[[i]]

    # extract dependent and independent variables
    DV = all.vars(form)[1]
    IV = all.vars(form)[-1]

    # check if all the independent variables are in the data frame
    if (!all(IV %in% names(.data))) {
      stop("Some variables used to calculate new variables are not present in the data")
    }
    # extract right-side formula
    right_from = as.character(form[-2]) %>% stringr::str_remove("~")
    # add the dependent variable to the data frame
    .data[, DV] = with(.data, eval(parse(text = right_from)))
  }

  return(.data)
}
