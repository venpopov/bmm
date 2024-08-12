#' Simulate data based on the M3.
#'
#' @param nRet Number of responses for each participant.
#' @param nPart Number of participants.
#' @param pars A list of parameters, including the distribution function and parameters for the function.
#' @param model A list of formulas and choice rules.
#' @param nOpt A vector of the number of options for each response.
#' @param group A vector of group variables.
#' @param addData A data frame containing additional data.
#' @param export A string indicating the type of data to export.
#' `parameters`: the values of each parameter for each participant;
#' `activation`: the activation level of each response category for each participant;
#' `probability`: the choice probability of each response category for each participant;
#' `response`: the number of response of each category for each participant.
#'
#' @return A data frame containing the simulated data.
#'
simulateM3 <- function(
    nRet = 100,
    nPart = 50,
    pars,
    model,
    nOpt,
    group = NULL,
    addData = NULL,
    export = "response"
){

  # The following information is expected from the model parameter.
  # @Gidon, can you adjust it so that it can be retrieved from the BMM model?
  formula <- model$formula
  choiceRule <- model$choiceRule

  # check if the group variables are in the addData
  if (is.null(group)) {
    next
  } else {
    if (is.null(addData)) {
      stop("Some of the group variables may not be in the addData.")
    } else {
      if (!all(group %in% names(addData))) {
        stop("Some of the group variables may not be in the addData.")
      }
    }
  }

  # check if "subj" is in the group
  group <- c("subj", group)

  # check if the addData is a data frame
  if (is.list(addData)) addData <- data.frame(addData)
  if (!is.data.frame(addData)) stop("addData must be either a list or a data frame.")

  # parameters
  List_pars <- list(subj = 1:nPart)
  for (par in names(pars)){

    # if the parameter is a list, sample from a normal distribution
    # otherwise, repeat the value nPart times
    if (is.list(pars[[par]])) {
      input_pars <- list(n = nPart) %>% append(pars[[par]]$pars)
      List_pars[[par]] <- do.call(pars[["a"]]$fun, input_pars)
    } else {
      List_pars[[par]] <- rep(pars[[par]], nPart)
    }
  }

  Data_pars <- as.data.frame(List_pars)

  if (export=="parameter") return(Data_pars)

  # This function is copied from the smartr package,
  # which add new columns to a data frame using formulas.
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
        stop("Some independent variables are not in the data frame.")
      }
      # extract right-side formula
      right_from = as.character(form[-2]) %>% stringr::str_remove("~")
      # add the dependent variable to the data frame
      .data[, DV] = with(.data, eval(parse(text = right_from)))
    }

    return(.data)

  }

  DVs <- c()
  IVs <- c()
  for (i in 1:length(formula)) {
    DVs <- c(DVs, all.vars(formula[[i]])[1])
    IVs <- c(IVs, all.vars(formula[[i]])[-1])
  }
  DVs <- unique(DVs)
  IVs <- unique(IVs)

  # response names
  respNames <- setdiff(DVs, IVs)
  DVparsNames <- intersect(DVs, IVs)
  parNames <- setdiff(IVs, DVpars)

  # Compute the activation level of each option.
  Data_acts <- Data_pars %>%
    cross_join(addData) %>%
    mutate_form(formula) %>%
    dplyr::select(all_of(group), all_of(respNames))

  if (export=="activation") return(Data_acts)



  names(nOpt) <- respNames

  # Compute the choice probabilities.
  Data_probs <- Data_acts %>%
    tidyr::pivot_longer(cols = all_of(respNames), names_to = "response") %>%
    dplyr::rowwise() %>%
    dplyr::mutate(
      nOpt = nOpt[response],
      wValue = ifelse(
        choiceRule == "softmax",
        exp(value)*nOpt,
        value*nOpt
      )
    ) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(
      sumValue = sum(wValue),
      .by = all_of(group)
    ) %>%
    dplyr::mutate(
      prob = wValue/sumValue
    ) %>%
    dplyr::select(all_of(group), response, prob)

  if (export=="probability") return(Data_probs)

  Data_Ret <- Data_probs %>%
    dplyr::mutate(
      nRet = unlist(rmultinom(1, nRet, prob = prob)[,1]),
      .by = all_of(group)
    ) %>%
    dplyr::select(-prob) %>%
    tidyr::pivot_wider(names_from = response, values_from = nRet)

  return(Data_Ret)

}
















