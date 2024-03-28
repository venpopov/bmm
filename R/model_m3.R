#############################################################################!
# MODELS                                                                 ####
#############################################################################!
# see file 'R/bmm_model_mixture3p.R' for an example

.model_m3 <- function(resp_cats = NULL, num_options = NULL, links = NULL,
                      choice_rule = "softmax", version = "custom", ...) {
   # name the number of options in each response categories if no names are provided
   if (is.null(names(num_options))) names(num_options) <- resp_cats

   # save model information
   out <- list(
      resp_vars = nlist(resp_cats),
      other_vars = nlist(num_options, choice_rule, ...),
      links = links,
      domain = 'Working Memory (categorical)',
      task = 'n-AFC retrieval',
      name = 'The Memory Measurement Model by Oberauer & Lewandowsky (2019)',
      citation = 'Oberauer, K., & Lewandowsky, S. (2019). Simple measurement models for complex working-memory tasks. Psychological Review, 126.',
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
      default_priors = list(),
      void_mu = FALSE
   )

   # add version specific information
   if (version == "ss") {
      ss_parameters <- list(
         c = "Context activation. This source of activation is added to the item cued to be recalled, that is the correct item.",
         a = "General activation. This source of activation is added to all items that were presented during the current trial."
      )
      ss_links <- list(
         c = "exp",
         a = "exp"
      )
      missing_links <- !names(out$links) %in% ss_links

      ss_default_priors <- list(
         a = list(main = "normal(0,1)", effects = "normal(0,1)"),
         c = list(main = "normal(0,1)", effects = "normal(0,1)"),
      )
      missing_priors <- !names(out$default_priors) %in% ss_default_priors

      # add version specific info to the model object
      out$parameters <- c(out$parameters, ss_parameters)
      out$links <- c(out$links, ss_links[missing_links])
      out$default_priors$s <- c(out$default_priors, ss_default_priors[missing_priors])
   } else if (version == "cs") {
      cs_parameters <- list(
         c = "Context activation. This source of activation is added to the item cued to be recalled, that is the correct item.",
         a = "General activation. This source of activation is added to all items that were presented during the current trial.",
         f = "Filtering. This parameter captures the extent to which distractors remained in working memory."
      )

      cs_links <- list(
         c = "exp",
         a = "exp",
         f = "logit"
      )
      missing_links <- !names(out$links) %in% cs_links

      cs_default_priors <- list(
         a = list(main = "normal(0,1)", effects = "normal(0,1)"),
         c = list(main = "normal(0,1)", effects = "normal(0,1)"),
         f = list(main = "normal(0,1)", effects = "normal(0,1)")
      )
      missing_priors <- !names(out$default_priors) %in% cs_default_priors

      # add version specific info to the model object
      out$parameters <- c(out$parameters, cs_parameters)
      out$links <- c(out$links, cs_links[missing_links])
      out$default_priors$s <- c(out$default_priors, cs_default_priors[missing_priors])
   }

   class(out) <- c('bmmodel','m3', paste0("m3_",version))
   out
}


# user facing alias
# information in the title and details sections will be filled in
# automatically based on the information in the .model_M3()$info

#' @title `r .model_m3()$name`
#' @name m3
#'
#'
#' @param resp_cats The variable names that contain the number of responses for each of the
#'   response categories used for the M3.
#' @param num_options Either an integer vector of the same length as `resp_cats` if the number
#'   of candidates in the respective response categories are constant across all conditions
#'   in the experiment. Or a vector specifying the variable names that contain the number of
#'   candidates in each response category. The order of these variables should be in the
#'   same order as the names of the response categories passed to `resp_cats`
#' @param links A named list that provides the link functions that should be used for all M3
#'   parameters used in the model calls. Current options for the link functions are: "identity",
#'   "log", and "logit". The "identity" link should be used for all parameters with an unbounded
#'   range from minus to plus infinity. The "log" link should be used for all parameters with a bounded
#'   range from zero to plus infinity (i.e., most activation sources). The "logit" link should be
#'   used for all parameters bounded between zero and one (i.e. proportional reductions in activations,
#'   such as filtering or removal)
#' @param choice_rule The choice rule that should be used for the M3. The options are "softmax"
#'   or "luce". The "softmax" option implements the softmax normalization of activation into
#'   probabilities for choosing the different response categories. The "luce" option implements
#'   the normalization of the different activations over the sum of all activations without
#'   exponentiating them. For details on the differences of these choice rules please see
#'   the appendix of Oberauer & Lewandowsky (2019) "Simple measurement models for complex
#'   working memory tasks. Psychological Review"
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
m3 <- function(resp_cats, num_options, links, choice_rule = "softmax", version = "custom", ...) {
   stop_missing_args()
   .model_m3(resp_cats = resp_cats, num_options = num_options,
             choice_rule = choice_rule, links = links, version = version, ...)
}


#############################################################################!
# CHECK_Model S3 methods                                                 ####
#############################################################################!

#' @export
check_model.m3 <- function(model, data = NULL, formula = NULL) {
   # name number of response options if names are empty
   if (is.null(names(model$other_vars$num_options))) names(model$other_vars$num_options) <- model$resp_vars$resp_cats
   NextMethod("check_model")
}

#' @export
check_model.m3_custom <- function(model, data = NULL, formula = NULL) {
   existing_par_names <- names(model$parameters)

   # add user defined parameters to the model object
   act_funs <- formula[model$resp_vars$resp_cats]
   user_pars <- rhs_vars(act_funs)
   user_pars <- user_pars[which(!user_pars %in% names(model$parameters))]
   if (length(user_pars > 0)) {
      model$parameters <- append(model$parameters, user_pars)
      names(model$parameters) <- c(existing_par_names,user_pars)
   }

   NextMethod("check_model")
}

#############################################################################!
# CHECK_Formula S3 methods                                               ####
#############################################################################!

#' @export
check_formula.m3 <- function(model, data, formula){
   formula <- apply_links(formula, model$links)
   formula <- assign_nl(formula)

   NextMethod("check_formula")
}

#' @export
check_formula.m3_custom <- function(model, data, formula){
   # test if activation functions for all categories are provided
   missing_act_funs <- which(!model$resp_vars$resp_cats %in% names(formula))
   stopif(length(missing_act_funs) > 0,
          paste0("You did not provide activation functions for all response categories.\n ",
                 "Please provide activation functions for the following response categories in your bmmformula:\n ",
                 model$resp_vars$resp_cats[missing_act_funs]))

   # test if all activation functions contain background noise "b"
   act_funs <- formula[model$resp_vars$resp_cats]
   form_miss_b <- unlist(lapply(act_funs, missing_b))
   stopif(any(form_miss_b),
          paste0("Some of your activation functions do not contain the background noise parameter \"b\".\n ",
                 "The following activation functions need a background noise parameter: \n",
                 model$resp_vars$resp_cats[which(form_miss_b)]))

   NextMethod("check_formula")
}

# helper to test if background noise par is missing
missing_b <- function(formula) {
   !("b" %in% rhs_vars(formula))
}

#############################################################################!
# Convert bmmformula to brmsformla methods                               ####
#############################################################################!
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

   # retrieve choice rule
   choice_rule <- tolower(model$other_vars$choice_rule)

   # add transformation to activation according to choice rules
   transform_act <- ifelse(choice_rule == "luce","log(","")
   end_act <- ifelse(choice_rule == "luce",")","")
   zero_Opt <- ifelse(model$other_vars$choice_rule == "softmax","(-100)","(exp(-100))")
   op_Nopts <- ifelse(model$other_vars$choice_rule == "softmax","+","*")
   trans_Nopts <- ifelse(model$other_vars$choice_rule == "softmax","log(","")
   end_Nopts <- ifelse(model$other_vars$choice_rule == "softmax",")","")

   # set the base brmsformula based
   brms_formula <- brms::bf(paste0("Y | trials(nTrials)", " ~", transform_act, nOpt_idx_vars[resp_cats[1]],"*(", resp_cats[1],
                                   op_Nopts, trans_Nopts, model$other_vars$num_options[resp_cats[1]],end_Nopts,")",
                                   " + (1-",nOpt_idx_vars[resp_cats[1]],") *",zero_Opt,end_act),nl = TRUE)

   # for each dependent parameter, check if it is used as a non-linear predictor of
   # another parameter and add the corresponding brms function
   for (i in 2:length(resp_cats) ) {
      brms_formula <- brms_formula +
         glue_nlf(paste0("mu",resp_cats[i]), " ~", transform_act, nOpt_idx_vars[resp_cats[i]],"*(", resp_cats[i],
                  op_Nopts, trans_Nopts, model$other_vars$num_options[resp_cats[i]],end_Nopts,")",
                  " + (1-",nOpt_idx_vars[resp_cats[i]],") *",zero_Opt,end_act)
   }

   brms_formula
}


#############################################################################!
# CONFIGURE_MODEL S3 METHODS                                             ####
#############################################################################!
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
   formula$family$dpars <- paste0("mu",model$resp_vars$resp_cats)

   # construct the default prior
   # prior <- fixed_pars_priors(model, formula)
   # if (getOption("bmm.default_priors", TRUE)) {
   #    prior <- prior +
   #       set_default_prior(bmm_formula, data,
   #                         prior_list=list(kappa =list(main = 'normal(2,1)',effects = 'normal(0,1)', nlpar = T),
   #                                         c = list(main = 'normal(0,1)',effects = 'normal(0,1)', nlpar = T),
   #                                         s = list(main = 'normal(0,1)',effects = 'normal(0,1)', nlpar = T)))
   # }

   # return the list
   out <- nlist(formula, data)
   return(out)
}
