#############################################################################!
# MODELS                                                                 ####
#############################################################################!
# see file 'R/bmm_model_mixture3p.R' for an example

.model_M3custom <- function(resp_cats = NULL, num_options = NULL, choice_rule = "softmax", links = NULL, ...) {
   # name the number of options in each response categories if no names are provided
   if (is.null(names(num_options))) names(num_options) <- resp_cats

   # save model information
   out <- list(
      resp_vars = nlist(resp_cats),
      other_vars = nlist(num_options, choice_rule, ...),
      links = links,
      info = list(
         domain = 'Working Memory (categorical)',
         task = 'n-AFC retrieval',
         name = 'The Memory Measurement Model by Oberauer & Lewandowsky (2019)',
         citation = 'Oberauer, K., & Lewandowsky, S. (2019). Simple measurement models for complex working-memory tasks. Psychological Review, 126.',
         version = 'custom',
         requirements = '- Provide names for variables specifying the number of responses in a set of response categories.\n
         - Specify activation sources for each response categories \n
         - Include at least an activation source "b" for all response categories \n
         - Predict the specified activation at least by a fixed intercept and any additional predictors from your data',
         parameters = list(
            b = "Background activation. This source of activation should be added to the
              activation function for each response category, and represents the background
              noise. This parameter is fixed for scaling, but needs to be included in all
              models."
         ),
         fixed_parameters = list(
            b = ifelse(choice_rule == "softmax", 0, 0.1)
         )
      ),
      void_mu = FALSE
   )
   class(out) <- c('bmmmodel','M3', 'M3custom')
   out
}


# user facing alias
# information in the title and details sections will be filled in
# automatically based on the information in the .model_M3()$info

#' @title `r .model_M3custom()$info$name`
#' @name M3
#'
#' @details
#'   #### Version: `M3custom`
#'   `r model_info(.model_M3custom(), components =c('domain', 'task', 'name', 'citation'))`
#'
#'
#' @param resp_cats The variable names that contain the number of responses for each of the
#'   response categories used for the M3.
#' @param num_options Either an integer vector of the same length as `resp_cats` if the number
#'   of candidates in the respective response categories are constant across all conditions
#'   in the experiment. Or a vector specifying the variable names that contain the number of
#'   candidates in each response category. The order of these variables should be in the
#'   same order as the names of the response categories passed to `resp_cats`
#' @param choice_rule The choice rule that should be used for the M3. The options are "softmax"
#'   or "luce". The "softmax" option implements the softmax normalization of activation into
#'   probabilities for choosing the different response categories. The "luce" option implements
#'   the normalization of the different activations over the sum of all activations without
#'   exponentiating them. For details on the differences of these choice rules please see
#'   the appendix of Oberauer & Lewandowsky (2019) "Simple measurement models for complex
#'   working memory tasks. Psychological Review"
#' @param links A named list that provides the link functions that should be used for all M3
#'   parameters used in the model calls. Current options for the link functions are: "identity",
#'   "log", and "logit". The "identity" link should be used for all parameters with an unbounded
#'   range from minus to plus infinity. The "log" link should be used for all parameters with a bounded
#'   range from zero to plus infinity (i.e., most activation sources). The "logit" link should be
#'   used for all parameters bounded between zero and one (i.e. proportional reductions in activations,
#'   such as filtering or removal)
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
M3custom <- function(resp_cats, num_options, choice_rule = "softmax", ...) {
   stop_missing_args()
   .model_M3custom(resp_cats = resp_cats, num_options = num_options, choice_rule = choice_rule, ...)
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
bmf2bf.M3 <- function(model, formula) {
   # retrieve required response arguments
   resp_cats <- model$resp_vars$resp_cats
   nOpt_idx_vars <- paste0("Idx_", resp_cats)
   names(nOpt_idx_vars) <- resp_cats

   # retrieve choice rule
   choice_rule <- tolower(model$other_vars$choice_rule)

   # add transformation to activations according to choice rules
   transform_act <- ifelse(choice_rule == "luce","log(","")
   end_act <- ifelse(choice_rule == "luce",")","")
   zero_Opt <- ifelse(model$other_vars$choice_rule == "softmax","(-100)","(exp(-100))")
   op_Nopts <- ifelse(model$other_vars$choice_rule == "softmax","+","*")
   trans_Nopts <- ifelse(model$other_vars$choice_rule == "softmax","log(","")
   end_Nopts <- ifelse(model$other_vars$choice_rule == "softmax",")","")

   # set the base brmsformula based
   brms_formula <- brms::bf(paste0("Y | trials(nTrials)", " ~", transform_act, nOpt_idx_vars[resp_cats[1]],"*(act", resp_cats[1],
                                   op_Nopts, trans_Nopts, model$other_vars$num_options[resp_cats[1]],end_Nopts,")",
                                   " + (1-",nOpt_idx_vars[resp_cats[1]],") *",zero_Opt,end_act),nl = TRUE)

   # for each dependent parameter, check if it is used as a non-linear predictor of
   # another parameter and add the corresponding brms function
   for (i in 2:length(resp_cats) ) {
      brms_formula <- brms_formula +
         glue_nlf(paste0("mu",resp_cats[i]), " ~", transform_act, nOpt_idx_vars[resp_cats[i]],"*(act", resp_cats[i],
                  op_Nopts, trans_Nopts, model$other_vars$num_options[resp_cats[i]],end_Nopts,")",
                  " + (1-",nOpt_idx_vars[resp_cats[i]],") *",zero_Opt,end_act)
   }

   links <- model$links
   if ("M3custom" %in% class(model)) {
      # for each dependent parameter, check if it is used as a non-linear predictor of
      # another parameter and add the corresponding brms function
      dpars <- names(formula)
      for (dpar in dpars[dpars %in% model$resp_vars$resp_cats]) {
         pform <- formula[[dpar]]
         deparse_form <- deparse(pform)
         split_form <- gsub("[[:space:]]", "", strsplit(deparse_form,"~")[[1]])

         for (par in names(par_links)) {
            if (is.numeric(par_links[[par]])) {
               replace <- as.character(links[[par]])
            } else {
               replace <- dplyr::case_when(links[[par]] == "log" ~ paste0(" exp(",par,") "),
                                           links[[par]] == "logit" ~ paste0(" inv_logit(",par,") "),
                                           TRUE ~ par)
            }
            par_name <- paste0("\\b", par, "\\b") # match whole word only
            split_form <- gsub(par_name,replace,split_form)
         }

         brms_formula <- brms_formula + glue_nlf("act",split_form[1],"~",split_form[2])
      }
   }

   brms_formula
}


#############################################################################!
# CONFIGURE_MODEL S3 METHODS                                             ####
#############################################################################!
# Each model should have a corresponding configure_model.* function. See
# ?configure_model for more information.

#' @export
configure_model.M3 <- function(model, data, formula) {
   # construct brms formula from the bmm formula
   bmm_formula <- formula
   formula <- bmf2bf(model, bmm_formula)

   # construct the family
   family <- brms::multinomial(refcat = NA)

   # construct the default prior
   prior <- fixed_pars_priors(model, partype = "nlpar", class = "b")
   # if (getOption("bmm.default_priors", TRUE)) {
   #    prior <- prior +
   #       set_default_prior(bmm_formula, data,
   #                         prior_list=list(kappa =list(main = 'normal(2,1)',effects = 'normal(0,1)', nlpar = T),
   #                                         c = list(main = 'normal(0,1)',effects = 'normal(0,1)', nlpar = T),
   #                                         s = list(main = 'normal(0,1)',effects = 'normal(0,1)', nlpar = T)))
   # }

   # return the list
   out <- nlist(formula, data, family, prior)
   return(out)
}
