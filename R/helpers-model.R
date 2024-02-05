#' @title Generic S3 method for configuring the model to be fit by brms
#' @description Called by fit_model() to automatically construct the model
#'   formula, family objects and default priors for the model specified by the
#'   user. It will call the appropriate configure_model.* functions based on the
#'   list of classes defined in the .model_* functions. Currently, we have a
#'   method only for the last class listed in the .model_* functions. This is to
#'   keep model configuration as simple as possible. In the future we may add
#'   shared methods for classes of models that share the same configuration.
#' @param model A model list object returned from check_model()
#' @param data The user supplied data.frame containing the data to be checked
#' @param formula The user supplied formula
#' @param ... Additional arguments passed to the configure_model.* functions
#' @return A named list containing at minimum the following elements:
#'
#'  - formula: An object of class `brmsformula`. The constructed model formula
#'  - data: the user supplied data.frame, preprocessed by check_data
#'  - family: the brms family object
#'  - prior: the brms prior object
#'  - stanvars: (optional) An object of class `stanvars` (for custom families).
#'   See [brms::custom_family()] for more details.
#'
#' @details A bare bones configure_model.* method should look like this:
#'
#'  ``` r
#'  configure_model.newmodel <- function(model, data, formula) {
#'
#'     # preprocessing - e.g. extract arguments from data check, construct new variables
#'     <preprocessing code>
#'
#'     # construct the formula
#'     formula <- formula + <new terms>
#'
#'     # construct the family
#'     family <- <code for new family>
#'
#'     # construct the default prior
#'     prior <- <code for new prior>
#'
#'     # return the list
#'     out <- nlist(formula, data, family, prior)
#'     return(out)
#'  }
#'  ```
#' @examples
#' \dontrun{
#' configure_model.3p <- function(model, data, formula) {
#'    # retrieve arguments from the data check
#'    max_setsize <- attr(data, "max_setsize")
#'    non_targets <- attr(data, "non_targets")
#'    lure_idx_vars <- attr(data, "lure_idx_vars")
#'    setsize_var <- attr(data, "setsize_var")
#'
#'    # names for parameters
#'    kappa_nts <- paste0('kappa', 2:max_setsize)
#'    kappa_unif <- paste0('kappa',max_setsize + 1)
#'    theta_nts <- paste0('theta',2:max_setsize)
#'    mu_nts <- paste0('mu', 2:max_setsize)
#'    mu_unif <- paste0('mu', max_setsize + 1)
#'
#'    # construct formula
#'    formula <- formula +
#'      brms::lf(mu1 ~ 1) +
#'      glue_lf(kappa_unif,' ~ 1') +
#'      glue_lf(mu_unif, ' ~ 1') +
#'      brms::nlf(theta1 ~ thetat) +
#'      brms::nlf(kappa1 ~ kappa)
#'    for (i in 1:(max_setsize-1)) {
#'      formula <- formula +
#'        glue_nlf(kappa_nts[i], ' ~ kappa') +
#'        glue_nlf(theta_nts[i], ' ~ ', lure_idx_vars[i], '*(thetant + log(inv_ss)) + ',
#'                 '(1-', lure_idx_vars[i], ')*(-100)') +
#'        glue_nlf(mu_nts[i], ' ~ ', non_targets[i])
#'    }
#'
#'    # define mixture family
#'    vm_list = lapply(1:(max_setsize+1), function(x) brms::von_mises(link="identity"))
#'    vm_list$order = "none"
#'    family <- brms::do_call(brms::mixture, vm_list)
#'
#'    # define prior
#'    prior <-
#'      brms::prior_("constant(0)", class = "Intercept", dpar = "mu1") +
#'      brms::prior_("constant(0)", class = "Intercept", dpar = mu_unif) +
#'      brms::prior_("constant(-100)", class = "Intercept", dpar = kappa_unif) +
#'      brms::prior_("normal(2, 1)", class = "b", nlpar = "kappa") +
#'      brms::prior_("logistic(0, 1)", class = "b", nlpar = "thetat") +
#'      brms::prior_("logistic(0, 1)", class = "b", nlpar = "thetant")
#'
#'    # if there is setsize 1 in the data, set constant prior over thetant for setsize1
#'    if ((1 %in% data$ss_numeric) && !is.numeric(data[[setsize_var]])) {
#'      prior <- prior +
#'        brms::prior_("constant(-100)", class="b", coef = paste0(setsize_var, 1), nlpar="thetant")
#'    }
#'
#'    out <- nlist(formula, data, family, prior)
#'    return(out)
#' }
#' }
#'
#' @export
#' @keywords internal
configure_model <- function(model, data, formula) {
  UseMethod("configure_model")
}

#############################################################################!
# HELPER FUNCTIONS                                                       ####
#############################################################################!

#' Measurement models available in `bmm`
#'
#' @param print_call Logical; If TRUE (default), the function will print information about
#'  how each model function should be called and its required arguments. If FALSE,
#'  the function will return a character vector with the names of the available
#'  models
#' @return A character vector of measurement models available in `bmm`
#' @export
#'
#' @examples
#' supported_models()
supported_models <- function(print_call=TRUE) {
  supported_models <- lsp("bmm", pattern = "^\\.model_")
  supported_models <- sub("^\\.model_", "", supported_models)
  if (print_call) {
    out <- "The following models are supported:\n\n"
    for (model in supported_models) {
      args <- methods::formalArgs(get(model))
      args <- args[!args %in% c("...")]
      args <- collapse_comma(args)
      args <- gsub("'", "", args)
      out <- paste0(out, '- `', model,'(',args,')`', "\n", sep='')
    }
    out <- paste0(out, "\nType `?modelname` to get information about a specific model, e.g. `?IMMfull`\n")
    cat(gsub("`", " ", out))
    return(invisible(out))
  }
  return(supported_models)
}

#' @title Generate a markdown list of the measurement models available in `bmm`
#' @description Used internally to automatically populate information in the README file
#' @return Markdown code for printing the list of measurement models available in `bmm`
#' @export
#' @keywords internal
print_pretty_models_md <- function() {
  ok_models <- supported_models(print_call=FALSE)
  domains <- c()
  models <- c()
  for (model in ok_models) {
    m <- get_model(model)
    args_list <- formals(m)
    test_args <- lapply(args_list, function(x) {NULL})
    m <- brms::do_call(m, test_args)
    domains <- c(domains, m$info$domain)
    models <- c(models, m$info$name)
  }
  unique_domains <- unique(domains)
  for (dom in unique_domains) {
    cat('####', dom, '\n\n')
    dom_models <- unique(models[domains == dom])
    for (model in dom_models) {
      cat('*', model, '\n\n')
    }
  }
}

# used to extract well formatted information from the model object to print
# in the @details section for the documentation of each model
model_info <- function(model, components = 'all') {
  UseMethod("model_info")
}


#' @export
model_info.bmmmodel <- function(model, components = 'all') {
  pars <- model$info$parameters
  par_info <- ""
  if (length(pars) > 0) {
    for (par in names(pars)) {
      par_info <- paste0(par_info, "   - `", par, "`: ", pars[[par]], "\n")
    }
  }

  info_all <-   list(
    domain = paste0("* **Domain:** ", model$info$domain, "\n\n"),
    task = paste0("* **Task:** ", model$info$task, "\n\n"),
    name = paste0("* **Name:** ", model$info$name, "\n\n"),
    citation = paste0("* **Citation:** \n\n   - ", model$info$citation, "\n\n"),
    version = paste0("* **Version:** ", model$info$version, "\n\n"),
    requirements = paste0("* **Requirements:** \n\n  ", model$info$requirements, "\n\n"),
    parameters = paste0("* **Parameters:** \n\n  ", par_info, "\n\n")
  )

  if (length(components) == 1 && components == 'all') {
    components <- names(info_all)
  }

  if (model$info$version == "NA" || model$info$version == "") {
    components <- components[components != "version"]
  }

  # return only the specified components
  return(collapse(info_all[components]))
}

#' Checks if the model is supported, and returns the model function
#' @param model A string with the name of the model supplied by the user
#' @return A list generated by a model function of type .model_*
#' @noRd
check_model <- function(model) {
  model_label <- class(model)[length(class(model))]
  ok_models <- supported_models(print_call=FALSE)
  if (not_in(model_label, ok_models)) {
    stop(model_label, " is not a supported model. Supported ",
         "models are:\n", collapse_comma(ok_models))
  }

  return(model)
}



#' @param model A string with the name of the model supplied by the user
#' @return A function of type .model_*
#' @details the returned object is a function. To get the model object, call the
#'   returned function, e.g. `get_model("mixture2p")()`
#' @noRd
get_model <- function(model) {
  get(paste0('.model_', model), mode='function')
}

# same as get_model2, but with the new model structure for the user facing alias
get_model2 <- function(model) {
  get(model, mode='function')
}





#' Create a file with a template for adding a new model (for developers)
#'
#' @param model_name A string with the name of the model. The file will be named
#'  `bmm_model_model_name.R` and all necessary functions will be created with the
#'  appropriate names and structure. The file will be saved in the `R/` directory
#' @param testing Logical; If TRUE, the function will return the file content but
#'  will not save the file. If FALSE (default), the function will save the file
#'
#' @return If `testing` is TRUE, the function will return the file content as a
#'  string. If `testing` is FALSE, the function will return NULL
#'
#' @details If you get a warning during check() about non-ASCII characters,
#'  this is often due to the citation field. You can find what the problem is by
#'  running
#'  ```r
#'  remotes::install_github("eddelbuettel/dang")
#'  dang::checkPackageAsciiCode(dir = ".")
#'  ```
#'  usually rewriting the numbers (issue, page numbers) manually fixes it
#'
#' @export
#'
#' @examples
#' \dontrun{
#'  # create a new model file
#'  use_model_template("newmodel")
#'  # check the file
#'  file.show("R/bmm_model_newmodel.R")
#'}
use_model_template <- function(model_name, testing=FALSE) {
  file_name <- paste0('bmm_model_', model_name, '.R')
  # check if model exists
  if (model_name %in% supported_models(print_call=FALSE)) {
    stop(paste0("Model ", model_name, " already exists"))
  }
  if (file.exists(paste0('R/', file_name))) {
    stop(paste0("File ", file_name, " already exists"))
  }

  model_header <- paste0(
  "#############################################################################!\n",
  "# MODELS                                                                 ####\n",
  "#############################################################################!\n",
  "# see file 'R/bmm_model_mixture3p.R' for an example\n\n")


  check_data_header <- paste0(
  "\n\n\n#############################################################################!\n",
  "# CHECK_DATA S3 methods                                                  ####\n",
  "#############################################################################!\n",
  "# A check_data.* function should be defined for each class of the model.\n",
  "# If a model shares methods with other models, the shared methods should be\n",
  "# defined in data-helpers.R. Put here only the methods that are specific to\n",
  "# the model. See ?check_data for details\n\n\n")

  configure_model_header <- paste0(
  "\n\n\n#############################################################################!\n",
  "# CONFIGURE_MODEL S3 METHODS                                             ####\n",
  "#############################################################################!\n",
  "# Each model should have a corresponding configure_model.* function. See\n",
  "# ?configure_model for more information.\n\n\n")

  model_object <- glue::glue(".model_<<model_name>> <- function(required_arg1, required_arg2, ...) {\n",
                             "   out <- list(\n",
                             "      vars = nlist(required_arg1, required_arg2),\n",
                             "      info = list(\n",
                             "         domain = '',\n",
                             "         task = '',\n",
                             "         name = '',\n",
                             "         citation = '',\n",
                             "         version = '',\n",
                             "         requirements = '',\n",
                             "         parameters = list()\n",
                             "      ))\n",
                             "   class(out) <- c('bmmmodel', '<<model_name>>')\n",
                             "   out\n",
                             "}\n\n\n",
                             .open = "<<", .close = ">>")

  user_facing_alias <- glue::glue("# user facing alias\n",
                                  "# information in the title and details sections will be filled in\n",
                                  "# automatically based on the information in the .model_<<model_name>>()$info\n",
                                  "#' @title `r .model_<<model_name>>()$info$name`\n",
                                  "#' @details `r model_info(<<model_name>>(NA,NA))`\n",
                                  "#' @param required_arg1 A description of the required argument\n",
                                  "#' @param required_arg2 A description of the required argument\n",
                                  "#' @param ... used internally for testing, ignore it\n",
                                  "#' @return An object of class `bmmmodel`\n",
                                  "#' @export\n",
                                  "#' @examples\n",
                                  "#' \\dontrun{\n",
                                  "#' # put a full example here (see 'R/bmm_model_mixture3p.R' for an example)\n",
                                  "#' }\n",
                                  "<<model_name>> <- .model_<<model_name>>\n\n",
                                  .open = "<<", .close = ">>")

  check_data_method <- glue::glue("#' @export\n",
                                  "check_data.<<model_name>> <- function(model, data, formula) {\n",
                                  "   # retrieve required arguments\n",
                                  "   required_arg1 <- model$vars$required_arg1\n",
                                  "   required_arg2 <- model$vars$required_arg2\n\n\n",
                                  "   # check the data (required)\n\n\n",
                                  "   # compute any necessary transformations (optional)\n\n\n",
                                  "   # save some variables as attributes of the data for later use (optional)\n\n\n",
                                  "   data = NextMethod('check_data')\n\n",
                                  "   return(data)\n",
                                  "}\n\n",
                                  .open = "<<", .close = ">>")

  configure_model_method <- glue::glue("#' @export\n",
                                       "configure_model.<<model_name>> <- function(model, data, formula) {\n",
                                       "   # retrieve required arguments\n",
                                       "   required_arg1 <- model$vars$required_arg1\n",
                                       "   required_arg2 <- model$vars$required_arg2\n\n\n",
                                       "   # retrieve arguments from the data check\n",
                                       "   my_precomputed_var <- attr(data, 'my_precomputed_var')\n\n\n",
                                       "   # construct the formula\n",
                                       "   formula <- formula + brms::lf()\n\n\n",
                                       "   # construct the family\n",
                                       "   family <- NULL\n\n\n",
                                       "   # construct the default prior\n",
                                       "   prior <- NULL\n\n\n",
                                       "   # return the list\n",
                                       "   out <- nlist(formula, data, family, prior)\n",
                                       "   return(out)\n",
                                       "}\n\n",
                                       .open = "<<", .close = ">>")

  file_content <- paste0(model_header,
                             model_object,
                             user_facing_alias,
                             check_data_header,
                             check_data_method,
                             configure_model_header,
                             configure_model_method)

  if (!testing) {
    writeLines(file_content, paste0('R/', file_name))
    utils::file.edit(paste0('R/', file_name))
  } else {
    cat(file_content)
  }
}
