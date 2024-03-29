#############################################################################!
# MODELS                                                                 ####
#############################################################################!


.model_imm <-
  function(resp_error = NULL, nt_features = NULL, nt_distances = NULL,
           set_size = NULL, regex = FALSE, links = NULL, version = "full",
           call = NULL, ...) {
    out <- structure(
      list(
        resp_vars = nlist(resp_error),
        other_vars = nlist(nt_features, nt_distances, set_size),
        domain = "Visual working memory",
        task = "Continuous reproduction",
        name = "Interference measurement model by Oberauer and Lin (2017).",
        version = version,
        citation = glue(
          "Oberauer, K., & Lin, H.Y. (2017). An interference model \\
          of visual working memory. Psychological Review, 124(1), 21-59"
        ),
        requirements = glue(
          '- The response vairable should be in radians and \\
          represent the angular error relative to the target
          - The non-target features should be in radians and be \\
          centered relative to the target'
        ),
        parameters = list(
          mu1 = glue(
            "Location parameter of the von Mises distribution for memory \\
            responses (in radians). Fixed internally to 0 by default."
          ),
          kappa = "Concentration parameter of the von Mises distribution",
          a = "General activation of memory items",
          c = "Context activation",
          s = "Spatial similarity gradient"
        ),
        links = list(
          mu1 = "tan_half",
          kappa = "log",
          a = "identity",
          c = "identity",
          s = "log"
        ),
        fixed_parameters = list(mu1 = 0, mu2 = 0, kappa2 = -100),
        default_priors = list(
          mu1 = list(main = "student_t(1, 0, 1)"),
          kappa = list(main = "normal(2, 1)", effects = "normal(0, 1)"),
          a = list(main = "normal(0, 1)", effects = "normal(0, 1)"),
          c = list(main = "normal(0, 1)", effects = "normal(0, 1)"),
          s = list(main = "normal(0, 1)", effects = "normal(0, 1)")
        ),
        void_mu = FALSE
      ),
      # attributes
      regex = regex,
      regex_vars = c('nt_features', 'nt_distances'),
      class = c("bmmodel", "vwm", "non_targets", "imm", paste0('imm_',version)),
      call = call
    )

    # add version specific information
    if (version == "abc") {
      out$parameters$s <- NULL
      out$links$s <- NULL
      out$default_priors$s <- NULL
      attributes(out)$regex_vars <- c('nt_features')
    } else if (version == "bsc") {
      out$parameters$a <- NULL
      out$links$a <- NULL
      out$default_priors$a <- NULL
    }

    out$links[names(links)] <- links
    out
  }

# user facing alias

#' @title `r .model_imm()$name`
#' @description Three versions of the `r .model_imm()$name` - the full, bsc, and abc.
#' `IMMfull()`, `IMMbsc()`, and `IMMabc()` are deprecated and will be removed in the future.
#' Please use `imm(version = 'full')`, `imm(version = 'bsc')`, or `imm(version = 'abc')` instead.
#'
#' @name imm
#' @details `r model_info(.model_imm(), components =c('domain', 'task', 'name', 'citation'))`
#' #### Version: `full`
#' `r model_info(.model_imm(version = "full"), components = c('requirements', 'parameters', 'fixed_parameters', 'links', 'prior'))`
#' #### Version: `bsc`
#' `r model_info(.model_imm(version = "bsc"), components = c('requirements', 'parameters', 'fixed_parameters', 'links', 'prior'))`
#' #### Version: `abc`
#' `r model_info(.model_imm(version = "abc"), components =c('requirements', 'parameters', 'fixed_parameters', 'links', 'prior'))`
#'
#' Additionally, all imm models have an internal parameter that is fixed to 0 to
#' allow the model to be identifiable. This parameter is not estimated and is not
#' included in the model formula. The parameter is:
#'
#'   - b = "Background activation (internally fixed to 0)"
#'
#' @param resp_error The name of the variable in the provided dataset containing
#'   the response error. The response Error should code the response relative to
#'   the to-be-recalled target in radians. You can transform the response error
#'   in degrees to radian using the `deg2rad` function.
#' @param nt_features A character vector with the names of the non-target
#'   variables. The non_target variables should be in radians and be centered
#'   relative to the target. Alternatively, if regex=TRUE, a regular
#'   expression can be used to match the non-target feature columns in the
#'   dataset.
#' @param nt_distances A vector of names of the columns containing the distances
#'   of non-target items to the target item. Alternatively, if regex=TRUE, a regular
#'   expression can be used to match the non-target distances columns in the
#'   dataset. Only necessary for the `bsc` and `full` versions.
#' @param set_size Name of the column containing the set size variable (if
#'   set_size varies) or a numeric value for the set_size, if the set_size is
#'   fixed.
#' @param regex Logical. If TRUE, the `nt_features` and `nt_distances` arguments
#'   are interpreted as a regular expression to match the non-target feature
#'   columns in the dataset.
#' @param links A list of links for the parameters. *Currently does not affect
#'   the model fits, but it will in the future.*
#' @param version Character. The version of the IMM model to use. Can be one of
#'  `full`, `bsc`, or `abc`. The default is `full`.
#' @param ... used internally for testing, ignore it
#' @return An object of class `bmmodel`
#' @keywords bmmodel
#' @examples
#' \dontrun{
#' # load data
#' data <- oberauer_lin_2017
#'
#' # define formula
#' ff <- bmmformula(
#'   kappa ~ 0 + set_size,
#'   c ~ 0 + set_size,
#'   a ~ 0 + set_size,
#'   s ~ 0 + set_size
#' )
#'
#' # specify the full IMM model with explicit column names for non-target features and distances
#' # by default this fits the full version of the model
#' model1 <- imm(resp_error = "dev_rad",
#'               nt_features = paste0('col_nt', 1:7),
#'               nt_distances = paste0('dist_nt', 1:7),
#'               set_size = 'set_size')
#'
#' # fit the model
#' fit <- bmm(formula = ff,
#'            data = data,
#'            model = model1,
#'            cores = 4,
#'            backend = 'cmdstanr')
#'
#' # alternatively specify the IMM model with a regular expression to match non-target features
#' # this is equivalent to the previous call, but more concise
#' model2 <- imm(resp_error = "dev_rad",
#'               nt_features = 'col_nt',
#'               nt_distances = 'dist_nt',
#'               set_size = 'set_size',
#'               regex = TRUE)
#'
#' # fit the model
#' fit <- bmm(formula = ff,
#'            data = data,
#'            model = model2,
#'            cores = 4,
#'            backend = 'cmdstanr')
#'
#' # you can also specify the `bsc` or `abc` versions of the model to fit a reduced version
#' model3 <- imm(resp_error = "dev_rad",
#'               nt_features = 'col_nt',
#'               set_size = 'set_size',
#'               regex = TRUE,
#'               version = 'abc')
#' fit <- bmm(formula = ff,
#'            data = data,
#'            model = model3,
#'            cores = 4,
#'            backend = 'cmdstanr')
#'}
#' @export
imm <- function(resp_error, nt_features, nt_distances, set_size, regex = FALSE,
                links = NULL, version = "full", ...) {
  call <- match.call()
  dots <- list(...)
  if ("setsize" %in% names(dots)) {
    set_size <- dots$setsize
    warning("The argument 'setsize' is deprecated. Please use 'set_size' instead.")
  }
  if (version == "abc") {
    nt_distances <- NULL
  }
  stop_missing_args()
  .model_imm(resp_error = resp_error, nt_features = nt_features,
             nt_distances = nt_distances, set_size = set_size, regex = regex,
             links = links, version = version, call = call, ...)
}


# deprecated calls for specific versions

#' @rdname imm
#' @keywords bmmodel
#' @export
IMMfull <- function(resp_error, nt_features, nt_distances, set_size, regex = FALSE,
                    links = NULL, ...) {
  call <- match.call()
  dots <- list(...)
  warning("The function `IMMfull()` is deprecated. Please use `imm(version = 'full')` instead.")
  if ("setsize" %in% names(dots)) {
    set_size <- dots$setsize
    warning("The argument 'setsize' is deprecated. Please use 'set_size' instead.")
  }
  stop_missing_args()
  .model_imm(resp_error = resp_error, nt_features = nt_features,
             nt_distances = nt_distances, set_size = set_size, regex = regex,
             links = links, version = "full", call = call, ...)
}


#' @rdname imm
#' @keywords bmmodel
#' @export
IMMbsc <- function(resp_error, nt_features, nt_distances, set_size, regex = FALSE,
                   links = NULL, ...) {
  call <- match.call()
  dots <- list(...)
  warning("The function `IMMbsc()` is deprecated. Please use `imm(version = 'bsc')` instead.")
  if ("setsize" %in% names(dots)) {
    set_size <- dots$setsize
    warning("The argument 'setsize' is deprecated. Please use 'set_size' instead.")
  }
  stop_missing_args()
  .model_imm(resp_error = resp_error, nt_features = nt_features,
             nt_distances = nt_distances, set_size = set_size, regex = regex,
             links = links, version = "bsc", call = call, ...)
}

#' @rdname imm
#' @keywords bmmodel
#' @export
IMMabc <- function(resp_error, nt_features, set_size, regex = FALSE, links = NULL,
                   ...) {
  call <- match.call()
  dots <- list(...)
  warning("The function `IMMabc()` is deprecated. Please use `imm(version = 'abc')` instead.")
  if ("setsize" %in% names(dots)) {
    set_size <- dots$setsize
    warning("The argument 'setsize' is deprecated. Please use 'set_size' instead.")
  }
  stop_missing_args()
  .model_imm(resp_error = resp_error, nt_features = nt_features,
             set_size = set_size, regex = regex, links = links,
             version = "abc", call = call, ...)
}

#############################################################################!
# CHECK_DATA S3 methods                                                  ####
#############################################################################!
# A check_data.* function should be defined for each class of the model.
# If a model shares methods with other models, the shared methods should be
# defined in data-helpers.R. Put here only the methods that are specific to
# the model. See ?check_data for details

#' @export
check_data.imm_bsc <- function(model, data, formula) {
  data <- .check_data_imm_dist(model, data, formula)
  NextMethod("check_data")
}

#' @export
check_data.imm_full <- function(model, data, formula) {
  data <- .check_data_imm_dist(model, data, formula)
  NextMethod("check_data")
}

.check_data_imm_dist <- function(model, data, formula) {
  nt_distances <- model$other_vars$nt_distances
  max_set_size <- attr(data, 'max_set_size')

  stopif(!isTRUE(all.equal(length(nt_distances), max_set_size - 1)),
         "The number of columns for non-target distances in the argument \\
         'nt_distances' should equal max(set_size)-1})")

  # replace nt_distances
  data[,nt_distances][is.na(data[,nt_distances])] <- 999

  stopif(any(data[,nt_distances] < 0),
         "All non-target distances to the target need to be postive.")
  data
}

#############################################################################!
# CONFIGURE_MODEL METHODS                                                ####
#############################################################################!
# Each model should have a corresponding configure_model.* function. See
# ?configure_model for more information.

#' @export
configure_model.imm_abc <- function(model, data, formula) {
  # retrieve arguments from the data check
  max_set_size <- attr(data, 'max_set_size')
  lure_idx <- attr(data, "lure_idx_vars")
  nt_features <- model$other_vars$nt_features
  set_size_var <- model$other_vars$set_size

  # construct main brms formula from the bmm formula
  formula <- bmf2bf(model, formula) +
    brms::lf(kappa2 ~ 1) +
    brms::lf(mu2 ~ 1) +
    brms::nlf(theta1 ~ c + a) +
    brms::nlf(kappa1 ~ kappa)

  # additional internal terms for the mixture model formula
  kappa_nts <- paste0("kappa", 3:(max_set_size + 1))
  theta_nts <- paste0("theta", 3:(max_set_size + 1))
  mu_nts <- paste0("mu", 3:(max_set_size + 1))

  for (i in 1:(max_set_size - 1)) {
    formula <- formula +
      glue_nlf("{kappa_nts[i]} ~ kappa") +
      glue_nlf("{theta_nts[i]} ~ {lure_idx[i]} * a + (1 - {lure_idx[i]}) * (-100)") +
      glue_nlf("{mu_nts[i]} ~ {nt_features[i]}")
  }

  # define mixture family
  formula$family <- brms::mixture(brms::von_mises("tan_half"),
                                  brms::von_mises("identity"),
                                  nmix = c(1, max_set_size),
                                  order = "none")

  nlist(formula, data)
}


#' @export
configure_prior.imm_abc <- function(model, data, formula, user_prior, ...) {
  # retrieve arguments from the data check
  set_size_var <- model$other_vars$set_size
  a_preds <- rhs_vars(formula$pforms$a)
  prior <- NULL
  if (any(data$ss_numeric == 1) && !is.numeric(data[[set_size_var]]) &&
      set_size_var %in% a_preds) {
    prior <- brms::prior_("constant(0)",
                          class = "b",
                          coef = paste0(set_size_var, 1),
                          nlpar = "a")
  }
  prior
}


#' @export
configure_model.imm_bsc <- function(model, data, formula) {
  # retrieve arguments from the data check
  max_set_size <- attr(data, 'max_set_size')
  lure_idx <- attr(data, "lure_idx_vars")
  nt_features <- model$other_vars$nt_features
  set_size_var <- model$other_vars$set_size
  nt_distances <- model$other_vars$nt_distances

  # construct main brms formula from the bmm formula
  formula <- bmf2bf(model, formula) +
    brms::lf(kappa2 ~ 1) +
    brms::lf(mu2 ~ 1) +
    brms::nlf(theta1 ~ c) +
    brms::nlf(kappa1 ~ kappa) +
    brms::nlf(expS ~ exp(s))

  # additional internal terms for the mixture model formula
  kappa_nts <- paste0("kappa", 3:(max_set_size + 1))
  theta_nts <- paste0("theta", 3:(max_set_size + 1))
  mu_nts <- paste0("mu", 3:(max_set_size + 1))

  for (i in 1:(max_set_size - 1)) {
    formula <- formula +
      glue_nlf("{kappa_nts[i]} ~ kappa") +
      glue_nlf("{theta_nts[i]} ~ {lure_idx[i]} * exp(-expS*{nt_distances[i]})",
               " * c + (1 - {lure_idx[i]}) * (-100)") +
      glue_nlf("{mu_nts[i]} ~ {nt_features[i]}")
  }

  # define mixture family
  formula$family <- brms::mixture(brms::von_mises("tan_half"),
                                  brms::von_mises("identity"),
                                  nmix = c(1, max_set_size),
                                  order = "none")

  nlist(formula, data)
}

#' @export
configure_prior.imm_bsc <- function(model, data, formula, user_prior, ...) {
  # retrieve arguments from the data check
  set_size_var <- model$other_vars$set_size
  s_preds <- rhs_vars(formula$pforms$s)
  prior <- NULL
  if (any(data$ss_numeric == 1) && !is.numeric(data[[set_size_var]]) &&
      set_size_var %in% s_preds) {
    prior <- brms::prior_("constant(0)",
                          class = "b",
                          coef = paste0(set_size_var, 1),
                          nlpar = "s")
  }
  prior
}

#' @export
configure_model.imm_full <- function(model, data, formula) {
  # retrieve arguments from the data check
  max_set_size <- attr(data, 'max_set_size')
  lure_idx <- attr(data, "lure_idx_vars")
  nt_features <- model$other_vars$nt_features
  set_size_var <- model$other_vars$set_size
  nt_distances <- model$other_vars$nt_distances

  # construct main brms formula from the bmm formula
  formula <- bmf2bf(model, formula) +
    brms::lf(kappa2 ~ 1) +
    brms::lf(mu2 ~ 1) +
    brms::nlf(theta1 ~ c + a) +
    brms::nlf(kappa1 ~ kappa) +
    brms::nlf(expS ~ exp(s))

  # additional internal terms for the mixture model formula
  kappa_nts <- paste0("kappa", 3:(max_set_size + 1))
  theta_nts <- paste0("theta", 3:(max_set_size + 1))
  mu_nts <- paste0("mu", 3:(max_set_size + 1))

  for (i in 1:(max_set_size - 1)) {
    formula <- formula +
      glue_nlf("{kappa_nts[i]} ~ kappa") +
      glue_nlf("{theta_nts[i]} ~ {lure_idx[i]} * (exp(-expS*{nt_distances[i]})",
               " * c + a) + (1 - {lure_idx[i]}) * (-100)") +
      glue_nlf("{mu_nts[i]} ~ {nt_features[i]}")
  }


  # define mixture family
  formula$family <- brms::mixture(brms::von_mises("tan_half"),
                                  brms::von_mises("identity"),
                                  nmix = c(1, max_set_size),
                                  order = "none")

  nlist(formula, data)
}

#' @export
configure_prior.imm_full <- function(model, data, formula, user_prior, ...) {
  # retrieve arguments from the data check
  set_size_var <- model$other_vars$set_size
  s_preds <- rhs_vars(formula$pforms$s)
  a_preds <- rhs_vars(formula$pforms$a)
  prior <- brms::empty_prior()
  if (any(data$ss_numeric == 1) && !is.numeric(data[[set_size_var]]) &&
      set_size_var %in% a_preds) {
    prior <- brms::prior_("constant(0)",
                          class = "b",
                          coef = paste0(set_size_var, 1),
                          nlpar = "a")
  }
  if (any(data$ss_numeric == 1) && !is.numeric(data[[set_size_var]]) &&
      set_size_var %in% s_preds) {
    prior <- prior + brms::prior_("constant(0)",
                                  class = "b",
                                  coef = paste0(set_size_var, 1),
                                  nlpar = "s")
  }
  prior
}
