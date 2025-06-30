#' Create a dynamic, formula-aware, and link-aware initialization function.
#'
#' This S3 method extracts the parameter names and dimensions from the brmsformula
#' generated for a `bmmodel` and accounts for the link function for each parameter
#' specified in the model object to provide maximum stability, flexibility, and robustness.
#'
#' @param model The `bmmodel` object.
#' @param data Your data frame.
#' @param formula The brms formula for your model.
#'
#' @return A new function that generates a safe, named list of initial values.
#'
#' @export
get_initfun <- function(object, data, brms_formula, init, ...) {
  UseMethod("get_initfun")
}

#' @export
get_initfun.default <- function(object, data, brms_formula, init, ...) {
  if(is.null(init)) {
    return(2)
  } else {
    return(init)
  }
}

#' @export
get_initfun.bmmodel <- function(model, data, brms_formula, init, ...) {
  if (!is.null(init)) {
    warning(glue("You have provided custom initial values for fitting your bmmmodel",
                 "These will overwrite the internally created initial values, that ensure proper sampling of bmmodels."))
    return(init)
  }

  # Extract names of parameters for generation of initial values
  stan_data <- standata(brms_formula, data)
  stan_code <- stancode(brms_formula,data)

  # Using sub to extract text between square brackets
  result <- sub(".parameters \\{(.*?)\\}.*", "\\1", stan_code)
  prior_data <- get_prior(brms_formula, data)
  prior_data <- subset(prior_data, class %in% c("Intercept","b"))
  prior_data$base_par <- paste0(prior_data$dpar,prior_data$nlpar)
  prior_data$par_name <- ifelse(nchar(prior_data$base_par) == 0, prior_data$class, paste(prior_data$class, prior_data$base_par, sep = "_"))
  prior_data$K_name   <- ifelse(nchar(prior_data$base_par) == 0,"K", paste("K",prior_data$base_par,sep = "_"))
  par_info <- prior_data[,c("base_par","par_name","K_name")]

  # create initfun
  custom_intifun <- function(chain_id = 1) {
    init_list <- list()

    # initialize fixed effects
    for (i in 1:nrow(par_info)) {
      par <- ifelse(nchar(par_info$base_par[i]) == 0, "mu",par_info$base_par[i])
      K_name <- par_info$K_name[i]
      par_name <- par_info$par_name[i]

      if ( (K_name %in% names(standata)) && !(par %in% names(model$fixed_parameters)) ) {
        # Get the link function for this parameter from the family object
        link_fun <- model$links[[par]]

        # Get value range for initial values
        if(grepl("Intercept",par_name)) { # parameter is intercept
          init_range <- model$init_ranges[[par]][["main"]]
          n_coefs <- 1
        } else if (grepl("b_",par_name) && paste0("Intercept_",par) %in% par_info$par_name) { # parameter is effect
          init_range <- model$init_ranges[[par]][["effects"]]
          n_coefs <- standata[[K_name]]
        } else { # no intercept in formula, need to detect which b_par is intercept and which is effect

        }

        # Transform ranges for initial values according to the link function
        init_range <- switch(
          link_fun,
          identity = init_range,
          log      = log(init_range),
          logit    = qlogis(init_range),
          {
            warning(paste("Unrecognized link for", par, ". Using log-scale init."))
            log(init_range)
          }
        )

        # Assign to init_list
        init_list[[par_name]] <- runif(n_coefs, init_range[1], init_range[2])
      }
    }

    # --- Initialize Random Effects ('sd', 'z') ---
    # This logic remains the same, as it's already robust.
    M_groups <- names(standata)[grepl("M_", names(standata))]
    M_groups <- as.numeric(gsub("M_","",M_groups))

    for (i in M_groups) {
      M <- standata[[paste0("M_", i)]]
      N <- standata[[paste0("N_", i)]]

      init_list[[paste0("sd_", i)]] <- runif(M, 0.01, 0.1) # Tiny but random sd
      init_list[[paste0("z_", i)]] <- matrix(rnorm(M * N, 0, 0.01), nrow = M, ncol = N) # Tiny variance in z
    }

    # Initialize covariance matrices ('L')
    L_groups <- names(standata)[grepl("L_", names(standata))]
    L_groups <- as.numeric(gsub("L_","",L_groups))

    for (i in L_groups) {
      init_list[[paste0("L_", i)]] <- diag(M) # Start with no correlation
    }

    return(init_list)
  }

  return(custom_intifun)
}
