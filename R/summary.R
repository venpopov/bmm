#' Create a summary of a fitted model represented by a \code{bmmfit} object
#'
#' @inheritParams brms::summary.brmsfit
#' @param backend Choose whether to display the *bmm* summary method (default),
#'   or to display the *brms* summary method.
#' @seealso \code{\link{summary.brmsfit}}
#' @note You can turn off the color output by setting the option
#' options(bmm.color_summary = FALSE) or bmm_options(color_summary = FALSE)
#' @export
summary.bmmfit <- function(object, priors = FALSE, prob = 0.95, robust = FALSE,  mc_se = FALSE, ..., backend = 'bmm') {
  backend <- match.arg(backend, c('bmm', 'brms'))

  # get summary object from brms, since it contains a lot of necessary information:
  out <- NextMethod('summary')
  if (backend == 'brms') {
    return(out)
  }

  # get the bmm specific information
  bmmargs <- object$bmm$fit_args
  bmmmodel <- object$bmm$model
  bmmform <- object$bmm$user_formula

  out$formula <- bmmform
  out$model <- bmmmodel
  out$data <- object$data

  # assign bmmsummary class to handle the printing
  class(out) <- 'bmmsummary'
  out
}


#' @export
print.bmmsummary <- function(x, digits = 2, color = getOption('bmm.color_summary', TRUE), ...) {
  options(bmm.color_summary = color)
  pars_to_print <- names(x$formula)
  # add check if the parameter is fixed by the user

  cat(style('#916af1')("  Model: "))
  cat(summarise_model(x$model, newline = TRUE, wsp = 9), "\n")
  cat(style('#916af1')("  Links: "))
  cat(summarise_links(x$model, x$formula), "\n")
  cat(style('#916af1')("Formula: "))
  cat(summarise_formula.bmmformula(x$formula, newline = TRUE, wsp = 9, model = x$model),'\n')
  cat(style('#916af1')("   Data:"), attr(x$data, 'data_name'),
      "(Number of observations:", paste0(nrow(x$data), ")"))
  cat("\n")
  total_ndraws <- ceiling((x$iter - x$warmup) / x$thin * x$chains)
  cat(paste0(
    style('#916af1')("  Draws: "), x$chains, " chains, each with iter = ", x$iter,
    "; warmup = ", x$warmup, "; thin = ", x$thin, ";\n",
    "         total post-warmup draws = ", total_ndraws, "\n\n"
  ))

  # if (length(x$splines)) {
  #   cat("Smoothing Spline Hyperparameters:\n")
  #   print_format(x$splines, digits)
  #   cat("\n")
  # }
  # if (length(x$gp)) {
  #   cat("Gaussian Process Hyperparameters:\n")
  #   print_format(x$gp, digits)
  #   cat("\n")
  # }
  # if (nrow(x$cor_pars)) {
  #   cat("Correlation Structures:\n")
  #   # TODO: better printing for correlation structures?
  #   print_format(x$cor_pars, digits)
  #   cat("\n")
  # }
  if (length(x$random)) {
    cat(style('darkgreen')("Multilevel Hyperparameters:\n"))
    for (i in seq_along(x$random)) {
      g <- names(x$random)[i]
      cat(paste0("~", g, " (Number of levels: ", x$ngrps[[g]], ") \n"))
      include <- sapply(paste0(pars_to_print,'_'), function(p) grepl(p, rownames(x$random[[g]])))
      include <- apply(include, 1, any)
      reduced <- x$random[[g]][include,]
      print_format(reduced, digits)
      cat("\n")
    }
  }
  if (nrow(x$fixed)) {
    cat(style('darkgreen')("Regression Coefficients:\n"))
    include <- sapply(paste0(pars_to_print,'_'), function(p) grepl(p, rownames(x$fixed)))
    include <- apply(include, 1, any)
    reduced <- x$fixed[include,]
    is_constant <- is.na(reduced$Rhat)
    print_format(reduced[!is_constant,], digits)
    cat("\n")
  }

  if (sum(is_constant)) {
    cat(style('darkgreen')("Additional Fixed Constant Parmaters:\n"))
    res <- reduced[is_constant,1]
    const_names <- rownames(reduced[is_constant,])
    res <- data.frame("Value" = res, row.names = paste0(const_names, "    "))
    print_format(res, digits)
    cat("\n")
  }
  # if (length(x[["mo"]])) {
  #   cat("Monotonic Simplex Parameters:\n")
  #   print_format(x[["mo"]], digits)
  #   cat("\n")
  # }
  # if (nrow(x$spec_pars)) {
  #   cat("Further Distributional Parameters:\n")
  #   print_format(x$spec_pars, digits)
  #   cat("\n")
  # }
  # if (length(x$rescor_pars)) {
  #   cat("Residual Correlations: \n")
  #   print_format(x$rescor, digits)
  #   cat("\n")
  # }
  cat(paste0("Draws were sampled using ", x$sampler, ". "))
  if (x$algorithm == "sampling") {
    cat(paste0(
      "For each parameter, Bulk_ESS\n",
      "and Tail_ESS are effective sample size measures, ",
      "and Rhat is the potential\n",
      "scale reduction factor on split chains ",
      "(at convergence, Rhat = 1)."
    ))
  }
  cat("\n")
  invisible(x)
}

summarise_links <- function(model, formula) {
  return(invisible(NULL))
}

summarise_formula.bmmformula <- function(formula, newline = TRUE, wsp=0, model = NULL) {
  fixpars <- NULL
  wspace <- collapse(rep(' ', wsp))
  sep <- paste0(ifelse(newline, '\n', ','), wspace)
  if (!is.null(model)) {
    fixpars <- model$info$fixed_parameters
    # TODO: abstract this from here and summarize_model
    fpnames <- names(fixpars)
    fpforms <- sapply(fpnames, function(fpar) {
      collapse(fpar, " = ", fixpars[[fpar]], sep)
    })
  }

  paste0(fpforms, paste0(formula, collapse=sep))
}

summarise_model <- function(model, ...) {
  UseMethod('summarise_model')
}

# TODO: build this up
#' @export
summarise_model.bmmmodel <- function(model, ...) {
  construct_model_call(model, ...)
}

# TODO: add nice coloring with crayon; add option to disable coloring
# creates a string representation of the call to the model with the user variables
# will likely also depend on the theme users have
construct_model_call <- function(model, newline = FALSE, wsp = 1, ...) {
  classes <- class(model)
  spec <- classes[length(classes)]

  # create necessary padding
  wspace <- collapse(rep(' ', wsp+nchar(spec)+1))
  sep <- paste0(ifelse(newline, ',\n', ','), wspace)

  # construct the inner part of the call
  rvars <- model$resp_vars
  ovars <- model$other_vars
  allvars <- c(rvars, ovars)
  vnames <- names(allvars)
  args <- sapply(vnames, function(var) {
    paste0(var, " = ", paste0(deparse(allvars[[var]]), collapse = ', '))
  })

  # combine with the padding and the function name
  args <- paste0(args, collapse = sep)
  paste0(style('coral2')(spec), "(", args, ")")
}

# helper function to print summary matrices in nice format
# also displays -0.00 as a result of round negative values to zero (#263)
# @param x object to be printed; coerced to matrix
# @param digits number of digits to show
# @param no_digits names of columns for which no digits should be shown
# exported from brms
print_format <- function(x, digits = 2, no_digits = c("Bulk_ESS", "Tail_ESS")) {
  x <- as.matrix(x)
  digits <- as.numeric(digits)
  if (length(digits) != 1L) {
    stop2("'digits' should be a single numeric value.")
  }
  out <- x
  fmt <- paste0("%.", digits, "f")
  for (i in seq_cols(x)) {
    if (isTRUE(colnames(x)[i] %in% no_digits)) {
      out[, i] <- sprintf("%.0f", x[, i])
    } else {
      out[, i] <- sprintf(fmt, x[, i])
    }
  }
  print(out, quote = FALSE, right = TRUE)
  invisible(x)
}

style <- function(...) {
  if (getOption('bmm.color_summary', TRUE)) {
    crayon::make_style(...)
  } else {
    function(x) x
  }
}
