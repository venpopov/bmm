#' Create a summary of a fitted model represented by a \code{bmmfit} object
#'
#' @inheritParams brms::summary.brmsfit
#' @param backend Choose whether to display the *bmm* summary method (default),
#'  or to display the *brms* summary method.
#' @seealso \code{\link{summary.brmsfit}}
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
print.bmmsummary <- function(x, ...) {
  pars_to_print <- names(formula)
  # add check if the parameter is fixed by the user

  cat(style('#916af1')("  Model: "))
  cat(summarise_model(x$model, newline = TRUE, wsp = 9), "\n")
  cat(style('#916af1')("  Links: "))
  cat("\n")
  cat(style('#916af1')("Formula: "))
  cat(summarise_formula.bmmformula(x$formula, newline = TRUE, wsp = 9, model = x$model),'\n')
  cat(style('#916af1')("   Data:"), attr(x$data, 'data_name'),
      "(Number of observations:", paste0(nrow(x$data), ")"))
  cat("\n")
  cat(style('#916af1')("  Draws: "))
  cat("\n\n")
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



style <- function(...) {
  crayon::make_style(...)
}
