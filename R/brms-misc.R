# The functions below are retrieved from the `brms` package source code.
# https://github.com/paul-buerkner/brms
# Copyright holder: Paul-Christian Bürkner under GPL-2.0 License

#' Execute a Function Call
#'
#' Execute a function call similar to \code{\link{do.call}}, but without
#' deparsing function arguments. For large number of arguments (i.e., more
#' than a few thousand) this function currently is somewhat inefficient
#' and should be used with care in this case.
#'
#' @param what Either a function or a non-empty character string naming the
#'   function to be called.
#' @param args A list of arguments to the function call. The names attribute of
#'   \code{args} gives the argument names.
#' @param pkg Optional name of the package in which to search for the
#'   function if \code{what} is a character string.
#' @param envir An environment within which to evaluate the call.
#'
#' @return The result of the (evaluated) function call.
#'
#' @details
#' code adapted from [brms]
#'
#'
#' @keywords internal
#' @export
do_call <- function(what, args, pkg = NULL, envir = parent.frame()) {
  call <- ""
  if (length(args)) {
    if (!is.list(args)) {
      stop("'args' must be a list.")
    }
    fun_args <- names(args)
    if (is.null(fun_args)) {
      fun_args <- rep("", length(args))
    } else {
      nzc <- nzchar(fun_args)
      fun_args[nzc] <- paste0("`", fun_args[nzc], "` = ")
    }
    names(args) <- paste0(".x", seq_along(args))
    call <- paste0(fun_args, names(args), collapse = ",")
  } else {
    args <- list()
  }
  if (is.function(what)) {
    args$.fun <- what
    what <- ".fun"
  } else {
    what <- paste0("`", what, "`")
    if (!is.null(pkg)) {
      what <- paste0(pkg, "::", what)
    }
  }
  call <- paste0(what, "(", call, ")")
  eval(call, envir = args, enclos = envir)
}

# check if x is a try-error resulting from try()
is_try_error <- function(x) {
  inherits(x, "try-error")
}

collapse <- function(..., sep = "") {
  paste(..., sep = sep, collapse = "")
}

collapse_comma <- function(...) {
  paste0("'", ..., "'", collapse = ", ")
}


# create a named list using object names
nlist <- function(...) {
  m <- match.call()
  dots <- list(...)
  no_names <- is.null(names(dots))
  has_name <- if (no_names) FALSE else nzchar(names(dots))
  if (all(has_name)) return(dots)
  nms <- as.character(m)[-1]
  if (no_names) {
    names(dots) <- nms
  } else {
    names(dots)[!has_name] <- nms[!has_name]
  }
  dots
}

# combine deparse lines into one string
# since R 4.0 we also have base::deparse1 for this purpose
deparse0 <- function(x, max_char = NULL, ...) {
  out <- collapse(deparse(x, ...))
  if (isTRUE(max_char > 0)) {
    out <- substr(out, 1L, max_char)
  }
  out
}
