# All code in this file was written by Paul-Christian Bürkner
# for the 'brms' package and is licensed under the GPL-2 license
# Copyright: Paul-Christian Bürkner

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

# find the name that 'x' had in a specific environment
substitute_name <- function(x, envir = parent.frame(), nchar = 50) {
  out <- substitute(x)
  out <- eval2(paste0("substitute(", out, ")"), envir = envir)
  if (missing(out)) {
    return(NULL)
  }
  substr(collapse(deparse(out)), 1, nchar)
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


# like 'eval' but parses characters before evaluation
eval2 <- function(expr, envir = parent.frame(), ...) {
  if (is.character(expr)) {
    expr <- str2expression(expr)
  }
  eval(expr, envir, ...)
}


SW <- function(expr) {
  base::suppressWarnings(expr)
}

# coerce 'x' to a single integer value
as_one_integer <- function(x, allow_na = FALSE) {
  s <- substitute(x)
  x <- SW(as.integer(x))
  if (length(x) != 1L || anyNA(x) && !allow_na) {
    s <- deparse0(s, max_char = 100L)
    stop2("Cannot coerce '{s}' to a single integer value.")
  }
  x
}

# coerce 'x' to a single numeric value
as_one_numeric <- function(x, allow_na = FALSE) {
  s <- substitute(x)
  x <- SW(as.numeric(x))
  if (length(x) != 1L || anyNA(x) && !allow_na) {
    s <- deparse0(s, max_char = 100L)
    stop2("Cannot coerce '{s}' to a single numeric value.")
  }
  x
}

# coerce 'x' to a single character string
as_one_character <- function(x, allow_na = FALSE) {
  s <- substitute(x)
  x <- SW(as.character(x))
  if (length(x) != 1L || anyNA(x) && !allow_na) {
    s <- deparse0(s, max_char = 100L)
    stop2("Cannot coerce '{s}' to a single character value.")
  }
  x
}

# check if x is a try-error resulting from try()
is_try_error <- function(x) {
  inherits(x, "try-error")
}

seq_rows <- function(x) {
  seq_len(NROW(x))
}

seq_cols <- function(x) {
  seq_len(NCOL(x))
}

# indicate if cell mean coding should be disabled
no_cmc <- function(x) {
  isFALSE(attr(x, "cmc", exact = TRUE))
}

is_terms <- function(x) {
  inherits(x, "terms")
}

# validate a terms object (or one that can be coerced to it)
# for use primarily in 'get_model_matrix'
# @param x any R object
# @return a (possibly amended) terms object or NULL
#   if 'x' could not be coerced to a terms object
validate_terms <- function(x) {
  no_int <- no_int(x)
  no_cmc <- no_cmc(x)
  if (is_formula(x) && !is_terms(x)) {
    x <- terms(x)
  }
  if (!is_terms(x)) {
    return(NULL)
  }
  if (no_int || !has_intercept(x) && no_cmc) {
    # allows to remove the intercept without causing cell mean coding
    attr(x, "intercept") <- 1
    attr(x, "int") <- FALSE
  }
  x
}

is_atomic_or_null <- function(x) {
  is.atomic(x) || is.null(x)
}

# checks if the formula contains an intercept
has_intercept <- function(formula) {
  if (is_terms(formula)) {
    out <- as.logical(attr(formula, "intercept"))
  } else {
    formula <- as.formula(formula)
    try_terms <- try(terms(formula), silent = TRUE)
    if (is_try_error(try_terms)) {
      out <- FALSE
    } else {
      out <- as.logical(attr(try_terms, "intercept"))
    }
  }
  out
}

# find all namespace entries of a package, which are of
# a particular type for instance all exported objects
# retrieved from 'brms' source code, which in turn is retrieved from
# https://github.com/raredd/rawr
# @param package the package name
# @param what type of the objects to retrieve ("all" for all objects)
# @param pattern regex that must be matches by the object names
# @return a character vector of object names
lsp <- function(package, what = "all", pattern = ".*") {
  if (!is.character(substitute(package)))
    package <- deparse0(substitute(package))
  ns <- asNamespace(package)

  ## base package does not have NAMESPACE
  if (isBaseNamespace(ns)) {
    res <- ls(.BaseNamespaceEnv, all.names = TRUE)
    return(res[grep(pattern, res, perl = TRUE, ignore.case = TRUE)])
  } else {
    ## for non base packages
    if (exists('.__NAMESPACE__.', envir = ns, inherits = FALSE)) {
      wh <- get('.__NAMESPACE__.', inherits = FALSE,
                envir = asNamespace(package, base.OK = FALSE))
      what <- if (missing(what)) 'all'
      else if ('?' %in% what) return(ls(wh))
      else ls(wh)[pmatch(what[1], ls(wh))]
      if (!is.null(what) && !any(what %in% c('all', ls(wh))))
        stop('\'what\' should be one of ',
             paste0(shQuote(ls(wh)), collapse = ', '),
             ', or \'all\'', domain = NA)
      res <- sapply(ls(wh), function(x) getNamespaceInfo(ns, x))
      res <- rapply(res, ls, classes = 'environment',
                    how = 'replace', all.names = TRUE)
      if (is.null(what))
        return(res[grep(pattern, res, perl = TRUE, ignore.case = TRUE)])
      if (what %in% 'all') {
        res <- ls(getNamespace(package), all.names = TRUE)
        return(res[grep(pattern, res, perl = TRUE, ignore.case = TRUE)])
      }
      if (any(what %in% ls(wh))) {
        res <- res[[what]]
        return(res[grep(pattern, res, perl = TRUE, ignore.case = TRUE)])
      }
    } else stop(sprintf('no NAMESPACE file found for package %s', package))
  }
}

# Construct design matrices for brms models
# @param formula a formula object
# @param data A data frame created with model.frame.
#   If another sort of object, model.frame is called first.
# @param cols2remove names of the columns to remove from
#   the model matrix; mainly used for intercepts
# @param rename rename column names via rename()?
# @param ... passed to stats::model.matrix
# @return
#   The design matrix for the given formula and data.
#   For details see ?stats::model.matrix
get_model_matrix <- function(formula, data = environment(formula),
                             cols2remove = NULL, rename = TRUE, ...) {
  stopifnot(is_atomic_or_null(cols2remove))
  terms <- validate_terms(formula)
  if (is.null(terms)) {
    return(NULL)
  }
  if (no_int(terms)) {
    cols2remove <- union(cols2remove, "(Intercept)")
  }
  X <- stats::model.matrix(terms, data, ...)
  cols2remove <- which(colnames(X) %in% cols2remove)
  if (length(cols2remove)) {
    X <- X[, -cols2remove, drop = FALSE]
  }
  if (rename) {
    colnames(X) <- rename(colnames(X), check_dup = TRUE)
  }
  X
}

# indicate if the intercept should be removed
no_int <- function(x) {
  isFALSE(attr(x, "int", exact = TRUE))
}

# rename specified patterns in a character vector
# @param x a character vector to be renamed
# @param pattern the regular expressions in x to be replaced
# @param replacement the replacements
# @param fixed same as for 'gsub'
# @param check_dup: logical; check for duplications in x after renaming
# @param ... passed to 'gsub'
# @return renamed character vector of the same length as x
rename <- function(x, pattern = NULL, replacement = NULL,
                   fixed = TRUE, check_dup = FALSE, ...) {
  pattern <- as.character(pattern)
  replacement <- as.character(replacement)
  if (!length(pattern) && !length(replacement)) {
    # default renaming to avoid special characters in coeffcient names
    pattern <- c(
      " ", "(", ")", "[", "]", ",", "\"", "'",
      "?", "+", "-", "*", "/", "^", "="
    )
    replacement <- c(rep("", 9), "P", "M", "MU", "D", "E", "EQ")
  }
  if (length(replacement) == 1L) {
    replacement <- rep(replacement, length(pattern))
  }
  stopifnot(length(pattern) == length(replacement))
  # avoid zero-length pattern error
  has_chars <- nzchar(pattern)
  pattern <- pattern[has_chars]
  replacement <- replacement[has_chars]
  out <- x
  for (i in seq_along(pattern)) {
    out <- gsub(pattern[i], replacement[i], out, fixed = fixed, ...)
  }
  dup <- duplicated(out)
  if (check_dup && any(dup)) {
    dup <- x[out %in% out[dup]]
    stop2("Internal renaming led to duplicated names. \n",
          "Occured for: ", collapse_comma(dup))
  }
  out
}
