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


# like 'eval' but parses characters before evaluation
eval2 <- function(expr, envir = parent.frame(), ...) {
  if (is.character(expr)) {
    expr <- str2expression(expr)
  }
  eval(expr, envir, ...)
}
