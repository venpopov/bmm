#' @title Wrap angles that extend beyond (-pi;pi)
#'
#' @description On the circular space, angles can be only in the range (-pi;pi or -180;180).
#'   When subtracting angles, this can result in values outside of this range. For example,
#'   when calculating the difference between a value of 10 degrees minus 340 degrees, this
#'   results in a difference of 330 degrees. However, the true difference between these
#'   two values is -30 degrees.
#'   This function wraps such values, so that they occur in the circle
#'
#' @param x A numeric vector, matrix or data.frame of angles to be wrapped. In
#'   radians (default) or degrees.
#' @param radians Logical. Is x in radians (default=TRUE) or degrees (FALSE)
#'
#' @return An object of the same type as x
#' @export
#'
#' @examples
#' x <- runif(1000, -pi, pi)
#' y <- runif(1000, -pi, pi)
#' diff <- x-y
#' hist(diff)
#' wrapped_diff <- wrap(x-y)
#' hist(wrapped_diff)
#'
wrap <- function(x, radians=TRUE) {
  stopifnot(is.logical(radians))
  if (radians) {
    return(((x+pi) %% (2*pi)) - pi)
  } else {
    return(((x+180) %% (2*180)) - 180)
  }
}
