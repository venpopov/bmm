#' @title Wrap angles that extend beyond (-pi;pi)
#'
#' @description On the circular space, angles can be only in the range (-pi;pi)
#' when subtracting angles, this can result in values outside of the range
#' this function wraps such values, so that they occur in the circle
#'
#' @param x a vector of angles to be wrapped. In radians (default)
#' @param radians is x in radians (default=TRUE) or degrees (FALSE)
#'
#' @return Vector
#'
#' @author Ven Popov

wrap <- function(x, radians=T) {
  if (radians) {
    return(((x+pi) %% (2*pi)) - pi)
  } else {
    return(((x+180) %% (2*180)) - 180)
  }
}
