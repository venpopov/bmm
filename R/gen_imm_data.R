#' @title Generate artificial data from the Interference measurement model
#' @description Given a set of parameters, and desired setsize, thi function generates
#' data following the assumptions of the interference measurement models
#'
#' @param parms Numeric. vector or matrix of IMM parameters, must contain at least one row
#'    of values for:
#'      c = context activation,
#'      a = general activation,
#'      s = spatial similarity gradient,
#'      n = background noise,
#'      kappa = precision of memory representations
#' @param nResp Numeric. number of responses to simulate for each subject
#' @param setsize Numeric. Number of items in memory set
#' @param relative_resp Logical. if TRUE, returns response error relative to the
#'   target and all non-target item locations are coded relative to the target.
#'   If FALSE, returns actual response and the location of all items is absolute
#'
#' @return A data.frame object. y is the response (if relative_resp==F) or
#'   response error (if relative_resp==T), t_loc is the value of the target
#'   (only if relative_resp==F), nt1_loc to nti_loc, are values of the
#'   non-targets, where i=setsize-1.
#'
#' @export
gen_imm_data <- function(parms = c(c = 3, a = 1, s = 0.2, n = 0, kappa = 10), nResp = 200, setsize=6, relative_resp=T){

}
