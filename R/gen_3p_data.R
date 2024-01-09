#' @title Generate artificial data from the Bays et al (2009) 3-parameter
#'   mixture model
#' @description Given a set of parameters, and desired setsize, it generates
#'   responses in a continuous reproduction task, with corresponding proportions
#'   coming from memory, non-target location errors, or guessing.
#'
#' @param N Numeric. Number of samples
#' @param pmem Numeric. Probability of response coming from memory
#' @param pnt Numeric. Probability of response coming from a non-target item
#' @param kappa Numeric. Precision of the von mises distribution
#' @param setsize Numeric. Number of presented items
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
#'
#' @examples
#' # example code
#' nsub = 30
#' df_3p_parms <- data.frame(
#'   theta_pmem = rnorm(nsub, mean = 2, sd = 1),
#'   theta_pnt = rnorm(nsub, mean = 1, sd = 0.5),
#'   kappa = pmax(0,rnorm(nsub, mean = 5, sd = 1)).
#'   pmem = numeric(),
#'   pnt = numeric(),
#'   pguess = numeric()
#' )
#'
#' # transform continous mixture weights into probabilities using the softmax
#' df_3p_parms[,c("pmem","pnt","pguess")] <- apply(df_3p_parms[,c("theta_pmem","theta_pnt")],1,utilities::softmax)
#'
gen_3p_data <- function(N=2000, pmem=0.6, pnt=0.3, kappa=10, setsize=2, relative_resp=T) {
  t_loc <- NULL
  resample <- function(x, ...) x[sample.int(length(x), ...)]

  # set parameters
  pguess = 1-pmem-pnt   # probability of guessing

  # uniformly distributed  locations on the circle
  locations <- matrix(0, nrow = N, ncol = setsize)
  for (i in 1:setsize) {
    locations[,i] <- brms::rvon_mises(N, 0, 0)
  }

  idx_t <- 1:(round(pmem*N))                                   # which responses are target based
  target_resp <- brms::rvon_mises(round(pmem*N),               # vector of target reponses
                                  locations[idx_t,1],
                                  kappa)
  if (pnt > 0) {
    Npnt <- round(pnt*N)
    idx_nt <- (round(pmem*N)+1):(round(pmem*N)+Npnt)           # which responses are non-target
    idx_loc <- resample(2:setsize, Npnt, replace=T)              # which lure generated the response
    idx_select <- cbind(idx_nt, idx_loc)
    nontarget_resp <- brms::rvon_mises(round(pnt*N),           # vector of nontarget responses
                                       locations[idx_select],
                                       kappa)
  } else {
    nontarget_resp <- c()
  }

  if (pguess > 0) {
    guess_resp <- brms::rvon_mises(N-round(pmem*N)-round(pnt*N), 0, 0) # vector of guessing responses
  } else {
    guess_resp <- c()
  }

  # put the data together in a data.frame
  dat <- data.frame(y = c(target_resp, nontarget_resp, guess_resp))
  dat <- do.call(cbind, list(dat, as.data.frame(locations)))
  names(dat) <- c('y','t_loc', paste0('nt',1:(setsize-1),'_loc'))


  # if a relative response is requested
  # recode responses such that the response is the error relative to the target
  # also recode nt_loc, such that it is relative to the target as well
  if (relative_resp) {
    dat <- bmm::wrap(dat-dat$t_loc)
    dat <- dplyr::select(dat, -t_loc)
  }
  return(dat)
}
