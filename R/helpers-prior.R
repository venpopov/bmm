# internal function to combine two priors (e.g. the default prior with the user given prior)
# parts present in prior2 will overwrite the corresponding parts in prior1
combine_prior <- function(prior1, prior2) {
  if (!is.null(prior2)) {
    combined_prior <- dplyr::anti_join(prior1, prior2, by=c('class', 'dpar','nlpar','coef','group','resp'))
    prior <- combined_prior + prior2
  } else {
    prior <- prior1
  }
  return(prior)
}
