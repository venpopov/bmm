# internal function to combine the default prior with the user given prior
combine_prior <- function(config_args, user_prior) {
  if (!is.null(user_prior)) {
    default_prior <- config_args$prior
    combined_prior <- dplyr::anti_join(default_prior, user_prior, by=c('class', 'dpar','nlpar','coef','group','resp'))
    config_args$prior <- combined_prior+user_prior
  }
  return(config_args)
}
