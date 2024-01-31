#' Measurement models available in `bmm`
#'
#' @return A character vector of measurement models available in `bmm`
#' @export
#'
#' @examples
#' supported_models()
supported_models <- function() {
  supported_models <- lsp("bmm", pattern = "^\\.model_")
  supported_models <- sub("^\\.model_", "", supported_models)
  return(supported_models)
}

#' Generate a markdown list of the measurement models available in `bmm`
#'
#' @return Markdown code for printing the list of measurement models available in `bmm`
#' @export
#'
print_pretty_models_md <- function() {
  ok_models <- supported_models()
  domains <- c()
  models <- c()
  for (model in ok_models) {
    domains <- c(domains, attr(get(paste0('.model_', model))(), 'domain'))
    models <- c(models, attr(get(paste0('.model_', model))(), 'name'))
  }
  unique_domains <- unique(domains)
  for (dom in unique_domains) {
    cat('####', dom, '\n\n')
    dom_models <- unique(models[domains == dom])
    for (model in dom_models) {
      cat('*', model, '\n\n')
    }
  }
}


# TODO: add citation attribute to each model

.model_2p <- function() {
  out <- list()
  attr(out, "domain") <- "Visual working memory"
  attr(out, "name") <- "Two-parameter mixture model by Zhang and Luck (2008)."
  out
}

.model_3p <- function() {
  out <- list()
  attr(out, "domain") <- "Visual working memory"
  attr(out, "name") <- "Three-parameter mixture model by Bays et al (2009)."
  out
}

.model_IMMabc <- function() {
  out <- list()
  attr(out, "domain") <- "Visual working memory"
  attr(out, "name") <- "Interference measurement model by Oberauer and Lin (2017)."
  out
}

.model_IMMbsc <- function() {
  out <- list()
  attr(out, "domain") <- "Visual working memory"
  attr(out, "name") <- "Interference measurement model by Oberauer and Lin (2017)."
  out
}

.model_IMMfull <- function() {
  out <- list()
  attr(out, "domain") <- "Visual working memory"
  attr(out, "name") <- "Interference measurement model by Oberauer and Lin (2017)."
  out
}

