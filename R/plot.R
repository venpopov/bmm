

#' @import ggplot2
#' @export
plot.brmsprior <- function(x, ...) {
  prior <- prep_brmsprior(x)



}

# preprocess a brmsprior object for plotting
prep_brmsprior <- function(x) {
  stopif(!brms::is.brmsprior(x), "x must be a brmsprior object")
  # remove empty priors
  priors <- x[x$prior != "",]

  # add mu to dpar if dpar is empty
  priors$dpar <- ifelse(priors$dpar == "" & priors$nlpar == "", "mu", priors$dpar)

  # remove constant priors
  priors <- priors[!grepl("constant", priors$prior),]

  priors$par <- ifelse(priors$nlpar != "", priors$nlpar, priors$dpar)
  priors
}


#' Plot a distribution object from the `distributional` package
#'
#' Uses `ggplot2` and `ggdist` to create a line plot of the probability density
#' function (pdf) or cumulative distribution function (cdf) of a distribution
#' object from the `distributional` package or a character vector of
#' distribution names of the type `distname(param1 = value1, param2 = value2)`
#'
#' @name plot-distribution
#'
#' @param x An object of class `distribution` from the `distributional` package
#' or a character vector of distribution names that can be parsed by
#' `ggdist::parse_dist()` of the type `distname(param1 = value1, param2 = value2)`
#' @param ... additional distribution objects to plot.
#' @param type The type of plot. One of "pdf" or "cdf".
#' @param facets Logical. If `TRUE`, the distributions are plotted in separate
#' facets. If `FALSE`, all distributions are plotted in the same plot. Default is
#' `FALSE`.
#' @param par_names Logical. If `TRUE`, the parameter names are included in the
#'  legend.
#' @param stat_slab_control A list of additional arguments passed to `ggdist::stat_slab()`
#'
#' @details By default all distributions are plotted as different lines in the
#' same plot. If you want to plot them in separate facets, set `facets = TRUE`.
#'
#' @return A ggplot object
#' @examples
#' x <- distributional::dist_normal(mean = 0, sd = c(0.5,1,1.2))
#' plot(x)
#'
#' x2 <- distributional::dist_wrap("sdm", mu = 0, c = c(1:10), kappa = 3)
#' plot(x2)
#'
#' plot('normal(0, 1)', 'normal(0, 2)', type = 'cdf')
#' plot(c('normal(0, 1)', 'student_t(3,0,2.5))', facets = T)
#' @keywords plot
#' @export
plot.distribution <- function(x, ..., type = 'pdf', facets = FALSE, par_names = FALSE,
                              stat_slab_control = list()) {
  dots <- list(...)
  if (length(dots) > 0) {
    stopif(any(!sapply(dots, inherits, what = "distribution")),
           "All additional arguments passed via ... must be of class 'distribution'")
    additional <- do.call(c, dots)
    x <- c(x, additional)
  }
  labels <- sapply(x, dist2string, par_names = par_names)
  labels <- factor(labels, levels = unique(labels))

  df <- data.frame(x = x, labels = labels)
  out <- ggplot(df) +
    aes(xdist = x, thickness = after_stat(eval(parse(text = type)))) +
    theme_dist() +
    theme(legend.position = "bottom")

  stat_slab_args <- stat_slab_control
  stat_slab_args$fill <- stat_slab_args$fill %||% NA
  stat_slab_args$subguide <- stat_slab_args$subguide %||%
    ggdist::subguide_outside(title = "density")
  stat_slab_args$scale <- stat_slab_args$scales %||% 1
  if (facets) {
    stat_slab_args$color <- stat_slab_args$color %||% "grey30"
    stat_slab_args$normalize <- stat_slab_args$normalize %||% "groups"
  } else {
    stat_slab_args$normalize <- stat_slab_args$normalize %||% "all"
  }

  out <- out + do.call(ggdist::stat_slab, stat_slab_args)

  if (facets) {
    out <- out + facet_wrap(~labels, scales = "free")
  } else {
    out <- out + aes(color = labels) + scale_color_discrete("")
  }

  out
}


#' @rdname plot-distribution
#' @export
plot.character <- function(x, ..., type = 'pdf', facets = FALSE, par_names = FALSE) {
  dots <- list(...)
  dists <- unlist(c(x, dots))
  stopifnot(all(is.character(dists)))
  dists <- ggdist::parse_dist(dists)
  plot(dists$.dist_obj, type = type, facets = facets, par_names = par_names)
}


# construct a string representation of a distribution object
dist2string <- function(x, par_names = FALSE) {
  family <- stats::family(x)
  parameters <- distributional::parameters(x)
  if (family == "wrap") {
    family <- parameters$dist
    parameters$dist <- NULL
  }
  if (!par_names) {
    parameters <- paste0(parameters, collapse = ", ")
  } else {
    parameters <- paste0(names(parameters), " = ", parameters, collapse = ", ")
  }
  out <- glue::glue("{family}({parameters})")
  out <- gsub("list\\(", "", out)
  out <- gsub("\\))", ")", out)
  out
}


#' ggplot theme for plotting distributions
#'
#' Based on `theme_ggdist()` from the `ggdist` package, but with the y-axis
#' removed and margins adjusted to make space for a custom y axis
#' @return A ggplot theme object
#' @keywords plot
#' @export
theme_dist <- function() {
  ggdist::theme_ggdist() +
  theme(plot.margin = margin(5.5, 5.5, 5.5, 50),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.title.y = element_blank())
}
