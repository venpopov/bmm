#' Plot a distribution object from the `distributional` package
#'
#' Uses `ggplot2` and `ggdist` to create a line plot of the probability density
#' function (pdf) or cumulative distribution function (cdf) of a distribution
#' object from the `distributional` package or a character vector of
#' distribution names of the type `distname(param1 = value1, param2 = value2)`
#'
#' @name plot-distribution
#'
#' @param x Several options. One of:
#' - An object of class `distribution` from the `distributional` package
#' - A character vector of distribution names that can be parsed by
#'   `ggdist::parse_dist()` of the type `distname(param1 = value1, param2 =
#'   value2)`
#' - A `brmsprior` object produced by [brms::set_prior], [brms::default_prior] or
#'   [brms::prior_summary()]
#' @param ... additional distribution objects to plot.
#' @param formula A `brmsformula` object. Only needed for plotting `brmsprior`
#'   objects if transformation of the priors is wanted via the specified links
#'   in the `brmsfamily`. The `brmsformula` must contain the `brmsfamily`
#' @param links A named list of links for the parameters. This is an alternative
#'   way to specify the links for the parameters. Only one of `links` or
#'   `formula` can be provided.
#' @param transform Logical. If `TRUE`, the priors are transformed according to
#'   the inverse of the link functions present either in the `brmsformula` or
#'   the `links` argument. Default is `FALSE`.
#' @param type The type of plot. One of "pdf" or "cdf".
#' @param labels A character vector of labels for the distributions. If `NULL`,
#'   the labels are automatically generated from the distribution objects. If a
#'   character vector is provided, it must be the same length as the number of
#'   distributions being plotted. Default is `NULL`.
#' @param facets Logical. If `TRUE`, the distributions are plotted in separate
#'   facets. If `FALSE`, all distributions are plotted in the same plot. Default
#'   is `FALSE`.
#' @param stat_slab_control A list of additional arguments passed to
#'   `ggdist::stat_slab()`
#' @param packages A character vector of package names to search for
#'   distributions
#'
#' @details These are generic methods for plotting distributions specified as
#' - `distribution` objects from the `distributional` package
#' - character vectors of distribution names that can be parsed by [ggdist::parse_dist()]
#' - `brmsprior` objects produced by [brms::set_prior], [brms::default_prior] or
#'  [brms::prior_summary()]
#'
#'  For `brmsprior` objects, you can also plot the transformations of the priors
#'  to the native scale of the parameters by setting `transform = TRUE`. This
#'  will transform the priors according to the inverse of the link functions
#'  present either in the `brmsformula` or the `links` argument. For example, if
#'  the prior is specified as `normal(0, 1)` and the link function is `log`, as
#'  it is often the case for parameters that need to be strictly positive, the
#'  prior will be transformed to `lognormal(0, 1)` by applying `exp()` to the
#'  distribution object.
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
#' plot(c('normal(0, 1)', 'student_t(3,0,2.5)'), facets = T)
#'
#' plot(distributional::dist_beta(1:10, 1)) + theme(legend.position = 'right')
#' @keywords plot
#' @export
plot.distribution <- function(x, ..., type = 'pdf', labels = NULL, facets = FALSE,
                              stat_slab_control = list()) {
  if (!requireNamespace('extraDistr')) {
    stop2("The 'extraDistr' package is required for plotting distributions")
  }
  dots <- list(...)
  if (length(dots) > 0) {
    stopif(any(!sapply(dots, inherits, what = "distribution")),
           "All additional arguments passed via ... must be of class 'distribution'")
    additional <- do.call(c, dots)
    x <- c(x, additional)
  }

  labels <- labels %||% format(x)
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
  stat_slab_args$subscale <- stat_slab_args$subscale %||%
    ggdist::subscale_thickness(expand = expansion(c(0, 0.1)))
  stat_slab_args$scale <- stat_slab_args$scale %||% 1
  stat_slab_args$p_limits <- stat_slab_args$p_limits %||% c(0.001, 0.999)
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
plot.character <- function(x, ..., type = 'pdf', labels = NULL, facets = FALSE,
                           stat_slab_control = list(),
                           packages = c("brms","extraDistr","bmm","ggdist")) {
  search_env <- pkg_search_env(packages)
  dots <- list(...)
  dists <- unlist(c(x, dots))
  stopifnot(all(is.character(dists)))
  dists <- ggdist::parse_dist(dists, package = search_env)
  plot(dists$.dist_obj, type = type, labels = labels, facets = facets,
       stat_slab_control = stat_slab_control)
}


#' @import ggplot2
#' @rdname plot-distribution
#' @export
plot.brmsprior <- function(x, formula = NULL, links = NULL, transform = FALSE,
                           type = 'pdf', facets = TRUE, stat_slab_control = list(),
                           packages = c("brms","extraDistr","bmm","ggdist"), ...) {
  prior <- prep_brmsprior(x, formula = formula, links = links)
  prior <- ggdist::parse_dist(prior, package = pkg_search_env(packages))

  if (transform) {
    stopif(is.null(formula) && is.null(links) && !("link" %in% names(prior)),
           glue("If transform = TRUE, either formula or links must be provided, \\
                or there must be a 'link' column in the brmsprior object"))
    for (i in 1:nrow(prior)) {
      prior$.dist_obj[i] <- inv_link(prior$.dist_obj[[i]], prior$link[[i]])
    }
  }


  prior$labels <- glue("{prior$resp}_{prior$class}_{prior$par}_{prior$coef}_{prior$group}")
  prior$labels <- gsub("_+", "_", prior$labels)
  prior$labels <- gsub("(^_|_$)", "", prior$labels)
  prior$labels <- paste0(prior$labels, " ~ ", format(prior$.dist_obj))
  prior <- prior[order(prior$par, prior$class),]
  prior$labels <- factor(prior$labels, levels = unique(prior$labels))
  plot(prior$.dist_obj, type = type, facets = facets, labels = prior$labels,
       stat_slab_control = stat_slab_control)
}


pkg_search_env <- function(packages) {
  stopif(any(!is.character(packages)))
  search_env <- globalenv()
  search_env <- c(search_env, rlang::search_envs())
  search_env <- c(search_env, lapply(packages, function(x) asNamespace(x)))
  search_env <- unname(search_env)
  as.environment(do.call(c, lapply(search_env, as.list)))
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


# TODO: add this after ggdist next release
#' #' ggdist::stat_slab() with nice defaults
#' #'
#' #' A wrapper for ggdist::stat_slab() that sets default subguides and scales to
#' #' display the density on the y axis.
#' #' @param ... Additional arguments passed to ggdist::stat_slab()
#' #' @inheritParams ggdist::geom_slab
#' #' @return a stat_slab() object
#' #' @keywords plot
#' #' @export
#' stat_slab2 <- function(...,
#'                        subguide = ggdist::subguide_outside(title = "density"),
#'                        subscale = if (packageVersion('ggdist') > "3.3.2") {
#'                          ggdist::subscale_thickness(expand = expansion(c(0, 0.05)))
#'                          } else {
#'                            NULL
#'                          },
#'                        normalize = 'groups') {
#'
#'   suppressWarnings(ggdist::stat_slab(..., subguide = subguide, subscale = subscale, normalize = normalize))
#' }

