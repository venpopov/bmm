
<!-- README.md is generated from README.Rmd. Please edit that file -->

# bmm <!-- badges: start --> <!-- badges: end -->

The goal of bmm (Bayesian Measurement Models for Visual Working Memory)
is to make it easier to estimate common mixture measurement models for
visual working memory using the ‘brms’ package. Currently implemented
are the two-parameter mixture model by Zhang and Luck (2008), and the
three- parameter mixture model by Bays et al (2009).

## Installation

You can install the development version of bmm from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("venpopov/bmm")
```

## Example

The three-parameter mixture model by Bays et al (2009) assumes that
responses can come from three different sources - noisy representation
of the target, confusion with noisy representation of non-target items,
or guessing based on a uniform distribution. To estimate these
parameters for a dataset, we can use the `fit_model()` function. First,
let’s generate a dataset with known parameters. We can use the function
`gen_3p_data()`

``` r
library(bmm)
library(tidyverse)
#> ── Attaching packages ─────────────────────────────────────── tidyverse 1.3.2 ──
#> ✔ ggplot2 3.4.0     ✔ purrr   1.0.1
#> ✔ tibble  3.1.8     ✔ dplyr   1.1.0
#> ✔ tidyr   1.3.0     ✔ stringr 1.5.0
#> ✔ readr   2.1.3     ✔ forcats 1.0.0
#> ── Conflicts ────────────────────────────────────────── tidyverse_conflicts() ──
#> ✖ dplyr::filter() masks stats::filter()
#> ✖ dplyr::lag()    masks stats::lag()
dat <- gen_3p_data(N=2000, pmem=0.6, pnt=0.3, kappa=10, setsize=4, relative_resp=T)
head(dat)
#>            y    nt1_loc    nt2_loc      nt3_loc
#> 1  0.2569243  2.6968595  1.1011967 -2.381452975
#> 2 -0.2995747  3.0590161 -2.6963216  2.097664573
#> 3 -0.6496101  1.0919187 -1.1204057 -0.897425564
#> 4  0.6172717  1.3327058  1.0317656 -0.522874907
#> 5 -0.2693346  2.1338354  0.2148838 -0.001552538
#> 6 -0.2150131 -0.3817029 -2.8162736  0.636291417
```

We have a dataset of 2000 observations of response error, of which 60%
(pmem=0.6) come from the target distribution, 30% (pnt=0.3) are
non-target swaps, and 10% are guessing. The precision of the von Mises
distribution is 10, the presented setsize is 4 (one target and three
lures/non-targets), and the values are coded relative to the target
value (i.e., response error for the y variable or displacement relative
to the target for the lures).

Just for visualization purposes, here’s a histogram of the error
distribution, demonstrating a typical pattern - a normal distribution
centered on 0, with long tails:

``` r
hist(dat$y, breaks = 60, xlab = "Response error relative to target")
```

<img src="man/figures/README-unnamed-chunk-2-1.png" width="100%" />
Another key property of the data is that some error responses are not
random, but that they are due to confusion of the target with one of the
lures. We can visualize this by centering the response error relative to
each of the possible non-target locations. We do this with the helper
function `calc_error_relative_to_nontargets()`:

``` r
dat %>% 
  calc_error_relative_to_nontargets('y', paste0('nt',1:3,'_loc')) %>% 
  ggplot(aes(y_nt)) +
  geom_histogram()
#> `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.
```

<img src="man/figures/README-unnamed-chunk-3-1.png" width="100%" />

Ok, so now let’s fit the three-parameter model. We only need to do two
things: - Specify the model formula - Call fit_model()

In this example the parameters don’t vary over conditions, so we have no
predictors. `y` is the name of the response error variable, whereas
`kappa`, `thetat` and `thetant` are the parameters of the model -
precision, mixing proportion for correct responses and mixing proportion
for non-target swaps.

``` r
ff <- brms::bf(y ~ 1,
         kappa ~ 1,
         thetat ~ 1,
         thetant ~ 1)
```

Finally we just run the model. The arguments to the function explained
in `help(fit_model)` and you can also pass any additional arguments that
you would pass to `brm`.

``` r
fit <- fit_model(formula = ff,
                 data = dat,
                 model_type = "3p",
                 lures = paste0('nt',1:3,'_loc'),
                 setsize=4,
                 parallel=T,
                 iter=500)
```
