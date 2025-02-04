library(MPTinR)
library(dplyr)
library(tidyr)
library(brms)

options(mc.cores = 4, brms.backend = "cmdstanr")

source(".dev/explore_mpt/helper-funs.r")

# Basic Two-high-threshold model for Broeder and Schutz
# (from https://cran.r-project.org/web/packages/MPTinR/vignettes/mptinr_introduction.pdf)
mod_2htm_2 <- "
  # Tree for old items (10%): First 'yes', then 'no'
  Do + (1 - Do) * g1
  (1-Do)*(1-g1)

  #Tree for new items (90%): First 'yes', then 'no'
  (1-Dn) * g1
  Dn + (1-Dn) * (1 - g1)

  # Tree for old items (25%): First 'yes', then 'no'
  Do + (1 - Do) * g2
  (1-Do)*(1-g2)

  #Tree for new items (75%): First 'yes', then 'no'
  (1-Dn) * g2
  Dn + (1-Dn) * (1 - g2)

  # Tree for old items (50%): First 'yes', then 'no'
  Do + (1 - Do) * g3
  (1-Do)*(1-g3)

  #Tree for new items (50%): First 'yes', then 'no'
  (1-Dn) * g3
  Dn + (1-Dn) * (1 - g3)

  # Tree for old items (75%): First 'yes', then 'no'
  Do + (1 - Do) * g4
  (1-Do)*(1-g4)

  #Tree for new items (25%): First 'yes', then 'no'
  (1-Dn) * g4
  Dn + (1-Dn) * (1 - g4)

  # Tree for old items (90%): First 'yes', then 'no'
  Do + (1 - Do) * g5
  (1-Do)*(1-g5)

  #Tree for new items (10%): First 'yes', then 'no'
  (1-Dn) * g5
  Dn + (1-Dn) * (1 - g5)
"

# load broeder data
data("d.broeder")
data <- d.broeder
head(data)

# fit the MPTinR model
br.2htm <- fit.mpt(data, textConnection(mod_2htm_2))
br.2htm$parameters$aggregated

# need to transform the data for brms
# we have 2x5 design, with old and new items and with prop_old
conditions <- expand.grid(
  resp_type = c("old", "new"), 
  item_type = c("old", "new"), 
  propold = as.character(c(0.1, 0.25, 0.5, 0.75, 0.9))
)
branch_names = with(conditions, paste0(resp_type, "_", item_type, "-", propold))
data_df <- to_long_mpt_data(data, branch_names) |> 
  separate_wider_delim(condition, delim = "-", names = c("item_type", "prop_old")) |> 
  mutate(is_old = as.numeric(item_type == "old"))


# brms formula. Main model function is the same, but we predict parameters by item_type or prop_old
mpt_2htm_form_h <-
  bf(old | trials(n) ~ logit(is_old * (D + (1 - D) * g) + (1 - is_old) * (1 - D) * g)) +
  nlf(D ~ inv_logit(lD)) +
  nlf(g ~ inv_logit(lg)) +
  lf(lD ~ 0 + item_type + (0 + item_type | id)) +
  lf(lg ~ 0 + prop_old + (0 + prop_old | id)) +
  set_nl(TRUE)

prior_2htm <-
  prior("logistic(0, 1)", nlpar = "lD", class = "b") +
  prior("logistic(0, 1)", nlpar = "lg", class = "b")


brmfit_2htm_h <- brm(
  formula = mpt_2htm_form_h,
  data = data_df,
  prior = prior_2htm,
  family = binomial()
)

# model summary and plots
brmfit_2htm_h
plot(brmfit_2htm_h)
pp_check(brmfit_2htm_h)

# technically incorrect because the random effects have to be addedd before transforming, but quick and dirty
as_draws_array(brmfit_2htm_h) |> 
  inv_logit() |> 
  summary()  |> 
  dplyr::filter(grepl("b_l.*", variable))
