library(MPTinR)
library(dplyr)
library(tidyr)
library(brms)

options(mc.cores = 4, brms.backend = "cmdstanr")

source(".dev/explore_mpt/helper-funs.r")

# Basic Two-high-threshold model
model_2htm <- "
D + (1 - D) * g         # p_old_old
(1 - D) * (1 - g)       # p_new_old

(1 - D) * g             # p_old_new
D + (1 - D) * (1 - g)   # p_new_new
"

# Generate random data from the MPTinR function from the above model
data <- gen.data(
  parameter.values = c(D = 0.7, g = 0.5), 
  samples = 100, 
  model.filename = textConnection(model_2htm), n.per.item.type = c(50, 50)
)

# fit mpt model and show results
mpt_fit <- fit.mpt(data, textConnection(model_2htm))
mpt_fit$parameters$aggregated

# transform data for brms
data_df <- to_long_mpt_data(data, c("old_old", "new_old", "old_new", "new_new"))
data_df$is_old = as.numeric(data_df$condition == "old")


mpt_2htm_form <-
  bf(old | trials(n) ~ logit(is_old * (D + (1 - D) * g) + (1 - is_old) * (1 - D) * g)) +
  nlf(D ~ inv_logit(lD)) +
  nlf(g ~ inv_logit(lg)) +
  lf(lD ~ 1 + (1|id)) +
  lf(lg ~ 1 + (1|id)) +
  set_nl(TRUE)

prior_2htm <-
  prior("logistic(0, 1)", nlpar = "lD", class = "b") +
  prior("logistic(0, 1)", nlpar = "lg", class = "b")


brmfit_2htm <- brm(
  formula = mpt_2htm_form,
  data = data_df,
  prior = prior_2htm,
  family = binomial()
)

# model summary
brmfit_2htm

# technically incorrect because the random effects have to be addedd before transforming, but they are null here so...
as_draws_array(brmfit_2htm) |> 
  inv_logit() |> 
  summary()  |> 
  dplyr::filter(grepl("b_l*", variable))
