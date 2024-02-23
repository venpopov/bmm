# setup some example fits for internal testing

library(bmm)
set.seed(1234)

ref_data <- function() {
  withr::local_package('dplyr')
  dat <- OberauerLin_2017
  dat <- dat %>%
    mutate(ID = as.factor(ID),
           set_size = as.factor(set_size)) %>%
    filter(ID %in% c(1,2,3,4,5,6,7,8,9,10),
           set_size %in% c(1,2,3,4)) %>%
    arrange(set_size, ID)
  dat
}

data <- ref_data()


formula <- bmf(c ~ 0 + set_size,
               kappa ~ 1)
model <- sdmSimple('dev_rad')
bmmfit_example1 <- fit_model(formula, data, model,
                             parallel = TRUE,
                             iter = 100,
                             refresh = 0,
                             init = 1,
                             silent = 1,
                             chains = 1,
                             backend = 'cmdstanr')


usethis::use_data(bmmfit_example1, internal = TRUE, overwrite = TRUE)
