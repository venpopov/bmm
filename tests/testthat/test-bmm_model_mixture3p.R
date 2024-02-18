test_that('mixture3p works when setsize is not predicted and there is setsize 1', {
  dat <- OberauerLin_2017
  formula <- bmf(
    kappa ~ 1,
    thetat ~ 1,
    thetant ~ 1
  )
  model <- mixture3p(resp_err = 'dev_rad',
                     nt_features = paste0("col_nt",1:7),
                     setsize = "set_size")
  res <- try(fit <- fit_model(formula, dat, model,
                   backend = 'mock', mock=1, rename = F), silent = TRUE)
  expect_false(is_try_error(res))

})
