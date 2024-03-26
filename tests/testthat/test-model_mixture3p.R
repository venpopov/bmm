test_that('mixture3p works when set_size is not predicted and there is set_size 1', {
  skip_on_cran()
  dat <- oberauer_lin_2017
  formula <- bmf(
    kappa ~ 1,
    thetat ~ 1,
    thetant ~ 1
  )
  model <- mixture3p(resp_error = 'dev_rad',
                     nt_features = paste0("col_nt",1:7),
                     set_size = "set_size")
  res <- try(fit <- bmm(formula, dat, model,
                   backend = 'mock', mock=1, rename = F), silent = TRUE)
  expect_false(is_try_error(res))

})

test_that('mixture3p gives an error if set_size is a predictor but there is an intercept', {
  skip_on_cran()
  dat <- oberauer_lin_2017
  formula <- bmf(
    kappa ~ 1,
    thetat ~ 1,
    thetant ~ set_size
  )
  model <- mixture3p(resp_error = 'dev_rad',
                     nt_features = paste0("col_nt",1:7),
                     set_size = "set_size")
  expect_error(bmm(formula, dat, model, backend = 'mock', mock=1, rename = F),
               'This model requires that the intercept is supressed when set_size is used as predictor.')


  formula <- bmf(
    kappa ~ 1 + session,
    thetat ~ 1,
    thetant ~ 0 + set_size
  )
  expect_silent({fit = bmm(formula, dat, model, backend = 'mock', mock=1, rename = F)})
})
