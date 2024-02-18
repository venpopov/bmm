test_that('IMMfull works when setsize is not predicted and there is setsize 1', {
  dat <- OberauerLin_2017
  formula <- bmf(
    kappa ~ 1,
    a ~ 1,
    c ~ 1,
    s ~ 1
  )
  model <- IMMfull(resp_err = 'dev_rad',
                   nt_features = paste0("col_nt", 1:7),
                   nt_distances = paste0("dist_nt", 1:7),
                   setsize = "set_size")
  res <- try(fit <- fit_model(formula, dat, model,
                   backend = 'mock', mock=1, rename = F))
  expect_false(is_try_error(res))

})

test_that('IMMabc works when setsize is not predicted and there is setsize 1', {
  dat <- OberauerLin_2017
  formula <- bmf(
    kappa ~ 1,
    a ~ 1,
    c ~ 1
  )
  model <- IMMabc(resp_err = 'dev_rad',
                   nt_features = paste0("col_nt", 1:7),
                   setsize = "set_size")
  res <- try(fit <- fit_model(formula, dat, model,
                              backend = 'mock', mock=1, rename = F))
  expect_false(is_try_error(res))

})


test_that('IMMbsc works when setsize is not predicted and there is setsize 1', {
  dat <- OberauerLin_2017
  formula <- bmf(
    kappa ~ 1,
    c ~ 1,
    s ~ 1
  )
  model <- IMMbsc(resp_err = 'dev_rad',
                  nt_features = paste0("col_nt", 1:7),
                  nt_distances = paste0("dist_nt", 1:7),
                  setsize = "set_size")
  res <- try(fit <- fit_model(formula, dat, model,
                              backend = 'mock', mock=1, rename = F))
  expect_false(is_try_error(res))

})
