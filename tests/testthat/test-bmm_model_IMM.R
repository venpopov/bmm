dat <- oberauer_lin_2017

test_that('imm works when set_size is not predicted and there is set_size 1', {
  skip_on_cran()
  formula <- bmf(kappa ~ 1, a ~ 1, c ~ 1, s ~ 1)
  model <- imm(resp_error = 'dev_rad',
               nt_features = paste0("col_nt", 1:7),
               nt_distances = paste0("dist_nt", 1:7),
               set_size = "set_size")
  res <- try(fit <- bmm(formula, dat, model,
                   backend = 'mock', mock=1, rename = F))
  expect_false(is_try_error(res))

})

test_that('imm_abc works when set_size is not predicted and there is set_size 1', {
  skip_on_cran()
  formula <- bmf(kappa ~ 1, a ~ 1, c ~ 1)
  model <- imm(resp_error = 'dev_rad',
               nt_features = paste0("col_nt", 1:7),
               set_size = "set_size",
               version = "abc")
  res <- try(fit <- bmm(formula, dat, model,
                        backend = 'mock', mock=1, rename = F))
  expect_false(is_try_error(res))

})


test_that('imm_bsc works when set_size is not predicted and there is set_size 1', {
  skip_on_cran()
  formula <- bmf(kappa ~ 1, c ~ 1, s ~ 1)
  model <- imm(resp_error = 'dev_rad',
               nt_features = paste0("col_nt", 1:7),
               nt_distances = paste0("dist_nt", 1:7),
               set_size = "set_size",
               version = "bsc")
  res <- try(fit <- bmm(formula, dat, model,
                        backend = 'mock', mock=1, rename = F))
  expect_false(is_try_error(res))

})


test_that('IMM models give an error if set_size is a predictor but there is an intercept', {
  skip_on_cran()
  formula <- bmf(kappa ~ 1, c ~ 1, a ~ set_size)
  model <- imm(resp_error = 'dev_rad',
               nt_features = paste0("col_nt",1:7),
               set_size = "set_size",
               version = "abc")
  expect_error(bmm(formula, dat, model, backend = 'mock', mock=1, rename = F),
               'This model requires that the intercept is supressed when set_size is used as predictor.')

  formula <- bmf(kappa ~ 1, c ~ 1, s ~ set_size)
  model <- imm(resp_error = 'dev_rad',
               nt_features = paste0("col_nt",1:7),
               nt_distances = paste0("dist_nt",1:7),
               set_size = "set_size",
               version = "bsc")
  expect_error(bmm(formula, dat, model, backend = 'mock', mock=1, rename = F),
               'This model requires that the intercept is supressed when set_size is used as predictor.')

  formula <- bmf(kappa ~ 1, a ~ 1, c ~ 1, s ~ set_size)
  model <- imm(resp_error = 'dev_rad',
                   nt_features = paste0("col_nt",1:7),
                   nt_distances = paste0("dist_nt",1:7),
                   set_size = "set_size")
  expect_error(bmm(formula, dat, model, backend = 'mock', mock=1, rename = F),
               'This model requires that the intercept is supressed when set_size is used as predictor.')

  formula <- bmf(kappa ~ 1, c ~ 0 + set_size, a ~ set_size)
  model <- imm(resp_error = 'dev_rad',
               nt_features = paste0("col_nt",1:7),
               set_size = "set_size",
               version = "abc")
  expect_error(bmm(formula, dat, model, backend = 'mock', mock=1, rename = F),
               'This model requires that the intercept is supressed when set_size is used as predictor.')

  formula <- bmf(kappa ~ 0 + set_size, c ~ 1, s ~ set_size)
  model <- imm(resp_error = 'dev_rad',
               nt_features = paste0("col_nt",1:7),
               nt_distances = paste0("dist_nt",1:7),
               set_size = "set_size",
               version = "bsc")
  expect_error(bmm(formula, dat, model, backend = 'mock', mock=1, rename = F),
               'This model requires that the intercept is supressed when set_size is used as predictor.')

  formula <- bmf(kappa ~ 1, a ~ 1, c ~ 0 + set_size, s ~ set_size)
  model <- imm(resp_error = 'dev_rad',
                nt_features = paste0("col_nt",1:7),
                nt_distances = paste0("dist_nt",1:7),
                set_size = "set_size")
  expect_error(bmm(formula, dat, model, backend = 'mock', mock=1, rename = F),
               'This model requires that the intercept is supressed when set_size is used as predictor.')
})


test_that('IMM models run when set_size is a predictor and intercept is supressed', {
  skip_on_cran()
  formula <- bmf(kappa ~ 1, c ~ 1, a ~ 0 + set_size)
  model <- imm(resp_error = 'dev_rad',
               nt_features = paste0("col_nt",1:7),
               set_size = "set_size",
               version = "abc")
  expect_silent(bmm(formula, dat, model, backend = 'mock', mock=1, rename = F))

  formula <- bmf(kappa ~ 1, c ~ 1, s ~ 0 + set_size)
  model <- imm(resp_error = 'dev_rad',
               nt_features = paste0("col_nt",1:7),
               nt_distances = paste0("dist_nt",1:7),
               set_size = "set_size",
               version = "bsc")
  expect_silent(bmm(formula, dat, model, backend = 'mock', mock = 1, rename = F))


  formula <- bmf(kappa ~ 1, a ~ 1, c ~ 1, s ~ 0 + set_size)
  model <- imm(resp_error = 'dev_rad',
               nt_features = paste0("col_nt",1:7),
               nt_distances = paste0("dist_nt",1:7),
               set_size = "set_size")
  expect_silent(bmm(formula, dat, model, backend = 'mock', mock=1, rename = F))
})
