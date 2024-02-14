test_that('wrong_parameters works', {
  f <- bmmformula(c ~ 1,
                  a ~ 1,
                  s ~ 1,
                  kappa ~ 1)
  expect_equal(length(wrong_parameters(IMMfull(NA,NA, NA,NA), f)), 0)
  expect_equal(wrong_parameters(IMMbsc(NA,NA, NA,NA), f), 'a')
  expect_equal(wrong_parameters(sdmSimple(NA), f), c('a','s'))
})

test_that('wrong_parameters doesnt reject non-linear transformations', {
  f <- bmmformula(c ~ 1,
                  a ~ 1,
                  s ~ 1,
                  kappa ~ exp(logkappa),
                  logkappa ~ 1)
  expect_equal(length(wrong_parameters(IMMfull(NA,NA, NA,NA), f)), 0)
  expect_equal(wrong_parameters(IMMbsc(NA,NA, NA,NA), f), 'a')
  expect_equal(wrong_parameters(sdmSimple(NA), f), c('a','s'))
})


test_that("add_missing_parameters works", {
  f <- bmmformula(c ~ 1)
  model_pars <- names(IMMfull(NA,NA, NA,NA)$info$parameters)
  expect_equal(names(suppressMessages(add_missing_parameters(IMMfull(NA,NA, NA,NA), f))), model_pars)

  f <- bmmformula(c ~ 1,
                  s ~ 1,
                  a ~ 1,
                  kappa ~ 1)
  model_pars <- names(IMMfull(NA,NA, NA,NA)$info$parameters)
  expect_equal(names(suppressMessages(add_missing_parameters(IMMfull(NA,NA, NA,NA), f))), model_pars)
})
