test_that("init argument is overwritten if the user supplies it", {
  config_args <- list(formula = 'a', family = 'b', prior = 'c', data = 'd', stanvars = 'e', init = 1)
  dots <- list(init = 2)
  out <- combine_args(nlist(config_args, dots))
  expect_equal(out, list(formula = 'a', family = 'b', prior = 'c', data = 'd', stanvars = 'e', init = 2))
})

test_that("user cannot overwrite the custom family", {
  config_args <- list(formula = 'a', family = 'b', prior = 'c', data = 'd', stanvars = 'e', init = 1)
  dots <- list(family = 'c')
  expect_error(combine_args(nlist(config_args, dots)), 'You cannot provide a family argument to fit_model')
})

test_that("empty dots don't crash the function", {
  config_args <- list(formula = 'a', family = 'b', prior = 'c', data = 'd', stanvars = 'e', init = 1)
  out <- combine_args(nlist(config_args))
  expect_equal(out, list(formula = 'a', family = 'b', prior = 'c', data = 'd', stanvars = 'e', init = 1))
})

test_that("missing arguments in models are handled correctly", {
  expect_error(mixture2p(), "arguments are missing in mixture2p\\(\\)\\: resp_err")
  expect_error(sdmSimple(), "arguments are missing in sdmSimple\\(\\)\\: resp_err")
  expect_error(mixture3p('y'), "arguments are missing in mixture3p\\(\\)\\: nt_features, setsize")
  expect_error(mixture3p(setsize='y'), "arguments are missing in mixture3p\\(\\)\\: resp_err, nt_features")
})
