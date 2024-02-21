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

test_that("save_pars_bmm2brm works as expected", {
  expect_equal(brms::save_pars(all = TRUE), save_pars_bmm2brms(all = TRUE))
  expect_error(save_pars_bmm2brms(all = "all"))
  expect_error(save_pars_bmm2brms(latent = TRUE))
})

