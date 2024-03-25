test_that("init argument is overwritten if the user supplies it", {
  config_args <- list(formula = 'a', family = 'b', data = 'd', stanvars = 'e', init = 1)
  dots <- list(init = 2)
  out <- combine_args(nlist(config_args, dots))
  expect_equal(out, list(formula = 'a', family = 'b', data = 'd', stanvars = 'e', init = 2))
})

test_that("user cannot overwrite the custom family", {
  config_args <- list(formula = 'a', family = 'b', data = 'd', stanvars = 'e', init = 1)
  dots <- list(family = 'c')
  expect_error(combine_args(nlist(config_args, dots)), 'You cannot provide a family argument to bmm')
})

test_that("empty dots don't crash the function", {
  config_args <- list(formula = 'a', family = 'b', data = 'd', stanvars = 'e', init = 1)
  out <- combine_args(nlist(config_args))
  expect_equal(out, list(formula = 'a', family = 'b', data = 'd', stanvars = 'e', init = 1))
})


test_that("missing arguments in models are handled correctly", {
  expect_error(mixture2p(), "arguments are missing in mixture2p\\(\\)\\: resp_err")
  expect_error(sdmSimple(), "arguments are missing in sdmSimple\\(\\)\\: resp_err")
  expect_error(mixture3p('y'), "arguments are missing in mixture3p\\(\\)\\: nt_features, set_size")
  expect_error(mixture3p(set_size = 'y'), "arguments are missing in mixture3p\\(\\)\\: resp_err, nt_features")
})

test_that("get_variables works", {
  expect_equal(get_variables('a', c('a', 'b', 'c')), 'a')
  expect_equal(get_variables('a', c('a', 'b', 'c'), regex = TRUE), 'a')
  expect_equal(get_variables('a', c('a', 'b', 'c'), regex = FALSE), 'a')
  expect_equal(get_variables('a|b', c('a', 'b', 'c'), regex = TRUE), c('a', 'b'))
  expect_equal(get_variables('abc', c('abc1', 'abc2', 'abc3', 'other'), regex = TRUE),
               c('abc1', 'abc2', 'abc3'))
  expect_equal(get_variables('^abc', c('abc1', 'abc2', 'abc3', 'other_abc4'), regex = TRUE),
               c('abc1', 'abc2', 'abc3'))
  expect_equal(get_variables('abc$', c('nt1_abc', 'nt2_abc', 'nt3_abc', 'other_abc4'), regex = TRUE),
               c('nt1_abc', 'nt2_abc', 'nt3_abc'))
  expect_equal(get_variables('nt.*_abc', c('nt1_abc', 'nt2_abc', 'nt3_abc', 'other_abc4'), regex = TRUE),
               c('nt1_abc', 'nt2_abc', 'nt3_abc'))
  expect_equal(get_variables('a|b', c('a', 'b', 'c'), regex = FALSE), 'a|b')
  expect_error(get_variables('d', c('a', 'b', 'c'), regex = TRUE))
})

test_that("bmm_options works", {
  withr::defer(suppressMessages(bmm_options()))
  expect_message(bmm_options(), "Current bmm options")
  expect_message(bmm_options(sort_data = TRUE), "sort_data = TRUE")
  expect_equal(getOption('bmm.sort_data'), TRUE)
  op <- suppressMessages(bmm_options(sort_data = FALSE))
  expect_equal(getOption('bmm.sort_data'), FALSE)
  options(op)
  expect_equal(getOption('bmm.sort_data'), TRUE)
})
