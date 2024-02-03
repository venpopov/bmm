test_that("check_data() produces expected errors and warnings", {
  expect_error(check_data(.model_mixture2p()),
               "Data must be specified using the 'data' argument.")
  expect_error(check_data(.model_mixture2p(), data.frame(), brms::bf(y ~ 1)),
               "Argument 'data' does not contain observations.")
  expect_error(check_data(.model_mixture2p(), data.frame(x = 1), brms::bf(y ~ 1)),
               "The response variable 'y' is not present in the data.")

  mls <- lapply(c('mixture2p','mixture3p','IMMabc','IMMbsc','IMMfull'), get_model2)
  for (ml in mls) {
    expect_warning(check_data(ml(non_targets = 'x', setsize=2, spaPos = 'z'),
                              data.frame(y = 12, x = 1, z = 2),
                              brms::bf(y ~ 1)),
                   "It appears your response variable is in degrees. We will transform it to radians.")
    expect_silent(check_data(ml(non_targets = 'x', setsize=2, spaPos = 'z'),
                             data.frame(y = 1, x = 1, z = 2), brms::bf(y ~ 1)))
  }

  mls <- lapply(c('mixture3p','IMMabc','IMMbsc','IMMfull'), get_model2)
  for (ml in mls) {
    expect_error(check_data(ml(non_targets = 'x', spaPos = 'z'), data.frame(y = 1, x = 1, z = 2), brms::bf(y ~ 1)),
                 'argument "setsize" is missing, with no default')
    expect_error(check_data(ml(setsize = 'x', spaPos = 'z'), data.frame(y = 1, x = 1, z = 2), brms::bf(y ~ 1)),
                 'argument "non_targets" is missing, with no default')
    expect_error(check_data(ml(non_targets='x', setsize = TRUE, spaPos = 'z'), data.frame(y = 1, x = 1, z = 2), brms::bf(y ~ 1)),
                 "Argument 'setsize' must be either a single numeric value or a character string.")
    expect_error(check_data(ml(non_targets='x', setsize = c(1,2,3), spaPos = 'z'), data.frame(y = 1, x = 1, z = 2), brms::bf(y ~ 1)),
                 "Argument 'setsize' must be either a single numeric value or a character string.")
    expect_error(check_data(ml(non_targets='x', setsize = 5, spaPos = 'z'), data.frame(y = 1, x = 1, z = 2), brms::bf(y ~ 1)),
                 "'non_targets' is less than max\\(setsize\\)-1")
    }

  mls <- lapply(c('IMMbsc','IMMfull'), get_model2)
  for (ml in mls) {
    expect_error(check_data(ml(non_targets=paste0('x',1:4), setsize = 5, spaPos = 'z'), data.frame(y = 1, x1 = 1, x2=2,x3=3,x4=4, z = 2), brms::bf(y ~ 1)),
                 "'spaPos' is less than max\\(setsize\\)-1")
  }
})

test_that("check_data() returns a data.frame()", {
  mls <- lapply(supported_models(print_call=FALSE), get_model)
  for (ml in mls) {
    expect_s3_class(check_data(ml(non_targets = 'x', setsize=2, spaPos = 'z'), data.frame(y = 1, x = 1, z = 2), brms::bf(y ~ 1)), "data.frame")
  }
})



test_that("wrap(x) returns the same for values between -pi and pi", {
  x <- runif(100, -pi, pi)
  expect_equal(wrap(x), x)
})

test_that("wrap(x) returns the correct value for values between (pi, 2*pi)", {
  x <- pi+1
  expect_equal(wrap(x), -(pi-1))
})


test_that("wrap(x) returns the correct value for values between (-2*pi, -pi)", {
  x <- -pi-1
  expect_equal(wrap(x), pi-1)
})

test_that("wrap(x) returns the correct value for values over 2*pi", {
  x <- 2*pi+1
  expect_equal(wrap(x), 1)
})

test_that("wrap(x) returns the correct value for values between (3*pi,4*pi)", {
  x <- 3*pi+1
  expect_equal(wrap(x), -(pi-1))
})
