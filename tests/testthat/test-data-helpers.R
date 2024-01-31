test_that("check_data() produces expected errors and warnings", {
  expect_error(check_data(.model_2p()),
               "Data must be specified using the 'data' argument.")
  expect_error(check_data(.model_2p(), data.frame(), brms::bf(y ~ 1)),
               "Argument 'data' does not contain observations.")
  expect_error(check_data(.model_2p(), data.frame(x = 1), brms::bf(y ~ 1)),
               "The response variable 'y' is not present in the data.")

  mls <- lapply(c('2p','3p','IMMabc','IMMbsc','IMMfull'), get_model)
  for (ml in mls) {
    expect_warning(check_data(ml(), data.frame(y = 12, x = 1, z = 2), brms::bf(y ~ 1),
                              non_targets = 'x', setsize=2, spaPos = 'z'),
                   "It appears your response variable is in degrees. We will transform it to radians.")
    expect_silent(check_data(ml(), data.frame(y = 1, x = 1, z = 2), brms::bf(y ~ 1),
                              non_targets = 'x', setsize=2, spaPos = 'z'))
  }

  mls <- lapply(c('3p','IMMabc','IMMbsc','IMMfull'), get_model)
  for (ml in mls) {
    expect_error(check_data(ml(), data.frame(y = 1, x = 1, z = 2), brms::bf(y ~ 1),
                            non_targets = 'x', spaPos = 'z'),
                 "Argument 'setsize' is not specified")
    expect_error(check_data(ml(), data.frame(y = 1, x = 1, z = 2), brms::bf(y ~ 1),
                            setsize = 'x', spaPos = 'z'),
                 "Argument 'non_targets' must be specified.")
    expect_error(check_data(ml(), data.frame(y = 1, x = 1, z = 2), brms::bf(y ~ 1),
                            non_targets='x', setsize = TRUE, spaPos = 'z'),
                 "Argument 'setsize' must be either a single numeric value or a character string.")
    expect_error(check_data(ml(), data.frame(y = 1, x = 1, z = 2), brms::bf(y ~ 1),
                            non_targets='x', setsize = c(1,2,3), spaPos = 'z'),
                 "Argument 'setsize' must be either a single numeric value or a character string.")
    expect_error(check_data(ml(), data.frame(y = 1, x = 1, z = 2), brms::bf(y ~ 1),
                            non_targets='x', setsize = 5, spaPos = 'z'),
                 "'non_targets' is less than max\\(setsize\\)-1")
    }

  mls <- lapply(c('IMMbsc','IMMfull'), get_model)
  for (ml in mls) {
    expect_error(check_data(ml(), data.frame(y = 1, x1 = 1, x2=2,x3=3,x4=4, z = 2), brms::bf(y ~ 1),
                            non_targets=paste0('x',1:4), setsize = 5, spaPos = 'z'),
                 "'spaPos' is less than max\\(setsize\\)-1")
  }
})

test_that("check_data() returns a data.frame()", {
  mls <- lapply(supported_models(), get_model)
  for (ml in mls) {
    expect_s3_class(check_data(ml(), data.frame(y = 1, x = 1, z = 2), brms::bf(y ~ 1),
                               non_targets = 'x', setsize=2, spaPos = 'z'), "data.frame")
  }
})
